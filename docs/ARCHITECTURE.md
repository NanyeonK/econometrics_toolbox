# Architecture

This document describes the internal pipeline, module responsibilities,
data-flow, drift signatures, the HAC code path, and the pipeline-isolation
discipline that produced this v0 bootstrap.

## Backend pipeline overview

The toolbox is a single CLI entrypoint orchestrating three stages:

1. **Validate** — parse the YAML call manifest and run all structural
   and data-level checks. No estimation runs until every check passes.
2. **Estimate** — apply `sample_filter`, apply `missing_policy`, build
   the exact `fixest::feols` formula, fit the model with `nthreads = 1`,
   capture warnings, and write the coefficient CSV plus model summary
   text file.
3. **Write** — assemble the result manifest list (header, call,
   input, model, outputs, warnings, drift_metadata, verdict,
   blocking_reasons) and serialise it as YAML.

A run exits with status 0 only after stage 3 completes; any failure in
any stage causes a `[VALIDATION ERROR]` / `[ESTIMATION ERROR]` /
`[OUTPUT ERROR]` message to stderr and a nonzero exit. See
`docs/USAGE.md` § "Exit codes" for the exact status code table.

## R source files

### `R/run_call_manifest.R`

CLI entrypoint. Locates its own directory so it can `source()` siblings
even when invoked from a different working directory. Parses argv,
guards for missing packages (`fixest`, `yaml`, `jsonlite`, `digest`),
reads the YAML, runs Group A/B/C validation, loads the CSV, runs
Group D validation, calls `estimate_panel_fe`, calls
`write_result_manifest`, and exits 0 on success. Every stage is wrapped
in `tryCatch` so the script always terminates with a deterministic exit
code.

### `R/validate_call_manifest.R`

Fail-closed validator with a single public function,
`validate_call_manifest(cm, df = NULL)`. When `df` is not supplied it
runs Group A (top-level structural), Group B (specification structural),
and Group C (output path) checks. When `df` is supplied it additionally
runs Group D (data-level: required columns, dependent variable, key
regressor, controls, fixed effects, cluster variables, weights column,
and `missing_policy` enum). On any violation it calls `stop()` with a
`"[VALIDATION ERROR] ..."` message; otherwise it returns
`invisible(TRUE)`.

### `R/estimate_panel_fe.R`

The estimation core. `estimate_panel_fe(cm, df)` records the pre-filter
row count, applies `sample_filter` via `eval(parse(...))`, applies the
declared `missing_policy`, constructs the formula `dep ~ key + controls
| fixed_effects`, builds the appropriate `vcov` / `cluster` argument
for the declared covariance method, builds the weights vector,
calls `fixest::feols(..., nthreads = 1)` with `withCallingHandlers` so
warnings are captured rather than printed, extracts tidy coefficients
(via `broom::tidy` if installed, else manual extraction from
`fixest::coeftable` + `confint`), and writes the coefficient CSV and
model summary text file. Returns a list of artefacts consumed by the
result-manifest writer.

### `R/write_result_manifest.R`

`write_result_manifest(cm, est_result, call_manifest_path)` collects R
and package versions, computes the four drift signatures via
`drift_metadata.R`, assembles the full result manifest list per the
v1 schema, and serialises with `yaml::write_yaml`. Sets
`verdict = "PASS"` and `blocking_reasons = list()` on success.

### `R/drift_metadata.R`

Defines four pure functions, each returning a 64-character SHA-256 hex
digest of a canonical-JSON serialisation of its contributing fields.
The canonical-JSON rule is: sort list keys alphabetically at every
nesting level, sort `controls` / `fixed_effects` / `cluster_variables`
alphabetically, serialise with `jsonlite::toJSON(..., auto_unbox = TRUE,
null = "null")`, then hash the UTF-8 bytes with
`digest::digest(..., algo = "sha256", serialize = FALSE)`.

## Data flow: call manifest in, result manifest out

```
+-------------------------+
| call manifest YAML      |
| (argv[1])               |
+-----------+-------------+
            |
            v
   yaml::read_yaml(cm)
            |
            v
+-------------------------+        Group A: structural
| validate_call_manifest  | -----> Group B: specification
| (cm)                    |        Group C: outputs
+-----------+-------------+
            |
            v
   utils::read.csv(data_path)  ---> df
            |
            v
+-------------------------+        Group D: data-level
| validate_call_manifest  | -----> required_columns / dep / key /
| (cm, df)                |        controls / fes / clusters / wt /
+-----------+-------------+        missing_policy enum
            |
            v
+-------------------------+        sample_filter
| estimate_panel_fe       | -----> missing_policy
| (cm, df)                |        formula build (dep ~ key + ctrls | fes)
|                         |        feols(..., nthreads = 1)
|                         |        tidy_coef -> coefficient CSV
|                         |        summary(fit) -> model summary txt
+-----------+-------------+
            |
            v
+-------------------------+        package_versions, R.version.string
| write_result_manifest   | -----> row counts, drop_reason_summary
| (cm, est, abs_path)     |        exact_formula
|                         |        4 drift signatures
|                         |        verdict = "PASS"
+-----------+-------------+
            |
            v
+-------------------------+
| result manifest YAML    |
| (cm$outputs$            |
|  result_manifest_path)  |
+-------------------------+
            |
            v
   quit(status = 0)
```

## Drift signatures

The result manifest carries four SHA-256 hex digests under
`drift_metadata`. Each is the digest of a canonical-JSON object whose
keys are sorted alphabetically.

| Signature                  | Hashes (sorted JSON object) |
|----------------------------|-----------------------------|
| `sample_signature`         | `{data_hash, row_count_used, sample_filter}` |
| `spec_signature`           | `{controls (sorted), dependent_variable, estimator, fixed_effects (sorted), key_regressor_or_treatment, method_family, weights}` |
| `inference_signature`      | `{cluster_variables (sorted), covariance_method, hac_settings: {enabled, kernel, lag}}` |
| `preprocessing_signature`  | `{missing_policy, source_preprocessing_spec}` |

These signatures are inputs to drift classification performed by the
empirical execution layer — the toolbox itself never decides whether a
drift is acceptable. See `research_system_contract/econometrics_toolbox_contract.md`
§ "Drift discipline" for the classification categories.

## HAC code path note

`fixest`'s Newey-West (`NW`) vcov requires a panel structure (unit +
time) when the data contains cross-sectional duplicates per time
period. The v0 call manifest schema does **not** carry an explicit
`panel.id` field. As an in-spec latitude, `estimate_panel_fe.R`
auto-detects panel columns from the loaded data using a small
conventional-name probe list:

- unit candidates: `unit_id`, `unit`, `id`, `i`
- time candidates: `time_id`, `time`, `year`, `period`, `t`

The first match in each list wins. If both are found, the HAC vcov
formula becomes `NW(lag = k) ~ time + unit`; if only a time column is
found, `NW(lag = k) ~ time`; otherwise the call falls back to bare
`vcov = "NW"` and `fixest` will treat the data as a single time series
in row order (and error out fail-closed on duplicates).

This auto-detection is the minimal-impact workaround that did not
require changing the call-manifest schema, the validator, or the
result-manifest writer. **It should be elevated to an explicit
`panel_id` / `time_id` schema field in v1** so the spec lives in the
manifest rather than in column-name heuristics.

## Pipeline isolation

This repository was bootstrapped by statsclaw under explicit isolation
between the agent roles:

```
planner  ──→  spec.md, test-spec.md, comprehension.md
                │           │
                v           v
            builder      tester
            (sees spec)  (sees test-spec)
                │           │
                └─────┬─────┘
                      v
                   scriber
                (reads run artefacts; writes docs only)
                      │
                      v
                   reviewer
                (reads everything)
```

Builder never sees the test spec. Tester never sees the implementation
spec. Tester writes only black-box tests that invoke the entrypoint
and assert on observable outputs (exit codes, file existence, file
contents, parsed YAML field values) — never on internal call shapes
of builder functions. Scriber writes documentation only; the only
edits allowed inside `R/*.R` are roxygen-style `#'` comment headers
above existing functions.
