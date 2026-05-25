# Usage: authoring a call manifest

This document is a field-by-field guide to writing a call manifest that
`Rscript R/run_call_manifest.R <manifest.yaml>` will accept. The canonical
schema lives in `inst/schemas/econometrics_call_manifest.schema.yaml`;
this page is the human-friendly walkthrough.

## Walking through the toy manifest

The minimal valid manifest is `examples/toy_panel_call_manifest.yaml`.
Below, each field is annotated with whether it is required and what the
validator does with it.

### Header block

```yaml
template: econometrics_call_manifest      # informational, free string
version: v1                               # informational
updated: 2026-05-25                       # informational
project: econometrics_toolbox             # copied into result manifest
phase: phase_2_pilot                      # copied into result manifest
owner: builder_v0_bootstrap               # informational
```

The header fields are not validated for content — they must merely be
parseable. `project` and `phase` are copied into the result manifest.

### `call` section

```yaml
call:
  call_id: TOY_PANEL_FE_001               # REQUIRED, non-empty
  allowed_reason: "Smoke test ..."        # REQUIRED, non-empty
  source_empirical_plan: "..."            # REQUIRED, non-empty
  source_gate_status: "qa/gate_status.yaml"   # informational in v0
  source_spec_grid_manifest: ""           # optional
  source_methodology_spec: ""             # optional
  source_preprocessing_spec: ""           # optional (feeds preprocessing_signature)
  backend_language: R                     # REQUIRED, must equal "R" exactly
  r_entrypoint: R/run_call_manifest.R     # REQUIRED, non-empty
```

`backend_language` must be the exact string `R`. Any other value (case
included) causes a `[VALIDATION ERROR]`. `source_preprocessing_spec`
participates in the `preprocessing_signature` drift hash, so changing
it is observable downstream even though it is optional at the schema
level.

### `input` section

```yaml
input:
  data_path: examples/toy_panel.csv       # REQUIRED, file must exist on disk
  data_hash: ""                           # optional; copied verbatim into result manifest
  row_count_before_filter: 30             # informational; toolbox records the actual count
  required_columns:                       # REQUIRED, non-empty list
    - unit_id
    - time_id
    - outcome
    - treatment
    - control1
    - control2
    - industry_fe
    - cluster_var
  sample_filter: ""                       # optional R expression; empty disables
  missing_policy: complete_cases          # REQUIRED, enum (see below)
```

Every name in `required_columns` must be present in the loaded data; a
missing column is a fail-closed error. `sample_filter`, when non-empty,
is evaluated as an R expression in the data's row environment
(`df[eval(parse(text = filter), envir = df), ]`).

### `specification` section

```yaml
specification:
  method_family: panel_fe_regression      # REQUIRED, must equal that string
  dependent_variable: outcome             # REQUIRED, must be in data
  key_regressor_or_treatment: treatment   # REQUIRED, must be in data
  controls:                               # optional list; may be empty
    - control1
    - control2
  fixed_effects:                          # optional list; may be empty
    - industry_fe
  weights: ""                             # optional; column name or empty
  estimator: fixest_feols                 # REQUIRED, must equal that string
  covariance_method: clustered            # REQUIRED, enum (see below)
  cluster_variables:                      # REQUIRED-when-clustered
    - cluster_var
  hac_settings:
    enabled: false                        # REQUIRED boolean
    lag: null                             # REQUIRED-positive-when-enabled
    kernel: ""                            # informational in v0
```

If `fixed_effects` is empty, the formula becomes `dep ~ key + controls`
with no `|` part. If `controls` is empty, the formula becomes
`dep ~ key | fixed_effects`.

### `outputs` section

```yaml
outputs:
  result_manifest_path: output/toy_result_manifest.yaml   # REQUIRED
  coefficient_table_path: output/toy_coefficients.csv     # REQUIRED
  model_summary_path: output/toy_model_summary.txt        # REQUIRED
  log_path: ""                                            # optional
```

Parent directories are created automatically if missing.

### `forbidden_changes` and `failure_policy`

```yaml
forbidden_changes:                        # REQUIRED, non-empty list
  - outcome
  - key_regressor_or_treatment
  - sample_filter
  - preprocessing_path
  - controls
  - fixed_effects
  - covariance_method
  - cluster_variables

failure_policy: fail_closed               # REQUIRED, must equal that string
decision_log_entry: "v0 bootstrap smoke run"   # optional
```

The toolbox never modifies any field listed in `forbidden_changes` —
the formula, vcov call, and weights argument are built exactly from the
declared values. `failure_policy` must be the literal string
`fail_closed`; any other value aborts before estimation.

## `missing_policy` enum

| Value                 | Semantics |
|-----------------------|-----------|
| `complete_cases`      | Drop any row that has NA in any column listed in `required_columns`. Record the drop count in `drop_reason_summary`. |
| `drop_na_outcome`     | Drop only rows where the dependent variable is NA. Other NAs are passed through to `feols` (which may drop them internally). |
| `fail_if_any_missing` | If any required column contains NA after `sample_filter`, exit nonzero with `[ESTIMATION ERROR] missing_policy is 'fail_if_any_missing' but NA values found in: ...`. |

## `covariance_method` enum

| Value         | Semantics |
|---------------|-----------|
| `clustered`   | Cluster-robust SE via `fixest::feols(..., cluster = ~c1 + c2 + ...)`. `cluster_variables` must be non-empty. |
| `hac`         | Newey-West HAC via `fixest::feols(..., vcov = NW(lag = k) ~ time + unit)`. `hac_settings.enabled` must be `true` and `hac_settings.lag` a positive integer. Panel columns are auto-detected from the data (see `docs/ARCHITECTURE.md` § "HAC code path note"). |
| `iid`         | Classical OLS standard errors via `fixest::feols(..., vcov = "iid")`. No cluster or HAC settings needed. |

## HAC manifest example

To request HAC instead of clustered SEs:

```yaml
specification:
  method_family: panel_fe_regression
  dependent_variable: outcome
  key_regressor_or_treatment: treatment
  controls: [control1, control2]
  fixed_effects: []
  weights: ""
  estimator: fixest_feols
  covariance_method: hac
  cluster_variables: []
  hac_settings:
    enabled: true
    lag: 2
    kernel: "Bartlett"        # informational; fixest NW uses Bartlett internally
```

The data must contain a recognised time column (`time_id`, `time`,
`year`, `period`, or `t`) and ideally a unit column (`unit_id`, `unit`,
`id`, or `i`). Without these, fixest will treat the input as a single
time series in row order and will error on duplicate time indices.

## Exit codes

| Situation                                              | Exit code |
|--------------------------------------------------------|-----------|
| Success: all outputs written, result manifest written  | 0         |
| argv missing or call manifest path not found on disk   | 2         |
| YAML parse failure                                     | 1         |
| Any `[VALIDATION ERROR] ...` from validator            | 1         |
| Any `[ESTIMATION ERROR] ...` from estimator            | 1         |
| Any `[OUTPUT ERROR] ...` from coefficient / summary / manifest writer | 1 |
| Required package (`fixest`, `yaml`, `jsonlite`, `digest`) not installed | 1 |
| Any other uncaught R error                             | 1         |

## Common failure → fix

| Symptom (stderr) | Cause | Fix |
|------------------|-------|-----|
| `[VALIDATION ERROR] required columns missing from data: <name>` | A name in `required_columns` is not a column of the loaded CSV. | Correct the column name in `required_columns` or fix the upstream data file. |
| `[VALIDATION ERROR] fixed_effect variable '<name>' not found in data` | A name in `fixed_effects` is not a column of the data. | Correct the FE name; do NOT add a synthetic column from inside the toolbox. |
| `[VALIDATION ERROR] backend_language must be 'R', got: <value>` | `call.backend_language` is missing or not the exact string `R`. | Set `backend_language: R` (capital R, no quotes needed in YAML). |
| `[ESTIMATION ERROR] sample_filter could not be applied: <R parser msg>` | The R expression in `input.sample_filter` does not parse, or references a column not in the data. | Test the expression interactively in R (`subset(df, <filter>)`); ensure all column names referenced exist. |
| `[VALIDATION ERROR] hac_settings.lag must be a positive integer when hac_settings.enabled is true` | HAC is enabled but `lag` is null, zero, or non-integer. | Set `hac_settings.lag: <positive integer>`, e.g. `lag: 2`. |
| `[VALIDATION ERROR] cluster_variables must be non-empty when covariance_method is 'clustered'` | `covariance_method: clustered` was declared but `cluster_variables` is empty. | Add at least one cluster column, or switch `covariance_method` to `iid` or `hac`. |
| `[ESTIMATION ERROR] missing_policy is 'fail_if_any_missing' but NA values found in: <cols>` | Strict missing policy and NAs are present. | Either clean the data upstream, or change `missing_policy` to `complete_cases` / `drop_na_outcome` and re-declare in the spec. |
