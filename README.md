# econometrics_toolbox

Deterministic, manifest-driven R backend for panel fixed-effects regression,
intended to be called by an external empirical execution layer. The toolbox
executes an econometric specification that has already been declared upstream;
it does not decide research design, sample, outcome, treatment, controls,
fixed effects, clustering, or inclusion of robustness checks. v0 is scoped
to panel OLS / fixed-effects regression with clustered or HAC (Newey-West)
standard errors via `fixest::feols`.

v1-2 (0.1.2) extends the backend with difference-in-differences (TWFE,
Sun-Abraham, Callaway-Sant'Anna, BJS) and event-study (classical,
Sun-Abraham) estimators. See `docs/USAGE.md` for per-variant call-manifest
structures. The de Chaisemartin-d'Haultfœuille variant is wired but
currently disabled pending an upstream fix; see `NEWS.md` v0.1.2 known
issues for details.

## What it is

- An R-only execution backend for a single econometric primitive
  (panel fixed-effects regression).
- A fail-closed CLI: any missing variable, missing path, schema violation,
  or undeclared spec change causes a nonzero exit before any output is
  written.
- A deterministic estimator: identical inputs produce byte-identical
  coefficient CSVs (single-threaded `feols`, no timestamped fields in the
  coefficient table).
- An emitter of result manifests carrying four SHA-256 drift signatures
  (sample, spec, inference, preprocessing) for the empirical execution
  layer to classify drift downstream.

## What it is NOT

- Not a general-purpose econometrics package.
- Not the place where research-design decisions are made. The toolbox
  cannot decide outcome, treatment, controls, fixed effects, clustering,
  weights, sample, or whether a robustness check belongs in the paper —
  those belong to the empirical execution layer and the methodology spec.
- Not a presentation layer. The result manifest and coefficient CSV are
  machine artefacts; rendering tables/figures is downstream.
- Not a drift adjudicator. The toolbox emits drift signatures; the
  empirical execution layer classifies them as `NO_DRIFT`,
  `ALLOWED_VARIANT`, `REQUIRES_DECISION_LOG`, or `BLOCKING_DRIFT`.

See `research_system_contract/econometrics_toolbox_contract.md`
(§ "Relationship to empirical execution layer") for the canonical boundary.

## Repository layout

```
econometrics_toolbox/
├── DESCRIPTION                                # R package manifest
├── NAMESPACE                                  # exports
├── README.md                                  # this file
├── PROJECT_PLAN.md                            # read-only v0 scope baseline
├── R/
│   ├── run_call_manifest.R                    # CLI entrypoint
│   ├── validate_call_manifest.R               # fail-closed validator
│   ├── estimate_panel_fe.R                    # feols call + output writes
│   ├── write_result_manifest.R                # result manifest assembler
│   └── drift_metadata.R                       # four SHA-256 signature fns
├── inst/schemas/
│   ├── econometrics_call_manifest.schema.yaml
│   └── econometrics_result_manifest.schema.yaml
├── examples/
│   ├── toy_panel.csv                          # 30-row balanced panel fixture
│   └── toy_panel_call_manifest.yaml           # minimal valid call manifest
├── tests/testthat/                            # testthat suites (32 tests)
├── output/                                    # destination for run artefacts
├── docs/
│   ├── ARCHITECTURE.md                        # pipeline + module map
│   └── USAGE.md                               # call manifest authoring guide
└── research_system_contract/                  # read-only canonical contract
```

The Package field in `DESCRIPTION` is `econometricstoolbox` (no hyphens, per
R convention); the on-disk directory is `econometrics_toolbox`.

## Quick start

Install the runtime R dependencies (CRAN), then run the toy manifest:

```bash
Rscript -e 'install.packages(c("fixest","sandwich","lmtest","yaml","jsonlite","testthat","digest","did","DIDmultiplegt","didimputation"), repos="https://cloud.r-project.org")'
Rscript R/run_call_manifest.R examples/toy_panel_call_manifest.yaml
```

The v0 dependencies (`fixest`, `sandwich`, `lmtest`, `yaml`, `jsonlite`,
`testthat`, `digest`) remain required. v1-2 adds three more as Imports:
`did` (Callaway-Sant'Anna), `DIDmultiplegt` (de Chaisemartin-d'Haultfœuille
— currently disabled at the dispatcher level, see `NEWS.md` v0.1.2), and
`didimputation` (Borusyak-Jaravel-Spiess).

A successful run prints `[SUCCESS] run complete.` to stdout and exits 0.

## Output

Each successful run writes three files under `output/` (paths are taken
verbatim from the call manifest):

- **`toy_coefficients.csv`** — coefficient table with exactly 7 columns:
  `term, estimate, std_error, statistic, p_value, conf_low, conf_high`.
- **`toy_model_summary.txt`** — the R `print(summary(fit))` text dump of
  the fitted `fixest` object.
- **`toy_result_manifest.yaml`** — the result manifest with R version,
  package versions, row counts before / after filter, exact formula,
  four drift signatures (`sample_signature`, `spec_signature`,
  `inference_signature`, `preprocessing_signature`), `verdict: PASS`,
  and `blocking_reasons: []`.

## Tests

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

The suite covers all PROJECT_PLAN acceptance criteria, every fail-closed
condition, determinism (byte-identical CSVs on repeated runs), schema
compliance, version recording, missing-policy variants, and the HAC
covariance variant. The `library(econometrics_toolbox)` line in
`tests/testthat.R` is a builder stub; invoking the suite via
`testthat::test_dir(...)` as shown above does not require a prior
`R CMD INSTALL`.

## v0 method scope

In scope:

- Panel OLS / fixed-effects regression via `fixest::feols`
- Clustered standard errors (one or more cluster variables)
- HAC / Newey-West covariance when explicitly declared
- Optional sample filter and missing-value policy (`complete_cases`,
  `drop_na_outcome`, `fail_if_any_missing`)
- Optional regression weights (one column name)

Out of scope for v0:

- DiD / event-study estimator execution (call schemas may be drafted,
  but the R backend is not implemented)
- Spatial econometrics
- Automatic model selection, estimator zoo interfaces
- Forecasting (owned by `macroforecast`)
- Asset-pricing / portfolio / CTF / backtest (owned by `eapctf`)
- Python econometric estimator wrappers
- Table / figure `FIX` decisions, manuscript prose, claim decisions

## Fail-closed contract

The CLI entrypoint exits nonzero before writing any output when any of
the following hold: the YAML cannot be parsed; `backend_language` is not
exactly `"R"`; `failure_policy` is not `"fail_closed"`; any required
top-level field is missing or empty; the input data path does not exist;
`required_columns`, the dependent variable, the key regressor, any
control, any fixed effect, any cluster variable, or the weights column
is absent from the loaded data; `covariance_method` is not one of
`clustered`, `hac`, `iid`; `cluster_variables` is empty when
`covariance_method == "clustered"`; `hac_settings.lag` is null or
non-positive when `hac_settings.enabled == true`; the sample filter
expression cannot be evaluated; the formula cannot be constructed
exactly as declared; `missing_policy == "fail_if_any_missing"` and a
required column contains NA; or any output write fails. See
`docs/USAGE.md` § "Exit codes" for the full table.

## Canonical contract

The authoritative contract for the toolbox lives at
`research_system_contract/econometrics_toolbox_contract.md`. It is
checked into this repo as a read-only copy of the Mac-owned source of
truth. Any disagreement between this README and the contract is resolved
in favour of the contract.

## License

MIT (see `DESCRIPTION`). A standalone `LICENSE` file is not yet present
in this v0 bootstrap — see the run scribe notes for the flag.

---

Generated as part of the statsclaw v0 bootstrap run
(`2026-05-25-v0-bootstrap`).
