# Econometrics Toolbox Project Plan

Created: 2026-05-25 KST
Target server: server3
Target path: `/home/nanyeon99/project/econometrics_toolbox`

## Purpose

Build the deterministic econometrics backend that can be called by Yeonchan's research-system empirical execution layer.

This is not a general-purpose econometrics package. It is an agent-safe backend for research projects where the empirical object has already been declared in:

- `empirical_plan.yaml`
- `spec_grid_manifest.yaml`
- `qa/gate_status.yaml`
- `specs/main_spec.md`
- `specs/data_spec.md`
- `specs/preprocessing_spec.md`
- `specs/methodology_spec.md`
- `specs/output_spec.md`

The toolbox computes declared econometric specifications. It does not decide the research design.

## Source-of-truth contract

Canonical contract lives in the Mac research system:

```text
/Users/nanyeon/Library/CloudStorage/SynologyDrive-second_brain/research_paper_system/02_workflows/econometrics_toolbox_contract.md
```

Related templates:

```text
/Users/nanyeon/Library/CloudStorage/SynologyDrive-second_brain/research_paper_system/04_templates/econometrics_call_manifest_template.yaml
/Users/nanyeon/Library/CloudStorage/SynologyDrive-second_brain/research_paper_system/04_templates/econometrics_result_manifest_template.yaml
```

Before implementing, confirm the Mac research-system contract has been committed or explicitly provided to the server3 worker.

## Non-negotiable rules

All econometric method code must be written in R.

Allowed non-R roles:

- CLI orchestration
- schema validation
- manifest comparison
- drift audit
- file movement
- package/test harness

Forbidden non-R roles:

- implementing OLS, fixed effects, DiD, event-study, IV, RD, HAC/Newey-West, clustered covariance, or other econometric estimators
- recomputing coefficients outside R
- changing estimator defaults outside the R backend

If a Python or shell wrapper exists, it must call an R entrypoint and treat the R-generated result manifest as source of truth.

## v0 scope

Implement only the minimum backend needed by current research-system projects.

Included:

- R backend entrypoint for panel OLS / fixed-effects regression
- `fixest::feols` as the preferred estimator
- clustered standard errors
- HAC / Newey-West settings only when explicitly declared
- call manifest validation before estimation
- result manifest emission after estimation
- coefficient output as machine-readable CSV or Parquet-compatible CSV
- model summary output
- fail-closed behavior when declared variables, FE, clusters, or paths are missing

Schema-first, execution-later:

- DiD and event-study call schemas may be drafted in v0
- Do not implement DiD/event-study estimators unless the R entrypoint, required fields, and manifest behavior are explicit

Excluded from v0:

- spatial econometrics
- automatic model selection
- estimator zoo interfaces
- forecasting experiment grammar owned by `macroforecast`
- asset-pricing, portfolio, CTF, or backtest grammar owned by `eapctf`
- Python econometric estimator wrappers
- table/figure `FIX` decisions
- manuscript prose or claim decisions

## Suggested repository structure

```text
econometrics_toolbox/
├── PROJECT_PLAN.md
├── README.md
├── DESCRIPTION
├── R/
│   ├── run_call_manifest.R
│   ├── validate_call_manifest.R
│   ├── estimate_panel_fe.R
│   ├── write_result_manifest.R
│   └── drift_metadata.R
├── inst/
│   └── schemas/
│       ├── econometrics_call_manifest.schema.yaml
│       └── econometrics_result_manifest.schema.yaml
├── examples/
│   ├── toy_panel.csv
│   └── toy_panel_call_manifest.yaml
├── tests/
│   └── testthat/
│       ├── test_validate_call_manifest.R
│       ├── test_estimate_panel_fe.R
│       └── test_result_manifest.R
└── output/
    └── .gitkeep
```

Do not scaffold this structure until implementation is explicitly requested. Current task is plan-only.

## Backend behavior

Main entrypoint:

```text
Rscript R/run_call_manifest.R path/to/econometrics_call_manifest.yaml
```

Required behavior:

1. Read call manifest.
2. Validate `backend_language: R`.
3. Validate the R entrypoint and input data path.
4. Check all required columns exist.
5. Apply declared sample filter exactly.
6. Apply declared missing policy exactly.
7. Build the declared formula exactly.
8. Estimate with the declared R backend.
9. Write coefficient table and model summary.
10. Write result manifest.
11. Exit nonzero on any missing field, unsupported option, or undeclared spec change.

The backend must not silently modify:

- sample filter
- dependent variable
- key regressor or treatment
- controls
- fixed effects
- weights
- cluster variables
- HAC settings
- preprocessing path

## Drift metadata

Every result manifest should include signatures for:

- sample
- specification
- inference
- preprocessing input

The toolbox itself does not decide whether drift is acceptable. It only exposes enough metadata for the empirical execution layer to classify:

- `NO_DRIFT`
- `ALLOWED_VARIANT`
- `REQUIRES_DECISION_LOG`
- `BLOCKING_DRIFT`

## R package defaults

Preferred packages:

- `fixest`
- `sandwich`
- `lmtest`
- `yaml`
- `jsonlite`
- `testthat`

Optional:

- `broom`
- `data.table`
- `arrow`, only if Parquet support is explicitly needed and available

Do not depend on Python for econometric estimation.

## Acceptance criteria for v0

The v0 implementation is acceptable only if:

- toy call manifest runs through `Rscript R/run_call_manifest.R`
- coefficients are written to a machine-readable output file
- result manifest records R version and package versions
- row counts before/after filtering are recorded
- missing required variable causes a nonzero exit
- undeclared FE or cluster variable causes a nonzero exit
- changing controls/FE/cluster in output without changing manifest is impossible through normal entrypoint behavior
- tests pass locally on server3

## First worker task

When implementation begins, the worker should:

1. Confirm this path:

   ```text
   /home/nanyeon99/project/econometrics_toolbox
   ```

2. Confirm R and required package availability.
3. Confirm the Mac research-system contract has been committed or copied into the task prompt.
4. Create a minimal R package scaffold.
5. Implement only manifest validation, toy panel FE estimation, and result manifest writing.
6. Add tests for failure behavior.
7. Stop before adding DiD/event-study execution.

## Boundary note

`empiricalkit` / empirical execution layer decides whether econometrics toolbox may run.

Econometrics toolbox only executes declared R methods and returns manifests.
