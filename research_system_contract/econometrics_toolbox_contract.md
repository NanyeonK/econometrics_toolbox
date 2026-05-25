# Econometrics Toolbox Contract

Updated: 2026-05-25

Purpose:
- define the backend contract for econometric estimation called by the empirical execution layer
- keep econometric methods deterministic, manifest-driven, and research-system gated
- separate estimation code from empirical design decisions

This contract is for Yeonchan's research-agent workflow. It is not a general-purpose econometrics package specification.

## Core rule

All econometric method code must be written in R.

Allowed non-R roles:
- orchestration
- schema validation
- manifest comparison
- drift audit
- table/figure map generation
- file movement and packaging

Forbidden non-R roles:
- implementing OLS, fixed effects, DiD, event-study, IV, RD, HAC/Newey-West, clustered covariance, or other econometric estimators
- changing estimator defaults outside the R backend
- recomputing econometric coefficients in Python or shell helpers

If a Python or shell wrapper calls the toolbox, it must call an R script or R package entrypoint and preserve the R-generated result manifest as source of truth.

## Relationship to empirical execution layer

The empirical execution layer decides whether estimation is allowed. The econometrics toolbox only computes an already-declared specification.

Before any toolbox call, the caller must provide:
- `empirical_plan.yaml`
- current `qa/gate_status.yaml`
- relevant spec lock files or mapped equivalents
- a call manifest based on `04_templates/econometrics_call_manifest_template.yaml`

The toolbox returns:
- machine-readable estimates
- an R-generated result manifest based on `04_templates/econometrics_result_manifest_template.yaml`
- logs sufficient to rerun the same call

The toolbox must not decide:
- research question
- sample
- outcome
- treatment, exposure, predictor, or signal
- controls
- fixed effects
- clustering or inference level
- robustness inclusion
- table/figure `FIX` status
- manuscript claims

## v0 method scope

v0 should support only deterministic applied-econometrics primitives needed by current research projects.

Allowed v0 method families:
- panel OLS / fixed-effects regression
- clustered standard errors
- HAC / Newey-West covariance when explicitly declared
- coefficient extraction and model summary manifests
- event-study / DiD call schemas, with execution only when the R backend implementation is explicit and the methodology spec is complete

Preferred R packages:
- `fixest` for fixed-effects regression and clustered covariance
- `sandwich` and `lmtest` for covariance/test routines not covered by `fixest`
- `broom` or project-local extraction helpers for tidy output
- `modelsummary` only for presentation-facing tables, not as the source of econometric truth

Out of v0 scope:
- spatial econometrics
- automatic model selection
- estimator zoo interfaces
- forecasting experiment grammar owned by `macroforecast`
- asset-pricing, portfolio, CTF, or backtest grammar owned by `eapctf`
- Python econometric estimator wrappers

## Call manifest

Every econometrics call must declare:
- call ID
- project
- phase
- allowed reason
- R entrypoint path
- input data path and data hash when available
- spec versions
- dependent variable
- key regressor or treatment
- controls
- fixed effects
- weights
- sample filter
- covariance/inference method
- cluster variables or HAC settings
- output paths
- forbidden changes

If the R entrypoint cannot run the declared spec exactly, it must fail rather than silently changing the spec.

## Result manifest

Every R backend run must emit:
- call ID
- R script or package entrypoint
- R version
- package versions
- input data path and hash when available
- row count used
- dropped row count and reason summary
- exact formula or model call
- fixed effects
- controls
- covariance/inference method
- coefficient table path
- model summary path
- warnings
- drift-relevant metadata

The result manifest is the authoritative bridge from estimation output to `table_figure_map.yaml` and `specs/output_spec.md`.

## Drift discipline

The R backend must expose drift-relevant metadata. A wrapper or agent must compare it against the call manifest and active specs.

Blocking drift includes:
- changed sample or time range
- changed outcome or key regressor
- changed controls, fixed effects, weights, clustering, HAC settings, or estimator
- changed preprocessing path
- dropped rows not explained by the declared missing policy
- package-default changes that affect inference and are not recorded

Allowed differences are only those declared in `spec_grid_manifest.yaml` or a lane contract.

## Failure behavior

The toolbox should fail closed.

Return `BLOCKED` or nonzero exit when:
- required input path is missing
- required variables are missing
- sample filter cannot be applied
- model formula cannot be constructed exactly
- fixed effects or cluster variables are missing
- result manifest cannot be written
- package versions cannot be recorded

The caller must not patch around failure by changing the spec. It must update the empirical plan, methodology spec, or human decision packet.

## Agent instructions

Use R for econometric methods even if a Python package can estimate the same model.

Do not write new econometric estimators in project-local ad hoc scripts unless the script is the declared R backend for the call and emits the required manifest.

Do not let table-generation code become the only place where estimation happens. Estimation output must exist as machine-readable results plus result manifest before a table pack is rendered.
