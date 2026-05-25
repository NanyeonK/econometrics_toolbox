# econometricstoolbox 0.1.2 (development)

## New features (backward-compatible)

- Added two new `method_family` values: `did` (4 active variants) and
  `event_study` (2 variants). Existing `panel_fe_regression` manifests
  continue to work unchanged.
- DiD variants:
  - `did_variant: twfe`              — `fixest::feols(y ~ treatment_indicator | unit + time)`
  - `did_variant: sun_abraham`       — `fixest::sunab(cohort, time_to_treat)`, reports aggregate ATT plus dynamics
  - `did_variant: callaway_santanna` — `did::att_gt()` + `did::aggte(type="simple")`
  - `did_variant: bjs`               — `didimputation::did_imputation()`
- Event-study variants:
  - `event_study_variant: classical`    — `fixest::feols(y ~ i(time_to_treat, ref=-1) | unit + time)`
  - `event_study_variant: sun_abraham`  — `fixest::sunab(...)` dynamic-only
- Schema bumps: both call manifest and result manifest now declare `version: v2`.
- Result manifest gains optional top-level `did_results` and `event_study_results`
  sections (populated only for the relevant method_family).
- Coefficient CSV gains optional `group`, `time`, `event_time` columns (NA-padded
  where not applicable).

## New dependencies (Imports)

- `did` (Callaway-Sant'Anna)
- `DIDmultiplegt` (dCdH; currently disabled — see known issues)
- `didimputation` (BJS)

## Known issues

- `did_variant: dchd` is DISABLED at the dispatcher level. Upstream package
  `DIDmultiplegt` v2.1.0 returns NaN for the static effect even on its bundled
  `wagepan_mgt` example (likely a dplyr 1.2.1 NA-propagation interaction in
  the package's internal `did_multiplegt_transform`). Wrapper code is retained
  at `R/estimate_did_dchd.R` for re-enablement once upstream ships a fix.
  Running a manifest with `did_variant: dchd` exits 1 with a clear pointer
  to this entry. Track via NEWS.md.
- Some DiD variants (e.g., callaway_santanna) have no notion of `cluster_variables`
  in the v0/v1-1 sense; their analytic SE is reported as supplied by the
  underlying package.

# econometricstoolbox 0.1.1 (development)

## Breaking changes

- HAC call manifests now require an explicit `specification.panel` block with
  `unit` and `time` field names. The v0 heuristic that auto-detected panel
  columns by conventional names (`unit_id|unit|id|i` / `time_id|time|year|period|t`)
  has been removed for the HAC code path. Update existing HAC manifests to add:

  ```yaml
  specification:
    panel:
      unit: <column name>
      time: <column name>
  ```

## Other changes

- (none in v1-1; doc-only updates accompany the schema/validator/estimator change)

# econometricstoolbox 0.1.0

- Initial v0 release. Deterministic, manifest-driven R backend for panel
  fixed-effects regression. Supports clustered SE, HAC SE (with column
  auto-detect — see 0.1.1 breaking change), and IID SE. R CMD INSTALL-safe
  as of the `fix(install)` patch (dual-mode Rscript / package source guard).
