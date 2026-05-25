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
