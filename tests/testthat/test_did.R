# test_did.R
# Black-box tests for the DiD estimator dispatcher (v1-2).
# Covers: twfe, sun_abraham, callaway_santanna, dchd (DISABLED), bjs
# All assertions go through Rscript R/run_call_manifest.R; no direct R/* calls.

# -------- Shared helper: run a committed example manifest, redirecting outputs
# into a tempdir so we don't depend on the on-repo output/ directory's state.
run_did_example <- function(example_name, out_prefix) {
  td <- tempfile("did_smoke_")
  dir.create(td, recursive = TRUE)
  src <- file.path(econ_repo_root(), "examples", example_name)
  m <- yaml::read_yaml(src)
  m$outputs$result_manifest_path <-
    file.path(td, paste0(out_prefix, "_result_manifest.yaml"))
  m$outputs$coefficient_table_path <-
    file.path(td, paste0(out_prefix, "_coefficients.csv"))
  m$outputs$model_summary_path <-
    file.path(td, paste0(out_prefix, "_model_summary.txt"))
  mf <- write_temp_manifest(m, paste0("did_", out_prefix))
  res <- run_entrypoint(mf)
  res$out_dir <- td
  res$manifest_path <- mf
  res$result_manifest_path <- m$outputs$result_manifest_path
  res$coefficient_table_path <- m$outputs$coefficient_table_path
  res$model_summary_path <- m$outputs$model_summary_path
  res
}

# Shared helper: mutate a committed example, write to a temp path, run.
run_mutated_example <- function(example_name, mutator, label = "mutated") {
  td <- tempfile(paste0("did_", label, "_"))
  dir.create(td, recursive = TRUE)
  src <- file.path(econ_repo_root(), "examples", example_name)
  m <- yaml::read_yaml(src)
  m$outputs$result_manifest_path <-
    file.path(td, paste0(label, "_result_manifest.yaml"))
  m$outputs$coefficient_table_path <-
    file.path(td, paste0(label, "_coefficients.csv"))
  m$outputs$model_summary_path <-
    file.path(td, paste0(label, "_model_summary.txt"))
  m <- mutator(m)
  mf <- write_temp_manifest(m, paste0("did_", label))
  res <- run_entrypoint(mf)
  res$manifest_path <- mf
  res
}

# Required coefficient-CSV columns for v2 (7 standard + 3 panel/event columns).
expected_coef_cols_v2 <- c(
  "term", "estimate", "std_error", "statistic", "p_value",
  "conf_low", "conf_high", "group", "time", "event_time"
)

# ============================================================================
# DiD variant 1: twfe
# ============================================================================
test_that("T-DID-TWFE-HAPPY: twfe manifest runs, emits valid coef CSV + result YAML", {
  res <- run_did_example("toy_did_twfe_call_manifest.yaml", "twfe")
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  expect_true(file.exists(res$coefficient_table_path))
  expect_true(file.exists(res$result_manifest_path))
  expect_true(file.exists(res$model_summary_path))

  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  expect_equal(colnames(coefs), expected_coef_cols_v2)
  expect_true("treatment" %in% coefs$term,
              info = paste("term col:", paste(coefs$term, collapse = ",")))

  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_equal(as.character(rm$version), "v2")
  expect_equal(rm$model$method_family, "did")
  expect_false(is.null(rm$did_results),
               info = "did_results block missing")
  expect_equal(rm$did_results$did_variant, "twfe")
  expect_true(is.numeric(rm$did_results$att_overall) &&
                is.finite(rm$did_results$att_overall),
              info = "att_overall must be a finite number")
  # group_time_att_path is intentionally empty for twfe (simple aggregation).
  expect_true(is.null(rm$did_results$group_time_att_path) ||
                identical(rm$did_results$group_time_att_path, ""),
              info = "group_time_att_path should be empty for twfe")
})

test_that("T-DID-TWFE-FAIL-MISMATCH: panel_fe_regression + did_variant rejected", {
  res <- run_mutated_example(
    "toy_did_twfe_call_manifest.yaml",
    function(m) {
      m$specification$method_family <- "panel_fe_regression"
      m
    },
    label = "twfe_mismatch"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("mismatch", res$stderr, fixed = TRUE) ||
                grepl("did_variant", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

test_that("T-DID-TWFE-FAIL-MISSING-TREATMENT-IND: twfe without treatment_indicator rejected", {
  res <- run_mutated_example(
    "toy_did_twfe_call_manifest.yaml",
    function(m) {
      m$specification$treatment_indicator <- ""
      m
    },
    label = "twfe_no_treat_ind"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("treatment_indicator", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

# ============================================================================
# DiD variant 2: sun_abraham
# ============================================================================
test_that("T-DID-SUNAB-HAPPY: sun_abraham manifest runs, emits did_results with variant=sun_abraham", {
  res <- run_did_example("toy_did_sunab_call_manifest.yaml", "sunab")
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  expect_true(file.exists(res$coefficient_table_path))
  expect_true(file.exists(res$result_manifest_path))

  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  expect_equal(colnames(coefs), expected_coef_cols_v2)
  # Sun-Abraham emits an att_overall row plus cohort-x-event-time rows (att_c..k..).
  any_overall_or_cohort <- any(grepl("^att_overall$", coefs$term)) ||
    any(grepl("^att_", coefs$term))
  expect_true(any_overall_or_cohort,
              info = paste("term col:", paste(coefs$term, collapse = ",")))

  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_equal(as.character(rm$version), "v2")
  expect_equal(rm$model$method_family, "did")
  expect_false(is.null(rm$did_results),
               info = "did_results block missing")
  expect_equal(rm$did_results$did_variant, "sun_abraham")
  expect_equal(rm$did_results$aggregation, "cohort")
})

test_that("T-DID-SUNAB-FAIL-MISSING-COHORT: sun_abraham without cohort_var rejected", {
  res <- run_mutated_example(
    "toy_did_sunab_call_manifest.yaml",
    function(m) {
      m$specification$cohort_var <- ""
      m
    },
    label = "sunab_no_cohort"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("cohort_var", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

# ============================================================================
# DiD variant 3: callaway_santanna
# ============================================================================
test_that("T-DID-CS-HAPPY: callaway_santanna manifest runs, emits group-time ATTs", {
  res <- run_did_example("toy_did_cs_call_manifest.yaml", "cs")
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  expect_true(file.exists(res$coefficient_table_path))
  expect_true(file.exists(res$result_manifest_path))

  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  expect_equal(colnames(coefs), expected_coef_cols_v2)
  # Expect at least one group-time ATT row: ^att_g\d+_t\d+$
  expect_true(any(grepl("^att_g[0-9]+_t[0-9]+$", coefs$term)),
              info = paste("term col:", paste(coefs$term, collapse = ",")))

  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_equal(as.character(rm$version), "v2")
  expect_equal(rm$model$method_family, "did")
  expect_false(is.null(rm$did_results),
               info = "did_results block missing")
  # The CS block keys (estimator and att_overall present, control_group present).
  expect_true(is.numeric(rm$did_results$att_overall) &&
                is.finite(rm$did_results$att_overall),
              info = "att_overall must be a finite number")
  expect_true(!is.null(rm$did_results$control_group) &&
                nzchar(as.character(rm$did_results$control_group)),
              info = "control_group must be recorded")
})

test_that("T-DID-CS-FAIL-MISSING-COHORT: callaway_santanna without cohort_var rejected", {
  res <- run_mutated_example(
    "toy_did_cs_call_manifest.yaml",
    function(m) {
      m$specification$cohort_var <- ""
      m
    },
    label = "cs_no_cohort"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("cohort_var", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

test_that("T-DID-CS-FAIL-BAD-COHORT-COL: callaway_santanna with nonexistent cohort col rejected", {
  res <- run_mutated_example(
    "toy_did_cs_call_manifest.yaml",
    function(m) {
      m$specification$cohort_var <- "nonexistent_col"
      m
    },
    label = "cs_bad_cohort_col"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("nonexistent_col", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

test_that("T-DID-CS-FAIL-BAD-CONTROL-GROUP: callaway_santanna with bogus control_group fails", {
  res <- run_mutated_example(
    "toy_did_cs_call_manifest.yaml",
    function(m) {
      m$specification$control_group <- "bogus_value"
      m
    },
    label = "cs_bad_ctrl"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("control_group", res$stderr, fixed = TRUE) ||
                grepl("enum", res$stderr, fixed = TRUE) ||
                grepl("bogus_value", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

# ============================================================================
# DiD variant 4: dchd (DISABLED at dispatcher due to upstream bug)
# ============================================================================
# This test ASSERTS the disabled-exit path. dchd is gated at the dispatcher
# with a documented error pointing to NEWS.md v0.1.2 known issues. The test
# passes when the run exits nonzero AND the stderr contains the documented
# "DISABLED" message — it is NOT skipped.
test_that("T-DID-DCHD-DISABLED: dchd dispatcher exits nonzero with documented DISABLED message", {
  res <- run_did_example("toy_did_dchd_call_manifest.yaml", "dchd")
  expect_true(res$exit_code > 0,
              info = paste("expected nonzero exit; stderr:", res$stderr))
  expect_true(grepl("dchd' is currently DISABLED", res$stderr, fixed = TRUE),
              info = paste("expected documented disabled-message; stderr:",
                           res$stderr))
})

# ============================================================================
# DiD variant 5: bjs
# ============================================================================
test_that("T-DID-BJS-HAPPY: bjs manifest runs, emits did_results block", {
  res <- run_did_example("toy_did_bjs_call_manifest.yaml", "bjs")
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  expect_true(file.exists(res$coefficient_table_path))
  expect_true(file.exists(res$result_manifest_path))

  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  expect_equal(colnames(coefs), expected_coef_cols_v2)
  expect_gte(nrow(coefs), 1L)

  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_equal(as.character(rm$version), "v2")
  expect_equal(rm$model$method_family, "did")
  expect_false(is.null(rm$did_results),
               info = "did_results block missing")
  # bjs records either `att_overall` or `att` (per spec). Accept either.
  has_att <- (!is.null(rm$did_results$att_overall) &&
                is.numeric(rm$did_results$att_overall) &&
                is.finite(rm$did_results$att_overall)) ||
    (!is.null(rm$did_results$att) &&
       is.numeric(rm$did_results$att) &&
       is.finite(rm$did_results$att))
  expect_true(has_att,
              info = "did_results must contain a finite att or att_overall")
})

test_that("T-DID-BJS-FAIL-MISSING-COHORT: bjs without cohort_var rejected", {
  res <- run_mutated_example(
    "toy_did_bjs_call_manifest.yaml",
    function(m) {
      m$specification$cohort_var <- ""
      m
    },
    label = "bjs_no_cohort"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("cohort_var", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

# ============================================================================
# Cross-cutting validator tests
# ============================================================================
test_that("T-DID-FAIL-MISSING-VARIANT: did family without did_variant rejected", {
  res <- run_mutated_example(
    "toy_did_twfe_call_manifest.yaml",
    function(m) {
      m$specification$did_variant <- NULL
      m
    },
    label = "did_no_variant"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("did_variant", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

test_that("T-DID-FAIL-BOGUS-VARIANT: did family with unknown did_variant rejected", {
  res <- run_mutated_example(
    "toy_did_twfe_call_manifest.yaml",
    function(m) {
      m$specification$did_variant <- "foobar"
      m
    },
    label = "did_bogus_variant"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("did_variant", res$stderr, fixed = TRUE) ||
                grepl("enum", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

test_that("T-MISMATCH-PANEL-WITH-DID-VARIANT: panel_fe with did_variant rejected", {
  res <- run_mutated_example(
    "toy_did_twfe_call_manifest.yaml",
    function(m) {
      m$specification$method_family <- "panel_fe_regression"
      m
    },
    label = "panel_with_did_variant"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("mismatch", res$stderr, fixed = TRUE) ||
                grepl("did_variant", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})

test_that("T-METHOD-FAMILY-UNKNOWN: unknown method_family rejected", {
  res <- run_mutated_example(
    "toy_did_twfe_call_manifest.yaml",
    function(m) {
      m$specification$method_family <- "not_a_family"
      m
    },
    label = "unknown_family"
  )
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
  expect_true(grepl("method_family", res$stderr, fixed = TRUE),
              info = paste("stderr:", res$stderr))
})
