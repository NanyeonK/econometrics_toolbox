# test_result_manifest.R
# Black-box tests of the result-manifest YAML emitted by the entrypoint.

# Convenience: load committed toy call manifest for FG-01 comparison.
load_toy_call_manifest <- function() {
  yaml::read_yaml(file.path(econ_repo_root(), "examples",
                            "toy_panel_call_manifest.yaml"))
}

# ---------- AC-03 ------------------------------------------------------------
test_that("AC-03 result_manifest_versions: R version and package versions are recorded", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_true(is.character(rm$call$r_version))
  expect_true(nzchar(rm$call$r_version))
  pv <- rm$call$package_versions
  for (pkg in c("fixest", "sandwich", "lmtest")) {
    expect_true(!is.null(pv[[pkg]]) && nzchar(as.character(pv[[pkg]])),
                info = paste("package_versions.", pkg, "should be non-empty"))
  }
  expect_true(!is.null(pv$broom),
              info = "package_versions.broom field must exist (value may be 'not_installed')")
})

# ---------- AC-08 ------------------------------------------------------------
test_that("AC-08 forbidden_changes_immutability: result manifest mirrors call manifest spec", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  cm <- load_toy_call_manifest()
  # Compare ordered lists by converting both sides via unlist().
  expect_identical(unlist(rm$model$controls), unlist(cm$specification$controls))
  expect_identical(unlist(rm$model$fixed_effects), unlist(cm$specification$fixed_effects))
  expect_identical(unlist(rm$model$cluster_variables),
                   unlist(cm$specification$cluster_variables))
  # Coefficient CSV must only contain declared regressor terms.
  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  declared <- c(cm$specification$key_regressor_or_treatment,
                unlist(cm$specification$controls))
  for (t in coefs$term) {
    if (t == "(Intercept)") next
    expect_true(t %in% declared,
                info = paste("coefficient term", t, "not in declared regressors:",
                             paste(declared, collapse = ",")))
  }
})

# ---------- SM-01 ------------------------------------------------------------
test_that("SM-01 result_manifest_schema_compliance: all required top-level/section keys present", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_equal(rm$template, "econometrics_result_manifest")

  call_keys <- c("call_id", "call_manifest_path", "backend_language",
                 "r_entrypoint", "r_version", "package_versions")
  for (k in call_keys) {
    expect_true(k %in% names(rm$call),
                info = paste("call section missing key", k))
  }

  input_keys <- c("data_path", "data_hash", "row_count_before_filter",
                  "row_count_used", "dropped_row_count", "drop_reason_summary")
  for (k in input_keys) {
    expect_true(k %in% names(rm$input),
                info = paste("input section missing key", k))
  }

  model_keys <- c("method_family", "estimator", "exact_formula",
                  "dependent_variable", "key_regressor_or_treatment", "controls",
                  "fixed_effects", "weights", "covariance_method",
                  "cluster_variables", "hac_settings")
  for (k in model_keys) {
    expect_true(k %in% names(rm$model),
                info = paste("model section missing key", k))
  }

  output_keys <- c("coefficient_table_path", "model_summary_path",
                   "diagnostics_path", "log_path")
  for (k in output_keys) {
    expect_true(k %in% names(rm$outputs),
                info = paste("outputs section missing key", k))
  }

  expect_true("warnings" %in% names(rm))

  drift_keys <- c("sample_signature", "spec_signature", "inference_signature",
                  "preprocessing_signature")
  for (k in drift_keys) {
    expect_true(k %in% names(rm$drift_metadata),
                info = paste("drift_metadata missing key", k))
  }

  expect_equal(rm$verdict, "PASS")
  expect_true(length(rm$blocking_reasons) == 0,
              info = paste("blocking_reasons:",
                           paste(unlist(rm$blocking_reasons), collapse = ",")))
})

# ---------- SM-02 ------------------------------------------------------------
test_that("SM-02 drift_signatures_populated: all four signatures are 64-char lowercase hex", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  sigs <- rm$drift_metadata
  for (k in c("sample_signature", "spec_signature",
              "inference_signature", "preprocessing_signature")) {
    s <- sigs[[k]]
    expect_true(is.character(s) && length(s) == 1L && nzchar(s),
                info = paste(k, "should be a non-empty string; got:",
                             paste(s, collapse = ",")))
    expect_equal(nchar(s), 64L, info = paste(k, "should be 64 chars; got:", s))
    expect_true(grepl("^[0-9a-f]+$", s),
                info = paste(k, "should be lowercase hex; got:", s))
  }
})

# ---------- DT-02 ------------------------------------------------------------
test_that("DT-02 drift_signature_determinism: signatures identical across two runs", {
  res1 <- run_toy_smoke()
  expect_equal(res1$exit_code, 0)
  res2 <- run_toy_smoke()
  expect_equal(res2$exit_code, 0)
  rm1 <- yaml::read_yaml(res1$result_manifest_path)
  rm2 <- yaml::read_yaml(res2$result_manifest_path)
  for (k in c("sample_signature", "spec_signature",
              "inference_signature", "preprocessing_signature")) {
    expect_equal(rm1$drift_metadata[[k]], rm2$drift_metadata[[k]],
                 info = paste("signature mismatch for", k))
  }
})

# ---------- FG-01 ------------------------------------------------------------
test_that("FG-01 model_mirrors_spec: result manifest model section mirrors call manifest spec", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  cm <- load_toy_call_manifest()
  rm <- yaml::read_yaml(res$result_manifest_path)
  expect_equal(rm$model$dependent_variable, cm$specification$dependent_variable)
  expect_equal(rm$model$key_regressor_or_treatment,
               cm$specification$key_regressor_or_treatment)
  expect_identical(unlist(rm$model$controls), unlist(cm$specification$controls))
  expect_identical(unlist(rm$model$fixed_effects), unlist(cm$specification$fixed_effects))
  expect_equal(rm$model$covariance_method, cm$specification$covariance_method)
  expect_identical(unlist(rm$model$cluster_variables),
                   unlist(cm$specification$cluster_variables))
  # weights may be "" in both
  expect_equal(as.character(rm$model$weights %||% ""),
               as.character(cm$specification$weights %||% ""))
  expect_equal(isTRUE(rm$model$hac_settings$enabled),
               isTRUE(cm$specification$hac_settings$enabled))
})

# ---------- VR-01 ------------------------------------------------------------
test_that("VR-01 r_version_recorded: call.r_version is a valid R version string", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  v <- rm$call$r_version
  expect_true(is.character(v) && nzchar(v))
  expect_true(grepl("R version", v, fixed = TRUE),
              info = paste("r_version was:", v))
  # Loose version-id check: contains digit.digit pattern.
  expect_true(grepl("[0-9]+\\.[0-9]+", v),
              info = paste("r_version was:", v))
})

# ---------- VR-02 ------------------------------------------------------------
test_that("VR-02 fixest_version_recorded: package_versions.fixest matches installed version", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  v <- as.character(rm$call$package_versions$fixest)
  expect_true(nzchar(v))
  expect_true(grepl("^[0-9]+\\.[0-9]+", v),
              info = paste("fixest version was:", v))
  # Compare against this test runner's installed version.
  skip_if_not_installed("fixest")
  installed <- as.character(packageVersion("fixest"))
  expect_equal(v, installed)
})
