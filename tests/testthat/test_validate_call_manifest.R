# test_validate_call_manifest.R
# Black-box validation tests for the call-manifest validator. All checks invoke
# the entrypoint via Rscript and assert on exit code + stderr content.

# ---------- AC-05 ------------------------------------------------------------
test_that("AC-05 missing_required_variable: nonzero exit when required column absent", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    required_columns = c("unit_id", "time_id", "outcome", "treatment",
                         "control1", "control2", "industry_fe", "cluster_var",
                         "nonexistent_col")
  )
  mf <- write_temp_manifest(m, "ac05")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(
    grepl("VALIDATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("required columns missing", res$stderr, fixed = TRUE) ||
      grepl("required_columns", res$stderr, fixed = TRUE),
    info = paste("stderr was:", res$stderr)
  )
})

# ---------- AC-06 ------------------------------------------------------------
test_that("AC-06 undeclared_fe_variable: nonzero exit when FE column absent in data", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    fixed_effects = list("ghost_fe")
  )
  mf <- write_temp_manifest(m, "ac06")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(
    grepl("VALIDATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("fixed_effect", res$stderr, fixed = TRUE) ||
      grepl("ghost_fe", res$stderr, fixed = TRUE),
    info = paste("stderr was:", res$stderr)
  )
})

# ---------- AC-07 ------------------------------------------------------------
test_that("AC-07 undeclared_cluster_variable: nonzero exit when cluster column absent", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    covariance_method = "clustered",
    cluster_variables = list("ghost_cluster")
  )
  mf <- write_temp_manifest(m, "ac07")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(
    grepl("VALIDATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("cluster variable", res$stderr, fixed = TRUE) ||
      grepl("cluster_variable", res$stderr, fixed = TRUE) ||
      grepl("ghost_cluster", res$stderr, fixed = TRUE),
    info = paste("stderr was:", res$stderr)
  )
})

# ---------- FC-01 ------------------------------------------------------------
test_that("FC-01 missing_data_path_field: nonzero exit when input.data_path empty", {
  td <- withr::local_tempdir()
  m <- make_manifest(data_path = "", out_dir = td)
  mf <- write_temp_manifest(m, "fc01")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
  expect_true(grepl("data_path", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-02 ------------------------------------------------------------
test_that("FC-02 data_file_not_on_disk: nonzero exit when data file does not exist", {
  td <- withr::local_tempdir()
  fake_path <- "/tmp/does_not_exist_12345.csv"
  if (file.exists(fake_path)) {
    # rare race / leftover — replace with another unique path
    fake_path <- tempfile("nonexistent_", fileext = ".csv")
  }
  m <- make_manifest(data_path = fake_path, out_dir = td)
  mf <- write_temp_manifest(m, "fc02")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
  expect_true(grepl(fake_path, res$stderr, fixed = TRUE),
              info = paste("expected path", fake_path, "in stderr; got:", res$stderr))
})

# ---------- FC-03 ------------------------------------------------------------
test_that("FC-03 backend_language_not_r: nonzero exit when backend_language != R", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    backend_language = "Python"
  )
  mf <- write_temp_manifest(m, "fc03")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
  expect_true(grepl("backend_language", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-04 ------------------------------------------------------------
test_that("FC-04 invalid_sample_filter: nonzero exit when sample_filter fails to parse", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    sample_filter = "this_is_not_valid_r_syntax_!!!"
  )
  mf <- write_temp_manifest(m, "fc04")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(
    grepl("ESTIMATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
    info = paste("stderr was:", res$stderr)
  )
  expect_true(grepl("sample_filter", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-05 ------------------------------------------------------------
test_that("FC-05 missing_dependent_variable_field: nonzero exit when dependent_variable empty", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    dependent_variable = ""
  )
  mf <- write_temp_manifest(m, "fc05")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-06 ------------------------------------------------------------
test_that("FC-06 fe_var_not_in_data: nonzero exit when FE var missing from data", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    fixed_effects = list("missing_column")
  )
  mf <- write_temp_manifest(m, "fc06")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-07 ------------------------------------------------------------
test_that("FC-07 cluster_var_not_in_data: nonzero exit when cluster var missing from data", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    covariance_method = "clustered",
    cluster_variables = list("missing_column")
  )
  mf <- write_temp_manifest(m, "fc07")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-08 ------------------------------------------------------------
test_that("FC-08 result_manifest_write_failure: nonzero exit when output dir not writable", {
  td <- withr::local_tempdir()
  m <- make_manifest(data_path = toy_data_path(), out_dir = td)
  # Override outputs to point at an unwritable / nonexistent parent dir.
  bad_parent <- "/nonexistent_dir_xyz_12345/result.yaml"
  m$outputs$result_manifest_path <- bad_parent
  m$outputs$coefficient_table_path <- "/nonexistent_dir_xyz_12345/coefs.csv"
  m$outputs$model_summary_path <- "/nonexistent_dir_xyz_12345/summary.txt"
  mf <- write_temp_manifest(m, "fc08")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(
    grepl("OUTPUT ERROR", res$stderr, fixed = TRUE) ||
      grepl("result manifest", res$stderr, fixed = TRUE) ||
      grepl("VALIDATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("nonexistent_dir", res$stderr, fixed = TRUE),
    info = paste("stderr was:", res$stderr)
  )
})

# ---------- FC-09 (environment-dependent) ------------------------------------
test_that("FC-09 fixest_not_installed: behavior when fixest absent (env-dependent)", {
  # fixest IS installed in this environment; we cannot uninstall it. Mark as
  # skip rather than assert. The presence of the runtime check is exercised
  # indirectly by successful smoke runs that read package_versions.
  skip_if(requireNamespace("fixest", quietly = TRUE),
          message = "fixest is installed; cannot exercise absence-of-package guard without uninstalling.")
  # If somehow fixest is absent, run a valid manifest and expect failure.
  td <- withr::local_tempdir()
  m <- make_manifest(data_path = toy_data_path(), out_dir = td)
  mf <- write_temp_manifest(m, "fc09")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("fixest", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-10 ------------------------------------------------------------
test_that("FC-10 clustered_no_cluster_vars: nonzero exit when clustered but empty cluster list", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    covariance_method = "clustered",
    cluster_variables = list()
  )
  mf <- write_temp_manifest(m, "fc10")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
  expect_true(grepl("cluster_variables", res$stderr, fixed = TRUE) ||
                grepl("cluster variable", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-11 ------------------------------------------------------------
test_that("FC-11 hac_enabled_null_lag: nonzero exit when HAC enabled but lag null", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    covariance_method = "hac",
    cluster_variables = list(),
    fixed_effects = list(),
    hac_enabled = TRUE,
    hac_lag = NULL
  )
  mf <- write_temp_manifest(m, "fc11")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
  expect_true(grepl("hac_settings.lag", res$stderr, fixed = TRUE) ||
                grepl("hac", res$stderr) && grepl("lag", res$stderr),
              info = paste("stderr was:", res$stderr))
})

# ---------- FC-12 ------------------------------------------------------------
test_that("FC-12 missing_call_id: nonzero exit when call.call_id is empty", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    call_id = ""
  )
  mf <- write_temp_manifest(m, "fc12")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(grepl("VALIDATION ERROR", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
  expect_true(grepl("call_id", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})
