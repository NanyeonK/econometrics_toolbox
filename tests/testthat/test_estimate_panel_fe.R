# test_estimate_panel_fe.R
# Black-box tests of the panel FE estimation entrypoint. All assertions are on
# the files written by Rscript R/run_call_manifest.R.

# ---------- AC-01 ------------------------------------------------------------
test_that("AC-01 toy_manifest_smoke_run: end-to-end run produces all three output files", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr, "stdout:", res$stdout))
  expect_true(file.exists(res$coefficient_table_path))
  expect_gt(file.info(res$coefficient_table_path)$size, 0)
  expect_true(file.exists(res$result_manifest_path))
  expect_gt(file.info(res$result_manifest_path)$size, 0)
  expect_true(file.exists(res$model_summary_path))
  expect_gt(file.info(res$model_summary_path)$size, 0)
  expect_false(grepl("^\\[ERROR\\]", res$stderr),
               info = paste("stderr was:", res$stderr))
  expect_false(grepl("^\\[VALIDATION ERROR\\]", res$stderr),
               info = paste("stderr was:", res$stderr))
})

# ---------- AC-02 ------------------------------------------------------------
test_that("AC-02 coefficient_csv_format: CSV has the expected 7-column schema and is parseable", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  expect_true(file.exists(res$coefficient_table_path))
  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  expect_equal(ncol(coefs), 7L)
  expect_equal(
    colnames(coefs),
    c("term", "estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high")
  )
  expect_gte(nrow(coefs), 1L)
  for (nm in c("estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high")) {
    expect_true(is.numeric(coefs[[nm]]),
                info = paste("column", nm, "should be numeric"))
    expect_false(any(is.na(coefs[[nm]])),
                 info = paste("column", nm, "has NA values"))
  }
  # Key regressor declared in fixture B is "treatment".
  expect_true("treatment" %in% coefs$term,
              info = paste("term column:", paste(coefs$term, collapse = ",")))
})

# ---------- AC-04 ------------------------------------------------------------
test_that("AC-04 row_counts_recorded: input row counts are present and arithmetically consistent", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  rm <- yaml::read_yaml(res$result_manifest_path)
  fixture_rows <- nrow(read.csv(toy_data_path()))
  expect_true(is.numeric(rm$input$row_count_before_filter))
  expect_equal(as.integer(rm$input$row_count_before_filter), fixture_rows)
  expect_true(is.numeric(rm$input$row_count_used))
  expect_lte(as.integer(rm$input$row_count_used),
             as.integer(rm$input$row_count_before_filter))
  expect_true(is.numeric(rm$input$dropped_row_count))
  expect_equal(
    as.integer(rm$input$dropped_row_count),
    as.integer(rm$input$row_count_before_filter) - as.integer(rm$input$row_count_used)
  )
  # drop_reason_summary may be present-but-empty list or non-empty list.
  expect_true(is.list(rm$input$drop_reason_summary) ||
                is.character(rm$input$drop_reason_summary) ||
                is.null(rm$input$drop_reason_summary))
})

# ---------- DT-01 ------------------------------------------------------------
test_that("DT-01 coefficient_csv_determinism: two runs produce byte-identical coefficient CSV", {
  res1 <- run_toy_smoke()
  expect_equal(res1$exit_code, 0)
  res2 <- run_toy_smoke()
  expect_equal(res2$exit_code, 0)
  md5_1 <- tools::md5sum(res1$coefficient_table_path)
  md5_2 <- tools::md5sum(res2$coefficient_table_path)
  names(md5_1) <- NULL
  names(md5_2) <- NULL
  expect_equal(md5_1, md5_2)
  # Also: full file contents identical.
  b1 <- readBin(res1$coefficient_table_path, what = "raw",
                n = file.info(res1$coefficient_table_path)$size)
  b2 <- readBin(res2$coefficient_table_path, what = "raw",
                n = file.info(res2$coefficient_table_path)$size)
  expect_true(identical(b1, b2))
})

# ---------- MP-01 ------------------------------------------------------------
test_that("MP-01 complete_cases_policy: drops rows with any NA in required columns", {
  td <- withr::local_tempdir()
  # Copy toy panel and inject 3 NAs into control1.
  data_path <- copy_toy_panel(td, mutator = function(d) {
    d$control1[c(1, 5, 12)] <- NA_real_
    d
  })
  m <- make_manifest(
    data_path = data_path,
    out_dir = td,
    missing_policy = "complete_cases"
  )
  mf <- write_temp_manifest(m, "mp01")
  res <- run_entrypoint(mf)
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  rm <- yaml::read_yaml(m$outputs$result_manifest_path)
  expect_equal(
    as.integer(rm$input$row_count_used),
    as.integer(rm$input$row_count_before_filter) - 3L
  )
  expect_equal(as.integer(rm$input$dropped_row_count), 3L)
  drop_reason_blob <- paste(unlist(rm$input$drop_reason_summary), collapse = " ")
  expect_true(grepl("complete_cases", drop_reason_blob, fixed = TRUE),
              info = paste("drop_reason_summary:", drop_reason_blob))
})

# ---------- MP-02 ------------------------------------------------------------
test_that("MP-02 drop_na_outcome_policy: drops rows where outcome is NA", {
  td <- withr::local_tempdir()
  data_path <- copy_toy_panel(td, mutator = function(d) {
    d$outcome[c(3, 17)] <- NA_real_
    d
  })
  m <- make_manifest(
    data_path = data_path,
    out_dir = td,
    missing_policy = "drop_na_outcome"
  )
  mf <- write_temp_manifest(m, "mp02")
  res <- run_entrypoint(mf)
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  rm <- yaml::read_yaml(m$outputs$result_manifest_path)
  expect_equal(as.integer(rm$input$dropped_row_count), 2L)
  drop_reason_blob <- paste(unlist(rm$input$drop_reason_summary), collapse = " ")
  expect_true(grepl("drop_na_outcome", drop_reason_blob, fixed = TRUE),
              info = paste("drop_reason_summary:", drop_reason_blob))
})

# ---------- MP-03 ------------------------------------------------------------
test_that("MP-03 fail_if_any_missing_policy: nonzero exit when any NA present", {
  td <- withr::local_tempdir()
  data_path <- copy_toy_panel(td, mutator = function(d) {
    d$control1[c(1, 5, 12)] <- NA_real_
    d
  })
  m <- make_manifest(
    data_path = data_path,
    out_dir = td,
    missing_policy = "fail_if_any_missing"
  )
  mf <- write_temp_manifest(m, "mp03")
  res <- run_entrypoint(mf)
  expect_gt(res$exit_code, 0)
  expect_true(
    grepl("ESTIMATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("VALIDATION ERROR", res$stderr, fixed = TRUE) ||
      grepl("fail_if_any_missing", res$stderr, fixed = TRUE),
    info = paste("stderr was:", res$stderr)
  )
  expect_true(grepl("fail_if_any_missing", res$stderr, fixed = TRUE),
              info = paste("stderr was:", res$stderr))
})

# ---------- HC-01 ------------------------------------------------------------
test_that("HC-01 hac_run: HAC manifest runs successfully with lag=2", {
  td <- withr::local_tempdir()
  m <- make_manifest(
    data_path = toy_data_path(),
    out_dir = td,
    covariance_method = "hac",
    cluster_variables = list(),
    fixed_effects = list(),
    hac_enabled = TRUE,
    hac_lag = 2L,
    hac_kernel = "Bartlett"
  )
  mf <- write_temp_manifest(m, "hc01")
  res <- run_entrypoint(mf)
  expect_equal(res$exit_code, 0,
               info = paste("stderr:", res$stderr))
  expect_true(file.exists(m$outputs$coefficient_table_path))
  coefs <- read.csv(m$outputs$coefficient_table_path, stringsAsFactors = FALSE)
  expect_equal(ncol(coefs), 7L)
  expect_equal(
    colnames(coefs),
    c("term", "estimate", "std_error", "statistic", "p_value", "conf_low", "conf_high")
  )
  rm <- yaml::read_yaml(m$outputs$result_manifest_path)
  expect_equal(rm$model$covariance_method, "hac")
  expect_true(isTRUE(rm$model$hac_settings$enabled))
  expect_equal(as.integer(rm$model$hac_settings$lag), 2L)
})

# ---------- FG-02 ------------------------------------------------------------
test_that("FG-02 coef_csv_declared_terms_only: CSV term column only contains declared terms", {
  res <- run_toy_smoke()
  expect_equal(res$exit_code, 0)
  coefs <- read.csv(res$coefficient_table_path, stringsAsFactors = FALSE)
  terms <- coefs$term
  # Required: treatment, control1, control2 appear.
  expect_true("treatment" %in% terms)
  expect_true("control1" %in% terms)
  expect_true("control2" %in% terms)
  # industry_fe absorbed as FE — must not appear as its own coefficient row.
  # (fixest may produce levels like "industry_feB" if it were not absorbed.)
  expect_false("industry_fe" %in% terms)
  expect_false(any(grepl("^industry_fe", terms)),
               info = paste("terms:", paste(terms, collapse = ",")))
  # Other data columns that are NOT declared (unit_id, time_id, cluster_var,
  # weight_var) must not appear as coefficient terms.
  forbidden <- c("unit_id", "time_id", "cluster_var", "weight_var")
  for (f in forbidden) {
    expect_false(f %in% terms,
                 info = paste("forbidden term", f, "present in coefficient CSV"))
  }
  # (Intercept) is acceptable per spec note; we do not assert presence/absence.
})
