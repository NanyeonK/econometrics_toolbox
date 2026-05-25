# helper-econ.R
# Shared helpers for econometrics_toolbox v0 tester suite.
# Black-box only: no calls into R/*.R internals. We only invoke the entrypoint
# via Rscript and inspect the resulting files.

# Null-coalesce helper used by some assertions.
`%||%` <- function(a, b) if (is.null(a)) b else a

# -------- Locate the target repo root ----------------------------------------
# Tests are run from inside tests/testthat/ by testthat::test_dir() but the
# working directory at the moment of test_that() execution is the testthat dir
# itself. We need the repo root (which contains examples/ and R/).
econ_repo_root <- function() {
  # When test_dir is invoked from repo root: getwd() at helper-load time
  # equals the repo root. testthat sources helpers from the testthat dir, so
  # we walk up two levels from the helper file's path if possible. Fall back
  # to the project's known absolute path.
  # First try environment variable, then fall back to known absolute path.
  env <- Sys.getenv("ECON_REPO_ROOT", "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env))
  }
  candidate <- "/home/nanyeon99/project/econometrics_toolbox"
  if (dir.exists(candidate) &&
      file.exists(file.path(candidate, "R", "run_call_manifest.R"))) {
    return(normalizePath(candidate))
  }
  # Walk up until we find R/run_call_manifest.R
  d <- normalizePath(getwd())
  for (i in 1:6) {
    if (file.exists(file.path(d, "R", "run_call_manifest.R"))) {
      return(d)
    }
    d <- dirname(d)
  }
  stop("Could not locate econometrics_toolbox repo root.")
}

# Path to the entrypoint Rscript file (relative-to-cwd path that the
# entrypoint itself expects: it loads R/* via source() which needs cwd=root).
econ_entrypoint <- function() {
  file.path(econ_repo_root(), "R", "run_call_manifest.R")
}

# Library path for user-installed packages
econ_libs_user <- function() {
  cand <- "/home/nanyeon99/R/x86_64-pc-linux-gnu-library/4.3"
  if (dir.exists(cand)) cand else Sys.getenv("R_LIBS_USER", "")
}

# -------- Run the entrypoint -------------------------------------------------
# Returns a list with: exit_code, stdout, stderr.
# The entrypoint must be invoked with the repo root as cwd so that its
# internal source("R/..." ) calls resolve correctly.
run_entrypoint <- function(manifest_path,
                           cwd = econ_repo_root(),
                           extra_env = character()) {
  out_file <- tempfile("econstdout_", fileext = ".txt")
  err_file <- tempfile("econstderr_", fileext = ".txt")
  on.exit({
    if (file.exists(out_file)) unlink(out_file)
    if (file.exists(err_file)) unlink(err_file)
  }, add = TRUE)

  env <- c(
    paste0("R_LIBS_USER=", econ_libs_user()),
    extra_env
  )

  old_wd <- getwd()
  setwd(cwd)
  on.exit(setwd(old_wd), add = TRUE)

  ec <- suppressWarnings(system2(
    "Rscript",
    args = c(shQuote(econ_entrypoint()), shQuote(manifest_path)),
    stdout = out_file,
    stderr = err_file,
    env = env
  ))

  list(
    exit_code = ec,
    stdout = if (file.exists(out_file)) paste(readLines(out_file, warn = FALSE), collapse = "\n") else "",
    stderr = if (file.exists(err_file)) paste(readLines(err_file, warn = FALSE), collapse = "\n") else ""
  )
}

# -------- Manifest construction helpers --------------------------------------
# Build a fresh call manifest as an R list, defaulting to a valid spec that
# targets the toy fixture. Tests can override individual fields by passing a
# list to `overrides` (deep-merged at the top level / nested via $).
toy_data_path <- function() {
  file.path(econ_repo_root(), "examples", "toy_panel.csv")
}

# Build a valid manifest list that points at an arbitrary data_path and an
# arbitrary output directory (typically inside tempdir()).
make_manifest <- function(data_path,
                          out_dir,
                          required_columns = c("unit_id", "time_id", "outcome",
                                               "treatment", "control1", "control2",
                                               "industry_fe", "cluster_var"),
                          dependent_variable = "outcome",
                          key_regressor = "treatment",
                          controls = c("control1", "control2"),
                          fixed_effects = list("industry_fe"),
                          weights = "",
                          covariance_method = "clustered",
                          cluster_variables = list("cluster_var"),
                          hac_enabled = FALSE,
                          hac_lag = NULL,
                          hac_kernel = "",
                          missing_policy = "complete_cases",
                          sample_filter = "",
                          backend_language = "R",
                          call_id = "TEST_CALL_001") {
  list(
    template = "econometrics_call_manifest",
    version = "v1",
    updated = "2026-05-25",
    project = "econometrics_toolbox_test",
    phase = "phase_2_pilot",
    owner = "tester",
    call = list(
      call_id = call_id,
      allowed_reason = "test",
      source_empirical_plan = "examples/toy_panel_call_manifest.yaml",
      source_gate_status = "qa/gate_status.yaml",
      source_spec_grid_manifest = "",
      source_methodology_spec = "",
      source_preprocessing_spec = "",
      backend_language = backend_language,
      r_entrypoint = "R/run_call_manifest.R"
    ),
    input = list(
      data_path = data_path,
      data_hash = "",
      row_count_before_filter = NULL,
      required_columns = as.list(required_columns),
      sample_filter = sample_filter,
      missing_policy = missing_policy
    ),
    specification = list(
      method_family = "panel_fe_regression",
      dependent_variable = dependent_variable,
      key_regressor_or_treatment = key_regressor,
      controls = as.list(controls),
      fixed_effects = if (length(fixed_effects) == 0) list() else as.list(fixed_effects),
      weights = weights,
      estimator = "fixest_feols",
      covariance_method = covariance_method,
      cluster_variables = if (length(cluster_variables) == 0) list() else as.list(cluster_variables),
      hac_settings = list(
        enabled = hac_enabled,
        lag = hac_lag,
        kernel = hac_kernel
      )
    ),
    outputs = list(
      result_manifest_path = file.path(out_dir, "result_manifest.yaml"),
      coefficient_table_path = file.path(out_dir, "coefficients.csv"),
      model_summary_path = file.path(out_dir, "model_summary.txt"),
      log_path = ""
    ),
    forbidden_changes = list(
      "outcome", "key_regressor_or_treatment", "sample_filter",
      "preprocessing_path", "controls", "fixed_effects",
      "covariance_method", "cluster_variables"
    ),
    failure_policy = "fail_closed",
    decision_log_entry = "tester run"
  )
}

# Write a manifest list to a temp YAML file. Returns the file path.
write_temp_manifest <- function(manifest_list, name = "manifest") {
  f <- tempfile(paste0(name, "_"), fileext = ".yaml")
  yaml::write_yaml(manifest_list, f)
  f
}

# -------- Toy fixture copy ---------------------------------------------------
# Copy the toy panel to a temp dir and optionally mutate it.
copy_toy_panel <- function(out_dir,
                           mutator = identity,
                           filename = "toy_panel.csv") {
  src <- toy_data_path()
  dat <- read.csv(src, stringsAsFactors = FALSE)
  dat <- mutator(dat)
  dst <- file.path(out_dir, filename)
  write.csv(dat, dst, row.names = FALSE)
  dst
}

# -------- Smoke run once, cached for read-only tests -------------------------
# Many AC/SM/FG/VR tests want to assert on the output files of the toy smoke
# run. To keep them independent and idempotent we just (re)run the entrypoint
# against the toy manifest at the top of each such test, writing into a temp
# output dir so we don't depend on or modify the committed output/.
run_toy_smoke <- function() {
  td <- tempfile("toysmoke_")
  dir.create(td, recursive = TRUE)
  # Build a manifest pointing the OUTPUT to td but reusing the committed
  # data fixture and call_id.
  m <- yaml::read_yaml(file.path(econ_repo_root(), "examples",
                                 "toy_panel_call_manifest.yaml"))
  m$outputs$result_manifest_path <- file.path(td, "toy_result_manifest.yaml")
  m$outputs$coefficient_table_path <- file.path(td, "toy_coefficients.csv")
  m$outputs$model_summary_path <- file.path(td, "toy_model_summary.txt")
  mf <- write_temp_manifest(m, "toy_smoke")
  res <- run_entrypoint(mf)
  res$out_dir <- td
  res$manifest_path <- mf
  res$result_manifest_path <- m$outputs$result_manifest_path
  res$coefficient_table_path <- m$outputs$coefficient_table_path
  res$model_summary_path <- m$outputs$model_summary_path
  res
}
