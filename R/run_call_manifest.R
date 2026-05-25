#!/usr/bin/env Rscript
# run_call_manifest.R
#
# CLI entrypoint for the econometrics toolbox.
#
# Pipeline:
#   1. Parse argv → exit 2 if missing or manifest file not found
#   2. Read YAML → exit 1 on parse failure
#   3. validate_call_manifest(cm) — Group A/B/C
#   4. Load CSV
#   5. validate_call_manifest(cm, df) — Group D
#   6. estimate_panel_fe(cm, df)
#   7. write_result_manifest(cm, est, abs_path)
#   8. exit 0
#
# All errors are tryCatch()-ed at the top level; any unexpected failure
# results in exit status 1.

# Locate the directory holding this script so we can source siblings even when
# called from a different cwd.
#' @title Resolve the directory of the currently-running R script.
#' @description Reads `commandArgs(trailingOnly = FALSE)` for the `--file=`
#'   token to locate the script, so sibling `R/*.R` files can be sourced
#'   regardless of the caller's working directory. Falls back to the current
#'   working directory when no `--file=` token is present.
#' @return Absolute path to the directory containing this script
#'   (a length-1 character string).
#' @noRd
.this_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) >= 1L) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), mustWork = FALSE))
  }
  # Fallback to cwd / "R".
  getwd()
}

# Detect Rscript-mode invocation by presence of `--file=` in commandArgs.
# During R CMD INSTALL lazy-load, commandArgs() does NOT include `--file=`,
# so we skip the source() bootstrap (the four sibling functions arrive via
# the package namespace as R parses each R/*.R file). During `Rscript
# R/run_call_manifest.R <manifest>`, `--file=R/run_call_manifest.R` IS
# present and the bootstrap runs.
.is_rscript_mode <- function() {
  any(grepl("^--file=", commandArgs(trailingOnly = FALSE), fixed = FALSE))
}

if (.is_rscript_mode()) {
  .script_dir <- .this_script_dir()
  source(file.path(.script_dir, "validate_call_manifest.R"), chdir = FALSE)
  source(file.path(.script_dir, "drift_metadata.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_panel_fe.R"), chdir = FALSE)
  source(file.path(.script_dir, "write_result_manifest.R"), chdir = FALSE)
  # v1-2: shared filter helper + DiD/event-study estimators
  source(file.path(.script_dir, "apply_data_filter.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_did.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_did_twfe.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_did_sunab.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_did_cs.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_did_dchd.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_did_bjs.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_event_study.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_event_study_classical.R"), chdir = FALSE)
  source(file.path(.script_dir, "estimate_event_study_sunab.R"), chdir = FALSE)
}

#' @title Write a message to stderr and exit with a given status code.
#' @description Used by the entrypoint to terminate the process with a
#'   deterministic exit code after printing a single error line. Adds a
#'   trailing newline if one is not already present.
#' @param msg Character scalar to write to stderr.
#' @param status Integer exit status. Defaults to 1.
#' @return Does not return; calls `quit(save = "no", ...)`.
#' @noRd
.die <- function(msg, status = 1L) {
  cat(msg, file = stderr())
  if (!endsWith(msg, "\n")) cat("\n", file = stderr())
  quit(save = "no", status = status, runLast = FALSE)
}

#' @title Entrypoint: parse argv, validate, estimate, write result manifest.
#' @description Orchestrates the full pipeline. Reads the call manifest path
#'   from `commandArgs(trailingOnly = TRUE)[1]`; validates required packages;
#'   parses the YAML; runs Group A/B/C validation; loads the CSV; runs
#'   Group D validation; calls `estimate_panel_fe`; calls
#'   `write_result_manifest`; exits 0 on success. Any failure routes through
#'   `.die()` with the appropriate `[VALIDATION ERROR]`, `[ESTIMATION ERROR]`,
#'   `[OUTPUT ERROR]`, or `[ERROR]` prefix.
#' @return Does not return; calls `quit()` directly.
#' @noRd
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L) {
    .die("[ERROR] Usage: Rscript R/run_call_manifest.R <call_manifest_path>", status = 2L)
  }
  call_manifest_path <- args[1]
  if (!file.exists(call_manifest_path)) {
    .die(sprintf("[ERROR] Call manifest not found: %s", call_manifest_path), status = 2L)
  }
  abs_manifest_path <- normalizePath(call_manifest_path, mustWork = TRUE)

  # Early dependency check.
  if (!requireNamespace("fixest", quietly = TRUE)) {
    .die("[ERROR] required package 'fixest' is not installed.", status = 1L)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    .die("[ERROR] required package 'yaml' is not installed.", status = 1L)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    .die("[ERROR] required package 'jsonlite' is not installed.", status = 1L)
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    .die("[ERROR] required package 'digest' is not installed.", status = 1L)
  }

  cm <- tryCatch(
    yaml::read_yaml(call_manifest_path),
    error = function(e) {
      .die(sprintf("[ERROR] failed to parse YAML: %s", conditionMessage(e)), status = 1L)
    }
  )

  # Group A/B/C validation.
  tryCatch(
    validate_call_manifest(cm),
    error = function(e) {
      .die(conditionMessage(e), status = 1L)
    }
  )

  # Load CSV.
  df <- tryCatch(
    utils::read.csv(cm$input$data_path, stringsAsFactors = FALSE),
    error = function(e) {
      .die(sprintf("[ERROR] failed to read data_path %s: %s",
                   cm$input$data_path, conditionMessage(e)),
           status = 1L)
    }
  )

  # Group D validation (column-presence checks).
  tryCatch(
    validate_call_manifest(cm, df = df),
    error = function(e) {
      .die(conditionMessage(e), status = 1L)
    }
  )

  # ---- Dispatch on method_family (v1-2 final) --------------------------------
  mf <- cm$specification$method_family
  if (identical(mf, "panel_fe_regression")) {
    est_result <- tryCatch(
      estimate_panel_fe(cm, df),
      error = function(e) {
        .die(conditionMessage(e), status = 1L)
      }
    )

    tryCatch(
      write_result_manifest(cm, est_result, abs_manifest_path),
      error = function(e) {
        .die(conditionMessage(e), status = 1L)
      }
    )

    cat("[SUCCESS] run complete.\n")
    quit(save = "no", status = 0L, runLast = FALSE)

  } else if (identical(mf, "did")) {
    did_v <- cm$specification$did_variant
    if (identical(did_v, "callaway_santanna") && !requireNamespace("did", quietly = TRUE)) {
      .die("[ERROR] required package 'did' is not installed.", status = 1L)
    }
    if (identical(did_v, "dchd")) {
      .die(paste0(
        "[ERROR] did_variant='dchd' is currently DISABLED in v1-2 due to a known ",
        "upstream bug in DIDmultiplegt v2.1.0 (returns NaN for the effect statistic ",
        "even on bundled wagepan_mgt example). Wrapper code is retained at ",
        "R/estimate_did_dchd.R for future re-enablement once upstream is fixed. ",
        "See NEWS.md v0.1.2 known issues."), status = 1L)
    }
    if (identical(did_v, "bjs") && !requireNamespace("didimputation", quietly = TRUE)) {
      .die("[ERROR] required package 'didimputation' is not installed.", status = 1L)
    }

    filt <- tryCatch(apply_data_filter(cm, df),
                     error = function(e) .die(conditionMessage(e), status = 1L))

    est <- tryCatch(estimate_did(cm, filt$df_filtered),
                    error = function(e) .die(conditionMessage(e), status = 1L))

    .ensure_dir(cm$outputs$coefficient_table_path)
    tryCatch(
      utils::write.csv(est$coefficient_table,
                       file = cm$outputs$coefficient_table_path,
                       row.names = FALSE, na = ""),
      error = function(e) .die(sprintf("[OUTPUT ERROR] failed to write coefficient table: %s",
                                       conditionMessage(e)), status = 1L)
    )

    .ensure_dir(cm$outputs$model_summary_path)
    tryCatch({
      con <- file(cm$outputs$model_summary_path, open = "wt")
      on.exit(close(con), add = TRUE)
      writeLines(est$model_summary_text, con)
    }, error = function(e) .die(sprintf("[OUTPUT ERROR] failed to write model summary: %s",
                                        conditionMessage(e)), status = 1L))

    est_result <- list(
      row_count_before = filt$row_count_before,
      row_count_used = filt$row_count_used,
      dropped_row_count = filt$dropped_row_count,
      drop_reason_summary = filt$drop_reason_summary,
      exact_formula_str = sprintf("did(%s): %d coefficient rows",
                                  if (is.null(did_v)) "<unset>" else did_v,
                                  nrow(est$coefficient_table)),
      warnings_captured = est$warnings
    )

    tryCatch(
      write_result_manifest(cm, est_result, abs_manifest_path,
                            did_results_block = est$did_results_block,
                            event_study_results_block = est$event_study_results_block),
      error = function(e) .die(conditionMessage(e), status = 1L)
    )

    cat("[SUCCESS] run complete.\n")
    quit(save = "no", status = 0L, runLast = FALSE)

  } else if (identical(mf, "event_study")) {
    evs_v <- cm$specification$event_study_variant

    filt <- tryCatch(apply_data_filter(cm, df),
                     error = function(e) .die(conditionMessage(e), status = 1L))

    est <- tryCatch(estimate_event_study(cm, filt$df_filtered),
                    error = function(e) .die(conditionMessage(e), status = 1L))

    .ensure_dir(cm$outputs$coefficient_table_path)
    tryCatch(
      utils::write.csv(est$coefficient_table,
                       file = cm$outputs$coefficient_table_path,
                       row.names = FALSE, na = ""),
      error = function(e) .die(sprintf("[OUTPUT ERROR] failed to write coefficient table: %s",
                                       conditionMessage(e)), status = 1L)
    )

    .ensure_dir(cm$outputs$model_summary_path)
    tryCatch({
      con <- file(cm$outputs$model_summary_path, open = "wt")
      on.exit(close(con), add = TRUE)
      writeLines(est$model_summary_text, con)
    }, error = function(e) .die(sprintf("[OUTPUT ERROR] failed to write model summary: %s",
                                        conditionMessage(e)), status = 1L))

    est_result <- list(
      row_count_before = filt$row_count_before,
      row_count_used = filt$row_count_used,
      dropped_row_count = filt$dropped_row_count,
      drop_reason_summary = filt$drop_reason_summary,
      exact_formula_str = sprintf("event_study(%s): %d coefficient rows",
                                  if (is.null(evs_v)) "<unset>" else evs_v,
                                  nrow(est$coefficient_table)),
      warnings_captured = est$warnings
    )

    tryCatch(
      write_result_manifest(cm, est_result, abs_manifest_path,
                            did_results_block = est$did_results_block,
                            event_study_results_block = est$event_study_results_block),
      error = function(e) .die(conditionMessage(e), status = 1L)
    )

    cat("[SUCCESS] run complete.\n")
    quit(save = "no", status = 0L, runLast = FALSE)

  } else {
    .die(sprintf("[ESTIMATION ERROR] unknown method_family: %s", mf), status = 1L)
  }
}

# Only invoke main() when run as a script, not when sourced.
#' @title Detect whether this file is being executed as a script.
#' @description Returns TRUE when `commandArgs(trailingOnly = FALSE)` contains
#'   a `--file=` token (which `Rscript` always supplies), and FALSE when the
#'   file is being sourced from an interactive R session or another script.
#' @return Logical scalar.
#' @noRd
.is_running_as_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("^--file=", args))
}

if (.is_running_as_script()) {
  tryCatch(
    main(),
    error = function(e) {
      .die(sprintf("[ERROR] unexpected failure: %s", conditionMessage(e)),
           status = 1L)
    }
  )
}
