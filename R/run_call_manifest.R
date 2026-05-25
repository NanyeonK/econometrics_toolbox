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

.script_dir <- .this_script_dir()
source(file.path(.script_dir, "validate_call_manifest.R"), chdir = FALSE)
source(file.path(.script_dir, "drift_metadata.R"), chdir = FALSE)
source(file.path(.script_dir, "estimate_panel_fe.R"), chdir = FALSE)
source(file.path(.script_dir, "write_result_manifest.R"), chdir = FALSE)

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
