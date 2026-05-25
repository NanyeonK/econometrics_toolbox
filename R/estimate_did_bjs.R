# estimate_did_bjs.R
#
# Borusyak-Jaravel-Spiess imputation DiD wrapper around didimputation::did_imputation.
# Returns the uniform estimator contract used by run_call_manifest.R.

#' @title Borusyak-Jaravel-Spiess imputation DiD estimator
#' @description Wraps didimputation::did_imputation. Returns overall ATT plus
#'   any horizon dynamics if requested in the manifest.
#' @param cm Parsed call manifest.
#' @param df data.frame already filtered.
#' @return list(coefficient_table, model_summary_text, did_results_block,
#'   event_study_results_block, warnings).
#' @noRd
estimate_did_bjs <- function(cm, df) {
  spec   <- cm$specification
  yname  <- spec$dependent_variable
  gname  <- spec$cohort_var
  idname <- spec$panel$unit
  tname  <- spec$panel$time
  horizon <- if (!is.null(spec$horizon)) spec$horizon else FALSE
  controls <- if (length(spec$controls) > 0L) spec$controls else NULL

  required <- c(yname, gname, idname, tname, controls)
  miss <- setdiff(required, names(df))
  if (length(miss) > 0L) {
    stop(sprintf("[ESTIMATE ERROR] BJS: missing columns in data: %s",
                 paste(miss, collapse = ", ")))
  }

  pretrends <- if (!is.null(spec$pretrends)) spec$pretrends else FALSE

  set.seed(20260525L)
  res <- tryCatch(
    didimputation::did_imputation(
      data      = as.data.frame(df),
      yname     = yname,
      gname     = gname,
      tname     = tname,
      idname    = idname,
      first_stage = if (is.null(controls)) NULL else as.formula(paste("~", paste(controls, collapse = " + "))),
      horizon   = horizon,
      pretrends = pretrends
    ),
    error = function(e) stop(sprintf("[ESTIMATE ERROR] did_imputation failed: %s",
                                     conditionMessage(e)))
  )

  if (!is.data.frame(res) || nrow(res) == 0L) {
    stop("[ESTIMATE ERROR] did_imputation returned no estimates")
  }

  est <- as.numeric(res$estimate)
  se  <- as.numeric(res$std.error)
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))
  lo  <- est - 1.96 * se
  hi  <- est + 1.96 * se

  if (!is.null(res$term)) {
    terms <- as.character(res$term)
  } else if (!is.null(res$lhs)) {
    terms <- as.character(res$lhs)
  } else {
    terms <- paste0("att_", seq_len(nrow(res)))
  }

  et <- rep(NA_real_, nrow(res))
  if (isTRUE(horizon) || (is.numeric(horizon) && length(horizon) > 0L)) {
    if (!is.null(res$rhs)) {
      et_try <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(res$rhs))))
      et <- ifelse(is.na(et_try), NA_real_, et_try)
    }
  }

  ct <- data.frame(
    term       = terms,
    estimate   = est,
    std_error  = se,
    statistic  = z,
    p_value    = p,
    conf_low   = lo,
    conf_high  = hi,
    group      = NA_real_,
    time       = NA_real_,
    event_time = et,
    stringsAsFactors = FALSE
  )

  overall_row <- which(terms %in% c("treat", "att", "0", "ATT", "Overall"))
  if (length(overall_row) > 0L) {
    overall_row <- overall_row[1L]
  } else {
    overall_row <- 1L
  }

  did_block <- list(
    estimator = "bjs",
    att = est[overall_row],
    att_se = se[overall_row],
    horizon = horizon,
    pretrends = pretrends,
    n_rows = nrow(res)
  )

  summary_text <- paste(c(
    "didimputation::did_imputation results:",
    capture.output(print(res))
  ), collapse = "\n")

  list(
    coefficient_table = ct,
    model_summary_text = summary_text,
    did_results_block = did_block,
    event_study_results_block = NULL,
    warnings = character(0)
  )
}
