# estimate_did_dchd.R
#
# de Chaisemartin and d'Haultfoeuille DiD wrapper around DIDmultiplegt::did_multiplegt
# (older non-polars static API). Returns the uniform estimator contract used by
# run_call_manifest.R.

#' @title de Chaisemartin-d'Haultfoeuille DiD estimator (static)
#' @description Wraps DIDmultiplegt::did_multiplegt for the instantaneous
#'   average treatment effect. Bootstrap is disabled for determinism;
#'   analytic SEs are used when supplied by the package.
#' @param cm Parsed call manifest.
#' @param df data.frame already sample/missing filtered.
#' @return list(coefficient_table, model_summary_text, did_results_block,
#'   event_study_results_block, warnings).
#' @noRd
estimate_did_dchd <- function(cm, df) {
  spec   <- cm$specification
  yname  <- spec$dependent_variable
  treat  <- spec$treatment_indicator
  idname <- spec$panel$unit
  tname  <- spec$panel$time
  controls   <- if (length(spec$controls) > 0L) as.character(spec$controls) else c()

  required <- c(yname, treat, idname, tname, controls)
  miss <- setdiff(required, names(df))
  if (length(miss) > 0L) {
    stop(sprintf("[ESTIMATE ERROR] dCdH: missing columns in data: %s",
                 paste(miss, collapse = ", ")))
  }

  set.seed(20260525L)
  mod <- tryCatch(
    DIDmultiplegt::did_multiplegt(
      mode     = "old",
      df       = as.data.frame(df),
      Y        = yname,
      G        = idname,
      T        = tname,
      D        = treat,
      controls = controls,
      placebo  = 0L,
      dynamic  = 0L,
      brep     = 0L,
      cluster  = NULL
    ),
    error = function(e) stop(sprintf("[ESTIMATE ERROR] did_multiplegt failed: %s",
                                     conditionMessage(e)))
  )

  est <- tryCatch(as.numeric(mod$effect), error = function(e) NA_real_)
  se  <- tryCatch(as.numeric(mod$se_effect), error = function(e) NA_real_)
  if (length(est) == 0L || is.na(est)) {
    stop("[ESTIMATE ERROR] dCdH: could not extract effect estimate")
  }

  if (is.na(se) || se <= 0) {
    z <- NA_real_; p <- NA_real_; lo <- NA_real_; hi <- NA_real_
    warns <- "dCdH: analytic SE unavailable (brep=0); statistic/p_value/CI set to NA"
  } else {
    z <- est / se
    p <- 2 * pnorm(-abs(z))
    lo <- est - 1.96 * se
    hi <- est + 1.96 * se
    warns <- character(0)
  }

  ct <- data.frame(
    term       = "att",
    estimate   = est,
    std_error  = se,
    statistic  = z,
    p_value    = p,
    conf_low   = lo,
    conf_high  = hi,
    group      = NA_real_,
    time       = NA_real_,
    event_time = NA_real_,
    stringsAsFactors = FALSE
  )

  did_block <- list(
    estimator = "dchd",
    att = est,
    att_se = se,
    effects_window = 0L,
    placebo_window = 0L,
    note = "DIDmultiplegt (static; non-polars)"
  )

  summary_text <- paste(c(
    "DIDmultiplegt::did_multiplegt results:",
    capture.output(print(mod))
  ), collapse = "\n")

  list(
    coefficient_table = ct,
    model_summary_text = summary_text,
    did_results_block = did_block,
    event_study_results_block = NULL,
    warnings = warns
  )
}
