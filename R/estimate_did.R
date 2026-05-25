# estimate_did.R
#
# Dispatcher for the 'did' method_family. Routes by did_variant to the
# variant-specific estimator. Each variant lives in its own file
# (R/estimate_did_<variant>.R) and exports estimate_did_<variant>(cm, df).

#' @title DiD estimator dispatcher
#' @description Routes a call manifest with method_family == "did" to one of
#'   the five supported variant implementations. Validation upstream has
#'   already confirmed did_variant is in the allowed enum and that all
#'   variant-specific required fields are present.
#' @param cm Parsed call manifest.
#' @param df data.frame already sample/missing filtered.
#' @return Uniform estimator contract list (coefficient_table,
#'   model_summary_text, did_results_block, event_study_results_block, warnings).
#' @noRd
estimate_did <- function(cm, df) {
  variant <- cm$specification$did_variant
  if (is.null(variant) || !nzchar(variant)) {
    stop("[ESTIMATE ERROR] estimate_did: did_variant is empty (validator should have caught this).")
  }
  switch(variant,
    twfe              = estimate_did_twfe(cm, df),
    sun_abraham       = estimate_did_sunab(cm, df),
    callaway_santanna = estimate_did_cs(cm, df),
    dchd              = estimate_did_dchd(cm, df),
    bjs               = estimate_did_bjs(cm, df),
    stop(sprintf("[ESTIMATE ERROR] estimate_did: unknown did_variant '%s'.", variant))
  )
}
