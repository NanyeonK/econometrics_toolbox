# estimate_event_study.R
#
# Dispatcher for the 'event_study' method_family. Routes by
# event_study_variant to the variant-specific estimator.

#' @title Event-study estimator dispatcher
#' @description Routes a call manifest with method_family == "event_study"
#'   to one of the supported variant implementations.
#' @param cm Parsed call manifest.
#' @param df data.frame already filtered.
#' @return Uniform estimator contract list.
#' @noRd
estimate_event_study <- function(cm, df) {
  variant <- cm$specification$event_study_variant
  if (is.null(variant) || !nzchar(variant)) {
    stop("[ESTIMATE ERROR] estimate_event_study: event_study_variant is empty (validator should have caught this).")
  }
  switch(variant,
    classical   = estimate_event_study_classical(cm, df),
    sun_abraham = estimate_event_study_sunab(cm, df),
    stop(sprintf("[ESTIMATE ERROR] estimate_event_study: unknown event_study_variant '%s'.", variant))
  )
}
