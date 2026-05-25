# estimate_event_study_sunab.R
#
# Event-study via Sun & Abraham (2021) interaction-weighted estimator using
# `fixest::sunab()` inside `fixest::feols()`. Unlike `estimate_did_sunab`,
# this helper exposes the dynamic cohort-averaged ATT(k) path ONLY (no
# aggregate ATT block). The aggregate is the DiD variant's responsibility.
#
# Pure helper — no top-level executable code.

#' @title Estimate a Sun-Abraham event-study via fixest::sunab.
#' @description Fits `dep ~ sunab(cohort, time) (+ controls) | unit + time`
#'   then extracts cohort-averaged dynamic ATT(k) via
#'   `summary(fit, agg = "ATT")` (which, for sunab fits, returns the per-event-time
#'   averaged path indexed by k). Reference period is -1 (sunab default).
#' @param cm Parsed call manifest list.
#' @param df Loaded data.frame.
#' @return Named list with `coefficient_table`, `model_summary_text`,
#'   `did_results_block` (NULL), `event_study_results_block`, `warnings`.
#' @noRd
estimate_event_study_sunab <- function(cm, df) {

  warnings_captured <- character(0)

  dep    <- cm$specification$dependent_variable
  cohort <- cm$specification$cohort_var
  ttt    <- cm$specification$time_to_treat_var
  unit   <- cm$specification$panel$unit
  time   <- cm$specification$panel$time
  ctrls  <- if (is.null(cm$specification$controls)) character(0)
            else as.character(unlist(cm$specification$controls, use.names = FALSE))
  ctrls  <- ctrls[nzchar(ctrls)]

  # Recode never_treated_value to Inf (sunab convention).
  ntv <- cm$specification$never_treated_value
  if (!is.null(ntv) && length(ntv) == 1L) {
    coh_vec <- df[[cohort]]
    ntv_chr <- as.character(ntv)
    if (identical(toupper(ntv_chr), "INF")) {
      ntv_num <- Inf
    } else {
      ntv_num <- suppressWarnings(as.numeric(ntv_chr))
    }
    if (!is.na(ntv_num)) {
      df[[cohort]] <- ifelse(coh_vec == ntv_num, Inf, as.numeric(coh_vec))
    }
  }

  sunab_period_arg <- if (!is.null(ttt) && length(ttt) == 1L && nzchar(ttt)) ttt else time

  rhs_main <- paste0("sunab(", cohort, ", ", sunab_period_arg, ")")
  if (length(ctrls) > 0L) {
    rhs_main <- paste0(rhs_main, " + ", paste(ctrls, collapse = " + "))
  }
  fml_str <- paste0(dep, " ~ ", rhs_main, " | ", unit, " + ", time)
  fml     <- stats::as.formula(fml_str)

  cov_method <- cm$specification$covariance_method
  clust_fml  <- NULL
  if (identical(cov_method, "clustered")) {
    clust_vars <- as.character(unlist(cm$specification$cluster_variables,
                                      use.names = FALSE))
    clust_vars <- clust_vars[nzchar(clust_vars)]
    if (length(clust_vars) > 0L) {
      clust_fml <- stats::as.formula(paste0("~", paste(clust_vars, collapse = " + ")))
    }
  }

  fit <- tryCatch(
    withCallingHandlers(
      {
        if (!is.null(clust_fml)) {
          fixest::feols(fml, data = df, cluster = clust_fml, nthreads = 1)
        } else {
          fixest::feols(fml, data = df, vcov = "iid", nthreads = 1)
        }
      },
      warning = function(w) {
        warnings_captured <<- c(warnings_captured, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(sprintf("[ESTIMATE ERROR] event_study_sunab feols() failed: %s",
                   conditionMessage(e)))
    }
  )

  # Dynamic cohort-averaged ATT(k) via summary(fit, agg = "ATT") --------------
  # For a sunab() fit, `agg = "ATT"` returns a coefficient table indexed by
  # event time k (cohort-averaged), with the reference period (-1) omitted.
  agg_ct <- tryCatch(
    {
      s <- summary(fit, agg = "ATT")
      fixest::coeftable(s)
    },
    error = function(e) {
      stop(sprintf("[ESTIMATE ERROR] event_study_sunab summary(agg='ATT') failed: %s",
                   conditionMessage(e)))
    }
  )
  agg_ci <- tryCatch(stats::confint(summary(fit, agg = "ATT")),
                     error = function(e) NULL)

  raw_terms <- rownames(agg_ct)
  # Term names look like "<period>::<k>"; extract k.
  k_chr <- sub("^[^:]+::(-?\\d+).*$", "\\1", raw_terms)
  k_int <- suppressWarnings(as.integer(k_chr))
  is_event <- !is.na(k_int) & (k_chr != raw_terms)
  # Defensive: drop the reference period (k == -1) if it sneaks in.
  keep <- is_event & (k_int != -1L)

  estimate  <- as.numeric(agg_ct[keep, "Estimate"])
  std_error <- as.numeric(agg_ct[keep, "Std. Error"])
  stat_col  <- intersect(c("t value", "z value"), colnames(agg_ct))[1]
  statistic <- if (!is.na(stat_col)) as.numeric(agg_ct[keep, stat_col])
               else rep(NA_real_, sum(keep))
  pval_col  <- grep("^Pr\\(", colnames(agg_ct), value = TRUE)[1]
  p_value   <- if (!is.na(pval_col)) as.numeric(agg_ct[keep, pval_col])
               else rep(NA_real_, sum(keep))
  if (is.null(agg_ci)) {
    conf_low  <- rep(NA_real_, sum(keep))
    conf_high <- rep(NA_real_, sum(keep))
  } else {
    conf_low  <- as.numeric(agg_ci[keep, 1])
    conf_high <- as.numeric(agg_ci[keep, 2])
  }
  k_keep <- as.integer(k_int[keep])

  coefficient_table <- data.frame(
    term       = sprintf("tau_%d", k_keep),
    estimate   = estimate,
    std_error  = std_error,
    statistic  = statistic,
    p_value    = p_value,
    conf_low   = conf_low,
    conf_high  = conf_high,
    group      = rep(NA_real_, sum(keep)),
    time       = rep(NA_real_, sum(keep)),
    event_time = k_keep,
    stringsAsFactors = FALSE
  )

  event_study_results_block <- list(
    event_study_variant    = "sun_abraham",
    leads_lags_path        = "",
    reference_periods_used = list(-1L),
    cohort_var             = as.character(cohort),
    time_to_treat_var      = ""
  )

  model_summary_text <- paste(
    utils::capture.output(summary(fit)),
    collapse = "\n"
  )

  list(
    coefficient_table         = coefficient_table,
    model_summary_text        = model_summary_text,
    did_results_block         = NULL,
    event_study_results_block = event_study_results_block,
    warnings                  = warnings_captured
  )
}
