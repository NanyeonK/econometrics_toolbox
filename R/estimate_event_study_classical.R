# estimate_event_study_classical.R
#
# Classical event-study via fixest::feols with i(time_to_treat, ref = -1) (or
# user-supplied reference_periods). Unit and time fixed effects are absorbed.
# Optional clustered SE.
#
# Pure helper file — no top-level executable code so R CMD INSTALL parses it
# cleanly.

#' @title Estimate a classical event-study via fixest::feols + i().
#' @description Fits `dep ~ i(time_to_treat_var, ref = reference_periods)
#'   (+ controls) | unit + time`. Reference periods default to `c(-1L)` when
#'   absent. Returns leads/lags as the coefficient table and populates
#'   `event_study_results_block`.
#' @param cm Parsed call manifest list.
#' @param df Loaded data.frame.
#' @return Named list with `coefficient_table`, `model_summary_text`,
#'   `did_results_block` (NULL), `event_study_results_block`, `warnings`.
#' @noRd
estimate_event_study_classical <- function(cm, df) {

  warnings_captured <- character(0)

  dep   <- cm$specification$dependent_variable
  ttt   <- cm$specification$time_to_treat_var
  unit  <- cm$specification$panel$unit
  time  <- cm$specification$panel$time
  ctrls <- if (is.null(cm$specification$controls)) character(0)
           else as.character(unlist(cm$specification$controls, use.names = FALSE))
  ctrls <- ctrls[nzchar(ctrls)]

  # Reference periods: default c(-1L) per spec §4 E9.
  ref_raw <- cm$specification$reference_periods
  if (is.null(ref_raw)) {
    ref_periods <- c(-1L)
  } else {
    ref_periods <- suppressWarnings(as.integer(unlist(ref_raw, use.names = FALSE)))
    if (length(ref_periods) == 0L || any(is.na(ref_periods))) {
      ref_periods <- c(-1L)
    }
  }

  ref_str <- if (length(ref_periods) == 1L) {
    as.character(ref_periods)
  } else {
    paste0("c(", paste(ref_periods, collapse = ", "), ")")
  }

  rhs_main <- paste0("i(", ttt, ", ref = ", ref_str, ")")
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
      stop(sprintf("[ESTIMATE ERROR] event_study_classical feols() failed: %s",
                   conditionMessage(e)))
    }
  )

  ct <- tryCatch(fixest::coeftable(fit),
                 error = function(e) {
                   stop(sprintf("[ESTIMATE ERROR] event_study_classical coeftable failed: %s",
                                conditionMessage(e)))
                 })
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)

  raw_terms <- rownames(ct)
  estimate  <- as.numeric(ct[, "Estimate"])
  std_error <- as.numeric(ct[, "Std. Error"])
  stat_col  <- intersect(c("t value", "z value"), colnames(ct))[1]
  statistic <- if (!is.na(stat_col)) as.numeric(ct[, stat_col])
               else rep(NA_real_, length(raw_terms))
  pval_col  <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  p_value   <- if (!is.na(pval_col)) as.numeric(ct[, pval_col])
               else rep(NA_real_, length(raw_terms))
  if (is.null(ci)) {
    conf_low  <- rep(NA_real_, length(raw_terms))
    conf_high <- rep(NA_real_, length(raw_terms))
  } else {
    conf_low  <- as.numeric(ci[, 1])
    conf_high <- as.numeric(ci[, 2])
  }

  # fixest names interaction coefficients as "<var>::<k>" (e.g.
  # "time_to_treat::-2", "time_to_treat::3"). Parse k and rename to tau_<k>.
  k_pat <- paste0("^", gsub("([.\\\\+*?\\[\\]^$()|])", "\\\\\\1", ttt),
                  "::(-?\\d+)$")
  k_chr <- sub(k_pat, "\\1", raw_terms)
  k_int <- suppressWarnings(as.integer(k_chr))
  is_event <- !is.na(k_int) & (k_chr != raw_terms)

  term_out <- raw_terms
  term_out[is_event] <- sprintf("tau_%d", k_int[is_event])

  group_col      <- rep(NA_real_, length(raw_terms))
  time_col       <- rep(NA_real_, length(raw_terms))
  event_time_col <- rep(NA_integer_, length(raw_terms))
  event_time_col[is_event] <- as.integer(k_int[is_event])

  coefficient_table <- data.frame(
    term       = term_out,
    estimate   = estimate,
    std_error  = std_error,
    statistic  = statistic,
    p_value    = p_value,
    conf_low   = conf_low,
    conf_high  = conf_high,
    group      = group_col,
    time       = time_col,
    event_time = event_time_col,
    stringsAsFactors = FALSE
  )

  event_study_results_block <- list(
    event_study_variant    = "classical",
    leads_lags_path        = "",
    reference_periods_used = as.list(as.integer(ref_periods)),
    cohort_var             = "",
    time_to_treat_var      = as.character(ttt)
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
