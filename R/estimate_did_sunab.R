# estimate_did_sunab.R
#
# DiD via Sun & Abraham (2021) interaction-weighted estimator using
# `fixest::sunab()` inside `fixest::feols()`. We extract BOTH the aggregate
# cohort-averaged ATT (via summary(fit, agg = "ATT")) AND the dynamic
# cohort-averaged event-time path (via summary(fit, agg = FALSE) filtered to
# event_time != -1).
#
# Design choices (documented for downstream stages):
#   - never_treated_value: helper recodes it to numeric Inf because
#     `fixest::sunab()` treats Inf-cohort units as the never-treated control.
#   - Aggregate ATT: we use `summary(fit, agg = "ATT")` — fixest collapses the
#     cohort-and-event-time interactions into a single average post-treatment
#     coefficient under the Sun-Abraham reweighting.
#   - Dynamic: cohort-averaged coefficients per event time k (k != -1).

#' @title Estimate a Sun-Abraham DiD via fixest::sunab.
#' @description Fits `dep ~ sunab(cohort, time_to_treat) (+ controls)
#'   | unit + time`. Populates BOTH a did_results_block (aggregate ATT) and an
#'   event_study_results_block (dynamic cohort-averaged ATT(k)).
#' @param cm Parsed call manifest list.
#' @param df Loaded data.frame.
#' @return Named list with `coefficient_table`, `model_summary_text`,
#'   `did_results_block`, `event_study_results_block`, `warnings`.
#' @noRd
estimate_did_sunab <- function(cm, df) {

  warnings_captured <- character(0)

  dep    <- cm$specification$dependent_variable
  cohort <- cm$specification$cohort_var
  ttt    <- cm$specification$time_to_treat_var
  unit   <- cm$specification$panel$unit
  time   <- cm$specification$panel$time
  ctrls  <- if (is.null(cm$specification$controls)) character(0)
            else as.character(unlist(cm$specification$controls, use.names = FALSE))
  ctrls  <- ctrls[nzchar(ctrls)]

  # Recode never_treated_value to Inf (fixest::sunab convention).
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

  # The sunab() second argument is the "period" (time) variable; sunab derives
  # the event-time internally as time - cohort. If the manifest declares an
  # explicit time_to_treat_var, we prefer it; otherwise pass the panel time.
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
      stop(sprintf("[ESTIMATE ERROR] did_sunab feols() failed: %s",
                   conditionMessage(e)))
    }
  )

  # Aggregate ATT via summary(fit, agg = "ATT") -------------------------------
  agg_ct <- tryCatch(
    {
      s <- summary(fit, agg = "ATT")
      fixest::coeftable(s)
    },
    error = function(e) {
      stop(sprintf("[ESTIMATE ERROR] did_sunab summary(agg='ATT') failed: %s",
                   conditionMessage(e)))
    }
  )
  # The aggregate ATT row in fixest is conventionally named "ATT" (post-treatment
  # average). Defensive: pick the row matching "ATT" or fall back to the first.
  att_rowname <- intersect(c("ATT"), rownames(agg_ct))[1]
  if (is.na(att_rowname)) att_rowname <- rownames(agg_ct)[1]
  att_overall    <- as.numeric(agg_ct[att_rowname, "Estimate"])
  att_overall_se <- as.numeric(agg_ct[att_rowname, "Std. Error"])
  pcol           <- grep("^Pr\\(", colnames(agg_ct), value = TRUE)[1]
  att_overall_p  <- if (!is.na(pcol)) as.numeric(agg_ct[att_rowname, pcol]) else NA_real_

  # Dynamic event-time coefficients via summary(fit, agg = FALSE) -------------
  dyn_ct <- tryCatch(
    {
      s_dyn <- summary(fit, agg = FALSE)
      fixest::coeftable(s_dyn)
    },
    error = function(e) {
      stop(sprintf("[ESTIMATE ERROR] did_sunab summary(agg=FALSE) failed: %s",
                   conditionMessage(e)))
    }
  )
  dyn_ci <- tryCatch(stats::confint(summary(fit, agg = FALSE)),
                     error = function(e) NULL)

  raw_terms <- rownames(dyn_ct)
  # Parse "<period>::<k>:cohort::<c>" or "<period>::<k>" → event_time integer k
  k_chr <- sub("^[^:]+::(-?\\d+).*$", "\\1", raw_terms)
  k_int <- suppressWarnings(as.integer(k_chr))

  # Keep only event-time rows (k parseable); drop control rows for ES table.
  is_event <- !is.na(k_int)
  # Drop the reference period (k == -1) — sunab uses -1 as the implicit ref.
  keep_dyn <- is_event & (k_int != -1L)

  # Build dynamic per-(cohort, k) coefficient table ---------------------------
  est_d  <- as.numeric(dyn_ct[keep_dyn, "Estimate"])
  se_d   <- as.numeric(dyn_ct[keep_dyn, "Std. Error"])
  stat_c <- intersect(c("t value", "z value"), colnames(dyn_ct))[1]
  stat_d <- if (!is.na(stat_c)) as.numeric(dyn_ct[keep_dyn, stat_c])
            else rep(NA_real_, sum(keep_dyn))
  pcol_d <- grep("^Pr\\(", colnames(dyn_ct), value = TRUE)[1]
  p_d    <- if (!is.na(pcol_d)) as.numeric(dyn_ct[keep_dyn, pcol_d])
            else rep(NA_real_, sum(keep_dyn))
  if (is.null(dyn_ci)) {
    cl_d <- rep(NA_real_, sum(keep_dyn))
    ch_d <- rep(NA_real_, sum(keep_dyn))
  } else {
    cl_d <- as.numeric(dyn_ci[keep_dyn, 1])
    ch_d <- as.numeric(dyn_ci[keep_dyn, 2])
  }
  k_d    <- k_int[keep_dyn]
  raw_d  <- raw_terms[keep_dyn]

  # Extract cohort from "...cohort::<c>" portion when present.
  coh_chr <- sub("^.*cohort::(-?[0-9eE.+-]+).*$", "\\1", raw_d)
  coh_num <- suppressWarnings(as.numeric(coh_chr))
  # When cohort capture failed (no cohort fragment), set NA.
  coh_num[coh_chr == raw_d] <- NA_real_

  dyn_term <- ifelse(is.na(coh_num),
                     sprintf("tau_%d", k_d),
                     sprintf("att_c%g_k%d", coh_num, k_d))

  # Also include the aggregate ATT row at the top.
  agg_row <- data.frame(
    term       = "att_overall",
    estimate   = att_overall,
    std_error  = att_overall_se,
    statistic  = NA_real_,
    p_value    = att_overall_p,
    conf_low   = NA_real_,
    conf_high  = NA_real_,
    group      = NA_real_,
    time       = NA_real_,
    event_time = NA_integer_,
    stringsAsFactors = FALSE
  )

  dyn_rows <- data.frame(
    term       = dyn_term,
    estimate   = est_d,
    std_error  = se_d,
    statistic  = stat_d,
    p_value    = p_d,
    conf_low   = cl_d,
    conf_high  = ch_d,
    group      = coh_num,
    time       = ifelse(is.na(coh_num), NA_real_, coh_num + k_d),
    event_time = as.integer(k_d),
    stringsAsFactors = FALSE
  )

  coefficient_table <- rbind(agg_row, dyn_rows)

  # Variant blocks -----------------------------------------------------------
  did_results_block <- list(
    did_variant         = "sun_abraham",
    aggregation         = "cohort",
    att_overall         = as.numeric(att_overall),
    att_overall_se      = as.numeric(att_overall_se),
    att_overall_p       = as.numeric(att_overall_p),
    group_time_att_path = ""
  )

  # Also expose the dynamic ATT(k) path through event_study_results_block so
  # downstream stages can consume the cohort-averaged dynamics without a
  # separate estimator invocation.
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
    did_results_block         = did_results_block,
    event_study_results_block = event_study_results_block,
    warnings                  = warnings_captured
  )
}
