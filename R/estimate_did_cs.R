# estimate_did_cs.R
#
# Callaway-Sant'Anna (group-time ATT) DiD estimator wrapper around did::att_gt.
# Returns the uniform estimator contract used by run_call_manifest.R.

#' @title Callaway-Sant'Anna DiD estimator
#' @description Wraps did::att_gt + did::aggte to produce group-time ATT(g,t)
#'   plus a simple-aggregation overall ATT. Bootstrap is disabled for
#'   determinism; analytic standard errors are used.
#' @param cm Parsed call manifest (named list with $specification).
#' @param df data.frame already filtered / NA-purged per missing_policy.
#' @return list(coefficient_table, model_summary_text, did_results_block,
#'   event_study_results_block, warnings).
#' @noRd
estimate_did_cs <- function(cm, df) {
  spec  <- cm$specification
  yname <- spec$dependent_variable
  gname <- spec$cohort_var
  idname <- spec$panel$unit
  tname  <- spec$panel$time
  anticip <- if (!is.null(spec$anticipation_periods)) as.integer(spec$anticipation_periods) else 0L
  ctrl    <- if (!is.null(spec$control_group)) spec$control_group else "notyettreated"

  required <- c(yname, gname, idname, tname)
  miss <- setdiff(required, names(df))
  if (length(miss) > 0L) {
    stop(sprintf("[ESTIMATE ERROR] Callaway-Sant'Anna: missing columns in data: %s",
                 paste(miss, collapse = ", ")))
  }

  set.seed(20260525L)
  att_gt_obj <- tryCatch(
    did::att_gt(
      yname = yname,
      tname = tname,
      idname = idname,
      gname = gname,
      data = as.data.frame(df),
      control_group = ctrl,
      anticipation = anticip,
      bstrap = FALSE,
      cband = FALSE,
      panel = TRUE,
      allow_unbalanced_panel = TRUE
    ),
    error = function(e) stop(sprintf("[ESTIMATE ERROR] did::att_gt failed: %s", conditionMessage(e)))
  )

  aggte_obj <- tryCatch(
    did::aggte(att_gt_obj, type = "simple", bstrap = FALSE, cband = FALSE),
    error = function(e) stop(sprintf("[ESTIMATE ERROR] did::aggte failed: %s", conditionMessage(e)))
  )

  groups <- att_gt_obj$group
  times  <- att_gt_obj$t
  atts   <- att_gt_obj$att
  ses    <- att_gt_obj$se
  n_gt   <- length(atts)

  if (n_gt == 0L) {
    stop("[ESTIMATE ERROR] did::att_gt returned no estimates")
  }

  zval <- atts / ses
  pval <- 2 * pnorm(-abs(zval))
  lo   <- atts - 1.96 * ses
  hi   <- atts + 1.96 * ses

  ct_gt <- data.frame(
    term       = sprintf("att_g%s_t%s", groups, times),
    estimate   = as.numeric(atts),
    std_error  = as.numeric(ses),
    statistic  = as.numeric(zval),
    p_value    = as.numeric(pval),
    conf_low   = as.numeric(lo),
    conf_high  = as.numeric(hi),
    group      = as.numeric(groups),
    time       = as.numeric(times),
    event_time = NA_real_,
    stringsAsFactors = FALSE
  )

  overall_z <- aggte_obj$overall.att / aggte_obj$overall.se
  overall_p <- 2 * pnorm(-abs(overall_z))
  ct_overall <- data.frame(
    term       = "att_overall",
    estimate   = as.numeric(aggte_obj$overall.att),
    std_error  = as.numeric(aggte_obj$overall.se),
    statistic  = as.numeric(overall_z),
    p_value    = as.numeric(overall_p),
    conf_low   = as.numeric(aggte_obj$overall.att - 1.96 * aggte_obj$overall.se),
    conf_high  = as.numeric(aggte_obj$overall.att + 1.96 * aggte_obj$overall.se),
    group      = NA_real_,
    time       = NA_real_,
    event_time = NA_real_,
    stringsAsFactors = FALSE
  )

  ct <- rbind(ct_gt, ct_overall)

  group_time_atts <- lapply(seq_len(n_gt), function(i) {
    list(group = as.numeric(groups[i]),
         time  = as.numeric(times[i]),
         estimate = as.numeric(atts[i]),
         se = as.numeric(ses[i]))
  })

  did_block <- list(
    estimator = "callaway_santanna",
    att_overall = as.numeric(aggte_obj$overall.att),
    att_overall_se = as.numeric(aggte_obj$overall.se),
    n_group_time_atts = n_gt,
    control_group = ctrl,
    anticipation_periods = anticip,
    group_time_atts = group_time_atts
  )

  summary_text <- paste(c(
    capture.output(summary(att_gt_obj)),
    "",
    "Overall ATT (simple aggregation):",
    capture.output(summary(aggte_obj))
  ), collapse = "\n")

  list(
    coefficient_table = ct,
    model_summary_text = summary_text,
    did_results_block = did_block,
    event_study_results_block = NULL,
    warnings = character(0)
  )
}
