# estimate_did_twfe.R
#
# DiD via classical two-way fixed effects (fixest::feols). Treatment is a
# binary indicator (0/1). Unit and time fixed effects are absorbed via the
# panel block of the call manifest. Optional clustered SE.
#
# Pure helper: this file defines a single estimator function and contains no
# top-level executable code, so it parses cleanly during R CMD INSTALL.

#' @title Estimate a two-way fixed-effects DiD via fixest::feols.
#' @description Constructs the formula
#'   `dep ~ treatment_indicator (+ controls) | unit + time`, fits with
#'   `fixest::feols(..., nthreads = 1)`, optionally with clustered standard
#'   errors when `covariance_method == "clustered"`. Returns the uniform
#'   Phase-2a estimator contract used by the dispatcher.
#' @param cm Parsed call manifest list (already validated).
#' @param df Loaded data.frame from `cm$input$data_path`.
#' @return Named list with `coefficient_table`, `model_summary_text`,
#'   `did_results_block`, `event_study_results_block`, `warnings`.
#' @noRd
estimate_did_twfe <- function(cm, df) {

  warnings_captured <- character(0)

  dep   <- cm$specification$dependent_variable
  treat <- cm$specification$treatment_indicator
  ctrls <- if (is.null(cm$specification$controls)) character(0)
           else as.character(unlist(cm$specification$controls, use.names = FALSE))
  ctrls <- ctrls[nzchar(ctrls)]
  unit  <- cm$specification$panel$unit
  time  <- cm$specification$panel$time

  rhs_terms <- c(treat, ctrls)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  rhs_main  <- paste(rhs_terms, collapse = " + ")
  fml_str   <- paste0(dep, " ~ ", rhs_main, " | ", unit, " + ", time)
  fml       <- stats::as.formula(fml_str)

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
      stop(sprintf("[ESTIMATE ERROR] did_twfe feols() failed: %s",
                   conditionMessage(e)))
    }
  )

  # Tidy coefficient table -----------------------------------------------------
  ct <- tryCatch(fixest::coeftable(fit),
                 error = function(e) {
                   stop(sprintf("[ESTIMATE ERROR] did_twfe coeftable extract failed: %s",
                                conditionMessage(e)))
                 })
  ci <- tryCatch(stats::confint(fit), error = function(e) NULL)

  term <- rownames(ct)
  estimate  <- as.numeric(ct[, "Estimate"])
  std_error <- as.numeric(ct[, "Std. Error"])
  stat_col  <- intersect(c("t value", "z value"), colnames(ct))[1]
  statistic <- if (!is.na(stat_col)) as.numeric(ct[, stat_col])
               else rep(NA_real_, length(term))
  pval_col  <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  p_value   <- if (!is.na(pval_col)) as.numeric(ct[, pval_col])
               else rep(NA_real_, length(term))
  if (is.null(ci)) {
    conf_low  <- rep(NA_real_, length(term))
    conf_high <- rep(NA_real_, length(term))
  } else {
    conf_low  <- as.numeric(ci[, 1])
    conf_high <- as.numeric(ci[, 2])
  }

  coefficient_table <- data.frame(
    term       = term,
    estimate   = estimate,
    std_error  = std_error,
    statistic  = statistic,
    p_value    = p_value,
    conf_low   = conf_low,
    conf_high  = conf_high,
    group      = rep(NA_real_, length(term)),
    time       = rep(NA_real_, length(term)),
    event_time = rep(NA_integer_, length(term)),
    stringsAsFactors = FALSE
  )

  # Build did_results_block (aggregation = "simple") --------------------------
  treat_row <- which(term == treat)
  if (length(treat_row) == 1L) {
    att     <- estimate[treat_row]
    att_se  <- std_error[treat_row]
    att_p   <- p_value[treat_row]
  } else {
    att <- NA_real_; att_se <- NA_real_; att_p <- NA_real_
    warnings_captured <- c(warnings_captured,
      sprintf("did_twfe: treatment_indicator '%s' not found in coefficient table", treat))
  }

  did_results_block <- list(
    did_variant          = "twfe",
    aggregation          = "simple",
    att_overall          = as.numeric(att),
    att_overall_se       = as.numeric(att_se),
    att_overall_p        = as.numeric(att_p),
    group_time_att_path  = ""
  )

  # Model summary text --------------------------------------------------------
  model_summary_text <- paste(
    utils::capture.output(summary(fit)),
    collapse = "\n"
  )

  list(
    coefficient_table         = coefficient_table,
    model_summary_text        = model_summary_text,
    did_results_block         = did_results_block,
    event_study_results_block = NULL,
    warnings                  = warnings_captured
  )
}
