# estimate_panel_fe.R
#
# Applies sample_filter + missing_policy, builds the fixest::feols formula
# exactly from the manifest, runs the regression with nthreads = 1, and writes
# the coefficient CSV and model summary text file.
#
# Returns a list of estimation artefacts consumed by write_result_manifest().

#' @title Normalise an absent / NULL / scalar / list value to a character vector.
#' @description Local helper duplicating `.char_vec` from
#'   `validate_call_manifest.R` so the estimator can be sourced standalone.
#' @param x Any R object.
#' @return Character vector (possibly empty).
#' @noRd
.char_vec2 <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x)) return(unlist(x, use.names = FALSE))
  as.character(x)
}

#' @title Create the parent directory of an output path if it does not exist.
#' @description Used before every output write so that callers can declare
#'   any nested output directory in the call manifest without having to
#'   `mkdir -p` beforehand.
#' @param path Character scalar; the full output file path.
#' @return `invisible(NULL)`.
#' @noRd
.ensure_dir <- function(path) {
  d <- dirname(path)
  if (nzchar(d) && !dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(NULL)
}

#' @title Extract a 7-column tidy coefficient table from a fixest model.
#' @description Prefers `broom::tidy(fit, conf.int = TRUE)` when broom is
#'   installed; otherwise extracts directly from `fixest::coeftable(fit)`
#'   and `confint(fit)`. Output columns are always exactly
#'   `term, estimate, std_error, statistic, p_value, conf_low, conf_high`.
#' @param fit A fitted `fixest` model object.
#' @return Data frame with 7 columns and one row per estimated coefficient.
#' @noRd
.build_tidy_coef <- function(fit) {
  # Prefer broom::tidy if the package is installed.
  if (requireNamespace("broom", quietly = TRUE)) {
    td <- tryCatch(
      broom::tidy(fit, conf.int = TRUE),
      error = function(e) NULL
    )
    if (!is.null(td) && all(c("term", "estimate", "std.error", "statistic",
                              "p.value", "conf.low", "conf.high") %in% names(td))) {
      out <- data.frame(
        term       = as.character(td$term),
        estimate   = as.numeric(td$estimate),
        std_error  = as.numeric(td$std.error),
        statistic  = as.numeric(td$statistic),
        p_value    = as.numeric(td$p.value),
        conf_low   = as.numeric(td$conf.low),
        conf_high  = as.numeric(td$conf.high),
        stringsAsFactors = FALSE
      )
      return(out)
    }
  }
  # Manual extraction from fixest objects.
  ct <- fixest::coeftable(fit)
  ci <- tryCatch(confint(fit), error = function(e) NULL)
  term <- rownames(ct)
  estimate <- as.numeric(ct[, "Estimate"])
  std_error <- as.numeric(ct[, "Std. Error"])
  # fixest column for the t/z statistic may be named "t value" or "z value".
  stat_col <- intersect(c("t value", "z value"), colnames(ct))[1]
  statistic <- if (!is.na(stat_col)) as.numeric(ct[, stat_col]) else rep(NA_real_, length(term))
  pval_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  p_value <- if (!is.na(pval_col)) as.numeric(ct[, pval_col]) else rep(NA_real_, length(term))
  if (is.null(ci)) {
    conf_low <- rep(NA_real_, length(term))
    conf_high <- rep(NA_real_, length(term))
  } else {
    conf_low <- as.numeric(ci[, 1])
    conf_high <- as.numeric(ci[, 2])
  }
  data.frame(
    term = term,
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = p_value,
    conf_low = conf_low,
    conf_high = conf_high,
    stringsAsFactors = FALSE
  )
}

#' @title Estimate panel fixed-effects regression from a validated call manifest.
#' @description Applies `sample_filter`, applies `missing_policy`, constructs
#'   the formula exactly from the manifest (no default controls, no extra
#'   FE), builds the vcov / cluster argument per the declared
#'   `covariance_method`, calls `fixest::feols(..., nthreads = 1)` while
#'   capturing warnings, writes the coefficient CSV and model summary text,
#'   and returns a list of artefacts consumed by `write_result_manifest`.
#' @param cm Parsed call manifest list.
#' @param df Loaded data.frame from `cm$input$data_path`.
#' @return A list with elements `fit`, `tidy_coef`, `df_filtered`,
#'   `row_count_before`, `row_count_used`, `dropped_row_count`,
#'   `drop_reason_summary`, `warnings_captured`, `exact_formula_str`.
#' @noRd
estimate_panel_fe <- function(cm, df) {

  row_count_before <- nrow(df)
  drop_log <- character(0)

  # Step 2 — sample_filter
  sample_filter <- cm$input$sample_filter
  if (!is.null(sample_filter) && length(sample_filter) == 1L && nzchar(sample_filter)) {
    n_before_filter <- nrow(df)
    df <- tryCatch(
      df[eval(parse(text = sample_filter), envir = df), , drop = FALSE],
      error = function(e) {
        stop(sprintf("[ESTIMATION ERROR] sample_filter could not be applied: %s",
                     conditionMessage(e)))
      }
    )
    n_dropped <- n_before_filter - nrow(df)
    drop_log <- c(drop_log, sprintf("sample_filter: %d rows dropped", n_dropped))
  }

  # Step 3 — missing_policy
  required_cols <- .char_vec2(cm$input$required_columns)
  policy <- cm$input$missing_policy
  n_before_missing <- nrow(df)

  if (identical(policy, "complete_cases")) {
    df <- df[stats::complete.cases(df[, required_cols, drop = FALSE]), , drop = FALSE]
    n_dropped <- n_before_missing - nrow(df)
    drop_log <- c(drop_log,
                  sprintf("missing_policy (complete_cases): %d rows dropped", n_dropped))
  } else if (identical(policy, "drop_na_outcome")) {
    dep_col <- cm$specification$dependent_variable
    df <- df[!is.na(df[[dep_col]]), , drop = FALSE]
    n_dropped <- n_before_missing - nrow(df)
    drop_log <- c(drop_log,
                  sprintf("missing_policy (drop_na_outcome): %d rows dropped", n_dropped))
  } else if (identical(policy, "fail_if_any_missing")) {
    any_na_cols <- required_cols[
      vapply(required_cols, function(col) any(is.na(df[[col]])), logical(1))
    ]
    if (length(any_na_cols) > 0L) {
      stop(sprintf("[ESTIMATION ERROR] missing_policy is 'fail_if_any_missing' but NA values found in: %s",
                   paste(any_na_cols, collapse = ", ")))
    }
    drop_log <- c(drop_log, "missing_policy (fail_if_any_missing): 0 rows dropped")
  } else {
    stop(sprintf("[ESTIMATION ERROR] unknown missing_policy: %s",
                 if (is.null(policy)) "NULL" else as.character(policy)))
  }

  df_filtered <- df
  row_count_used <- nrow(df_filtered)
  dropped_row_count <- row_count_before - row_count_used

  # Step 4 — build formula
  dep <- cm$specification$dependent_variable
  key <- cm$specification$key_regressor_or_treatment
  ctrls <- .char_vec2(cm$specification$controls)
  fes <- .char_vec2(cm$specification$fixed_effects)

  rhs_terms <- c(key, ctrls)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  rhs_main <- paste(rhs_terms, collapse = " + ")
  fes <- fes[nzchar(fes)]
  if (length(fes) > 0L) {
    fe_part <- paste(fes, collapse = " + ")
    fml_str <- paste0(dep, " ~ ", rhs_main, " | ", fe_part)
  } else {
    fml_str <- paste0(dep, " ~ ", rhs_main)
  }
  fml <- stats::as.formula(fml_str)

  # Step 5 — vcov / cluster
  cov_method <- cm$specification$covariance_method
  clust_fml <- NULL
  nlag_arg <- NULL
  hac_panel_fml <- NULL
  if (identical(cov_method, "clustered")) {
    clust_vars <- .char_vec2(cm$specification$cluster_variables)
    clust_vars <- clust_vars[nzchar(clust_vars)]
    clust_fml <- stats::as.formula(paste0("~", paste(clust_vars, collapse = " + ")))
  } else if (identical(cov_method, "hac")) {
    nlag_arg <- as.integer(cm$specification$hac_settings$lag)
    # fixest::NW requires a panel structure (unit + time) when there are
    # cross-sectional duplicates per time period. The manifest schema does
    # not carry an explicit panel.id field, so we deduce it from data
    # columns using conventional names. Probe in order:
    #   1. ("unit_id", "time_id") — toy fixture / common convention
    #   2. ("unit", "time")
    #   3. ("id", "year") / ("id", "t")
    # If a (unit, time) pair is found, build vcov = NW(lag=k) ~ time + unit.
    # If only a time column is present, build vcov = NW(lag=k) ~ time.
    # Otherwise fall back to plain "NW" with nlag (time-series ordering).
    data_cols <- colnames(df_filtered)
    unit_candidates <- c("unit_id", "unit", "id", "i")
    time_candidates <- c("time_id", "time", "year", "period", "t")
    unit_col <- unit_candidates[unit_candidates %in% data_cols][1]
    time_col <- time_candidates[time_candidates %in% data_cols][1]
    if (!is.na(time_col) && !is.na(unit_col)) {
      hac_panel_fml <- stats::as.formula(paste0("~", time_col, " + ", unit_col))
    } else if (!is.na(time_col)) {
      hac_panel_fml <- stats::as.formula(paste0("~", time_col))
    } else {
      hac_panel_fml <- NULL
    }
  }

  # Step 6 — weights
  wt <- cm$specification$weights
  if (!is.null(wt) && length(wt) == 1L && nzchar(wt)) {
    wt_arg <- df_filtered[[wt]]
  } else {
    wt_arg <- NULL
  }

  # Step 7 — feols, capture warnings
  warnings_captured <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      {
        if (identical(cov_method, "clustered")) {
          fixest::feols(
            fml,
            data = df_filtered,
            cluster = clust_fml,
            weights = wt_arg,
            nthreads = 1
          )
        } else if (identical(cov_method, "hac")) {
          # Build a fixest vcov-request: NW(lag = k) ~ time + unit (or ~ time).
          # If no panel/time was deduced, fall back to a bare "NW" string and
          # let fixest treat the data as a single time series in row order.
          if (!is.null(hac_panel_fml)) {
            nw_request <- fixest::NW(lag = nlag_arg)
            # nw_request is a function-call object; combine with the RHS panel
            # formula using update.formula equivalent: build a formula whose
            # LHS is the NW(lag=k) request and RHS is the panel spec.
            nw_full <- stats::as.formula(
              paste0("NW(lag = ", nlag_arg, ") ~ ",
                     paste(all.vars(hac_panel_fml), collapse = " + "))
            )
            environment(nw_full) <- asNamespace("fixest")
            fixest::feols(
              fml,
              data = df_filtered,
              vcov = nw_full,
              weights = wt_arg,
              nthreads = 1
            )
          } else {
            fixest::feols(
              fml,
              data = df_filtered,
              vcov = "NW",
              weights = wt_arg,
              nthreads = 1
            )
          }
        } else {
          fixest::feols(
            fml,
            data = df_filtered,
            vcov = "iid",
            weights = wt_arg,
            nthreads = 1
          )
        }
      },
      warning = function(w) {
        warnings_captured <<- c(warnings_captured, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(sprintf("[ESTIMATION ERROR] feols() failed: %s", conditionMessage(e)))
    }
  )

  # Step 8 — tidy coefficients
  tidy_coef <- .build_tidy_coef(fit)

  # Step 9 — write coefficient CSV
  .ensure_dir(cm$outputs$coefficient_table_path)
  tryCatch(
    utils::write.csv(tidy_coef,
                     file = cm$outputs$coefficient_table_path,
                     row.names = FALSE),
    error = function(e) {
      stop(sprintf("[OUTPUT ERROR] failed to write coefficient table: %s",
                   conditionMessage(e)))
    }
  )

  # Step 10 — write model summary
  .ensure_dir(cm$outputs$model_summary_path)
  tryCatch(
    {
      con <- file(cm$outputs$model_summary_path, open = "wt")
      sink(con)
      on.exit({
        sink()
        close(con)
      }, add = TRUE)
      print(summary(fit))
    },
    error = function(e) {
      stop(sprintf("[OUTPUT ERROR] failed to write model summary: %s",
                   conditionMessage(e)))
    }
  )

  exact_formula_str <- paste(deparse(stats::formula(fit)), collapse = " ")

  list(
    fit = fit,
    tidy_coef = tidy_coef,
    df_filtered = df_filtered,
    row_count_before = as.integer(row_count_before),
    row_count_used = as.integer(row_count_used),
    dropped_row_count = as.integer(dropped_row_count),
    drop_reason_summary = drop_log,
    warnings_captured = warnings_captured,
    exact_formula_str = exact_formula_str
  )
}
