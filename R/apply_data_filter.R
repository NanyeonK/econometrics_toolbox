# apply_data_filter.R
#
# Shared sample_filter + missing_policy application for v1-2 did/event_study
# estimators. estimate_panel_fe.R has its own equivalent embedded logic; this
# helper exists so the v1-2 dispatcher can produce identical row counting
# semantics without modifying estimate_panel_fe.R.

#' @title Apply sample_filter and missing_policy to a data.frame.
#' @description Mirrors the filter + missing-policy steps used by
#'   estimate_panel_fe(). Returns the filtered data.frame plus the row
#'   counting bookkeeping that write_result_manifest() expects.
#' @param cm Parsed call manifest list.
#' @param df Raw data.frame loaded from cm$input$data_path.
#' @return list with elements:
#'   - df_filtered: data.frame after sample_filter + missing_policy
#'   - row_count_before: integer (input rows)
#'   - row_count_used: integer (rows after both steps)
#'   - dropped_row_count: integer
#'   - drop_reason_summary: character vector
#' @noRd
apply_data_filter <- function(cm, df) {
  row_count_before <- nrow(df)
  drop_log <- character(0)

  # Step 1 - sample_filter
  sample_filter <- cm$input$sample_filter
  if (!is.null(sample_filter) && nzchar(sample_filter)) {
    keep <- tryCatch(
      with(df, eval(parse(text = sample_filter))),
      error = function(e) stop(sprintf(
        "[ESTIMATION ERROR] sample_filter could not be evaluated: %s",
        conditionMessage(e)
      ))
    )
    if (!is.logical(keep) || length(keep) != nrow(df)) {
      stop("[ESTIMATION ERROR] sample_filter must evaluate to a logical vector with one element per row")
    }
    keep[is.na(keep)] <- FALSE
    n_before <- nrow(df)
    df <- df[keep, , drop = FALSE]
    drop_log <- c(drop_log, sprintf("sample_filter: %d rows dropped",
                                    n_before - nrow(df)))
  } else {
    drop_log <- c(drop_log, "sample_filter: 0 rows dropped (no filter)")
  }

  # Step 2 - missing_policy
  required_cols <- cm$input$required_columns
  if (is.null(required_cols)) required_cols <- character(0)
  required_cols <- as.character(unlist(required_cols))
  required_cols <- intersect(required_cols, names(df))

  policy <- cm$input$missing_policy
  if (identical(policy, "complete_cases")) {
    if (length(required_cols) > 0L) {
      n_before <- nrow(df)
      df <- df[stats::complete.cases(df[, required_cols, drop = FALSE]), , drop = FALSE]
      n_dropped <- n_before - nrow(df)
      drop_log <- c(drop_log,
                    sprintf("missing_policy (complete_cases): %d rows dropped", n_dropped))
    } else {
      drop_log <- c(drop_log, "missing_policy (complete_cases): 0 rows dropped (no required_columns)")
    }
  } else if (identical(policy, "drop_na_outcome")) {
    yname <- cm$specification$dependent_variable
    if (!is.null(yname) && nzchar(yname) && yname %in% names(df)) {
      n_before <- nrow(df)
      df <- df[!is.na(df[[yname]]), , drop = FALSE]
      n_dropped <- n_before - nrow(df)
      drop_log <- c(drop_log,
                    sprintf("missing_policy (drop_na_outcome): %d rows dropped", n_dropped))
    } else {
      drop_log <- c(drop_log, "missing_policy (drop_na_outcome): 0 rows dropped (no dependent_variable in data)")
    }
  } else if (identical(policy, "fail_if_any_missing")) {
    if (length(required_cols) > 0L) {
      bad <- vapply(required_cols, function(col) any(is.na(df[[col]])), logical(1L))
      if (any(bad)) {
        stop(sprintf(
          "[ESTIMATION ERROR] missing_policy is 'fail_if_any_missing' but NA values found in: %s",
          paste(required_cols[bad], collapse = ", ")
        ))
      }
    }
    drop_log <- c(drop_log, "missing_policy (fail_if_any_missing): 0 rows dropped")
  } else {
    stop(sprintf("[ESTIMATION ERROR] unknown missing_policy: %s",
                 if (is.null(policy)) "<NULL>" else as.character(policy)))
  }

  list(
    df_filtered = df,
    row_count_before = as.integer(row_count_before),
    row_count_used = as.integer(nrow(df)),
    dropped_row_count = as.integer(row_count_before - nrow(df)),
    drop_reason_summary = drop_log
  )
}
