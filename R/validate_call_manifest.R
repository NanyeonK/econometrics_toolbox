# validate_call_manifest.R
#
# Fail-closed validation of a parsed call manifest list.
#
# Group A: top-level structural checks
# Group B: specification structural checks
# Group C: output path checks
# Group D: data-level checks (run after data is loaded; pass df to enable)
#
# On any violation, calls stop() with a "[VALIDATION ERROR] ..." message.
# Returns invisible(TRUE) on success.

#' @title Test whether a value is a single non-empty, non-NA character string.
#' @description Internal predicate used throughout the validator to enforce
#'   "required, non-empty string" semantics on manifest fields.
#' @param x Any R object.
#' @return Logical scalar.
#' @noRd
.is_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

#' @title Test whether a value is a single positive integer.
#' @description Used to validate `hac_settings.lag` (which YAML may load as
#'   numeric). Accepts any numeric scalar whose value equals its integer cast
#'   and is strictly greater than zero.
#' @param x Any R object.
#' @return Logical scalar.
#' @noRd
.is_positive_integer <- function(x) {
  if (is.null(x)) return(FALSE)
  if (length(x) != 1L) return(FALSE)
  if (is.na(x)) return(FALSE)
  if (!is.numeric(x)) return(FALSE)
  x_int <- suppressWarnings(as.integer(x))
  if (is.na(x_int)) return(FALSE)
  if (x_int != x) return(FALSE)
  x_int > 0L
}

#' @title Normalise an absent / NULL / scalar / list value to a character vector.
#' @description Manifest list fields may arrive from `yaml::read_yaml` as
#'   NULL, a length-1 string, or a list of strings depending on the YAML
#'   shape. This helper coerces all three into a `character(0+)` for
#'   uniform downstream handling.
#' @param x Any R object.
#' @return Character vector (possibly empty).
#' @noRd
.char_vec <- function(x) {
  # Normalise an absent / NULL / single-string / list-of-strings into a character vector.
  if (is.null(x)) return(character(0))
  if (is.list(x)) return(unlist(x, use.names = FALSE))
  as.character(x)
}

#' @title Validate a parsed econometrics call manifest.
#' @description Runs all fail-closed checks on a call manifest list. Group A
#'   covers top-level structural fields, Group B covers `specification`,
#'   Group C covers `outputs`. Group D (data-level: column presence,
#'   `missing_policy` enum) runs only when `df` is supplied. On any
#'   violation, calls `stop()` with a `"[VALIDATION ERROR] ..."` message;
#'   on success returns invisibly.
#' @param cm A list, typically produced by `yaml::read_yaml`.
#' @param df Optional data.frame. When supplied, Group D data-level checks run.
#' @return `invisible(TRUE)` on success; otherwise `stop()` is called.
#' @noRd
validate_call_manifest <- function(cm, df = NULL) {

  # ---- Group A: top-level structural checks ----------------------------------
  if (!.is_nonempty_string(cm$call$call_id)) {
    stop("[VALIDATION ERROR] call.call_id is missing or empty")
  }
  bl <- cm$call$backend_language
  if (!identical(bl, "R")) {
    stop(sprintf("[VALIDATION ERROR] backend_language must be 'R', got: %s",
                 if (is.null(bl)) "NULL" else as.character(bl)))
  }
  if (!.is_nonempty_string(cm$call$allowed_reason)) {
    stop("[VALIDATION ERROR] call.allowed_reason is missing or empty")
  }
  if (!.is_nonempty_string(cm$call$source_empirical_plan)) {
    stop("[VALIDATION ERROR] call.source_empirical_plan is missing or empty")
  }
  if (!.is_nonempty_string(cm$call$r_entrypoint)) {
    stop("[VALIDATION ERROR] call.r_entrypoint is missing or empty")
  }
  if (!.is_nonempty_string(cm$input$data_path)) {
    stop("[VALIDATION ERROR] input.data_path is missing or empty")
  }
  if (!file.exists(cm$input$data_path)) {
    stop(sprintf("[VALIDATION ERROR] input.data_path does not exist: %s",
                 cm$input$data_path))
  }
  if (!identical(cm$failure_policy, "fail_closed")) {
    stop("[VALIDATION ERROR] failure_policy must be 'fail_closed'")
  }

  # ---- Group B: specification structural checks ------------------------------
  mf <- cm$specification$method_family
  if (!.is_nonempty_string(mf) ||
      !(mf %in% c("panel_fe_regression", "did", "event_study"))) {
    stop("[VALIDATION ERROR] specification.method_family must be one of: panel_fe_regression, did, event_study")
  }
  # estimator enum is only strict for panel_fe_regression; other families have
  # their own estimator label conventions (informational), checked by Group E.
  if (identical(mf, "panel_fe_regression")) {
    if (!identical(cm$specification$estimator, "fixest_feols")) {
      stop("[VALIDATION ERROR] specification.estimator must be 'fixest_feols'")
    }
  }
  if (!.is_nonempty_string(cm$specification$dependent_variable)) {
    stop("[VALIDATION ERROR] specification.dependent_variable is missing or empty")
  }
  if (!.is_nonempty_string(cm$specification$key_regressor_or_treatment)) {
    stop("[VALIDATION ERROR] specification.key_regressor_or_treatment is missing or empty")
  }
  cov_method <- cm$specification$covariance_method
  if (!.is_nonempty_string(cov_method) ||
      !(cov_method %in% c("clustered", "hac", "iid"))) {
    stop("[VALIDATION ERROR] specification.covariance_method must be one of: clustered, hac, iid")
  }

  cluster_vars <- .char_vec(cm$specification$cluster_variables)
  if (identical(cov_method, "clustered")) {
    if (length(cluster_vars) == 0L || all(!nzchar(cluster_vars))) {
      stop("[VALIDATION ERROR] cluster_variables must be non-empty when covariance_method is 'clustered'")
    }
  }

  hac_enabled <- isTRUE(cm$specification$hac_settings$enabled)
  if (hac_enabled) {
    lag_val <- cm$specification$hac_settings$lag
    if (!.is_positive_integer(lag_val)) {
      stop("[VALIDATION ERROR] hac_settings.lag must be a positive integer when hac_settings.enabled is true")
    }
  }

  if (identical(cov_method, "hac")) {
    panel <- cm$specification$panel
    if (is.null(panel) || !is.list(panel)) {
      stop("[VALIDATION ERROR] specification.panel block is required when covariance_method is 'hac'")
    }
    if (!.is_nonempty_string(panel$unit)) {
      stop("[VALIDATION ERROR] specification.panel.unit must be a non-empty string when covariance_method is 'hac'")
    }
    if (!.is_nonempty_string(panel$time)) {
      stop("[VALIDATION ERROR] specification.panel.time must be a non-empty string when covariance_method is 'hac'")
    }
  }

  # ---- Group C: output path checks -------------------------------------------
  if (!.is_nonempty_string(cm$outputs$result_manifest_path)) {
    stop("[VALIDATION ERROR] outputs.result_manifest_path is missing or empty")
  }
  if (!.is_nonempty_string(cm$outputs$coefficient_table_path)) {
    stop("[VALIDATION ERROR] outputs.coefficient_table_path is missing or empty")
  }
  if (!.is_nonempty_string(cm$outputs$model_summary_path)) {
    stop("[VALIDATION ERROR] outputs.model_summary_path is missing or empty")
  }

  # ---- Group D: data-level checks (only when df is provided) -----------------
  if (!is.null(df)) {
    cols <- colnames(df)
    required_cols <- .char_vec(cm$input$required_columns)
    if (length(required_cols) == 0L) {
      stop("[VALIDATION ERROR] input.required_columns is missing or empty")
    }
    missing_req <- setdiff(required_cols, cols)
    if (length(missing_req) > 0L) {
      stop(sprintf("[VALIDATION ERROR] required columns missing from data: %s",
                   paste(missing_req, collapse = ", ")))
    }

    dep <- cm$specification$dependent_variable
    if (!(dep %in% cols)) {
      stop(sprintf("[VALIDATION ERROR] dependent_variable '%s' not found in data", dep))
    }
    key <- cm$specification$key_regressor_or_treatment
    if (!(key %in% cols)) {
      stop(sprintf("[VALIDATION ERROR] key_regressor_or_treatment '%s' not found in data", key))
    }

    ctrls <- .char_vec(cm$specification$controls)
    for (c in ctrls) {
      if (nzchar(c) && !(c %in% cols)) {
        stop(sprintf("[VALIDATION ERROR] control variable '%s' not found in data", c))
      }
    }
    fes <- .char_vec(cm$specification$fixed_effects)
    for (f in fes) {
      if (nzchar(f) && !(f %in% cols)) {
        stop(sprintf("[VALIDATION ERROR] fixed_effect variable '%s' not found in data", f))
      }
    }
    for (cv in cluster_vars) {
      if (nzchar(cv) && !(cv %in% cols)) {
        stop(sprintf("[VALIDATION ERROR] cluster variable '%s' not found in data", cv))
      }
    }
    wt <- cm$specification$weights
    if (!is.null(wt) && length(wt) == 1L && nzchar(wt)) {
      if (!(wt %in% cols)) {
        stop(sprintf("[VALIDATION ERROR] weights variable '%s' not found in data", wt))
      }
    }

    if (identical(cov_method, "hac")) {
      if (!(cm$specification$panel$unit %in% cols)) {
        stop(sprintf("[VALIDATION ERROR] panel.unit '%s' not found in data",
                     cm$specification$panel$unit))
      }
      if (!(cm$specification$panel$time %in% cols)) {
        stop(sprintf("[VALIDATION ERROR] panel.time '%s' not found in data",
                     cm$specification$panel$time))
      }
    }

    policy <- cm$input$missing_policy
    if (!.is_nonempty_string(policy) ||
        !(policy %in% c("complete_cases", "drop_na_outcome", "fail_if_any_missing"))) {
      stop("[VALIDATION ERROR] input.missing_policy must be one of: complete_cases, drop_na_outcome, fail_if_any_missing")
    }
  }

  # ---- Group E: method_family + variant validation (v1-2) --------------------
  # Runs after Group D so it can also reference df when supplied. Returns
  # invisibly if all checks pass; stops on first violation per existing
  # validator convention.
  errs <- validate_group_e(cm, df)
  if (length(errs) > 0L) {
    stop(errs[[1L]])
  }

  invisible(TRUE)
}

#' @title Group E: method-family + variant validation (v1-2).
#' @description Enforces did/event_study variant requirements per spec.md §4.
#'   Group A's enum check on method_family is assumed to have already run; this
#'   helper does not re-emit that error. Returns a character vector of error
#'   messages (empty = pass). Stops at first violation per existing convention,
#'   so the returned vector contains 0 or 1 element.
#' @param cm Parsed call manifest list.
#' @param df Optional data.frame for column-presence and domain checks (E7/E8).
#' @return character(0) on pass, otherwise length-1 character with the error.
#' @noRd
validate_group_e <- function(cm, df = NULL) {
  mf <- cm$specification$method_family
  # Group A guarantees mf is in the allowed enum; if not, return empty (Group A
  # already errored). Defensive: if Group A is bypassed in tests, treat
  # unrecognised mf as "no E checks to run".
  if (!.is_nonempty_string(mf) ||
      !(mf %in% c("panel_fe_regression", "did", "event_study"))) {
    return(character(0))
  }

  did_v   <- cm$specification$did_variant
  evs_v   <- cm$specification$event_study_variant
  cohort  <- cm$specification$cohort_var
  ttt     <- cm$specification$time_to_treat_var
  treat   <- cm$specification$treatment_indicator

  did_enum <- c("twfe", "sun_abraham", "callaway_santanna", "dchd", "bjs")
  evs_enum <- c("classical", "sun_abraham")

  # E2: did requires did_variant in enum
  if (identical(mf, "did")) {
    if (!.is_nonempty_string(did_v) || !(did_v %in% did_enum)) {
      return(paste0(
        "[VALIDATION ERROR] specification.did_variant missing or not in enum ",
        "{twfe, sun_abraham, callaway_santanna, dchd, bjs}"))
    }
  }

  # E3: event_study requires event_study_variant in enum
  if (identical(mf, "event_study")) {
    if (!.is_nonempty_string(evs_v) || !(evs_v %in% evs_enum)) {
      return(paste0(
        "[VALIDATION ERROR] specification.event_study_variant missing or not in enum ",
        "{classical, sun_abraham}"))
    }
  }

  # E4: mismatch — variant set but family does not match
  if (.is_nonempty_string(did_v) && !identical(mf, "did")) {
    return(sprintf(
      "[VALIDATION ERROR] method_family.mismatch: did_variant=%s incompatible with method_family=%s",
      did_v, mf))
  }
  if (.is_nonempty_string(evs_v) && !identical(mf, "event_study")) {
    return(sprintf(
      "[VALIDATION ERROR] method_family.mismatch: event_study_variant=%s incompatible with method_family=%s",
      evs_v, mf))
  }

  # E5: per-variant required-field matrix
  variant_required <- function(family, variant) {
    if (identical(family, "did")) {
      switch(variant,
        twfe              = "treatment_indicator",
        sun_abraham       = "cohort_var",
        callaway_santanna = "cohort_var",
        dchd              = "treatment_indicator",
        bjs               = "cohort_var",
        NULL
      )
    } else if (identical(family, "event_study")) {
      switch(variant,
        classical    = "time_to_treat_var",
        sun_abraham  = "cohort_var",
        NULL
      )
    } else {
      NULL
    }
  }

  variant <- if (identical(mf, "did")) did_v
             else if (identical(mf, "event_study")) evs_v
             else NULL
  req_field <- variant_required(mf, variant)
  if (!is.null(req_field)) {
    req_val <- cm$specification[[req_field]]
    if (!.is_nonempty_string(req_val)) {
      return(sprintf(
        "[VALIDATION ERROR] specification.%s required for %s/%s",
        req_field, mf, variant))
    }
  }

  # E6: panel.unit / panel.time required when method_family in {did, event_study}
  if (mf %in% c("did", "event_study")) {
    panel <- cm$specification$panel
    if (is.null(panel) || !is.list(panel) ||
        !.is_nonempty_string(panel$unit)) {
      return(sprintf(
        "[VALIDATION ERROR] specification.panel.unit required for method_family=%s",
        mf))
    }
    if (!.is_nonempty_string(panel$time)) {
      return(sprintf(
        "[VALIDATION ERROR] specification.panel.time required for method_family=%s",
        mf))
    }
  }

  # E9 (type-check only): reference_periods, when present, must be coercible to
  # a vector of integers.
  ref_p <- cm$specification$reference_periods
  if (!is.null(ref_p)) {
    rp_vec <- tryCatch(suppressWarnings(as.integer(unlist(ref_p, use.names = FALSE))),
                       error = function(e) NA_integer_)
    if (any(is.na(rp_vec))) {
      return("[VALIDATION ERROR] specification.reference_periods must be a list of integers")
    }
  }

  # E7 + E8: column-presence + treatment_indicator domain (need df)
  if (!is.null(df)) {
    cols <- colnames(df)
    required_cols <- .char_vec(cm$input$required_columns)
    variant_fields <- list(
      cohort_var          = cohort,
      time_to_treat_var   = ttt,
      treatment_indicator = treat
    )
    for (fname in names(variant_fields)) {
      v <- variant_fields[[fname]]
      if (.is_nonempty_string(v)) {
        if (!(v %in% cols)) {
          return(sprintf(
            "[VALIDATION ERROR] specification.%s='%s' not present in loaded data",
            fname, v))
        }
        if (!(v %in% required_cols)) {
          return(sprintf(
            "[VALIDATION ERROR] specification.%s='%s' not declared in required_columns",
            fname, v))
        }
      }
    }
    # E8: treatment_indicator domain {0, 1, NA}
    if (.is_nonempty_string(treat) && (treat %in% cols)) {
      col_vals <- df[[treat]]
      ok_vals <- is.na(col_vals) | (suppressWarnings(as.numeric(col_vals)) %in% c(0, 1))
      if (!all(ok_vals)) {
        return(sprintf(
          "[VALIDATION ERROR] specification.treatment_indicator='%s' has values outside {0,1}",
          treat))
      }
    }
  }

  # E10: control_group / anticipation_periods only meaningful for cs/bjs —
  # validator silently accepts on others (belt-and-suspenders).

  character(0)
}
