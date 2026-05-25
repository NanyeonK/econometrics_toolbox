# write_result_manifest.R
#
# Assembles the result manifest list following the v1 schema and writes it
# as YAML.

#' @title Look up a package version string, returning "not_installed" if absent.
#' @description Used to populate `call.package_versions.*` in the result
#'   manifest. Never errors when the package is missing.
#' @param pkg Character scalar package name.
#' @return Character scalar — either the `packageVersion` string or
#'   `"not_installed"`.
#' @noRd
.pkg_ver <- function(pkg) {
  v <- tryCatch(utils::packageVersion(pkg), error = function(e) NULL)
  if (is.null(v)) "not_installed" else as.character(v)
}

#' @title Normalise an absent / NULL / scalar / list value to a character vector.
#' @description Local helper duplicating `.char_vec` from
#'   `validate_call_manifest.R` so the writer can be sourced standalone.
#' @param x Any R object.
#' @return Character vector (possibly empty).
#' @noRd
.char_vec3 <- function(x) {
  if (is.null(x)) return(character(0))
  if (is.list(x)) return(unlist(x, use.names = FALSE))
  as.character(x)
}

#' @title Coerce a possibly-NULL / possibly-empty value to a character scalar.
#' @description Used to copy optional manifest fields into the result
#'   manifest, falling back to a supplied default (typically `""`) when
#'   the field is absent.
#' @param x Any R object.
#' @param default Character scalar to return when `x` is NULL or empty.
#' @return Character scalar.
#' @noRd
.maybe_string <- function(x, default = "") {
  if (is.null(x)) return(default)
  if (length(x) == 0L) return(default)
  as.character(x)
}

# Wrap a (possibly empty) character vector so yaml::write_yaml emits a list.
#' @title Wrap a character vector so YAML output renders it as a list.
#' @description `yaml::write_yaml` collapses a length-0 character vector
#'   into nothing; this helper forces it to be emitted as an empty YAML
#'   list `[]`. Non-empty vectors are converted element-by-element into a
#'   list of strings.
#' @param x Character vector (may be empty).
#' @return List of character scalars (may be empty list).
#' @noRd
.as_list_of_strings <- function(x) {
  if (length(x) == 0L) return(list())
  as.list(as.character(x))
}

#' @title Assemble and write the econometrics result manifest YAML.
#' @description Collects R and package versions, computes the four drift
#'   signatures from `drift_metadata.R`, assembles the v1 result-manifest
#'   list (header, call, input, model, outputs, warnings, drift_metadata,
#'   verdict, blocking_reasons), and serialises it with
#'   `yaml::write_yaml`. Sets `verdict = "PASS"` and `blocking_reasons = []`
#'   on success. Creates the parent directory if needed.
#' @param cm Parsed call manifest list.
#' @param est_result List returned by `estimate_panel_fe`.
#' @param call_manifest_path Absolute path to the source call manifest YAML.
#' @return `invisible(rm)` — the assembled manifest list.
#' @noRd
write_result_manifest <- function(cm, est_result, call_manifest_path) {

  # ---- Drift signatures ------------------------------------------------------
  sample_sig <- compute_sample_signature(
    data_hash = .maybe_string(cm$input$data_hash, ""),
    row_count_used = est_result$row_count_used,
    sample_filter = .maybe_string(cm$input$sample_filter, "")
  )
  spec_sig <- compute_spec_signature(
    controls = .char_vec3(cm$specification$controls),
    dependent_variable = cm$specification$dependent_variable,
    estimator = cm$specification$estimator,
    fixed_effects = .char_vec3(cm$specification$fixed_effects),
    key_regressor_or_treatment = cm$specification$key_regressor_or_treatment,
    method_family = cm$specification$method_family,
    weights = .maybe_string(cm$specification$weights, "")
  )
  inference_sig <- compute_inference_signature(
    cluster_variables = .char_vec3(cm$specification$cluster_variables),
    covariance_method = cm$specification$covariance_method,
    hac_settings = cm$specification$hac_settings
  )
  preproc_sig <- compute_preprocessing_signature(
    missing_policy = cm$input$missing_policy,
    source_preprocessing_spec = .maybe_string(cm$call$source_preprocessing_spec, "")
  )

  # ---- HAC settings (with lag preserved as NA -> null) -----------------------
  hac_lag <- cm$specification$hac_settings$lag
  hac_block <- list(
    enabled = isTRUE(cm$specification$hac_settings$enabled),
    lag = if (is.null(hac_lag)) NULL else as.integer(hac_lag),
    kernel = .maybe_string(cm$specification$hac_settings$kernel, "")
  )

  rm <- list(
    template = "econometrics_result_manifest",
    version = "v1",
    updated = format(Sys.time(), "%Y-%m-%d", tz = "UTC"),
    project = .maybe_string(cm$project, ""),
    phase = .maybe_string(cm$phase, ""),
    call = list(
      call_id = .maybe_string(cm$call$call_id, ""),
      call_manifest_path = as.character(call_manifest_path),
      backend_language = "R",
      r_entrypoint = .maybe_string(cm$call$r_entrypoint, ""),
      r_version = R.version.string,
      package_versions = list(
        fixest = .pkg_ver("fixest"),
        sandwich = .pkg_ver("sandwich"),
        lmtest = .pkg_ver("lmtest"),
        broom = .pkg_ver("broom")
      )
    ),
    input = list(
      data_path = .maybe_string(cm$input$data_path, ""),
      data_hash = .maybe_string(cm$input$data_hash, ""),
      row_count_before_filter = est_result$row_count_before,
      row_count_used = est_result$row_count_used,
      dropped_row_count = est_result$dropped_row_count,
      drop_reason_summary = .as_list_of_strings(est_result$drop_reason_summary)
    ),
    model = list(
      method_family = .maybe_string(cm$specification$method_family, ""),
      estimator = .maybe_string(cm$specification$estimator, ""),
      exact_formula = est_result$exact_formula_str,
      dependent_variable = .maybe_string(cm$specification$dependent_variable, ""),
      key_regressor_or_treatment = .maybe_string(cm$specification$key_regressor_or_treatment, ""),
      controls = .as_list_of_strings(.char_vec3(cm$specification$controls)),
      fixed_effects = .as_list_of_strings(.char_vec3(cm$specification$fixed_effects)),
      weights = .maybe_string(cm$specification$weights, ""),
      covariance_method = .maybe_string(cm$specification$covariance_method, ""),
      cluster_variables = .as_list_of_strings(.char_vec3(cm$specification$cluster_variables)),
      hac_settings = hac_block
    ),
    outputs = list(
      coefficient_table_path = .maybe_string(cm$outputs$coefficient_table_path, ""),
      model_summary_path = .maybe_string(cm$outputs$model_summary_path, ""),
      diagnostics_path = "",
      log_path = .maybe_string(cm$outputs$log_path, "")
    ),
    warnings = .as_list_of_strings(est_result$warnings_captured),
    drift_metadata = list(
      sample_signature = sample_sig,
      spec_signature = spec_sig,
      inference_signature = inference_sig,
      preprocessing_signature = preproc_sig
    ),
    verdict = "PASS",
    blocking_reasons = list()
  )

  # Ensure output directory exists.
  out_path <- cm$outputs$result_manifest_path
  out_dir <- dirname(out_path)
  if (nzchar(out_dir) && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  tryCatch(
    yaml::write_yaml(rm, out_path),
    error = function(e) {
      stop(sprintf("[OUTPUT ERROR] failed to write result manifest: %s",
                   conditionMessage(e)))
    }
  )

  invisible(rm)
}
