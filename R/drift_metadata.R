# drift_metadata.R
#
# Four SHA-256 signature functions for drift detection.
# Canonical JSON construction rule:
#   1. Construct a named R list with the contributing fields.
#   2. Sort list keys alphabetically at every nesting level.
#   3. Serialize with jsonlite::toJSON(..., auto_unbox = TRUE, null = "null").
#   4. Compute SHA-256 with digest::digest(canonical_str, algo = "sha256",
#                                          serialize = FALSE).

#' @title Recursively sort a named list by key at every nesting level.
#' @description Used as the first step of canonical-JSON construction so
#'   two payloads with the same key/value pairs in different insertion
#'   orders produce byte-identical JSON (and therefore byte-identical
#'   SHA-256 digests).
#' @param x A (possibly nested) named list.
#' @return The same list with keys alphabetised at every level.
#' @noRd
.sort_named_list <- function(x) {
  # Sort top-level names; recurse into named lists.
  if (is.list(x) && !is.null(names(x)) && length(x) > 0L) {
    x <- x[order(names(x))]
    x <- lapply(x, function(el) {
      if (is.list(el) && !is.null(names(el))) .sort_named_list(el) else el
    })
  }
  x
}

#' @title Serialise a named list as deterministic canonical JSON.
#' @description Sorts keys with `.sort_named_list`, then serialises with
#'   `jsonlite::toJSON(..., auto_unbox = TRUE, null = "null")`. Result is
#'   compact, key-sorted UTF-8 with no trailing whitespace.
#' @param lst A (possibly nested) named list.
#' @return Length-1 character vector containing the JSON string.
#' @noRd
.canonical_json <- function(lst) {
  sorted <- .sort_named_list(lst)
  jsonlite::toJSON(sorted, auto_unbox = TRUE, null = "null")
}

#' @title SHA-256 hex digest of a UTF-8 string.
#' @description Thin wrapper over
#'   `digest::digest(s, algo = "sha256", serialize = FALSE)`.
#' @param s Character scalar.
#' @return 64-character lower-case hex digest string.
#' @noRd
.sha256_hex <- function(s) {
  digest::digest(s, algo = "sha256", serialize = FALSE)
}

#' @title Compute the sample drift signature.
#' @description Hashes the canonical JSON of
#'   `{data_hash, row_count_used, sample_filter}` (keys sorted) so any
#'   change to the data hash, the row count actually used, or the
#'   declared sample filter produces a different digest.
#' @param data_hash Character scalar (may be `""`).
#' @param row_count_used Integer; rows actually used in estimation.
#' @param sample_filter Character scalar (may be `""`).
#' @return SHA-256 hex digest string (64 lower-case hex characters).
#' @noRd
compute_sample_signature <- function(data_hash, row_count_used, sample_filter) {
  if (is.null(data_hash)) data_hash <- ""
  if (is.null(sample_filter)) sample_filter <- ""
  if (is.null(row_count_used)) row_count_used <- 0L
  payload <- list(
    data_hash = as.character(data_hash),
    row_count_used = as.integer(row_count_used),
    sample_filter = as.character(sample_filter)
  )
  .sha256_hex(.canonical_json(payload))
}

#' @title Compute the specification drift signature.
#' @description Hashes the canonical JSON of the regression specification
#'   (dependent variable, key regressor, controls, fixed effects, weights,
#'   estimator, method family). `controls` and `fixed_effects` are sorted
#'   alphabetically before serialisation so set-equivalent specifications
#'   collide.
#' @param controls Character vector of control variable names (may be empty).
#' @param dependent_variable Character scalar.
#' @param estimator Character scalar (e.g. `"fixest_feols"`).
#' @param fixed_effects Character vector of FE variable names (may be empty).
#' @param key_regressor_or_treatment Character scalar.
#' @param method_family Character scalar (e.g. `"panel_fe_regression"`).
#' @param weights Character scalar (variable name or `""`).
#' @return SHA-256 hex digest string (64 lower-case hex characters).
#' @noRd
compute_spec_signature <- function(controls, dependent_variable, estimator,
                                   fixed_effects, key_regressor_or_treatment,
                                   method_family, weights) {
  if (is.null(controls)) controls <- character(0)
  if (is.null(fixed_effects)) fixed_effects <- character(0)
  if (is.null(weights)) weights <- ""
  controls_sorted <- sort(as.character(controls))
  fes_sorted <- sort(as.character(fixed_effects))
  # Keep empty vectors as [] in JSON via I()
  payload <- list(
    controls = if (length(controls_sorted) == 0L) I(list()) else controls_sorted,
    dependent_variable = as.character(dependent_variable),
    estimator = as.character(estimator),
    fixed_effects = if (length(fes_sorted) == 0L) I(list()) else fes_sorted,
    key_regressor_or_treatment = as.character(key_regressor_or_treatment),
    method_family = as.character(method_family),
    weights = as.character(weights)
  )
  .sha256_hex(.canonical_json(payload))
}

#' @title Compute the inference drift signature.
#' @description Hashes the canonical JSON of
#'   `{cluster_variables, covariance_method, hac_settings}`.
#'   `cluster_variables` is sorted alphabetically and `hac_settings`
#'   sub-keys are sorted; a NULL HAC lag is rendered as JSON `null`.
#' @param cluster_variables Character vector (may be empty).
#' @param covariance_method One of `"clustered"`, `"hac"`, `"iid"`.
#' @param hac_settings List with elements `enabled`, `lag`, `kernel`.
#' @return SHA-256 hex digest string (64 lower-case hex characters).
#' @noRd
compute_inference_signature <- function(cluster_variables, covariance_method, hac_settings) {
  if (is.null(cluster_variables)) cluster_variables <- character(0)
  clust_sorted <- sort(as.character(cluster_variables))
  if (is.null(hac_settings)) hac_settings <- list()
  hac_enabled <- isTRUE(hac_settings$enabled)
  hac_lag <- hac_settings$lag
  hac_kernel <- if (is.null(hac_settings$kernel)) "" else as.character(hac_settings$kernel)
  hac_payload <- list(
    enabled = hac_enabled,
    kernel = hac_kernel,
    lag = if (is.null(hac_lag)) NA else as.integer(hac_lag)
  )
  # NA -> null in jsonlite when na = "null"; we want explicit null for lag when absent.
  # Use a small post-processing step to set lag explicitly.
  payload <- list(
    cluster_variables = if (length(clust_sorted) == 0L) I(list()) else clust_sorted,
    covariance_method = as.character(covariance_method),
    hac_settings = hac_payload
  )
  # Build canonical JSON with null for lag if needed.
  sorted <- .sort_named_list(payload)
  json_str <- jsonlite::toJSON(sorted, auto_unbox = TRUE, na = "null", null = "null")
  .sha256_hex(json_str)
}

#' @title Compute the preprocessing drift signature.
#' @description Hashes the canonical JSON of
#'   `{missing_policy, source_preprocessing_spec}` so any change to the
#'   declared NA-handling policy or to the upstream preprocessing-spec
#'   reference is observable downstream.
#' @param missing_policy Character scalar — one of `"complete_cases"`,
#'   `"drop_na_outcome"`, `"fail_if_any_missing"`.
#' @param source_preprocessing_spec Character scalar (may be `""`).
#' @return SHA-256 hex digest string (64 lower-case hex characters).
#' @noRd
compute_preprocessing_signature <- function(missing_policy, source_preprocessing_spec) {
  if (is.null(missing_policy)) missing_policy <- ""
  if (is.null(source_preprocessing_spec)) source_preprocessing_spec <- ""
  payload <- list(
    missing_policy = as.character(missing_policy),
    source_preprocessing_spec = as.character(source_preprocessing_spec)
  )
  .sha256_hex(.canonical_json(payload))
}
