#' Regress an outcome on a set of predictors and tidy the coefficients
#'
#' @param .data A cross-sectional data frame with one row per country.
#' @param outcome Character string. Column name of the dependent variable.
#' @param predictors Character vector. Column names of the regressors.
#'
#' @return A tibble with one row per estimated coefficient and columns
#'   \code{outcome}, \code{term}, \code{estimate}, \code{std_error},
#'   \code{p_value}, \code{conf_low}, \code{conf_high}, \code{n_obs} and
#'   \code{r_squared}.
fit_outcome_model <- function(.data, outcome, predictors) {
  model <- lm(
    reformulate(predictors, response = outcome),
    data = .data
  )

  model_summary <- summary(model)
  coefficients <- model_summary[["coefficients"]]
  confidence_intervals <- confint(model)[rownames(coefficients), , drop = FALSE]

  tibble::tibble(
    outcome = outcome,
    term = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    p_value = coefficients[, "Pr(>|t|)"],
    conf_low = confidence_intervals[, 1],
    conf_high = confidence_intervals[, 2],
    n_obs = nobs(model),
    r_squared = model_summary[["r.squared"]]
  )
}
