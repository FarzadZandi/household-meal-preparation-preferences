run_diagnostics <- function(models, cleaned, core_sample, extended_sample) {
  extended_ols <- models$extended_ols
  poisson <- models$poisson_ppml
  interaction <- models$preference_diet_interactions

  cooks <- stats::cooks.distance(extended_ols)
  vif_values <- suppressMessages(car::vif(interaction))
  if (is.matrix(vif_values)) {
    vif_values <- vif_values[, "GVIF^(1/(2*Df))"]
  }

  bp <- lmtest::bptest(extended_ols)
  reset <- lmtest::resettest(extended_ols, power = 2:3, type = "fitted")
  poisson_dispersion <- sum(stats::residuals(poisson, type = "pearson")^2) /
    stats::df.residual(poisson)

  tibble::tibble(
    diagnostic = c(
      "Breusch-Pagan heteroskedasticity p-value",
      "Ramsey RESET p-value",
      "Largest adjusted VIF in interaction model",
      "Poisson Pearson dispersion",
      "Influential observations above Cook's 4/n",
      "Top-coded outcome observations",
      "Exact duplicate analytic rows",
      "Primary complete-case retention",
      "Extended complete-case retention"
    ),
    value = c(
      unname(bp$p.value),
      unname(reset$p.value),
      max(vif_values, na.rm = TRUE),
      poisson_dispersion,
      sum(cooks > 4 / length(cooks)),
      sum(cleaned$meal_preparation_topcoded, na.rm = TRUE),
      sum(duplicated(dplyr::select(cleaned, -respondent_row))),
      nrow(core_sample) / sum(!is.na(cleaned$meal_preparation_count)),
      nrow(extended_sample) / sum(!is.na(cleaned$meal_preparation_count))
    ),
    interpretation = c(
      "Small values indicate non-constant OLS residual variance; HC3 inference is reported.",
      "Small values indicate remaining functional-form misspecification.",
      "Centered interactions reduce artificial collinearity; values above 5 merit review.",
      "Values materially above 1 indicate overdispersion relative to Poisson.",
      "Sensitivity models should be checked when influential observations are common.",
      "The report compares lower-bound encodings of 10 and 11.",
      "Duplicates are retained because the de-identified extract has no respondent key.",
      "Share of respondents with the outcome retained by the primary specification.",
      "Share retained by the extended specification."
    )
  )
}
