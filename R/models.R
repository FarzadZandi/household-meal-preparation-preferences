core_formula <- function() {
  stats::as.formula(
    paste(
      "meal_preparation_count ~ risk_taking + future_orientation + procrastination +",
      "age_decade + is_male + high_education + income_thousand +",
      "healthy_diet + refrigerator_access"
    )
  )
}

extended_formula <- function() {
  stats::update.formula(
    core_formula(),
    . ~ . + food_from_banks + food_sharing + food_thrown_away +
      delivered_meals_change + healthy_diet_change
  )
}

interaction_formula <- function() {
  stats::as.formula(
    paste(
      "meal_preparation_count ~",
      "(risk_taking_centered + future_orientation_centered + procrastination_centered) * healthy_diet_centered +",
      "age_decade + is_male + high_education + income_thousand + refrigerator_access +",
      "food_from_banks + food_sharing + food_thrown_away +",
      "delivered_meals_change + healthy_diet_change"
    )
  )
}

legacy_formula <- function() {
  stats::as.formula(
    paste(
      "meal_preparation_count ~ risk_taking + future_orientation + procrastination +",
      "age_decade + high_education + food_from_banks + food_sharing +",
      "refrigerator_access + food_thrown_away + delivered_meals_changed +",
      "healthy_diet_changed + height_cm + income_thousand"
    )
  )
}

fit_model_suite <- function(core_sample, extended_sample) {
  legacy_sample <- extended_sample |>
    dplyr::filter(stats::complete.cases(height_cm, delivered_meals_changed, healthy_diet_changed))

  topcode10_sample <- extended_sample |>
    dplyr::mutate(
      meal_preparation_count = dplyr::if_else(
        meal_preparation_topcoded,
        10,
        meal_preparation_count
      )
    )

  models <- list(
    core_ols = stats::lm(core_formula(), data = core_sample),
    extended_ols = stats::lm(extended_formula(), data = extended_sample),
    poisson_ppml = stats::glm(extended_formula(), data = extended_sample, family = stats::poisson(link = "log")),
    negative_binomial = MASS::glm.nb(extended_formula(), data = extended_sample),
    preference_diet_interactions = stats::lm(interaction_formula(), data = extended_sample),
    robust_mm = MASS::rlm(extended_formula(), data = extended_sample, method = "MM"),
    topcode10_ols = stats::lm(extended_formula(), data = topcode10_sample),
    legacy_replication = stats::lm(legacy_formula(), data = legacy_sample),
    legacy_plus_gender = stats::lm(
      stats::update.formula(legacy_formula(), . ~ . + is_male),
      data = legacy_sample
    )
  )

  models
}

model_vcov <- function(model) {
  if (inherits(model, "rlm")) return(stats::vcov(model))
  sandwich::vcovHC(model, type = "HC3")
}

tidy_model_robust <- function(model, model_name) {
  vcov_matrix <- model_vcov(model)
  estimates <- stats::coef(model)
  standard_errors <- sqrt(diag(vcov_matrix))
  statistic <- estimates / standard_errors
  p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  is_count_model <- inherits(model, "glm") || inherits(model, "negbin")
  effect_scale_value <- if (is_count_model) "log count" else "meals"

  tibble::tibble(
    model = model_name,
    term = names(estimates),
    estimate = unname(estimates),
    robust_std_error = unname(standard_errors),
    statistic = unname(statistic),
    p_value = unname(p_value),
    conf_low = estimate - stats::qnorm(0.975) * robust_std_error,
    conf_high = estimate + stats::qnorm(0.975) * robust_std_error,
    effect_scale = effect_scale_value
  ) |>
    dplyr::mutate(
      incidence_rate_ratio = dplyr::if_else(effect_scale == "log count", exp(estimate), NA_real_),
      irr_conf_low = dplyr::if_else(effect_scale == "log count", exp(conf_low), NA_real_),
      irr_conf_high = dplyr::if_else(effect_scale == "log count", exp(conf_high), NA_real_)
    )
}

combine_model_coefficients <- function(models) {
  dplyr::bind_rows(Map(tidy_model_robust, models, names(models)))
}

glance_model <- function(model, model_name) {
  is_lm <- inherits(model, "lm") && !inherits(model, c("glm", "rlm"))
  observations <- if (inherits(model, "rlm")) length(stats::residuals(model)) else stats::nobs(model)
  aic_value <- if (inherits(model, "rlm")) NA_real_ else stats::AIC(model)
  bic_value <- if (inherits(model, "rlm")) NA_real_ else stats::BIC(model)
  r_squared_value <- if (is_lm) summary(model)$r.squared else NA_real_
  adjusted_r_squared_value <- if (is_lm) summary(model)$adj.r.squared else NA_real_
  residual_sigma_value <- if (is_lm) summary(model)$sigma else NA_real_
  tibble::tibble(
    model = model_name,
    observations = observations,
    aic = aic_value,
    bic = bic_value,
    r_squared = r_squared_value,
    adjusted_r_squared = adjusted_r_squared_value,
    residual_sigma = residual_sigma_value
  )
}

combine_model_fit <- function(models) {
  dplyr::bind_rows(Map(glance_model, models, names(models)))
}
