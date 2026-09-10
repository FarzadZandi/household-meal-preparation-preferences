test_that("model formulas include the three primary predictors", {
  terms <- attr(stats::terms(core_formula()), "term.labels")
  expect_true(all(c("risk_taking", "future_orientation", "procrastination") %in% terms))
})

test_that("count and OLS models fit a valid synthetic analysis sample", {
  set.seed(1)
  n <- 120
  sample <- tibble::tibble(
    meal_preparation_count = stats::rnbinom(n, mu = 4, size = 3),
    meal_preparation_topcoded = FALSE,
    risk_taking = stats::runif(n, 0, 10),
    future_orientation = stats::runif(n, 0, 10),
    procrastination = stats::runif(n, 0, 10),
    risk_taking_centered = risk_taking - mean(risk_taking),
    future_orientation_centered = future_orientation - mean(future_orientation),
    procrastination_centered = procrastination - mean(procrastination),
    age_decade = stats::runif(n, 2, 8),
    is_male = stats::rbinom(n, 1, 0.5),
    high_education = stats::rbinom(n, 1, 0.5),
    income_thousand = stats::runif(n, 0.25, 10),
    healthy_diet = sample(1:6, n, replace = TRUE),
    healthy_diet_centered = healthy_diet - mean(healthy_diet),
    refrigerator_access = stats::rbinom(n, 1, 0.9),
    food_from_banks = stats::rbinom(n, 1, 0.05),
    food_sharing = stats::rbinom(n, 1, 0.05),
    food_thrown_away = stats::rbinom(n, 1, 0.3),
    delivered_meals_change = sample(-2:2, n, replace = TRUE),
    healthy_diet_change = sample(-2:2, n, replace = TRUE),
    delivered_meals_changed = stats::rbinom(n, 1, 0.5),
    healthy_diet_changed = stats::rbinom(n, 1, 0.5),
    height_cm = stats::rnorm(n, 172, 9)
  )
  models <- fit_model_suite(sample, sample)
  expect_s3_class(models$core_ols, "lm")
  expect_s3_class(models$poisson_ppml, "glm")
  expect_true(inherits(models$negative_binomial, "negbin"))
})
