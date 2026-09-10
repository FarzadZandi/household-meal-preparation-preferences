project_config <- function() {
  list(
    project_title = "Household Meal Preparation and Behavioral Preferences",
    raw_data = "data/raw/respondi_wave1.csv",
    outcome = "meal_preparation_count",
    primary_predictors = c("risk_taking", "future_orientation", "procrastination"),
    core_controls = c(
      "age_decade", "is_male", "high_education", "income_thousand",
      "healthy_diet", "refrigerator_access"
    ),
    extended_controls = c(
      "food_from_banks", "food_sharing", "food_thrown_away",
      "delivered_meals_change", "healthy_diet_change"
    ),
    output_dirs = c("outputs/tables", "outputs/figures", "outputs/report")
  )
}

ensure_output_dirs <- function(config = project_config()) {
  invisible(vapply(config$output_dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

