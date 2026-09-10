required_raw_columns <- function() {
  c(
    "Q80_1", "Q81", "Q140", "Q186", "Q188", "Q46", "Q52",
    "Q52_12_TEXT", "Q59", "Q173_1", "Q153_1", "Q144", "Q82_1",
    "Q84", "Q168", "Q169", "Q86"
  )
}

assert_required_columns <- function(data, required = required_raw_columns()) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Missing required survey columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

read_wave1_survey <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Private survey data not found at '", path,
      "'. See data/README.md for the expected local file.",
      call. = FALSE
    )
  }

  raw <- readr::read_csv(
    path,
    na = c("", "NA"),
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE,
    progress = FALSE
  )
  assert_required_columns(raw)

  # Qualtrics exports place the question text and ImportId metadata in rows 1-2.
  if (nrow(raw) >= 2L && grepl("ImportId", raw[[1]][2], fixed = TRUE)) {
    raw <- raw[-c(1L, 2L), , drop = FALSE]
  }

  tibble::as_tibble(raw)
}

parse_scale_0_10 <- function(x) {
  value <- suppressWarnings(readr::parse_number(as.character(x), na = c("", "NA")))
  value[!is.na(value) & (value < 0 | value > 10)] <- NA_real_
  value
}

recode_yes_no <- function(x) {
  dplyr::case_when(
    x == "Ja" ~ 1,
    x == "Nein" ~ 0,
    TRUE ~ NA_real_
  )
}

recode_income_midpoint <- function(x) {
  cleaned <- stringr::str_replace_all(as.character(x), "[^[:alnum:]-]", "")
  lower <- suppressWarnings(as.numeric(stringr::str_match(cleaned, "^(\\d+)-(\\d+)$")[, 2]))
  upper <- suppressWarnings(as.numeric(stringr::str_match(cleaned, "^(\\d+)-(\\d+)$")[, 3]))
  midpoint <- (lower + upper) / 2
  dplyr::if_else(stringr::str_detect(cleaned, "^Mehrals10000$"), 10000, midpoint)
}

recode_ordered_change <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "^(stark.*abgenommen|wesentlich un)") ~ -2,
    stringr::str_detect(x, "^leicht.*abgenommen|^leicht un") ~ -1,
    stringr::str_detect(x, "nicht ver|^unver") ~ 0,
    stringr::str_detect(x, "^leicht.*zugenommen|^leicht ge") ~ 1,
    stringr::str_detect(x, "^stark.*zugenommen|^wesentlich ge|^signifikant ge") ~ 2,
    TRUE ~ NA_real_
  )
}

recode_meal_preparation <- function(choice, detail = NULL, topcode = 11) {
  parsed <- suppressWarnings(readr::parse_number(as.character(choice), na = c("", "NA")))
  detail_value <- if (is.null(detail)) rep(NA_real_, length(parsed)) else
    suppressWarnings(readr::parse_number(as.character(detail), na = c("", "NA")))
  is_topcoded <- dplyr::coalesce(
    stringr::str_detect(as.character(choice), "Mehr als 10"),
    FALSE
  )
  parsed[is_topcoded] <- dplyr::coalesce(detail_value[is_topcoded], as.numeric(topcode))
  parsed[parsed < 0] <- NA_real_
  parsed
}

clean_survey_data <- function(raw, topcode = 11) {
  cleaned <- raw |>
    dplyr::transmute(
      meal_preparation_count = recode_meal_preparation(Q52, Q52_12_TEXT, topcode),
      meal_preparation_topcoded = stringr::str_detect(Q52, "Mehr als 10"),
      risk_taking = parse_scale_0_10(Q84),
      future_orientation = parse_scale_0_10(Q168),
      procrastination = parse_scale_0_10(Q169),
      age = suppressWarnings(readr::parse_number(Q80_1)),
      age_decade = age / 10,
      is_male = dplyr::case_when(
        stringr::str_detect(Q81, "^M") ~ 1,
        Q81 %in% c("Weiblich", "Divers") ~ 0,
        TRUE ~ NA_real_
      ),
      high_education = dplyr::case_when(
        is.na(Q140) ~ NA_real_,
        stringr::str_detect(
          Q140,
          "^(Abitur|Duale Hochschule|Fachhochschule|Sonstige Hochschule|Universit|Promotion)"
        ) ~ 1,
        TRUE ~ 0
      ),
      income_midpoint = recode_income_midpoint(Q86),
      income_thousand = income_midpoint / 1000,
      healthy_diet = dplyr::case_when(
        Q59 == "Sehr ungesund" ~ 1,
        Q59 == "Ungesund" ~ 2,
        Q59 == "Eher ungesund" ~ 3,
        Q59 == "Eher gesund" ~ 4,
        Q59 == "Gesund" ~ 5,
        Q59 == "Sehr gesund" ~ 6,
        TRUE ~ NA_real_
      ),
      refrigerator_access = dplyr::case_when(
        Q46 == "Nein - zu keinem" ~ 0,
        Q46 == "Ja - zu beidem" | stringr::str_detect(Q46, "^Zugang nur zu") ~ 1,
        TRUE ~ NA_real_
      ),
      food_from_banks = recode_yes_no(Q186),
      food_sharing = recode_yes_no(Q188),
      food_thrown_away = recode_yes_no(Q173_1),
      delivered_meals_change = recode_ordered_change(Q153_1),
      healthy_diet_change = recode_ordered_change(Q144)
    ) |>
    dplyr::mutate(
      height_cm = suppressWarnings(readr::parse_number(raw$Q82_1)),
      delivered_meals_changed = dplyr::if_else(
        is.na(delivered_meals_change), NA_real_, as.numeric(delivered_meals_change != 0)
      ),
      healthy_diet_changed = dplyr::if_else(
        is.na(healthy_diet_change), NA_real_, as.numeric(healthy_diet_change != 0)
      ),
      respondent_row = dplyr::row_number(),
      across(dplyr::all_of(c("risk_taking", "future_orientation", "procrastination")),
             ~ .x - mean(.x, na.rm = TRUE), .names = "{.col}_centered"),
      healthy_diet_centered = healthy_diet - mean(healthy_diet, na.rm = TRUE)
    )

  cleaned
}

analysis_variables <- function(config = project_config(), model = c("core", "extended")) {
  model <- match.arg(model)
  vars <- c(config$outcome, config$primary_predictors, config$core_controls)
  if (model == "extended") vars <- c(vars, config$extended_controls)
  unique(vars)
}

make_analysis_sample <- function(cleaned, config = project_config(), model = c("core", "extended")) {
  model <- match.arg(model)
  vars <- analysis_variables(config, model)
  cleaned |>
    dplyr::filter(stats::complete.cases(dplyr::pick(dplyr::all_of(vars))))
}

build_sample_flow <- function(cleaned, core_sample, extended_sample) {
  tibble::tibble(
    stage = c(
      "Survey responses after metadata rows",
      "Non-missing meal-preparation outcome",
      "Complete cases: primary model",
      "Complete cases: extended model"
    ),
    rows = c(
      nrow(cleaned),
      sum(!is.na(cleaned$meal_preparation_count)),
      nrow(core_sample),
      nrow(extended_sample)
    )
  ) |>
    dplyr::mutate(retained_from_previous = rows / dplyr::lag(rows, default = dplyr::first(rows)))
}

profile_data_quality <- function(cleaned) {
  numeric_columns <- names(cleaned)[vapply(cleaned, is.numeric, logical(1))]
  dplyr::bind_rows(lapply(numeric_columns, function(variable) {
    x <- cleaned[[variable]]
    tibble::tibble(
      variable = variable,
      n = length(x),
      missing_n = sum(is.na(x)),
      missing_rate = mean(is.na(x)),
      distinct_n = dplyr::n_distinct(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      min = suppressWarnings(min(x, na.rm = TRUE)),
      median = stats::median(x, na.rm = TRUE),
      max = suppressWarnings(max(x, na.rm = TRUE))
    )
  }))
}
