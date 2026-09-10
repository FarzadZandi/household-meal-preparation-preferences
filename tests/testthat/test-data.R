test_that("scale parsing preserves endpoints and rejects invalid values", {
  x <- c("0=not willing", "5", "10=very willing", "12", NA)
  expect_equal(parse_scale_0_10(x), c(0, 5, 10, NA, NA))
})

test_that("top-coded meal responses use the selected lower bound", {
  x <- c("0", "3", "Mehr als 10", NA)
  expect_equal(recode_meal_preparation(x, topcode = 11), c(0, 3, 11, NA))
  expect_equal(recode_meal_preparation(x, topcode = 10), c(0, 3, 10, NA))
})

test_that("income categories map to midpoints", {
  x <- c("0-500€", "9501-10.000€", "Mehr als 10.000€", NA)
  expect_equal(recode_income_midpoint(x), c(250, 9750.5, 10000, NA))
})

test_that("missing gender and education are not silently coded as reference categories", {
  raw <- tibble::tibble(
    Q80_1 = "40", Q81 = NA_character_, Q140 = NA_character_, Q186 = "Nein",
    Q188 = "Nein", Q46 = "Ja - zu beidem", Q52 = "3", Q52_12_TEXT = NA_character_,
    Q59 = "Eher gesund", Q173_1 = "Nein", Q153_1 = "sich nicht verändert",
    Q144 = "unverändert", Q84 = "5", Q168 = "6", Q169 = "4", Q86 = "2501-3000€",
    Q82_1 = "175"
  )
  cleaned <- clean_survey_data(raw)
  expect_true(is.na(cleaned$is_male))
  expect_true(is.na(cleaned$high_education))
})

