write_csv_output <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(data, path, na = "")
  path
}

plot_outcome_distribution <- function(cleaned, path = "outputs/figures/outcome_distribution.png") {
  plot_data <- cleaned |>
    dplyr::filter(!is.na(meal_preparation_count))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = meal_preparation_count)) +
    ggplot2::geom_bar(fill = "#2C6E9B", width = 0.8) +
    ggplot2::scale_x_continuous(breaks = 0:11) +
    ggplot2::labs(
      title = "Meals prepared for the household over two days",
      subtitle = "Responses above 10 are encoded at the lower bound of 11",
      x = "Meals prepared",
      y = "Respondents"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = 8, height = 4.8, dpi = 160)
  path
}

plot_preference_estimates <- function(coefficients, path = "outputs/figures/preference_estimates.png") {
  labels <- c(
    risk_taking = "Risk tolerance",
    future_orientation = "Future orientation",
    procrastination = "Procrastination"
  )

  plot_data <- coefficients |>
    dplyr::filter(
      model %in% c("core_ols", "extended_ols", "poisson_ppml", "negative_binomial"),
      term %in% names(labels)
    ) |>
    dplyr::mutate(
      predictor = factor(labels[term], levels = rev(unname(labels))),
      model = factor(
        model,
        levels = c("core_ols", "extended_ols", "poisson_ppml", "negative_binomial"),
        labels = c("OLS: core", "OLS: extended", "Poisson PML", "Negative binomial")
      ),
      estimate_plot = dplyr::if_else(effect_scale == "log count", incidence_rate_ratio, estimate),
      low_plot = dplyr::if_else(effect_scale == "log count", irr_conf_low, conf_low),
      high_plot = dplyr::if_else(effect_scale == "log count", irr_conf_high, conf_high),
      reference = dplyr::if_else(effect_scale == "log count", 1, 0),
      scale_label = dplyr::if_else(effect_scale == "log count", "Incidence-rate ratio", "Meals per one-point increase")
    )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = estimate_plot, y = predictor, colour = model)
  ) +
    ggplot2::geom_vline(
      data = dplyr::distinct(plot_data, scale_label, reference),
      ggplot2::aes(xintercept = reference),
      colour = "grey55",
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = low_plot, xmax = high_plot), height = 0.18) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~scale_label, scales = "free_x", ncol = 1) +
    ggplot2::labs(
      title = "Behavioral-preference estimates across model families",
      subtitle = "Points show estimates; lines show HC3-robust 95% confidence intervals",
      x = NULL,
      y = NULL,
      colour = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = 8, height = 7, dpi = 160)
  path
}

plot_ols_diagnostics <- function(model, path = "outputs/figures/ols_diagnostics.png") {
  diagnostics <- tibble::tibble(
    fitted = stats::fitted(model),
    residual = stats::rstandard(model),
    cooks_distance = stats::cooks.distance(model)
  )

  p <- ggplot2::ggplot(diagnostics, ggplot2::aes(x = fitted, y = residual)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(size = cooks_distance), alpha = 0.45, colour = "#2C6E9B") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#B34D3E") +
    ggplot2::scale_size_continuous(range = c(1, 5)) +
    ggplot2::labs(
      title = "Extended OLS residuals",
      x = "Fitted meals prepared",
      y = "Standardized residual",
      size = "Cook's distance"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(path, p, width = 8, height = 4.8, dpi = 160)
  path
}

render_analysis_report <- function(output_file = "meal_preparation_analysis.html") {
  dir.create("outputs/report", recursive = TRUE, showWarnings = FALSE)
  if (!rmarkdown::pandoc_available()) {
    pandoc_candidates <- c(
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
      "C:/Program Files/Quarto/bin/tools"
    )
    pandoc_dir <- pandoc_candidates[file.exists(file.path(pandoc_candidates, "pandoc.exe"))][1]
    if (is.na(pandoc_dir)) {
      stop("Pandoc was not found. Install RStudio or Quarto to render the HTML report.", call. = FALSE)
    }
    Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  }
  rendered <- rmarkdown::render(
    input = "analysis/report.Rmd",
    output_file = output_file,
    output_dir = "outputs/report",
    knit_root_dir = getwd(),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  normalizePath(rendered, winslash = "/", mustWork = TRUE)
}
