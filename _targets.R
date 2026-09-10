library(targets)

source("R/config.R", encoding = "UTF-8")
source("R/data.R", encoding = "UTF-8")
source("R/models.R", encoding = "UTF-8")
source("R/diagnostics.R", encoding = "UTF-8")
source("R/reporting.R", encoding = "UTF-8")

tar_option_set(
  packages = c(
    "broom", "car", "dplyr", "ggplot2", "lmtest", "MASS", "readr",
    "rmarkdown", "sandwich", "stringr", "tibble"
  ),
  seed = 20260909
)

list(
  tar_target(config, project_config()),
  tar_target(report_source, "analysis/report.Rmd", format = "file"),
  tar_target(raw_data_file, config$raw_data, format = "file"),
  tar_target(raw_survey, read_wave1_survey(raw_data_file)),
  tar_target(cleaned_survey, clean_survey_data(raw_survey, topcode = 11)),
  tar_target(core_sample, make_analysis_sample(cleaned_survey, config, "core")),
  tar_target(extended_sample, make_analysis_sample(cleaned_survey, config, "extended")),
  tar_target(sample_flow, build_sample_flow(cleaned_survey, core_sample, extended_sample)),
  tar_target(data_quality, profile_data_quality(cleaned_survey)),
  tar_target(models, fit_model_suite(core_sample, extended_sample)),
  tar_target(model_coefficients, combine_model_coefficients(models)),
  tar_target(model_fit, combine_model_fit(models)),
  tar_target(diagnostics, run_diagnostics(models, cleaned_survey, core_sample, extended_sample)),
  tar_target(sample_flow_file, write_csv_output(sample_flow, "outputs/tables/sample_flow.csv"), format = "file"),
  tar_target(data_quality_file, write_csv_output(data_quality, "outputs/tables/data_quality.csv"), format = "file"),
  tar_target(model_coefficients_file, write_csv_output(model_coefficients, "outputs/tables/model_coefficients.csv"), format = "file"),
  tar_target(model_fit_file, write_csv_output(model_fit, "outputs/tables/model_fit.csv"), format = "file"),
  tar_target(diagnostics_file, write_csv_output(diagnostics, "outputs/tables/diagnostics.csv"), format = "file"),
  tar_target(outcome_plot, plot_outcome_distribution(cleaned_survey), format = "file"),
  tar_target(preference_plot, plot_preference_estimates(model_coefficients), format = "file"),
  tar_target(ols_diagnostics_plot, plot_ols_diagnostics(models$extended_ols), format = "file"),
  tar_target(
    report,
    {
      c(sample_flow_file, data_quality_file, model_coefficients_file, model_fit_file,
        diagnostics_file, outcome_plot, preference_plot, ols_diagnostics_plot, report_source)
      render_analysis_report()
    },
    format = "file"
  )
)
