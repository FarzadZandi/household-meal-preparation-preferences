# Household Meal Preparation and Behavioral Preferences

This project studies whether risk tolerance, future orientation, and
procrastination are associated with the number of main meals respondents
prepared for themselves or other household members over the previous two days.
It uses a de-identified German Wave 1 survey extract and treats meal preparation
as a bounded count outcome.

The analysis is observational. Results are conditional associations and should
not be interpreted as causal effects. “IV” in the archived coursework means
independent variable, not instrumental variable.

## Why the project was rebuilt

The archived notebook mixed data preparation, visualization, regressions, and
file writes in more than 1,200 lines. It also:

- used fragile column-position deletion;
- silently coded missing gender and education values as reference categories;
- created a male indicator but omitted it while treating height as a control;
- modeled a bounded count only with OLS;
- used uncentered interaction terms, inflating conventional VIF diagnostics;
- relied on stepwise specifications and an undocumented binary cutoff;
- duplicated generated plots, tables, and notebook code throughout the root.

The new project separates ingestion, recoding, models, diagnostics, reporting,
and tests into a reproducible `{targets}` pipeline.

## Variables

Outcome:

- `meal_preparation_count`: main meals prepared over the prior two days. The
  category “more than 10” has no usable numeric follow-up in the supplied
  extract, so the primary encoding is the lower bound 11. A sensitivity model
  uses the legacy value 10.

Primary independent variables (each 0–10):

- `risk_taking`: general willingness to take risks;
- `future_orientation`: willingness to give up a present benefit for a larger
  future benefit;
- `procrastination`: self-reported tendency to delay tasks.

Core controls cover age, gender, education, household income, self-rated diet,
and refrigerator/freezer access. The extended model adds food-access indicators,
food waste, delivered-meal change, and diet change. See `R/data.R` for exact
question-to-variable mappings.

## Models

- OLS with HC3 heteroskedasticity-robust inference;
- Poisson pseudo-maximum likelihood with robust inference;
- negative-binomial regression for overdispersion sensitivity;
- centered preference-by-diet interactions;
- robust MM regression;
- sensitivity to the top-coded outcome convention;
- a legacy-control replication for comparison.

Poisson PML is the main count-model extension. The negative-binomial model is a
sensitivity check rather than an automatic replacement; the generated
diagnostics report the observed Poisson dispersion.

## Reproduce the analysis

1. Install R 4.4 or a compatible newer release.
2. Clone the repository and open
   `Household-Meal-Preparation-Preferences.Rproj`.
3. Place the private Wave 1 extract at
   `data/raw/respondi_wave1.csv` as described in `data/README.md`.
4. Restore packages and run the pipeline:

```r
install.packages("renv")
renv::restore()
source("run_pipeline.R")
```

The pipeline writes tables to `outputs/tables`, figures to `outputs/figures`,
and the rendered analysis to
`outputs/report/meal_preparation_analysis.html`. Raw data, generated outputs,
pipeline cache, and archived source materials remain local and are ignored by
Git.

## Test

```r
testthat::test_dir("tests/testthat")
```

The tests cover endpoint parsing, top-code handling, income recoding, missing
category treatment, formulas, and model execution on synthetic data.

## Project structure

```text
R/                 Data, model, diagnostic, and reporting functions
analysis/          Reproducible HTML analysis source
data/README.md     Private-input contract
tests/testthat/     Unit and model smoke tests
_targets.R         Pipeline definition
renv.lock          Exact package versions
archive/           Local legacy files; excluded from Git
outputs/           Generated tables, figures, and report; excluded from Git
```

## Privacy

No respondent-level data, survey documents, guide papers, or legacy outputs are
committed. The full Qualtrics export includes direct and quasi-identifiers and
must not be pushed to GitHub, even in a private repository.

