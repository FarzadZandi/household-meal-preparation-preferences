# Private survey input

The pipeline expects the de-identified Wave 1 extract at:

`data/raw/respondi_wave1.csv`

The file must contain the original Qualtrics header plus the question-text and
ImportId rows. `R/data.R` removes those two metadata rows explicitly.

Raw survey files are excluded from Git because they contain respondent-level
answers. The full original export also contains direct or quasi-identifiers and
must remain in the local ignored archive.

