## ============================================================
## CCHS 2022 POPULATION HEALTH ANALYSIS
## Script 05: Build Reporting Data Mart
##
## Purpose:
##   Construct a small reporting-oriented data mart from the
##   descriptive and statistical analysis outputs created in
##   Scripts 03 and 04.
##
##   The resulting fact and dimension tables are designed for
##   downstream use in business-intelligence and visualization
##   tools such as Power BI and Tableau.
##
## Inputs:
##   data/processed/overall_prevalence.csv
##   data/processed/subgroup_prevalence_long.csv
##   data/processed/diabetes_odds_ratio_long.csv
##   data/processed/high_bp_odds_ratio_long.csv
##
## Outputs:
##   data/mart/fact_prevalence.csv
##   data/mart/fact_regression.csv
##   data/mart/dim_outcome.csv
##   data/mart/dim_characteristic.csv
##
## Data-mart design:
##   fact_prevalence
##     - weighted prevalence estimates
##
##   fact_regression
##     - unadjusted and adjusted odds-ratio estimates
##
##   dim_outcome
##     - health-outcome lookup table
##
##   dim_characteristic
##     - demographic, socioeconomic, behavioural, health,
##       and geographic characteristic lookup table
## ============================================================


## ------------------------------------------------------------
## 1. Load required packages
## ------------------------------------------------------------

library(tidyverse)

## ------------------------------------------------------------
## 2. Define input and output locations
## ------------------------------------------------------------

overall_prevalence_file <-
  "data/processed/overall_prevalence.csv"

subgroup_prevalence_file <-
  "data/processed/subgroup_prevalence_long.csv"

diabetes_regression_file <-
  "data/processed/diabetes_odds_ratio_long.csv"

high_bp_regression_file <-
  "data/processed/high_bp_odds_ratio_long.csv"


# Create the data-mart directory if it does not already exist.

mart_directory <- "data/mart"

dir.create(
  mart_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

## ------------------------------------------------------------
## 3. Verify required input files
## ------------------------------------------------------------

input_files <- c(
  overall_prevalence_file,
  subgroup_prevalence_file,
  diabetes_regression_file,
  high_bp_regression_file
)


input_check <- tibble(
  file = input_files,
  exists = file.exists(input_files)
)

input_check


# Stop execution if any required upstream output is missing.

if (!all(input_check$exists)) {
  stop(
    "One or more required input files are missing. ",
    "Run Scripts 03 and 04 before building the data mart."
  )
}

## ------------------------------------------------------------
## 4. Import analysis outputs
## ------------------------------------------------------------

overall_prevalence <- read_csv(
  overall_prevalence_file,
  show_col_types = FALSE
)

subgroup_prevalence <- read_csv(
  subgroup_prevalence_file,
  show_col_types = FALSE
)

diabetes_regression <- read_csv(
  diabetes_regression_file,
  show_col_types = FALSE
)

high_bp_regression <- read_csv(
  high_bp_regression_file,
  show_col_types = FALSE
)


## 4.1 Inspect imported tables

# Examine dimensions and column structure before constructing
# the reporting data mart.

dim(overall_prevalence)
glimpse(overall_prevalence)

dim(subgroup_prevalence)
glimpse(subgroup_prevalence)

dim(diabetes_regression)
glimpse(diabetes_regression)

dim(high_bp_regression)
glimpse(high_bp_regression)

## ------------------------------------------------------------
## 5. Build prevalence fact table
## ------------------------------------------------------------

## 5.1 Prepare overall prevalence estimates

# Convert the overall prevalence table to the same structure as
# the subgroup prevalence table.
# "Overall" identifies the characteristic and "All adults"
# identifies the population group represented by the estimate.
overall_prevalence_mart <- overall_prevalence %>%
  transmute(
    characteristic = "Overall",
    group = "All adults",
    outcome = outcome,
    prevalence_percent = weighted_prevalence_percent
  )

overall_prevalence_mart


## 5.2 Combine overall and subgroup prevalence estimates
fact_prevalence <- bind_rows(
  overall_prevalence_mart,
  subgroup_prevalence
)

fact_prevalence


## 5.3 Add prevalence fact identifier

# Add a unique identifier for each prevalence record in the
# reporting fact table.
fact_prevalence <- fact_prevalence %>%
  mutate(
    prevalence_id = row_number(),
    .before = 1
  )

fact_prevalence


## 5.4 Inspect prevalence fact table

# Confirm expected dimensions.
# Expected: 68 rows and 5 columns.
dim(fact_prevalence)

# Inspect column structure.
glimpse(fact_prevalence)

# Count prevalence records by characteristic.
fact_prevalence %>%
  count(characteristic)

# Count prevalence records by health outcome.
# Each outcome should contain 34 prevalence estimates.
fact_prevalence %>%
  count(outcome)

## ------------------------------------------------------------
## 6. Build regression fact table
## ------------------------------------------------------------

## 6.1 Add outcome labels to regression results

# The diabetes and high blood pressure regression files have the
# same structure, but the outcome is currently implied by the
# source file rather than stored as a column.
#
# Add an explicit outcome variable before combining the tables.

diabetes_regression_mart <- diabetes_regression %>%
  mutate(
    outcome = "Diabetes",
    .before = 1
  )

high_bp_regression_mart <- high_bp_regression %>%
  mutate(
    outcome = "High blood pressure",
    .before = 1
  )


# Inspect the two prepared regression tables.

glimpse(diabetes_regression_mart)

glimpse(high_bp_regression_mart)

## 6.2 Combine regression results

fact_regression <- bind_rows(
  diabetes_regression_mart,
  high_bp_regression_mart
)

fact_regression

## 6.3 Add regression fact identifier

fact_regression <- fact_regression %>%
  mutate(
    regression_id = row_number(),
    .before = 1
  )

fact_regression

## 6.4 Inspect regression fact table

# Confirm expected dimensions.
# Expected: 64 rows and 9 columns.

dim(fact_regression)


# Inspect column structure.

glimpse(fact_regression)


# Confirm equal numbers of records for each health outcome.

fact_regression %>%
  count(outcome)


# Confirm equal numbers of adjusted and unadjusted records.

fact_regression %>%
  count(model)


# Examine the number of regression records by predictor.

fact_regression %>%
  count(predictor)

## ------------------------------------------------------------
## 7. Build dimension tables
## ------------------------------------------------------------

## 7.1 Outcome dimension

# Create a lookup table containing the health outcomes used
# throughout the reporting data mart.

dim_outcome <- tibble(
  outcome_id = c(1, 2),
  outcome = c(
    "Diabetes",
    "High blood pressure"
  )
)

dim_outcome

## 7.2 Characteristic dimension

# Create a lookup table containing the descriptive and regression
# characteristics used throughout the data mart.
#
# "Overall" appears only in the prevalence fact table, while
# province / territory appears only in the descriptive prevalence
# analysis.

dim_characteristic <- tibble(
  characteristic_id = 1:8,
  characteristic = c(
    "Overall",
    "Age group",
    "Sex",
    "Education",
    "BMI group",
    "Smoking status",
    "Income quintile",
    "Province / territory"
  )
)

dim_characteristic

## ------------------------------------------------------------
## 8. Add dimension keys to fact tables
## ------------------------------------------------------------

## 8.1 Add dimension keys to prevalence fact table

# Join the outcome and characteristic dimensions to the
# prevalence fact table using their descriptive labels.

fact_prevalence <- fact_prevalence %>%
  left_join(
    dim_outcome,
    by = "outcome"
  ) %>%
  left_join(
    dim_characteristic,
    by = "characteristic"
  ) %>%
  relocate(
    outcome_id,
    characteristic_id,
    .after = prevalence_id
  )

fact_prevalence

## 8.2 Add dimension keys to regression fact table

# The regression table uses the column name "predictor", but these
# predictors correspond to the same concepts stored in the
# characteristic dimension.

fact_regression <- fact_regression %>%
  left_join(
    dim_outcome,
    by = "outcome"
  ) %>%
  left_join(
    dim_characteristic,
    by = c(
      "predictor" = "characteristic"
    )
  ) %>%
  relocate(
    outcome_id,
    characteristic_id,
    .after = regression_id
  )

fact_regression

## 8.3 Validate dimension relationships

# Every record in each fact table should successfully match an
# outcome and characteristic dimension record.
# All four checks should return 0.

sum(is.na(fact_prevalence$outcome_id))
sum(is.na(fact_prevalence$characteristic_id))

sum(is.na(fact_regression$outcome_id))
sum(is.na(fact_regression$characteristic_id))

## ------------------------------------------------------------
## 9. Export data mart tables
## ------------------------------------------------------------

write_csv(
  fact_prevalence,
  "data/mart/fact_prevalence.csv"
)

write_csv(
  fact_regression,
  "data/mart/fact_regression.csv"
)

write_csv(
  dim_outcome,
  "data/mart/dim_outcome.csv"
)

write_csv(
  dim_characteristic,
  "data/mart/dim_characteristic.csv"
)

## ------------------------------------------------------------
## 10. Final quality-control checks
## ------------------------------------------------------------

# Confirm expected row counts.

nrow(dim_outcome)              # Expected: 2
nrow(dim_characteristic)       # Expected: 8
nrow(fact_prevalence)          # Expected: 68
nrow(fact_regression)          # Expected: 64


# Confirm that fact-table dimension keys are complete.

sum(is.na(fact_prevalence$outcome_id))
sum(is.na(fact_prevalence$characteristic_id))

sum(is.na(fact_regression$outcome_id))
sum(is.na(fact_regression$characteristic_id))


# Confirm that all expected data-mart files exist.

mart_files <- c(
  "data/mart/fact_prevalence.csv",
  "data/mart/fact_regression.csv",
  "data/mart/dim_outcome.csv",
  "data/mart/dim_characteristic.csv"
)

mart_qc <- tibble(
  file = mart_files,
  exists = file.exists(mart_files)
)

mart_qc


# Stop execution if any expected mart file is missing.

if (!all(mart_qc$exists)) {
  stop(
    "One or more expected data-mart files were not created."
  )
}


cat(
  "\n=====================================================\n",
  "REPORTING DATA MART COMPLETE\n",
  "=====================================================\n",
  "\nFact tables:\n",
  "  fact_prevalence:", nrow(fact_prevalence), "rows\n",
  "  fact_regression:", nrow(fact_regression), "rows\n",
  "\nDimension tables:\n",
  "  dim_outcome:", nrow(dim_outcome), "rows\n",
  "  dim_characteristic:", nrow(dim_characteristic), "rows\n",
  "\nAll expected data-mart files were successfully created.\n"
)