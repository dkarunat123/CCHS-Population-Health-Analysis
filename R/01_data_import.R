## ============================================================
## CCHS 2022 POPULATION HEALTH ANALYSIS
## Script 01: Data Import and Initial Inspection
##
## Purpose:
##   Import the 2022 Canadian Community Health Survey (CCHS)
##   Public Use Microdata File (PUMF) and inspect its basic
##   structure before data cleaning and statistical analysis.
##
## Data source:
##   Statistics Canada
##   Canadian Community Health Survey (CCHS), 2022 PUMF
##
## ============================================================

## ------------------------------------------------------------
## 1. Load required packages
## ------------------------------------------------------------

library(tidyverse)

## ------------------------------------------------------------
## 2. Verify raw data file
## ------------------------------------------------------------

cchs_file <- "data/raw/pumf_cchs.csv"

file.exists(cchs_file)

## ------------------------------------------------------------
## 3. Import CCHS 2022 PUMF
## ------------------------------------------------------------

cchs <- read_csv(cchs_file, show_col_types = FALSE)

## ------------------------------------------------------------
## 4. Inspect basic dataset structure
## ------------------------------------------------------------

# number of respondents/records (67,079)
nrow(cchs)
# number of variables (255)
ncol(cchs)
   # first 20 variable names
   names(cchs)[1:20]

# compact overview of the dataset & variable type
glimpse(cchs)

## ------------------------------------------------------------
## 5. Identify variables for the planned analysis
## ------------------------------------------------------------

analysis_vars <- c(
  "ADM_RNO",    # respondent record number
  "DHHGAGE",    # age group
  "DHH_SEX",    # sex at birth
  "EDDVH3",     # highest household education level
  "CCC_05",     # diabetes
  "CCC_80",     # high blood pressure
  "HWTDGBCC",   # adjusted adult BMI classification
  "SMKDVSTY",   # smoking status
  "INCDGRCA",   # national household income quintile
  "GEOGPRV",    # province/territory
  "WTS_M"       # survey weight
)

# confirm that every selected variable exists in the dataset
analysis_vars %in% names(cchs)

# display the selected variables
cchs_selected <- cchs %>%
  select(all_of(analysis_vars))

glimpse(cchs_selected)

## ------------------------------------------------------------
## 6. Variable codebook
## ------------------------------------------------------------

# The following coding definitions are based on the
# Statistics Canada CCHS 2022 PUMF Data Dictionary.

# ADM_RNO
# Sequential record number / respondent identifier


# DHHGAGE
# Age group
# 1 = 12 to 17 years
# 2 = 18 to 34 years
# 3 = 35 to 49 years
# 4 = 50 to 64 years
# 5 = 65 years and older


# DHH_SEX
# Sex at birth
# 1 = Male
# 2 = Female


# EDDVH3
# Highest level of education in household
# 1 = Less than secondary school graduation
# 2 = Secondary school graduation, no post-secondary education
# 3 = Post-secondary certificate/diploma/university degree
# 9 = Not stated


# CCC_05
# Has diabetes
# 1 = Yes
# 2 = No
# 9 = Not stated


# CCC_80
# Has high blood pressure
# 1 = Yes
# 2 = No
# 9 = Not stated


# HWTDGBCC
# Adjusted BMI classification for adults (18+)
# 1 = Underweight / Normal weight
# 2 = Overweight / Obese (Class I, II, III)
# 6 = Valid skip
# 9 = Not stated


# SMKDVSTY
# Smoking status (traditional definition; adults 18+)
# 01 = Current daily smoker
# 02 = Current occasional smoker
# 03 = Former daily smoker
# 04 = Former occasional smoker
# 05 = Experimental smoker (>1 cigarette, <100 cigarettes)
# 06 = Lifetime abstainer (never smoked a whole cigarette)
# 96 = Valid skip
# 99 = Not stated


# INCDGRCA
# Distribution of household income - national level
# 1 = Quintile 1 (lowest)
# 2 = Quintile 2
# 3 = Quintile 3
# 4 = Quintile 4
# 5 = Quintile 5 (highest)
# 6 = Valid skip
# 9 = Not stated


# GEOGPRV
# Province or territory of residence
# 10 = Newfoundland and Labrador
# 11 = Prince Edward Island
# 12 = Nova Scotia
# 13 = New Brunswick
# 24 = Quebec
# 35 = Ontario
# 46 = Manitoba
# 47 = Saskatchewan
# 48 = Alberta
# 59 = British Columbia
# 60 = Yukon / Northwest Territories / Nunavut


# WTS_M
# Master survey weight
# Continuous numeric variable used to produce population-level estimates

## ------------------------------------------------------------
## 7. Validate observed values against the codebook
## ------------------------------------------------------------

# Show the unique values observed in each categorical variable

sort(unique(cchs_selected$DHHGAGE))
sort(unique(cchs_selected$DHH_SEX))
sort(unique(cchs_selected$EDDVH3))
sort(unique(cchs_selected$CCC_05))
sort(unique(cchs_selected$CCC_80))
sort(unique(cchs_selected$HWTDGBCC))
sort(unique(cchs_selected$SMKDVSTY))
sort(unique(cchs_selected$INCDGRCA))
sort(unique(cchs_selected$GEOGPRV))

# All observed codes match the categories documented in the
# Statistics Canada CCHS 2022 PUMF Data Dictionary.
# No unexpected categorical values were identified.

## ------------------------------------------------------------
## 8. Inspect unweighted frequencies
## ------------------------------------------------------------

# Examine the number of survey respondents in each raw category.
# These are unweighted sample counts and should not be interpreted
# as population prevalence estimates.

table(cchs_selected$DHHGAGE, useNA = "ifany")
table(cchs_selected$DHH_SEX, useNA = "ifany")
table(cchs_selected$EDDVH3, useNA = "ifany")
table(cchs_selected$CCC_05, useNA = "ifany")
table(cchs_selected$CCC_80, useNA = "ifany")
table(cchs_selected$HWTDGBCC, useNA = "ifany")
table(cchs_selected$SMKDVSTY, useNA = "ifany")
table(cchs_selected$INCDGRCA, useNA = "ifany")
table(cchs_selected$GEOGPRV, useNA = "ifany")

## ------------------------------------------------------------
## 9. Summary of initial inspection
## ------------------------------------------------------------

# The CCHS 2022 PUMF contains 67,079 respondent records and
# 255 variables.
#
# Eleven variables were selected for the planned analysis,
# including demographic characteristics, socioeconomic factors,
# health outcomes, behavioural factors, geography, and the
# master survey weight.
#
# Observed categorical codes were checked against the Statistics
# Canada CCHS 2022 PUMF Data Dictionary. No unexpected codes
# were identified.
#
# Raw frequency tables were inspected to distinguish substantive
# response categories from special codes such as valid skips and
# not-stated responses.
#
# Frequencies examined in this script are unweighted sample
# counts. Population-level estimates will be calculated using
# the CCHS master survey weight (WTS_M) in subsequent analyses.

## ------------------------------------------------------------
## 10. Save selected raw variables for downstream scripts
## ------------------------------------------------------------

write_rds(
  cchs_selected,
  "data/processed/cchs_selected_raw.rds"
)