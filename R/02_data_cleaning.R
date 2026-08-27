## ============================================================
## CCHS 2022 POPULATION HEALTH ANALYSIS
## Script 02: Data Cleaning and Recoding
##
## Purpose:
##   Prepare selected CCHS 2022 variables for statistical
##   analysis by restricting the analytical population,
##   recoding categorical variables, and handling special
##   response codes.
##
## Input:
##   data/processed/cchs_selected_raw.rds
##
## Output:
##   A cleaned, analysis-ready CCHS dataset.
## ============================================================

## ------------------------------------------------------------
## 1. Load required packages
## ------------------------------------------------------------

library(tidyverse)

## ------------------------------------------------------------
## 2. Import selected raw variables
## ------------------------------------------------------------

selected_file <- "data/processed/cchs_selected_raw.rds"

file.exists(selected_file)

cchs_selected <- read_rds(selected_file)

glimpse(cchs_selected)

## ------------------------------------------------------------
## 3. Restrict analysis to adults aged 18+
## ------------------------------------------------------------

# The smoking-status and adjusted BMI variables used in this
# project are defined for respondents aged 18 years and older.
# Therefore, respondents aged 12 to 17 are excluded from the
# primary dataset.

cchs_adult <- cchs_selected %>%
  filter(DHHGAGE != 1)

# Compare sample size before and after restricting to adults
nrow(cchs_selected)
nrow(cchs_adult)

## ---------------------------------------------------------------
## 4. Recode demographic variables (Age, Sex at Birth & Education)
## ---------------------------------------------------------------

cchs_clean <- cchs_adult %>%
  mutate(
    
    age_group = case_when(
      DHHGAGE == 2 ~ "18 to 34",
      DHHGAGE == 3 ~ "35 to 49",
      DHHGAGE == 4 ~ "50 to 64",
      DHHGAGE == 5 ~ "65 and older",
      TRUE ~ NA_character_
    ),
    
    sex = case_when(
      DHH_SEX == 1 ~ "Male",
      DHH_SEX == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    education = case_when(
      EDDVH3 == 1 ~ "Less than secondary school",
      EDDVH3 == 2 ~ "Secondary school, no post-secondary",
      EDDVH3 == 3 ~ "Post-secondary education",
      EDDVH3 == 9 ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# Check recoded demographic variables

table(cchs_clean$age_group, useNA = "ifany")
table(cchs_clean$sex, useNA = "ifany")
table(cchs_clean$education, useNA = "ifany")

## -------------------------------------------------------------------
## 5. Recode health outcome variables (Diabetes & High Blood Pressure)
## -------------------------------------------------------------------

cchs_clean <- cchs_clean %>%
  mutate(
    
    diabetes = case_when(
      CCC_05 == 1 ~ "Yes",
      CCC_05 == 2 ~ "No",
      CCC_05 == 9 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    high_blood_pressure = case_when(
      CCC_80 == 1 ~ "Yes",
      CCC_80 == 2 ~ "No",
      CCC_80 == 9 ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# Check recoded health outcome variables (total: 63,318)
table(cchs_clean$diabetes, useNA = "ifany")
table(cchs_clean$high_blood_pressure, useNA = "ifany")

## ------------------------------------------------------------
## 6. Recode BMI and smoking variables
## ------------------------------------------------------------

cchs_clean <- cchs_clean %>%
  mutate(
    
    bmi_group = case_when(
      HWTDGBCC == 1 ~ "Underweight / Normal weight",
      HWTDGBCC == 2 ~ "Overweight / Obese",
      HWTDGBCC %in% c(6, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    smoking_status = case_when(
      SMKDVSTY == 1 ~ "Current daily smoker",
      SMKDVSTY == 2 ~ "Current occasional smoker",
      SMKDVSTY == 3 ~ "Former daily smoker",
      SMKDVSTY == 4 ~ "Former occasional smoker",
      SMKDVSTY == 5 ~ "Experimental smoker",
      SMKDVSTY == 6 ~ "Lifetime abstainer",
      SMKDVSTY %in% c(96, 99) ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# Check recoded BMI and smoking variables
table(cchs_clean$bmi_group, useNA = "ifany")
table(cchs_clean$smoking_status, useNA = "ifany")

# Inspect original special codes after the adult restriction
table(cchs_adult$HWTDGBCC, useNA = "ifany")
table(cchs_adult$SMKDVSTY, useNA = "ifany")

## ------------------------------------------------------------
## 7. Recode socioeconomic and geographic variables
## ------------------------------------------------------------

cchs_clean <- cchs_clean %>%
  mutate(
    
    income_quintile = case_when(
      INCDGRCA == 1 ~ "Quintile 1 (lowest)",
      INCDGRCA == 2 ~ "Quintile 2",
      INCDGRCA == 3 ~ "Quintile 3",
      INCDGRCA == 4 ~ "Quintile 4",
      INCDGRCA == 5 ~ "Quintile 5 (highest)",
      INCDGRCA %in% c(6, 9) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    province = case_when(
      GEOGPRV == 10 ~ "Newfoundland and Labrador",
      GEOGPRV == 11 ~ "Prince Edward Island",
      GEOGPRV == 12 ~ "Nova Scotia",
      GEOGPRV == 13 ~ "New Brunswick",
      GEOGPRV == 24 ~ "Quebec",
      GEOGPRV == 35 ~ "Ontario",
      GEOGPRV == 46 ~ "Manitoba",
      GEOGPRV == 47 ~ "Saskatchewan",
      GEOGPRV == 48 ~ "Alberta",
      GEOGPRV == 59 ~ "British Columbia",
      GEOGPRV == 60 ~ "Territories",
      TRUE ~ NA_character_
    )
  )

# Check recoded socioeconomic and geographic variables

table(cchs_clean$income_quintile, useNA = "ifany")
table(cchs_clean$province, useNA = "ifany")

## ------------------------------------------------------------
## 8. Convert cleaned categorical variables to factors
## ------------------------------------------------------------

cchs_clean <- cchs_clean %>%
  mutate(
    
    age_group = factor(
      age_group,
      levels = c(
        "18 to 34",
        "35 to 49",
        "50 to 64",
        "65 and older"
      ),
      ordered = TRUE
    ),
    
    sex = factor(
      sex,
      levels = c(
        "Male",
        "Female"
      )
    ),
    
    education = factor(
      education,
      levels = c(
        "Less than secondary school",
        "Secondary school, no post-secondary",
        "Post-secondary education"
      ),
      ordered = TRUE
    ),
    
    diabetes = factor(
      diabetes,
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    high_blood_pressure = factor(
      high_blood_pressure,
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    bmi_group = factor(
      bmi_group,
      levels = c(
        "Underweight / Normal weight",
        "Overweight / Obese"
      )
    ),
    
    smoking_status = factor(
      smoking_status,
      levels = c(
        "Lifetime abstainer",
        "Experimental smoker",
        "Former occasional smoker",
        "Former daily smoker",
        "Current occasional smoker",
        "Current daily smoker"
      )
    ),
    
    income_quintile = factor(
      income_quintile,
      levels = c(
        "Quintile 1 (lowest)",
        "Quintile 2",
        "Quintile 3",
        "Quintile 4",
        "Quintile 5 (highest)"
      ),
      ordered = TRUE
    ),
    
    province = factor(
      province,
      levels = c(
        "Newfoundland and Labrador",
        "Prince Edward Island",
        "Nova Scotia",
        "New Brunswick",
        "Quebec",
        "Ontario",
        "Manitoba",
        "Saskatchewan",
        "Alberta",
        "British Columbia",
        "Territories"
      )
    )
  )

# Check factor structure and levels
glimpse(cchs_clean)

levels(cchs_clean$age_group)
levels(cchs_clean$education)
levels(cchs_clean$diabetes)
levels(cchs_clean$bmi_group)
levels(cchs_clean$smoking_status)
levels(cchs_clean$income_quintile)

## ------------------------------------------------------------
## 9. Final quality-control checks
## ------------------------------------------------------------

# Confirm final number of adult respondents
nrow(cchs_clean)


# Confirm respondent identifiers are unique
n_distinct(cchs_clean$ADM_RNO)
anyDuplicated(cchs_clean$ADM_RNO)

# Inspect survey weights
summary(cchs_clean$WTS_M)
sum(is.na(cchs_clean$WTS_M))
sum(cchs_clean$WTS_M <= 0, na.rm = TRUE)


# Examine missing values in cleaned analysis variables
cchs_clean %>%
  summarise(
    n_total = n(),
    missing_age = sum(is.na(age_group)),
    missing_sex = sum(is.na(sex)),
    missing_education = sum(is.na(education)),
    missing_diabetes = sum(is.na(diabetes)),
    missing_high_blood_pressure = sum(is.na(high_blood_pressure)),
    missing_bmi = sum(is.na(bmi_group)),
    missing_smoking = sum(is.na(smoking_status)),
    missing_income = sum(is.na(income_quintile)),
    missing_province = sum(is.na(province)),
    missing_weight = sum(is.na(WTS_M))
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "count"
  )

## ------------------------------------------------------------
## 10. Save cleaned analysis dataset
## ------------------------------------------------------------

# Save the cleaned adult CCHS dataset for use in subsequent
# analysis scripts. The original raw data remain unchanged.

clean_file <- "data/processed/cchs_clean.rds"
write_rds(cchs_clean, clean_file)