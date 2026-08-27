## ============================================================
## CCHS 2022 POPULATION HEALTH ANALYSIS
## Script 03: Survey-Weighted Descriptive Analysis
##
## Purpose:
##   Conduct descriptive analyses of the cleaned CCHS 2022
##   adult population, accounting for the CCHS master survey
##   weight.
##
##   Analyses will describe the study population and estimate
##   the prevalence of diabetes and high blood pressure overall
##   and across demographic, socioeconomic, behavioural,
##   health, and geographic characteristics.
##
## Input:
##   data/processed/cchs_clean.rds
##
## Outputs:
##   Survey-weighted descriptive statistics, prevalence
##   estimates, tables, and figures for downstream reporting
##   and visualization.
## ============================================================

## ------------------------------------------------------------
## 1. Load required packages
## ------------------------------------------------------------

library(tidyverse)
library(survey)

## ------------------------------------------------------------
## 2. Import cleaned analysis dataset
## ------------------------------------------------------------

clean_file <- "data/processed/cchs_clean.rds"

file.exists(clean_file)

cchs_clean <- read_rds(clean_file)

# Confirm expected dataset dimensions
dim(cchs_clean)

# Inspect cleaned dataset structure
glimpse(cchs_clean)

## ------------------------------------------------------------
## 3. Create survey design object
## ------------------------------------------------------------

# Create a survey design object using the CCHS master survey
# weight. Each respondent contributes to population estimates
# according to their value of WTS_M.

# Methodological note:
   # The PUMF analysis uses the master survey weight (WTS_M) to
   # produce weighted population-level point estimates.

   # Because additional information required to fully represent the
   # complex CCHS sampling design is not specified in this analysis,
   # ids = ~1 is used and observations are treated as independently
   # sampled for variance estimation.

  # Therefore, standard errors and confidence intervals calculated
  # from this survey design should not be interpreted as official
  # Statistics Canada design-based variance estimates. The primary
  # use of WTS_M in this project is to obtain population-weighted
  # point estimates.

cchs_design <- svydesign(
  ids = ~1,
  weights = ~WTS_M,
  data = cchs_clean
)

cchs_design


# Compare sample size with weighted population total
nrow(cchs_clean)
sum(cchs_clean$WTS_M)

## ------------------------------------------------------------
## 4. Compare unweighted and weighted diabetes prevalence
## ------------------------------------------------------------

## 4.1 Unweighted prevalence

# This describes the proportion of survey respondents with
# valid diabetes data who reported having diabetes.
cchs_clean %>%
  filter(!is.na(diabetes)) %>%
  count(diabetes) %>%
  mutate(
    percent = n / sum(n) * 100
  )

## 4.2 Survey-weighted prevalence

# This estimates the proportion of the represented Canadian
# adult population reporting diabetes, using the CCHS master
# survey weight.
svymean(
  ~diabetes,
  design = subset(cchs_design, !is.na(diabetes))
)

## 4.3 Examine survey weights by diabetes status

# Compare the distribution of the master survey weight among
# respondents with and without diabetes. This helps illustrate
# why the weighted prevalence estimate differs from the
# unweighted sample prevalence.

cchs_clean %>%
  filter(!is.na(diabetes)) %>%
  group_by(diabetes) %>%
  summarise(
    respondents = n(),
    mean_weight = mean(WTS_M),
    median_weight = median(WTS_M),
    total_weight = sum(WTS_M)
  )

## ------------------------------------------------------------
## 5. Overall weighted health outcome prevalence
## ------------------------------------------------------------

## 5.1 Diabetes prevalence

diabetes_prev <- svymean(
  ~diabetes,
  design = subset(cchs_design, !is.na(diabetes))
)

diabetes_prev


## 5.2 High blood pressure prevalence

hbp_prev <- svymean(
  ~high_blood_pressure,
  design = subset(cchs_design, !is.na(high_blood_pressure))
)

hbp_prev

## 5.3 Create clean overall prevalence table

overall_prevalence <- tibble(
  outcome = c(
    "Diabetes",
    "High blood pressure"
  ),
  weighted_prevalence_percent = c(
    coef(diabetes_prev)["diabetesYes"],
    coef(hbp_prev)["high_blood_pressureYes"]
  ) * 100
)

overall_prevalence

## ------------------------------------------------------------
## 6. Weighted prevalence by age group
## ------------------------------------------------------------

## 6.1 Diabetes prevalence by age group

diabetes_by_age <- svyby(
  ~I(diabetes == "Yes"),
  ~age_group,
  design = subset(cchs_design, !is.na(diabetes)),
  FUN = svymean,
  na.rm = TRUE
)

diabetes_by_age

## 6.2 High blood pressure prevalence by age group

hbp_by_age <- svyby(
  ~I(high_blood_pressure == "Yes"),
  ~age_group,
  design = subset(cchs_design, !is.na(high_blood_pressure)),
  FUN = svymean,
  na.rm = TRUE
)

hbp_by_age

## 6.3 Create clean age-specific prevalence table

age_prevalence <- tibble(
  age_group = diabetes_by_age$age_group,
  
  diabetes_prevalence_percent =
    diabetes_by_age$`I(diabetes == "Yes")TRUE` * 100,
  
  high_blood_pressure_prevalence_percent =
    hbp_by_age$`I(high_blood_pressure == "Yes")TRUE` * 100
)

age_prevalence


## ------------------------------------------------------------
## 7. Create reusable function for subgroup prevalence
## ------------------------------------------------------------

# Calculate weighted prevalence of diabetes and high blood
# pressure within levels of a specified grouping variable.

prevalence_by_group <- function(group_var, design) {
  
  group_formula <- as.formula(
    paste0("~", group_var)
  )
  
  diabetes_result <- svyby(
    ~I(diabetes == "Yes"),
    group_formula,
    design = subset(design, !is.na(diabetes)),
    FUN = svymean,
    na.rm = TRUE
  )
  
  hbp_result <- svyby(
    ~I(high_blood_pressure == "Yes"),
    group_formula,
    design = subset(design, !is.na(high_blood_pressure)),
    FUN = svymean,
    na.rm = TRUE
  )
  
  result <- tibble(
    group = diabetes_result[[group_var]],
    
    diabetes_prevalence_percent =
      diabetes_result$`I(diabetes == "Yes")TRUE` * 100,
    
    high_blood_pressure_prevalence_percent =
      hbp_result$`I(high_blood_pressure == "Yes")TRUE` * 100
  )
  
  return(result)
}

# Check that the function reproduces the manually calculated
# age-specific prevalence table.
age_prevalence_function <- prevalence_by_group(
  "age_group",
  cchs_design
)

age_prevalence_function

## --------------------------------------------------------------
## 8. Calculate weighted prevalence by additional characteristics
## --------------------------------------------------------------

# Sex-specific prevalence table
sex_prevalence <- prevalence_by_group(
  "sex",
  cchs_design
)

sex_prevalence

# Education-specific prevalence table

education_prevalence <- prevalence_by_group(
  "education",
  cchs_design
)

education_prevalence

# BMIGroup-specific prevalence table

bmi_prevalence <- prevalence_by_group(
  "bmi_group",
  cchs_design
)

bmi_prevalence


# SmokingStatus-specific prevalence table

smoking_prevalence <- prevalence_by_group(
  "smoking_status",
  cchs_design
)

smoking_prevalence


# IncomeQuintile-specific prevalence table

income_prevalence <- prevalence_by_group(
  "income_quintile",
  cchs_design
)

income_prevalence


# ProvinceTerritory-specific prevalence table

province_prevalence <- prevalence_by_group(
  "province",
  cchs_design
)

province_prevalence

## ------------------------------------------------------------
## 9. Combine subgroup prevalence results
## ------------------------------------------------------------

subgroup_prevalence <- bind_rows(
  
  age_prevalence %>%
    rename(group = age_group) %>%
    mutate(
      group = as.character(group),
      characteristic = "Age group"
    ),
  
  sex_prevalence %>%
    mutate(
      group = as.character(group),
      characteristic = "Sex"
    ),
  
  education_prevalence %>%
    mutate(
      group = as.character(group),
      characteristic = "Education"
    ),
  
  bmi_prevalence %>%
    mutate(
      group = as.character(group),
      characteristic = "BMI group"
    ),
  
  smoking_prevalence %>%
    mutate(
      group = as.character(group),
      characteristic = "Smoking status"
    ),
  
  income_prevalence %>%
    mutate(
      group = as.character(group),
      characteristic = "Income quintile"
    ),
  
  province_prevalence %>%
    mutate(
      group = as.character(group),
      characteristic = "Province / territory"
    )
) %>%
  select(
    characteristic,
    group,
    diabetes_prevalence_percent,
    high_blood_pressure_prevalence_percent
  )

subgroup_prevalence

## 9.1 Convert subgroup results to long format

subgroup_prevalence_long <- subgroup_prevalence %>%
  pivot_longer(
    cols = c(
      diabetes_prevalence_percent,
      high_blood_pressure_prevalence_percent
    ),
    names_to = "outcome",
    values_to = "prevalence_percent"
  ) %>%
  mutate(
    outcome = recode(
      outcome,
      diabetes_prevalence_percent = "Diabetes",
      high_blood_pressure_prevalence_percent = "High blood pressure"
    )
  )

subgroup_prevalence_long

## ------------------------------------------------------------
## 10. Create descriptive prevalence visualizations
## ------------------------------------------------------------

## 10.1 Function for horizontal grouped prevalence bar charts

plot_prevalence <- function(
    data,
    characteristic_name,
    plot_title,
    y_axis_title,
    group_order = NULL
) {
  
  plot_data <- data %>%
    filter(characteristic == characteristic_name)
  
  # Apply a custom category order when one is provided.
  # The order is reversed here so that after coord_flip(),
  # categories appear from top to bottom in the order supplied.
  if (!is.null(group_order)) {
    plot_data <- plot_data %>%
      mutate(
        group = factor(
          group,
          levels = rev(group_order)
        )
      )
  }
  
  ggplot(
    plot_data,
    aes(
      x = group,
      y = prevalence_percent,
      fill = outcome
    )
  ) +
    geom_col(
      position = "dodge",
      width = 0.75
    ) +
    
    scale_fill_manual(
      values = c(
        "Diabetes" = "#9EC5E5",
        "High blood pressure" = "#F2A3A3"
      )
    ) +
    
    coord_flip() +
    
    labs(
      title = plot_title,
      x = y_axis_title,
      y = "Weighted prevalence (%)",
      fill = "Health outcome",
      caption = "Source: Statistics Canada, Canadian Community Health Survey 2022 PUMF"
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(
        size = 10,
        face = "bold",
        hjust = 0.5
      ),
      
      axis.title = element_text(
        size = 8
      ),
      
      axis.text = element_text(
        size = 8
      ),
      
      legend.title = element_text(
        size = 8,
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 7
      ),
      
      plot.caption = element_text(
        size = 8,
        hjust = 0.5
      )
    )
}


## 10.2 Prevalence by age group

age_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "Age group",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby Age Group",
  y_axis_title = "Age group",
  group_order = c(
    "18 to 34",
    "35 to 49",
    "50 to 64",
    "65 and older"
  )
)

age_plot


## 10.3 Prevalence by sex

sex_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "Sex",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby Sex",
  y_axis_title = "Sex",
  group_order = c(
    "Male",
    "Female"
  )
)

sex_plot


## 10.4 Prevalence by income quintile

income_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "Income quintile",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby Household Income Quintile",
  y_axis_title = "Household income quintile",
  group_order = c(
    "Quintile 1 (lowest)",
    "Quintile 2",
    "Quintile 3",
    "Quintile 4",
    "Quintile 5 (highest)"
  )
)

income_plot


## 10.5 Prevalence by BMI group

bmi_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "BMI group",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby BMI Group",
  y_axis_title = "BMI group",
  group_order = c(
    "Underweight / Normal weight",
    "Overweight / Obese"
  )
)

bmi_plot


## 10.6 Prevalence by education

education_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "Education",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby Household Education Level",
  y_axis_title = "Education level",
  group_order = c(
    "Less than secondary school",
    "Secondary school, no post-secondary",
    "Post-secondary education"
  )
)

education_plot


## 10.7 Prevalence by smoking status

smoking_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "Smoking status",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby Smoking Status",
  y_axis_title = "Smoking status",
  group_order = c(
    "Lifetime abstainer",
    "Experimental smoker",
    "Former occasional smoker",
    "Former daily smoker",
    "Current occasional smoker",
    "Current daily smoker"
  )
)

smoking_plot


## 10.8 Prevalence by province / territory

province_plot <- plot_prevalence(
  data = subgroup_prevalence_long,
  characteristic_name = "Province / territory",
  plot_title = "Weighted Prevalence of Diabetes and High Blood Pressure\nby Province / Territory",
  y_axis_title = "Province / territory",
  group_order = c(
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

province_plot

## ------------------------------------------------------------
## 11. Save descriptive prevalence figures
## ------------------------------------------------------------

# Save all descriptive prevalence figures to the figures folder.
# Using ggsave() ensures that figures can be reproduced directly
# by rerunning the analysis script.

ggsave(
  filename = "figures/prevalence_by_age.png",
  plot = age_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "figures/prevalence_by_sex.png",
  plot = sex_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figures/prevalence_by_income.png",
  plot = income_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "figures/prevalence_by_bmi.png",
  plot = bmi_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figures/prevalence_by_education.png",
  plot = education_plot,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "figures/prevalence_by_smoking.png",
  plot = smoking_plot,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "figures/prevalence_by_province.png",
  plot = province_plot,
  width = 10,
  height = 8,
  dpi = 300
)


## ------------------------------------------------------------
## 12. Export descriptive analysis tables
## ------------------------------------------------------------

# Export overall weighted prevalence estimates

write_csv(
  overall_prevalence,
  "data/processed/overall_prevalence.csv"
)


# Export subgroup prevalence estimates in wide format

write_csv(
  subgroup_prevalence,
  "data/processed/subgroup_prevalence.csv"
)


# Export subgroup prevalence estimates in long format for
# visualization tools such as Tableau and Power BI

write_csv(
  subgroup_prevalence_long,
  "data/processed/subgroup_prevalence_long.csv"
)

## ------------------------------------------------------------
## 13. Final quality-control checks
## ------------------------------------------------------------

# Verify that all expected output files exist

output_files <- c(
  "figures/prevalence_by_age.png",
  "figures/prevalence_by_sex.png",
  "figures/prevalence_by_income.png",
  "figures/prevalence_by_bmi.png",
  "figures/prevalence_by_education.png",
  "figures/prevalence_by_smoking.png",
  "figures/prevalence_by_province.png",
  "data/processed/overall_prevalence.csv",
  "data/processed/subgroup_prevalence.csv",
  "data/processed/subgroup_prevalence_long.csv"
)

qc_results <- data.frame(
  file = output_files,
  exists = file.exists(output_files)
)

print(qc_results)


# Stop execution if any expected output is missing

if (!all(qc_results$exists)) {
  stop(
    "One or more expected output files were not created."
  )
}


# Display a brief project summary

cat(
  "\n=====================================================\n",
  "DESCRIPTIVE ANALYSIS COMPLETE\n",
  "=====================================================\n",
  "\nSurvey: Canadian Community Health Survey 2022 PUMF\n",
  "\nOverall weighted prevalence estimates:\n"
)

print(overall_prevalence)

cat(
  "\nNumber of subgroup prevalence estimates:",
  nrow(subgroup_prevalence),
  "\n"
)

cat(
  "Number of visualization-ready observations:",
  nrow(subgroup_prevalence_long),
  "\n"
)

cat(
  "\nAll expected output files were successfully created.\n"
)