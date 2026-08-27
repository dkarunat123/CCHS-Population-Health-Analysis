## ============================================================
## CCHS 2022 POPULATION HEALTH ANALYSIS
## Script 04: Statistical Analysis
##
## Purpose:
##   Examine associations between demographic, socioeconomic,
##   behavioural, and health characteristics and the prevalence
##   of diabetes and high blood pressure among Canadian adults
##   using survey-weighted logistic regression.
##
## Data source:
##   Statistics Canada
##   Canadian Community Health Survey (CCHS), 2022 PUMF
##
## Input:
##   data/processed/cchs_clean.rds
##
## Outputs:
##   Regression model results, odds-ratio tables, and figures
##   for downstream reporting and visualization.
##
## Important:
##   The CCHS master survey weight (WTS_M) is used to produce
##   weighted estimates. Because the complete complex survey
##   design information is not incorporated, model-based
##   standard errors and confidence intervals should not be
##   interpreted as official Statistics Canada design-based
##   variance estimates.
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
# weight. Each respondent contributes to model estimates
# according to their value of WTS_M.

cchs_design <- svydesign(
  ids = ~1,
  weights = ~WTS_M,
  data = cchs_clean
)

cchs_design

## ------------------------------------------------------------
## 4. Verify outcome and predictor reference levels
## ------------------------------------------------------------

# Check factor levels before fitting logistic regression models.

levels(cchs_clean$diabetes)
levels(cchs_clean$high_blood_pressure)

levels(cchs_clean$age_group)
levels(cchs_clean$sex)
levels(cchs_clean$education)
levels(cchs_clean$bmi_group)
levels(cchs_clean$smoking_status)
levels(cchs_clean$income_quintile)

## ------------------------------------------------------------
## 5. Prepare variables for regression modelling
## ------------------------------------------------------------

# Convert ordered categorical predictors to unordered factors.
# This ensures that regression coefficients compare each category
# directly with a clearly defined reference category rather than
# using polynomial contrasts for ordered factors.
#
# In R, the first factor level is used as the reference category.
# Reference categories are chosen to make the resulting odds ratios
# straightforward to interpret:
#
#   Age:       18 to 34 years
#   Education: Post-secondary education
#   Income:    Quintile 5 (highest)
#
# This means that the regression models will compare older age
# groups with the youngest group, lower education levels with
# post-secondary education, and lower income quintiles with the
# highest income quintile.


cchs_clean <- cchs_clean %>%
  mutate(
    
    # Youngest adult age group is the reference category.
    # as.character() removes the original ordered-factor class
    # before recreating the variable as an unordered factor.
    age_group = factor(
      as.character(age_group),
      levels = c(
        "18 to 34",
        "35 to 49",
        "50 to 64",
        "65 and older"
      ),
      ordered = FALSE
    ),
    
    # Highest education category is the reference category.
    education = factor(
      as.character(education),
      levels = c(
        "Post-secondary education",
        "Secondary school, no post-secondary",
        "Less than secondary school"
      ),
      ordered = FALSE
    ),
    
    # Highest household income quintile is the reference category.
    income_quintile = factor(
      as.character(income_quintile),
      levels = c(
        "Quintile 5 (highest)",
        "Quintile 4",
        "Quintile 3",
        "Quintile 2",
        "Quintile 1 (lowest)"
      ),
      ordered = FALSE
    )
  )


# Recreate the survey design object so that it contains the
# updated factor coding.

cchs_design <- svydesign(
  ids = ~1,
  weights = ~WTS_M,
  data = cchs_clean
)


## ------------------------------------------------------------
## 5.1 Confirm regression factor structure and reference levels
## ------------------------------------------------------------

# The first displayed level is the reference category used in
# treatment contrasts for each regression predictor.

levels(cchs_clean$age_group)
levels(cchs_clean$education)
levels(cchs_clean$income_quintile)


# Confirm that these variables are no longer ordered factors.
# All three checks should return FALSE.

is.ordered(cchs_clean$age_group)
is.ordered(cchs_clean$education)
is.ordered(cchs_clean$income_quintile)


# Inspect the treatment-contrast coding for age.
#
# The reference category ("18 to 34") should contain zeros across
# all contrast columns, while each older age group receives its
# own comparison column.

contrasts(cchs_clean$age_group)

## ------------------------------------------------------------
## 6. Unadjusted survey-weighted logistic regression
## ------------------------------------------------------------

## 6.1 Diabetes and BMI

# Fit an unadjusted logistic regression model examining the
# association between BMI group and diabetes.

diabetes_bmi_model <- svyglm(
  diabetes ~ bmi_group,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(bmi_group)
  ),
  family = quasibinomial()
)

summary(diabetes_bmi_model)

## 6.2 Convert regression coefficients to odds ratios

# Logistic regression coefficients are expressed in log-odds units.
# Exponentiating the coefficients converts them to odds ratios,
# which are easier to interpret.

exp(coef(diabetes_bmi_model))

## ------------------------------------------------------------
## 7. Unadjusted diabetes models
## ------------------------------------------------------------

# Fit separate survey-weighted logistic regression models for
# diabetes and each predictor.
#
# Because each model contains only one predictor, the resulting
# odds ratios describe unadjusted associations. They do not
# account for differences in the other characteristics.


## 7.1 Age group

diabetes_age_model <- svyglm(
  diabetes ~ age_group,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(age_group)
  ),
  family = quasibinomial()
)


## 7.2 Sex

diabetes_sex_model <- svyglm(
  diabetes ~ sex,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(sex)
  ),
  family = quasibinomial()
)


## 7.3 Education

diabetes_education_model <- svyglm(
  diabetes ~ education,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(education)
  ),
  family = quasibinomial()
)


## 7.4 BMI group

# This reproduces the BMI model examined individually in
# Section 6.

diabetes_bmi_model <- svyglm(
  diabetes ~ bmi_group,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(bmi_group)
  ),
  family = quasibinomial()
)


## 7.5 Smoking status

diabetes_smoking_model <- svyglm(
  diabetes ~ smoking_status,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(smoking_status)
  ),
  family = quasibinomial()
)


## 7.6 Household income quintile

diabetes_income_model <- svyglm(
  diabetes ~ income_quintile,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(income_quintile)
  ),
  family = quasibinomial()
)

## 7.7 Examine unadjusted odds ratios

exp(coef(diabetes_age_model))
exp(coef(diabetes_sex_model))
exp(coef(diabetes_education_model))
exp(coef(diabetes_bmi_model))
exp(coef(diabetes_smoking_model))
exp(coef(diabetes_income_model))

## ------------------------------------------------------------
## 8. Create clean unadjusted diabetes odds-ratio table
## ------------------------------------------------------------

## 8.1 Function to extract odds ratios, confidence intervals,
# and p-values from a fitted model

extract_or <- function(model, predictor_name) {
  
  # Extract regression coefficients
  beta <- coef(model)
  
  # Extract coefficient table from model summary
  coef_table <- summary(model)$coefficients
  
  # Extract standard errors
  se <- coef_table[, "Std. Error"]
  
  # Calculate 95% confidence intervals on the log-odds scale
  lower_log <- beta - 1.96 * se
  upper_log <- beta + 1.96 * se
  
  # Convert coefficients and confidence limits to odds ratios
  model_results <- tibble(
    predictor = predictor_name,
    comparison = names(beta),
    odds_ratio = exp(beta),
    lower_ci = exp(lower_log),
    upper_ci = exp(upper_log),
    p_value = coef_table[, "Pr(>|t|)"]
  ) %>%
    filter(comparison != "(Intercept)")
  
  return(model_results)
}

## 8.2 Combine unadjusted diabetes odds ratios

diabetes_unadjusted_or <- bind_rows(
  
  extract_or(
    diabetes_age_model,
    "Age group"
  ),
  
  extract_or(
    diabetes_sex_model,
    "Sex"
  ),
  
  extract_or(
    diabetes_education_model,
    "Education"
  ),
  
  extract_or(
    diabetes_bmi_model,
    "BMI group"
  ),
  
  extract_or(
    diabetes_smoking_model,
    "Smoking status"
  ),
  
  extract_or(
    diabetes_income_model,
    "Income quintile"
  )
)

diabetes_unadjusted_or

## ------------------------------------------------------------
## 9. Adjusted diabetes model
## ------------------------------------------------------------

# Fit a multivariable survey-weighted logistic regression model
# including all selected predictors simultaneously.

# The resulting odds ratios are adjusted for the other variables
# included in the model.

diabetes_adjusted_model <- svyglm(
  diabetes ~
    age_group +
    sex +
    education +
    bmi_group +
    smoking_status +
    income_quintile,
  design = subset(
    cchs_design,
    !is.na(diabetes) &
      !is.na(age_group) &
      !is.na(sex) &
      !is.na(education) &
      !is.na(bmi_group) &
      !is.na(smoking_status) &
      !is.na(income_quintile)
  ),
  family = quasibinomial()
)

summary(diabetes_adjusted_model)

# Examine adjusted odds ratios
exp(coef(diabetes_adjusted_model))

## ------------------------------------------------------------
## 10. Create clean adjusted diabetes odds-ratio table
## ------------------------------------------------------------

# Extract adjusted odds ratios from the multivariable diabetes
# model. Each odds ratio represents the association between a
# predictor and diabetes after accounting for all other variables
# included in the model.

diabetes_adjusted_or <- extract_or(
  diabetes_adjusted_model,
  "Adjusted model"
)

diabetes_adjusted_or

## ------------------------------------------------------------
## 11. Compare unadjusted and adjusted diabetes odds ratios
## ------------------------------------------------------------

## 11.1 Combine unadjusted and adjusted odds ratios

diabetes_or_comparison <- diabetes_unadjusted_or %>%
  select(
    comparison,
    unadjusted_or = odds_ratio,
    unadjusted_lower_ci = lower_ci,
    unadjusted_upper_ci = upper_ci,
    unadjusted_p_value = p_value
  ) %>%
  
  left_join(
    diabetes_adjusted_or %>%
      select(
        comparison,
        adjusted_or = odds_ratio,
        adjusted_lower_ci = lower_ci,
        adjusted_upper_ci = upper_ci,
        adjusted_p_value = p_value
      ),
    by = "comparison"
  )

## 11.2 Add readable predictor and comparison labels

diabetes_or_comparison <- diabetes_or_comparison %>%
  mutate(
    
    predictor = case_when(
      str_starts(comparison, "age_group") ~ "Age group",
      str_starts(comparison, "sex") ~ "Sex",
      str_starts(comparison, "education") ~ "Education",
      str_starts(comparison, "bmi_group") ~ "BMI group",
      str_starts(comparison, "smoking_status") ~ "Smoking status",
      str_starts(comparison, "income_quintile") ~ "Income quintile",
      TRUE ~ NA_character_
    ),
    
    comparison_label = case_when(
      
      comparison == "age_group35 to 49" ~
        "35 to 49 vs 18 to 34",
      
      comparison == "age_group50 to 64" ~
        "50 to 64 vs 18 to 34",
      
      comparison == "age_group65 and older" ~
        "65 and older vs 18 to 34",
      
      comparison == "sexFemale" ~
        "Female vs Male",
      
      comparison == "educationSecondary school, no post-secondary" ~
        "Secondary school, no post-secondary vs Post-secondary",
      
      comparison == "educationLess than secondary school" ~
        "Less than secondary school vs Post-secondary",
      
      comparison == "bmi_groupOverweight / Obese" ~
        "Overweight / Obese vs Underweight / Normal weight",
      
      comparison == "smoking_statusExperimental smoker" ~
        "Experimental smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusFormer occasional smoker" ~
        "Former occasional smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusFormer daily smoker" ~
        "Former daily smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusCurrent occasional smoker" ~
        "Current occasional smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusCurrent daily smoker" ~
        "Current daily smoker vs Lifetime abstainer",
      
      comparison == "income_quintileQuintile 4" ~
        "Quintile 4 vs Quintile 5 (highest)",
      
      comparison == "income_quintileQuintile 3" ~
        "Quintile 3 vs Quintile 5 (highest)",
      
      comparison == "income_quintileQuintile 2" ~
        "Quintile 2 vs Quintile 5 (highest)",
      
      comparison == "income_quintileQuintile 1 (lowest)" ~
        "Quintile 1 (lowest) vs Quintile 5 (highest)",
      
      TRUE ~ comparison
    )
  ) %>%
  
  select(
    predictor,
    comparison = comparison_label,
    
    unadjusted_or,
    unadjusted_lower_ci,
    unadjusted_upper_ci,
    unadjusted_p_value,
    
    adjusted_or,
    adjusted_lower_ci,
    adjusted_upper_ci,
    adjusted_p_value
  )

diabetes_or_comparison

## ------------------------------------------------------------
## 12. Prepare diabetes odds ratios for visualization
## ------------------------------------------------------------

diabetes_or_long <- bind_rows(
  
  diabetes_or_comparison %>%
    transmute(
      predictor,
      comparison,
      model = "Unadjusted",
      odds_ratio = unadjusted_or,
      lower_ci = unadjusted_lower_ci,
      upper_ci = unadjusted_upper_ci,
      p_value = unadjusted_p_value
    ),
  
  diabetes_or_comparison %>%
    transmute(
      predictor,
      comparison,
      model = "Adjusted",
      odds_ratio = adjusted_or,
      lower_ci = adjusted_lower_ci,
      upper_ci = adjusted_upper_ci,
      p_value = adjusted_p_value
    )
) %>%
  mutate(
    
    comparison = factor(
      comparison,
      levels = c(
        
        # Income quintile
        "Quintile 1 (lowest) vs Quintile 5 (highest)",
        "Quintile 2 vs Quintile 5 (highest)",
        "Quintile 3 vs Quintile 5 (highest)",
        "Quintile 4 vs Quintile 5 (highest)",
        
        # Smoking status
        "Current daily smoker vs Lifetime abstainer",
        "Current occasional smoker vs Lifetime abstainer",
        "Former daily smoker vs Lifetime abstainer",
        "Former occasional smoker vs Lifetime abstainer",
        "Experimental smoker vs Lifetime abstainer",
        
        # BMI group
        "Overweight / Obese vs Underweight / Normal weight",
        
        # Education
        "Less than secondary school vs Post-secondary",
        "Secondary school, no post-secondary vs Post-secondary",
        
        # Sex
        "Female vs Male",
        
        # Age group
        "65 and older vs 18 to 34",
        "50 to 64 vs 18 to 34",
        "35 to 49 vs 18 to 34"
      )
    )
  )

diabetes_or_long

## ------------------------------------------------------------
## 13. Create diabetes odds-ratio comparison plot
## ------------------------------------------------------------

# Define the order of predictor groups in the plot

predictor_order <- c(
  "Age group",
  "Sex",
  "Education",
  "BMI group",
  "Smoking status",
  "Income quintile"
)

diabetes_or_plot_data <- diabetes_or_long %>%
  mutate(
    predictor = factor(
      predictor,
      levels = predictor_order
    )
  )


diabetes_or_plot <- ggplot(
  diabetes_or_plot_data,
  aes(
    x = odds_ratio,
    y = comparison,
    shape = model
  )
) +
  
  # Reference line indicating no association
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  
  # 95% confidence intervals
  geom_errorbar(
    aes(
      xmin = lower_ci,
      xmax = upper_ci
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  
  # Odds-ratio point estimates
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.5)
  ) +
  
  # Odds ratios are displayed on a logarithmic scale
  scale_x_log10() +
  
  # Separate predictor groups while retaining a common x-axis
  facet_grid(
    predictor ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  labs(
    title = "Unadjusted and Adjusted Odds Ratios for Diabetes",
    x = "Odds ratio (log scale)",
    y = NULL,
    shape = "Model",
    caption = paste0(
      "Reference line at OR = 1 indicates no association.\n",
      "Confidence intervals are based on the simplified weighted survey design ",
      "and are not official Statistics Canada design-based variance estimates."
    )
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.text = element_text(
      size = 8
    ),
    
    axis.title = element_text(
      size = 8
    ),
    
    strip.text.y = element_text(
      size = 8,
      face = "bold",
      angle = 0
    ),
    
    legend.title = element_text(
      size = 8,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 7
    ),
    
    plot.caption = element_text(
      size = 6.5,
      hjust = 0.5
    )
  )

diabetes_or_plot

# Saving plot to file
ggsave(
  "figures/diabetes_odds_ratio_forest_plot.png",
  plot = diabetes_or_plot,
  width = 12,
  height = 7,
  dpi = 300
)

## ------------------------------------------------------------
## 14. Export diabetes regression results
## ------------------------------------------------------------

# Export the combined unadjusted and adjusted diabetes
# odds-ratio table for reporting and downstream visualization.
write_csv(
  diabetes_or_comparison,
  "data/processed/diabetes_odds_ratio_comparison.csv"
)


# Export the long-format diabetes regression results used
# for the forest plot. This format is convenient for Tableau,
# Power BI, and other visualization tools.
write_csv(
  diabetes_or_long,
  "data/processed/diabetes_odds_ratio_long.csv"
)

## ------------------------------------------------------------
## 15. Unadjusted high blood pressure models
## ------------------------------------------------------------

# Fit separate survey-weighted logistic regression models for
# high blood pressure and each predictor.
#
# Because each model contains only one predictor, the resulting
# odds ratios describe unadjusted associations. They do not
# account for differences in the other characteristics.


## 15.1 Age group

high_bp_age_model <- svyglm(
  high_blood_pressure ~ age_group,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(age_group)
  ),
  family = quasibinomial()
)


## 15.2 Sex

high_bp_sex_model <- svyglm(
  high_blood_pressure ~ sex,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(sex)
  ),
  family = quasibinomial()
)


## 15.3 Education

high_bp_education_model <- svyglm(
  high_blood_pressure ~ education,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(education)
  ),
  family = quasibinomial()
)


## 15.4 BMI group

high_bp_bmi_model <- svyglm(
  high_blood_pressure ~ bmi_group,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(bmi_group)
  ),
  family = quasibinomial()
)


## 15.5 Smoking status

high_bp_smoking_model <- svyglm(
  high_blood_pressure ~ smoking_status,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(smoking_status)
  ),
  family = quasibinomial()
)


## 15.6 Household income quintile

high_bp_income_model <- svyglm(
  high_blood_pressure ~ income_quintile,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(income_quintile)
  ),
  family = quasibinomial()
)


## 15.7 Examine unadjusted odds ratios

# Exponentiate the regression coefficients to convert the
# log-odds coefficients to odds ratios.
#
# For categorical predictors, each odds ratio compares the
# displayed category with that predictor's reference category.

exp(coef(high_bp_age_model))
exp(coef(high_bp_sex_model))
exp(coef(high_bp_education_model))
exp(coef(high_bp_bmi_model))
exp(coef(high_bp_smoking_model))
exp(coef(high_bp_income_model))

## ----------------------------------------------------------------
## 16. Create clean unadjusted high blood pressure odds-ratio table
## ----------------------------------------------------------------

# Use the extract_or() function created in Section 8 to extract
# odds ratios, 95% confidence intervals, and p-values from each
# unadjusted high blood pressure model.

high_bp_unadjusted_or <- bind_rows(
  
  extract_or(
    high_bp_age_model,
    "Age group"
  ),
  
  extract_or(
    high_bp_sex_model,
    "Sex"
  ),
  
  extract_or(
    high_bp_education_model,
    "Education"
  ),
  
  extract_or(
    high_bp_bmi_model,
    "BMI group"
  ),
  
  extract_or(
    high_bp_smoking_model,
    "Smoking status"
  ),
  
  extract_or(
    high_bp_income_model,
    "Income quintile"
  )
)


# Examine the combined unadjusted results.
high_bp_unadjusted_or

## ------------------------------------------------------------
## 17. Adjusted high blood pressure model
## ------------------------------------------------------------

# Fit a multivariable survey-weighted logistic regression model
# including all selected predictors simultaneously.

# The resulting odds ratios are adjusted for the other variables
# included in the model.

high_bp_adjusted_model <- svyglm(
  high_blood_pressure ~
    age_group +
    sex +
    education +
    bmi_group +
    smoking_status +
    income_quintile,
  design = subset(
    cchs_design,
    !is.na(high_blood_pressure) &
      !is.na(age_group) &
      !is.na(sex) &
      !is.na(education) &
      !is.na(bmi_group) &
      !is.na(smoking_status) &
      !is.na(income_quintile)
  ),
  family = quasibinomial()
)


# Examine the fitted multivariable model.

summary(high_bp_adjusted_model)


# Examine adjusted odds ratios.

exp(coef(high_bp_adjusted_model))

## ------------------------------------------------------------
## 18. Create clean adjusted high blood pressure odds-ratio table
## ------------------------------------------------------------

# Extract adjusted odds ratios, 95% confidence intervals, and
# p-values from the multivariable high blood pressure model.
#
# Each odds ratio represents the association between a predictor
# and high blood pressure after accounting for all other variables
# included in the model.

high_bp_adjusted_or <- extract_or(
  high_bp_adjusted_model,
  "Adjusted model"
)


# Examine the adjusted results.

high_bp_adjusted_or

## ------------------------------------------------------------
## 19. Compare unadjusted and adjusted high blood pressure odds ratios
## ------------------------------------------------------------

## 19.1 Combine unadjusted and adjusted odds ratios

high_bp_or_comparison <- high_bp_unadjusted_or %>%
  select(
    comparison,
    unadjusted_or = odds_ratio,
    unadjusted_lower_ci = lower_ci,
    unadjusted_upper_ci = upper_ci,
    unadjusted_p_value = p_value
  ) %>%
  
  left_join(
    high_bp_adjusted_or %>%
      select(
        comparison,
        adjusted_or = odds_ratio,
        adjusted_lower_ci = lower_ci,
        adjusted_upper_ci = upper_ci,
        adjusted_p_value = p_value
      ),
    by = "comparison"
  )


## 19.2 Add readable predictor and comparison labels

high_bp_or_comparison <- high_bp_or_comparison %>%
  mutate(
    
    predictor = case_when(
      str_starts(comparison, "age_group") ~ "Age group",
      str_starts(comparison, "sex") ~ "Sex",
      str_starts(comparison, "education") ~ "Education",
      str_starts(comparison, "bmi_group") ~ "BMI group",
      str_starts(comparison, "smoking_status") ~ "Smoking status",
      str_starts(comparison, "income_quintile") ~ "Income quintile",
      TRUE ~ NA_character_
    ),
    
    comparison_label = case_when(
      
      comparison == "age_group35 to 49" ~
        "35 to 49 vs 18 to 34",
      
      comparison == "age_group50 to 64" ~
        "50 to 64 vs 18 to 34",
      
      comparison == "age_group65 and older" ~
        "65 and older vs 18 to 34",
      
      comparison == "sexFemale" ~
        "Female vs Male",
      
      comparison == "educationSecondary school, no post-secondary" ~
        "Secondary school, no post-secondary vs Post-secondary",
      
      comparison == "educationLess than secondary school" ~
        "Less than secondary school vs Post-secondary",
      
      comparison == "bmi_groupOverweight / Obese" ~
        "Overweight / Obese vs Underweight / Normal weight",
      
      comparison == "smoking_statusExperimental smoker" ~
        "Experimental smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusFormer occasional smoker" ~
        "Former occasional smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusFormer daily smoker" ~
        "Former daily smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusCurrent occasional smoker" ~
        "Current occasional smoker vs Lifetime abstainer",
      
      comparison == "smoking_statusCurrent daily smoker" ~
        "Current daily smoker vs Lifetime abstainer",
      
      comparison == "income_quintileQuintile 4" ~
        "Quintile 4 vs Quintile 5 (highest)",
      
      comparison == "income_quintileQuintile 3" ~
        "Quintile 3 vs Quintile 5 (highest)",
      
      comparison == "income_quintileQuintile 2" ~
        "Quintile 2 vs Quintile 5 (highest)",
      
      comparison == "income_quintileQuintile 1 (lowest)" ~
        "Quintile 1 (lowest) vs Quintile 5 (highest)",
      
      TRUE ~ comparison
    )
  ) %>%
  
  select(
    predictor,
    comparison = comparison_label,
    
    unadjusted_or,
    unadjusted_lower_ci,
    unadjusted_upper_ci,
    unadjusted_p_value,
    
    adjusted_or,
    adjusted_lower_ci,
    adjusted_upper_ci,
    adjusted_p_value
  )


# Examine the combined comparison table.

high_bp_or_comparison

## ------------------------------------------------------------
## 20. Prepare high blood pressure odds ratios for visualization
## ------------------------------------------------------------

high_bp_or_long <- bind_rows(
  
  high_bp_or_comparison %>%
    transmute(
      predictor,
      comparison,
      model = "Unadjusted",
      odds_ratio = unadjusted_or,
      lower_ci = unadjusted_lower_ci,
      upper_ci = unadjusted_upper_ci,
      p_value = unadjusted_p_value
    ),
  
  high_bp_or_comparison %>%
    transmute(
      predictor,
      comparison,
      model = "Adjusted",
      odds_ratio = adjusted_or,
      lower_ci = adjusted_lower_ci,
      upper_ci = adjusted_upper_ci,
      p_value = adjusted_p_value
    )
) %>%
  mutate(
    
    comparison = factor(
      comparison,
      levels = c(
        
        # Income quintile
        "Quintile 1 (lowest) vs Quintile 5 (highest)",
        "Quintile 2 vs Quintile 5 (highest)",
        "Quintile 3 vs Quintile 5 (highest)",
        "Quintile 4 vs Quintile 5 (highest)",
        
        # Smoking status
        "Current daily smoker vs Lifetime abstainer",
        "Current occasional smoker vs Lifetime abstainer",
        "Former daily smoker vs Lifetime abstainer",
        "Former occasional smoker vs Lifetime abstainer",
        "Experimental smoker vs Lifetime abstainer",
        
        # BMI group
        "Overweight / Obese vs Underweight / Normal weight",
        
        # Education
        "Less than secondary school vs Post-secondary",
        "Secondary school, no post-secondary vs Post-secondary",
        
        # Sex
        "Female vs Male",
        
        # Age group
        "65 and older vs 18 to 34",
        "50 to 64 vs 18 to 34",
        "35 to 49 vs 18 to 34"
      )
    )
  )

high_bp_or_long

## ------------------------------------------------------------
## 21. Create high blood pressure odds-ratio comparison plot
## ------------------------------------------------------------

high_bp_or_plot_data <- high_bp_or_long %>%
  mutate(
    predictor = factor(
      predictor,
      levels = predictor_order
    )
  )


high_bp_or_plot <- ggplot(
  high_bp_or_plot_data,
  aes(
    x = odds_ratio,
    y = comparison,
    shape = model
  )
) +
  
  # Reference line indicating no association
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  
  # 95% confidence intervals
  geom_errorbar(
    aes(
      xmin = lower_ci,
      xmax = upper_ci
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  
  # Odds-ratio point estimates
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.5)
  ) +
  
  # Odds ratios are displayed on a logarithmic scale
  scale_x_log10() +
  
  # Separate predictor groups while retaining a common x-axis
  facet_grid(
    predictor ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  labs(
    title = "Unadjusted and Adjusted Odds Ratios for High Blood Pressure",
    x = "Odds ratio (log scale)",
    y = NULL,
    shape = "Model",
    caption = paste0(
      "Reference line at OR = 1 indicates no association.\n",
      "Confidence intervals are based on the simplified weighted survey design ",
      "and are not official Statistics Canada design-based variance estimates."
    )
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.text = element_text(
      size = 8
    ),
    
    axis.title = element_text(
      size = 8
    ),
    
    strip.text.y = element_text(
      size = 8,
      face = "bold",
      angle = 0
    ),
    
    legend.title = element_text(
      size = 8,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 7
    ),
    
    plot.caption = element_text(
      size = 6.5,
      hjust = 0.5
    )
  )

high_bp_or_plot

## ------------------------------------------------------------
## 22. Export high blood pressure regression results
## ------------------------------------------------------------

# Save the high blood pressure forest plot.
ggsave(
  "figures/high_bp_odds_ratio_forest_plot.png",
  plot = high_bp_or_plot,
  width = 12,
  height = 7,
  dpi = 300
)


# Export the combined unadjusted and adjusted high blood pressure
# odds-ratio table for reporting and downstream visualization.
write_csv(
  high_bp_or_comparison,
  "data/processed/high_bp_odds_ratio_comparison.csv"
)


# Export the long-format high blood pressure regression results
# used for the forest plot. This format is convenient for use in 
# subsequent visualization in Tableau and Power BI.
write_csv(
  high_bp_or_long,
  "data/processed/high_bp_odds_ratio_long.csv"
)

## ------------------------------------------------------------
## 23. Final quality-control checks
## ------------------------------------------------------------

# Confirm expected numbers of regression results.
# Each outcome contains 16 predictor comparisons.
# The long-format tables contain each comparison twice:
# once for the unadjusted model and once for the adjusted model.

nrow(diabetes_or_comparison)      # Expected: 16
nrow(diabetes_or_long)            # Expected: 32

nrow(high_bp_or_comparison)       # Expected: 16
nrow(high_bp_or_long)             # Expected: 32


# Check for missing values in the final regression tables.
# All results should return 0.

sum(is.na(diabetes_or_long$odds_ratio))
sum(is.na(diabetes_or_long$lower_ci))
sum(is.na(diabetes_or_long$upper_ci))

sum(is.na(high_bp_or_long$odds_ratio))
sum(is.na(high_bp_or_long$lower_ci))
sum(is.na(high_bp_or_long$upper_ci))


# Confirm that odds ratios and confidence interval limits are positive.
# All checks should return TRUE.

all(diabetes_or_long$odds_ratio > 0)
all(diabetes_or_long$lower_ci > 0)
all(diabetes_or_long$upper_ci > 0)

all(high_bp_or_long$odds_ratio > 0)
all(high_bp_or_long$lower_ci > 0)
all(high_bp_or_long$upper_ci > 0)


# Confirm that all expected output files were created successfully.
# All values should return TRUE.

file.exists(
  c(
    "data/processed/diabetes_odds_ratio_comparison.csv",
    "data/processed/diabetes_odds_ratio_long.csv",
    "data/processed/high_bp_odds_ratio_comparison.csv",
    "data/processed/high_bp_odds_ratio_long.csv",
    "figures/diabetes_odds_ratio_forest_plot.png",
    "figures/high_bp_odds_ratio_forest_plot.png"
  )
)