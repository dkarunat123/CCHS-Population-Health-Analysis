-- ============================================================
-- CCHS 2022 Population Health Analysis
-- SQL Portfolio Queries
-- ============================================================


-- ------------------------------------------------------------
-- Query 1: Weighted diabetes prevalence by age group
-- ------------------------------------------------------------

SELECT
    d.outcome,
    f."group" AS age_group,
    ROUND(f.prevalence_percent, 1) AS prevalence_percent
FROM fact_prevalence AS f
JOIN dim_outcome AS d
    ON f.outcome_id = d.outcome_id
WHERE d.outcome = 'Diabetes'
  AND f.characteristic = 'Age group'
ORDER BY f.prevalence_percent DESC;


-- ------------------------------------------------------------
-- Query 2: Population groups with the highest diabetes prevalence
-- ------------------------------------------------------------

SELECT
    f.characteristic,
    f."group",
    ROUND(f.prevalence_percent, 1) AS prevalence_percent
FROM fact_prevalence AS f
JOIN dim_outcome AS d
    ON f.outcome_id = d.outcome_id
WHERE d.outcome = 'Diabetes'
  AND f.characteristic <> 'Overall'
ORDER BY f.prevalence_percent DESC;


-- ------------------------------------------------------------
-- Query 3: Diabetes prevalence by province / territory
-- ------------------------------------------------------------

SELECT
    f."group" AS province_territory,
    ROUND(f.prevalence_percent, 1) AS prevalence_percent
FROM fact_prevalence AS f
JOIN dim_outcome AS d
    ON f.outcome_id = d.outcome_id
WHERE d.outcome = 'Diabetes'
  AND f.characteristic = 'Province / territory'
ORDER BY f.prevalence_percent DESC;


-- ------------------------------------------------------------
-- Query 4: Statistically significant adjusted regression results
-- ------------------------------------------------------------

SELECT
    d.outcome,
    r.predictor,
    r.comparison,
    ROUND(r.odds_ratio, 2) AS odds_ratio,
    ROUND(r.lower_ci, 2) AS lower_95_ci,
    ROUND(r.upper_ci, 2) AS upper_95_ci,
    r.p_value
FROM fact_regression AS r
JOIN dim_outcome AS d
    ON r.outcome_id = d.outcome_id
WHERE d.outcome = 'Diabetes'
  AND r.model = 'Adjusted'
  AND r.p_value < 0.05
ORDER BY r.odds_ratio DESC;


-- ------------------------------------------------------------
-- Query 5: Adjusted vs. unadjusted odds ratios for smoking status
-- ------------------------------------------------------------

SELECT
    d.outcome,
    r.predictor,
    r.comparison,
    r.model,
    ROUND(r.odds_ratio, 2) AS odds_ratio,
    ROUND(r.lower_ci, 2) AS lower_95_ci,
    ROUND(r.upper_ci, 2) AS upper_95_ci,
    r.p_value
FROM fact_regression AS r
JOIN dim_outcome AS d
    ON r.outcome_id = d.outcome_id
WHERE d.outcome = 'Diabetes'
  AND r.predictor = 'Smoking status'
ORDER BY r.comparison, r.model;