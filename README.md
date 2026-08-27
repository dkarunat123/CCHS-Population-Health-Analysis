# CCHS 2022 Population Health Analysis

This project explores population health patterns in the [2022 Canadian Community Health Survey (CCHS) Public Use Microdata File](https://www150.statcan.gc.ca/n1/pub/82m0013x/82m0013x2024001-eng.htm). The goal was to build a complete analytics workflow starting with raw survey data and ending with cleaned analytical datasets, weighted prevalence estimates, regression results, a reusable data mart, SQL queries, and interactive dashboards.

The analysis focuses on diabetes and high blood pressure, with comparisons across age, sex, BMI, education, income, smoking status, and province or territory.

## Data and analysis

The original CCHS 2022 PUMF contains approximately 67,000 respondents and 255 variables. The raw data were imported and processed in R, where survey variables were recoded into more interpretable analysis-ready categories and the variables needed for the project were selected.

The descriptive analysis focused on weighted prevalence estimates rather than raw sample proportions. This allowed subgroup comparisons to better reflect the Canadian population represented by the survey. Weighted prevalence was calculated for diabetes and high blood pressure across several demographic, socioeconomic, behavioural, and geographic characteristics.

The statistical analysis extended this work using logistic regression. Both unadjusted and adjusted models were fitted, and the resulting odds ratios, 95% confidence intervals, and p-values were saved for later use in the data mart and dashboards.

The R workflow is organized into five scripts:

```text
R/
├── 01_data_import.R
├── 02_data_cleaning.R
├── 03_descriptive_analysis.R
├── 04_statistical_analysis.R
└── 05_build_data_mart.R
```
The scripts are intended to be run sequentially, beginning with the raw CCHS data and ending with the analytical tables used by SQL and business intelligence tools.

An example of the survey-weighted analysis used to estimate diabetes prevalence by age group is:

```r
cchs_design <- svydesign(
  ids = ~1,
  weights = ~WTS_M,
  data = cchs_clean
)

diabetes_by_age <- svyby(
  ~I(diabetes == "Yes"),
  ~age_group,
  design = subset(cchs_design, !is.na(diabetes)),
  FUN = svymean,
  na.rm = TRUE
)
```

## Data mart

After the descriptive and regression analyses were completed, the outputs were reorganized into a small analytical data mart.

The data mart follows a simple star-schema style structure:

```text
data/mart/
├── dim_characteristic.csv
├── dim_outcome.csv
├── fact_prevalence.csv
└── fact_regression.csv
```

`dim_outcome` stores the health outcomes included in the project, while `dim_characteristic` stores the population characteristics used for subgroup comparisons. `fact_prevalence` contains the weighted prevalence estimates, and `fact_regression` contains the regression outputs.

This structure separates descriptive information from analytical measurements and makes the processed results easier to reuse across different tools.

## SQL

The data mart was imported into a SQLite database and queried using DB Browser for SQLite.

The SQL portion of the project demonstrates relational querying using joins, filtering, sorting, aliases, rounding, and statistical conditions. The queries were written to answer practical analytical questions rather than simply demonstrate syntax.

Examples include identifying diabetes prevalence by age group, ranking population groups by prevalence, comparing diabetes prevalence across provinces and territories, filtering adjusted regression results by statistical significance, and comparing adjusted and unadjusted smoking-related odds ratios.

The SQL files are stored here:

```text
sql/
├── analysis_queries.sql
└── cchs_2022_health.db
```

An example query used to retrieve weighted diabetes prevalence by age group is:

```sql
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
```

## Power BI

The analytical data mart was also used to build an interactive Power BI report.

The report contains two main pages.

The **Prevalence Overview** page allows users to select a health outcome and population characteristic and compare weighted prevalence across groups. It also includes an overall weighted prevalence summary and interactive filtering.

The **Regression Results** page focuses on adjusted and unadjusted odds ratios. Users can filter by outcome and predictor, compare model estimates visually, and inspect a table containing odds ratios, confidence intervals, and p-values.

The Power BI file is stored in:

```text
powerbi/
```

## R visualizations

The R analysis also generated a set of static figures for subgroup prevalence and regression results.

These include prevalence comparisons by age, sex, BMI, education, income, smoking status, and province or territory, as well as regression forest plots for diabetes and high blood pressure.

The exported figures are stored in:

```text
figures/
```

These figures provide a static counterpart to the interactive Power BI dashboard and make the main results easy to preview directly from the repository.

## Selected results

One of the clearest patterns in the analysis was the increase in weighted diabetes prevalence with age.

In the 2022 CCHS data used here, diabetes prevalence increased from approximately 1.0% among adults aged 18 to 34 to approximately 18.2% among adults aged 65 and older.

The age-group estimates were:

- **18 to 34:** 1.0%
- **35 to 49:** 3.6%
- **50 to 64:** 10.3%
- **65 and older:** 18.2%

Provincial and territorial differences were also visible. In the processed results, Newfoundland and Labrador had the highest weighted diabetes prevalence among the available provincial and territorial estimates at approximately 11.9%, followed by Nova Scotia at approximately 9.8%.

The regression models provided an additional way to examine how demographic and behavioral characteristics were associated with the selected health outcomes. These results are presented alongside 95% confidence intervals and p-values so that the size and uncertainty of each estimate can be considered together.

## Processed data

Intermediate and analysis-ready outputs are stored in the `data/processed/` directory.

These include prevalence tables, regression output tables, and R data objects created during the workflow.

The larger original CCHS files are intentionally excluded from the repository.

## Repository structure

```text
CCHS-Population-Health-Analysis/
│
├── R/
│   ├── 01_data_import.R
│   ├── 02_data_cleaning.R
│   ├── 03_descriptive_analysis.R
│   ├── 04_statistical_analysis.R
│   └── 05_build_data_mart.R
│
├── data/
│   ├── mart/
│   │   ├── dim_characteristic.csv
│   │   ├── dim_outcome.csv
│   │   ├── fact_prevalence.csv
│   │   └── fact_regression.csv
│   │
│   └── processed/
│
├── figures/
│
├── powerbi/
│
├── sql/
│   ├── analysis_queries.sql
│   └── cchs_2022_health.db
│
├── tableau/
│
├── CCHS-Population-Health-Analysis.Rproj
├── .gitignore
└── README.md
```

## Reproducibility

The analytical workflow is organized so that the project can be rebuilt from the source data by running the R scripts in order:

```text
01_data_import.R
02_data_cleaning.R
03_descriptive_analysis.R
04_statistical_analysis.R
05_build_data_mart.R
```

The resulting data mart can then be used directly in SQLite, Power BI, or Tableau.

The raw CCHS data are not included in the repository because of their size. The `data/raw/` directory is excluded through `.gitignore`.

## Tools

This project uses **R, RStudio, SQLite, DB Browser for SQLite, and Power BI**.

R was used for the main data-processing and statistical workflow, SQLite for relational querying, and Power BI for interactive reporting.

A Tableau visualization will be added as a later extension using the same analytical data mart.

## Data source

Statistics Canada, **Canadian Community Health Survey (CCHS), 2022 Public Use Microdata File**.

The original survey data and accompanying documentation are available from Statistics Canada:

[CCHS 2022 Public Use Microdata File](https://www150.statcan.gc.ca/n1/pub/82m0013x/82m0013x2024001-eng.htm)
