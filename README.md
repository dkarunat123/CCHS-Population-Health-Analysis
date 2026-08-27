# CCHS 2022 Population Health Analysis

An end-to-end population health analytics project using the **2022 Canadian Community Health Survey (CCHS) Public Use Microdata File (PUMF)**.

The project demonstrates a complete analytics workflow including data cleaning, survey-weighted descriptive analysis, regression modelling, data mart construction, SQL querying, and interactive dashboard development.

## Project Objectives

This project was created to demonstrate practical skills in:

- R programming
- data cleaning and transformation
- survey-weighted population health analysis
- logistic regression
- data visualization
- relational database design
- data mart / star-schema concepts
- SQL querying
- Power BI dashboard development
- reproducible analytics workflows

Tableau visualizations will also be added as a later extension of the project.

---

## Dataset

The analysis uses the **2022 Canadian Community Health Survey (CCHS) Public Use Microdata File**, published by Statistics Canada.

The CCHS collects information on health status, health behaviours, demographic characteristics, and determinants of health among the Canadian population.

The original PUMF contains approximately:

- **67,000 respondents**
- **255 variables**

Raw source files are not included in this repository due to file size and data redistribution considerations.

---

## Health Outcomes

The current analysis focuses on two health outcomes:

- Diabetes
- High blood pressure

Population prevalence and regression results were examined across characteristics including:

- Age group
- Sex
- BMI group
- Education
- Income quintile
- Smoking status
- Province / territory

---

## Analytical Workflow

### 1. Data Import

The CCHS PUMF was imported and inspected in R.

The import workflow includes:

- loading the original CSV
- inspecting variables and data types
- selecting variables relevant to the analysis
- saving intermediate R objects for reproducibility

Script:

```text
R/01_data_import.R
