# 📊 1993 US Car Specifications: Comprehensive Statistical Analysis

<div align="left">
  <img src="https://img.shields.io/badge/Language-R-blue?style=flat-square&logo=R" alt="R Badge">
  <img src="https://img.shields.io/badge/Environment-RStudio-75AADB?style=flat-square&logo=RStudio&logoColor=white" alt="RStudio Badge">
  <img src="https://img.shields.io/badge/Focus-Statistical_Analysis_%7C_Regression-success?style=flat-square" alt="Focus Badge">
  <img src="https://img.shields.io/badge/Institution-AUTh-orange?style=flat-square" alt="AUTh Badge">
</div>
<br>

**Author:** Alexandros Tsingos  
**Program:** MSc in Data and Web Science, Aristotle University of Thessaloniki (AUTh)

## 🚀 Project Overview

This repository contains an end-to-end statistical analysis of the **1993 US Car Specifications** dataset (`93Cars`). The project explores the relationships between vehicle characteristics, fuel efficiency, and market pricing through rigorous statistical methodologies. 

The workflow spans from initial data cleaning and advanced regression imputation for handling missing values, through comprehensive hypothesis testing, to the development of predictive regression models for both vehicle minimum price and city fuel efficiency (MPG).

## 🛠️ Methodologies & Techniques

*   **Data Engineering:** Data cleaning, variable transformation, and regression imputation to accurately handle missing observations.
*   **Exploratory Data Analysis (EDA):** Univariate and bivariate analysis, distribution mapping (linear and log scales), and correlation matrices.
*   **Hypothesis Testing:**
    *   Analysis of Variance (ANOVA) and ANCOVA to test group means (e.g., maximum price by airbag availability).
    *   Chi-Square tests for categorical independence.
*   **Predictive Modeling:** 
    *   Linear Regression modeling (predicting Minimum Price).
    *   Regression Trees (predicting City MPG).
    *   Residual analysis and model diagnostics.

## 📂 Repository Structure

```text
.
├── archive/                                # Legacy compressed files
├── data/
│   ├── processed/                          # Cleaned dataset (cleaned_93Cars.csv)
│   └── raw/                                # Original 93Cars labels and values
├── references/                             # Variable dictionaries and dataset metadata
├── reports/                                # Final analytical report (Report_ΑΕΜ_205.pdf)
├── results/
│   └── figures/                            # All exported visualisations (boxplots, trees, regression plots)
├── scripts/                                # R scripts ordered sequentially by execution
│   ├── 01_Data_Preparation.R               # Cleaning & regression imputation
│   ├── 02_Univariate_Analysis.R
│   ├── 03_Bivariate_Analysis.R
│   ├── 04_ANOVA_Analysis.R
│   ├── 05_Chi_Square_Analysis.R
│   ├── 06_Correlation_Analysis.R
│   ├── 07_ANCOVA.R
│   ├── 08_ONEWAY_ANOVA_MaxPrice_AirBags.R
│   ├── 09_Regression_preparation_Minimum_Price.R
│   ├── 10_Regression_Analysis_Minimu_Price.R
│   ├── 11_Regression_preparation_CityMPG.R
│   └── 12_regression_Citympg.R
├── README.md                               # Project overview
└── Statistical-Analysis-Project.Rproj      # RStudio project configuration

```

## ⚙️ How to Run the Analysis

1. Clone the repository:

```bash
    git clone [https://github.com/Alexisjr2001/YOUR-REPO-NAME.git](https://github.com/Alexisjr2001/YOUR-REPO-NAME.git)
    ```
2.  Open the `Statistical-Analysis-Project.Rproj` file in **RStudio**. This will automatically set the correct working directory.
3.  Navigate to the `scripts/` folder.
4.  Run the scripts sequentially, starting with `01_Data_Preparation.R`. This script will read from `data/raw/` and generate the `cleaned_93Cars.csv` file inside `data/processed/`, which is required for all subsequent scripts.

## 📈 Key Findings

*Detailed insights, including model coefficients, hypothesis test results, and final conclusions, are thoroughly documented in the `reports/Report_ΑΕΜ_205.pdf` file.*