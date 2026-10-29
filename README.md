# ST558_Project2

This project creates a shiny app for dynamic data exploration of the “Music and Mental Health” (MXMH) survey.

The MxMH survey was collected by Catherine Rasgaitis in 2022, and this dataset is available for public use on [Kaggle.com](https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results).

## /data

This project uses a copy of the MxMH data that was downloaded on 2026-10-20 (mxmh_survey_results.csv). Minimal data cleaning was performed in the Static_EDA.qmd file, and the resulting dataset is saved to the project space (mxmh.RData).

## helpers.R

The helper file defines four key data objects that are referenced throughout the shiny app. Each object is a vector of variable names, and the vector itself is named with corresponding variable labels.

- `vars_lfbg` is a vector of all the "listening frequency by genre" variable names. These variables are ordinal categories: Never (1) \< Rarely (2) \< Sometimes (3) \< Very frequently (4).

- `vars_mh` is vector of all the "self-reported mental health" variable names. These variables are numeric with range 0-10 (0 = "I do not experience this", 10 = "I experience this regularly, constantly/or to an extreme").

- `vars_oth_n` is a vector of all other numeric variables on the dataset.

- `vars_oth_c` is a vector of all other categorical variables on the dataset.

## Static_EDA.qmd

This program performs basic data cleaning and exploratory data analysis for the MxMH survey. The tables and plots developed in this file appear in the shiny app.

**Numeric Summaries**

- Contingency Tables for categorical variables (one-way and two-way)
- Table of summary statistics for numeric variables (mean, standard deviation, min, median, max)

**Graphical Summaries**

- Density Plots for numeric variables (univariate and multivariate \[facet\])
- Boxplots for numeric variables (univariate and multivariate \[side-by-side\])
- Bar charts for categorical variables (univariate and multivariate \[side-by-side\])
- Heatmap for a pair of categorical variables (multivariate only)

## app.R

This file contains complete code for the shiny app. A deployed version of the app is available at <https://kayla-nowak-ncsu.shinyapps.io/st558_project2/>

The app supports data subsets, direct data download, and interactive versions of the exploratory data analyses developed in the Static_EDA program.

Please see the app's landing page ("About" tab) for additional information.
