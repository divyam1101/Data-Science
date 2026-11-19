# Data Science Case Studies (R)

A portfolio of end-to-end analytics and machine-learning case studies in R, covering regression, classification, time-series forecasting and exploratory data analysis on business problems.

| Project | Business question | Techniques | Folder |
|---|---|---|---|
| **HR Analytics: Employee Attrition** | A company has ~15% annual attrition. Which factors drive it, and which employees are likely to leave? | Data prep & EDA, logistic regression (`glm`), stepwise feature selection (`stepAIC`), VIF, ROC / cutoff tuning (`ROCR`, `caret`) | [`Data-Science/HR Analytics`](Data-Science/HR%20Analytics) |
| **Car Price Prediction** | Which variables significantly affect car prices in a new market? | Multiple linear regression, `stepAIC`, multicollinearity checks, model evaluation | [`Data-Science/Car Price Prediction`](Data-Science/Car%20Price%20Prediction) |
| **Handwritten Digit Recognition** | Classify handwritten digits (MNIST) from pixel values | Support Vector Machines (`kernlab::ksvm`), linear vs RBF kernels, `caret` cross-validation | [`Data-Science/Digit Recognition`](Data-Science/Digit%20Recognition) |
| **Credit Risk Analysis (Gramener / Lending Club)** | Which applicant attributes predict loan default? | Data cleaning, univariate / bivariate / segmented EDA, `dplyr`, `sqldf`, `ggplot2`; report in PDF | [`Data-Science/Gramener Case Study`](Data-Science/Gramener%20Case%20Study) |
| **Investment Strategy** | Where should an asset-management firm invest: funding type, countries, sectors? | Data cleaning & merging, aggregation with `sqldf` / `dplyr`, charts; deliverables in Excel and PDF | [`Data-Science/Investment Case Study`](Data-Science/Investment%20Case%20Study) |
| **Uber Supply–Demand Gap** | When and where do cab requests go unfulfilled between the city and the airport? | Time-slot analysis, `ggplot2` visualisation, presentation of recommendations | [`Data-Science/Uber Demand Supply gap`](Data-Science/Uber%20Demand%20Supply%20gap) |
| **Bitcoin Price Forecasting** | Forecast Bitcoin prices from historical data | Time series, `forecast::auto.arima` | [`Bitcoin Price Forecasting.R`](Bitcoin%20Price%20Forecasting.R) |

## Tech stack

R · tidyverse (`dplyr`, `tidyr`, `ggplot2`) · `caret` · `MASS` · `car` · `kernlab` · `e1071` · `ROCR` · `forecast` · `sqldf`

## Running a project

Open the `.R` script in RStudio, put the dataset named at the top of the script in the working directory, install the listed packages and run the script top to bottom.
