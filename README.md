# Actuarial Risk Analysis and Decision Support System (GLM)

This project is a quantitative risk modeling and decision support system developed to predict insurance claim probabilities. It utilizes Generalized Linear Models (GLM) with a Binomial family (Logistic Regression) to analyze portfolio risk based on real-world insurance data.

The application provides an interactive dashboard for underwriters and risk managers to assess individual customer risk profiles in real-time.

## Project Objective
To develop a statistically valid model for predicting the Probability of Default (Claim Frequency) and to deploy this model as a user-friendly web application using R Shiny. The system aims to bridge the gap between complex statistical modeling and operational decision-making.

## Methodology and Technical Stack

* **Language:** R
* **Interface:** Shiny, Bslib (Cyborg Theme)
* **Modeling:** Generalized Linear Models (GLM - Logistic Regression)
* **Data Manipulation:** Tidyverse (Dplyr)
* **Statistical Analysis:** Caret, pROC, Pscl

## Model Performance and Diagnostics
The model's validity and discriminatory power have been rigorously tested using standard actuarial metrics:

* **AUC Score:** 0.828 (Indicates excellent discriminatory power between claim and non-claim segments).
* **McFadden R-Squared:** 0.22 (Indicates a good model fit for logistic regression standards).
* **VIF (Variance Inflation Factor):** < 2.0 for all variables (No multicollinearity issues detected).

## Key Risk Drivers
Statistical analysis identifies the following variables as the most significant predictors of risk:
1.  **Driver Age:** The 16-25 age group exhibits a significantly higher claim probability compared to older demographics.
2.  **Credit Score:** A strong negative correlation exists between financial stability (credit score) and claim frequency.
3.  **Vehicle Type:** Sports vehicles demonstrate a higher risk exposure compared to sedans.

## Repository Structure
* **app.r:** The main source code containing data processing, model training, and the Shiny application logic.
* **Car_Insurance_Claim.csv:** The dataset used for training and testing the model.

## How to Run
To run this application locally in RStudio:
1.  Clone this repository.
2.  Ensure the required packages are installed: `shiny`, `tidyverse`, `bslib`, `DT`, `caret`, `pROC`, `plotly`, `pscl`.
3.  Open `app.r` and click "Run App".
