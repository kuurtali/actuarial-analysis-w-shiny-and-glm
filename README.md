# 🛡️ Actuarial Risk Analysis & Decision Support System (GLM + Shiny)

**An interactive insurance risk scoring platform** built with R Shiny and Generalized Linear Models (Logistic Regression). The system predicts claim probability in real-time and provides a comprehensive dashboard for underwriters and risk managers.

> *How likely is a given customer to file an insurance claim — and which risk factors drive that probability?*

---

## 📌 Project Objective

To develop a statistically valid model for predicting **Probability of Default (Claim Frequency)** and to deploy this model as a user-friendly web application using R Shiny. The system bridges the gap between complex statistical modeling and operational decision-making.

---

## 📊 Model Performance & Diagnostics

The model's validity and discriminatory power have been rigorously tested using standard actuarial metrics:

| Metric | Value | Interpretation |
|--------|-------|----------------|
| **AUC Score** | 0.828 | Excellent discriminatory power between claim / non-claim segments |
| **McFadden R²** | 0.22 | Good model fit for logistic regression standards |
| **VIF** | < 2.0 (all vars) | No multicollinearity detected |

> An AUC of 0.828 means the model correctly ranks a random "claim" customer above a random "non-claim" customer **82.8% of the time**.

---

## 🔑 Key Risk Drivers

Statistical analysis identifies the following variables as the most significant predictors:

| Rank | Variable | Effect | Interpretation |
|------|----------|--------|----------------|
| 1 | **Driver Age** | 16–25 group → highest risk | Young drivers exhibit significantly higher claim probability |
| 2 | **Credit Score** | Strong negative correlation | Higher financial stability → fewer claims |
| 3 | **Vehicle Type** | Sports > Sedan | Sports vehicles demonstrate higher risk exposure |
| 4 | **Driving Experience** | 0–9y → highest risk | Less experienced drivers file more claims |
| 5 | **Vehicle Year** | Before 2015 → higher risk | Older vehicles correlate with increased claims |

---

## 🖥️ Application Tabs

The Shiny dashboard contains **5 interactive tabs**:

### 1. Risk Dashboard
- Real-time **risk score gauge** (0–100%) with color-coded severity
- **Portfolio positioning** histogram — shows where the customer falls relative to the entire portfolio
- Executive summary panel with model description

### 2. Veri Keşfi (EDA)
- Interactive **variable explorer** — select any predictor to see its relationship with claim outcome
- Stacked bar charts showing **claim proportions** by category
- Powered by Plotly for zoom, hover, and export

### 3. İstatistiksel Kanıt
- **ROC Curve** with AUC annotation — visual proof of model discriminatory power
- **Variable Importance** chart — which predictors carry the most weight in the model

### 4. Ham Veri
- Searchable, paginated **data table** (DT) showing the cleaned dataset
- Supports sorting, filtering, and export

### 5. Model Detayları
- Full **GLM regression summary** (coefficients, standard errors, p-values)
- **Odds Ratios** (exponentiated coefficients) for business interpretation

---

## 🛠️ Technical Stack

| Component | Technology |
|-----------|-----------|
| Language | R |
| Web Framework | Shiny |
| UI Theme | Bslib (Cyborg — dark theme) |
| Modeling | GLM — Binomial family (Logistic Regression) |
| Data Manipulation | Tidyverse (dplyr) |
| Visualization | Plotly, ggplot2 |
| Model Validation | caret, pROC |
| Pseudo R² | pscl (McFadden) |
| Data Tables | DT |

---

## 📊 Methodology

### Data
- **Source:** `Car_Insurance_Claim.csv` — real-world insurance portfolio data
- **Records:** ~10,000 policies after cleaning
- **Target Variable:** `OUTCOME` (0 = No Claim, 1 = Claim)

### Data Cleaning
- Imputation: Missing `CREDIT_SCORE` and `ANNUAL_MILEAGE` filled with column means
- Logical filtering: Impossible age–experience combinations removed (e.g., age 16–25 with 30y+ experience)
- All character columns converted to factors

### Model Specification
```r
OUTCOME ~ AGE + GENDER + DRIVING_EXPERIENCE + VEHICLE_TYPE + VEHICLE_YEAR + CREDIT_SCORE
family = binomial(link = "logit")
```

### Smart Sidebar Logic
The sidebar dynamically restricts **Driving Experience** options based on the selected **Age Group** to prevent impossible combinations:
- Age 16–25 → only 0–9y
- Age 26–39 → 0–9y, 10–19y
- Age 40–64 → 0–9y, 10–19y, 20–29y
- Age 65+ → all options

---

## 📁 Project Structure

```
actuarial-analysis-w-shiny-and-glm/
├── app.r                      # Main application (data processing + model + Shiny UI/Server)
├── Car_Insurance_Claim.csv    # Insurance portfolio dataset (~10K records)
├── .gitignore                 # R-specific ignore rules
├── .gitattributes             # Line ending normalization
├── LICENSE                    # MIT License
└── README.md                 # This file
```

---

## 🚀 How to Run

### Prerequisites
- **R 4.x** with the following packages:
  ```r
  install.packages(c("tidyverse", "shiny", "bslib", "DT", "caret", "pROC", "plotly", "pscl"))
  ```

### Steps
1. Clone this repository
2. Open `app.r` in RStudio
3. Click **"Run App"** (or run from terminal: `Rscript -e "shiny::runApp('app.r')"`)
4. The dashboard will open in your browser at `http://127.0.0.1:xxxx`

> **Note:** The app auto-installs missing packages on first run.

---

## 🔗 Related Projects

- **[ADAS Pricing Paradox (Vol 1)](https://github.com/kuurtali/ADAS-Pricing-Paradox)** — End-to-end actuarial pricing analysis investigating the frequency–severity trade-off in ADAS-equipped vehicles (100K policies, Poisson + Gamma GLM).
- **[VOL2 — ADAS Pricing Paradox (Advanced Edition)](https://github.com/kuurtali/VOL2-ADAS-Pricing-Paradox)** — Extended analysis with 200K policies, GLM interaction terms, Gini Index, and Lift Charts.

---

## 📜 License

MIT License — see [LICENSE](LICENSE) for details.
