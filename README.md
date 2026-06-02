# 🏥 Medical Charges Prediction App (Shiny + Machine Learning)

## 🧠 Overview

This project presents an interactive **Shiny web application** that estimates individual annual and monthly medical insurance charges using a **log-linear regression model** trained on a structured healthcare dataset.

The system transforms patient-level inputs into real-time cost predictions, demonstrating how classical statistical learning models can be deployed as lightweight, interpretable decision-support tools in healthcare analytics.

The application bridges **statistical modeling, clinical informatics, and interactive data visualization** in a reproducible R-based environment.

---

## 🎯 Objective

To develop a transparent and interpretable predictive system that estimates medical insurance costs based on patient demographics and lifestyle factors, including:

- Age
- Sex
- BMI
- Smoking status
- Number of dependents
- Geographic region

The goal is to demonstrate how **statistical learning models can be operationalized into real-world clinical decision tools**.

---

## 🧬 Modeling Approach

A **log-linear multiple regression model** was trained to predict medical expenses:

- Response variable: `log(Charges)`
- Predictors:
  - Age
  - Sex
  - BMI
  - Children
  - Smoker status
  - Region

### Model Formulation

The model estimates:

\[
\log(\text{Charges}) = \beta_0 + \beta_1(\text{Age}) + \beta_2(\text{Sex}) + \beta_3(\text{BMI}) + \beta_4(\text{Children}) + \beta_5(\text{Smoker}) + \beta_6(\text{Region})
\]

Final predictions are transformed back to the original scale using exponential back-transformation.

---

## ⚙️ System Workflow

<p align="center">
  <img src="Pic 1.png" width="90%">
</p>

### 🔄 Pipeline Description

1. User inputs patient attributes via Shiny UI  
2. Input validation and factor alignment  
3. Feature encoding consistent with training data  
4. Prediction using trained regression model  
5. Log-scale cost estimation  
6. Exponential transformation to monetary values  
7. Output of annual and monthly estimates  

---

## 🧠 Prediction Output Example

<p align="center">
  <img src="Pic 2.png" width="70%">
</p>

### Interpretation

The model generates:

- **Annual medical cost estimate**
- **Derived monthly premium estimate**

These outputs provide an interpretable financial risk approximation based on patient-level characteristics.

---

## 📦 Input Variables

| Variable | Description |
|----------|------------|
| Age | Patient age (years) |
| Sex | Female / Male |
| BMI | Body Mass Index |
| Smoker | Smoking status (Yes/No) |
| Children | Number of dependents |
| Region | Geographic classification |

---

## 🧠 Key Features of the System

- Real-time prediction via Shiny reactive framework  
- Log-linear modeling for variance stabilization  
- Factor-level consistency between training and inference  
- Interactive user interface with structured inputs  
- Back-transformed cost estimates in interpretable currency scale  

---

## 🧬 Scientific Interpretation

The model captures clinically meaningful cost drivers:

- Smoking status is typically the strongest cost predictor  
- BMI reflects chronic health risk burden  
- Age contributes to baseline healthcare utilization  
- Regional variation captures systemic cost differences  

This aligns with established findings in **health economics and actuarial science**.

---

## ⚠️ Limitations

- Linear model assumption may underfit complex nonlinear interactions  
- Dataset does not include clinical comorbidities  
- Predictions are population-based estimates, not individual diagnoses  
- No temporal dynamics or longitudinal tracking  

---

## 🚀 Future Improvements

- Replace linear model with Gradient Boosting / XGBoost  
- Add interaction terms (BMI × Smoker, Age × BMI)  
- Incorporate uncertainty intervals for predictions  
- Deploy as cloud-based clinical decision tool  
- Extend to hospitalization risk modeling  

---

## 🧠 Research Significance

This project demonstrates how **classical statistical learning models can be operationalized into real-time predictive healthcare tools**, emphasizing:

- Interpretability over black-box complexity  
- Reproducible clinical analytics  
- Deployment readiness using R Shiny  
- Translational data science in healthcare systems  

---

## 👤 Author

**Daniel Oluwafemi Olofin**  
Computational Biostatistics • Machine Learning in Healthcare • Clinical Data Science  

- GitHub: https://github.com/Olofin98  
- Portfolio: https://olofin98.github.io/Daniel.github.io  

> “The value of predictive modeling in healthcare is not only accuracy, but interpretability and clinical usability.”
