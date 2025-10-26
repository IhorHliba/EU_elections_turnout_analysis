# 🇪🇺 EU Elections Turnout Analysis (R)

This repository contains an **R-based statistical analysis** exploring factors influencing voter turnout in the **2019 European Parliament elections** across EU member states.  
The project applies exploratory data analysis (EDA), multiple linear regression, and data visualization techniques to uncover relationships between **socioeconomic variables** and **electoral participation**.

---

## 📘 Overview

The project focuses on:
- Understanding how socioeconomic variables (e.g. unemployment, population size, years in EU membership) correlate with electoral participation.  
- Building multiple linear regression models to test these relationships.  
- Visualizing the strength and direction of associations using `ggplot2` and `plotly`.  

---

## 🧩 Methodology

1. **Data Preparation:**  
   - Original data collected from EU election datasets.  
   - Created log-transformed variables (`log10`, `log`) to normalize skewed distributions.

2. **Exploratory Analysis:**  
   - Histograms, scatter plots, and pairwise relationships between variables.  
   - Visual inspection of variable distributions and outliers.

3. **Regression Models:**  
   - 10+ models testing voter turnout against unemployment, population, seats, and years of EU membership.  
   - Used both simple and multiple regression approaches to evaluate interactions.

4. **Diagnostics:**  
   - Examined residuals, Cook’s distance, and standardized errors to assess model fit and detect influential data points.  
   - Residual plots and color-coded residual magnitudes for visual interpretation.

---

## 📈 Visualizations

| Figure | Description |
|:-------|:-------------|
| ![Figure 1](figures/Rplot01.png) | **Figure 1:** Relationship between EU Membership Duration and Voter Turnout *(positive correlation)* |
| ![Figure 2](figures/Rplot02.png) | **Figure 2:** Residual Plot for Regression Model *(YEARS_LOG)* |
| ![Figure 3](figures/Rplot03.png) | **Figure 3:** Predicted vs Actual Turnout with Residual Distances *(model fit visualization)* |
| ![Figure 4](figures/Rplot04.png) | **Figure 4:** Number of Seats vs Voter Turnout *(weak correlation)* |

---

## 🧰 Tools and Libraries

- `tidyverse` – data manipulation and visualization  
- `plotly` – interactive visualizations  
- `ggplot2` – statistical plotting  
- `readxl` – Excel data import  
- `pathviewr` – regression residual analysis  

---
## 👨‍💻 Authors scripts
**Ihor Hliba** and **Kristýna Ševčíková**  
[LinkedIn](https://www.linkedin.com/in/ihorhliba)  

---


## ▶️ How to Run

1. Clone this repository:
   ```bash
   git clone https://github.com/IhorHliba/EU_elections_turnout_analysis.git
