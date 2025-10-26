# ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
# ???????? EU Elections Turnout Analysis (R)
# Description: 
#   Exploratory data analysis and regression modeling of voter turnout 
#   in EU Parliament elections. Includes data transformation, 
#   visualization, multiple regression models, and diagnostic checks.
# ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦

# ¦¦ 1. Environment setup ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
rm(list = ls())

# Load required packages
libs <- c(
  "readxl", "dplyr", "tidyr", "janitor", "ggplot2", "plotly",
  "purrr", "broom", "stringr", "car", "lmtest", "sandwich"
)
invisible(lapply(libs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}))
lapply(libs, library, character.only = TRUE)

# ¦¦ 2. Load and clean data ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
# Make sure your Excel file name contains no diacritics.
# Example: data/EU_EP_2019_data.xlsx
df_raw <- readxl::read_excel("data/EU_EP_2019_data.xlsx", sheet = 1) %>%
  janitor::clean_names() # converts variable names to snake_case

# ¦¦ 3. Helper function: safe logarithm ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
safe_log <- function(x, base = exp(1)) {
  # Handles zero or negative values safely
  min_pos <- min(x[x > 0], na.rm = TRUE)
  shift <- ifelse(is.finite(min_pos), min_pos / 2, 1)
  val <- ifelse(x > 0, x, x + shift)
  if (base == 10) log10(val) else log(val)
}

# ¦¦ 4. Feature engineering ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
df <- df_raw %>%
  mutate(
    compulsory_attendance_log = safe_log(compulsory_attendance),
    population_log            = safe_log(population),
    seats_log                 = safe_log(number_of_seats),
    years_log                 = safe_log(number_of_years_in_eu),
    unemployment_log          = safe_log(unemployment)
  )

# ¦¦ 5. Exploratory data analysis (histograms) ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
eda_vars <- c(
  "turnout", "unemployment", "age", "compulsory_attendance",
  "population", "number_of_seats", "number_of_years_in_eu"
)

plot_hist <- function(var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 20, color = "white", fill = "steelblue") +
    labs(title = paste("Distribution of", var))
}
# Example: plot_hist("turnout")

# ¦¦ 6. Scatter plots with linear regression line ¦¦¦¦¦¦¦¦¦¦¦¦¦¦
scatter_lm <- function(x) {
  ggplot(df, aes(x = .data[[x]], y = turnout, color = factor(country))) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(x = x, y = "Turnout") +
    theme_minimal()
}
# Example: scatter_lm("years_log")

# ¦¦ 7. Model specification grid ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
# Define multiple regression models for comparison
specs <- list(
  m1  = turnout ~ log10(number_of_seats) + compulsory_attendance_log,
  m2  = turnout ~ log10(unemployment) + seats_log,
  m3  = turnout ~ log10(unemployment) + years_log,
  m4  = turnout ~ log10(unemployment) + compulsory_attendance,
  m5  = turnout ~ log10(number_of_seats) + years_log,
  m6  = turnout ~ log10(population) + years_log,
  m7  = turnout ~ log10(population) + seats_log,
  m8  = turnout ~ log10(population) + unemployment_log,
  m9  = turnout ~ log10(number_of_seats) + compulsory_attendance,
  m10 = turnout ~ log10(number_of_years_in_eu) + compulsory_attendance,
  m11 = turnout ~ log10(unemployment) + compulsory_attendance + seats_log + years_log,
  m12 = turnout ~ log10(population) + compulsory_attendance + seats_log + years_log + age,
  m13 = turnout ~ log10(population) + unemployment_log + age,
  m14 = turnout ~ log10(population) + seats_log + years_log
)

# Fit all models at once
fits <- imap(specs, ~ lm(.x, data = df))

# ¦¦ 8. Model comparison table ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
model_tbl <- imap_dfr(fits, ~ broom::glance(.x) %>%
                        mutate(model = .y)) %>%
  select(model, r.squared, adj.r.squared, AIC, BIC, sigma, statistic, p.value, df, nobs) %>%
  arrange(AIC)

# View(model_tbl)

# ¦¦ 9. Robust standard errors and coefficient table ¦¦¦¦¦¦¦¦¦¦¦
robust_coefs <- imap(fits, ~ {
  vc  <- sandwich::vcovHC(.x, type = "HC1")
  ct  <- lmtest::coeftest(.x, vcov. = vc)
  tibble::rownames_to_column(as.data.frame(ct), "term") %>%
    as_tibble() %>%
    rename(
      estimate = Estimate, std.error = `Std. Error`,
      statistic = `t value`, p.value = `Pr(>|t|)`
    ) %>%
    mutate(model = .y)
}) %>% bind_rows()
# View(robust_coefs)

# ¦¦ 10. Diagnostics for the best model ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
best_id  <- model_tbl$model[1]
best_fit <- fits[[best_id]]

# Multicollinearity check
car::vif(best_fit)

# Heteroskedasticity test
lmtest::bptest(best_fit)

# Influence diagnostics
infl <- influence.measures(best_fit)
which(infl$is.inf)

# Cook's distance
cooks <- cooks.distance(best_fit)
which(cooks > 4 / nobs(best_fit))

# ¦¦ 11. Residual comparison (full vs reduced model) ¦¦¦¦¦¦¦¦¦¦¦
fit_full    <- best_fit
fit_reduced <- lm(turnout ~ years_log, data = df)

df_resid <- df %>%
  mutate(
    res_full    = rstandard(fit_full),
    res_reduced = rstandard(fit_reduced),
    pathway     = abs(res_reduced) - abs(res_full)
  ) %>%
  select(country, years_log, turnout, res_full, res_reduced, pathway) %>%
  arrange(desc(pathway))

# ¦¦ 12. Residual visualization ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
d <- df %>%
  mutate(
    predicted = predict(fit_reduced),
    residuals = residuals(fit_reduced)
  )

ggplot(d, aes(x = years_log, y = turnout)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey60") +
  geom_segment(aes(xend = years_log, yend = predicted), alpha = 0.2) +
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +
  scale_color_continuous(low = "green", high = "red", name = "|Residual|") +
  guides(size = "none") +
  theme_bw() +
  labs(title = "Regression fit and residuals", x = "Years in EU (log)", y = "Turnout")

# ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦
# End of Script
# ¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦¦