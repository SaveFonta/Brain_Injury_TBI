############################################################################
### Quick & Temperature Models with Robust Regression                    ###
### (Final Code for Client)                                              ###
############################################################################

# Main Author: Valentin Johner

# Load necessary libraries
# Note: MASS should be loaded before tidyverse to avoid masking select()
library(data.table)
library(ggplot2)
library(MASS)
library(tidyverse)
library(car)
library(sfsmisc) # For p.res.2x, if needed for specific diagnostics (commented out in final version)
library(robustbase) # For lmrob()
library(lemon) # For coord_capped_cart in coefficient plots

# Set root directory for knitr to ensure correct file paths
knitr::opts_knit$set(root.dir = normalizePath(".."))

# Define path to cleaned data
clean_path <- "02_data/02_clean_data/"

## 1. Data Loading and Initial Preparation

# Load the pre-processed polytrauma population data
# This dataset already contains age_gen, gcs_cat, and iss_cat as ordered factors,
# and specific brain injury indicators as factors, as prepared in data_prep.R.
poly <- readRDS(paste0(clean_path, "population_poly.rds"))

# Select only the variables relevant for the Quick and Temperature models
# This ensures a focused dataset for the analysis.
poly <- poly %>%
  select(
    research_case_id, bp, hr, temperature, inr, quick, iss, iss_cat,
    invasive, gcs, gcs_cat, sex, age, age_cat, age_gen, bleeding, fracture,
    concussion, brain_edema, brain_compression, unconsciousness,
    Thorax, severe_thoracic_injury
  )

# Handle initial data quality issues for vital signs:
# Remove cases with missing Blood Pressure (bp) or Heart Rate (hr) as these
# are expected to be consistently measured at admission.
poly <- poly %>% drop_na(bp, hr)

# Remove extreme outlier for Heart Rate (HR > 250 bpm is physiologically unlikely)
poly <- poly %>% filter(hr < 250)

## 2. Handling Missingness and Variable Transformations

# For Quick, missing values were primarily due to tests
# not being clinically indicated (informative missingness).
# For Temperature, probably not informative missingness (however, still checked with imputation).
# After testing imputation strategies, it was found that results were robust
# to simply removing cases with missing values for these specific outcomes.
quick_nona <- poly %>% drop_na(quick)
temp_nona <- poly %>% drop_na(temperature)

# Quick variable transformation:
# Initial exploration showed that INR had a very skewed distribution.
# While Quick (inversely related to INR) was better, squaring Quick (Quick^2)
# further improved its distribution and model diagnostics.
# This transformation helps meet the assumptions of linear regression.
# (See "Exploratory Analysis of INR and Quick" in Section 6.3 for details on why Quick^2 was chosen)

## 3. Final Model Building

# Define a helper function for plotting coefficients with nice names
# This function is used to generate the coefficient plots included in the report.
coefficient_plot <- function(model, outcome_name, plot_color = "darkblue", alpha =.05, x_breaks,
                             coef_names = data.frame(
                               variable = c("gcs_cat.L", "gcs_cat.Q", "gcs_cat.C", "concussion1",
                                            "bleeding1", "fracture1", "brain_edema1",
                                            "brain_compression1", "unconsciousness1", "invasive1",
                                            "iss_cat.L", "iss_cat.Q", "iss_cat.C",
                                            "severe_thoracic_injury1",
                                            "age_gen.L", "age_gen.Q",
                                            "sexm"),
                               variable_nice = c("GCS category, linear", "GCS category, quadratic",
                                                 "GCS category, cubic", "Concussion", "Intracranial bleeding",
                                                 "Skull fracture", "Brain Edema", "Brain Compression",
                                                 "Loss of consciousness", "Invasive procedure",
                                                 "ISS category, linear", "ISS category, quadratic",
                                                 "ISS category, cubic",
                                                 "Severe thoracic injury",
                                                 "Age category, linear", "Age category, quadratic",
                                                 "Male sex"))) {
  # make nice names for interaction terms
  interaction_names <- names(model$coefficients)
  interaction_names <- interaction_names[grepl(pattern = ":", x = interaction_names)]
  coef_names <- rbind(coef_names,
                      data.frame(variable = interaction_names,
                                 variable_nice = interaction_names |>
                                   str_replace_all(c(gcs_cat = "GCS category",
                                                     iss_cat = "ISS category",
                                                     age_gen = "Age category",
                                                     `\\.L` = ", linear",
                                                     `\\.Q` = ", quadratic",
                                                     `\\.C` = ", cubic",
                                                     `:` = " x "))))
  
  model_coefs <- summary(model)$coefficients |>
    data.frame(check.names = FALSE) |>
    rownames_to_column(var = "variable") |>
    filter(variable!= "(Intercept)") |>
    left_join(coef_names, by = "variable") |>
    mutate(cil = Estimate - qt(p = alpha / 2, df = model$df.residual) * `Std. Error`,
           ciu = Estimate + qt(p = alpha / 2, df = model$df.residual) * `Std. Error`,
           variable_nice = factor(variable_nice, levels = coef_names$variable_nice))
  
  model_coefs |>
    ggplot(aes(x = Estimate, y = fct_rev(variable_nice))) +
    geom_vline(xintercept = 0, color = "black") +
    geom_point(color = plot_color) +
    geom_errorbarh(aes(xmin = cil, xmax = ciu), height = 0,
                   color = plot_color) +
    theme_classic() +
    scale_x_continuous(breaks = x_breaks, limits = range(x_breaks)) +
    labs(x = "Coefficient Estimate (95% Confidence Interval)",
         y = NULL, title = paste0("Model Coefficients: ", outcome_name)) +
    coord_capped_cart(bottom = "both") +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(color = "black"),
          panel.grid.major.x = element_line(color = "gray"))
}


### 3.1 Quick Model

# Final model for Quick (squared-transformed)
# This model includes an interaction between age_gen and iss_cat,
# which was found to be statistically significant after ANOVA comparison.
# Classical linear model (lm) was chosen over robust regression (lmrob)
# as coefficient estimates were stable, indicating outliers did not
# disproportionately influence the results for Quick.
fm_quick_final <- lm(quick^2 ~ gcs_cat + age_gen * iss_cat + severe_thoracic_injury + sex +
                       bleeding + fracture + concussion + brain_edema + brain_compression +
                       unconsciousness, data = quick_nona)

# Summary of the final Quick model (for Appendix)
summary(fm_quick_final)

# Diagnostic plots for the final Quick model
# These plots help assess model assumptions (linearity, normality of residuals, homoscedasticity).
par(mfrow = c(2, 2))
plot(fm_quick_final)
par(mfrow = c(1, 1))

# Coefficient plot for the final Quick model (for Report Section 5.3)
coefficient_plot(model = fm_quick_final, outcome_name = "Quick^2", x_breaks = c(-3500, 3500))


### 3.2 Temperature Model

# Final model for Temperature (no transformation)
# This model includes interactions between gcs_cat and iss_cat, and gcs_cat and age_gen,
# which were found to be statistically significant after ANOVA comparison.
# Robust regression (lmrob) was chosen for Temperature due to the presence of
# influential outliers that significantly affected classical linear model (lm) results.
fm_temp_final <- lmrob(temperature ~ gcs_cat * iss_cat + gcs_cat:age_gen + age_gen + severe_thoracic_injury + sex +
                         bleeding + fracture + concussion + brain_edema + brain_compression +
                         unconsciousness, data = temp_nona, fast.s.large.n = Inf, setting = "KS2014")

# Summary of the final Temperature model (for Appendix)
summary(fm_temp_final)

# Diagnostic plots for the final Temperature model
# These plots help assess model assumptions and identify influential observations.
par(mfrow = c(2, 3)) # lmrob plot function uses 2x3 layout
plot(fm_temp_final)
par(mfrow = c(1, 1))

# Plot of residuals vs. robustness weights for the Temperature model
# This visually demonstrates how lmrob down-weights influential outliers.
df_temp_weights <- data.frame(
  Residuals = residuals(fm_temp_final),
  Weights = weights(fm_temp_final, type = "robustness")
)
ggplot(df_temp_weights, aes(x = Weights, y = Residuals)) +
  geom_point(aes(color = Weights), size = 2, alpha = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_color_viridis_c(option = "plasma", end = 0.85) +
  labs(
    title = "Temperature: Residuals vs Robustness Weights",
    x = "Robustness Weights",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Coefficient plot for the final Temperature model (for Report Section 5.4)
coefficient_plot(model = fm_temp_final, outcome_name = "Temperature", x_breaks = c(-0.65, 0.65))


## 4. Exploratory Plots: Relationship between Quick/Temperature and GCS

# Boxplot of Quick by GCS Category (original scale)
# Provides a visual overview of the relationship before transformation.
ggplot(quick_nona, aes(x = gcs_cat, y = quick)) +
  geom_boxplot() +
  labs(title = "Quick by GCS Category (Original Scale)", x = "GCS Category", y = "Quick") +
  theme_minimal()

# Boxplot of Temperature by GCS Category
# Provides a visual overview of the relationship.
ggplot(temp_nona, aes(x = gcs_cat, y = temperature)) +
  geom_boxplot() +
  labs(title = "Temperature by GCS Category", x = "GCS Category", y = "Temperature (°C)") +
  theme_minimal()


## 5. Predictive Performance Check (Random Forest)

# Random Forest model for Quick to check predictive power (R^2)
# This was done to compare against the linear model's R^2 and confirm
# that linear assumptions are reasonable given the data's inherent variability.
rf_quick <- randomForest(quick ~ gcs_cat + age_gen * iss_cat + severe_thoracic_injury + sex +
                           bleeding + fracture + concussion + brain_edema + brain_compression +
                           unconsciousness, data = quick_nona, importance = TRUE)
print(rf_quick)
# Calculate in-sample R^2 for Random Forest (for comparison)
rf_quick_pred <- unname(predict(rf_quick, quick_nona))
R2_quick_rf <- 1 - (sum((quick_nona$quick - rf_quick_pred)^2) / sum((quick_nona$quick - mean(quick_nona$quick))^2))
message(paste("Random Forest In-sample R^2 for Quick:", round(R2_quick_rf, 3)))


# Random Forest model for Temperature to check predictive power (R^2)
rf_temp <- randomForest(temperature ~ gcs_cat * iss_cat + gcs_cat:age_gen + age_gen + severe_thoracic_injury + sex +
                          bleeding + fracture + concussion + brain_edema + brain_compression +
                          unconsciousness, data = temp_nona, importance = TRUE)
print(rf_temp)
# Calculate in-sample R^2 for Random Forest (for comparison)
rf_temp_pred <- unname(predict(rf_temp, temp_nona))
R2_temp_rf <- 1 - (sum((temp_nona$temperature - rf_temp_pred)^2) / sum((temp_nona$temperature - mean(temp_nona$temperature))^2))
message(paste("Random Forest In-sample R^2 for Temperature:", round(R2_temp_rf, 3)))


## 6. Summary of Exploratory Steps and Decisions (for Documentation)

# This section summarizes key decisions and explorations that led to the final models.
# It's intended for documentation and to provide context on why certain paths were taken or discarded.

# 6.1 INR vs. Quick Decision:
# - Initial consideration of INR as a coagulopathy marker.
# - INR showed erratic distributions even after log transformation (visualized with boxplots/histograms).
# - Quick, while not perfectly inverse to INR by formula, showed a much better distribution.
# - Decision: Proceed with Quick, and specifically Quick^2, due to better statistical behavior and interpretability.

# 6.2 Missing Data Handling (Quick & Temperature):
# - Identified "informative missingness" for Quick and Temperature (tests only done when clinically indicated).
# - Tested imputation strategies (e.g., replacing NAs with "normal" values like Quick=90, Temp=37).
# - Conclusion: Model coefficients and significance were robust (did not change meaningfully) whether NAs were imputed or simply removed.
# - Decision: For simplicity and to avoid making assumptions about missing data mechanisms, cases with missing Quick/Temperature were removed from their respective analyses.

# 6.3 Quick Variable Transformation (Quick^2):
# - Initial linear model for Quick showed issues with residual diagnostics (e.g., non-normality, heteroscedasticity).
# - Explored Box-Cox transformations, which suggested a power transformation close to 2 (squaring).
# - Decision: Use Quick^2 as the outcome, as it significantly improved model diagnostics and linearity assumptions.

# 6.4 Robust Regression Application (Quick vs. Temperature):
# - For Quick: Compared lm() and lmrob(). Coefficients remained stable, indicating that outliers did not disproportionately influence the results.
# - Decision for Quick: Use the classical lm() model for simplicity, as robust regression did not yield significantly different interpretations.
# - For Temperature: Compared lm() and lmrob(). Significant differences in coefficients and identification of influential outliers were observed.
# - Decision for Temperature: Use lmrob() to ensure reliable parameter estimates by down-weighting these influential observations.

# 6.5 Interaction Terms:
# - For Quick: Tested interactions between gcs_cat and age_gen, gcs_cat and iss_cat, and age_gen and iss_cat using ANOVA.
# - Decision for Quick: Included the age_gen * iss_cat interaction as it was statistically significant.
# - For Temperature: Tested interactions between gcs_cat and age_gen, gcs_cat and iss_cat, and age_gen and iss_cat using ANOVA.
# - Decision for Temperature: Included both gcs_cat * iss_cat and gcs_cat:age_gen interactions as they were statistically significant.

# 6.6 Categorical Variable Levels (Age, GCS, ISS):
# - Age was categorized into 3 ordered levels (<40, 40-69, 70+) for better clinical interpretability and to handle potential non-linear effects or truncation in continuous age.
# - GCS and ISS were already provided as ordered factors with their respective categories.
# - Explored binary GCS models (e.g., 0/1 vs 2/3) but found no significant improvement in diagnostics or interpretability over the ordered factor, so reverted to the ordered factor for GCS.

# 6.7 Predictive Power (R^2):
# - Noted that R^2 values for linear models are generally low (e.g., 10.3% for Quick, 8.4% for Temperature).
# - Performed Random Forest models as a check for higher predictive power. While Random Forest showed slightly higher in-sample R^2, the linear models were deemed sufficient for identifying and quantifying relationships, given the complexity of biological systems and the goal of interpretability for clinical insights.
