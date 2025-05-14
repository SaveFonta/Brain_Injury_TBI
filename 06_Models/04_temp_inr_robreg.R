############################################################################
### Quick & Temp Models w/ Robust Regression (rec from R-God personally) ###
############################################################################

rm(list = ls())

library(data.table)
library(ggplot2)
library(MASS) # load before tidyverse, otherwise select() gets masked
library(tidyverse)
library(car) 
library(sfsmisc)

knitr::opts_knit$set(root.dir = normalizePath(".."))

clean_path <- "02_data/02_clean_data/"


## ---- Load data & copy paste stuff from last script ----

poly <- readRDS(paste0(clean_path, "population_poly.rds"))

# Note: now select quick as well!

poly <- poly %>% select(research_case_id, bp, hr, temperature, inr, quick, iss, iss_cat,
                        invasive, gcs, gcs_cat, sex, age, age_cat, bleeding, fracture,
                        concussion, brain_edema, brain_compression, unconsciousness)

# For heart rate and blood pressure must be documentation mistakes (always measured), 
# so I exclude them in the analysis
poly <- poly %>% drop_na(bp, hr)

# Also, a HR of 865 seems hardly normal, take it out
poly <- poly %>% filter(hr < 250)


# CAVEAT: REALIZED THAT THESE ARE NOT FACTORS!
sapply(poly[, c("bleeding", "fracture", "concussion", "brain_edema", 
                      "brain_compression", "unconsciousness")], class)
# NEED TO ENCODE AS SUCH!
poly <- poly %>% mutate(across(c(bleeding, fracture, concussion, brain_edema, 
                                 brain_compression, unconsciousness), ~ factor(.)))


## ---- NA treatment and inr/quick analysis ----

# RECALL: inr has a very weird distribution that we can't just plug in a model
boxplot(poly$inr)
# Even taking logs is not enough
boxplot(log(poly$inr))

# Discussed approach: Take some stronger trafo, e.g. 1/inr (box-cox w/ lambda = -1)
# or even 1/inr^2 (boxcox w/ lambda = -2). But then idea: Why use such a trafo of inr
# if we have quick, that is defined as 100/(2*INR - 1)?!
hist(poly$inr)
hist(poly$quick)
# Indeed, quick looks a lot better. Still a bit right skewed, but ok.
# Just a quick check if quick is reliably calculated like this

test <- poly %>%
  mutate(quicktest = 100 / (2*inr - 1)) %>%
  select(inr, quick, quicktest)

boxplot(test$quick)
boxplot(test$quicktest)
# If not enough, might try squared
boxplot(poly$quick^2)

# Unfortunately, the quick values in our dataset are not exactly the same as the ones
# calculated with the formula. They are, however, some kind of inverse. I will have a
# look if the model changes significantly, but for now I will just use the original quick.

# For temp and inr/quickmissing probably indicates no problem and thus not measured! 
# After consulting with the team and Prof. Mächler:
# I will fit a few models with NA's imputed by "normal" values and one with the NA's
# ommited and look at how (or if) the models change.

# After some research: For quick, a value in the normal range seems to be between 
# 80 and 100, which is consistent with the boxplot. I will try with a range of values.
quick_nona <- poly %>% drop_na(quick)
quick_impna <- poly %>% mutate(quick = replace_na(quick, 90))

# According to wikipedia, the body temperature ranges from 36.5–37.5 °C. Again, I will
# have a look at the models based on imputed NA values for a range of different values. 
temp_nona <- poly %>% drop_na(temperature)
temp_impna <- poly %>% mutate(temperature = replace_na(temperature, 37))



## ---- Quick Model & First Stage Residual Analysis ----

fm_quick_nona <- lm(quick ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                    bleeding + fracture + concussion + brain_edema + brain_compression +
                    unconsciousness, data = quick_nona)

# fm_quick_impna <- lm(quick ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
#                      bleeding + fracture + concussion + brain_edema + brain_compression +
#                      unconsciousness, data = quick_impna)

# Looks like there are only slight differences in the coefs (&sign.)
summary(fm_quick_nona)
# summary(fm_quick_impna)

# Some diagnostics
par(mfrow=c(2,2))
plot(fm_quick_nona)
par(mfrow=c(1,1))

# par(mfrow=c(2,2))
# plot(fm_quick_impna)
# par(mfrow=c(1,1))

# Almost no difference between imputed and omitted NAs. If I had to see sth, maybe the imputed NA seem
# to have a slightly better QQ-plot. This is also the most worrying issue. Thus,
# try squared quick (even stronger --> corresponds to boxcox w/ lambda -2)
boxplot(quick_nona$quick^2)
fm_quick_sq <- lm(quick^2 ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                  bleeding + fracture + concussion + brain_edema + brain_compression +
                  unconsciousness, data = quick_nona)

summary(fm_quick_sq)

par(mfrow=c(2,2))
plot(fm_quick_sq)
par(mfrow=c(1,1))
# Looks better than non-squared version. However, how bad is the original scale? Is it worth transforming
# and loosing interpretability? Can predict on original scale, but can't backtransform coefs!
# Using boxcox to find best trafo
boxcox(fm_quick_nona, lambda = seq(-2, 2, 0.1))
boxcox(fm_quick_sq, lambda = seq(-2, 2, 0.1))


## ---- Quick Model: Plot Residuals against all predictors ----

# As advised from Prof. Mächler, let's plot the residuals against all predictors to detect
# possible non-linearities!
par(mfrow=c(3,4))
for (var in c("gcs_cat", "iss_cat", "invasive", "sex", "age_cat", 
              "bleeding", "fracture", "concussion", "brain_edema", 
              "brain_compression", "unconsciousness")) {
  plot(quick_nona[[var]], resid(fm_quick_nona), main = paste("Residuals vs", var), xlab = var, ylab = "Residuals")
}
par(mfrow=c(1,1))

# I would guess looks ok. There is a slight increase in level AND spread/variance for age_cat tho.
# Maybe higher variability in response for older people? Artifact of truncation? 
# Is this ok or need to change sth?
"blue"

# Similar results for squared version
par(mfrow=c(3,4))
for (var in c("gcs_cat", "iss_cat", "invasive", "sex", "age_cat", 
              "bleeding", "fracture", "concussion", "brain_edema", 
              "brain_compression", "unconsciousness")) {
  plot(quick_nona[[var]], resid(fm_quick_sq), main = paste("Residuals vs", var), xlab = var, ylab = "Residuals")
}
par(mfrow=c(1,1))



## ---- Quick Model: Try Stahel's Plot (Residuals against two predictors) ----

# red for neg, blue for pos

# Tried several combinations, nothing worrying. Not that I suspected something tho.


## ---- Temperature Model & First Stage Residual Analysis ----

# RECALL:
hist(temp_nona$temperature)
boxplot(temp_nona$temperature)

# As advised by Prof. Mächler, we'll try a robust regression with lmrob()
library(robustbase)
fm_temp_nona <- lmrob(temperature ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                      bleeding + fracture + concussion + brain_edema + brain_compression +
                      unconsciousness, data = temp_nona, fast.s.large.n = Inf)
# fm_temp_impna <- lmrob(temperature ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
#                        bleeding + fracture + concussion + brain_edema + brain_compression +
#                        unconsciousness, data = temp_impna, fast.s.large.n = Inf)
summary(fm_temp_nona)
# summary(fm_temp_impna) # Some differences visible, not too much tho

par(mfrow=c(2,3))
plot(fm_temp_nona) # Note that this plot() function is different than the "normal" plot.lm!
par(mfrow=c(1,1))
# Not too bad I'd say, only the response vs fitted values seems a bit off 

# Compare to standard lm model
fm_temp_lm <- lm(temperature ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                 bleeding + fracture + concussion + brain_edema + brain_compression +
                 unconsciousness, data = temp_nona)
par(mfrow=c(2,2))
plot(fm_temp_lm)
par(mfrow=c(1,1))


# QQ-plot looks exactly like in lm! Maybe that's the problem, not weighted residuals? ASK!
# Try this:
weighted_resid <- residuals(fm_temp_nona) * weights(fm_temp_nona, type = "robustness")
qqnorm(weighted_resid, main = "Q-Q Plot of Weighted Residuals")
qqline(weighted_resid, col = "red")
# Doesn't really look better


# Note: No real difference btw imputed and omitted NA
# par(mfrow=c(2,3))
# plot(fm_temp_impna)
# par(mfrow=c(1,1))



## ---- Temperature Model: Plot Residuals against all predictors ----

# Same as before
par(mfrow=c(3,4))
for (var in c("gcs_cat", "iss_cat", "invasive", "sex", "age_cat", 
              "bleeding", "fracture", "concussion", "brain_edema", 
              "brain_compression", "unconsciousness")) {
  plot(temp_nona[[var]], resid(fm_temp_nona), main = paste("Residuals vs", var), xlab = var, ylab = "Residuals")
}
par(mfrow=c(1,1))

# I would guess looks ok!




## ---- Quick Model: Try Stahel's Plot (Residuals against two predictors) ----

p.res.2x(x = as.numeric(temp_nona$age_cat),
         y = as.numeric(temp_nona$gcs_cat),
         z = resid(fm_temp_nona),
         main = "Residuals vs Age and GCS",
         xlab = "Age Category",
         ylab = "GCS Category",
         scol = c("red", "blue"))  # red for neg, blue for pos

# Tried several combinations, nothing worrying (not exactly sure how to read it tho!)





## ---- Some Plots on Relationship between Quick/Temp and GCS. Original Scale and Squared ----

## Quick
ggplot(quick_nona, aes(x=gcs_cat, y=quick)) +
  geom_boxplot() +
  labs(title="Quick by GCS Category", x="GCS Category", y="Quick") +
  theme_minimal()
# ggplot(quick_impna, aes(x=gcs_cat, y=quick)) +
#   geom_boxplot() +
#   labs(title="Quick by GCS Category", x="GCS Category", y="Quick") +
#   theme_minimal()
# Also here, minor difference. Ofc, bit less var with impna.

# ggplot(quick_impna, aes(x=gcs_cat, y=quick^2)) +
#   geom_boxplot() +
#   labs(title="Quick by GCS Category", x="GCS Category", y="Quick") +
#   theme_minimal()

## Temperature
ggplot(temp_nona, aes(x=gcs_cat, y=temperature)) +
  geom_boxplot() +
  labs(title="Temperature by GCS Category", x="GCS Category", y="Temperature") +
  theme_minimal()
# ggplot(temp_impna, aes(x=gcs_cat, y=temperature)) +
#   geom_boxplot() +
#   labs(title="Temperature by GCS Category", x="GCS Category", y="Temperature") +
#   theme_minimal()
# I can see no difference at all!



## ---- NOTES FROM MEETING ----


# CAN TRY AROUND WITH MAKING GCS A BINARY (0 OR NOT) IN QUICK MODEL TO HAVE EVEN MORE
# EASILY INTERPRETABLE RESULTS (MAYBE A BIT LESS PREDICTIVE POWER, BUT STILL OK AND WAY EASIER TO INTERPRET)
# SAME THING: COULD ALSO DO LESS LEVELS FOR E.G. AGE (BUT NOT MOST IMPORTANT Q / ASPECT HE SAID)


# ADD COLOR OF GCS VALUE IN LEVERAGE PLOT TO SEE IF FOUR GROUPS CORRESPOND TO GCS SCORES 
# THEN, IF YES, TRY MODEL ONLY WITH HIGHER GCS SCORES (WITHOUT LEVEL 0)
# TO SEE IF THERE'S SOMETHING GOING ON THERE

# ALSO, LOWER BODY TEMP ARE MORE INTERESTING -> LOGISTIC W/ BINARY TOO LOW / NORMSL TEMP



# DO BOXPLOTS FOR RESIDUALS AGAINST PREDICTORS FOR BINARY CASES (INSTEAD)
# --> use methods formula


# CAN ALSO RECYCLE SAVERIO'S SUMMARY OUTPUT PLOT CODE TO VISUALIZE




"blue"
# NEW
# R Squared both for temp and quick very low. Is this even useable?
# What is advantage of the binary gcs or additional levels? Doesn't make the model better
# Update Leverage Plot not gcs groups!
# Binary Temp Model really bad. Or I can't interpret.
# CAVEAT: I realized that the injuries were not encoded as factors!
# After fixing this, the residuals vs predictors plots show boxplots for these binary variables as well.
# Nothing worrying.




## ---- Binary GCS Quick Model ----

# While doing a new binary gcs variable 0/not 0 I realized that there aren't that many 0, but many 1.
# So the grouping in the Residuals vs Leverage plot is not what we assumed! 
"blue"
plot(quick_nona$gcs_cat)
# I'll try a model with binary 0,1 vs 2,3 (not sure if good bc not a lot of obs for 2/3)
# However, I'll try to color it (as he suggested) to see where this artifact comes from
quick_nona <- quick_nona %>% 
  mutate(gcs_bin = ifelse(gcs_cat == "0.unknown" | gcs_cat == "1.mild", 0, 1)) %>%
  mutate(gcs_bin = factor(gcs_bin))

fm_quick_bin <- lm(quick ~ gcs_bin + iss_cat + invasive + sex + age_cat + 
                           bleeding + fracture + concussion + brain_edema + brain_compression +
                           unconsciousness, data = quick_nona)

summary(fm_quick_bin)
# Some diagnostics
par(mfrow=c(2,2))
plot(fm_quick_bin)
par(mfrow=c(1,1))

"blue"
# Doesn't really change anything in the diagnostics. 
# Maybe still easier to interpret then and thus better in practice? ASK!!!


## ---- Rework Levels ----

# Create a new age variable with only 3 levels
quick_nona <- quick_nona %>%
  mutate(age_gen = case_when(
    age_cat %in% c("1.<30", "2.30-39") ~ "1.<40",
    age_cat %in% c("3.40-49", "4.50-59", "5.60-69") ~ "2.40-69",
    age_cat %in% c("6.70-78", "7.79+") ~ "3.70<")) %>%
  mutate(age_gen = factor(age_gen, 
                             levels = c("1.<40", "2.40-69", "3.70<"), 
                             ordered = TRUE))

# Quite balanced
plot(quick_nona$age_gen)

fm_quick_gen <- lm(quick ~ gcs_bin + iss_cat + invasive + sex + age_gen + 
                     bleeding + fracture + concussion + brain_edema + brain_compression +
                     unconsciousness, data = quick_nona)

# Still age is highly significant. Don't know what the advantage was here.
summary(fm_quick_gen)


## ---- Updated Leverage Plot ----

# Prepare the data
diagnostics_df <- data.frame(
  leverage = hatvalues(fm_quick_nona),
  std_resid = rstandard(fm_quick_nona),
  gcs_cat = quick_nona$gcs_cat
)

# Plot with distinct colors
ggplot(diagnostics_df, aes(x = leverage, y = std_resid, color = gcs_cat)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black", "blue", "green", "red")) +
  labs(
    title = "Residuals vs Leverage",
    x = "Leverage",
    y = "Standardized Residuals",
    color = "GCS Category"
  ) +
  theme_minimal()

# Doesn't look like gcs is responsible for this clustering


# Try around with different predictors
diagnostics_df <- data.frame(
  leverage = hatvalues(fm_quick_nona),
  std_resid = rstandard(fm_quick_nona),
  variable = quick_nona$iss_cat
)
ggplot(diagnostics_df, aes(x = leverage, y = std_resid, color = variable)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("black", "blue", "green", "red")) +
  labs(
    title = "Residuals vs Leverage",
    x = "Leverage",
    y = "Standardized Residuals",
    color = "Level"
  ) +
  theme_minimal()
















## ---- Binary Temp Model ----

# According to various sources, hypothermia happens if body temp drops below 35 degrees Celsius.
# We'll look at a model where I have a binary variable with too low / ok

temp_nona <- temp_nona %>% 
  mutate(hypothermia = ifelse(temperature <= 35, 1, 0)) %>%
  mutate(hypothermia = factor(hypothermia))

table(temp_nona$hypothermia)

fm_hypothermia <- glm(hypothermia ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                      bleeding + fracture + concussion + brain_edema + brain_compression +
                      unconsciousness, data = temp_nona, family = binomial(link = "logit"))

library(boot)
glm.diag.plots(fm_hypothermia)

"blue"
# Useless I guess, or I can't interpret my results.






# SAVE'S SUMMARY PLOT!

# ADD AIS THORAX VARIABLE (BINARY SEVERE_THORAX)

# --> Save's going to add it in the dataset I load here (also factor encoding I corrected here)
# --> check if done tmrw


# ASK MÄCHLER IF ROBUST REGRESSION IF DOESN'T CHANGE OR LM()

# MAKE A LIST OF MY QUESTIONS!






















