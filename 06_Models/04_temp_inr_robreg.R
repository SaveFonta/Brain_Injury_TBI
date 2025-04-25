############################################################################
### Quick & Temp Models w/ Robust Regression (rec from R-God personally) ###
############################################################################

rm(list = ls())

library(data.table)
library(ggplot2)
library(MASS) # load before tidyverse, otherwise select() gets masked
library(tidyverse)
library(car) 

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

fm_quick_impna <- lm(quick ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                     bleeding + fracture + concussion + brain_edema + brain_compression +
                     unconsciousness, data = quick_impna)

# Looks like there are only slight differences in the coefs (&sign.)
summary(fm_quick_nona)
summary(fm_quick_impna)

# Some diagnostics
par(mfrow=c(2,2))
plot(fm_quick_nona)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot(fm_quick_impna)
par(mfrow=c(1,1))
# Almost no difference between imputed and omitted NAs. If I had to see sth, maybe the imputed NA seem
# to have a slightly better QQ-plot. This is also the most worrying issue. Thus,
# try squared quick (even stronger --> corresponds to boxcox w/ lambda -2)
boxplot(quick_nona$quick^2)
fm_quick_sq <- lm(quick^2 ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                  bleeding + fracture + concussion + brain_edema + brain_compression +
                  unconsciousness, data = quick_nona)

par(mfrow=c(2,2))
plot(fm_quick_sq)
par(mfrow=c(1,1))
# Looks better than non-squared version. However, how bad is the original scale? Is it worth transforming
# and loosing interpretability? Can predict on original scale, but can't backtransform coefs!
"blue"
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

library(sfsmisc)
p.res.2x(x = as.numeric(quick_nona$age_cat),
         y = as.numeric(quick_nona$gcs_cat),
         z = resid(fm_quick_nona),
         main = "Residuals vs Age and GCS (Quick²)",
         xlab = "Age Category",
         ylab = "GCS Category",
         scol = c("red", "blue"))  # red for neg, blue for pos

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
fm_temp_impna <- lmrob(temperature ~ gcs_cat + iss_cat + invasive + sex + age_cat + 
                       bleeding + fracture + concussion + brain_edema + brain_compression +
                       unconsciousness, data = temp_impna, fast.s.large.n = Inf)
summary(fm_temp_nona)
summary(fm_temp_impna) # Some differences visible, not too much tho

par(mfrow=c(2,3))
plot(fm_temp_nona) # Note that this plot() function is different than the "normal" plot.lm!
par(mfrow=c(1,1))
# Not too bad I'd say, only the response vs fitted values seems a bit off 
"blue"

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
par(mfrow=c(2,3))
plot(fm_temp_impna)
par(mfrow=c(1,1))



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
"blue"



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
ggplot(quick_impna, aes(x=gcs_cat, y=quick)) +
  geom_boxplot() +
  labs(title="Quick by GCS Category", x="GCS Category", y="Quick") +
  theme_minimal()
# Also here, minor difference. Ofc, bit less var with impna.

ggplot(quick_impna, aes(x=gcs_cat, y=quick^2)) +
  geom_boxplot() +
  labs(title="Quick by GCS Category", x="GCS Category", y="Quick") +
  theme_minimal()

## Temperature
ggplot(temp_nona, aes(x=gcs_cat, y=temperature)) +
  geom_boxplot() +
  labs(title="Temperature by GCS Category", x="GCS Category", y="Temperature") +
  theme_minimal()
ggplot(temp_impna, aes(x=gcs_cat, y=temperature)) +
  geom_boxplot() +
  labs(title="Temperature by GCS Category", x="GCS Category", y="Temperature") +
  theme_minimal()
# I can see no difference at all!



## ---- Notes ----





# NOW WE CAN MAKE THE MODELS

# QUICK: TRY LM

# TEMP: USE PACKAGE ROBUST BASE (lmrob()) 

#DO THE 4 BASIC PLOTS (FIRST STAGE)
#THEN: PLOT RESIDUALS AGAINST ALL PREDICTORS
#THEN MAYBE: PLOT WHERE SEE RESIDUALS VS TWO PREDICTORS FOR INTERACRTION
#--> SFS.MISC AND SOME FUNCTION, ALSO IN STAHEL'S BOOK (stahel's residual plot against two X's)

#WHEN USING LMROB RESIDUAL PLOT DIFFERENTLY --> WEIGHTED RESIDUALS SHOULD LOOK LIKE NORMAL, NOT NORMAL RESIDUALS
















