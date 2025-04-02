###############
### Try Models
###############

rm(list = ls())

setwd("/Users/valentinjohner/Desktop/ETH/FS25/Stats Lab")

library(tidyverse)
library(sjPlot)

load("02_data/02_clean_data/interventions1_wide.RData")
load("02_data/02_clean_data/mapping_cleaned.RData")


# =============================================
# AIS Dataset
# =============================================
# Take cases with at least 2 injuries in at least 2 AIS regions
polytrauma_patients <- mapping_cleaned %>%
  group_by(research_case_id) %>%
  filter(n() >= 2, n_distinct(AIS_CHAPTER) >= 2) %>%
  ungroup()

# Make a table with max AIS Head score, total number of head injuries, total injuries in case,
# nr of different body regions affected
ais_df <- polytrauma_patients %>%
  group_by(research_case_id) %>%
  summarise(
    max_AIS_head = max(MAX_AIS_SEVERITY[AIS_CHAPTER == 1], na.rm = TRUE),
    num_head_injuries = sum(AIS_CHAPTER == 1), 
    total_injuries = n(), 
    num_regions = n_distinct(AIS_CHAPTER) 
  ) %>%
  ungroup()

# Note that there are a lot of warnings where no head injury exists. 
# Question: How can I classify those cases that have no head injury? 
# Natural intuition: Make them 0. There are, however 8 other cases that are 0 as well and have 
# head injury. Can look at the injuries, I list the cases here:

zero_cases <- c("1E362E5E-D6C9-4967-9343-9330202F5A3B", "25E9A734-0414-4DA7-8A34-19D4C0BE02B4",
                "42BCD9A2-ACA9-4883-9BCA-F760CE0DCE46", "66FECA95-DF63-41AC-8BEB-FE0549C4CF2A",
                "88558286-C843-419A-B700-40E9DE55964B", "B8C2CAD5-1D87-4F30-BF12-1EA99001516A",
                "BA24D9DC-EEB1-42E5-B971-48D8DDDA3E83", "C19331DB-E475-4384-B857-AF2D6785DF03")

zero_cases <- polytrauma_patients %>%
  filter(research_case_id %in% zero_cases)

# Looks like they're not really specified, i.e.,
# "injury of blood vessels of head, not elsewhere classified" or
# "unspecified intracranial injury" or
# "other specified injuries of head"
# CHECK APPROACH WITH GROUP OR MENTION TO CLIENT/PROFESSOR WHAT I DECIDED.
# IT SEEMS LIKE THE HEAD WAS NEVER THE MAIN INJURY AND NOT SEVERE AT ALL
# I DECIDE TO CLASSIFY THE CASES WITH NO AIS HEAD INJURY AS 0!

ais_df <- ais_df %>%
  mutate(max_AIS_head = ifelse(is.infinite(max_AIS_head), 0, max_AIS_head))


# =============================================
# Interventions Dataset
# =============================================
# For an analysis with a dummy want to classify severe / not severe interventions. I take this classification.
# SUBJECT TO CHANGE BASED ON LEVY'S PROFESSIONAL OPINION!

severe_interventions <- c("Korrektur Aneurysma/intrakranielle Gefässe",
                          "Korrektur Knocheneingriffe Schädel",
                          "Korrektur Kraniotomie/Kraniektomie",
                          "Korrektur Zugänge zur Schädelbasis/Schädelkalotte",
                          "Korrektur Inzision an den Hirnhäuten")

interventions <- c("Korrektur Intrakranielles Monitoring",
                   "Korrektur Aneurysma/intrakranielle Gefässe",
                   "Korrektur Knocheneingriffe Schädel",
                   "Korrektur Kraniotomie/Kraniektomie",
                   "Korrektur Bohrloch",
                   "Korrektur Zugänge zur Schädelbasis/Schädelkalotte",
                   "Korrektur Inzision an den Hirnhäuten",
                   "Korrektur Punktion/Drainage/Inzision",           
                   "Korrektur Ventrikelsystem",                    
                   "Korrektur Ekzision/Destruktion",                 
                   "Korrektur Hirnhäute/intrakranielle Nerven")


# Create dummy in a new dataset
inter <- interventions1_wide %>% 
  mutate(intervention_severe = if_any(all_of(severe_interventions), ~ . == 1)) %>%
  mutate(intervention_severe = as.integer(intervention_severe)) %>%
  select(research_case_id, intervention_severe)

# CREATE DIFFERENT DUMMY AND RUN AGAIN! E.g ALL vs NONE
# inter <- interventions1_wide %>%
#   mutate(intervention_severe = if_any(all_of(interventions), ~ . == 1)) %>%
#   mutate(intervention_severe = as.integer(intervention_severe)) %>%
#   select(research_case_id, intervention_severe)

# =============================================
# Merge and CHI^2 Test
# =============================================
# I perform a left join since the inter dataset contains all cases (also not polytrauma cases)
dfdummy <- left_join(ais_df, inter, by = "research_case_id")

table(is.na(dfdummy$intervention_severe))
test <- dfdummy %>%
  filter(is.na(intervention_severe))
# Unfortunately, we have 286 cases that do not have any recorded interventions. There is no pattern
# there, and some of the cases are severe (e.g., 17 injuries, AIS 3, etc). I don't believe that 
# no intervention was performed there, so we can't really set it to 0 or sth like that. Thus,
# I TREAT THEM AS TRUE MISSING AND REMOVE THESE CASES!
dfdummy <- dfdummy %>% filter(!is.na(intervention_severe))


# Simple test for association of variables
chisq.test(table(dfdummy$max_AIS_head, dfdummy$intervention_severe))

# Highly associated!

# =============================================
# Import Controls
# =============================================

patients <- read_csv("02_data/02_clean_data/patients_clean.csv") %>%
  select(research_case_id, age, sex, death)

dfdummy <- left_join(dfdummy, patients, by = "research_case_id")

table(is.na(dfdummy))
# No mismatch for once! Nice!

# Make sure factors are interpreted as such
dfdummy$max_AIS_head <- as.factor(dfdummy$max_AIS_head)
dfdummy$intervention_severe <- as.factor(dfdummy$intervention_severe)
dfdummy$sex <- as.factor(dfdummy$sex)
dfdummy$death <- as.factor(dfdummy$death)
str(dfdummy)


# =============================================
# Logistic Regression with Dummy
# =============================================
# NOTE THAT I DON'T INCLUDE num_head_injuries, total_injuries, num_regions SINCE THEY'RE HIGHLY 
# CORRELATED WITH AIS HEAD SCORE. MAYBE total_injuries COULD MAKE SENSE TO INCLUDE (doesn't change anything)

# Controls: Age, Sex, Death. WHAT ELSE?

binary_model <- glm(intervention_severe ~ max_AIS_head + age + sex + death, 
             data = dfdummy, 
             family = binomial)

summary(binary_model)
# Odds over 1 --> seems like ais does increase chance of intervention
exp(coef(binary_model))

# Plot model
plot_model(binary_model, vline.color = "red")  
plot_model(binary_model, show.values = TRUE, value.offset = .3) 

# Surprising: AIS 1 cases have lower odds of intervention than AIS 0 (p = 0.025).  
# Due to very severe AIS 3 cases "pushing down" AIS 1 in relative terms?
# I CAN'T MAKE SENSE OF IT
## ADD-ON LATER: W/ DUMMY 1/0 IF ANY INTERVENTION NO SUCH PROBLEM
# Age slightly lowers intervention chance (p = 0.018), but small effect.
# (Maybe conservative with elerdly patients?)
# Death strongly associated with intervention (p < 0.001), as expected.  
# No effect of sex (p = 0.878).  


# See what happens when we run the model without AIS_head = 3
df_no_AIS3 <- dfdummy %>% filter(max_AIS_head != 3)
fm_no_AIS3 <- glm(intervention_severe ~ max_AIS_head + age + sex + death, 
                     data = df_no_AIS3, family = binomial)
summary(fm_no_AIS3)
# SAME RESULT!
# Check total injuries per AIS group
dfdummy %>% group_by(max_AIS_head) %>% 
  summarise(mean_injuries = mean(total_injuries, na.rm = TRUE),
            median_injuries = median(total_injuries, na.rm = TRUE),
            n = n())
# Also nothing that could explain it!


# =============================================
# Logistic Regression with Dummy & Interactions
# =============================================

dummy_interage <- glm(intervention_severe ~ max_AIS_head * age + sex + death, 
                      data = dfdummy, family = binomial)
summary(dummy_interage)

dummy_interdeath <- glm(intervention_severe ~ max_AIS_head * death + sex + age, 
                        data = dfdummy, family = binomial)
summary(dummy_interdeath)

dummy_inter <- glm(intervention_severe ~ max_AIS_head  + death + sex + age + max_AIS_head:death + max_AIS_head:age, 
                        data = dfdummy, family = binomial)
summary(dummy_inter)


plot_model(dummy_interage, vline.color = "red")
plot_model(dummy_interdeath, vline.color = "red")
plot_model(dummy_inter, vline.color = "red")

## Interaction Models:
## Age × AIS (dummy_interage): Slight improvement in fit, but age effect weakens. 
# Older patients with severe AIS (3) less likely to get intervention.
# No negative effect of AIS 1 and 2 anymore compared to AIS 0!
## Death × AIS (dummy_interdeath): Best single interaction model. 
# Severe AIS (3) patients who die are less likely to receive an intervention.
## Full Model (dummy_inter): Best fit, confirms Death × AIS (3) effect, but weakens Age × AIS.
# No negative effect of AIS 1 and 2 anymore compared to AIS 0!

# TAKE FULL MODEL?

# ADD-ON LATER: IF CLASSIFICATION IF ANY INTERVENTION NO NEGATIVE EFFECTS
# --> DIFFERENT BEHAVIOUR OF INTERACTIONS

# NOTE
"red"
# SUMMARIZE BEFORE TALK!!! 



# =============================================
# glmer for each intervention
# =============================================

interlong <- read_csv("02_data/02_clean_data/interventions1_long.csv") %>%
  mutate(procedure = str_remove(procedure, "Korrektur ")) %>%
  select(-source)

interlong$procedure <- as.factor(interlong$procedure)
str(interlong)

dfindiv <- left_join(ais_df, interventions1_wide, by = "research_case_id") %>% 
  na.omit()

## Check multicollinearity etc.
correlation_matrix <- cor(dfindiv %>% select(starts_with("Korrektur")), use = "pairwise.complete.obs")
# No correlation above 0.7, but still some instability in the models (do not always converge)

# These two highest correlation (0.677)
table(dfindiv$`Korrektur Kraniotomie/Kraniektomie`, dfindiv$`Korrektur Inzision an den Hirnhäuten`)


# Join patient data, rename and ensure correct encoding

dfindiv <- left_join(dfindiv, patients, by = "research_case_id") %>%
  rename(Intrakranielles_Monitoring = `Korrektur Intrakranielles Monitoring`,
         Aneurysma_intrakranielle_Gefässe = `Korrektur Aneurysma/intrakranielle Gefässe`,
         Knocheneingriffe_Schädel = `Korrektur Knocheneingriffe Schädel`,
         Kraniotomie_Kraniektomie = `Korrektur Kraniotomie/Kraniektomie`,
         Bohrloch = `Korrektur Bohrloch`,
         Zugänge_Schädelbasis_Schädelkalotte = `Korrektur Zugänge zur Schädelbasis/Schädelkalotte`,
         Inzision_Hirnhäute = `Korrektur Inzision an den Hirnhäuten`,
         Punktion_Drainage_Inzision = `Korrektur Punktion/Drainage/Inzision`, 
         Ventrikelsystem = `Korrektur Ventrikelsystem`,
         Ekzision_Destruktion = `Korrektur Ekzision/Destruktion`,
         Hirnhäute_intrakranielle_Nerven = `Korrektur Hirnhäute/intrakranielle Nerven`) %>%
  mutate(across(c(max_AIS_head,
                  Intrakranielles_Monitoring, 
                  Aneurysma_intrakranielle_Gefässe, 
                  Knocheneingriffe_Schädel, 
                  Kraniotomie_Kraniektomie, 
                  Bohrloch, 
                  Zugänge_Schädelbasis_Schädelkalotte, 
                  Inzision_Hirnhäute, 
                  Punktion_Drainage_Inzision, 
                  Ventrikelsystem, 
                  Ekzision_Destruktion, 
                  Hirnhäute_intrakranielle_Nerven), as.factor))

str(dfindiv)


# NOTE: mvProbit() function runs too long; in documentation stated that this might be the case.
# Also, a lot of warnings are given that the correlation matrix is not pd. We might have very 
# rare interventions, (almost) perfect collinearity or sth like that. We'll check later.

# brms ran too long as well. Also, I can't estimate correlations btw outcomes with bernoulli (only in
# multivariate gaussian or student models)


# MY THOUGHTS: RUNNING INDIVIDUAL glm() MODELS MIGHT NOT MAKE SENSE SINCE IT DOES NOT ACCOUNT FOR 
# CORRELATION. HOW CAN I KNOW IF CORRELATION TOO LARGE?
# IF WE FIT SEPARATE glmer() MODELS WE STILL DO NOT FULLY ACCOUNT FOR IT, BUT MAYBE 
# FOR SOME CORRELATION SINCE WE ADD A RANEF FOR EACH PATIENT (patient with one procedure may be
# more likely to receive another). BUT: TOO COMPLEX WITH RANEF FOR research_case_id (doesn't converge; too large eigenvalues)

# OTHER ALTERNATIVE, TRIED BELOW: SINGLE glmer() MODEL WITH ALL INTERVENTIONS IN LONG FORMAT



library(lme4)
# model1 <- glmer(Intrakranielles_Monitoring ~ max_AIS_head + sex + age + death + (1 | research_case_id), 
#                 data = dfindiv, family = binomial)

# Fails

# Individual glms
model1 <- glm(Intrakranielles_Monitoring ~ max_AIS_head + age + sex + death, 
              data = dfindiv, family = binomial)
model2 <- glm(Aneurysma_intrakranielle_Gefässe ~ max_AIS_head + age + sex + death, 
                data = dfindiv, family = binomial)
model3 <- glm(Knocheneingriffe_Schädel ~ max_AIS_head + age + sex + death, 
              data = dfindiv, family = binomial)
summary(model1)
summary(model2)
summary(model3)
# etc.

# NOTE
"red"
# HOW CAN I VISUALIZE / SUMMARIZE THE FINDINGS OF THESE MODELS? --> CHATGPT
# HOW CAN I CHECK IF IT SUITS THE DATA? --> CHATGPT




# =============================================
# glmer all together
# =============================================

# Convert to long format
df_long <- dfindiv %>%
  pivot_longer(cols = 6:16, 
               names_to = "intervention_type", 
               values_to = "intervention_received")

df_long$intervention_type <- as.factor(df_long$intervention_type)
str(df_long)

# fit
df_long$age <- scale(df_long$age)
multi_glmer <- glmer(intervention_received ~ max_AIS_head + age + sex + death + 
                       (1 | intervention_type),  
                     data = df_long, family = binomial)

summary(multi_glmer)

# Model failed to converge again (with ranef for unique_case_id)
# If no ranef for unique_case_id faster, but nearly unidentifiable (large eigenvalue)
# --> rescale age --> no warnings





# NOTE
"red"
# WHAT DOES IT MEAN IF NO RANEF FOR patients/case? Still valid? --> CHATGPT
# Answer: If cases have multiple interventions, then observations from the same case are correlated.
#         Ignoring this could lead to underestimated standard errors, meaning p-values might be misleading.
# --> REMOVE SOME HIGHLY CORRELATED VARIABLES? OR WHAT TO DO? --> MÄCHLER

# NOTE
"red"
# FIGURE OUT VISUALIZATION OF RESULTS --> CHATGPT
library(sjPlot)
plot_model(multi_glmer, type = "est", show.values = TRUE, show.p = TRUE)

ranefs <- as.data.frame(ranef(multi_glmer)$intervention_type)
ranefs$intervention_type <- rownames(ranefs) 
ggplot(ranefs, aes(x = reorder(intervention_type, `(Intercept)`), y = `(Intercept)`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Random Effects of Intervention Type",
       y = "Intercept Adjustment",
       x = "Intervention Type") +
  coord_flip()

# NOTE
"red"
# INTERPRETATION --> ASK CHATGPT
# Intercept (-6.39)
# Very low log-odds --> intervention is rare overall.
# max_AIS_head3 (2.55)
# Higher AIS scores --> much higher odds of intervention.
# age (-0.11)
# Older patients are less likely to receive the intervention.
# death (0.80)
# Patients who died had a higher probability of receiving an intervention.
# sexm (0.05, p=0.40)
# No significant effect of sex.
















