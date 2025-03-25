# Load libraries
library(dplyr)       # For data manipulation
library(tidyr)       # For data cleaning
library(ggplot2)     # For data visualization
library(readr)       # For reading CSV files
library(lubridate)   # For date manipulation
library(car)         # For statistical tests
library(broom)       # For tidy model outputs



# Load datasets
vitals <- read_csv("02_data/02_clean_data/vitals_clean.csv")
lab <- read_csv("02_data/02_clean_data/lab_clean_wide.csv") #uso il wide perche penso sia meglio
patients <- read_csv("02_data/02_clean_data/patients_clean.csv")
interventions <- read_csv("02_data/02_clean_data/interventions1_wide.csv")
diagnosis <- read_csv("02_data/02_clean_data/Diagnosis_cleaned.csv")
diagnosis_long <- read_csv("02_data/02_clean_data/Diagnosis_long.csv") #not useful
load("02_data/02_clean_data/mapping_cleaned.RData")
mapping <- mapping_cleaned
#mapping 


# =============================================
# HEAD INJURY ANALYSIS - RANDOM COMMENTS
# =============================================

"AIS_CHAPTER: integer, represents the chapter number of the AIS (Abbreviated Injury Scale) classification.
Dat: Date, the date when the injury or event occurred (format: YYYY-MM-DD).
DESCRIPTION: character, a detailed description of the injury or condition.
ISS_BODY_REGION: integer, represents the body region affected by the injury based on AIS classification (for example, 4 could indicate a specific region like “limbs”).
MAX_AIS_SEVERITY: integer, the maximum severity score of the injury on the AIS scale (range 0 to 6).
research_case_id: character, ID of the case, which uniquely identifies the patient or incident."

#stiamo un attimo su mapping. Il problema maggiore è proprio che hai piu righe per ogni infortunio.
#pero posso estrarre lo stesso solamente quelli che hanno ISS-BODY_regione == 1 e prendere solo i tizi che hanno inforuni alla testa

#quindi il piano è estrarre gli ID della gente che ha un infortunio alla testa. 
#Una volta estratti quegli ID, filtro il df mapping per avere nel nuovo df solo quei pazienti che hanno avuto una botta alla testa
#da qui posso capire quanti pazienit hanno avuto politrauma (e quindi la loro ID compare piu di una volta) e quanti invece solo un infortunio alla testa


"Intracranial Injury Distribution " 
#--> can use AIS_MAX!
# --> can use diagnosis!
                              

#AIS_CHAPTER --> -1 è tipo avvelenamento o che ne so io 

#Levy dice usare AIS_CHAPTER, non ISS_BODY_REGION






# =============================================
# HEAD INJURY ANALYSIS - DATA PREPARATION
# =============================================


# 1. Identify patients with head injuries (AIS Chapter 1)
AIS1 <- mapping[mapping$AIS_CHAPTER == 1, ]  # Only those with head injuries
head_inj <- unique(AIS1$research_case_id)     # IDs of those with head injuries
total_ID <- unique(mapping$research_case_id)  # Total number of IDs in dataset

# Basic statistics
length(head_inj)  # Count of patients with head injuries
length(total_ID)  # Total patients in dataset
length(head_inj) / length(total_ID)  # Percentage with head injuries (44.8%)

# Create filtered dataset of only  patients who had an head injury
head_inj_mapping <- mapping[mapping$research_case_id %in% head_inj, ] 
dim(head_inj_mapping)

#remember that the sme patient can have more than one injuries at the head (so maybe two rows
#for the same patients with AIS_CHAPTER == 1)


# =============================================
# HEAD INJURY ANALYSIS - POLYTRAUMA YES / POLYTRAUMA NO??
# =============================================
#let's see if all cases are polytrauma
#this means, does every ID appears more than once?
unique_values <- names(which(table(head_inj_mapping$research_case_id) == 1))
unique_values

#ok not all cases are polytraumas
#plot them 
table(head_inj_mapping$research_case_id) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = Freq)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Number of Injuries per Patient",
       x = "Number of Injuries",
       y = "Frequency")

"So we can conclude that not all cases of head injuries are head injuries"



# =============================================
# HEAD INJURY ANALYSIS - COUNT OF SEVERITY
# =============================================

#Ok, now use AIS1 to create a table for the MAX_AIS_SEVERITY
AIS1 #df containing all the lines w/ head injuries 
#count
AIS1 %>%  #only those injuries that are at the head (use head_inj_mapping if you want to check the severity of all people who had an head injury)
  group_by(MAX_AIS_SEVERITY) %>% 
  summarise(n = n())


#plot
AIS1 %>% 
  group_by(MAX_AIS_SEVERITY) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = MAX_AIS_SEVERITY, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Intracranial Injury Distribution by AIS Head",
       x = "MAX_AIS_SEVERITY",
       y = "Frequency")

#PROBLEM: maybe the same person can enter more than once cause he can have more than one head injury


# Create a frequency table of HEAD injuries per patient with severity information
#do the same only with head injuries
injury_counts <- AIS1 %>%
  group_by(research_case_id) %>%
  summarise(
    num_injuries = n(),
    max_severity = max(MAX_AIS_SEVERITY)  # Get the maximum severity for each patient
  )

# Plot
ggplot(injury_counts, aes(x = num_injuries, fill = factor(max_severity))) +
  geom_bar() +
  labs(title = "Number of Head Injuries per Patient by Maximum AIS Severity",
       x = "Number of Head Injuries",
       y = "Count of Patients",
       fill = "Maximum AIS Severity") +
  scale_fill_discrete(name = "Maximum AIS Severity") +
  theme_minimal()




# =============================================
# HEAD INJURY ANALYSIS - JUST A RANDOM CHECK
# =============================================
"Are there really two people with 11 head injuries??"
table <- AIS1 %>%  
  group_by(research_case_id) %>%   
  summarise(
    MAX_AIS_SEVERITY = max(MAX_AIS_SEVERITY, na.rm = TRUE),  # Highest severity per patient
    NUM_HEAD_INJURIES = n()  # Total number of head injuries per patient
  ) %>%  
  group_by(MAX_AIS_SEVERITY, NUM_HEAD_INJURIES) %>%  # Group by both severity and number of injuries
  summarise(n = n(), .groups = "drop") %>%  # Count occurrences
  arrange(desc(MAX_AIS_SEVERITY), NUM_HEAD_INJURIES)  # Sort by severity




patients_with_11 <- AIS1 %>%  
  group_by(research_case_id) %>%   
  summarise(NUM_HEAD_INJURIES = n(), .groups = "drop") %>%  # Count injuries per patient
  filter(NUM_HEAD_INJURIES == 11)  # Keep only those with 11 injuries

print(patients_with_11)
#yes it's true, even if each one of the injuries can have a different MAX_AIS_SEVERITY





# =============================================
# HEAD INJURY ANALYSIS - ADD GCS ????
# =============================================
# Add GCS groups to vitals data
vitals <- vitals %>%
  mutate(
    GCS_group = case_when(
      gcs >= 13 ~ "Mild (13-15)",
      gcs >= 9  ~ "Moderate (9-12)",
      gcs <= 8  ~ "Severe (3-8)",
      TRUE      ~ NA_character_
    )
  )

vitals %>% filter(is.na(gcs)) %>% count()  # Count of missing GCS


#create df with the worst head injury per patient
severity_per_patient <- mapping %>%
  filter(AIS_CHAPTER == 1) %>%           # Keep only head injuries (AIS Chapter 1)
  group_by(research_case_id) %>%         # Group by patient ID
  summarise(MAX_AIS_SEVERITY = max(MAX_AIS_SEVERITY, na.rm = TRUE))  # Find worst head injury per patient

#merge with vitals, dropping any missing GCS (?)
merged_data <- vitals %>%
  left_join(severity_per_patient, by = "research_case_id") %>%  # Merge GCS + AIS data
  filter(!is.na(GCS_group))  # Remove patients without a GCS score


table2 <- merged_data %>%
  group_by(GCS_group, MAX_AIS_SEVERITY) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = MAX_AIS_SEVERITY,
    values_from = Count,
    values_fill = 0
  )

# Print the table
print(table2)


knitr::kable(table2, caption = "Table 2: AIS_MAX by GCS Groups")

#so many missings in both ways.....




# =============================================
# HEAD INJURY ANALYSIS - INJURY DESCRIPTION
# =============================================
# Tabulate injury descriptions
head_injury_table <- as.data.frame(table(AIS1$DESCRIPTION))

# Sort by frequency
head_injury_table <- head_injury_table[order(-head_injury_table$Freq), ]

# View the first injuries with a nice visualization 
head_injury_table %>%
  head(10) %>%
  ggplot(aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "pink") +
  coord_flip() +
  labs(title = "Top 10 Head Injuries",
       x = "Injury Description",
       y = "Frequency")






# =============================================
# HEAD INJURY ANALYSIS - COUNT OF SEVERITY for GCS
# =============================================

#I NA di GCS sono troppi, rimuovo
vitals_no_NA <- vitals %>% filter(!is.na(GCS_group))


vitals_no_NA %>%  #only those injuries that are at the head (use head_inj_mapping if you want to check the severity of all people who had an head injury)
  group_by(GCS_group) %>% 
  summarise(n = n())


#plot
vitals_no_NA %>% 
  group_by(GCS_group) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = GCS_group, y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Intracranial Injury Distribution by GCS Group",
       x = "GCS Group",
       y = "Frequency")




# =============================================
# HEAD INJURY ANALYSIS - ADD DIAGNOSIS
# =============================================
#Too many diagnosis column. I want to group them a little

diagnosis_reduced <- diagnosis %>%
  mutate(
    # 1. Bleeding: 1 if ANY bleeding type occurred
    any_bleeding = as.integer(bleeding == 1 | subdural_bleeding == 1 | 
                                epidural_bleeding == 1 | subarachnoid_bleeding == 1 | 
                                other_bleeding == 1),
    
    # 2. Fracture: 1 if ANY fracture type occurred
    any_fracture = as.integer(fracture == 1 | skull_base_fracture == 1 | skull_cap_fracture == 1),
    
    # 3. Brain injury: 1 if EITHER contusion or diffuse injury occurred
    any_brain_injury = as.integer(contusion == 1 | diffuse_brain_injuries_and_ripping_shearing == 1),
    
    # 4. Symptoms: Keep as-is (already dummies)
    # (Renamed for clarity)
    unconsciousness = unconsciousness,
    concussion = concussion,
    brain_edema = brain_edema,
    brain_compression = brain_compression
  ) %>%
  
  # REMOVE all original component columns (keep only grouped dummies)
  select(
    research_case_id,
    date,
    # Grouped dummies:
    any_bleeding,
    any_fracture,
    any_brain_injury,
    # Symptoms:
    unconsciousness,
    concussion,
    brain_edema,
    brain_compression,
    # Concussion corrections (keep as dummies):
    concussion_correction_formula,
    concussion_correction_including_unconsciousness_in_tbi,
    # Other:
    num_injuries,
    injury_descriptions
  )








# =============================================
# HEAD INJURY ANALYSIS - DIAGNOSIS + AIS
# =============================================
# Merge the datasets on research_case_id
combined_data <- merge(diagnosis_reduced, AIS1, by = "research_case_id")

# Convert MAX_AIS_SEVERITY to ordered factor
combined_data$MAX_AIS_SEVERITY <- factor(combined_data$MAX_AIS_SEVERITY, 
                                         levels = 1:3, #should go till 6, but bruh, we only arrive at 3 
                                         ordered = TRUE)

# Create a composite diagnosis category
combined_data$diagnosis_category <- with(combined_data, {
  case_when(
    any_brain_injury == 1 ~ "Brain Injury",
    concussion == 1 ~ "Concussion",
    unconsciousness == 1 ~ "Unconsciousness",
    any_bleeding == 1 ~ "Intracranial Bleeding",
    any_fracture == 1 ~ "Skull Fracture",
    TRUE ~ "Other"
  )
})




# Create contingency table
severity_diagnosis_table <- table(combined_data$MAX_AIS_SEVERITY, 
                                  combined_data$diagnosis_category)

# Add margins and proportions
addmargins(severity_diagnosis_table)
round(prop.table(severity_diagnosis_table, margin = 1) * 100, 1) # Row percentages

# Chi-squared test for association
chisq_test <- chisq.test(severity_diagnosis_table)
print(chisq_test)




#PLOT
library(ggplot2)
library(viridis)

ggplot(as.data.frame(severity_diagnosis_table), 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Count", option = "C") +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  labs(title = "AIS Severity vs Diagnosis Categories",
       subtitle = paste("Chi-squared p-value:", format.pval(chisq_test$p.value, digits = 3)),
       x = "Diagnosis Category",
       y = "AIS Severity Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(ggalluvial)

# Prepare summarized data
alluvial_data <- as.data.frame(severity_diagnosis_table)
names(alluvial_data) <- c("AIS_Severity", "Diagnosis", "Freq")

ggplot(alluvial_data,
       aes(axis1 = AIS_Severity, axis2 = Diagnosis, y = Freq)) +
  geom_alluvium(aes(fill = Diagnosis), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("AIS Severity", "Diagnosis"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Flow Between AIS Severity and Diagnosis Categories",
       y = "Number of Cases") +
  theme_minimal()



# =============================================
# HEAD INJURY ANALYSIS - DIAGNOSIS + AIS
# =============================================
# Merge the datasets on research_case_id
combined_data <- merge(diagnosis_reduced, AIS1, by = "research_case_id")

# Convert MAX_AIS_SEVERITY to ordered factor
combined_data$MAX_AIS_SEVERITY <- factor(combined_data$MAX_AIS_SEVERITY, 
                                         levels = 1:3, #should go till 6, but bruh, we only arrive at 3 
                                         ordered = TRUE)

# Create a composite diagnosis category
combined_data$diagnosis_category <- with(combined_data, {
  case_when(
    any_brain_injury == 1 ~ "Brain Injury",
    concussion == 1 ~ "Concussion",
    unconsciousness == 1 ~ "Unconsciousness",
    any_bleeding == 1 ~ "Intracranial Bleeding",
    any_fracture == 1 ~ "Skull Fracture",
    TRUE ~ "Other"
  )
})




# Create contingency table
severity_diagnosis_table <- table(combined_data$MAX_AIS_SEVERITY, 
                                  combined_data$diagnosis_category)

# Add margins and proportions
addmargins(severity_diagnosis_table)
round(prop.table(severity_diagnosis_table, margin = 1) * 100, 1) # Row percentages

# Chi-squared test for association
chisq_test <- chisq.test(severity_diagnosis_table)
print(chisq_test)




#PLOT
library(ggplot2)
library(viridis)

ggplot(as.data.frame(severity_diagnosis_table), 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Count", option = "C") +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  labs(title = "AIS Severity vs Diagnosis Categories",
       subtitle = paste("Chi-squared p-value:", format.pval(chisq_test$p.value, digits = 3)),
       x = "Diagnosis Category",
       y = "AIS Severity Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(ggalluvial)

# Prepare summarized data
alluvial_data <- as.data.frame(severity_diagnosis_table)
names(alluvial_data) <- c("AIS_Severity", "Diagnosis", "Freq")

ggplot(alluvial_data,
       aes(axis1 = AIS_Severity, axis2 = Diagnosis, y = Freq)) +
  geom_alluvium(aes(fill = Diagnosis), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("AIS Severity", "Diagnosis"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Flow Between AIS Severity and Diagnosis Categories",
       y = "Number of Cases") +
  theme_minimal()









#########################################################à
# =============================================
# HEAD INJURY ANALYSIS - DIAGNOSIS + GCS
# =============================================
# Merge the datasets on research_case_id
combined_data_gcs <- merge(diagnosis_reduced, vitals, by = "research_case_id")

# Convert MAX_AIS_SEVERITY to ordered factor
combined_data$MAX_AIS_SEVERITY <- factor(combined_data$MAX_AIS_SEVERITY, 
                                         levels = 1:3, #should go till 6, but bruh, we only arrive at 3 
                                         ordered = TRUE)

# Create a composite diagnosis category
combined_data_gcs$diagnosis_category <- with(combined_data_gcs, {
  case_when(
    any_brain_injury == 1 ~ "Brain Injury",
    concussion == 1 ~ "Concussion",
    unconsciousness == 1 ~ "Unconsciousness",
    any_bleeding == 1 ~ "Intracranial Bleeding",
    any_fracture == 1 ~ "Skull Fracture",
    TRUE ~ "Other"
  )
})




# Create contingency table
severity_diagnosis_table_gcs <- table(combined_data_gcs$GCS_group, 
                                  combined_data_gcs$diagnosis_category)

# Add margins and proportions
addmargins(severity_diagnosis_table_gcs)
round(prop.table(severity_diagnosis_table_gcs, margin = 1) * 100, 1) # Row percentages

# Chi-squared test for association
chisq_test_gcs <- chisq.test(severity_diagnosis_table_gcs)
print(chisq_test_gcs)




#PLOT

ggplot(as.data.frame(severity_diagnosis_table_gcs), 
       aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Count", option = "C") +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  labs(title = "GCS Group vs Diagnosis Categories",
       subtitle = paste("Chi-squared p-value:", format.pval(chisq_test$p.value, digits = 3)),
       x = "Diagnosis Category",
       y = "GCS Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(ggalluvial)

# Prepare summarized data
alluvial_data <- as.data.frame(severity_diagnosis_table_gcs)
names(alluvial_data) <- c("GCS Group", "Diagnosis", "Freq")

ggplot(alluvial_data,
       aes(axis1 = `GCS Group`, axis2 = Diagnosis, y = Freq)) +
  geom_alluvium(aes(fill = Diagnosis), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("GCS Group", "Diagnosis"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Flow Between GCS Group and Diagnosis Categories",
       y = "Number of Cases") +
  theme_minimal()



