# ────────────────────────────────────────────────────────────────────────────────
# INITIAL SETUPS
# ────────────────────────────────────────────────────────────────────────────────
# for data import
import_path <- "02_data/01_raw_data/"
# for export of inspection objects (spreadsheets)
inspect_path <- "03_spreadsheets/"
# for export of clean data
clean_path <- "02_data/02_clean_data/"
# for intermediate results
intermediate_path <- "02_data/03_dump/"
# for tables
table_path <- "04_tables/"
# for figures
figure_path <- "05_figures/"



library(data.table)
library(rio)
library(here)  
library(tidyverse)






# ────────────────────────────────────────────────────────────────────────────────
# LOAD DATASETS
# ────────────────────────────────────────────────────────────────────────────────

# Load diagnosis data (TBI indicators)
load(paste0(clean_path, "Diagnosis_cleaned.RData"))
setDT(Diagnosis_cleaned)  # Convert to data.table for efficiency

# Load patient demographics and hospital stay data
load(paste0(clean_path, "patients_clean.RData"))

# Load admission vital signs and laboratory results
load(paste0(clean_path, "vitals_clean.RData"))
load(paste0(clean_path, "lab_clean.RData"))

# Load trauma scores and AIS region mapping
load(paste0(clean_path, "iss_clean.RData"))
load(paste0(clean_path, "mapping_cleaned.RData"))

# Load intervention data (wide format)
load(paste0(clean_path, "interventions1_wide.RData"))





# ────────────────────────────────────────────────────────────────────────────────
# BUILD pop DATASET
# ────────────────────────────────────────────────────────────────────────────────

# Convert each one if the diagnosis columns to integer (0/1)
diagnoses <- c("bleeding", "fracture", "concussion",
               "brain_edema", "brain_compression", "unconsciousness")
Diagnosis_cleaned[, (diagnoses) := lapply(.SD, \(x) as.integer(as.character(x))),
                  .SDcols = diagnoses]

# FIRST MERG:  diagnosis + patient demographic data
pop <- merge(
  Diagnosis_cleaned[, .(research_case_id, date, bleeding, fracture,
                        concussion, brain_edema, brain_compression, unconsciousness)],
  patients_clean[, .(research_case_id, age, elderly, sex, stay_days,
                     icu, icu_days, death, death_days)],
  by = "research_case_id",
  all.x = TRUE
)



# ────────────────────────────────────────────────────────────────────────────────
# DEFINE TBI STATUS
# ────────────────────────────────────────────────────────────────────────────────

# Flag TBI patients in 'pop'
pop[, tbi := as.integer(research_case_id %in% 
                          Diagnosis_cleaned[rowSums(.SD > 0) > 0, research_case_id]),
    .SDcols = diagnoses]



# ────────────────────────────────────────────────────────────────────────────────
# MERGE ADDITIONAL CLINICAL DATA
# ────────────────────────────────────────────────────────────────────────────────

# Add vitals, labs, and ISS data
pop <- merge(pop, vitals_clean, by = "research_case_id", all.x = TRUE)
pop <- merge(pop, lab_clean_wide, by = "research_case_id", all.x = TRUE)
pop <- merge(pop, iss_clean, by = "research_case_id", all.x = TRUE)


# ────────────────────────────────────────────────────────────────────────────────
# COMPUTE AIS BY REGION
# ────────────────────────────────────────────────────────────────────────────────

# Maximum AIS severity per region for each patient
ais <- mapping_cleaned[, .(ais = max(MAX_AIS_SEVERITY)),   #here Is where I trasform and just keep an AIS score for each person.
                       by = .(research_case_id, AIS_CHAPTER)]

# Map numeric AIS_CHAPTER codes to region names
ais_regions <- data.table(
  AIS_CHAPTER = -1:9,
  ais_region = c("No_Map", "Other_Trauma", "Head", "Face", "Neck",
                 "Thorax", "Abdomen", "Spine", "Upper_Extremity",
                 "Lower_Extremity", "Unspecified")
)

# Add region names to AIS data
ais <- merge(ais, ais_regions, by = "AIS_CHAPTER", all.x = TRUE)

# Convert to wide format: one column per AIS region
ais <- dcast(ais, research_case_id ~ ais_region, value.var = "ais")
ais <- ais[, mget(c("research_case_id", ais_regions$ais_region))]

# Merge region AIS into pop
pop <- merge(pop, ais, by = "research_case_id", all.x = TRUE)

#set NA to zero for those variables
pop$Head[is.na(pop$No_Map)] <- 0
pop$Head[is.na(pop$Other_Trauma)] <- 0
pop$Head[is.na(pop$Head)] <- 0
pop$Face[is.na(pop$Face)] <- 0
pop$Neck[is.na(pop$Neck)] <- 0
pop$Thorax[is.na(pop$Thorax)] <- 0
pop$Abdomen[is.na(pop$Abdomen)] <- 0
pop$Spine[is.na(pop$Spine)] <- 0
pop$Upper_Extremity[is.na(pop$Upper_Extremity)] <- 0
pop$Upper_Extremity[is.na(pop$Lower_Extremity)] <- 0
pop$Unspecified[is.na(pop$Unspecified)] <- 0






# ────────────────────────────────────────────────────────────────────────────────
# HANDLE MISSING DATA FLAGS
# ────────────────────────────────────────────────────────────────────────────────

pop[, `:=`(
  missing_vitals = as.integer(!(research_case_id %in% vitals_clean$research_case_id)),
  missing_labs   = as.integer(!(research_case_id %in% lab_clean_wide$research_case_id)),
  missing_ais    = as.integer(!(research_case_id %in% ais$research_case_id)),
  missing_iss    = as.integer(is.na(iss))
)]


# ────────────────────────────────────────────────────────────────────────────────
# DEFINE POLYTRAUMA
# ────────────────────────────────────────────────────────────────────────────────

# Count number of distinct regions with AIS > 0 (excluding 'Other Trauma')
polytrauma <- mapping_cleaned[
  MAX_AIS_SEVERITY > 0 & AIS_CHAPTER > 0,
  .(n_regions = uniqueN(AIS_CHAPTER)),
  by = research_case_id
]

# Merge and flag polytrauma cases (2+ regions affected)
pop <- merge(pop, polytrauma, by = "research_case_id", all.x = TRUE)
pop[is.na(n_regions), n_regions := 0]
pop[, polytrauma := as.integer(n_regions >= 2)]




# ────────────────────────────────────────────────────────────────────────────────
# CATEGORICAL VARIABLE CREATION
# ────────────────────────────────────────────────────────────────────────────────

# GCS categories as an ordered factor (severity increases)
pop[, gcs_cat := factor(
  x = case_when(
    is.na(gcs) ~ "0.unknown",
    gcs >= 13 ~ "1.mild",
    gcs >= 9 ~ "2.moderate",
    gcs <= 8 ~ "3.severe"
  ),
  levels = c("0.unknown", "1.mild", "2.moderate", "3.severe"),
  ordered = TRUE
)]



# Define categorical variables WITH ORDERING
# Creating 10-year age intervals for age categories
pop[, age_cat := factor(
  x = case_when(
    age < 30 ~ "1.<30",
    age < 40 ~ "2.30-39",
    age < 50 ~ "3.40-49",
    age < 60 ~ "4.50-59",
    age < 70 ~ "5.60-69", 
    age < 79 ~ "6.70-78",
    TRUE ~ "7.79+"
  ), 
  levels = c("1.<30", "2.30-39", "3.40-49", "4.50-59", "5.60-69", "6.70-78", "7.79+"),
  ordered = TRUE
)]




# ISS categories as an ordered factor (severity increases)
pop[, iss_cat := factor(
  x = case_when(
    iss <= 8 ~ "1.1-8",
    iss <= 15 ~ "2.9-15",
    iss <= 24 ~ "3.16-24",
    iss >= 25 ~ "4.25+"
  ),
  levels = c("1.1-8", "2.9-15", "3.16-24", "4.25+"),
  ordered = TRUE
)]


# ────────────────────────────────────────────────────────────────────────────────
# INVASIVE INTERVENTION IDENTIFICATION
# ────────────────────────────────────────────────────────────────────────────────

# Flag patients with any recorded intervention
interventions1_wide[, invasive := as.integer(
  rowSums(interventions1_wide[, .SD, .SDcols = !c(1, 2)]) > 0
)]

# Merge intervention data into pop
pop <- merge(pop,
             interventions1_wide[, .(research_case_id, invasive)],
             by = "research_case_id", all.x = TRUE)

# Assume missing intervention data = no intervention
pop[is.na(invasive), invasive := 0]

"ATTENZIONE!!!!!"
"Here we are putting invasive if any of the interventions was realized. Is it ok? Is interventions1_wide just
a df with invasive interventions?? I hope so"

"VALE: Levy and I have been using it like this. I think it's ok. Otherwise we would need to slightly adjust"

# ────────────────────────────────────────────────────────────────────────────────
# DATA TYPE CONVERSION
# ────────────────────────────────────────────────────────────────────────────────

# Convert binary/categorical variables to factors
pop[, `:=`(
  sex = factor(sex),
  invasive = factor(invasive),
  tbi = factor(tbi),
  polytrauma = factor(polytrauma),
  missing_vitals = factor(missing_vitals),
  missing_labs = factor(missing_labs),
  missing_ais = factor(missing_ais),
  missing_iss = factor(missing_iss)
)]


# Optional: Convert injuries to factors if needed
# pop[, (injuries) := lapply(.SD, factor), .SDcols = injuries]


"DEBATABLE:"
# Convert AIS regions to ordered factors
ais_regions <- c("Head", "Face", "Neck", "Thorax", "Abdomen", 
                 "Spine", "Upper_Extremity", "Lower_Extremity")
pop[, (ais_regions) := lapply(.SD, \(x) ordered(x, levels = 0:6)), 
    .SDcols = ais_regions] 



# ────────────────────────────────────────────────────────────────────────────────
# DATA VALIDATION AND SUBSET CREATION
# ────────────────────────────────────────────────────────────────────────────────

setkey(pop, research_case_id)

# Verify no duplicate cases
if (any(duplicated(pop$research_case_id))) {
  warning("Duplicate research_case_id found - investigate before analysis")
}



# Create subsets
subsets <- list(
  pop_poly = pop[polytrauma == 1],
  pop_tbi = pop[tbi == 1],
  pop_poly_tbi = pop[polytrauma == 1 & tbi == 1]
)

# Print summary information
cat("\nDataset dimensions:\n")
cat(sprintf("%-15s: %d rows × %d columns\n", "Full population", nrow(pop), ncol(pop)))
for (subset_name in names(subsets)) {
  cat(sprintf("%-15s: %d rows × %d columns\n", 
              subset_name, 
              nrow(subsets[[subset_name]]), 
              ncol(subsets[[subset_name]])))
}


"Why tf the number for pop_poly is not the same number as the Venn diagram??"

# Attach subsets to global environment 
list2env(subsets, envir = .GlobalEnv)


# ────────────────────────────────────────────────────────────────────────────────
# FINAL SAVES
# ────────────────────────────────────────────────────────────────────────────────
saveRDS(object = pop, file = "02_data/02_clean_data/population.rds")
saveRDS(object = pop_poly, file = "02_data/02_clean_data/population_poly.rds")
saveRDS(object = pop_tbi, file = "02_data/02_clean_data/population_tbi.rds")
saveRDS(object = pop_poly_tbi, file = "02_data/02_clean_data/population_poly_tbi.rds")



