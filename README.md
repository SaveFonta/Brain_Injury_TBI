# 🧠 Traumatic Brain Injury in Polytrauma Patients: Statistical Analysis

## 📌 Summary

This repository presents the results of a collaborative analysis between the **ETH Zürich MSc Statistics program** and the **Department of Traumatology at the University Hospital Zurich**. The goal is to understand how **Traumatic Brain Injury (TBI)** influences the broader **pathophysiology in polytrauma patients**, using real-world hospital data.

Over **7,000 anonymized patient records** from 2012 to 2020 were analyzed using:

- Generalized Linear Mixed Models (GLMMs)
- Robust linear models

The analysis revealed a **strong association between TBI severity (measured by Glasgow Coma Scale)** and key physiological disruptions, such as:

- Circulatory shock
- Coagulopathy
- Hypothermia
- Soft tissue damage

The results provide **clinical insights and actionable recommendations** for trauma management and future research.

---

## 📂 Repository Structure

```text
.
├── 01_raw_data/              # Raw input data (user must provide manually)
│   └── .gitkeep
├── 02_clean_data/           # Cleaned datasets (produced by preprocessing scripts)
│   └── .gitkeep
├── 03_spreadsheets/         # Temporary spreadsheet path (used by 01_clean_diagnosen+mapping.Rmd)
│   └── .gitkeep
├── 04_cleaning_notebooks/   # Data cleaning RMarkdown notebooks
│   ├── 01_clean_diagnosen+mapping.Rmd
│   ├── 01_clean_iss+interventions.Rmd
│   ├── 01_clean_vital+lab+patients.Rmd
│   ├── 01_table4_01.Rmd
│   ├── 03_table1_01.Rmd
│   └── master_data_prep.R
├── 05_Exploration/          # Exploratory data analysis and issue identification
│   ├── 02_data_issues_02.Rmd
│   ├── 02_explore_Inter_AIS.R
│   ├── 02_explore_data_02.Rmd
│   └── Random_diagnosis_evaluation.R
├── 06_Models/               # Statistical modeling and correlation analysis
│   ├── 04_OLD_explore_circ_temp_inr.Rmd
│   ├── 04_Quick_Temp_Models.R
│   ├── 04_Soft_Tissue_Damage.Rmd
│   ├── 04_models_pph_overall_01.Rmd
│   ├── 04_sf_ordinal.Rmd
│   ├── data_prep.R
│   ├── models_references.bib
│   ├── pathophysiology_correlations_03.Rmd
│   └── vancouver.csl
├── 07_Report/               # Final project report and bibliographic files
│   ├── Report_final.Rmd
│   ├── Report_final_html.Rmd
│   ├── Report_final_pdf.Rmd
│   ├── report_references.bib
│   └── vancouver.csl
├── .gitignore
├── Brain_Injury.Rproj       # RStudio project file
