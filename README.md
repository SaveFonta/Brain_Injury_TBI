# 🧠 Traumatic Brain Injury in Polytrauma Patients: Statistical Analysis

## 📚 Table of Contents
- [📌 Summary](#-summary)
- [📂 Repository Structure](#-repository-structure)
- [📄 Quick Access: Final Report (PDF)](#-quick-access-final-report-pdf)
- [🚀 Getting Started](#-getting-started)
  - [📥 How to Add Your Data](#-how-to-add-your-data)
  - [📊 About `03_spreadsheets/`](#-about-03_spreadsheets)
  - [🛠️ Requirements](#-requirements)
  - [🧹 Data Cleaning Workflow](#-data-cleaning-workflow)
  - [🔍 05_Exploration/](#-05_exploration)
  - [🧠 06_Models/](#-06_models)
  - [📄 07_Report/ — Final Results](#-07_report--final-results)


  
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
├── 02_data/   # Data to use
│   ├── 01_raw_data
│   └── 02_clean_data                    
│  
├── 03_spreadsheets         # Temporary spreadsheet path (used by 01_clean_diagnosen+mapping.Rmd)
│ 
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

```

---

## 📄 Quick Access: Final Report (PDF)

If you prefer not to run any code and just want to view the final results, you can access the fully compiled report here:

👉 [**Download Final Report (PDF)**](https://github.com/SaveFonta/Brain_Injury_TBI/raw/main/07_Report/Report_final.pdf)

This document includes all major findings, methodology, statistical models, and clinical interpretations from the project.

---


# 🚀 Getting Started

To explore or reproduce the project, follow these steps:

---

## 📥 How to Add Your Data

The required patient-level dataset is **not included** in this repository due to privacy and confidentiality restrictions. The user must **manually insert** the data into the `02_data/01_raw_data` directory.

### 🖼️ Expected Data Structure

The following diagram illustrates the expected naming and format of input data files:

![Structure](https://github.com/SaveFonta/Brain_Injury_TBI/blob/main/Images/structure.png?raw=true)

> ℹ️ You must match these filenames and structures for the cleaning scripts to run correctly.

--

## 📊 About `03_spreadsheets/`

The folder `03_spreadsheets/` is used within the cleaning scripts `01_clean_diagnosen+mapping.Rmd` and `01_clean_vital+lab+patients.Rmd` as a **temporary inspection path**. You **do not** need to manually add or modify anything inside this folder.

---

## 🛠️ Requirements

To run the analysis, ensure you have the following:

- R (≥ 4.0)
- RStudio
- Required R packages (see individual `.Rmd` files for `library()` calls)

## Package Installation

Run the following code in your R console to install all necessary packages:

```r
install.packages(c(
  # Data Manipulation
  "data.table", "dplyr", "tidyr", "tidyverse", "rio", "stringi", "strata",
  
  # Statistical Modeling
  "MASS", "car", "robustbase", "lme4", "lmerTest", "glmmTMB", 
  "ordinal", "poLCA", "logistf", "sfsmisc",
  
  # Visualization
  "ggplot2", "vcd", "eulerr", "lemon",
  
  # Reporting & Tables
  "tableone", "knitr", "kableExtra", "tinytex",
  
  # Model Evaluation
  "pROC", "broom"
))
```

## Important Notes

### Load Order Matters  
**Always load `MASS` before `tidyverse`** to prevent function masking (especially `select()`).

```r
library(MASS)     # Load first
library(tidyverse) # Load after to ensure select() isn't masked
```

---

## 🧹 Data Cleaning Workflow

All raw data must undergo preprocessing before modeling. The cleaning process is documented in a series of RMarkdown notebooks located in the `04_cleaning_notebooks/` folder. These scripts handle tasks such as:

- Diagnoses processing (`Diagnosen.xlsx`)
- Intervention and ISS data cleaning
- Patient vitals, lab values, and demographic info

You can run individual scripts as needed (e.g., `01_clean_diagnosen+mapping.Rmd` for diagnoses), which will clean and save specific datasets. However, since the cleaning process is **not the main focus** of this project, we provide a **master script** that runs the entire cleaning pipeline:
`04_cleaning_notebooks/master_data_prep.R`


### ✅ Output Location

After running the master script, verify that the cleaned files are correctly saved in:
`02_data/02_clean_data`


---

## 🔍 05_Exploration/

The `05_Exploration/` folder contains various exploratory scripts we used to inspect and better understand the raw data. These analyses were helpful for identifying data inconsistencies, variable behavior, and modeling challenges.

> ℹ️ This section is **less critical** for drawing final conclusions. It was primarily used for internal data comprehension.  

---

## 🧠 06_Models/

> 📌 **Important:**  
Before running any model scripts, make sure to execute `06_Models/data_prep.R`.  
This script creates the **final cleaned patient subpopulations** used in the final report and saved in `02_data/02_clean_data`:

```r
population.rds
population_poly.rds
population_tbi.rds
population_poly_tbi.rds
```


The `06_Models/` folder also contains scripts and notebooks used to implement and test various statistical models, including:

- Generalized Linear Mixed Models (GLMMs)
- Robust regression models
- Correlation analyses related to pathophysiological outcomes

Users are welcome to explore these scripts for detailed implementation.  
However, **the final versions of the models and their results are fully documented in the report** provided in the next section.

---

## 📄 07_Report/ — Final Results

The most important deliverable of this project is the **final report**, which consolidates all key results, methodology, and interpretation.

The report is provided in the following formats:

- 📄 **PDF**: [`Report_final.pdf`](https://github.com/SaveFonta/Brain_Injury_TBI/blob/main/07_Report/Report_final.pdf)
- 🌐 **HTML**: [`Report_final_html.Rmd`](07_Report/Report_final_html.Rmd) *(knit to produce HTML)*
- ✍️ **Source RMarkdown**: `Report_final_pdf.Rmd`

📄 **Note:** This `.Rmd` file generates the PDF/HTML report. You must **knit** it in RStudio to produce the final output.

### 🧰 PDF Rendering Note

To knit the PDF report (`Report_final_pdf.Rmd`), you need to have LaTeX installed. We recommend using the lightweight **TinyTeX** distribution, which works well with RMarkdown.

You can install it by running:

```r
tinytex::install_tinytex()
```
---


