# master_data_prep.R

"Just run, this and enjoy all the cleaned df created!!"



# Define the order of R Markdown files to run 
rmarkdown_files <- c(
  "01_clean_diagnosen.Rmd",
  "01_clean_iss_interventions_01.Rmd",
  "01_clean_iss_interventions.Rmd",
  "01_clean_vital_lab_patients.Rmd",
  "01_clean_vital_lab_patients02.Rmd",
  "01_table4_01.Rmd",
  "03_table1_01.Rmd"
)

# Define the main project paths
import_path <- "02_data/01_raw_data/"
clean_path <- "02_data/02_clean_data/"

# Create clean directory if it doesn't exist
if (!dir.exists(clean_path)) {
  log_message(paste("Creating clean directory:", clean_path))
  dir.create(clean_path, recursive = TRUE)
  log_message(paste("Clean directory created successfully"))
} else {
  log_message(paste("Clean directory already exists:", clean_path))
}

# Define log file name
log_file <- file.path(clean_path, "data_cleaning_log.txt")

# Initialize log file
write("DATA CLEANING PROCESS LOG\n", file = log_file)

# Enhanced logging function with message types
log_message <- function(message, type = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste(timestamp, "-", type, "-", message, "\n")
  cat(log_entry) # Print to console
  write(log_entry, file = log_file, append = TRUE)
}

# Function to verify expected output files
verify_output_files <- function(rmd_file) {
  # Define expected output patterns for each Rmd (customize as needed)
  expected_outputs <- list(
    "01_clean_diagnosen.Rmd" = "clean_diagnosen.*\\.(RData|rds|csv)",
    "01_clean_iss_interventions_01.Rmd" = "clean_interventions.*\\.(RData|rds|csv)",
    # Add patterns for all your Rmd files
    "default" = ".*\\.(RData|rds|csv)" # Default pattern if not specified
  )
  
  pattern <- expected_outputs[[rmd_file]] %||% expected_outputs$default
  output_files <- list.files(clean_path, pattern = pattern, full.names = TRUE)
  
  if (length(output_files) > 0) {
    log_message(paste("Verified output files created by", rmd_file, ":", 
                      paste(basename(output_files), collapse = ", ")), "SUCCESS")
    return(TRUE)
  } else {
    log_message(paste("WARNING: No expected output files found for", rmd_file), "WARNING")
    return(FALSE)
  }
}

# Enhanced render function with file verification
render_rmd <- function(rmd_file) {
  input_file <- file.path(getwd(), "04_cleaning_notebooks", rmd_file)
  
  if (!file.exists(input_file)) {
    log_message(paste("File not found:", input_file), "ERROR")
    return(FALSE)
  }
  
  log_message(paste("Starting render of:", rmd_file), "INFO")
  log_message(paste("Input path:", input_file), "DEBUG")
  log_message(paste("Output directory:", clean_path), "DEBUG")
  
  start_time <- Sys.time()
  
  result <- tryCatch({
    render_result <- rmarkdown::render(
      input = input_file,
      output_dir = clean_path,
      params = list(
        import_path = import_path,
        clean_path = clean_path
      ),
      envir = new.env(),
      quiet = FALSE
    )
    
    log_message(paste("Render completed successfully:", render_result), "SUCCESS")
    log_message(paste("Render time:", 
                      format(round(Sys.time() - start_time, 2))), "INFO")
    
    # Verify output files were created
    file_verification <- verify_output_files(rmd_file)
    
    return(TRUE && file_verification)
  }, 
  error = function(e) {
    log_message(paste("Render failed:", e$message), "ERROR")
    return(FALSE)
  },
  warning = function(w) {
    log_message(paste("Render warning:", w$message), "WARNING")
    return(TRUE) # Continue despite warnings
  })
  
  return(result)
}

# Log system information at start
log_message(paste("R version:", R.version$version.string), "SYSTEM")
log_message(paste("System:", Sys.info()["sysname"], Sys.info()["release"]), "SYSTEM")
log_message(paste("Working directory:", getwd()), "SYSTEM")
log_message("Starting data cleaning process...", "INFO")

# Initialize tracking variables
successful_renders <- 0
failed_renders <- 0
warnings_encountered <- 0
process_start_time <- Sys.time()

# Loop through and render each R Markdown file
for (rmd in rmarkdown_files) {
  result <- render_rmd(rmd)
  
  if (isTRUE(result)) {
    successful_renders <- successful_renders + 1
  } else if (isFALSE(result)) {
    failed_renders <- failed_renders + 1
  } else {
    warnings_encountered <- warnings_encountered + 1
  }
}

# Final summary
process_duration <- Sys.time() - process_start_time
log_message("\nPROCESS SUMMARY:", "HEADER")
log_message(paste("Total files to process:", length(rmarkdown_files)), "SUMMARY")
log_message(paste("Successfully rendered:", successful_renders), "SUMMARY")
log_message(paste("Failed renders:", failed_renders), ifelse(failed_renders > 0, "WARNING", "SUMMARY"))
log_message(paste("Renders completed with warnings:", warnings_encountered), 
            ifelse(warnings_encountered > 0, "WARNING", "SUMMARY"))
log_message(paste("Total processing time:", format(round(process_duration, 2))), "SUMMARY")

# Final verification of all expected outputs
log_message("\nVERIFYING ALL OUTPUT FILES:", "HEADER")
all_output_files <- list.files(clean_path, pattern = "\\.(RData|rds|csv)$")
if (length(all_output_files) > 0) {
  log_message(paste("Found", length(all_output_files), "output files:"), "INFO")
  log_message(paste(all_output_files, collapse = "\n"), "INFO")
} else {
  log_message("WARNING: No output files found in clean directory!", "WARNING")
}

log_message("\nData cleaning process completed", "HEADER")
cat(paste("\nDetailed log saved to:", log_file, "\n"))