# =====================================
# Script: Final Cancer and Population Data Merger
# Author: Ke Pang
# Date: 2025-08-12
# Purpose: Merge cancer incidence/mortality with population/mortality data for lifetime risk calculations
# Output: Final dataset with ISOcode, AgeStart, AgeEnd, Ri, Di, Ni, Mi
# =====================================

library(dplyr)

#1. Configuration - Change these parameters ----
CANCER_TYPE <- "rectum"  # Options: "colorectum" or "rectum"

# File paths
POPULATION_MORTALITY_FILE <- "Data/population data from UN/population_mortality_combined_85.csv"
CANCER_INCIDENCE_FILE <- file.path("Data", CANCER_TYPE, "age_specific_incidence.csv")
CANCER_MORTALITY_FILE <- file.path("Data", CANCER_TYPE, "age_specific_mortality.csv")

# Output file
OUTPUT_FILE <- file.path("Data", CANCER_TYPE,paste0("2024_", CANCER_TYPE, "_data.csv"))

#2. Print configuration ----
cat("=== FINAL DATA MERGING CONFIGURATION ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Population/Mortality File:", POPULATION_MORTALITY_FILE, "\n")
cat("Cancer Incidence File:", CANCER_INCIDENCE_FILE, "\n")
cat("Cancer Mortality File:", CANCER_MORTALITY_FILE, "\n")
cat("Output File:", OUTPUT_FILE, "\n")
cat("========================================\n\n")

#3. File validation ----
cat("Checking file availability...\n")

# List of required files
required_files <- c(
  POPULATION_MORTALITY_FILE,
  CANCER_INCIDENCE_FILE,
  CANCER_MORTALITY_FILE
)

missing_files <- c()
for (file in required_files) {
  if (!file.exists(file)) {
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  cat("ERROR: Missing files:\n")
  for (file in missing_files) {
    cat("  -", file, "\n")
  }
  stop("Please ensure all required files are present before running the script.")
}
cat("All files found! ✓\n\n")

#4. Data loading ----
cat("Loading data files...\n")

# Load population and mortality data (Ni, Mi)
pop_mort_data <- read.csv(POPULATION_MORTALITY_FILE, stringsAsFactors = FALSE)
cat("  Population/Mortality data loaded:", nrow(pop_mort_data), "records\n")

# Load cancer incidence data (Ri)
cancer_inc_data <- read.csv(CANCER_INCIDENCE_FILE, stringsAsFactors = FALSE)
cat("  Cancer incidence data loaded:", nrow(cancer_inc_data), "records\n")

# Load cancer mortality data (Di)
cancer_mort_data <- read.csv(CANCER_MORTALITY_FILE, stringsAsFactors = FALSE)
cat("  Cancer mortality data loaded:", nrow(cancer_mort_data), "records\n")

#5. Data validation ----
cat("Validating data structure...\n")

# Check required columns
required_pop_cols <- c("ISOcode", "AgeStart", "AgeEnd", "Ni", "Mi")
required_inc_cols <- c("ISOcode", "AgeStart", "AgeEnd", "Ri")
required_mort_cols <- c("ISOcode", "AgeStart", "AgeEnd", "Di")

# Validate population/mortality data
missing_pop_cols <- setdiff(required_pop_cols, colnames(pop_mort_data))
if (length(missing_pop_cols) > 0) {
  stop("Missing columns in population/mortality data: ", paste(missing_pop_cols, collapse = ", "))
}

# Validate cancer incidence data
missing_inc_cols <- setdiff(required_inc_cols, colnames(cancer_inc_data))
if (length(missing_inc_cols) > 0) {
  stop("Missing columns in cancer incidence data: ", paste(missing_inc_cols, collapse = ", "))
}

# Validate cancer mortality data
missing_mort_cols <- setdiff(required_mort_cols, colnames(cancer_mort_data))
if (length(missing_mort_cols) > 0) {
  stop("Missing columns in cancer mortality data: ", paste(missing_mort_cols, collapse = ", "))
}

cat("Data structure validation passed ✓\n\n")

#6. Data merging ----
cat("Merging datasets...\n")

# Step 1: Merge cancer incidence and mortality
cancer_data <- merge(
  cancer_inc_data[, c("ISOcode", "AgeStart", "AgeEnd", "Ri")],
  cancer_mort_data[, c("ISOcode", "AgeStart", "AgeEnd", "Di")],
  by = c("ISOcode", "AgeStart", "AgeEnd"),
  all = TRUE  # Outer join to keep all cancer data
)
cat("  Cancer incidence and mortality merged:", nrow(cancer_data), "records\n")

# Step 2: Merge with population/mortality data
final_data <- merge(
  pop_mort_data[, c("ISOcode", "AgeStart", "AgeEnd", "Ni", "Mi")],
  cancer_data,
  by = c("ISOcode", "AgeStart", "AgeEnd"),
  all.x = FALSE,  # Keep only regions that have population data
  all.y = TRUE    # Keep all cancer data, even if no population match
)
cat("  Final merge completed:", nrow(final_data), "records\n")

# Handle missing values - replace NA with 0 for cancer data
final_data$Ri[is.na(final_data$Ri)] <- 0
final_data$Di[is.na(final_data$Di)] <- 0

cat("  Missing values handled (NA -> 0 for Ri, Di)\n")

#7. Data quality checks ----
cat("Performing data quality checks...\n")

# Check for missing population/mortality data
missing_pop_mort <- sum(is.na(final_data$Ni) | is.na(final_data$Mi))
if (missing_pop_mort > 0) {
  cat("  Warning:", missing_pop_mort, "records with missing population/mortality data\n")
}

# Check for negative values
negative_ri <- sum(final_data$Ri < 0, na.rm = TRUE)
negative_di <- sum(final_data$Di < 0, na.rm = TRUE)
negative_ni <- sum(final_data$Ni < 0, na.rm = TRUE)
negative_mi <- sum(final_data$Mi < 0, na.rm = TRUE)

if (negative_ri > 0) cat("  Warning:", negative_ri, "negative Ri values\n")
if (negative_di > 0) cat("  Warning:", negative_di, "negative Di values\n")
if (negative_ni > 0) cat("  Warning:", negative_ni, "negative Ni values\n")
if (negative_mi > 0) cat("  Warning:", negative_mi, "negative Mi values\n")

# Sort final data
final_data <- final_data %>%
  arrange(ISOcode, AgeStart, AgeEnd) %>%
  select(ISOcode, AgeStart, AgeEnd, Ri, Di, Ni, Mi)  # Ensure column order

cat("  Data quality checks completed ✓\n")

#8. Save results ----
cat("Saving final dataset...\n")

# Create output directory if it doesn't exist
if (!dir.exists(dirname(OUTPUT_FILE))) {
  dir.create(dirname(OUTPUT_FILE), recursive = TRUE)
}

# Save final merged data
write.csv(final_data, OUTPUT_FILE, row.names = FALSE)

#9. Summary ----
cat("\n=== FINAL MERGING COMPLETE ===\n")
cat("Cancer Type:", CANCER_TYPE, "\n")
cat("Total records:", nrow(final_data), "\n")
cat("Regions/Countries:", length(unique(final_data$ISOcode)), "\n")
cat("Age groups:", length(unique(paste(final_data$AgeStart, final_data$AgeEnd, sep="-"))), "\n")
cat("Age range:", min(final_data$AgeStart), "-", max(final_data$AgeEnd), "\n")
cat("Output saved to:", OUTPUT_FILE, "\n")
cat("==============================\n")

# Show sample of results
cat("\nSample of final dataset:\n")
print(head(final_data, 10))

# Show summary statistics
cat("\nSummary statistics:\n")
cat("Incidence (Ri): Min =", min(final_data$Ri), ", Max =", max(final_data$Ri), ", Mean =", round(mean(final_data$Ri), 2), "\n")
cat("Cancer Mortality (Di): Min =", min(final_data$Di), ", Max =", max(final_data$Di), ", Mean =", round(mean(final_data$Di), 2), "\n")
cat("Population (Ni): Min =", min(final_data$Ni, na.rm = TRUE), ", Max =", max(final_data$Ni, na.rm = TRUE), ", Mean =", round(mean(final_data$Ni, na.rm = TRUE), 2), "\n")
cat("All-cause Mortality (Mi): Min =", min(final_data$Mi, na.rm = TRUE), ", Max =", max(final_data$Mi, na.rm = TRUE), ", Mean =", round(mean(final_data$Mi, na.rm = TRUE), 2), "\n")

# Show regions coverage
regions_with_complete_data <- final_data %>%
  filter(!is.na(Ni) & !is.na(Mi) & Ri >= 0 & Di >= 0) %>%
  pull(ISOcode) %>%
  unique() %>%
  length()

cat("Regions with complete data:", regions_with_complete_data, "\n")