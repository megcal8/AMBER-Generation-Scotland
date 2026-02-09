# Main script for calling all other scripts
# There are better tools for workflows eg. nextflow and snakemake
# but let's learn one thing at a time :-)

# ---------------------------------------
# Load in packages and any functions needed
library(here) # this package helps with relative file paths
source(here::here("Scripts/00_startup.R"))

# ---------------------------------------
# Read in the separate data files and merge them into one dataframe
source(here::here("Scripts/01_get_data.R"))

# Create a named list of the file paths to read into a merged dataframe
# Specify the version date, which is the prefix for the folder name
# (this version variable is also used at the end of this script for saving processed data)
version <- "11_November"
file_paths <- list(
  all = here(glue("GenScotDepression/data/AMBER_questionnaire/{version}_AMBER_Responses/AMBER_antidepressants.csv")),
  general = here(glue("GenScotDepression/data/AMBER_questionnaire/{version}_AMBER_Responses/AMBER_general.csv"))
)

# Create a named list of columns to keep
# the names must be the same as in the file_paths list above
# if you want to keep all the columns then specify TRUE
cols_to_keep <- list(
  all = TRUE,
  general = c("GsId", "SurveyResponseId", "AMBER_treatments_nonad_ect")
)

# Run the function sourced from 01_get_data.R to read in the data and
# merge into one dataframe
questionnaire_data <- main_get_data(file_paths, cols_to_keep, "GsId")

# ---------------------------------------
# Create new columns:
# - Create a column for each of the 15 drugs called "ad_6_weeks_[drug]"
#   indicating if the person was on the drug for at least 6 weeks.
# - Create columns based on the collective drug data, this includes:
#   - The total number of switches between all drugs (n_switches_broad)
#   - The total number of switches between SSRI drugs (n_switches_broad_SSRI)
#   - The total number of switches between all drugs excluding missing data (n_switches_missing_broad)
#   - The total number of switches between SSRI drugs excluding missing data (n_switches_missing_SSRI)
#   - Whether any drug was effective or not (any_ad_effective_broad)
#   - Whether any SSRI drug was effective or not (any_ad_effective_SSRI)
# These above columns are used to classify responders/non-responders.
# We may want to classify people based on all the drugs, or we may be interested in
# classification based on SSRIs only.
# This is why we loop over different drug vectors, creating cols with different
# suffixes. eg. "n_switches_missing_broad" or "n_switches_missing_SSRI"

# Create variable with short names for each drug
all_drugs <- c(
  "cit", "par", "esc", "flu", "ser", "ven",
  "dul", "ami", "mir", "tra", "clo", "dos", "nor", "lof", "imi"
)
length(all_drugs) == 15

ssri_drugs <- c(
  "cit", "par", "esc", "flu", "ser"
)

# Source functions needed to make above columns
source(here::here("Scripts/02_create_new_cols.R"))

# Some people put a character string in the AMBER_ad_time_[med] columns
# these columns are answers to questions about how long the person was on a drug
# Get the number of people that put a character string in the AMBER_ad_time_[med] columns
ad_time_n_character(questionnaire_data)
# We will check these character strings manually assisted by the program below

# --------
# This program will create or update a file called "AMBER_ad_time_checks.csv"
# This csv file contains columns "GsId", "program_ad_time_string_[drug]",
# and "program_ad_time_6_weeks_[drug]".
# Data is in wide format, ie. 1 row per participant. So multiple columns for different drugs.
# The "program_ad_time_6_weeks_[drug]" columns contains TRUE or FALSE for if the character string
# is >= 6 weeks or not, "UNKNOWN" if it was difficult to determine and "NOT CHECKED" if it has not been checked yet.

# The column "program_ad_time_6_weeks" is then used in 02_create_new_cols.R
# in the create_n_switches(), create_n_switches_missing() and detect_switch()
# functions.

# Load functions in that runs this program
source(here::here("Scripts/AMBER_ad_time_program/main.R"))

# Set up paths to temporary and datastore csv files
csv_path_tmp <- here::here("AMBER_ad_time_checks.csv")
csv_path_datastore <- here::here("GenScotDepression/data/AMBER_questionnaire/processed_data/AMBER_ad_time_checks.csv")

# Start the AMBER_ad_time_program to manually choose whether a time string 
# equates to >= 6 weeks (T) or not (F) or if you're not sure for unknown (U).
# The program will save the results to a temporary csv file called 
# "AMBER_ad_time_checks.csv", after each individual is checked.
# On completion of the program, or when exiting (Q) the temporary csv file is 
# moved to datastore.
main_AMBER_ad_time_program(
  csv_path_tmp,
  csv_path_datastore,
  questionnaire_data,
  all_drugs
)

# Ensure tmp intermediate "AMBER_ad_time_checks.csv" file is moved to DataStore
# this is done when exiting or completing the program. But can be manually run
# here, eg. if the program was interrupted or your computer crashes.
move_ad_time_checks(
  csv_path_tmp,
  csv_path_datastore
)

# Merge in the AMBER_ad_time_checks.csv file
# Error if the file does not exist
csv_path <- here::here("GenScotDepression/data/AMBER_questionnaire/processed_data/AMBER_ad_time_checks.csv")
if (!file.exists(csv_path)) {
  stop("The file AMBER_ad_time_checks.csv does not exist. Please run the AMBER_ad_time_program first.")
}

ad_program_dataframe <- read.csv(csv_path, stringsAsFactors = FALSE)

# Check there's no duplicate IDs (duplicates suggest it's not true wide format)
ad_program_dataframe %>%
  filter(duplicated(GsId) | duplicated(GsId, fromLast = TRUE)) %>%
  nrow() == 0

# Check if there's any more strings to process
any(grepl("NOT CHECKED", as.matrix(ad_program_dataframe)))

# Get IDs with "NOT CHECKED" in any of the AMBER_ad_time_[med] columns
rows <- apply(ad_program_dataframe, 1, function(row) any(grepl("NOT CHECKED", row)))
ad_program_dataframe[rows, ]$GsId

# Full join the questionnaire_data with the ad_program_dataframe
questionnaire_data <- dplyr::full_join(
  questionnaire_data,
  ad_program_dataframe,
  by = "GsId"
)

# Check again there's no duplicate IDs (duplicates suggest it's not true wide format)
questionnaire_data %>%
  filter(duplicated(GsId) | duplicated(GsId, fromLast = TRUE)) %>%
  nrow() == 0

# --------

# Create cols for each drug if person was on the drug >= 6 weeks
# (needed for defining responder)
# Create cols for each drug if person was on the drug >= 6 weeks
# (needed for defining responder)
for (drug in all_drugs) {
  program_col <- paste0("program_ad_time_6_weeks_", drug)
  
  # Calculate from raw data for everyone first
  ad_time_drug <- questionnaire_data[[paste0("AMBER_ad_time_", drug)]]
  ad_6_weeks <- sapply(ad_time_drug, function(ad_time_string) {
    more_than_6_weeks(ad_time_string)
  })
  
  # Then overwrite with program values where they exist and are not NA
  if (program_col %in% names(questionnaire_data)) {
    program_values <- questionnaire_data[[program_col]]
    # Only use program values where they're not NA
    ad_6_weeks[!is.na(program_values)] <- program_values[!is.na(program_values)]
  }
  
  questionnaire_data[[paste0("ad_6_weeks_", drug)]] <- ad_6_weeks
}

# Loop over all drugs and SSRIs separately to create columns with different suffixes
# There's definitely a more efficient way of doing this, future refactoring task.

drugs_list <- setNames(
  list(all_drugs, ssri_drugs),
  c("broad", "ssri")
)

# The following takes a little while to run, this will take longer as the number 
# of people/rows increases
mapply(function(drug_data, drug_type) {
  col_data <- get_column_data(drug_data, questionnaire_data)
  questionnaire_data[[paste0("n_switches_", drug_type)]] <<- col_data$n_switches
  questionnaire_data[[paste0("n_switches_missing_", drug_type)]] <<- col_data$n_switches_missing
  questionnaire_data[[paste0("any_ad_effective_", drug_type)]] <<- col_data$any_ad_effective
  colnames(questionnaire_data)
  beepr::beep(1) # Make a noise when mapply has finished running, beep(4) and beep(6) are alarming!
}, drugs_list, names(drugs_list))

# ---------------------------------------
# Check columns we have created and ones we are using in their raw state
# calculate counts of TRUE/FALSE/NA for each column
source(here::here("Scripts/03_check_cols.R"))

# ---------------------------------------
# Classify individuals as responders or non-responders
source(here::here("Scripts/04_classification.R"))

# Create new columns for each drug that indicates if the person is a responder or non-responder
# split by all drugs and SSRIs only
for (drug in all_drugs) {
  for (drug_type in names(drugs_list)) {
    questionnaire_data[[paste0("responder_", drug, "_", drug_type)]] <-
      responds_to_drug(questionnaire_data, drug, drug_type)
  }
}

# Create a new column combining response to all drugs to determine final classification
mapply(function(drug_data, drug_type) {
  questionnaire_data[[paste0("responder_", drug_type)]] <<- apply(
    questionnaire_data[, paste0("responder_", drug_data, "_", drug_type)],
    1,
    function(x) {
      if (any(x == "responder", na.rm = TRUE)) {
        "responder"
      } else if (any(x == "non-responder", na.rm = TRUE)) {
        "non-responder"
      } else {
        "other"
      }
    }
  )
}, drugs_list, names(drugs_list))

for (drug_type in names(drugs_list)) {
  questionnaire_data[[paste0("responder_", drug_type)]] <- apply(
    questionnaire_data %>%
      select(matches(paste0("^responder_.*_", drug_type, "$"))),
    1, # apply over each row:
    function(x) {
      if (any(x == "responder", na.rm = TRUE) &&
        !any(x == "non-responder", na.rm = TRUE)) {
        "responder"
      } else if (!any(x == "responder", na.rm = TRUE) &&
        any(x == "non-responder", na.rm = TRUE)) {
        "non-responder"
      } else {
        "other"
      }
    }
  )
}

# Get union of responder_broad and responder_ssri
# this would give us "responders who respond to SSRIs" and "non-responders who did not respond to SSRIs"
# whilst still ensuring data on all the drugs is included. I think...

questionnaire_data$responder_union <- sapply(seq_len(nrow(questionnaire_data)), function(i) {
  ssri <- questionnaire_data$responder_ssri[i]
  broad <- questionnaire_data$responder_broad[i]
  
  # Check if they took any non-SSRI medications
  non_ssri_drugs <- c("ven", "dul", "ami", "mir", "tra", "clo", "dos", "nor", "lof", "imi")
  took_non_ssri <- any(
    questionnaire_data[i, paste0("AMBER_ad_past_", non_ssri_drugs)] == 1 |
      questionnaire_data[i, paste0("AMBER_ad_lastyear_", non_ssri_drugs)] == 1 |
      questionnaire_data[i, paste0("AMBER_ad_current_", non_ssri_drugs)] == 1,
    na.rm = TRUE
  )
  
  # If both classifications agree
  if (ssri == "responder" && broad == "responder") {
    "responder"
  } else if (ssri == "non-responder" && broad == "non-responder") {
    "non-responder"
  }
  # If SSRI responder and never took non-SSRIs
  else if (ssri == "responder" && !took_non_ssri) {
    "responder"
  }
  # If broad responder and never took SSRIs
  else if (broad == "responder" && ssri == "other") {
    ssri_drugs_check <- c("cit", "par", "esc", "flu", "ser")
    took_ssri <- any(
      questionnaire_data[i, paste0("AMBER_ad_past_", ssri_drugs_check)] == 1 |
        questionnaire_data[i, paste0("AMBER_ad_lastyear_", ssri_drugs_check)] == 1 |
        questionnaire_data[i, paste0("AMBER_ad_current_", ssri_drugs_check)] == 1,
      na.rm = TRUE
    )
    if (!took_ssri) "responder" else "other"
  }
  # If one is non-responder, they're non-responder overall
  else if (ssri == "non-responder" || broad == "non-responder") {
    "non-responder"
  }
  else {
    "other"
  }
})

# ---------------------------------------
# QC classification
source(here::here("Scripts/05_QC_classification.R"))
# look at responder_union counts for final counts

# ---------------------------------------
# Save the questionnaire_data data frame with new columns
dir.create(here("GenScotDepression/data/AMBER_questionnaire/processed_data"))
write.csv(questionnaire_data, here(glue("GenScotDepression/data/AMBER_questionnaire/processed_data/processed_AMBER_{version}.csv")),
  row.names = FALSE
)
