# Script for the main decision tree logic, using raw data columns (columns
# starting with "AMBER_") and columns we created in 02_create_new_cols.R and
# main.R (columns do not start with "AMBER_")
#
# ------------------------------------------------------------------------------
# Function to classify responders and non-responders per drug
# questionnaire_data is the dataframe of all the questionnaire responses
# drug is 3 letter character string of the drug, eg. "cit" for "citalopram"
# drug_type is whether we are classifying based on all the drugs, or just SSRIs
#   drug_type only affects the n_siwtches_missing_* and any_ad_effective_* columns
responds_to_drug <- function(questionnaire_data, drug, drug_type) {
  case_when(
    # "responder" to drug if all of these conditions are met:
    # never had ECT, AMBER_treatments_nonad_ect == 0 (include missing? ie. -97)
    # n_switches == 0 (function to create this col in 02_create_new_cols.R, called in main.R)
    # any_ad_effective == TRUE (function to create this col in 02_create_new_cols.R, called in main.R)
    # AMBER_overall_[drug] == 2 (Yes, a lot)
    # ad_6_weeks_[drug] == TRUE (function to create this col in 02_create_new_cols.R, called in main.R)
    # AMBER_reason_stop[drug] == 1 OR drug %in% AMBER_ad_current (ie. currently taking the drug)
    questionnaire_data[[paste0("n_switches_missing_", drug_type)]] == 0 &
      questionnaire_data$any_ad_effective_broad == TRUE & 
      questionnaire_data[[paste0("AMBER_overall_", drug)]] == 2 &
      questionnaire_data[[paste0("ad_6_weeks_", drug)]] == TRUE &
      (questionnaire_data[[paste0("AMBER_reason_stop_", drug)]] == 1 | 
        questionnaire_data[[paste0("AMBER_ad_current_", drug)]] == 1) ~ "responder",

    # "non-responder" to drug if all of these conditions are met:
    # n_switches >= 2
    # any_ad_effective == FALSE
    # AMBER_overall_[drug] == 0 (No)
    questionnaire_data[[paste0("n_switches_missing_", drug_type)]] >= 2 &
      questionnaire_data$any_ad_effective_broad == FALSE &
      questionnaire_data[[paste0("AMBER_overall_", drug)]] == 0 ~ "non-responder",

    # return "other" for all other cases
    TRUE ~ "other"
  )
}
