# Script that creates columns needed for defining responsders & non-responders
# ------------------------------------------------------------------------------

require(dplyr)
require(stringr)

# Define function to check missing data and print to console.
# Missing data is defined as: checked the box for taking a specific drug
# but not answered the question of interest (ie. col_name_prefix)
# about that specific drug
# Missing data here does not include people that are not taking that specific drug
# So to define this we will check for a drug match in the columns:
# AMBER_ad_past, AMBER_ad_lastyear or AMBER_ad_current
# AND a missing value ("", NA or -97) in the column for the question
# about that specific drug
# Function returns IDs of people with missing data
check_missing_data <- function(data, col_name_prefix, drug) {
  message(paste0(col_name_prefix, drug, " missing data count:"))
  
  # Get full column name with drug
  full_col_name <- paste0(col_name_prefix, drug)
  
  if (!full_col_name %in% colnames(data)) {
    stop(paste0(full_col_name), " column not found in dataframe")
  }
  
  # Subset to rows where drug is present in
  # AMBER_ad_past, AMBER_ad_lastyear or AMBER_ad_current
  data_subset <- data %>%
    filter(
      str_detect(AMBER_ad_past, paste0("\\b", drug, "\\b")) |
        str_detect(AMBER_ad_lastyear, paste0("\\b", drug, "\\b")) |
        str_detect(AMBER_ad_current, paste0("\\b", drug, "\\b"))
    )
  
  # SPECIAL LOGIC FOR REASON_STOP: 
  # Don't flag as missing if they're currently taking the drug
  if (str_detect(col_name_prefix, "reason_stop")) {
    current_col <- paste0("AMBER_ad_current_", drug)
    
    # For reason_stop, exclude people currently taking the drug from missing data check
    data_drug_missing <- data_subset %>%
      filter(
        !!sym(full_col_name) %in% c(-97, "", NA) &
          (is.na(!!sym(current_col)) | !!sym(current_col) != 1)
      )
  } else if (str_detect(col_name_prefix, "ad_time")) {
    # SPECIAL LOGIC FOR AD_TIME:
    # Don't flag as missing if:
    # 1) They stopped THIS drug because it was working (reason_stop = 1) AND
    # 2) They are currently taking ANY medication (SSRI or non-SSRI)
    reason_col <- paste0("AMBER_reason_stop_", drug)
    all_drugs <- c("cit", "par", "esc", "flu", "ser", "ven", "dul", "ami", "mir", "tra", "clo", "dos", "nor", "lof", "imi")
    
    data_drug_missing <- data_subset %>%
      rowwise() %>%
      mutate(
        currently_taking_any = any(c_across(paste0("AMBER_ad_current_", all_drugs)) == 1, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(
        !!sym(full_col_name) %in% c(-97, "", NA) &
          !(!!sym(reason_col) == 1 & currently_taking_any == TRUE)
      )
  }
  else {
    # For other columns, use original logic
    data_drug_missing <- data_subset %>%
      filter(!!sym(full_col_name) %in% c(-97, "", NA))
  }
  
  n_taken_drug <- nrow(data_subset)
  n_missing <- nrow(data_drug_missing)
  
  message(paste0(
    n_taken_drug, " people have taken ", drug, ", ", n_missing, " of those have ",
    " missing data for ", full_col_name, "."
  ))
  
  message("\n")
  
  # Return the GS Ids for people who have taken that drug but have not
  # answered the question for col_name_prefix
  data_drug_missing %>%
    pull(GsId)
}

# ------------------------------------------------------------------------------
# Define if the drug counts towards a switch
# - on an antidepressant for > 6 weeks
# - reason for stopping is inefficacy ie. AMBER_reason_stop == 3
# 1 = i started feeling better; 2 = side effects; 3 = my symptoms were unchanged;
# 4 = my prescription was changed; 5 = other (with free text box in
# AMBER_stop_medname)

# Function that extracts the time period and value from the AMBER_ad_time_[med] column time string
extract_time_string <- function(ad_time_string) {
  # splitting the string by " = " to get a list of vectors
  ad_time_parts <- str_split(ad_time_string, " = ")
  # get the string before the " = "
  duration <- ad_time_parts[[1]][1]
  # get the string after the " = ", remove quotes and make numeric
  duration_length <- ad_time_parts[[1]][2] %>%
    str_remove_all(., '\\"')
  list("duration" = duration, "duration_length" = duration_length)
}

# Define a function that returns TRUE or FALSE for whether
# they were taking an antidepressant for more than or equal to 6 weeks
# function takes a character string from the AMBER_ad_time_[med] column
more_than_6_weeks <- function(ad_time_string) {
  ad_time_parts <- extract_time_string(ad_time_string)
  duration <- ad_time_parts$duration
  duration_length <- ad_time_parts$duration_length %>%
                      as.numeric()
  # Series of if else logic to determine if the duration is more than 6 weeks
  # if someone says Years assume this is always more than 6 weeks
  if (!is.na(duration) && duration == "Years") {
    n_weeks_6_plus <- TRUE
  } else if (!is.na(duration) && duration == "Months") {
    weeks <- duration_length * 4.345
    n_weeks_6_plus <- weeks >= 6
  } else if (!is.na(duration) && duration == "Days") {
    weeks <- duration_length / 7
    n_weeks_6_plus <- weeks >= 6
  } else {
    weeks <- duration_length
    n_weeks_6_plus <- weeks >= 6
  }

  # NOTE: if someone answered eg. "Months" and input a character string not a
  # number then this will show as NA as when a character is converted to numeric
  # it returns NA. This is why we assumed Years is always more than 6 weeks. as
  # many people wrote a character string when responding with "Years".
  # We also have AMBER_ad_time_program/main.R to go through string responses manually

  # It also turns out it's possible for participants to answer with multiple duration_lengths
  # for one drug, eg. "Years = \"1\" Weeks = \"2\"", so Years may come up in
  # the manual program in this case.

  n_weeks_6_plus
}

# ------------------------------------------------------------------------------
# How many people have put a character string in the AMBER_ad_time_[med] columns
ad_time_n_character <- function(questionnaire_data) {
  ad_time_cols <- names(questionnaire_data)[startsWith(names(questionnaire_data), "AMBER_ad_time_")] %>%
    str_subset(., "AMBER_ad_time_other", negate = TRUE)
  result <- lapply(ad_time_cols, function(col) {
    time_strings <- sapply(questionnaire_data[[col]], function(drug_col) {
      extract_time_string(drug_col)$duration_length
      })
    
    # Identify rows where the extracted string is non-numeric
    is_char <- sapply(time_strings, function(time_string) {
      is.na(suppressWarnings(as.numeric(time_string))) && !is.na(time_string)
    })

    # Return the IDs of people with character string responses
    questionnaire_data$GsId[is_char]
  })

  names(result) <- ad_time_cols
  result

  sapply(result, length)

  # Message of total number of people across all drug that put a character string answer
  message(
    "A total of ", length(unique(unlist(result))), " people answered with a",
    " character string in at least one AMBER_ad_time_[med] column."
  )
}

# ------------------------------------------------------------------------------
# Define a function that returns TRUE or FALSE for whether the reason for
# stopping was due to inefficacy (TRUE) or a different reason (FALSE) or the
# answer was missing (NA)
# function takes a character string from the AMBER_reason_stop_[med] column
define_inefficacy <- function(reason_stop) {
  case_when(
    reason_stop == 3 ~ TRUE,   # 3 = symptoms unchanged (ineffective)
    reason_stop %in% c(1, 2, 4, 5) ~ FALSE,  # Other valid reasons (felt better, side effects, prescription changed, other)
    reason_stop %in% c(-97, -98, -99, NA) ~ NA,  # Missing data
    TRUE ~ NA  # Catch any other unexpected values
  )
}

# ------------------------------------------------------------------------------
detect_switch <- function(id, program_ad_time_6_weeks, ad_time, reason_stop, questionnaire_data, drug) {
  duration_6_weeks <- more_than_6_weeks(ad_time)
  ineffective <- define_inefficacy(reason_stop)
  
  if (program_ad_time_6_weeks %in% c("TRUE", "FALSE")) {
    duration_6_weeks <- program_ad_time_6_weeks %>% switch(.,
                                                           "TRUE" = TRUE,
                                                           "FALSE" = FALSE
    )
  }
  
  # Check if currently taking this specific drug
  current_col <- paste0("AMBER_ad_current_", drug)
  is_current <- questionnaire_data %>%
    filter(GsId == id) %>%
    pull(!!sym(current_col)) == 1
  
  if (length(is_current) == 0) is_current <- FALSE
  
  switch <- case_when(
    duration_6_weeks == TRUE & ineffective == TRUE ~ TRUE,
    duration_6_weeks == FALSE & ineffective == FALSE ~ FALSE,
    duration_6_weeks == TRUE & ineffective == FALSE ~ FALSE,
    duration_6_weeks == FALSE & ineffective == TRUE ~ FALSE,
    # Only treat missing stop reason as "no switch" if CURRENTLY taking this drug
    duration_6_weeks == TRUE & is.na(ineffective) & is_current ~ FALSE,
    !(duration_6_weeks %in% c(TRUE, FALSE)) | is.na(ineffective) ~ NA
  )
  switch
}

# Define a function that counts the total number of switches over all drugs
# for each individual.
# This function loops over each individual
# then for each individual it loops over each drug
# it returns TRUE/FALSE/NA vector from detect_switch()
# then sums the TRUE values to get the total number of switches across all drugs

create_n_switches <- function(questionnaire_data, drugs) {
  # Define a vector of GS IDs
  GsId <- questionnaire_data$GsId

  # first loop over each person
  all_switches <- lapply(GsId, function(id) {
    # for each person, loop over each drug and return if drug is a switch
    switches <- sapply(drugs, function(drug) {
      ad_time <- questionnaire_data %>%
        filter(GsId == id) %>%
        pull(paste0("AMBER_ad_time_", drug))

      # It is possible that not all the drugs have the program_ad_time_6_weeks column,
      # ie. when we ran the manual program to process ad_time_strings to determine 
      # if they were >= 6 weeks or not, some people did not have character strings
      # for some of the drugs. Therefore no column was made for that drug
      # The following section checks if the column exists for the drug in question,
      # and if it does it will pull the value from that column for that person
      # otherwise it will return NA
      program_ad_time_6_weeks <- questionnaire_data %>%
        filter(GsId == id) %>%
        {
          if (paste0("program_ad_time_6_weeks_", drug) %in% names(.)) pull(., paste0("program_ad_time_6_weeks_", drug)) else NA
        }

      reason_stop <- questionnaire_data %>%
        filter(GsId == id) %>%
        pull(paste0("AMBER_reason_stop_", drug))

      detect_switch(
        id,
        program_ad_time_6_weeks,
        ad_time,
        reason_stop,
        questionnaire_data,  # ADD THIS
        drug                 # ADD THIS
      )
    })
    # return a value NA, TRUE or FALSE for each drug
    switches
  })

  # for each person sum all the drugs to get the total number of switches
  n_switches <- sapply(all_switches, function(switch) {
    sum(unlist(switch), na.rm = TRUE)
  })

  n_switches
}

# NOTE: Number of total switches equal to 0 may be because of missing data
# Therefore we will also create a column called n_switches_missing using the
# definition of missing defined further up in "check_missing_data"
# it will put NA in the n_switches_missing column, as we wont know if that
# missing data meant that drug contributed to a "switch" or not so it's
# ambiguous to count the total number of switches for that person

create_n_switches_missing <- function(questionnaire_data, drugs, missingIds) {
  # Define a vector of GS IDs
  GsId <- questionnaire_data$GsId

  # first loop over each person
  all_switches <- lapply(GsId, function(id) {
    # for each person, loop over each drug and return if drug is a switch
    switches <- sapply(drugs, function(drug) {
      # if id is in missingID for that drug then return "Missing"
      if (id %in% missingIds[[drug]]) {
        "Missing"
      } else {
        ad_time <- questionnaire_data %>%
          filter(GsId == id) %>%
          pull(paste0("AMBER_ad_time_", drug))

        program_ad_time_6_weeks <- questionnaire_data %>%
          filter(GsId == id) %>%
          {
            if (paste0("program_ad_time_6_weeks_", drug) %in% names(.)) pull(., paste0("program_ad_time_6_weeks_", drug)) else NA
          }

        reason_stop <- questionnaire_data %>%
          filter(GsId == id) %>%
          pull(paste0("AMBER_reason_stop_", drug))

        detect_switch(
          id,
          program_ad_time_6_weeks,
          ad_time,
          reason_stop,
          questionnaire_data,  # ADD THIS
          drug                 # ADD THIS
        )
      }
    })
    # return a value NA, TRUE or FALSE for each drug
    switches
  })

  # for each person sum all the drugs to get the total number of switches
  # replace value with NA if they had missing data for one of the drugs (they took)
  n_switches_missing <- sapply(all_switches, function(switch) {
    # Check if person took any of these drugs
    took_any_drug <- any(!is.na(unlist(switch)) & unlist(switch) != "Missing")
    
    # If they didn't take any drugs, return NA
    if (!took_any_drug) {
      return(NA)
    }
    
    # If they have missing data for drugs they took, return NA
    if (any(unlist(switch) == "Missing", na.rm = TRUE)) {
      return(NA)
    }
    
    # Otherwise, count the switches
    sum(unlist(switch), na.rm = TRUE)
  })
  
  n_switches_missing
}

# ------------------------------------------------------------------------------
# Create a col called any_ad_effective this is using the column
# AMBER_overall_[med] for overall effectiveness: 2 = Yes, a lot; 1 = Yes, a
# little; 0 = No; -99 = Don’t know; -98 = Prefer not to answer; -97 = Empty
# Definitions:
# TRUE requires (a) at least one drug to be "Yes, a lot" and (b) never answered "No"
# to any drug being effective.
# FALSE requires (a) at least one drug to be "No" (ie. not effective) and (b) 
# never answered "Yes, a lot" or "Yes, a little" to any drug being effective.
# NA for all other cases

# megan
create_any_ad_effective <- function(questionnaire_data, drugs) {
  sapply(seq_len(nrow(questionnaire_data)), function(i) {
    # Get effectiveness responses for all drugs
    all_drugs <- sapply(drugs, function(drug) {
      questionnaire_data[[paste0("AMBER_overall_", drug)]][i]
    })
    
    # Only consider valid responses (not NA, -97, -98, -99)
    valid_responses <- all_drugs[!is.na(all_drugs) & 
                                   !all_drugs %in% c(-97, -98, -99)]
    
    # If no valid responses, return NA
    if (length(valid_responses) == 0) {
      return(NA)
    }
    
    case_when(
      # TRUE: At least one "Yes, a lot" (2) AND never "No" (0)
      any(valid_responses == 2) & !any(valid_responses == 0) ~ TRUE,
      # FALSE: At least one "No" (0) AND never "Yes, a little" (1) or "Yes, a lot" (2)
      any(valid_responses == 0) & !any(valid_responses %in% c(1, 2)) ~ FALSE,
      # NA for all other cases
      TRUE ~ NA
    )
  })
}

# ------------------------------------------------------------------------------
# Call the above functions in one function that returns separate vectors for
# n_switches, n_switches_missing and ad_any_effective
get_column_data <- function(drugs, questionnaire_data) {
  # Get Ids for people with missing data, ie. if they have have taken a drug
  # but not answered a question about how long they were on the drug or the reason
  # they stopped then that data is missing and we dont know if that drug counts
  # as a switch or not
  missingIds_for_switch <- sapply(drugs, function(drug) {
    c(
      check_missing_data(questionnaire_data, "AMBER_ad_time_", drug),
      check_missing_data(questionnaire_data, "AMBER_reason_stop_", drug)
    ) %>%
      unique()
  })

  # add new columns to questionnaire_data data frame
  # (the functions creating switches are a bit slow, could be improved)
  n_switches <- create_n_switches(questionnaire_data, drugs)
  n_switches_missing <- create_n_switches_missing(questionnaire_data, drugs, missingIds_for_switch)
  any_ad_effective <- create_any_ad_effective(questionnaire_data, drugs)

  list(
    n_switches = n_switches,
    n_switches_missing = n_switches_missing,
    any_ad_effective = any_ad_effective
  )
}
