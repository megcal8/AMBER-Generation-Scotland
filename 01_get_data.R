# Read in data from questionnaire, each row is a questionnaire response and each
# column is a question. Requires symbolic link to GenScotDepression (see
# README.md)
# ------------------------------------------------------------------------------
# Define function that takes path to data file and returns a single merged
# dataframe

require(dplyr)

read_data <- function(path) {
  AMBER_data <- read.csv(path)
  # check that there are no duplicate GsId or SurveyResponseId
  if (any(duplicated(AMBER_data$GsId))) {
    stop(paste0("Duplicate GsId found in data at path: ", path))
  }
  if (any(duplicated(AMBER_data$SurveyResponseId))) {
    stop(paste0("Duplicate SurveyResponseId found in data at path: ", path))
  }
  AMBER_data
}

subset_cols <- function(data, cols_to_keep) {
  if (isTRUE(cols_to_keep)) {
    return(data) # return all columns if TRUE
  } else {
    valid_cols <- intersect(cols_to_keep, names(data))
    missing_cols <- setdiff(cols_to_keep, names(data))
    if (length(missing_cols) > 0) {
      warning(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
    }
    data[, valid_cols, drop = FALSE]
  }
}

merge_data <- function(list_of_dataframes, merge_column) {
  Reduce(function(x, y) full_join(x, y, by = merge_column), list_of_dataframes)
}

# ------------------------------------------------------------------------------
# Main function called in main.R
main_get_data <- function(paths, cols_to_keep, merge_column) {
  # Read in data as a list of dataframes
  AMBER_data_list <- lapply(paths, read_data)

  # Subset to columns we want to keep in each dataframe
  # Match names of dataframes to corresponding column sets
  AMBER_subset_data_list <- mapply(function(name, data) {
    keep_cols <- cols_to_keep[[name]]
    subset_cols(data, keep_cols)
  }, name = names(AMBER_data_list), data = AMBER_data_list, SIMPLIFY = FALSE)

  # Merge the dataframes in the list into a single dataframe
  responses <- merge_data(AMBER_subset_data_list, merge_column)
  
  # Check if the responses dataframe is empty or null
  if (is.null(responses) || nrow(responses) == 0) {
    stop(
      "Data has not been read in, subsetted or merged correctly.",
      "Please check the input paths and column names."
    )
  } else {
    # Print message if data read in successfully
    message(
      "Data read in and processed successfully. Number of rows: ",
      nrow(responses), " and number of columns: ", ncol(responses)
    )
  }
  responses
}
