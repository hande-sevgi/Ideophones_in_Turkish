# =============================================================================
# 01_validate_inputs.R
#
# Purpose:
#   Validate the three organized Chapter 4 input workbooks before processing.
#
# This script:
#   1. Confirms that all required workbooks exist.
#   2. Confirms that the expected worksheets exist.
#   3. Checks required columns.
#   4. Checks row and participant counts.
#   5. Checks observations per participant.
#   6. Checks missing values.
#   7. Checks rating ranges.
#   8. Checks the number of experimental, filler, and catch trials.
#
# This script does not modify any input files.
# =============================================================================


# ---- Required package --------------------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'readxl' is required.\n",
      "Install it once with:\n\n",
      'install.packages("readxl")'
    ),
    call. = FALSE
  )
}


# ---- File locations ----------------------------------------------------------

input_directory <- file.path("data", "input")

study_specifications <- list(

  exp1 = list(
    study_name = "Experiment I",
    file_name = "Experiment_I.xlsx",
    sheet_name = "List_Organized",
    expected_rows = 1400L,
    expected_participants = 50L,
    expected_trials_per_participant = 28L,
    rating_min = 1,
    rating_max = 5,
    expected_trial_counts = c(
      Experimental = 12L,
      CatchTrial = 8L,
      Filler = 8L
    )
  ),

  exp2 = list(
    study_name = "Experiment II",
    file_name = "Experiment_II.xlsx",
    sheet_name = "List_Organized",
    expected_rows = 1372L,
    expected_participants = 49L,
    expected_trials_per_participant = 28L,
    rating_min = 1,
    rating_max = 5,
    expected_trial_counts = c(
      Experimental = 12L,
      CatchTrial = 8L,
      Filler = 8L
    )
  ),

  exp3 = list(
    study_name = "Experiment III",
    file_name = "Experiment_III.xlsx",
    sheet_name = "List_Base_MainDocument",
    expected_rows = 2784L,
    expected_participants = 116L,
    expected_trials_per_participant = 24L,
    rating_min = 0,
    rating_max = 100,
    expected_trial_counts = c(
      Experimental = 16L,
      CatchTrial = 8L
    )
  )
)


# ---- Helper functions --------------------------------------------------------

fail_validation <- function(study_name, message) {
  stop(
    paste0("[", study_name, "] ", message),
    call. = FALSE
  )
}


classify_trial <- function(trial, study_id) {

  if (study_id %in% c("exp1", "exp2")) {

    ifelse(
      grepl("CT", trial, fixed = TRUE),
      "CatchTrial",
      ifelse(
        grepl("_F_", trial, fixed = TRUE),
        "Filler",
        "Experimental"
      )
    )

  } else if (study_id == "exp3") {

    ifelse(
      grepl("^Filler_", trial),
      "CatchTrial",
      "Experimental"
    )

  } else {
    stop("Unknown study identifier.", call. = FALSE)
  }
}


validate_study <- function(study_id, specification) {

  study_name <- specification$study_name
  input_file <- file.path(
    input_directory,
    specification$file_name
  )

  cat("\n")
  cat("============================================\n")
  cat("Validating ", study_name, "\n", sep = "")
  cat("============================================\n")

  # Check that the workbook exists.
  if (!file.exists(input_file)) {
    fail_validation(
      study_name,
      paste0(
        "Input workbook not found: ",
        input_file
      )
    )
  }

  # Check that the required worksheet exists.
  available_sheets <- readxl::excel_sheets(input_file)

  if (!specification$sheet_name %in% available_sheets) {
    fail_validation(
      study_name,
      paste0(
        "Worksheet '",
        specification$sheet_name,
        "' was not found in ",
        specification$file_name,
        ". Available worksheets: ",
        paste(available_sheets, collapse = ", ")
      )
    )
  }

  # Read the selected worksheet.
  data <- readxl::read_excel(
    path = input_file,
    sheet = specification$sheet_name,
    col_types = c("text", "text", "numeric"),
    .name_repair = "minimal"
  )

  # Check required variables.
  required_columns <- c(
    "Response_ID",
    "Trial",
    "Rating"
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  if (length(missing_columns) > 0L) {
    fail_validation(
      study_name,
      paste0(
        "Required columns are missing: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  # Retain only required columns for validation.
  data <- data[required_columns]

  # Remove completely empty spreadsheet rows, if any.
  completely_empty <- (
    is.na(data$Response_ID) &
      is.na(data$Trial) &
      is.na(data$Rating)
  )

  data <- data[!completely_empty, , drop = FALSE]

  # Check row count.
  observed_rows <- nrow(data)

  if (observed_rows != specification$expected_rows) {
    fail_validation(
      study_name,
      paste0(
        "Expected ",
        specification$expected_rows,
        " rows but found ",
        observed_rows,
        "."
      )
    )
  }

  # Check missing values.
  missing_response_ids <- sum(
    is.na(data$Response_ID) |
      trimws(data$Response_ID) == ""
  )

  missing_trials <- sum(
    is.na(data$Trial) |
      trimws(data$Trial) == ""
  )

  missing_ratings <- sum(is.na(data$Rating))

  if (missing_response_ids > 0L) {
    fail_validation(
      study_name,
      paste0(
        "Response_ID contains ",
        missing_response_ids,
        " missing values."
      )
    )
  }

  if (missing_trials > 0L) {
    fail_validation(
      study_name,
      paste0(
        "Trial contains ",
        missing_trials,
        " missing values."
      )
    )
  }

  if (missing_ratings > 0L) {
    fail_validation(
      study_name,
      paste0(
        "Rating contains ",
        missing_ratings,
        " missing values."
      )
    )
  }

  # Check participant count.
  observed_participants <- length(
    unique(data$Response_ID)
  )

  if (
    observed_participants !=
      specification$expected_participants
  ) {
    fail_validation(
      study_name,
      paste0(
        "Expected ",
        specification$expected_participants,
        " participants but found ",
        observed_participants,
        "."
      )
    )
  }

  # Check the total number of observations per participant.
  observations_per_participant <- table(
    data$Response_ID
  )

  incorrect_participant_counts <- observations_per_participant[
    observations_per_participant !=
      specification$expected_trials_per_participant
  ]

  if (length(incorrect_participant_counts) > 0L) {
    fail_validation(
      study_name,
      paste0(
        "Every participant should have ",
        specification$expected_trials_per_participant,
        " observations, but ",
        length(incorrect_participant_counts),
        " participant(s) have a different number."
      )
    )
  }

  # Check rating range.
  observed_rating_min <- min(data$Rating)
  observed_rating_max <- max(data$Rating)

  if (
    observed_rating_min < specification$rating_min ||
      observed_rating_max > specification$rating_max
  ) {
    fail_validation(
      study_name,
      paste0(
        "Ratings must be between ",
        specification$rating_min,
        " and ",
        specification$rating_max,
        ". Observed range: ",
        observed_rating_min,
        " to ",
        observed_rating_max,
        "."
      )
    )
  }

  # Experiments I and II use integer ordinal ratings.
  if (study_id %in% c("exp1", "exp2")) {

    allowed_ratings <- 1:5

    unexpected_ratings <- setdiff(
      sort(unique(data$Rating)),
      allowed_ratings
    )

    if (length(unexpected_ratings) > 0L) {
      fail_validation(
        study_name,
        paste0(
          "Unexpected rating values detected: ",
          paste(unexpected_ratings, collapse = ", ")
        )
      )
    }
  }

  # Classify trials.
  data$trial_type <- classify_trial(
    data$Trial,
    study_id
  )

  observed_trial_types <- unique(
    data$trial_type
  )

  expected_trial_types <- names(
    specification$expected_trial_counts
  )

  unexpected_trial_types <- setdiff(
    observed_trial_types,
    expected_trial_types
  )

  if (length(unexpected_trial_types) > 0L) {
    fail_validation(
      study_name,
      paste0(
        "Unexpected trial types detected: ",
        paste(unexpected_trial_types, collapse = ", ")
      )
    )
  }

  # Check trial-type counts for each participant.
  participant_trial_counts <- table(
    data$Response_ID,
    data$trial_type
  )

  for (
    trial_type in
    names(specification$expected_trial_counts)
  ) {

    expected_count <-
      specification$expected_trial_counts[[trial_type]]

    if (
      !trial_type %in%
        colnames(participant_trial_counts)
    ) {
      fail_validation(
        study_name,
        paste0(
          "Trial type '",
          trial_type,
          "' is absent."
        )
      )
    }

    observed_counts <-
      participant_trial_counts[, trial_type]

    if (any(observed_counts != expected_count)) {
      fail_validation(
        study_name,
        paste0(
          "Every participant should have ",
          expected_count,
          " ",
          trial_type,
          " observations."
        )
      )
    }
  }

  # Check the three adverbial types in Experiments I and II.
  if (study_id %in% c("exp1", "exp2")) {

    experimental_trials <- data$Trial[
      data$trial_type == "Experimental"
    ]

    required_adverb_codes <- c(
      "Time",
      "Manner",
      "Red"
    )

    missing_adverb_codes <- required_adverb_codes[
      !vapply(
        required_adverb_codes,
        function(code) {
          any(grepl(code, experimental_trials, fixed = TRUE))
        },
        logical(1)
      )
    ]

    if (length(missing_adverb_codes) > 0L) {
      fail_validation(
        study_name,
        paste0(
          "Experimental trials are missing these ",
          "adverbial codes: ",
          paste(missing_adverb_codes, collapse = ", ")
        )
      )
    }
  }

  # Check Experiment III condition codes.
  if (study_id == "exp3") {

    experimental_trials <- data$Trial[
      data$trial_type == "Experimental"
    ]

    required_codes <- c(
      "Ide",
      "NonIde",
      "Red",
      "Cvb",
      "RegMod",
      "Cont",
      "Aff",
      "Neg"
    )

    missing_codes <- required_codes[
      !vapply(
        required_codes,
        function(code) {
          any(grepl(code, experimental_trials, fixed = TRUE))
        },
        logical(1)
      )
    ]

    if (length(missing_codes) > 0L) {
      fail_validation(
        study_name,
        paste0(
          "Experimental trials are missing these ",
          "condition codes: ",
          paste(missing_codes, collapse = ", ")
        )
      )
    }
  }

  # Return a compact summary.
  summary <- data.frame(
    study = study_name,
    file = specification$file_name,
    sheet = specification$sheet_name,
    rows = observed_rows,
    participants = observed_participants,
    observations_per_participant =
      specification$expected_trials_per_participant,
    rating_min = observed_rating_min,
    rating_max = observed_rating_max,
    stringsAsFactors = FALSE
  )

  cat("Workbook: ", specification$file_name, "\n", sep = "")
  cat("Worksheet: ", specification$sheet_name, "\n", sep = "")
  cat("Rows: ", observed_rows, "\n", sep = "")
  cat("Participants: ", observed_participants, "\n", sep = "")
  cat(
    "Observations per participant: ",
    specification$expected_trials_per_participant,
    "\n",
    sep = ""
  )
  cat(
    "Rating range: ",
    observed_rating_min,
    "–",
    observed_rating_max,
    "\n",
    sep = ""
  )
  cat("Status: PASSED\n")

  summary
}


# ---- Run validation ----------------------------------------------------------

validation_results <- do.call(
  rbind,
  lapply(
    names(study_specifications),
    function(study_id) {
      validate_study(
        study_id,
        study_specifications[[study_id]]
      )
    }
  )
)

rownames(validation_results) <- NULL


# ---- Print final summary -----------------------------------------------------

cat("\n")
cat("============================================\n")
cat("All input datasets passed validation\n")
cat("============================================\n\n")

print(
  validation_results,
  row.names = FALSE
)

cat(
  "\nThe input workbooks were read but not modified.\n"
)
