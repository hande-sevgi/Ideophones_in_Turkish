# =============================================================================
# 02_prepare_processed_data.R
#
# Purpose:
#   Convert the organized Chapter 4 Excel inputs into documented,
#   analysis-ready CSV datasets.
#
# This script:
#   1. Reads the validated workbook sheets.
#   2. Replaces platform response IDs with anonymous study-specific IDs.
#   3. Decodes experimental trial labels.
#   4. Constructs consistent analysis variables.
#   5. Reproduces the two Experiment III attention-check exclusions.
#   6. Writes processed CSV datasets.
#
# The input workbooks are never modified.
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


# ---- Directories -------------------------------------------------------------

input_directory <- file.path("data", "input")
processed_directory <- file.path("data", "processed")

if (!dir.exists(processed_directory)) {
  dir.create(
    processed_directory,
    recursive = TRUE
  )
}


# ---- Helper functions --------------------------------------------------------

normalize_trial_code <- function(x) {

  # Replace non-breaking spaces with ordinary spaces.
  x <- gsub("\u00A0", " ", x, fixed = TRUE)

  # Remove leading and trailing spaces.
  x <- trimws(x)

  # Replace repeated internal spaces with one space.
  x <- gsub("[[:space:]]+", " ", x)

  x
}


create_anonymous_ids <- function(
  original_ids,
  study_prefix
) {

  id_order <- unique(original_ids)

  anonymous_number <- match(
    original_ids,
    id_order
  )

  sprintf(
    "%s_%03d",
    study_prefix,
    anonymous_number
  )
}


extract_scenario <- function(
  trial,
  maximum_scenario
) {

  pattern <- paste0(
    "^([1-",
    maximum_scenario,
    "]).*"
  )

  has_scenario <- grepl(
    pattern,
    trial
  )

  scenario <- ifelse(
    has_scenario,
    sub(pattern, "\\1", trial),
    NA_character_
  )

  scenario
}


read_input_sheet <- function(
  file_name,
  sheet_name
) {

  file_path <- file.path(
    input_directory,
    file_name
  )

  if (!file.exists(file_path)) {
    stop(
      paste0(
        "Input file not found: ",
        file_path
      ),
      call. = FALSE
    )
  }

  available_sheets <-
    readxl::excel_sheets(file_path)

  if (!sheet_name %in% available_sheets) {
    stop(
      paste0(
        "Worksheet '",
        sheet_name,
        "' not found in ",
        file_name,
        "."
      ),
      call. = FALSE
    )
  }

  data <- readxl::read_excel(
    path = file_path,
    sheet = sheet_name,
    col_types = c(
      "text",
      "text",
      "numeric"
    ),
    .name_repair = "minimal"
  )

  data <- as.data.frame(
    data,
    stringsAsFactors = FALSE
  )

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
    stop(
      paste0(
        "Missing required columns in ",
        file_name,
        ": ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data <- data[required_columns]

  completely_empty <- (
    is.na(data$Response_ID) &
      is.na(data$Trial) &
      is.na(data$Rating)
  )

  data <- data[
    !completely_empty,
    ,
    drop = FALSE
  ]

  data$Response_ID <- trimws(
    data$Response_ID
  )

  data$Trial <- normalize_trial_code(
    data$Trial
  )

  data
}


# ---- Prepare Experiments I and II --------------------------------------------

prepare_contrastive_experiment <- function(
  study_id,
  study_name,
  input_file,
  input_sheet,
  output_file,
  word_order
) {

  cat("\n")
  cat("Preparing ", study_name, "\n", sep = "")
  cat("--------------------------------------------\n")

  input_data <- read_input_sheet(
    file_name = input_file,
    sheet_name = input_sheet
  )

  input_rows <- nrow(input_data)

  input_participants <- length(
    unique(input_data$Response_ID)
  )

  processed <- input_data

  processed$study_id <- study_id

  processed$participant_id <-
    create_anonymous_ids(
      original_ids = processed$Response_ID,
      study_prefix = study_id
    )

  processed$trial <- processed$Trial
  processed$rating <- processed$Rating

  processed$scenario <- extract_scenario(
    trial = processed$trial,
    maximum_scenario = 6
  )

  processed$trial_type <- ifelse(
    grepl(
      "CT",
      processed$trial,
      fixed = TRUE
    ),
    "catch",
    ifelse(
      grepl(
        "_F_",
        processed$trial,
        fixed = TRUE
      ),
      "filler",
      "experimental"
    )
  )

  processed$negation_position <- ifelse(
    grepl(
      "Aff",
      processed$trial,
      fixed = TRUE
    ),
    "second_clause",
    ifelse(
      grepl(
        "Neg",
        processed$trial,
        fixed = TRUE
      ),
      "first_clause",
      NA_character_
    )
  )

  processed$adverb_type <- ifelse(
    processed$trial_type != "experimental",
    NA_character_,
    ifelse(
      grepl(
        "Time",
        processed$trial,
        fixed = TRUE
      ),
      "temporal",
      ifelse(
        grepl(
          "Manner",
          processed$trial,
          fixed = TRUE
        ),
        "manner",
        ifelse(
          grepl(
            "Red",
            processed$trial,
            fixed = TRUE
          ),
          "ideophone",
          NA_character_
        )
      )
    )
  )

  processed$ideophone_form <- ifelse(
    processed$adverb_type == "ideophone",
    "reduplication",
    NA_character_
  )

  processed$continuation_type <- ifelse(
    processed$trial_type != "experimental",
    NA_character_,
    ifelse(
      grepl(
        "Loc",
        processed$trial,
        fixed = TRUE
      ),
      "mismatch",
      "match"
    )
  )

  processed$condition <- ifelse(
    processed$trial_type == "catch",
    ifelse(
      grepl(
        "Bad",
        processed$trial,
        fixed = TRUE
      ),
      "infelicitous",
      ifelse(
        grepl(
          "Good",
          processed$trial,
          fixed = TRUE
        ),
        "felicitous",
        NA_character_
      )
    ),
    ifelse(
      processed$trial_type == "filler",
      ifelse(
        grepl(
          "InDirOb",
          processed$trial,
          fixed = TRUE
        ),
        "indirect_object",
        ifelse(
          grepl(
            "DirObj",
            processed$trial,
            fixed = TRUE
          ),
          "direct_object",
          NA_character_
        )
      ),
      processed$continuation_type
    )
  )

  processed$word_order <- word_order

  # The public organized inputs already contain
  # the final analysed samples.
  processed$included <- TRUE
  processed$exclusion_reason <- ""

  processed <- processed[c(
    "study_id",
    "participant_id",
    "trial",
    "rating",
    "scenario",
    "trial_type",
    "negation_position",
    "adverb_type",
    "ideophone_form",
    "continuation_type",
    "condition",
    "word_order",
    "included",
    "exclusion_reason"
  )]

  # Confirm that processing did not change the number of rows.
  if (nrow(processed) != input_rows) {
    stop(
      paste0(
        "[",
        study_name,
        "] Row count changed during processing."
      ),
      call. = FALSE
    )
  }

  output_path <- file.path(
    processed_directory,
    output_file
  )

  write.csv(
    processed,
    file = output_path,
    row.names = FALSE,
    na = ""
  )

  cat("Input rows: ", input_rows, "\n", sep = "")
  cat(
    "Participants: ",
    input_participants,
    "\n",
    sep = ""
  )
  cat(
    "Experimental observations: ",
    sum(
      processed$trial_type == "experimental"
    ),
    "\n",
    sep = ""
  )
  cat(
    "Ideophone observations: ",
    sum(
      processed$adverb_type == "ideophone",
      na.rm = TRUE
    ),
    "\n",
    sep = ""
  )
  cat("Written: ", output_path, "\n", sep = "")

  data.frame(
    study = study_name,
    input_rows = input_rows,
    input_participants = input_participants,
    included_participants = input_participants,
    excluded_participants = 0L,
    output_file = output_file,
    stringsAsFactors = FALSE
  )
}


# ---- Prepare Experiment III --------------------------------------------------

prepare_experiment_3 <- function() {

  study_id <- "exp3"
  study_name <- "Experiment III"
  input_file <- "Experiment_III.xlsx"
  input_sheet <- "List_Base_MainDocument"
  output_file <- "exp3_monoclause.csv"

  cat("\n")
  cat("Preparing ", study_name, "\n", sep = "")
  cat("--------------------------------------------\n")

  input_data <- read_input_sheet(
    file_name = input_file,
    sheet_name = input_sheet
  )

  input_rows <- nrow(input_data)

  input_participants <- length(
    unique(input_data$Response_ID)
  )

  processed <- input_data

  processed$study_id <- study_id

  processed$participant_id <-
    create_anonymous_ids(
      original_ids = processed$Response_ID,
      study_prefix = study_id
    )

  processed$trial <- processed$Trial
  processed$rating <- processed$Rating

  processed$trial_type <- ifelse(
    grepl(
      "^Filler_",
      processed$trial
    ),
    "catch",
    "experimental"
  )

  processed$scenario <- ifelse(
    processed$trial_type == "experimental",
    extract_scenario(
      trial = processed$trial,
      maximum_scenario = 8
    ),
    NA_character_
  )

  processed$polarity <- ifelse(
    grepl(
      "_Aff_",
      processed$trial,
      fixed = TRUE
    ),
    "affirmative",
    ifelse(
      grepl(
        "_Neg_",
        processed$trial,
        fixed = TRUE
      ),
      "negative",
      NA_character_
    )
  )

  processed$iconicity_condition <- ifelse(
    processed$trial_type != "experimental",
    NA_character_,
    ifelse(
      grepl(
        "NonIde",
        processed$trial,
        fixed = TRUE
      ),
      "nonideophone",
      ifelse(
        grepl(
          "_Ide_",
          processed$trial,
          fixed = TRUE
        ),
        "ideophone",
        NA_character_
      )
    )
  )

  processed$adverb_type <- ifelse(
    processed$trial_type != "experimental",
    NA_character_,
    ifelse(
      grepl(
        "Cont",
        processed$trial,
        fixed = TRUE
      ),
      "temporal",
      ifelse(
        grepl(
          "RegMod",
          processed$trial,
          fixed = TRUE
        ),
        "manner",
        ifelse(
          grepl(
            "Red",
            processed$trial,
            fixed = TRUE
          ),
          "reduplication",
          ifelse(
            grepl(
              "Cvb",
              processed$trial,
              fixed = TRUE
            ),
            "converbial",
            NA_character_
          )
        )
      )
    )
  )

  processed$semantic_class <- ifelse(
    processed$trial_type != "experimental",
    NA_character_,
    ifelse(
      processed$adverb_type == "temporal",
      "temporal",
      "manner"
    )
  )

  processed$morphological_integration <- ifelse(
    processed$adverb_type == "reduplication",
    "low",
    ifelse(
      processed$adverb_type == "converbial",
      "high",
      NA_character_
    )
  )

  processed$catch_grammaticality <- ifelse(
    processed$trial_type != "catch",
    NA_character_,
    ifelse(
      grepl(
        "_UnGr_",
        processed$trial,
        fixed = TRUE
      ),
      "ungrammatical",
      ifelse(
        grepl(
          "_Gr_",
          processed$trial,
          fixed = TRUE
        ),
        "grammatical",
        NA_character_
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Reproduce the Experiment III attention-check exclusions.
  #
  # A catch-trial failure is defined as:
  #   - an affirmative ungrammatical catch trial rated above 50; or
  #   - an affirmative grammatical catch trial rated below 50.
  #
  # Participants with two or more failures are excluded.
  # ---------------------------------------------------------------------------

  processed$attention_check_failure <- (
    processed$trial_type == "catch" &
      processed$polarity == "affirmative" &
      processed$catch_grammaticality ==
        "ungrammatical" &
      processed$rating > 50
  ) | (
    processed$trial_type == "catch" &
      processed$polarity == "affirmative" &
      processed$catch_grammaticality ==
        "grammatical" &
      processed$rating < 50
  )

  failure_counts <- tapply(
    as.integer(
      processed$attention_check_failure
    ),
    processed$participant_id,
    sum
  )

  processed$attention_check_failures <-
    as.integer(
      failure_counts[
        processed$participant_id
      ]
    )

  processed$included <-
    processed$attention_check_failures < 2L

  processed$exclusion_reason <- ifelse(
    processed$included,
    "",
    "failed_two_or_more_affirmative_catch_trials"
  )

  included_participants <- length(
    unique(
      processed$participant_id[
        processed$included
      ]
    )
  )

  excluded_participants <- length(
    unique(
      processed$participant_id[
        !processed$included
      ]
    )
  )

  # The dissertation reports 114 included and
  # 2 attention-check exclusions.
  if (included_participants != 114L) {
    stop(
      paste0(
        "[Experiment III] Expected 114 included ",
        "participants but found ",
        included_participants,
        "."
      ),
      call. = FALSE
    )
  }

  if (excluded_participants != 2L) {
    stop(
      paste0(
        "[Experiment III] Expected 2 excluded ",
        "participants but found ",
        excluded_participants,
        "."
      ),
      call. = FALSE
    )
  }

  processed <- processed[c(
    "study_id",
    "participant_id",
    "trial",
    "rating",
    "scenario",
    "trial_type",
    "polarity",
    "iconicity_condition",
    "adverb_type",
    "semantic_class",
    "morphological_integration",
    "catch_grammaticality",
    "attention_check_failure",
    "attention_check_failures",
    "included",
    "exclusion_reason"
  )]

  # Confirm that processing preserved every input row.
  if (nrow(processed) != input_rows) {
    stop(
      paste0(
        "[Experiment III] Row count changed ",
        "during processing."
      ),
      call. = FALSE
    )
  }

  output_path <- file.path(
    processed_directory,
    output_file
  )

  write.csv(
    processed,
    file = output_path,
    row.names = FALSE,
    na = ""
  )

  cat("Input rows: ", input_rows, "\n", sep = "")
  cat(
    "Input participants: ",
    input_participants,
    "\n",
    sep = ""
  )
  cat(
    "Included participants: ",
    included_participants,
    "\n",
    sep = ""
  )
  cat(
    "Excluded participants: ",
    excluded_participants,
    "\n",
    sep = ""
  )
  cat(
    "Experimental observations: ",
    sum(
      processed$trial_type == "experimental"
    ),
    "\n",
    sep = ""
  )
  cat("Written: ", output_path, "\n", sep = "")

  data.frame(
    study = study_name,
    input_rows = input_rows,
    input_participants = input_participants,
    included_participants =
      included_participants,
    excluded_participants =
      excluded_participants,
    output_file = output_file,
    stringsAsFactors = FALSE
  )
}


# ---- Run preparation ---------------------------------------------------------

processing_summary <- rbind(

  prepare_contrastive_experiment(
    study_id = "exp1",
    study_name = "Experiment I",
    input_file = "Experiment_I.xlsx",
    input_sheet = "List_Organized",
    output_file =
      "exp1_contrastive_canonical.csv",
    word_order = "canonical"
  ),

  prepare_contrastive_experiment(
    study_id = "exp2",
    study_name = "Experiment II",
    input_file = "Experiment_II.xlsx",
    input_sheet = "List_Organized",
    output_file =
      "exp2_contrastive_noncanonical.csv",
    word_order = "noncanonical"
  ),

  prepare_experiment_3()
)

rownames(processing_summary) <- NULL


# ---- Print summary -----------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("Processed datasets created successfully\n")
cat("============================================\n\n")

print(
  processing_summary,
  row.names = FALSE
)

cat(
  "\nThe input workbooks were not modified.\n"
)
