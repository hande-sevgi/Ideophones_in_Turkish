# =============================================================================
# 05_analyze_experiment_2.R
#
# Purpose:
#   Reproduce the primary Experiment II analysis reported in Chapter 4.
#
# Analysis:
#   - Manner and temporal adverbials only
#   - Experimental trials only
#   - 2 × 2 × 2 design:
#       continuation type × adverb type × negation position
#   - Five-point ordinal response
#   - Sum-coded predictors
#   - Random intercepts for participant and scenario
#
# The later 06_exp2_extended_analysis.R script will extend this analysis
# by including ideophones as the third adverbial-type level.
# =============================================================================


# ---- Required packages -------------------------------------------------------

required_packages <- c(
  "ordinal",
  "emmeans",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Install the following packages first:\n\n",
      "install.packages(c(",
      paste(
        paste0('"', missing_packages, '"'),
        collapse = ", "
      ),
      "))"
    ),
    call. = FALSE
  )
}


# ---- Reproducibility ---------------------------------------------------------

set.seed(20260810)


# ---- File locations ----------------------------------------------------------

input_file <- file.path(
  "data",
  "processed",
  "exp2_contrastive_noncanonical.csv"
)

figure_directory <- file.path(
  "output",
  "figures",
  "exp2_primary"
)

table_directory <- file.path(
  "output",
  "tables",
  "exp2_primary"
)

model_directory <- file.path(
  "output",
  "models",
  "exp2_primary"
)

for (
  directory in c(
    figure_directory,
    table_directory,
    model_directory
  )
) {
  if (!dir.exists(directory)) {
    dir.create(
      directory,
      recursive = TRUE
    )
  }
}


# ---- Read data ---------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Processed Experiment II data not found:\n",
      input_file,
      "\n\nRun 02_prepare_processed_data.R first."
    ),
    call. = FALSE
  )
}

exp2_all <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


# ---- Validate processed data -------------------------------------------------

required_columns <- c(
  "participant_id",
  "trial",
  "rating",
  "scenario",
  "trial_type",
  "negation_position",
  "adverb_type",
  "continuation_type",
  "word_order",
  "included"
)

missing_columns <- setdiff(
  required_columns,
  names(exp2_all)
)

if (length(missing_columns) > 0L) {
  stop(
    paste0(
      "Experiment II processed data are missing: ",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}


# ---- Select the primary analysis --------------------------------------------

exp2_primary <- exp2_all[
  exp2_all$included == TRUE &
    exp2_all$trial_type == "experimental" &
    exp2_all$adverb_type %in%
      c("temporal", "manner"),
  ,
  drop = FALSE
]

rownames(exp2_primary) <- NULL

observed_participants <- length(
  unique(exp2_primary$participant_id)
)

observed_observations <- nrow(
  exp2_primary
)

observed_scenarios <- length(
  unique(exp2_primary$scenario)
)

observed_word_orders <- unique(
  exp2_primary$word_order
)

if (
  length(observed_word_orders) != 1L ||
    observed_word_orders != "noncanonical"
) {
  stop(
    paste0(
      "Experiment II primary data must contain only ",
      "noncanonical word order."
    ),
    call. = FALSE
  )
}

if (observed_participants != 49L) {
  stop(
    paste0(
      "Expected 49 participants but found ",
      observed_participants,
      "."
    ),
    call. = FALSE
  )
}

# The archived Experiment II allocation contains
# 196 temporal and 196 manner observations.
if (observed_observations != 392L) {
  stop(
    paste0(
      "Expected 392 primary-analysis observations ",
      "but found ",
      observed_observations,
      "."
    ),
    call. = FALSE
  )
}

if (observed_scenarios != 6L) {
  stop(
    paste0(
      "Expected 6 scenarios but found ",
      observed_scenarios,
      "."
    ),
    call. = FALSE
  )
}

if (
  any(is.na(exp2_primary$rating)) ||
    any(!exp2_primary$rating %in% 1:5)
) {
  stop(
    "Experiment II ratings must be integers from 1 to 5.",
    call. = FALSE
  )
}


# ---- Factor coding -----------------------------------------------------------

# Ratings are ordered from least natural (1)
# to most natural (5).
exp2_primary$rating_ordered <- ordered(
  exp2_primary$rating,
  levels = 1:5
)

exp2_primary$participant_id <- factor(
  exp2_primary$participant_id
)

exp2_primary$scenario <- factor(
  exp2_primary$scenario,
  levels = as.character(1:6)
)

exp2_primary$continuation_type <- factor(
  exp2_primary$continuation_type,
  levels = c(
    "match",
    "mismatch"
  )
)

exp2_primary$adverb_type <- factor(
  exp2_primary$adverb_type,
  levels = c(
    "temporal",
    "manner"
  )
)

exp2_primary$negation_position <- factor(
  exp2_primary$negation_position,
  levels = c(
    "second_clause",
    "first_clause"
  )
)

if (
  anyNA(exp2_primary$continuation_type) ||
    anyNA(exp2_primary$adverb_type) ||
    anyNA(exp2_primary$negation_position) ||
    anyNA(exp2_primary$scenario)
) {
  stop(
    "Unexpected or missing factor levels detected.",
    call. = FALSE
  )
}

# Sum coding reproduces the dissertation analysis.
contrasts(
  exp2_primary$continuation_type
) <- contr.sum(2)

contrasts(
  exp2_primary$adverb_type
) <- contr.sum(2)

contrasts(
  exp2_primary$negation_position
) <- contr.sum(2)


# ---- Cell counts -------------------------------------------------------------

cell_counts <- as.data.frame(
  table(
    continuation_type =
      exp2_primary$continuation_type,
    adverb_type =
      exp2_primary$adverb_type,
    negation_position =
      exp2_primary$negation_position
  ),
  stringsAsFactors = FALSE
)

names(cell_counts)[
  names(cell_counts) == "Freq"
] <- "observations"

write.csv(
  cell_counts,
  file.path(
    table_directory,
    "exp2_primary_cell_counts.csv"
  ),
  row.names = FALSE
)


# ---- Descriptive summary -----------------------------------------------------

group_variables <- interaction(
  exp2_primary$continuation_type,
  exp2_primary$adverb_type,
  exp2_primary$negation_position,
  drop = TRUE,
  lex.order = TRUE
)

descriptive_summary <- do.call(
  rbind,
  lapply(
    split(
      exp2_primary,
      group_variables
    ),
    function(group_data) {

      data.frame(
        continuation_type =
          as.character(
            group_data$continuation_type[1]
          ),
        adverb_type =
          as.character(
            group_data$adverb_type[1]
          ),
        negation_position =
          as.character(
            group_data$negation_position[1]
          ),
        observations =
          nrow(group_data),
        participants =
          length(
            unique(
              group_data$participant_id
            )
          ),
        mean_rating =
          mean(group_data$rating),
        sd_rating =
          stats::sd(group_data$rating),
        se_rating =
          stats::sd(group_data$rating) /
          sqrt(nrow(group_data)),
        median_rating =
          stats::median(group_data$rating),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(descriptive_summary) <- NULL

write.csv(
  descriptive_summary,
  file.path(
    table_directory,
    "exp2_primary_descriptive_summary.csv"
  ),
  row.names = FALSE
)


# ---- Fit cumulative-link mixed model ----------------------------------------

primary_formula <- (
  rating_ordered ~
    continuation_type *
    adverb_type *
    negation_position +
    (1 | participant_id) +
    (1 | scenario)
)

primary_model <- ordinal::clmm(
  formula = primary_formula,
  data = exp2_primary,
  link = "logit",
  Hess = TRUE,
  nAGQ = 1,
  model = TRUE
)


# ---- Model summary -----------------------------------------------------------

model_summary <- summary(
  primary_model
)

capture.output(
  model_summary,
  file = file.path(
    table_directory,
    "exp2_primary_model_summary.txt"
  )
)


# ---- Fixed effects -----------------------------------------------------------

coefficient_table <- as.data.frame(
  stats::coef(model_summary)
)

coefficient_table$term <- rownames(
  coefficient_table
)

fixed_effect_names <- names(
  primary_model$beta
)

fixed_effect_rows <- match(
  fixed_effect_names,
  coefficient_table$term
)

if (anyNA(fixed_effect_rows)) {
  stop(
    "Could not match all fixed effects in the model summary.",
    call. = FALSE
  )
}

fixed_effects <- data.frame(
  term = fixed_effect_names,
  estimate =
    coefficient_table[
      fixed_effect_rows,
      1
    ],
  std_error =
    coefficient_table[
      fixed_effect_rows,
      2
    ],
  z_value =
    coefficient_table[
      fixed_effect_rows,
      3
    ],
  p_value =
    coefficient_table[
      fixed_effect_rows,
      4
    ],
  stringsAsFactors = FALSE
)

write.csv(
  fixed_effects,
  file.path(
    table_directory,
    "exp2_primary_fixed_effects.csv"
  ),
  row.names = FALSE
)


# ---- Random effects ----------------------------------------------------------

variance_components <- ordinal::VarCorr(
  primary_model
)

random_effects <- do.call(
  rbind,
  lapply(
    names(variance_components),
    function(group_name) {

      variance_matrix <-
        variance_components[[group_name]]

      variance_value <- as.numeric(
        variance_matrix[1, 1]
      )

      data.frame(
        grouping_factor = group_name,
        variance = variance_value,
        standard_deviation =
          sqrt(variance_value),
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(random_effects) <- NULL

write.csv(
  random_effects,
  file.path(
    table_directory,
    "exp2_primary_random_effects.csv"
  ),
  row.names = FALSE
)


# ---- Model convergence checks ------------------------------------------------

convergence_code <- if (
  !is.null(
    primary_model$optRes$convergence
  )
) {
  as.character(
    primary_model$optRes$convergence
  )
} else {
  NA_character_
}

maximum_absolute_gradient <- if (
  !is.null(primary_model$gradient)
) {
  max(
    abs(primary_model$gradient)
  )
} else if (
  !is.null(
    primary_model$optRes$gradient
  )
) {
  max(
    abs(
      primary_model$optRes$gradient
    )
  )
} else {
  NA_real_
}

positive_definite_hessian <- tryCatch(
  {
    hessian_values <- eigen(
      (
        primary_model$Hessian +
          t(primary_model$Hessian)
      ) / 2,
      symmetric = TRUE,
      only.values = TRUE
    )$values

    all(hessian_values > 0)
  },
  error = function(error) {
    NA
  }
)

hessian_condition_number <- tryCatch(
  {
    kappa(primary_model$Hessian)
  },
  error = function(error) {
    NA_real_
  }
)

model_diagnostics <- data.frame(
  diagnostic = c(
    "convergence_code",
    "maximum_absolute_gradient",
    "positive_definite_hessian",
    "hessian_condition_number",
    "log_likelihood",
    "AIC",
    "BIC"
  ),
  value = c(
    convergence_code,
    maximum_absolute_gradient,
    positive_definite_hessian,
    hessian_condition_number,
    as.numeric(
      stats::logLik(primary_model)
    ),
    stats::AIC(primary_model),
    stats::BIC(primary_model)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  model_diagnostics,
  file.path(
    table_directory,
    "exp2_primary_model_diagnostics.csv"
  ),
  row.names = FALSE
)


# ---- Model-predicted mean ratings --------------------------------------------

# For an ordinal model, mode = "mean.class"
# converts category probabilities into an expected
# rating on the original 1–5 scale.
prediction_grid <- emmeans::emmeans(
  primary_model,
  specs = ~
    continuation_type *
    adverb_type *
    negation_position,
  mode = "mean.class"
)

predictions_raw <- as.data.frame(
  summary(
    prediction_grid,
    infer = c(TRUE, FALSE),
    level = 0.95
  )
)

estimate_column <- intersect(
  c(
    "emmean",
    "mean.class",
    "response"
  ),
  names(predictions_raw)
)[1]

lower_column <- grep(
  "LCL|lower\\.CL",
  names(predictions_raw),
  value = TRUE
)[1]

upper_column <- grep(
  "UCL|upper\\.CL",
  names(predictions_raw),
  value = TRUE
)[1]

if (
  is.na(estimate_column) ||
    is.na(lower_column) ||
    is.na(upper_column)
) {
  stop(
    paste0(
      "Could not identify prediction or confidence-interval ",
      "columns returned by emmeans."
    ),
    call. = FALSE
  )
}

model_predictions <- data.frame(
  continuation_type =
    predictions_raw$continuation_type,
  adverb_type =
    predictions_raw$adverb_type,
  negation_position =
    predictions_raw$negation_position,
  predicted_rating =
    predictions_raw[[estimate_column]],
  std_error =
    predictions_raw$SE,
  lower_95 =
    predictions_raw[[lower_column]],
  upper_95 =
    predictions_raw[[upper_column]],
  stringsAsFactors = FALSE
)

write.csv(
  model_predictions,
  file.path(
    table_directory,
    "exp2_primary_model_predictions.csv"
  ),
  row.names = FALSE
)


# ---- Raw-rating figure -------------------------------------------------------

raw_rating_table <- prop.table(
  table(
    rating = exp2_primary$rating,
    continuation_type =
      exp2_primary$continuation_type,
    adverb_type =
      exp2_primary$adverb_type,
    negation_position =
      exp2_primary$negation_position
  ),
  margin = c(2, 3, 4)
)

raw_rating_plot_data <- as.data.frame(
  raw_rating_table,
  stringsAsFactors = FALSE
)

names(
  raw_rating_plot_data
)[
  names(raw_rating_plot_data) == "Freq"
] <- "proportion"

raw_rating_plot <-
  ggplot2::ggplot(
    raw_rating_plot_data,
    ggplot2::aes(
      x = continuation_type,
      y = proportion,
      fill = factor(rating)
    )
  ) +
  ggplot2::geom_col(
    color = "white",
    linewidth = 0.2
  ) +
  ggplot2::facet_grid(
    negation_position ~ adverb_type
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format()
  ) +
  ggplot2::scale_fill_brewer(
    palette = "YlGnBu",
    direction = 1
  ) +
  ggplot2::labs(
    title =
      "Experiment II: observed rating distributions",
    subtitle =
      "Primary manner–temporal analysis",
    x = "Continuation type",
    y = "Proportion",
    fill = "Rating"
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_directory,
    "exp2_primary_raw_ratings.png"
  ),
  plot = raw_rating_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ---- Model-prediction figure -------------------------------------------------

prediction_plot_data <- model_predictions

prediction_plot_data$continuation_type <- factor(
  prediction_plot_data$continuation_type,
  levels = c(
    "match",
    "mismatch"
  ),
  labels = c(
    "Match",
    "Mismatch"
  )
)

prediction_plot_data$adverb_type <- factor(
  prediction_plot_data$adverb_type,
  levels = c(
    "temporal",
    "manner"
  ),
  labels = c(
    "Temporal",
    "Manner"
  )
)

prediction_plot_data$negation_position <- factor(
  prediction_plot_data$negation_position,
  levels = c(
    "second_clause",
    "first_clause"
  ),
  labels = c(
    "Negation in second clause",
    "Negation in first clause"
  )
)

prediction_plot <-
  ggplot2::ggplot(
    prediction_plot_data,
    ggplot2::aes(
      x = continuation_type,
      y = predicted_rating,
      group = adverb_type,
      color = adverb_type
    )
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 3
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower_95,
      ymax = upper_95
    ),
    width = 0.08,
    linewidth = 0.6
  ) +
  ggplot2::facet_wrap(
    ~ negation_position
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Temporal" = "#E76F51",
      "Manner" = "#2A9D8F"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(1, 5)
  ) +
  ggplot2::labs(
    title =
      "Experiment II: cumulative-link model predictions",
    subtitle =
      "Expected ratings with 95% confidence intervals",
    x = "Continuation type",
    y = "Model-predicted rating",
    color = "Adverb type"
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_directory,
    "exp2_primary_model_predictions.png"
  ),
  plot = prediction_plot,
  width = 10,
  height = 6,
  dpi = 300
)


# ---- Model metadata ----------------------------------------------------------

model_metadata <- data.frame(
  field = c(
    "study",
    "analysis",
    "input_file",
    "participants",
    "observations",
    "scenarios",
    "response_scale",
    "model_family",
    "link",
    "fixed_effects",
    "random_effects",
    "contrast_coding",
    "rating_order"
  ),
  value = c(
    "Experiment II",
    "Primary manner–temporal analysis",
    input_file,
    observed_participants,
    observed_observations,
    observed_scenarios,
    "Ordinal ratings from 1 to 5",
    "Cumulative link mixed model",
    "logit",
    paste(
      "continuation_type * adverb_type *",
      "negation_position"
    ),
    paste(
      "Random intercepts for participant_id",
      "and scenario"
    ),
    "Sum coding",
    "1 < 2 < 3 < 4 < 5"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  model_metadata,
  file.path(
    table_directory,
    "exp2_primary_model_metadata.csv"
  ),
  row.names = FALSE
)


# ---- Save model and session information -------------------------------------

saveRDS(
  primary_model,
  file.path(
    model_directory,
    "exp2_primary_clmm.rds"
  )
)

capture.output(
  sessionInfo(),
  file = file.path(
    model_directory,
    "session-info.txt"
  )
)


# ---- Completion message ------------------------------------------------------

cat("\n")
cat("============================================\n")
cat("Experiment II primary analysis complete\n")
cat("============================================\n\n")

cat(
  "Participants: ",
  observed_participants,
  "\n",
  sep = ""
)

cat(
  "Model observations: ",
  observed_observations,
  "\n",
  sep = ""
)

cat(
  "Scenarios: ",
  observed_scenarios,
  "\n",
  sep = ""
)

cat(
  "Maximum absolute gradient: ",
  format(
    maximum_absolute_gradient,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Positive-definite Hessian: ",
  positive_definite_hessian,
  "\n",
  sep = ""
)

cat(
  "\nFigures written to: ",
  figure_directory,
  "\n",
  sep = ""
)

cat(
  "Tables written to: ",
  table_directory,
  "\n",
  sep = ""
)

cat(
  "Model written to: ",
  model_directory,
  "\n",
  sep = ""
)
