# =============================================================================
# 04_exp1_extended_analysis.R
#
# Purpose:
#   Reproduce the extended Experiment I analysis reported in Chapter 4.
#
# Analysis:
#   - Temporal, manner, and ideophone trials
#   - Experimental trials only
#   - 3 × 2 × 2 design:
#       adverb type × continuation type × negation position
#   - Five-point ordinal response
#   - Sum-coded predictors
#   - Random intercepts for participant and scenario
#
# This is an extended dissertation analysis, not a sensitivity analysis.
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

set.seed(20260808)


# ---- File locations ----------------------------------------------------------

input_file <- file.path(
  "data",
  "processed",
  "exp1_contrastive_canonical.csv"
)

figure_directory <- file.path(
  "output",
  "figures",
  "exp1_extended"
)

table_directory <- file.path(
  "output",
  "tables",
  "exp1_extended"
)

model_directory <- file.path(
  "output",
  "models",
  "exp1_extended"
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
      "Processed Experiment I data not found:\n",
      input_file,
      "\n\nRun 02_prepare_processed_data.R first."
    ),
    call. = FALSE
  )
}

exp1_all <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)


# ---- Validate required columns ----------------------------------------------

required_columns <- c(
  "participant_id",
  "trial",
  "rating",
  "scenario",
  "trial_type",
  "negation_position",
  "adverb_type",
  "continuation_type",
  "included"
)

missing_columns <- setdiff(
  required_columns,
  names(exp1_all)
)

if (length(missing_columns) > 0L) {
  stop(
    paste0(
      "Experiment I processed data are missing: ",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}


# ---- Select extended analysis data ------------------------------------------

exp1_extended <- exp1_all[
  exp1_all$included == TRUE &
    exp1_all$trial_type == "experimental" &
    exp1_all$adverb_type %in%
      c(
        "temporal",
        "manner",
        "ideophone"
      ),
  ,
  drop = FALSE
]

rownames(exp1_extended) <- NULL

observed_participants <- length(
  unique(exp1_extended$participant_id)
)

observed_observations <- nrow(
  exp1_extended
)

observed_scenarios <- length(
  unique(exp1_extended$scenario)
)

if (observed_participants != 50L) {
  stop(
    paste0(
      "Expected 50 participants but found ",
      observed_participants,
      "."
    ),
    call. = FALSE
  )
}

if (observed_observations != 600L) {
  stop(
    paste0(
      "Expected 600 experimental observations but found ",
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
  any(is.na(exp1_extended$rating)) ||
    any(!exp1_extended$rating %in% 1:5)
) {
  stop(
    "Experiment I ratings must be integers from 1 to 5.",
    call. = FALSE
  )
}


# ---- Verify archived allocation ---------------------------------------------

observed_adverb_counts <- table(
  exp1_extended$adverb_type
)

expected_adverb_counts <- c(
  temporal = 200L,
  manner = 184L,
  ideophone = 216L
)

for (adverb in names(expected_adverb_counts)) {

  if (!adverb %in% names(observed_adverb_counts)) {
    stop(
      paste0(
        "Adverb type '",
        adverb,
        "' is missing."
      ),
      call. = FALSE
    )
  }

  if (
    observed_adverb_counts[[adverb]] !=
      expected_adverb_counts[[adverb]]
  ) {
    stop(
      paste0(
        "Expected ",
        expected_adverb_counts[[adverb]],
        " observations for ",
        adverb,
        " but found ",
        observed_adverb_counts[[adverb]],
        "."
      ),
      call. = FALSE
    )
  }
}


# ---- Factor coding -----------------------------------------------------------

exp1_extended$rating_ordered <- ordered(
  exp1_extended$rating,
  levels = 1:5
)

exp1_extended$participant_id <- factor(
  exp1_extended$participant_id
)

exp1_extended$scenario <- factor(
  exp1_extended$scenario,
  levels = as.character(1:6)
)

exp1_extended$continuation_type <- factor(
  exp1_extended$continuation_type,
  levels = c(
    "match",
    "mismatch"
  )
)

# This ordering ensures that the two explicit
# sum-coded coefficients correspond to:
#   adverb_type1 = temporal
#   adverb_type2 = manner
# Ideophone is represented implicitly as the
# negative sum of the first two contrasts.
exp1_extended$adverb_type <- factor(
  exp1_extended$adverb_type,
  levels = c(
    "temporal",
    "manner",
    "ideophone"
  )
)

exp1_extended$negation_position <- factor(
  exp1_extended$negation_position,
  levels = c(
    "second_clause",
    "first_clause"
  )
)

if (
  anyNA(exp1_extended$continuation_type) ||
    anyNA(exp1_extended$adverb_type) ||
    anyNA(exp1_extended$negation_position) ||
    anyNA(exp1_extended$scenario)
) {
  stop(
    "Unexpected or missing factor levels detected.",
    call. = FALSE
  )
}

contrasts(
  exp1_extended$continuation_type
) <- contr.sum(2)

contrasts(
  exp1_extended$adverb_type
) <- contr.sum(3)

contrasts(
  exp1_extended$negation_position
) <- contr.sum(2)


# ---- Cell counts -------------------------------------------------------------

cell_counts <- as.data.frame(
  table(
    continuation_type =
      exp1_extended$continuation_type,
    adverb_type =
      exp1_extended$adverb_type,
    negation_position =
      exp1_extended$negation_position
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
    "exp1_extended_cell_counts.csv"
  ),
  row.names = FALSE
)


# ---- Participant allocation table -------------------------------------------

participant_adverb_counts <- as.data.frame(
  table(
    participant_id =
      exp1_extended$participant_id,
    adverb_type =
      exp1_extended$adverb_type
  ),
  stringsAsFactors = FALSE
)

names(participant_adverb_counts)[
  names(participant_adverb_counts) == "Freq"
] <- "observations"

write.csv(
  participant_adverb_counts,
  file.path(
    table_directory,
    "exp1_extended_participant_adverb_counts.csv"
  ),
  row.names = FALSE
)


# ---- Descriptive summary -----------------------------------------------------

group_variables <- interaction(
  exp1_extended$continuation_type,
  exp1_extended$adverb_type,
  exp1_extended$negation_position,
  drop = TRUE,
  lex.order = TRUE
)

descriptive_summary <- do.call(
  rbind,
  lapply(
    split(
      exp1_extended,
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
    "exp1_extended_descriptive_summary.csv"
  ),
  row.names = FALSE
)


# ---- Fit the extended cumulative-link model ---------------------------------

extended_formula <- (
  rating_ordered ~
    continuation_type *
    adverb_type *
    negation_position +
    (1 | participant_id) +
    (1 | scenario)
)

extended_model <- ordinal::clmm(
  formula = extended_formula,
  data = exp1_extended,
  link = "logit",
  Hess = TRUE,
  nAGQ = 1,
  model = TRUE
)


# ---- Model summary -----------------------------------------------------------

model_summary <- summary(
  extended_model
)

capture.output(
  model_summary,
  file = file.path(
    table_directory,
    "exp1_extended_model_summary.txt"
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
  extended_model$beta
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
    "exp1_extended_fixed_effects.csv"
  ),
  row.names = FALSE
)


# ---- Random effects ----------------------------------------------------------

variance_components <- ordinal::VarCorr(
  extended_model
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
    "exp1_extended_random_effects.csv"
  ),
  row.names = FALSE
)


# ---- Convergence diagnostics -------------------------------------------------

convergence_code <- if (
  !is.null(
    extended_model$optRes$convergence
  )
) {
  as.character(
    extended_model$optRes$convergence
  )
} else {
  NA_character_
}

maximum_absolute_gradient <- if (
  !is.null(extended_model$gradient)
) {
  max(
    abs(extended_model$gradient)
  )
} else if (
  !is.null(
    extended_model$optRes$gradient
  )
) {
  max(
    abs(
      extended_model$optRes$gradient
    )
  )
} else {
  NA_real_
}

positive_definite_hessian <- tryCatch(
  {
    hessian_values <- eigen(
      (
        extended_model$Hessian +
          t(extended_model$Hessian)
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
    kappa(extended_model$Hessian)
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
      stats::logLik(extended_model)
    ),
    stats::AIC(extended_model),
    stats::BIC(extended_model)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  model_diagnostics,
  file.path(
    table_directory,
    "exp1_extended_model_diagnostics.csv"
  ),
  row.names = FALSE
)


# ---- Pairwise adverb comparisons --------------------------------------------

# The dissertation reports unadjusted pairwise comparisons.
# We preserve those p-values and also add Holm-adjusted
# p-values for transparent supplementary reporting.

adverb_emmeans <- emmeans::emmeans(
  extended_model,
  specs = ~
    adverb_type |
    continuation_type,
  mode = "latent"
)

pairwise_unadjusted <- as.data.frame(
  summary(
    emmeans::contrast(
      adverb_emmeans,
      method = "pairwise",
      adjust = "none"
    ),
    infer = c(TRUE, TRUE),
    level = 0.95
  )
)

pairwise_unadjusted$p_value_holm <-
  ave(
    pairwise_unadjusted$p.value,
    pairwise_unadjusted$continuation_type,
    FUN = function(p_value) {
      stats::p.adjust(
        p_value,
        method = "holm"
      )
    }
  )

write.csv(
  pairwise_unadjusted,
  file.path(
    table_directory,
    "exp1_extended_adverb_contrasts.csv"
  ),
  row.names = FALSE
)


# ---- Model-predicted ratings -------------------------------------------------

prediction_grid <- emmeans::emmeans(
  extended_model,
  specs = ~
    continuation_type *
    adverb_type *
    negation_position,
  mode = "mean.class"
)

predictions_raw <- as.data.frame(
  summary(
    prediction_grid,
    infer = c(TRUE, TRUE),
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
    "Could not identify prediction columns returned by emmeans.",
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
    "exp1_extended_model_predictions.csv"
  ),
  row.names = FALSE
)


# ---- Observed-rating figure --------------------------------------------------

raw_rating_table <- prop.table(
  table(
    rating = exp1_extended$rating,
    continuation_type =
      exp1_extended$continuation_type,
    adverb_type =
      exp1_extended$adverb_type,
    negation_position =
      exp1_extended$negation_position
  ),
  margin = c(2, 3, 4)
)

raw_rating_plot_data <- as.data.frame(
  raw_rating_table,
  stringsAsFactors = FALSE
)

names(raw_rating_plot_data)[
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
    palette = "YlGnBu"
  ) +
  ggplot2::labs(
    title =
      "Experiment I: observed rating distributions",
    subtitle =
      "Extended analysis including ideophones",
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
    "exp1_extended_raw_ratings.png"
  ),
  plot = raw_rating_plot,
  width = 11,
  height = 7,
  dpi = 300
)


# ---- Prediction figure -------------------------------------------------------

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
    "manner",
    "ideophone"
  ),
  labels = c(
    "Temporal",
    "Manner",
    "Ideophone"
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
      "Manner" = "#2A9D8F",
      "Ideophone" = "#6A4C93"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(1, 5)
  ) +
  ggplot2::labs(
    title =
      "Experiment I: extended model predictions",
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
    "exp1_extended_model_predictions.png"
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
    "temporal_observations",
    "manner_observations",
    "ideophone_observations",
    "scenarios",
    "response_scale",
    "model_family",
    "link",
    "fixed_effects",
    "random_effects",
    "contrast_coding",
    "rating_order",
    "pairwise_reporting"
  ),
  value = c(
    "Experiment I",
    "Extended analysis including ideophones",
    input_file,
    observed_participants,
    observed_observations,
    observed_adverb_counts[["temporal"]],
    observed_adverb_counts[["manner"]],
    observed_adverb_counts[["ideophone"]],
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
    "1 < 2 < 3 < 4 < 5",
    paste(
      "Unadjusted p-values reproduce the dissertation;",
      "Holm-adjusted p-values are also supplied"
    )
  ),
  stringsAsFactors = FALSE
)

write.csv(
  model_metadata,
  file.path(
    table_directory,
    "exp1_extended_model_metadata.csv"
  ),
  row.names = FALSE
)


# ---- Save model and session information -------------------------------------

saveRDS(
  extended_model,
  file.path(
    model_directory,
    "exp1_extended_clmm.rds"
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
cat("Experiment I extended analysis complete\n")
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
  "Temporal observations: ",
  observed_adverb_counts[["temporal"]],
  "\n",
  sep = ""
)

cat(
  "Manner observations: ",
  observed_adverb_counts[["manner"]],
  "\n",
  sep = ""
)

cat(
  "Ideophone observations: ",
  observed_adverb_counts[["ideophone"]],
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
