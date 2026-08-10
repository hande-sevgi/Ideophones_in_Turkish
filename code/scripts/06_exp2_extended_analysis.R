# =============================================================================
# 06_exp2_extended_analysis.R
#
# Purpose:
#   Reproduce the extended Experiment II analysis reported in Chapter 4.
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
  "ggplot2",
  "scales"
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
  "exp2_extended"
)

table_directory <- file.path(
  "output",
  "tables",
  "exp2_extended"
)

model_directory <- file.path(
  "output",
  "models",
  "exp2_extended"
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


# ---- Select extended analysis data ------------------------------------------

exp2_extended <- exp2_all[
  exp2_all$included == TRUE &
    exp2_all$trial_type == "experimental" &
    exp2_all$adverb_type %in%
      c(
        "temporal",
        "manner",
        "ideophone"
      ),
  ,
  drop = FALSE
]

rownames(exp2_extended) <- NULL

observed_participants <- length(
  unique(exp2_extended$participant_id)
)

observed_observations <- nrow(
  exp2_extended
)

observed_scenarios <- length(
  unique(exp2_extended$scenario)
)

observed_word_orders <- unique(
  exp2_extended$word_order
)

if (
  length(observed_word_orders) != 1L ||
    observed_word_orders != "noncanonical"
) {
  stop(
    paste0(
      "Experiment II extended data must contain only ",
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

if (observed_observations != 588L) {
  stop(
    paste0(
      "Expected 588 experimental observations but found ",
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
  any(is.na(exp2_extended$rating)) ||
    any(!exp2_extended$rating %in% 1:5)
) {
  stop(
    "Experiment II ratings must be integers from 1 to 5.",
    call. = FALSE
  )
}


# ---- Verify archived allocation ---------------------------------------------

observed_adverb_counts <- table(
  exp2_extended$adverb_type
)

expected_adverb_counts <- c(
  temporal = 196L,
  manner = 196L,
  ideophone = 196L
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

exp2_extended$rating_ordered <- ordered(
  exp2_extended$rating,
  levels = 1:5
)

exp2_extended$participant_id <- factor(
  exp2_extended$participant_id
)

exp2_extended$scenario <- factor(
  exp2_extended$scenario,
  levels = as.character(1:6)
)

exp2_extended$continuation_type <- factor(
  exp2_extended$continuation_type,
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
exp2_extended$adverb_type <- factor(
  exp2_extended$adverb_type,
  levels = c(
    "temporal",
    "manner",
    "ideophone"
  )
)

exp2_extended$negation_position <- factor(
  exp2_extended$negation_position,
  levels = c(
    "second_clause",
    "first_clause"
  )
)

if (
  anyNA(exp2_extended$continuation_type) ||
    anyNA(exp2_extended$adverb_type) ||
    anyNA(exp2_extended$negation_position) ||
    anyNA(exp2_extended$scenario)
) {
  stop(
    "Unexpected or missing factor levels detected.",
    call. = FALSE
  )
}

contrasts(
  exp2_extended$continuation_type
) <- contr.sum(2)

contrasts(
  exp2_extended$adverb_type
) <- contr.sum(3)

contrasts(
  exp2_extended$negation_position
) <- contr.sum(2)


# ---- Cell counts -------------------------------------------------------------

cell_counts <- as.data.frame(
  table(
    continuation_type =
      exp2_extended$continuation_type,
    adverb_type =
      exp2_extended$adverb_type,
    negation_position =
      exp2_extended$negation_position
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
    "exp2_extended_cell_counts.csv"
  ),
  row.names = FALSE
)


# ---- Participant allocation table -------------------------------------------

participant_adverb_counts <- as.data.frame(
  table(
    participant_id =
      exp2_extended$participant_id,
    adverb_type =
      exp2_extended$adverb_type
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
    "exp2_extended_participant_adverb_counts.csv"
  ),
  row.names = FALSE
)


# ---- Descriptive summary -----------------------------------------------------

group_variables <- interaction(
  exp2_extended$continuation_type,
  exp2_extended$adverb_type,
  exp2_extended$negation_position,
  drop = TRUE,
  lex.order = TRUE
)

descriptive_summary <- do.call(
  rbind,
  lapply(
    split(
      exp2_extended,
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
    "exp2_extended_descriptive_summary.csv"
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
  data = exp2_extended,
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
    "exp2_extended_model_summary.txt"
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
    "exp2_extended_fixed_effects.csv"
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
    "exp2_extended_random_effects.csv"
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
    "exp2_extended_model_diagnostics.csv"
  ),
  row.names = FALSE
)


# ---- Pairwise adverb comparisons --------------------------------------------

# The dissertation reports marginal pairwise comparisons
# among the three adverb types for Experiment II.
# We preserve the unadjusted p-values and also add
# Holm-adjusted p-values for supplementary reporting.

adverb_emmeans <- emmeans::emmeans(
  extended_model,
  specs = ~ adverb_type,
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
  stats::p.adjust(
    pairwise_unadjusted$p.value,
    method = "holm"
  )

write.csv(
  pairwise_unadjusted,
  file.path(
    table_directory,
    "exp2_extended_adverb_contrasts.csv"
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
    "exp2_extended_model_predictions.csv"
  ),
  row.names = FALSE
)


# ---- Observed-rating figure --------------------------------------------------

raw_rating_table <- prop.table(
  table(
    rating = exp2_extended$rating,
    continuation_type =
      exp2_extended$continuation_type,
    adverb_type =
      exp2_extended$adverb_type,
    negation_position =
      exp2_extended$negation_position
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
      "Experiment II: observed rating distributions",
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
    "exp2_extended_raw_ratings.png"
  ),
  plot = raw_rating_plot,
  width = 11,
  height = 7,
  dpi = 300
)


# ---- Ordinal predicted-category probabilities -------------------------------

# A line joining categorical conditions can imply continuity that is not
# present in the design. The primary model figure therefore displays the
# predicted probability of every ordinal response category (1--5).

identify_emmeans_column <- function(
  data,
  candidates,
  description
) {
  result <- intersect(
    candidates,
    names(data)
  )[1]

  if (is.na(result)) {
    stop(
      paste0(
        "Could not identify the ",
        description,
        " column returned by emmeans. Available columns: ",
        paste(names(data), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  result
}

probability_grid <- emmeans::emmeans(
  extended_model,
  specs = ~
    rating_ordered |
    continuation_type *
    adverb_type *
    negation_position,
  mode = "prob"
)

probability_raw <- as.data.frame(
  summary(
    probability_grid,
    infer = c(TRUE, FALSE),
    level = 0.95
  )
)

probability_column <- identify_emmeans_column(
  probability_raw,
  c("prob", "response", "emmean"),
  "predicted probability"
)

probability_lower_column <- identify_emmeans_column(
  probability_raw,
  c("asymp.LCL", "lower.CL", "LCL"),
  "lower confidence limit"
)

probability_upper_column <- identify_emmeans_column(
  probability_raw,
  c("asymp.UCL", "upper.CL", "UCL"),
  "upper confidence limit"
)

category_probabilities <- data.frame(
  rating = as.character(
    probability_raw$rating_ordered
  ),
  continuation_type =
    probability_raw$continuation_type,
  adverb_type =
    probability_raw$adverb_type,
  negation_position =
    probability_raw$negation_position,
  predicted_probability =
    probability_raw[[probability_column]],
  std_error = probability_raw$SE,
  lower_95 =
    probability_raw[[probability_lower_column]],
  upper_95 =
    probability_raw[[probability_upper_column]],
  stringsAsFactors = FALSE
)

probability_sums <- stats::aggregate(
  predicted_probability ~
    continuation_type +
    adverb_type +
    negation_position,
  data = category_probabilities,
  FUN = sum
)

if (
  any(
    abs(
      probability_sums$predicted_probability - 1
    ) > 1e-5
  )
) {
  stop(
    "Predicted rating-category probabilities do not sum to one.",
    call. = FALSE
  )
}

write.csv(
  category_probabilities,
  file.path(
    table_directory,
    "exp2_extended_predicted_category_probabilities.csv"
  ),
  row.names = FALSE
)


# ---- Ordinal probability figure ---------------------------------------------

probability_plot_data <- category_probabilities

probability_plot_data$rating <- factor(
  probability_plot_data$rating,
  levels = as.character(1:5)
)

probability_plot_data$continuation_type <- factor(
  probability_plot_data$continuation_type,
  levels = c("match", "mismatch"),
  labels = c("Match", "Mismatch")
)

probability_plot_data$adverb_type <- factor(
  probability_plot_data$adverb_type,
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

probability_plot_data$negation_position <- factor(
  probability_plot_data$negation_position,
  levels = c(
    "second_clause",
    "first_clause"
  ),
  labels = c(
    "Negation in second clause",
    "Negation in first clause"
  )
)

probability_plot <- ggplot2::ggplot(
  probability_plot_data,
  ggplot2::aes(
    x = continuation_type,
    y = predicted_probability,
    fill = rating
  )
) +
  ggplot2::geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.25
  ) +
  ggplot2::facet_grid(
    negation_position ~ adverb_type
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.01)
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1)
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "1" = "#B84D6A",
      "2" = "#D4977C",
      "3" = "#D9C8A9",
      "4" = "#91B39A",
      "5" = "#16857F"
    ),
    drop = FALSE
  ) +
  ggplot2::labs(
    title =
      "Experiment II: model-predicted rating probabilities",
    subtitle =
      "Extended cumulative-link mixed model",
    x = "Continuation type",
    y = "Predicted probability",
    fill = "Rating"
  ) +
  ggplot2::theme_minimal(
    base_size = 13
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor =
      ggplot2::element_blank(),
    panel.grid.major.x =
      ggplot2::element_blank(),
    strip.text =
      ggplot2::element_text(face = "bold")
  )

# This overwrites the former line plot with the ordinal probability plot.
ggplot2::ggsave(
  filename = file.path(
    figure_directory,
    "exp2_extended_model_predictions.png"
  ),
  plot = probability_plot,
  width = 10,
  height = 7,
  dpi = 300
)


# ---- Conditional model contrasts --------------------------------------------

# Conditional comparisons are estimated because the fitted model contains
# interactions. The confidence intervals are unadjusted 95% intervals.
# Holm correction is applied to the p-values within each contrast family.

create_contrast_family <- function(
  focal_predictor,
  conditioning_predictors,
  effect_label
) {
  specifications <- stats::as.formula(
    paste0(
      "~ ",
      focal_predictor,
      " | ",
      paste(
        conditioning_predictors,
        collapse = " * "
      )
    )
  )

  marginal_means <- emmeans::emmeans(
    extended_model,
    specs = specifications,
    mode = "latent"
  )

  contrast_result <- emmeans::contrast(
    marginal_means,
    method = "pairwise",
    adjust = "none"
  )

  contrast_raw <- as.data.frame(
    summary(
      contrast_result,
      infer = c(TRUE, TRUE),
      level = 0.95
    )
  )

  lower_column <- identify_emmeans_column(
    contrast_raw,
    c("asymp.LCL", "lower.CL", "LCL"),
    "lower confidence limit"
  )

  upper_column <- identify_emmeans_column(
    contrast_raw,
    c("asymp.UCL", "upper.CL", "UCL"),
    "upper confidence limit"
  )

  statistic_column <- identify_emmeans_column(
    contrast_raw,
    c("z.ratio", "t.ratio"),
    "test statistic"
  )

  context <- apply(
    contrast_raw[
      ,
      conditioning_predictors,
      drop = FALSE
    ],
    1,
    function(row) {
      paste(
        paste0(
          gsub(
            "_",
            " ",
            conditioning_predictors
          ),
          " = ",
          gsub("_", " ", row)
        ),
        collapse = "; "
      )
    }
  )

  output <- data.frame(
    effect = effect_label,
    contrast = contrast_raw$contrast,
    context = context,
    estimate = contrast_raw$estimate,
    std_error = contrast_raw$SE,
    lower_95 = contrast_raw[[lower_column]],
    upper_95 = contrast_raw[[upper_column]],
    statistic = contrast_raw[[statistic_column]],
    p_value = contrast_raw$p.value,
    stringsAsFactors = FALSE
  )

  output$p_value_holm <- stats::p.adjust(
    output$p_value,
    method = "holm"
  )

  output
}

continuation_contrasts <- create_contrast_family(
  focal_predictor = "continuation_type",
  conditioning_predictors = c(
    "adverb_type",
    "negation_position"
  ),
  effect_label = "Continuation contrast"
)

adverb_contrasts <- create_contrast_family(
  focal_predictor = "adverb_type",
  conditioning_predictors = c(
    "continuation_type",
    "negation_position"
  ),
  effect_label = "Adverb-type contrast"
)

negation_contrasts <- create_contrast_family(
  focal_predictor = "negation_position",
  conditioning_predictors = c(
    "continuation_type",
    "adverb_type"
  ),
  effect_label = "Negation-position contrast"
)

conditional_contrasts <- rbind(
  continuation_contrasts,
  adverb_contrasts,
  negation_contrasts
)

conditional_contrasts$significance <- ifelse(
  conditional_contrasts$p_value_holm < 0.05,
  "Holm-adjusted p < .05",
  "Holm-adjusted p >= .05"
)

conditional_contrasts$display_label <- paste0(
  conditional_contrasts$effect,
  ": ",
  gsub(
    "_",
    " ",
    conditional_contrasts$contrast
  ),
  "\n",
  conditional_contrasts$context
)

write.csv(
  conditional_contrasts,
  file.path(
    table_directory,
    "exp2_extended_conditional_contrasts.csv"
  ),
  row.names = FALSE
)


# ---- Conditional-contrast figure --------------------------------------------

conditional_contrasts$display_label <- factor(
  conditional_contrasts$display_label,
  levels = rev(
    unique(
      conditional_contrasts$display_label
    )
  )
)

contrast_plot <- ggplot2::ggplot(
  conditional_contrasts,
  ggplot2::aes(
    x = display_label,
    y = estimate,
    ymin = lower_95,
    ymax = upper_95,
    color = significance
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "grey45",
    linetype = "dashed"
  ) +
  ggplot2::geom_errorbar(
    width = 0.18,
    linewidth = 0.65
  ) +
  ggplot2::geom_point(
    size = 2.8
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_color_manual(
    values = c(
      "Holm-adjusted p < .05" =
        "#007C78",
      "Holm-adjusted p >= .05" =
        "#777777"
    )
  ) +
  ggplot2::labs(
    title =
      "Experiment II: conditional model contrasts",
    subtitle = paste(
      "Estimates and unadjusted 95% confidence intervals",
      "on the latent log-odds scale;",
      "colour represents Holm-adjusted p-values"
    ),
    x = NULL,
    y =
      "Estimated contrast (latent log odds)",
    color = NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor =
      ggplot2::element_blank(),
    panel.grid.major.y =
      ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(
    figure_directory,
    "exp2_extended_conditional_contrasts.png"
  ),
  plot = contrast_plot,
  width = 12,
  height = 14,
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
    "Experiment II",
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
    "exp2_extended_model_metadata.csv"
  ),
  row.names = FALSE
)


# ---- Save model and session information -------------------------------------

saveRDS(
  extended_model,
  file.path(
    model_directory,
    "exp2_extended_clmm.rds"
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
cat("Experiment II extended analysis complete\n")
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
