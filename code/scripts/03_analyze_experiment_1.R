# =============================================================================
# 03_analyze_experiment_1.R
#
# Purpose:
#   Reproduce the primary Experiment I analysis reported in Chapter 4.
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
# The later 04_exp1_extended_analysis.R script will extend this analysis
# by including ideophones as the third adverbial-type level.
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
  "exp1_primary"
)

table_directory <- file.path(
  "output",
  "tables",
  "exp1_primary"
)

model_directory <- file.path(
  "output",
  "models",
  "exp1_primary"
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


# ---- Select the primary analysis --------------------------------------------

exp1_primary <- exp1_all[
  exp1_all$included == TRUE &
    exp1_all$trial_type == "experimental" &
    exp1_all$adverb_type %in%
      c("temporal", "manner"),
  ,
  drop = FALSE
]

rownames(exp1_primary) <- NULL

observed_participants <- length(
  unique(exp1_primary$participant_id)
)

observed_observations <- nrow(
  exp1_primary
)

observed_scenarios <- length(
  unique(exp1_primary$scenario)
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

# The archived Experiment I allocation contains
# 200 temporal and 184 manner observations.
if (observed_observations != 384L) {
  stop(
    paste0(
      "Expected 384 primary-analysis observations ",
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
  any(is.na(exp1_primary$rating)) ||
    any(!exp1_primary$rating %in% 1:5)
) {
  stop(
    "Experiment I ratings must be integers from 1 to 5.",
    call. = FALSE
  )
}


# ---- Factor coding -----------------------------------------------------------

# Ratings are ordered from least natural (1)
# to most natural (5).
exp1_primary$rating_ordered <- ordered(
  exp1_primary$rating,
  levels = 1:5
)

exp1_primary$participant_id <- factor(
  exp1_primary$participant_id
)

exp1_primary$scenario <- factor(
  exp1_primary$scenario,
  levels = as.character(1:6)
)

exp1_primary$continuation_type <- factor(
  exp1_primary$continuation_type,
  levels = c(
    "match",
    "mismatch"
  )
)

exp1_primary$adverb_type <- factor(
  exp1_primary$adverb_type,
  levels = c(
    "temporal",
    "manner"
  )
)

exp1_primary$negation_position <- factor(
  exp1_primary$negation_position,
  levels = c(
    "second_clause",
    "first_clause"
  )
)

if (
  anyNA(exp1_primary$continuation_type) ||
    anyNA(exp1_primary$adverb_type) ||
    anyNA(exp1_primary$negation_position) ||
    anyNA(exp1_primary$scenario)
) {
  stop(
    "Unexpected or missing factor levels detected.",
    call. = FALSE
  )
}

# Sum coding reproduces the dissertation analysis.
contrasts(
  exp1_primary$continuation_type
) <- contr.sum(2)

contrasts(
  exp1_primary$adverb_type
) <- contr.sum(2)

contrasts(
  exp1_primary$negation_position
) <- contr.sum(2)


# ---- Cell counts -------------------------------------------------------------

cell_counts <- as.data.frame(
  table(
    continuation_type =
      exp1_primary$continuation_type,
    adverb_type =
      exp1_primary$adverb_type,
    negation_position =
      exp1_primary$negation_position
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
    "exp1_primary_cell_counts.csv"
  ),
  row.names = FALSE
)


# ---- Descriptive summary -----------------------------------------------------

group_variables <- interaction(
  exp1_primary$continuation_type,
  exp1_primary$adverb_type,
  exp1_primary$negation_position,
  drop = TRUE,
  lex.order = TRUE
)

descriptive_summary <- do.call(
  rbind,
  lapply(
    split(
      exp1_primary,
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
    "exp1_primary_descriptive_summary.csv"
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
  data = exp1_primary,
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
    "exp1_primary_model_summary.txt"
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
    "exp1_primary_fixed_effects.csv"
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
    "exp1_primary_random_effects.csv"
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
    "exp1_primary_model_diagnostics.csv"
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
    "exp1_primary_model_predictions.csv"
  ),
  row.names = FALSE
)


# ---- Raw-rating figure -------------------------------------------------------

raw_rating_table <- prop.table(
  table(
    rating = exp1_primary$rating,
    continuation_type =
      exp1_primary$continuation_type,
    adverb_type =
      exp1_primary$adverb_type,
    negation_position =
      exp1_primary$negation_position
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
      "Experiment I: observed rating distributions",
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
    "exp1_primary_raw_ratings.png"
  ),
  plot = raw_rating_plot,
  width = 10,
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
  primary_model,
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
    "exp1_primary_predicted_category_probabilities.csv"
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
  levels = c("temporal", "manner"),
  labels = c("Temporal", "Manner")
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
    limits = c(0, 1),
    labels = scales::percent_format(
      accuracy = 1
    ),
    expand = c(0, 0)
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
      "Experiment I: model-predicted rating probabilities",
    subtitle =
      "Primary cumulative-link mixed model",
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
    "exp1_primary_model_predictions.png"
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
    primary_model,
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
    "exp1_primary_conditional_contrasts.csv"
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
      "Experiment I: conditional model contrasts",
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
    "exp1_primary_conditional_contrasts.png"
  ),
  plot = contrast_plot,
  width = 12,
  height = 10,
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
    "Experiment I",
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
    "exp1_primary_model_metadata.csv"
  ),
  row.names = FALSE
)


# ---- Save model and session information -------------------------------------

saveRDS(
  primary_model,
  file.path(
    model_directory,
    "exp1_primary_clmm.rds"
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
cat("Experiment I primary analysis complete\n")
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
