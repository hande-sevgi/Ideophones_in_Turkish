# =============================================================================
# 07_analyze_experiment_3.R
#
# Purpose:
#   Reproduce the Experiment III analyses reported in Chapter 4.
#
# Analyses:
#   1. Primary ideophone-group model:
#        rating ~ adverb type * polarity
#   2. Full supplementary model:
#        rating ~ adverb type * iconicity condition * polarity
#
# Both models use continuous 0--100 ratings, treatment-coded predictors,
# and random intercepts for participant and scenario.
# =============================================================================


# ---- Required packages -------------------------------------------------------

required_packages <- c(
  "lme4",
  "lmerTest",
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
  "exp3_monoclause.csv"
)

figure_directory <- file.path(
  "output",
  "figures",
  "exp3"
)

table_directory <- file.path(
  "output",
  "tables",
  "exp3"
)

model_directory <- file.path(
  "output",
  "models",
  "exp3"
)

for (
  directory in c(
    figure_directory,
    table_directory,
    model_directory
  )
) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ---- Read data ---------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Processed Experiment III data not found:\n",
      input_file,
      "\n\nRun 02_prepare_processed_data.R first."
    ),
    call. = FALSE
  )
}

exp3_all <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  na.strings = c("NA")
)


# ---- Validate processed data -------------------------------------------------

required_columns <- c(
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
  "attention_check_failures",
  "included",
  "exclusion_reason"
)

missing_columns <- setdiff(
  required_columns,
  names(exp3_all)
)

if (length(missing_columns) > 0L) {
  stop(
    paste0(
      "Experiment III processed data are missing: ",
      paste(missing_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

input_participants <- length(
  unique(exp3_all$participant_id)
)

included_participants <- length(
  unique(
    exp3_all$participant_id[
      exp3_all$included == TRUE
    ]
  )
)

excluded_participants <- length(
  unique(
    exp3_all$participant_id[
      exp3_all$included == FALSE
    ]
  )
)

if (nrow(exp3_all) != 2784L) {
  stop(
    paste0(
      "Expected 2784 processed rows but found ",
      nrow(exp3_all),
      "."
    ),
    call. = FALSE
  )
}

if (input_participants != 116L) {
  stop(
    paste0(
      "Expected 116 input participants but found ",
      input_participants,
      "."
    ),
    call. = FALSE
  )
}

if (
  included_participants != 114L ||
    excluded_participants != 2L
) {
  stop(
    paste0(
      "Expected 114 included and 2 excluded participants but found ",
      included_participants,
      " included and ",
      excluded_participants,
      " excluded."
    ),
    call. = FALSE
  )
}


# ---- Select analysis datasets -----------------------------------------------

exp3_full <- exp3_all[
  exp3_all$included == TRUE &
    exp3_all$trial_type == "experimental",
  ,
  drop = FALSE
]

rownames(exp3_full) <- NULL

exp3_primary <- exp3_full[
  exp3_full$iconicity_condition == "ideophone",
  ,
  drop = FALSE
]

rownames(exp3_primary) <- NULL

if (nrow(exp3_full) != 1824L) {
  stop(
    paste0(
      "Expected 1824 included experimental observations but found ",
      nrow(exp3_full),
      "."
    ),
    call. = FALSE
  )
}

if (nrow(exp3_primary) != 912L) {
  stop(
    paste0(
      "Expected 912 primary ideophone observations but found ",
      nrow(exp3_primary),
      "."
    ),
    call. = FALSE
  )
}

if (
  any(is.na(exp3_full$rating)) ||
    any(exp3_full$rating < 0) ||
    any(exp3_full$rating > 100)
) {
  stop(
    "Experiment III ratings must be complete and range from 0 to 100.",
    call. = FALSE
  )
}

observed_scenarios <- length(
  unique(exp3_full$scenario)
)

if (observed_scenarios != 8L) {
  stop(
    paste0(
      "Expected 8 scenarios but found ",
      observed_scenarios,
      "."
    ),
    call. = FALSE
  )
}


# ---- Factor coding -----------------------------------------------------------

prepare_factors <- function(data) {
  data$participant_id <- factor(
    data$participant_id
  )

  data$scenario <- factor(
    data$scenario,
    levels = as.character(1:8)
  )

  # Treatment coding reproduces the dissertation analysis.
  # Reference levels are manner, affirmative, and ideophone.
  data$adverb_type <- factor(
    data$adverb_type,
    levels = c(
      "manner",
      "converbial",
      "reduplication",
      "temporal"
    )
  )

  data$polarity <- factor(
    data$polarity,
    levels = c(
      "affirmative",
      "negative"
    )
  )

  data$iconicity_condition <- factor(
    data$iconicity_condition,
    levels = c(
      "ideophone",
      "nonideophone"
    )
  )

  if (
    anyNA(data$participant_id) ||
      anyNA(data$scenario) ||
      anyNA(data$adverb_type) ||
      anyNA(data$polarity) ||
      anyNA(data$iconicity_condition)
  ) {
    stop(
      "Unexpected or missing Experiment III factor levels detected.",
      call. = FALSE
    )
  }

  contrasts(data$adverb_type) <-
    stats::contr.treatment(
      levels(data$adverb_type),
      base = 1
    )

  contrasts(data$polarity) <-
    stats::contr.treatment(
      levels(data$polarity),
      base = 1
    )

  contrasts(data$iconicity_condition) <-
    stats::contr.treatment(
      levels(data$iconicity_condition),
      base = 1
    )

  data
}

exp3_full <- prepare_factors(exp3_full)
exp3_primary <- prepare_factors(exp3_primary)


# ---- Sample flow and endpoint counts ----------------------------------------

sample_flow <- data.frame(
  input_rows = nrow(exp3_all),
  input_participants = input_participants,
  included_participants = included_participants,
  excluded_participants = excluded_participants,
  primary_observations = nrow(exp3_primary),
  full_model_observations = nrow(exp3_full),
  stringsAsFactors = FALSE
)

write.csv(
  sample_flow,
  file.path(
    table_directory,
    "exp3_sample_flow.csv"
  ),
  row.names = FALSE
)

endpoint_counts <- data.frame(
  analysis = c(
    "primary_ideophone",
    "full"
  ),
  observations = c(
    nrow(exp3_primary),
    nrow(exp3_full)
  ),
  exact_zero = c(
    sum(exp3_primary$rating == 0),
    sum(exp3_full$rating == 0)
  ),
  exact_hundred = c(
    sum(exp3_primary$rating == 100),
    sum(exp3_full$rating == 100)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  endpoint_counts,
  file.path(
    table_directory,
    "exp3_endpoint_counts.csv"
  ),
  row.names = FALSE
)


# ---- Cell counts -------------------------------------------------------------

cell_counts <- as.data.frame(
  table(
    iconicity_condition =
      exp3_full$iconicity_condition,
    adverb_type = exp3_full$adverb_type,
    polarity = exp3_full$polarity
  ),
  stringsAsFactors = FALSE
)

names(cell_counts)[
  names(cell_counts) == "Freq"
] <- "observations"

if (any(cell_counts$observations != 114L)) {
  stop(
    "Experiment III experimental cells must each contain 114 observations.",
    call. = FALSE
  )
}

write.csv(
  cell_counts,
  file.path(
    table_directory,
    "exp3_cell_counts.csv"
  ),
  row.names = FALSE
)


# ---- Descriptive summary -----------------------------------------------------

descriptive_groups <- interaction(
  exp3_full$iconicity_condition,
  exp3_full$adverb_type,
  exp3_full$polarity,
  drop = TRUE,
  lex.order = TRUE
)

descriptive_summary <- do.call(
  rbind,
  lapply(
    split(exp3_full, descriptive_groups),
    function(group_data) {
      data.frame(
        iconicity_condition =
          as.character(
            group_data$iconicity_condition[1]
          ),
        adverb_type =
          as.character(
            group_data$adverb_type[1]
          ),
        polarity =
          as.character(
            group_data$polarity[1]
          ),
        observations = nrow(group_data),
        participants = length(
          unique(group_data$participant_id)
        ),
        mean_rating = mean(group_data$rating),
        sd_rating = stats::sd(group_data$rating),
        se_rating = stats::sd(group_data$rating) /
          sqrt(nrow(group_data)),
        median_rating = stats::median(group_data$rating),
        q1_rating = stats::quantile(
          group_data$rating,
          probabilities = 0.25,
          names = FALSE
        ),
        q3_rating = stats::quantile(
          group_data$rating,
          probabilities = 0.75,
          names = FALSE
        ),
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
    "exp3_descriptive_summary.csv"
  ),
  row.names = FALSE
)


# ---- Fit dissertation models ------------------------------------------------

primary_formula <-
  rating ~
  adverb_type * polarity +
  (1 | participant_id) +
  (1 | scenario)

primary_model <- lmerTest::lmer(
  formula = primary_formula,
  data = exp3_primary,
  REML = TRUE
)

full_formula <-
  rating ~
  adverb_type *
  iconicity_condition *
  polarity +
  (1 | participant_id) +
  (1 | scenario)

full_model <- lmerTest::lmer(
  formula = full_formula,
  data = exp3_full,
  REML = TRUE
)


# ---- Model-output helpers ----------------------------------------------------

write_fixed_effects <- function(
  model,
  output_file
) {
  coefficient_table <- as.data.frame(
    stats::coef(summary(model))
  )

  coefficient_table$term <- rownames(
    coefficient_table
  )

  output <- data.frame(
    term = coefficient_table$term,
    estimate = coefficient_table[["Estimate"]],
    std_error = coefficient_table[["Std. Error"]],
    degrees_freedom = coefficient_table[["df"]],
    t_value = coefficient_table[["t value"]],
    p_value = coefficient_table[["Pr(>|t|)"]],
    stringsAsFactors = FALSE
  )

  write.csv(
    output,
    output_file,
    row.names = FALSE
  )

  output
}

write_random_effects <- function(
  model,
  output_file
) {
  output <- as.data.frame(
    lme4::VarCorr(model)
  )

  write.csv(
    output,
    output_file,
    row.names = FALSE
  )

  output
}

model_diagnostics <- function(model) {
  convergence_messages <-
    model@optinfo$conv$lme4$messages

  convergence_ok <-
    is.null(convergence_messages)

  maximum_absolute_gradient <- tryCatch(
    max(
      abs(
        model@optinfo$derivs$gradient
      )
    ),
    error = function(error) {
      NA_real_
    }
  )

  positive_definite_hessian <- tryCatch(
    {
      hessian <- model@optinfo$derivs$Hessian
      eigenvalues <- eigen(
        (hessian + t(hessian)) / 2,
        symmetric = TRUE,
        only.values = TRUE
      )$values
      all(eigenvalues > 0)
    },
    error = function(error) {
      NA
    }
  )

  data.frame(
    diagnostic = c(
      "observations",
      "convergence_ok",
      "convergence_messages",
      "maximum_absolute_gradient",
      "positive_definite_hessian",
      "singular_fit",
      "residual_sigma",
      "log_likelihood",
      "AIC",
      "BIC"
    ),
    value = c(
      stats::nobs(model),
      convergence_ok,
      if (convergence_ok) {
        "none"
      } else {
        paste(
          convergence_messages,
          collapse = "; "
        )
      },
      maximum_absolute_gradient,
      positive_definite_hessian,
      lme4::isSingular(
        model,
        tol = 1e-4
      ),
      stats::sigma(model),
      as.numeric(stats::logLik(model)),
      stats::AIC(model),
      stats::BIC(model)
    ),
    stringsAsFactors = FALSE
  )
}


# ---- Save model summaries and coefficients ----------------------------------

capture.output(
  summary(primary_model),
  file = file.path(
    table_directory,
    "exp3_primary_model_summary.txt"
  )
)

capture.output(
  summary(full_model),
  file = file.path(
    table_directory,
    "exp3_full_model_summary.txt"
  )
)

primary_fixed_effects <- write_fixed_effects(
  primary_model,
  file.path(
    table_directory,
    "exp3_primary_fixed_effects.csv"
  )
)

full_fixed_effects <- write_fixed_effects(
  full_model,
  file.path(
    table_directory,
    "exp3_full_fixed_effects.csv"
  )
)

primary_random_effects <- write_random_effects(
  primary_model,
  file.path(
    table_directory,
    "exp3_primary_random_effects.csv"
  )
)

full_random_effects <- write_random_effects(
  full_model,
  file.path(
    table_directory,
    "exp3_full_random_effects.csv"
  )
)

primary_diagnostics <- model_diagnostics(
  primary_model
)

full_diagnostics <- model_diagnostics(
  full_model
)

write.csv(
  primary_diagnostics,
  file.path(
    table_directory,
    "exp3_primary_model_diagnostics.csv"
  ),
  row.names = FALSE
)

write.csv(
  full_diagnostics,
  file.path(
    table_directory,
    "exp3_full_model_diagnostics.csv"
  ),
  row.names = FALSE
)


# ---- Estimated marginal means and pairwise comparisons ----------------------

identify_column <- function(
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
        " column returned by emmeans."
      ),
      call. = FALSE
    )
  }

  result
}

prepare_predictions <- function(
  model,
  specifications
) {
  prediction_grid <- emmeans::emmeans(
    model,
    specs = specifications
  )

  prediction_raw <- as.data.frame(
    summary(
      prediction_grid,
      infer = c(TRUE, FALSE),
      level = 0.95
    )
  )

  estimate_column <- identify_column(
    prediction_raw,
    c("emmean", "response"),
    "prediction"
  )

  lower_column <- identify_column(
    prediction_raw,
    c("lower.CL", "asymp.LCL", "LCL"),
    "lower confidence limit"
  )

  upper_column <- identify_column(
    prediction_raw,
    c("upper.CL", "asymp.UCL", "UCL"),
    "upper confidence limit"
  )

  prediction_raw$predicted_rating <-
    prediction_raw[[estimate_column]]
  prediction_raw$lower_95 <-
    prediction_raw[[lower_column]]
  prediction_raw$upper_95 <-
    prediction_raw[[upper_column]]

  prediction_raw
}

primary_predictions <- prepare_predictions(
  primary_model,
  ~ adverb_type * polarity
)

full_predictions <- prepare_predictions(
  full_model,
  ~ adverb_type *
    iconicity_condition *
    polarity
)

write.csv(
  primary_predictions,
  file.path(
    table_directory,
    "exp3_primary_model_predictions.csv"
  ),
  row.names = FALSE
)

write.csv(
  full_predictions,
  file.path(
    table_directory,
    "exp3_full_model_predictions.csv"
  ),
  row.names = FALSE
)

primary_adverb_emmeans <- emmeans::emmeans(
  primary_model,
  specs = ~ adverb_type | polarity
)

primary_pairwise <- as.data.frame(
  summary(
    emmeans::contrast(
      primary_adverb_emmeans,
      method = "pairwise",
      adjust = "tukey"
    ),
    infer = c(TRUE, TRUE),
    level = 0.95
  )
)

write.csv(
  primary_pairwise,
  file.path(
    table_directory,
    "exp3_primary_adverb_contrasts.csv"
  ),
  row.names = FALSE
)

full_adverb_emmeans <- emmeans::emmeans(
  full_model,
  specs = ~
    adverb_type |
    polarity *
    iconicity_condition
)

full_pairwise <- as.data.frame(
  summary(
    emmeans::contrast(
      full_adverb_emmeans,
      method = "pairwise",
      adjust = "tukey"
    ),
    infer = c(TRUE, TRUE),
    level = 0.95
  )
)

write.csv(
  full_pairwise,
  file.path(
    table_directory,
    "exp3_full_adverb_contrasts.csv"
  ),
  row.names = FALSE
)


# ---- Observed-rating figure --------------------------------------------------

observed_plot_data <- exp3_full

observed_plot_data$adverb_type <- factor(
  observed_plot_data$adverb_type,
  levels = c(
    "manner",
    "converbial",
    "reduplication",
    "temporal"
  ),
  labels = c(
    "Manner",
    "Converbial",
    "Reduplication",
    "Temporal"
  )
)

observed_plot_data$polarity <- factor(
  observed_plot_data$polarity,
  levels = c("affirmative", "negative"),
  labels = c("Affirmative", "Negative")
)

observed_plot_data$iconicity_condition <- factor(
  observed_plot_data$iconicity_condition,
  levels = c("ideophone", "nonideophone"),
  labels = c("Ideophone stimuli", "Non-ideophone stimuli")
)

observed_plot <- ggplot2::ggplot(
  observed_plot_data,
  ggplot2::aes(
    x = adverb_type,
    y = rating,
    fill = polarity
  )
) +
  ggplot2::geom_boxplot(
    position = ggplot2::position_dodge(
      width = 0.78
    ),
    width = 0.68,
    outlier.alpha = 0.18,
    outlier.size = 0.8
  ) +
  ggplot2::facet_wrap(
    ~ iconicity_condition
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Affirmative" = "#E76F51",
      "Negative" = "#2A9D8F"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 100)
  ) +
  ggplot2::labs(
    title = "Experiment III: observed rating distributions",
    subtitle = "Included experimental observations",
    x = "Adverb type",
    y = "Rating",
    fill = "Polarity"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  file.path(
    figure_directory,
    "exp3_observed_ratings.png"
  ),
  observed_plot,
  width = 11,
  height = 6,
  dpi = 300
)


# ---- Model-prediction figures ------------------------------------------------

format_prediction_factors <- function(
  data,
  include_iconicity = FALSE
) {
  data$adverb_type <- factor(
    data$adverb_type,
    levels = c(
      "manner",
      "converbial",
      "reduplication",
      "temporal"
    ),
    labels = c(
      "Manner",
      "Converbial",
      "Reduplication",
      "Temporal"
    )
  )

  data$polarity <- factor(
    data$polarity,
    levels = c("affirmative", "negative"),
    labels = c("Affirmative", "Negative")
  )

  if (include_iconicity) {
    data$iconicity_condition <- factor(
      data$iconicity_condition,
      levels = c(
        "ideophone",
        "nonideophone"
      ),
      labels = c(
        "Ideophone stimuli",
        "Non-ideophone stimuli"
      )
    )
  }

  data
}

primary_prediction_plot_data <-
  format_prediction_factors(
    primary_predictions
  )

primary_prediction_plot <- ggplot2::ggplot(
  primary_prediction_plot_data,
  ggplot2::aes(
    x = adverb_type,
    y = predicted_rating,
    ymin = lower_95,
    ymax = upper_95,
    color = polarity
  )
) +
  ggplot2::geom_errorbar(
    position = ggplot2::position_dodge(
      width = 0.45
    ),
    width = 0.14,
    linewidth = 0.7
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.45
    ),
    size = 3
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Affirmative" = "#E76F51",
      "Negative" = "#2A9D8F"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 100)
  ) +
  ggplot2::labs(
    title = "Experiment III: primary model predictions",
    subtitle = "Ideophone stimuli; point estimates and 95% confidence intervals",
    x = "Adverb type",
    y = "Model-predicted rating",
    color = "Polarity"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

ggplot2::ggsave(
  file.path(
    figure_directory,
    "exp3_primary_model_predictions.png"
  ),
  primary_prediction_plot,
  width = 10,
  height = 6,
  dpi = 300
)

full_prediction_plot_data <-
  format_prediction_factors(
    full_predictions,
    include_iconicity = TRUE
  )

full_prediction_plot <- ggplot2::ggplot(
  full_prediction_plot_data,
  ggplot2::aes(
    x = adverb_type,
    y = predicted_rating,
    ymin = lower_95,
    ymax = upper_95,
    color = polarity
  )
) +
  ggplot2::geom_errorbar(
    position = ggplot2::position_dodge(
      width = 0.45
    ),
    width = 0.14,
    linewidth = 0.7
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.45
    ),
    size = 3
  ) +
  ggplot2::facet_wrap(
    ~ iconicity_condition
  ) +
  ggplot2::scale_color_manual(
    values = c(
      "Affirmative" = "#E76F51",
      "Negative" = "#2A9D8F"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 100)
  ) +
  ggplot2::labs(
    title = "Experiment III: full model predictions",
    subtitle = "Point estimates and 95% confidence intervals",
    x = "Adverb type",
    y = "Model-predicted rating",
    color = "Polarity"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "top",
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(
  file.path(
    figure_directory,
    "exp3_full_model_predictions.png"
  ),
  full_prediction_plot,
  width = 11,
  height = 6,
  dpi = 300
)


# ---- Pairwise-contrast figures ----------------------------------------------

prepare_contrast_plot_data <- function(
  contrast_data,
  context_columns
) {
  lower_column <- identify_column(
    contrast_data,
    c("lower.CL", "asymp.LCL", "LCL"),
    "contrast lower confidence limit"
  )

  upper_column <- identify_column(
    contrast_data,
    c("upper.CL", "asymp.UCL", "UCL"),
    "contrast upper confidence limit"
  )

  context <- apply(
    contrast_data[, context_columns, drop = FALSE],
    1,
    function(row) {
      paste(
        paste0(
          gsub("_", " ", context_columns),
          " = ",
          gsub("_", " ", row)
        ),
        collapse = "; "
      )
    }
  )

  output <- data.frame(
    contrast = contrast_data$contrast,
    context = context,
    estimate = contrast_data$estimate,
    lower_95 = contrast_data[[lower_column]],
    upper_95 = contrast_data[[upper_column]],
    p_value = contrast_data$p.value,
    stringsAsFactors = FALSE
  )

  output$significance <- ifelse(
    output$p_value < 0.05,
    "Tukey-adjusted p < .05",
    "Tukey-adjusted p >= .05"
  )

  output$display_label <- paste0(
    gsub("_", " ", output$contrast),
    "\n",
    output$context
  )

  output
}

make_contrast_plot <- function(
  plot_data,
  title,
  output_file,
  height
) {
  plot_data$display_label <- factor(
    plot_data$display_label,
    levels = rev(
      unique(plot_data$display_label)
    )
  )

  plot <- ggplot2::ggplot(
    plot_data,
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
    ggplot2::geom_point(size = 2.8) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(
      values = c(
        "Tukey-adjusted p < .05" = "#007C78",
        "Tukey-adjusted p >= .05" = "#777777"
      )
    ) +
    ggplot2::labs(
      title = title,
      subtitle =
        "Estimated rating differences with Tukey-adjusted 95% confidence intervals",
      x = NULL,
      y = "Estimated rating difference",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    output_file,
    plot,
    width = 12,
    height = height,
    dpi = 300
  )
}

primary_contrast_plot_data <-
  prepare_contrast_plot_data(
    primary_pairwise,
    "polarity"
  )

make_contrast_plot(
  primary_contrast_plot_data,
  "Experiment III: ideophone-group adverb contrasts",
  file.path(
    figure_directory,
    "exp3_primary_adverb_contrasts.png"
  ),
  height = 9
)

full_contrast_plot_data <-
  prepare_contrast_plot_data(
    full_pairwise,
    c(
      "polarity",
      "iconicity_condition"
    )
  )

make_contrast_plot(
  full_contrast_plot_data,
  "Experiment III: full-model adverb contrasts",
  file.path(
    figure_directory,
    "exp3_full_adverb_contrasts.png"
  ),
  height = 14
)


# ---- Residual diagnostic figures --------------------------------------------

write_diagnostic_figures <- function(
  model,
  prefix,
  model_label
) {
  diagnostic_data <- data.frame(
    fitted = stats::fitted(model),
    standardized_residual =
      stats::residuals(model) /
      stats::sigma(model)
  )

  residual_plot <- ggplot2::ggplot(
    diagnostic_data,
    ggplot2::aes(
      x = fitted,
      y = standardized_residual
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      color = "grey45",
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      alpha = 0.3,
      size = 1.2,
      color = "#2A6F97"
    ) +
    ggplot2::labs(
      title = paste0(
        "Experiment III: ",
        model_label,
        " residuals versus fitted values"
      ),
      x = "Fitted rating",
      y = "Standardized residual"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  ggplot2::ggsave(
    file.path(
      figure_directory,
      paste0(prefix, "_residuals_vs_fitted.png")
    ),
    residual_plot,
    width = 8,
    height = 6,
    dpi = 300
  )

  qq_values <- stats::qqnorm(
    diagnostic_data$standardized_residual,
    plot.it = FALSE
  )

  qq_data <- data.frame(
    theoretical = qq_values$x,
    observed = qq_values$y
  )

  qq_plot <- ggplot2::ggplot(
    qq_data,
    ggplot2::aes(
      x = theoretical,
      y = observed
    )
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      color = "#D1495B",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      alpha = 0.4,
      size = 1.2,
      color = "#2A6F97"
    ) +
    ggplot2::labs(
      title = paste0(
        "Experiment III: ",
        model_label,
        " residual Q-Q plot"
      ),
      x = "Theoretical quantile",
      y = "Standardized residual"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  ggplot2::ggsave(
    file.path(
      figure_directory,
      paste0(prefix, "_residual_qq.png")
    ),
    qq_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
}

write_diagnostic_figures(
  primary_model,
  "exp3_primary",
  "primary model"
)

write_diagnostic_figures(
  full_model,
  "exp3_full",
  "full model"
)


# ---- Model metadata ----------------------------------------------------------

model_metadata <- data.frame(
  field = c(
    "study",
    "input_file",
    "input_participants",
    "included_participants",
    "excluded_participants",
    "primary_observations",
    "full_observations",
    "scenarios",
    "response_scale",
    "primary_model",
    "full_model",
    "random_effects",
    "contrast_coding",
    "pairwise_adjustment"
  ),
  value = c(
    "Experiment III",
    input_file,
    input_participants,
    included_participants,
    excluded_participants,
    nrow(exp3_primary),
    nrow(exp3_full),
    observed_scenarios,
    "Continuous ratings from 0 to 100",
    paste(
      "rating ~ adverb_type * polarity +",
      "(1 | participant_id) + (1 | scenario)"
    ),
    paste(
      "rating ~ adverb_type * iconicity_condition * polarity +",
      "(1 | participant_id) + (1 | scenario)"
    ),
    "Random intercepts for participant and scenario",
    paste(
      "Treatment coding with manner, affirmative,",
      "and ideophone as reference levels"
    ),
    "Tukey"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  model_metadata,
  file.path(
    table_directory,
    "exp3_model_metadata.csv"
  ),
  row.names = FALSE
)


# ---- Save models and session information ------------------------------------

saveRDS(
  primary_model,
  file.path(
    model_directory,
    "exp3_primary_lmer.rds"
  )
)

saveRDS(
  full_model,
  file.path(
    model_directory,
    "exp3_full_lmer.rds"
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

primary_convergence <-
  primary_diagnostics$value[
    primary_diagnostics$diagnostic ==
      "convergence_ok"
  ]

full_convergence <-
  full_diagnostics$value[
    full_diagnostics$diagnostic ==
      "convergence_ok"
  ]

primary_singular <-
  primary_diagnostics$value[
    primary_diagnostics$diagnostic ==
      "singular_fit"
  ]

full_singular <-
  full_diagnostics$value[
    full_diagnostics$diagnostic ==
      "singular_fit"
  ]

cat("\n")
cat("============================================\n")
cat("Experiment III analysis complete\n")
cat("============================================\n\n")

cat("Input participants: ", input_participants, "\n", sep = "")
cat("Included participants: ", included_participants, "\n", sep = "")
cat("Excluded participants: ", excluded_participants, "\n", sep = "")
cat("Primary ideophone observations: ", nrow(exp3_primary), "\n", sep = "")
cat("Full-model observations: ", nrow(exp3_full), "\n", sep = "")
cat("Scenarios: ", observed_scenarios, "\n", sep = "")
cat("Primary model converged: ", primary_convergence, "\n", sep = "")
cat("Full model converged: ", full_convergence, "\n", sep = "")
cat("Primary model singular: ", primary_singular, "\n", sep = "")
cat("Full model singular: ", full_singular, "\n", sep = "")

cat("\nFigures written to: ", figure_directory, "\n", sep = "")
cat("Tables written to: ", table_directory, "\n", sep = "")
cat("Models written to: ", model_directory, "\n", sep = "")
