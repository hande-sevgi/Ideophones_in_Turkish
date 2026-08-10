# Ideophones and Adverbial Interpretation in Turkish

Data, experimental materials, and analysis code for three acceptability-judgment experiments investigating the interpretation of manner adverbials and ideophones in Turkish.

This project accompanies Chapter 4, "Where Are Your Manners?", of:

> Sevgi, Hande. 2026. *Manner Modification Across Modalities: Insights from Gesture, Sign, and Spoken Language*. Doctoral dissertation, Harvard University.

## Project overview

Event-modifying expressions differ in semantic class, discourse function, depictive form, and degree of morphological integration. This project examines how those properties affect the interpretation of Turkish manner adverbials and ideophones, particularly in affirmative and negative environments.

The studies compare:

* lexical manner adverbials;
* temporal adverbials;
* reduplicated ideophones;
* ideophone-derived converbial forms;
* different constituent orders and information-structural configurations.

Together, the experiments investigate how semantic class, negation, information structure, depictive form, and morphological integration interact during sentence interpretation.

## Research questions

The project addresses the following questions:

1. Do manner and temporal adverbials behave differently under negation?
2. Does an adverbial's position relative to the default focus position affect its interpretation?
3. Do Turkish ideophones differ from ordinary lexical manner adverbs?
4. Does the morphological integration of an ideophone affect its acceptability under negation?
5. Is ideophone interpretation primarily determined by depictive form or by semantic class?

## Studies

### Experiment I: Adverbial interpretation under negation

Experiment I investigates manner, temporal, and ideophonic adverbials in two-clause contrastive constructions with canonical constituent order. The complete experimental design is 3 x 2 x 2:

* Target adverbial type: manner, temporal, or ideophone
* Continuation type: match or mismatch
* Negation position: first or second clause

Participants evaluated sentence naturalness using a five-point ordinal scale. Sixty native Turkish speakers were recruited. Ten were excluded using the attention-check criteria reported in the dissertation, resulting in an analysed sample of 50 participants.

The analysis is implemented in two stages. The primary 2 x 2 x 2 analysis compares lexical manner and temporal adverbials. The extended 3 x 2 x 2 analysis adds ideophones as the third adverbial-type level.

### Experiment II: Adverbials and information structure

Experiment II uses the same three adverbial types and factorial structure as Experiment I but changes the relative position of the target and locative adverbials. The target adverbial is moved away from the immediately preverbal position, which is commonly associated with focus in Turkish.

Participants evaluated sentence naturalness using a five-point ordinal scale. Sixty native Turkish speakers were recruited. Eleven were excluded using the criteria reported in the dissertation, resulting in an analysed sample of 49 participants.

As in Experiment I, the primary 2 x 2 x 2 analysis compares manner and temporal adverbials, and the extended 3 x 2 x 2 analysis includes ideophones.

### Experiment III: Ideophones without a contrastive discourse context

Experiment III examines adverbials in isolated, single-clause sentences. Participants evaluated sentence naturalness using a continuous scale from 0 to 100.

The experiment compares:

* reduplicated ideophones with relatively low morphological integration;
* ideophone-derived converbial forms with relatively high morphological integration;
* lexical manner adverbs;
* temporal adverbs;
* ideophonic and non-ideophonic stimulus sets;
* affirmative and negative sentences.

One hundred twenty participants were recruited. Four were excluded because they reported that they were not native speakers of Turkish. Two additional participants were excluded using the documented attention-check criterion. The final analysed sample contained 114 participants.

## Methods

Experiments I and II use cumulative link mixed models for five-point ordinal ratings. The models use sum-coded predictors and random intercepts for participant and scenario. Experiment III uses linear mixed-effects models for continuous ratings, with participant and scenario random intercepts.

The principal analyses are conducted in R using packages including:

* `readxl`
* `ordinal`
* `emmeans`
* `ggplot2`
* `scales`
* `lme4`
* `lmerTest`

### Model evaluation

The ordinal-analysis scripts record the model convergence code, maximum absolute gradient, Hessian status, Hessian condition number, log likelihood, AIC, and BIC. Experiment III records convergence messages and singularity checks and generates residual-versus-fitted and normal Q-Q plots.

Model summaries, coefficient tables, estimated marginal means, conditional contrasts, prediction tables, diagnostic information, figures, and fitted model objects are saved under `output/`. Diagnostic limitations should be considered when interpreting individual coefficients or contrasts.

## Repository contents

```text
Ideophones_in_Turkish/
|-- README.md
|-- code/
|   |-- legacy/
|   |   `-- Sevgi_Chapter4.R
|   `-- scripts/
|       |-- 01_validate_inputs.R
|       |-- 02_prepare_processed_data.R
|       |-- 03_analyze_experiment_1.R
|       |-- 04_exp1_extended_analysis.R
|       |-- 05_analyze_experiment_2.R
|       |-- 06_exp2_extended_analysis.R
|       `-- 07_analyze_experiment_3.R
|-- data/
|   |-- README.md
|   |-- input/
|   |   |-- Experiment_I.xlsx
|   |   |-- Experiment_II.xlsx
|   |   `-- Experiment_III.xlsx
|   |-- processed/
|   |   |-- exp1_contrastive_canonical.csv
|   |   |-- exp2_contrastive_noncanonical.csv
|   |   `-- exp3_monoclause.csv
|   `-- codebook/
|       |-- README.md
|       |-- files.csv
|       `-- variables.csv
|-- materials/
|   `-- README.md
|-- output/
|   |-- figures/
|   |-- tables/
|   `-- models/
`-- .gitignore
```

### `data/`

Contains the immutable organized workbooks, generated analysis-ready datasets, and codebook documentation. Source files under `data/input/` are read but not modified.

### `materials/`

Documents the experimental stimuli, instructions, condition lists, catch and filler trials, and related archival materials. Large materials may remain on OSF rather than being duplicated on GitHub.

### `code/legacy/`

Preserves the original Chapter 4 R analysis for provenance.

### `code/scripts/`

Contains the ordered validation, data-preparation, and dissertation-analysis scripts.

| Script | Purpose |
| --- | --- |
| `01_validate_inputs.R` | Validates the three organized workbooks without modifying them. |
| `02_prepare_processed_data.R` | Standardizes variables, constructs condition labels, applies the documented Experiment III attention-check exclusion, and creates three analysis-ready datasets. |
| `03_analyze_experiment_1.R` | Reproduces the primary Experiment I manner-temporal cumulative link mixed model. |
| `04_exp1_extended_analysis.R` | Reproduces the extended Experiment I model including ideophones. |
| `05_analyze_experiment_2.R` | Reproduces the primary Experiment II manner-temporal cumulative link mixed model. |
| `06_exp2_extended_analysis.R` | Reproduces the extended Experiment II model including ideophones. |
| `07_analyze_experiment_3.R` | Reproduces the primary ideophone-group and supplementary full Experiment III linear mixed models. |

#### Primary and extended analyses

For Experiments I and II, `primary` and `extended` refer to two analytical scopes within the same experiment, not to separate experiments or separately collected datasets.

| Analysis | Adverbial-type levels | Factorial structure |
| --- | --- | --- |
| Primary | Manner and temporal | 2 x 2 x 2 |
| Extended | Manner, temporal, and ideophone | 3 x 2 x 2 |

Scripts 03 and 05 reproduce the original manner-temporal comparisons. Scripts 04 and 06 reproduce the dissertation analyses that extend those comparisons to ideophones. The extended scripts are not post-dissertation sensitivity analyses.

### Implemented results

The Experiment I primary model uses 384 observations from 50 participants and converges with a positive-definite Hessian. It identifies interactions of continuation type with adverb type and with negation position. The extended model uses all 600 experimental observations. Holm-adjusted comparisons show that, in mismatch continuations, temporal targets receive lower ratings than manner and ideophonic targets, while manner and ideophonic targets do not differ reliably. No adverbial-type comparison in match continuations is significant after Holm adjustment.

The Experiment II primary model uses 392 observations from 49 participants and converges with a positive-definite Hessian. Continuation type, adverb type, and negation position contribute to the model, while the tested interaction terms are not statistically significant. The extended Experiment II script adds ideophones to the same factorial analysis and generates the corresponding conditional contrasts and ordinal category-probability visualizations.

The Experiment III workflow analyses 114 included participants. The primary model contains 912 observations and the supplementary full model contains 1,824 observations across eight scenarios. Both models converge and are non-singular. In the primary model, negative sentences receive substantially lower ratings. The temporal-by-negative interaction is positive, while the interactions involving the two ideophonic forms are not statistically significant. These results support the dissertation's broader conclusion that semantic class is central to the interpretation of these expressions.

### `output/`

Contains generated figures, tables, model summaries, diagnostic information, fitted model objects, and R session information. Generated outputs should not be edited manually.

## Data files

| Study | Immutable input | Analysis-ready dataset |
| --- | --- | --- |
| Experiment I | `Experiment_I.xlsx` | `exp1_contrastive_canonical.csv` |
| Experiment II | `Experiment_II.xlsx` | `exp2_contrastive_noncanonical.csv` |
| Experiment III | `Experiment_III.xlsx` | `exp3_monoclause.csv` |

Variable definitions, coded values, missing-value conventions, and exclusion rules are documented in `data/codebook/`.

## Open Science Framework archive

The companion OSF project contains the archived research materials, including raw participant responses, organized workbooks, experimental materials, and the original Chapter 4 analysis:

**[View the OSF project](https://osf.io/buftx/)**

GitHub is used for the documented computational workflow. OSF serves as the long-term archival location for research materials and large files.

## Reproducibility

This repository provides an implemented, end-to-end workflow for validating the three source workbooks, preparing analysis-ready data, reproducing the Chapter 4 analyses, evaluating the fitted models, and generating documented outputs.

The workflow:

1. validates workbook names, worksheets, variables, participant counts, observation counts, and rating ranges;
2. preserves the source workbooks without modification;
3. creates study-specific anonymous participant identifiers;
4. standardizes variable names and experimental condition labels;
5. applies and records the documented participant-exclusion criteria;
6. creates three analysis-ready CSV datasets;
7. fits the primary, extended, and supplementary dissertation models;
8. evaluates convergence, Hessian status, singularity, and residual patterns as appropriate;
9. generates statistical tables and publication-ready figures;
10. saves fitted model objects and session information.

### Running the workflow

Open the repository in RStudio and ensure that the repository root is the active project directory.

Install the required packages if they are not already available:

```r
install.packages(c(
  "readxl",
  "ordinal",
  "emmeans",
  "ggplot2",
  "scales",
  "lme4",
  "lmerTest"
))
```

Run the scripts from the repository root in numerical order:

```r
source("code/scripts/01_validate_inputs.R")
source("code/scripts/02_prepare_processed_data.R")
source("code/scripts/03_analyze_experiment_1.R")
source("code/scripts/04_exp1_extended_analysis.R")
source("code/scripts/05_analyze_experiment_2.R")
source("code/scripts/06_exp2_extended_analysis.R")
source("code/scripts/07_analyze_experiment_3.R")
```

Scripts 03 through 07 reproduce analyses reported in Chapter 4. Scripts 04 and 06 extend the primary manner-temporal models to the complete three-level adverbial-type factor; they supplement the primary comparisons but are not sensitivity analyses.

The source files under `data/input/` remain unchanged. Analysis-ready datasets are written to `data/processed/`, and generated results are written to `output/`.

Do not use `setwd()` inside the scripts. All file paths are defined relative to the repository root.

## AI-assisted workflow disclosure

The development of this reproducible computational workflow was assisted by OpenAI's ChatGPT and Codex. AI assistance was used to help reorganize the original dissertation materials, refactor the legacy R analysis into numbered scripts, troubleshoot code, design validation checks, structure generated outputs, and improve repository documentation.

The research questions, experimental designs, data collection, original dissertation analyses, and substantive scholarly conclusions are the work of the author. The author reviewed the AI-assisted code, ran the scripts locally, checked participant and observation counts, evaluated model convergence and diagnostics, and compared the reproduced results with the dissertation. AI tools did not independently collect data or make final decisions about data exclusion, statistical interpretation, or reporting.

This disclosure is provided for transparency. Responsibility for the accuracy of the repository and its scholarly interpretation remains with the author.

## Participant data and responsible use

The datasets contain behavioral judgments collected for academic research. Users should:

* Treat participant identifiers as pseudonymous research identifiers.
* Avoid attempting to identify participants.
* Avoid combining the data with external information for re-identification.
* Follow applicable ethical, institutional, and data-use requirements.
* Cite the project when using its data, materials, or code.

## Ethics and funding

The studies were conducted as part of doctoral research in the Department of Linguistics at Harvard University.

Data collection was supported by a Harvard Mind, Brain, and Behavior graduate student grant.

Public files should be reviewed to ensure that they do not disclose direct identifiers or other information inappropriate for public research dissemination.

## Citation

If you use this project, please cite:

```text
Sevgi, Hande. 2026.
Manner Modification Across Modalities:
Insights from Gesture, Sign, and Spoken Language.
Doctoral dissertation, Harvard University.
```

## Author

**Hande Sevgi**  
Linguist working on semantics, event structure, ideophones, gesture, sign language, and multimodal communication.

* [Academic website](https://hande-sevgi.github.io/)
* [OSF project](https://osf.io/buftx/)
