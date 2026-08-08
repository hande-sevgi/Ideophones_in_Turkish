# Ideophones and Adverbial Interpretation in Turkish

This repository contains the data, documentation, and R analysis materials for three acceptability-judgment experiments investigating manner adverbials and ideophones in Turkish.

The studies form Chapter 4, “Where Are Your Manners?”, of my doctoral dissertation, *Manner Modification Across Modalities: Insights from Gesture, Sign, and Spoken Language* (Harvard University, 2026).

The corresponding research archive is available through the [Open Science Framework](https://osf.io/buftx/overview).

## Research overview

This project examines how negation, information structure, semantic class, and morphological integration affect the interpretation of Turkish event-modifying expressions.

The central research questions are:

* Do manner and temporal adverbials behave differently under negation?
* Does an adverbial’s position relative to the default focus position affect its interpretation?
* Do Turkish ideophones differ from ordinary lexical manner adverbs?
* Does the morphological integration of an ideophone affect its acceptability under negation?
* Is ideophone interpretation primarily determined by depictive form or by semantic class?

Across the three experiments, lexical and ideophonic manner expressions pattern together and differ from temporal expressions. The findings suggest that their interpretation is shaped primarily by semantic class rather than depictive form alone.

## Studies

| Study          | Research focus                                                    | Full design                                                                    | Analysis strategy                                                             |             Analysed sample |
| -------------- | ----------------------------------------------------------------- | ------------------------------------------------------------------------------ | ----------------------------------------------------------------------------- | --------------------------: |
| Experiment I   | Adverbial interpretation under negation in contrastive contexts   | 3 × 2 × 2 ordinal acceptability-judgment design                                | Primary manner–temporal analysis followed by an analysis including ideophones |  50 native Turkish speakers |
| Experiment II  | Information structure and adverbial position                      | Parallel 3 × 2 × 2 ordinal design with altered constituent order               | Primary manner–temporal analysis followed by an analysis including ideophones |  49 native Turkish speakers |
| Experiment III | Ideophones under negation without a contrastive discourse context | Ideophonic and non-ideophonic adverbials in affirmative and negative sentences | Comparisons of semantic class, iconicity, and morphological integration       | 114 native Turkish speakers |

## Experiment I: Adverbial interpretation under negation

Experiment I investigates manner, temporal, and ideophonic adverbials in two-clause contrastive constructions.

The complete experimental dataset follows a 3 × 2 × 2 structure:

* target adverbial type: manner, temporal, or ideophone;
* continuation type: matching or mismatching;
* position of negation: first or second clause.

Participants evaluated sentence naturalness using a five-point ordinal scale.

The statistical analysis was conducted in two stages. The primary analysis compared lexical manner and temporal adverbials using a 2 × 2 × 2 cumulative link mixed model. A subsequent extended analysis introduced ideophones as the third level of target adverbial type, producing a 3 × 2 × 2 analysis.

This staged approach reproduces the original comparison between manner and temporal adverbials before testing whether ideophones pattern with lexical manner adverbs or constitute a distinct interpretive class.

A total of 60 native Turkish speakers were recruited. Ten participants were excluded using the attention-check criteria described in the dissertation, resulting in an analysed sample of 50 participants.

## Experiment II: Adverbials and information structure

Experiment II uses the same three adverbial types and factorial structure as Experiment I but changes the relative position of the target and locative adverbials in the first clause.

The complete experimental dataset follows a parallel 3 × 2 × 2 structure:

* target adverbial type: manner, temporal, or ideophone;
* continuation type: matching or mismatching;
* position of negation: first or second clause.

The target adverbial is moved away from the immediately preverbal position, which is commonly associated with focus in Turkish. This manipulation tests whether information structure affects the interpretation and acceptability of different adverbial classes.

As in Experiment I, the analysis was conducted in two stages. The primary 2 × 2 × 2 analysis compared lexical manner and temporal adverbials. The extended 3 × 2 × 2 analysis then included ideophones as the third adverbial-type level.

This extended analysis tests whether ideophones pattern with lexical manner adverbs and whether that relationship changes when the target adverbial is displaced from the immediately preverbal position.

Participants evaluated sentence naturalness using a five-point ordinal scale.

A total of 60 native Turkish speakers were recruited. Eleven participants were excluded according to the attention-check criteria described in the dissertation, resulting in an analysed sample of 49 participants.

## Experiment III: Ideophones without a contrastive discourse context

Experiment III examines adverbials in isolated, single-clause sentences. Unlike Experiments I and II, it does not use contrastive continuations.

Participants evaluated sentence naturalness using a continuous scale from 0 to 100.

The experiment compares:

* reduplicated ideophones with relatively low morphological integration;
* ideophone-derived converbial forms with greater morphological integration;
* lexical manner adverbs;
* temporal adverbs;
* ideophonic and non-ideophonic stimulus sets;
* affirmative and negative sentences.

A total of 120 participants were recruited. Four participants were excluded because they reported that they were not native speakers of Turkish. Two additional participants were excluded using the attention-check criteria. The final analysed sample contained 114 participants.

## Analysis structure

For Experiments I and II, “primary” and “extended” refer to different analytical scopes within the same experiments—not to separate experiments or separately collected datasets.

| Analysis          | Adverbial-type levels           | Factorial structure |
| ----------------- | ------------------------------- | ------------------- |
| Primary analysis  | Manner and temporal             | 2 × 2 × 2           |
| Extended analysis | Manner, temporal, and ideophone | 3 × 2 × 2           |

The extended analyses supplement the primary comparisons by directly testing whether ideophones align with lexical manner adverbials or show a distinct response pattern.

## Repository structure

The repository is being organized using a reproducible computational social science workflow:

```text
ideophones_in_Turkish/
├── code/
│   └── legacy/
│   │   └── Sevgi_Chapter4.R
│   └── scripts/
│       ├── 01_validate_inputs.R
│       ├── 02_prepare_processed_data.R
│       ├── 03_analyze_experiment_1.R
│       ├── 04_exp1_extended_analysis.R
│       ├── 05_analyze_experiment_2.R
│       ├── 06_exp2_extended_analysis.R
│       └── 07_analyze_experiment_3.R
├── data/
│   ├── input/
│   │   ├── Experiment_I.xlsx
│   │   ├── Experiment_II.xlsx
│   │   └── Experiment_III.xlsx
│   ├── processed/
│   ├── codebook/
│   │   ├── README.md
│   │   ├── files.csv
│   │   └── variables.csv
│   └── README.md
├── materials/
│   └── README.md
├── output/
│   ├── figures/
│   ├── tables/
│   └── models/
├── .gitignore
├── README.md
└── renv.lock
```

Empty directories may not appear on GitHub until they contain a file. Directory-level `README.md` files may therefore be used to document their intended contents.

## Data provenance

The original research materials are archived in the project’s [OSF repository](https://osf.io/buftx/overview), titled “Encoding manner across modalities.”

The Turkish manner-adverbial component of the OSF archive contains:

* the preliminary continuation task;
* raw participant-response data for Experiment I;
* raw participant-response data for Experiment II;
* raw participant-response data for Experiment III;
* organized Excel workbooks used in the dissertation analyses;
* the original Chapter 4 R analysis script.

The principal source files include:

| Study component                   | OSF source file                        |
| --------------------------------- | -------------------------------------- |
| Preliminary continuation task     | `PreliminaryStudyContinuationTask.csv` |
| Experiment I raw responses        | `ContinuationTask_Canonical.csv`       |
| Experiment II raw responses       | `ContinuationTask_Noncanonical.csv`    |
| Experiment III raw responses      | `Monoclause.csv`                       |
| Experiment I organized workbook   | `Experiment_I.xlsx`                    |
| Experiment II organized workbook  | `Experiment_II.xlsx`                   |
| Experiment III organized workbook | `Experiment_III.xlsx`                  |
| Original dissertation analysis    | `Sevgi_Chapter4.R`                     |

The OSF archive serves as the long-term research deposit. This GitHub repository provides the documented and executable analysis workflow.

## Data directories

### `data/input/`

This directory contains immutable copies of the source data used by the reproducible workflow.

Files in this directory should not be manually edited. Any recoding, exclusion, reshaping, or variable construction should be implemented through an R script.

### `data/processed/`

This directory contains analysis-ready datasets created by `02_prepare_processed_data.R`.

Processed files should be reproducible from the files in `data/input/`. They should not contain changes that cannot be traced to the preparation script.

### `data/codebook/`

This directory documents the datasets and variables.

* `README.md` explains the experimental conditions, coding conventions, missing values, participant exclusions, and relationships among the datasets.
* `files.csv` describes the purpose, provenance, and processing status of every data file.
* `variables.csv` defines each variable, its data type, allowed values, units, and interpretation.

## Materials directory

The `materials/` directory documents the experimental materials associated with the three studies.

Where public release is appropriate, this directory may contain:

* experimental stimuli;
* participant instructions;
* catch and filler trials;
* condition lists;
* translated examples;
* information about counterbalancing and randomization.

A directory-level README should explain how each material relates to the experiments and to the corresponding dissertation appendix.

## Analysis workflow

The intended workflow is:

1. `01_validate_inputs.R` checks filenames, variables, participant counts, condition labels, missing values, rating ranges, and study-specific expectations.
2. `02_prepare_processed_data.R` imports the source files, standardizes variable names and factor levels, applies the documented exclusion criteria, and writes analysis-ready datasets.
3. `03_analyze_experiment_1.R` reproduces the primary 2 × 2 × 2 manner–temporal analysis for Experiment I.
4. `04_exp1_extended_analysis.R` extends Experiment I to the complete 3 × 2 × 2 design by including ideophones.
5. `05_analyze_experiment_2.R` reproduces the primary 2 × 2 × 2 manner–temporal analysis for Experiment II.
6. `06_exp2_extended_analysis.R` extends Experiment II to the complete 3 × 2 × 2 design by including ideophones.
7. `07_analyze_experiment_3.R` analyses ideophonic and non-ideophonic adverbials in Experiment III.

The source datasets remain unchanged throughout the workflow.

## Original dissertation analysis

The original dissertation analysis is preserved in `code/legacy/Sevgi_Chapter4.R`, together with the organized Excel workbooks originally used by the script.

The original analyses include:

* cumulative link mixed models for the five-point ordinal ratings in Experiments I and II;
* linear mixed-effects models for the continuous ratings in Experiment III;
* participant and scenario random intercepts;
* estimated marginal means;
* post-hoc pairwise comparisons;
* descriptive and model-based visualizations.

These legacy files document the original analytical process. The numbered scripts under `code/scripts/` provide the reorganized, auditable workflow and should become the primary entry point for reproduction.

## Running the reproducible workflow

Open the repository’s R project in RStudio and run the scripts from the repository root in numerical order:

```r
source("code/scripts/01_validate_inputs.R")
source("code/scripts/02_prepare_processed_data.R")
source("code/scripts/03_analyze_experiment_1.R")
source("code/scripts/04_exp1_extended_analysis.R")
source("code/scripts/05_analyze_experiment_2.R")
source("code/scripts/06_exp2_extended_analysis.R")
source("code/scripts/07_analyze_experiment_3.R")
```

Do not use `setwd()` inside the analysis scripts. All file paths should be defined relative to the repository root.

The exact package environment will be recorded in `renv.lock`. After cloning or downloading the repository, the required package versions can be restored with:

```r
install.packages("renv")
renv::restore()
```

## Outputs

Reproducible outputs are written to:

```text
output/
├── figures/
├── tables/
└── models/
```

Figures should be saved as publication-ready image files. Tables should be saved in open formats such as CSV. Fitted model objects may be saved as RDS files when they are useful for verification or reuse.

Generated outputs should not be manually edited.

## Principal findings

The experiments provide evidence that:

* matching continuations are generally preferred over mismatching continuations;
* manner and temporal adverbials exhibit different interpretive profiles under negation and contrastive focus;
* adverbial position relative to the immediately preverbal position affects acceptability;
* Turkish ideophones pattern closely with lexical manner adverbs rather than forming an entirely separate interpretive class;
* the observed effects cannot be attributed solely to the iconic or depictive form of ideophones;
* semantic class plays a central role in the interpretation of event-modifying expressions.

## Relationship to the dissertation

These materials support Chapter 4 of:

> Sevgi, Hande. 2026. *Manner Modification Across Modalities: Insights from Gesture, Sign, and Spoken Language*. Doctoral dissertation, Harvard University.

The dissertation provides the complete theoretical motivation, experimental materials, participant criteria, statistical results, and interpretation.

## Open research materials

The complete OSF project covers the dissertation’s three empirical domains:

* classifier constructions in Turkish Sign Language;
* path and manner in written English and co-speech gesture;
* manner adverbials and ideophones in Turkish.

Research archive: https://osf.io/buftx/overview

Code repository: https://github.com/hande-sevgi/ideophones_in_Turkish

## Ethics and funding

The studies were conducted as part of doctoral research in the Department of Linguistics at Harvard University.

Data collection was supported by a Harvard Mind, Brain, and Behavior graduate student grant.

Public files should be reviewed to ensure that they do not disclose direct identifiers or other information inappropriate for public research dissemination.

## Citation

If you use these materials, please cite the dissertation:

```bibtex
@phdthesis{sevgi2026manner,
  author = {Sevgi, Hande},
  title = {Manner Modification Across Modalities:
           Insights from Gesture, Sign, and Spoken Language},
  school = {Harvard University},
  year = {2026}
}
```

A repository-specific citation and versioned archive citation may be added when a permanent release of the computational workflow is deposited.

## License

No reuse license has yet been specified for this repository. Please contact the author before reproducing or redistributing the data, materials, or analysis code.

## Author

**Hande Sevgi**
PhD in Linguistics, Harvard University
[GitHub profile](https://github.com/hande-sevgi)
