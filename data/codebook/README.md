# Data codebook

This directory documents the source and processed datasets used in the Chapter 4 analysis workflow for *Ideophones and Adverbial Interpretation in Turkish*.

## Contents

- `files.csv` inventories the three immutable input workbooks and the three processed CSV datasets.
- `variables.csv` defines every column in the processed datasets, including its scope, data type, allowed values, units, missing-value convention, interpretation, and derivation.

## Dataset overview

| Processed dataset | Study | Rows | Participants represented | Rating scale |
| --- | --- | ---: | ---: | --- |
| `exp1_contrastive_canonical.csv` | Experiment I | 1,400 | 50 | 1â€“5 ordinal |
| `exp2_contrastive_noncanonical.csv` | Experiment II | 1,372 | 49 | 1â€“5 ordinal |
| `exp3_monoclause.csv` | Experiment III | 2,784 | 116 total; 114 included | 0â€“100 continuous |

Each row represents one participantâ€™s rating of one trial. Participant identifiers in the processed datasets are study-specific anonymous identifiers such as `exp1_001`.

## Experimental structure

### Experiments I and II

The complete experimental design is 3 X 2 X 2:

- target adverbial type: `manner`, `temporal`, or `ideophone`;
- continuation type: `match` or `mismatch`;
- negation position: `first_clause` or `second_clause`.

The primary analyses compare manner and temporal adverbials using a 2 X 2 X 2 design. The extended analyses include ideophones and use the complete 3 X 2 X 2 design. These are two analytical scopes within each experiment, not separate datasets or separately collected studies.

Experiment I uses canonical constituent order. Experiment II uses noncanonical constituent order in which the target adverbial is displaced from the immediately preverbal position.

### Experiment III

Experiment III examines adverbials in affirmative and negative single-clause sentences. The experimental trials cross polarity with ideophonic and non-ideophonic stimulus sets and compare:

- lexical manner adverbs;
- temporal adverbs;
- reduplicated ideophones with relatively low morphological integration;
- ideophone-derived converbial forms with relatively high morphological integration.

## Coding conventions

Condition labels use lowercase `snake_case`. Blank CSV fields mean that a variable is structurally not applicable to that observation; they do not indicate an undocumented data loss.

Examples include:

- `adverb_type`, `continuation_type`, and `ideophone_form` are blank for catch and filler trials in Experiments I and II;
- `scenario` is blank for catch trials in Experiment III;
- `iconicity_condition`, `adverb_type`, `semantic_class`, and `morphological_integration` are blank for Experiment III catch trials;
- `catch_grammaticality` is blank for Experiment III experimental trials;
- `exclusion_reason` is blank for included participants.

The complete field-level definitions are provided in `variables.csv`.

## Participant inclusion and exclusions

The organized Experiment I and II workbooks already contain the final analysed samples. The preparation script therefore marks all rows in these two processed datasets as `included = TRUE` and does not apply additional participant exclusions.

Experiment III retains both included and excluded participants in the processed CSV for auditability. An affirmative catch trial is coded as a failure when:

- an ungrammatical catch trial receives a rating above 50; or
- a grammatical catch trial receives a rating below 50.

Participants with two or more such failures are excluded. The processed dataset contains 116 participants: 114 included and 2 excluded. Excluded rows remain in the dataset with `included = FALSE` and `exclusion_reason = failed_two_or_more_affirmative_catch_trials`.

## Experiment I allocation note

The organized Experiment I input is not perfectly balanced across target adverbial types at the participant level.

Forty-two participants contribute four observations for each of the three adverbial types. Eight participants contribute six ideophone observations, four temporal observations, and two manner observations.

This allocation is present in the archived input workbook and was not introduced during data processing. The observations are preserved without alteration, and the mixed-effects models account for the unequal cell counts.

## Data-processing principles

Files in `data/input/` are immutable source data and should not be manually edited.

`code/scripts/02_prepare_processed_data.R` performs all identifier anonymization, trial-code normalization, variable construction, factor-level standardization, missing-value handling, and Experiment III attention-check exclusions. It writes the analysis-ready files in `data/processed/` without modifying the input workbooks.

Processed datasets are generated files and should not be edited manually. Any correction should be made in the preparation script and reproduced by rerunning the workflow from the repository root:

```r
source("code/scripts/01_validate_inputs.R")
source("code/scripts/02_prepare_processed_data.R")
```

## Provenance

The input workbooks are organized archival datasets associated with Chapter 4 of Hande Sevgiâ€™s doctoral dissertation. The corresponding long-term research archive is available through the [Open Science Framework](https://osf.io/buftx/overview). `files.csv` records the relationship between each input workbook and its generated processed dataset.
