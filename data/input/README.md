# Input data

This directory contains the organized research datasets used by the reproducible analysis workflow.

The input files are organized Excel workbooks rather than untouched Qualtrics exports. They contain participant response identifiers, trial codes, and ratings, but exclude administrative fields such as IP addresses, geographic coordinates, assignment identifiers, and completion codes.

| Study | Workbook | Input sheet | Recruited | Present in input sheet | Final analysed |
|---|---|---|---:|---:|---:|
| Preliminary study | PreliminaryStudyContinuationTask_long_utf8.csv | 304 | 38 | 38 |
| Experiment I | `Experiment_I.xlsx` | `List_Organized` | 60 | 50 | 50 |
| Experiment II | `Experiment_II.xlsx` | `List_Organized` | 60 | 49 | 49 |
| Experiment III | `Experiment_III.xlsx` | `List_Base_MainDocument` | 120 | 116 | 114 |

## Processing status

### Experiment I

Ten participants were excluded using the attention-check criteria described in the dissertation before the organized input sheet was created.

The public input sheet contains the final 50 participants. Because the excluded responses are not present, these exclusions cannot be independently recomputed from this repository.

### Experiment II

Eleven participants were excluded using the attention-check criteria described in the dissertation before the organized input sheet was created.

The public input sheet contains the final 49 participants. Because the excluded responses are not present, these exclusions cannot be independently recomputed from this repository.

### Experiment III

Four participants who reported that they were not native speakers of Turkish were removed before `List_Base_MainDocument` was created.

The input sheet contains the remaining 116 participants. The two additional attention-check exclusions are reproduced by the data-preparation script, resulting in the final analysed sample of 114 participants.

## Data-handling principles

These workbooks are treated as immutable analysis inputs and should not be edited manually.

All trial decoding, variable construction, factor-level standardization, participant-ID replacement, and reproducible exclusions are implemented through scripts in `code/scripts/`.

The original data-collection exports are preserved separately in the project’s OSF archive and are not used directly by the public GitHub workflow.
