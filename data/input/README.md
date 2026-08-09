# Input data

This directory contains the original source datasets used in the reproducible analysis workflow.

| File | Study component |
|---|---|
| `PreliminaryStudyContinuationTask.csv` | Preliminary continuation task |
| `ContinuationTask_Canonical.csv` | Experiment I |
| `ContinuationTask_Noncanonical.csv` | Experiment II |
| `Monoclause.csv` | Experiment III |

The files were copied from the project’s [OSF archive](https://osf.io/buftx/overview).

These datasets are treated as immutable inputs. They should not be edited manually. All exclusions, recoding, variable standardization, and data transformations must be performed by scripts in `code/scripts/`.
