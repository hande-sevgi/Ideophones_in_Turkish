# Materials used in the experiments

All CSV files in this package use **UTF-8 with BOM (UTF-8-SIG)** and Unicode **NFC normalization**. This preserves Turkish characters such as **ç, ğ, ı, İ, ö, ş, ü** and is compatible with current versions of Excel, R, Python, and text editors.

## Files

- `preliminary_completion_trials.csv` — preliminary completion-task experimental stimuli.
- `experiment_1_trials.csv` — Experiment I experimental trials.
- `experiment_1_fillers.csv` — Experiment I filler trials.
- `experiment_1_catch.csv` — Experiment I catch trials.
- `experiment_3_trials.csv` — Experiment III experimental trials.
- `experiment_3_catch_trials.csv` — Experiment III catch trials.

The Experiment I and II differ only with respect to the word order.

## Reading the files

In R:

```r
data <- read.csv("preliminary_completion_trials.csv", fileEncoding = "UTF-8-BOM")
```

In Python:

```python
import pandas as pd
data = pd.read_csv("preliminary_completion_trials.csv", encoding="utf-8-sig")
```
