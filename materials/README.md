# Materials used in the experiments

## Files

- `preliminary_completion_trials.csv` — preliminary completion-task experimental stimuli.
- `experiment_1_trials.csv` — Experiment I experimental trials.
- `experiment_1_fillers.csv` — Experiment I filler trials.
- `experiment_1_catch_trials.csv` — Experiment I catch trials.
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
