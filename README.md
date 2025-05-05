# BiostatsMaterials

> Tools, templates, and utilities for biostatistics workflows, data summaries, and educational materials.

## 📖 Overview

`BiostatsMaterials` is an R package designed to streamline biostatistics analysis workflows, support reproducible reporting, and facilitate teaching through well-documented functions and templates. It includes:

- Preprocessing and merging utilities  
- Summary statistics and table generation tools  
- Custom plotting functions for longitudinal data  
- Examples of repeated-measures analyses  
- Educational examples for teaching biostatistics concepts  

## 🛠 Installation

To install the development version of the package from GitHub:

```r
# install.packages("devtools")
devtools::install_github("annieaitken123/BiostatsMaterials")
```

## 📚 Example Usage

```r
library(BiostatsMaterials)

# Merge example metadata
merged_df <- merge_data_files("data/")

# Generate a summary table by group
summary_table <- create_summary_table(merged_df, group_var = "Arm", vars = c("age", "sex"))

# Plot trajectories of a variable by arm
plot_trajectory_variable(merged_df, variable_name = "outcome_score")
```

## 📁 Package Structure

- `R/` — Core functions
- `data-raw/` — Scripts to prepare example datasets
- `vignettes/` — Tutorials and example workflows
- `man/` — Function documentation
- `tests/` — Unit tests (optional)

## ✅ Features

- 🔄 Data cleaning and merging helpers  
- 📊 GTSummary and `rstatix`-based reporting  
- 📈 Group-specific trajectory plotting  
- 📦 Designed for easy teaching and reproducibility  
- 🧪 Compatible with tidyverse workflows  

## 🔬 Example Functions

| Function                 | Description                                      |
|--------------------------|--------------------------------------------------|
| `merge_data_files()`     | Merges raw data files into a clean dataframe     |
| `create_summary_table()` | Generates gtsummary tables by group              |
| `plot_trajectory_variable()` | Plots baseline and end-of-study trends        |
| `run_rm_anova()`         | Runs repeated-measures ANOVA                     |

## 👩‍🔬 Author

**Annie Aitken**  
[GitHub @annieaitken123](https://github.com/annieaitken123)  
Research Scientist & Biostatistics Consultant
