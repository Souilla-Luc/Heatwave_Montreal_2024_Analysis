# Heatwave Montreal 2024 — Montreal Summer Heatwave Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R version](https://img.shields.io/badge/R-%3E%3D4.3.0-blue)](https://www.r-project.org/)

This repository contains the complete analysis code for the **Heatwave Montreal 2024** study, a longitudinal field study examining the environmental, physiological, perceptual and behavioral effects of an heatwave on community-dwelling older adults residing in social housing in Montreal, Quebec, Canada (June 2024).

> **Data availability:** Raw and processed datasets are not included in this repository. Processed data are publicly available at [INSERT DATA REPOSITORY LINK — ].  
> To reproduce the analyses, download the data, place the files in the `DAT/Processed` folder as described below, and run the scripts in the order listed in the [Execution Order](#execution-order) section.

---

## Study Overview

| Item | Details |
|---|---|
| Study period | June 11–27, 2024 |
| Location | Montreal, Quebec, Canada |
| Heatwave period | June 18–20, 2024 (Montreal Public Health definition) |
| Participants | 26 community-dwelling older adults |


---

## Repository Structure

```
HeatSuite2024/
├── PRG/                          # All analysis scripts
│   ├── Data_preparation.Rmd      # Baseline data and participant lookup tables
│   ├── Physiological/            # Data preparation and analysis per outcome
│   │   ├── blood_pressure.Rmd
│   │   ├── body_mass.Rmd
│   │   ├── oral_temperature.Rmd
│   │   ├── urine_frequency.Rmd
│   │   ├── physical_activity.Rmd
│   │   └── main_analysis_physio.Rmd
│   ├── Environmental/
│   │   ├── indoor_environment_daily.Rmd
│   │   ├── indoor_environment_hourly.Rmd
│   │   ├── outdoor_environment.Rmd
│   │   └── main_analysis_environment.Rmd
│   ├── Perceptuals/
│   │   ├── surveys_responses.Rmd
│   │   └── main_analysis_perceptuals.Rmd
│   └── Functions/                # Custom R functions sourced by all scripts
│       ├── functions_participant_period.R   # ID assignment, period labelling, heatwave status
│       ├── functions_cleaning_data.R        # Deduplication and outlier removal
│       ├── functions_adjusted_ref_time.R    # Night-period reference time correction
│       ├── functions_descriptive_stats.R    # Descriptive statistics tables
│       ├── functions_descriptive_plot.R     # Time series, boxplots, heatmaps
│       ├── functions_linear_models.R        # MMRM, CLMM, glmmTMB models
│       ├── functions_check_clmm.R           # Proportional odds assumption tests
│       └── residual_check_mmrm.R            # MMRM residual diagnostic plots
│
├── DAT/                          # Data folder (NOT included — see Data Availability)
│   ├── Raw/                      # Raw exports from smartwatch platform
│   └── Processed/                # Cleaned datasets output by PRG/ scripts
│
├── RES/                          # Results folder (NOT included)
│   ├── Physiological/
│   │   ├── Graph/
│   │   └── Table/
│   ├── Environmental/
│   │   ├── Graph/
│   │   └── Table/
│   └── Perceptuals/
│       ├── Graph/
│       └── Table/
│
├── README.md
├── .gitignore
└── HeatSuite2024.Rproj           # R project file — defines the here() root
```

---

## Execution Order

Scripts must be run in the following order. Each script depends on outputs from the previous steps.

### Step 1 — Baseline preparation
```
PRG/Data_preparation.Rmd
```
Produces `DAT/Processed/baseline_long.csv` and `DAT/Processed/characteristics_participants.csv`, which are required by all subsequent scripts for participant ID matching.

### Step 2 — Physiological data preparation (run in any order)
```
PRG/Physiological/blood_pressure.Rmd
PRG/Physiological/body_mass.Rmd
PRG/Physiological/oral_temperature.Rmd
PRG/Physiological/urine_frequency.Rmd
PRG/Physiological/physical_activity.Rmd
```
Each script reads raw data from `DAT/Raw/` and writes a cleaned processed file to `DAT/Processed/`.

### Step 3 — Environmental data preparation (run in any order)
```
PRG/Environmental/indoor_environment_daily.Rmd
PRG/Environmental/indoor_environment_hourly.Rmd
PRG/Environmental/outdoor_environment.Rmd
```

### Step 4 — Perceptual survey preparation
```
PRG/Perceptuals/surveys_responses.Rmd
```

### Step 5 — Main analyses (require all Step 2–4 outputs)
```
PRG/Physiological/main_analysis_physio.Rmd
PRG/Environmental/main_analysis_environment.Rmd
PRG/Perceptuals/main_analysis_perceptuals.Rmd
```

---

## Setup and Dependencies

### R version
R ≥ 4.3.0 is required.

### Required packages
Install all required packages by running:

```r
required_packages <- c(
  # Data wrangling
  "here", "dplyr", "tidyr", "lubridate", "stringr", "purrr",
  # Reporting
  "knitr", "rmarkdown", "kableExtra", "openxlsx",
  # Visualization
  "ggplot2", "patchwork", "viridis", "ggeffects",
  # Statistical modelling
  "rstatix", "carData", "car", "emmeans", "mmrm",
  "mlogit", "ordinal", "glmmTMB",
  # glmmTMB diagnostics
  "marginaleffects", "DHARMa", "performance"
)

install.packages(required_packages)
```

### Setting up the `here()` root

This project uses the `here` package for all file paths. The root is anchored by the `.Rproj` file at the repository root. To ensure correct path resolution:

1. Open `Heatwave_Montreal_2024_Analysis.Rproj` in RStudio before running any script.
2. Each script calls `here::i_am("PRG/.../script_name.Rmd")` at the top to verify the root.
3. Do **not** use `setwd()` — all paths are relative to the project root.

### Setting up the data folders

After downloading the data from the public repository, place the files in DAT/Processed


## Codebooks

Variable-level documentation for all datasets is available in the repository [INSERT LINK here] 

---

## Citation

If you use this code in your research, please cite:

> Souilla, L. et al. (2025). *[Article title]*. *[Journal]*. DOI: [INSERT DOI]

```bibtex
@article{souilla2025heatsuite,
  author  = {Souilla, Luc and others},
  title   = {[Article title]},
  journal = {[Journal]},
  year    = {2025},
  doi     = {[INSERT DOI]}
}
```

---

## License

This code is released under the [MIT License](LICENSE).  
Data are released under [INSERT DATA LICENSE — e.g. CC BY 4.0] at [INSERT DATA REPOSITORY LINK].