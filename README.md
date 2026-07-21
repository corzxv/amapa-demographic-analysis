# Demographic Analysis of Amapá (2000–2024)

![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white)
![LaTeX](https://img.shields.io/badge/LaTeX-008080?style=flat-square&logo=latex&logoColor=white)
![Data](https://img.shields.io/badge/Data-IBGE%20%26%20DATASUS-4E8C3A?style=flat-square)

A demographic analysis of the Brazilian state of Amapá between 2000 and 2024, developed for the **Demography course** in the Bachelor's Degree in Statistics at the University of Brasília.

## About the project

This repository contains an academic project focused on the demographic dynamics of Amapá, Brazil — state code 16 according to the Brazilian Institute of Geography and Statistics (IBGE).

The project combines:

- data collection and preparation in R;
- demographic indicator calculation;
- data quality checks;
- statistical tables and visualizations;
- a final academic report written in LaTeX.

The analyses were based on data from the Brazilian Live Birth Information System (SINASC), Mortality Information System (SIM), Fetal Death Information System (SIM-DOFET), IBGE population projections, and the 2022 Brazilian Census.

## Topics covered

The project includes the following analyses:

- Lexis diagram for deaths among children under five;
- probability of survival to the first birthday and to exact age five;
- infant mortality by race or skin color in 2022 and 2023;
- birth, fertility, and reproduction indicators;
- comparison of 2022 indicators using population projections and Census data;
- contingency tables for maternal age, education level, and type of delivery;
- crude death rate and age- and sex-specific mortality rates;
- infant, neonatal, post-neonatal, and perinatal mortality;
- distribution of causes of death, with emphasis on COVID-19;
- abridged life tables by sex for 2010 and 2024.

## Project structure

```text
demografia_ap/
├── dados/
│   ├── brutos/             # downloaded or imported raw datasets
│   └── processados/        # processed datasets and data quality logs
├── scripts/                # R scripts organized by analysis stage
├── outputs/
│   ├── tabelas/            # exported tables
│   └── graficos/           # generated visualizations
└── relatorio/              # supporting material for the report

relatorio_overleaf/         # final LaTeX project for Overleaf
```

The original directory and file names were kept in Portuguese because the project was developed as an academic assignment in Brazil.

## Main scripts

| Script | Description |
|---|---|
| `00_setup_e_dados.R` | Configures the project, downloads or imports the datasets, and performs initial data quality checks. |
| `01_lexis_sobrevivencia.R` | Creates the Lexis diagram and estimates childhood survival probabilities. |
| `02_fecundidade.R` | Calculates birth, fertility, and reproduction indicators and produces contingency tables using SINASC 2024 data. |
| `03_mortalidade_geral_infantil.R` | Calculates general mortality rates, specific mortality rates, and infant and perinatal mortality indicators. |
| `ofc_q3_c_d.R` | Analyzes the distribution of causes of death and constructs abridged life tables. |

## Data sources

### DATASUS — Brazilian Ministry of Health

- **SINASC** — Live Birth Information System;
- **SIM** — Mortality Information System;
- **SIM-DOFET** — Fetal Death Information System.

### IBGE

- Population Projections — 2024 Revision;
- 2022 Brazilian Census.

## Technologies

- **R**
- **LaTeX**
- **Overleaf**
- **DATASUS public health data**
- **IBGE demographic data**

## Reproducibility

The scripts are organized according to the main stages of the analysis.

Start by running the project setup and data preparation script from the project root:

```r
source("demografia_ap/scripts/00_setup_e_dados.R")
```

The remaining scripts can then be executed according to the demographic topic being analyzed.

Some source datasets may need to be downloaded again from DATASUS or IBGE, depending on their availability and access method.

## Final report

The final academic report is available in:

```text
relatorio_overleaf/
```

The report was written in Portuguese and formatted in LaTeX for compilation in Overleaf.

## Course context

This project was developed for the **Demography course** in the Bachelor's Degree in Statistics at the University of Brasília.

The repository documents my practical work with demographic indicators, Brazilian public datasets, reproducible data analysis in R, and academic reporting.
