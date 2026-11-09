# Willingness, counterpart search and by-product exchange among businesses in Arab towns in Israel

R code for the article of the same title by Rotem Zelingher, Liron Amdur, Omar Asi, Alaa Haj-Yahya and Ibrahim Yehia.

## Data availability

The survey data underlying the article cannot be shared publicly because they were collected from named small businesses under an assurance of confidentiality, and the combination of town, sector and business characteristics would permit individual businesses to be identified. This repository therefore contains no business-level records. It contains the code, the codebook, and the tables and figure that the code writes. The three study towns are labelled A, B and C, from the most to the least surveyed.

## Contents

| File | Content |
| --- | --- |
| `01_make_waste_tidy.R` | Reads the survey workbook and builds three tidy tables: businesses, storage containers and waste streams. Codes the free-text answers on how each arrangement began (item 36) and on why a business would not take in another business's by-products. |
| `02_build_analysis_frame.R` | Builds the business-level analysis table, recodes the variables and labels the towns A, B and C. Prints the number of businesses by survey category and sector. |
| `03_waste_analysis.R` | Tests the bivariate association of each variable with transfer, compares transfer rates by material group, estimates the logistic models of transfer and of willingness to supply with their diagnostics and robustness checks, the bivariate probit and multinomial models, and the penalised-likelihood model of willingness to take in by-products. Writes the regression tables and the figure. |
| `04_descriptive_stats.R` | Writes the descriptive tables of Appendix B. |
| `05_discovery_tables.R` | Writes the table on how the existing arrangements began and whether the taker is in the same town. |
| `inputs/codebook.xlsx` | The recoding rules, one sheet per survey item: sectors, material groups, container volumes, weekly volume bands, business size, business age, education and owner roles. Each sheet opens with a note explaining it. |
| `outputs/` | The tables and the figure written by the scripts. |

## Tables and figure in the article

| Article | Script | Output |
| --- | --- | --- |
| Table 1 | `05_discovery_tables.R` | `outputs/tab_discovery.tex` |
| Figure 1 | `03_waste_analysis.R`, section 12 | `outputs/fig_coefficients.pdf` |
| Table A.2 | `02_build_analysis_frame.R`, section 4 | printed to the console |
| Table A.3 | `03_waste_analysis.R`, section 3 | `outputs/tab_screen_raw.tex`, formatted for the article |
| Table A.5 | `03_waste_analysis.R`, section 4 | printed to the console |
| Tables A.6 and A.7 | `03_waste_analysis.R`, section 11 | `outputs/tab_model1.tex` and `outputs/tab_model2.tex` |
| Tables B.1 to B.3 | `04_descriptive_stats.R` | `outputs/tab_descriptives_skim_categorical.tex`, `outputs/tab_descriptives_skim_numeric.tex` and `outputs/tab_descriptives_balance.tex` |

Tables A.1 and A.4 are not produced by the code.

## Running the code

The scripts run in order, from `01` to `05`. Script 01 reads the confidential survey workbook from `../report_EN/analysis_reduced_ENG.xlsm`, relative to this folder, so the code cannot be run from this repository alone. Scripts 01 and 02 save business-level tables in `inputs/`, and script 03 saves the analysis table and the fitted models in `outputs/`. These files are not part of the repository. File paths are resolved with the `here` package from the `.here` file in this folder.

## Software

R 4.6.1 with broom 1.0.13, caret 7.0.1, dplyr 1.2.1, forcats 1.0.1, ggplot2 4.0.3, here 1.0.2, kableExtra 1.4.0, knitr 1.51, logistf 1.26.1, marginaleffects 1.0.0, mice 3.19.0, modelsummary 2.6.0, nnet 7.3.20, performance 0.17.0, pROC 1.19.0.1, readxl 1.5.0, ResourceSelection 0.3.6, stringr 1.6.0, tidyr 1.3.2 and VGAM 1.1.14.
