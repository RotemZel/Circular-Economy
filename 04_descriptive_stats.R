# 04_descriptive_stats.R
#
# Builds the descriptive-statistics tables for Appendix B: sample
# characteristics overall, and the main variables by whether the business
# transfers a by-product.
#
# Reads:  inputs/analysis_df.rds (written by 02_build_analysis_frame.R)
# Writes: outputs/tab_descriptives_skim_categorical.tex
#         outputs/tab_descriptives_skim_numeric.tex
#         outputs/tab_descriptives_balance.tex

# 1. set the table format and load the analysis table ----

# modelsummary writes booktabs LaTeX tables with plain numbers
options(modelsummary_factory_latex         = "kableExtra",
        modelsummary_factory_latex_tabular = "kableExtra",
        modelsummary_format_numeric_latex  = "plain")

# the analysis table, with the variables named as the tables show them
descriptives <- readRDS(here::here("inputs", "analysis_df.rds")) |>
  dplyr::transmute(
    Sector                      = sector,
    Town                        = settlement,
    `Micro-business (4 or fewer employees)` = factor(micro, levels = c(0, 1), labels = c("No", "Yes")),
    `Respondent sex (female)`   = factor(female,     levels = c(0, 1), labels = c("No", "Yes")),
    `Tertiary education`        = factor(tertiary,   levels = c(0, 1), labels = c("No", "Yes")),
    Transfer                    = factor(transfer,   levels = c(0, 1), labels = c("No", "Yes")),
    `Willingness to supply`     = factor(willing_tr, levels = c(0, 1), labels = c("No", "Yes")),
    `Payment for waste disposal` = factor(pays,      levels = c(0, 1), labels = c("No", "Yes")),
    `Awareness of local exchange` = factor(knows,    levels = c(0, 1), labels = c("No", "Yes")),
    `Organic residues`          = factor(organic,    levels = c(0, 1), labels = c("No", "Yes")),
    `Waste oils`                = factor(oils,       levels = c(0, 1), labels = c("No", "Yes")),
    `Firm age (years)`          = firm_age,
    `Respondent age (years)`    = resp_age,
    `Storage capacity (litres)` = capacity,
    `Material groups (n)`       = n_groups)

# 2. summarise the whole sample: categorical and continuous variables ----

modelsummary::datasummary_skim(
  descriptives, type = "categorical", output = "latex_tabular",
  title = paste0("Sample characteristics: categorical variables (n = ",
                 nrow(descriptives), " businesses).")) |>
  writeLines(here::here("outputs", "tab_descriptives_skim_categorical.tex"))

modelsummary::datasummary_skim(
  descriptives, type = "numeric", output = "latex_tabular") |>
  writeLines(here::here("outputs", "tab_descriptives_skim_numeric.tex"))

# 3. summarise the main variables by transfer status ----
# The main variables are compared between the businesses that transfer a
# by-product and those that do not.

modelsummary::datasummary_balance(
  ~ Transfer,
  data   = dplyr::select(descriptives, Transfer, Sector,
                         `Micro-business (4 or fewer employees)`,
                         `Respondent sex (female)`, `Tertiary education`,
                         `Awareness of local exchange`, `Organic residues`, `Waste oils`,
                         `Firm age (years)`, `Storage capacity (litres)`,
                         `Material groups (n)`),
  dinm   = FALSE,
  output = "latex",
  title  = paste0("Sample characteristics by transfer status (n = ",
                  nrow(descriptives), " businesses).")) |>
  writeLines(here::here("outputs", "tab_descriptives_balance.tex"))
