###############################################################################
##  04_descriptive_stats.R
##
##  Companion to 03_waste_analysis.R. Produces the descriptive-statistics
##  tables for Appendix B: sample characteristics overall, and the same
##  variables broken down by whether the business transfers a by-product.
##
##  Input : inputs/analysis_df.rds  (built by 01_make_waste_tidy.R and
##          02_build_analysis_frame.R; run those first if missing)
##  Output: outputs/tab_descriptives_skim_categorical.tex
##          outputs/tab_descriptives_skim_numeric.tex
##          outputs/tab_descriptives_balance.tex
##
##  NOTE: these three files are finished by hand in the manuscript before use.
##  The float, the caption, the \label and the note about percentages are added
##  there, not here, so do not overwrite the versions in tabs/ without
##  re-adding them.
###############################################################################

library(dplyr)
library(tidyr)
library(modelsummary)
library(here)

if (!file.exists(here("inputs", "analysis_df.rds"))) {
  stop("inputs/analysis_df.rds not found under:\n  ", here(),
       "\nRun 01_make_waste_tidy.R and 02_build_analysis_frame.R first. If the",
       " folder above is not the project folder, run",
       " here::set_here(\"<full path to the analysis folder>\") once and",
       " restart R.")
}

dir.create(here("outputs"), showWarnings = FALSE)

options(modelsummary_factory_latex         = "gt")
options(modelsummary_factory_latex_tabular = "kableExtra")
options(modelsummary_format_numeric_latex  = "plain")

## readable names for the table rows
d <- readRDS(here("inputs", "analysis_df.rds")) %>%
  as.data.frame() %>%
  mutate(
    Sector                      = factor(sector, levels = c("Retail", "Food",
                                                            "Manufacturing", "Services")),
    Settlement                  = factor(settlement),
    Micro                       = factor(micro,      levels = c(0, 1), labels = c("No", "Yes")),
    Female                      = factor(female,     levels = c(0, 1), labels = c("No", "Yes")),
    `Tertiary education`        = factor(tertiary,   levels = c(0, 1), labels = c("No", "Yes")),
    Transfers                   = factor(transfer,   levels = c(0, 1), labels = c("No", "Yes")),
    `Willing to supply`         = factor(willing_tr, levels = c(0, 1), labels = c("No", "Yes")),
    `Pays for disposal`         = factor(pays,       levels = c(0, 1), labels = c("No", "Yes")),
    `Aware of exchangers`       = factor(knows,      levels = c(0, 1), labels = c("No", "Yes")),
    Organic                     = factor(organic,    levels = c(0, 1), labels = c("No", "Yes")),
    `Waste oil`                 = factor(oils,       levels = c(0, 1), labels = c("No", "Yes")),
    `Firm age (years)`          = firm_age,
    `Respondent age (years)`    = resp_age,
    `Storage capacity (litres)` = capacity,
    `Waste streams (n)`         = n_streams)

cat("\n########## APPENDIX B: DESCRIPTIVE STATISTICS, n =", nrow(d), "##########\n")

###############################################################################
## 1. overall sample characteristics
###############################################################################

skim_vars <- c("Sector", "Settlement", "Micro", "Female", "Tertiary education",
               "Transfers", "Willing to supply", "Pays for disposal",
               "Aware of exchangers", "Organic", "Waste oil",
               "Firm age (years)", "Respondent age (years)",
               "Storage capacity (litres)", "Waste streams (n)")

datasummary_skim(d[, skim_vars], type = "categorical", output = "latex_tabular",
                 title = "Sample characteristics: categorical variables (n = 200 businesses).") |>
  writeLines(here("outputs", "tab_descriptives_skim_categorical.tex"))

datasummary_skim(d[, skim_vars], type = "numeric", output = "latex_tabular",
                 title = "Sample characteristics: continuous variables (n = 200 businesses).") |>
  writeLines(here("outputs", "tab_descriptives_skim_numeric.tex"))

cat("\ncategorical:\n")
print(datasummary_skim(d[, skim_vars], type = "categorical", output = "data.frame"))
cat("\ncontinuous:\n")
print(datasummary_skim(d[, skim_vars], type = "numeric", output = "data.frame"))

###############################################################################
## 2. the same variables by transfer status
###############################################################################
## datasummary_balance() reports the difference in means and its standard
## error, and no test for the categorical variables, so the caption says
## exactly that. To report p-values instead of standard errors, add
## dinm_statistic = "p.value" and change the caption to match.

balance_vars <- c("Sector", "Micro", "Female", "Tertiary education",
                  "Aware of exchangers", "Organic", "Waste oil",
                  "Firm age (years)", "Storage capacity (litres)",
                  "Waste streams (n)")

datasummary_balance(
  ~ Transfers,
  data   = d[, c("Transfers", balance_vars)],
  output = "latex_tabular",
  title  = paste("Sample characteristics by transfer status, with differences",
                 "in means (n = 200 businesses).")) |>
  writeLines(here("outputs", "tab_descriptives_balance.tex"))

cat("\nby transfer status:\n")
print(datasummary_balance(~ Transfers, data = d[, c("Transfers", balance_vars)],
                          output = "data.frame"))

cat("\nthree tables written to outputs/\n")
cat("\n########## DONE ##########\n")
