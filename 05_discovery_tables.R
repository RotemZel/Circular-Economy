###############################################################################
##  05_discovery_tables.R
##
##  How existing by-product arrangements were found, and whether the receiving
##  business is in the same town. Two descriptive items, neither of which enters
##  any model. They supply the evidence for the embeddedness argument in the
##  Discussion: the local social network is not the channel through which
##  by-product exchange is discovered, and most exchange leaves the town.
##
##  This runs on its own, after 02_build_analysis_frame.R. It used to be a block
##  pasted into the middle of the analysis script, which meant it depended on
##  objects created elsewhere.
##
##  Input : inputs/analysis_df.rds
##  Output: outputs/tab_discovery.tex
###############################################################################

library(dplyr)
library(modelsummary)
library(here)

dir.create(here("outputs"), showWarnings = FALSE)

## `same_town` is TRUE/FALSE in the analysis table; give it the labels the
## paper uses so the table reads the same way
if (!file.exists(here("inputs", "analysis_df.rds"))) {
  stop("inputs/analysis_df.rds not found under:\n  ", here(),
       "\nRun 01_make_waste_tidy.R and 02_build_analysis_frame.R first.")
}

discovery <- readRDS(here("inputs", "analysis_df.rds")) %>%
  mutate(same_town = factor(same_town, levels = c(TRUE, FALSE),
                            labels = c("Same town", "Different town")))

cat("\n########## DISCOVERY CHANNEL AND LOCALITY OF EXCHANGE ##########\n")

cat("\nhow the counterpart was found (item 36):\n")
print(datasummary(found_via ~ N + Percent("col"), data = discovery,
                  output = "data.frame"))

cat("\nis the receiving business in the same town? (item 29):\n")
print(datasummary(same_town ~ N + Percent("col"), data = discovery,
                  output = "data.frame"))

cat("\nclassified:", sum(!is.na(discovery$found_via)),
    "| destination recorded:", sum(!is.na(discovery$same_town)),
    "| businesses reporting an arrangement:", sum(discovery$transfer == 1, na.rm = TRUE), "\n")

## the table used in the manuscript: the two items stacked in one block
tab_discovery <- datasummary(
  (`How the counterpart was found`      = found_via) +
  (`Location of the receiving business`  = same_town) ~ N + Percent("col"),
  data   = discovery,
  output = "latex_tabular",
  title  = paste("How existing by-product arrangements were found, and whether",
                 "the receiving business lies in the same town."),
  notes  = paste("Both items are asked only of businesses reporting an existing",
                 "arrangement. The discovery item is free text, classified by the",
                 "rule set out in Section~\\ref{sec:analytical}. Percentages are of",
                 "the classified or recorded total, not of the full sample."))

writeLines(tab_discovery, here("outputs", "tab_discovery.tex"))
cat("\ntab_discovery.tex written to outputs/\n")

## The version of this table in the manuscript is finished by hand (the float,
## the caption, the \label and the two italic block headings are added there),
## so check the numbers below against it rather than replacing the file blindly.

## the three figures quoted in the Results and the Conclusions
found_shares <- prop.table(table(discovery$found_via))
cat("\ntaker-initiated:", round(100 * found_shares[["Taker approached the generator"]], 1), "%",
    "| social tie:",     round(100 * found_shares[["Social tie or word of mouth"]], 1), "%",
    "| same town:",      round(100 * mean(discovery$same_town == "Same town", na.rm = TRUE), 1), "%\n")
