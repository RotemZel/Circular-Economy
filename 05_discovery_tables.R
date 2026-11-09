# 05_discovery_tables.R
#
# Describes how existing by-product arrangements were found (item 36) and
# whether the receiving business is in the same town (item 29), as counts and
# as percentages of the businesses with a coded answer to each item (item-36
# answers that no pattern matched are left out), prints both to the console
# and writes them stacked in one LaTeX table.
#
# Reads:  inputs/analysis_df.rds (written by 02_build_analysis_frame.R)
# Writes: outputs/tab_discovery.tex

# 1. load the analysis table ----

businesses <- readRDS(here::here("inputs", "analysis_df.rds"))

# the businesses that transfer a by-product (1) and those that do not (0)
print(table(transfer = businesses$transfer))

# 2. count the coded answers to each item and their share of the coded answers ----

discovery <- businesses |>
  dplyr::transmute(
    `Origin of the arrangement`          = found_via,
    `Location of the receiving business` = factor(same_town, levels = c(TRUE, FALSE),
                                                  labels = c("Same town", "Different town"))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "item", values_to = "answer",
                      values_drop_na = TRUE) |>
  dplyr::count(item, answer, name = "N") |>
  dplyr::mutate(Percent = 100 * N / sum(N), .by = item)

print(as.data.frame(discovery))

# 3. write both items stacked in one LaTeX table ----
# Each item's name is shown on its first row only.

discovery |>
  dplyr::mutate(item = dplyr::if_else(duplicated(item), "", item)) |>
  knitr::kable(format = "latex", booktabs = TRUE, digits = 1, linesep = "",
               col.names = c("", "", "n", "%"),
               caption = paste("Existing by-product arrangements: origin of the arrangement and",
                               "location of the receiving business (percent of the businesses",
                               "answering each item).")) |>
  writeLines(here::here("outputs", "tab_discovery.tex"))
