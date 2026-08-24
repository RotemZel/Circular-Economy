###############################################################################
##  02_build_analysis_frame.R
##
##  Builds the business-level table the models use, from the three tidy tables
##  written by 01_make_waste_tidy.R. Everything here is a join, a group-and-
##  summarise, or a mutate: the three lookups it needs (sector groups, owner
##  roles and volume bands) are read from codebook.xlsx, not written into the
##  script.
##
##  Variables and their types are the same as before, so scripts 03, 04 and 05
##  read this table unchanged.
##
##  Output: inputs/analysis_df.rds
###############################################################################

library(dplyr)
library(tidyr)
library(forcats)
library(readxl)
library(here)

if (!file.exists(here("inputs", "waste_tidy.RData"))) {
  stop("inputs/waste_tidy.RData not found under:\n  ", here(),
       "\nRun 01_make_waste_tidy.R first. If the folder above is not the",
       " project folder, run here::set_here(\"<full path to the analysis",
       " folder>\") once and restart R.")
}

load(here("inputs", "waste_tidy.RData"))
survey     <- waste_tidy$survey
containers <- waste_tidy$containers
streams    <- waste_tidy$streams

## the tables in codebook.xlsx start on row 3, below a note explaining each one
codebook <- here("inputs", "codebook.xlsx")

sector_codes <- read_excel(codebook, sheet = "sector_groups",      skip = 2)
owner_codes  <- read_excel(codebook, sheet = "owner_roles",        skip = 2)
volume_codes <- read_excel(codebook, sheet = "waste_volume_bands", skip = 2)

## ---------------------------------------------------------------------------
## 1. what each business generates
## ---------------------------------------------------------------------------
## A business can report several streams, and several streams can belong to the
## same broad group, so the groups are reduced to one row per business per
## group first.

groups_present <- streams %>%
  filter(!is.na(stream_group)) %>%
  distinct(business_id, stream_group)

## one 0/1 column per group (organic, fibre, plastic, oils, durable)
stream_indicators <- groups_present %>%
  mutate(present = 1L) %>%
  pivot_wider(names_from = stream_group, values_from = present, values_fill = 0L)

## how many distinct groups the business generates
stream_counts <- count(groups_present, business_id, name = "n_streams")

## the highest weekly volume band reported across the business's streams.
## Businesses with no usable band simply do not appear here, so they end up
## missing after the join, which is what we want.
volume_max <- streams %>%
  filter(!is.na(band_number)) %>%
  group_by(business_id) %>%
  summarise(volume_n = max(band_number), .groups = "drop")

## ---------------------------------------------------------------------------
## 2. storage capacity
## ---------------------------------------------------------------------------
## Capacity is the number of containers multiplied by the smallest container
## volume the business reported, so it is a lower bound. "Other" has no volume
## in the codebook and is left out of the minimum.

smallest_container <- containers %>%
  filter(!is.na(litres)) %>%
  group_by(business_id) %>%
  summarise(litres_min = min(litres), .groups = "drop")

## ---------------------------------------------------------------------------
## 3. the business-level table
## ---------------------------------------------------------------------------

businesses <- survey %>%
  left_join(sector_codes,      by = "business_type") %>%
  left_join(stream_indicators, by = "business_id") %>%
  left_join(stream_counts,     by = "business_id") %>%
  left_join(volume_max,        by = "business_id") %>%
  left_join(smallest_container, by = "business_id") %>%
  mutate(
    ## the four outcomes
    transfer   = as.numeric(transfers_byproduct),
    willing_tr = as.numeric(willing_to_transfer),
    receive    = as.numeric(receives_byproduct),
    willing_rc = as.numeric(willing_to_receive),

    ## town, relabelled A, B, C in order of sample size. LETTERS is used so
    ## this does not assume there are exactly three towns.
    settlement = fct_relabel(city, ~ LETTERS[seq_along(.x)]),

    ## sector, with retail as the reference category
    sector = relevel(factor(sector), ref = "Retail"),
    food   = as.integer(sector == "Food"),

    ## firm and respondent characteristics
    micro       = as.integer(is_micro),
    firm_age    = years_midpoint,
    firm_age_ln = log(firm_age),
    owner       = if_else(is.na(respondent_role), NA_integer_,
                          as.integer(respondent_role %in% owner_codes$respondent_role)),
    resp_age    = respondent_age,
    female      = as.integer(respondent_sex == "Female"),
    tertiary    = as.integer(is_tertiary),

    ## waste-management context
    pays    = as.numeric(pays_for_disposal),
    problem = as.numeric(perceives_disposal_problem),
    reduces = as.numeric(tries_to_reduce_waste),   # ENDOGENOUS: screen only
    epr     = as.numeric(has_tamir_arrangement),
    knows   = as.numeric(knows_other_exchangers),

    ## descriptive discovery items, carried through but never modelled
    same_town = receiver_same_town,

    ## storage capacity
    n_cont      = n_containers,
    capacity    = n_containers * litres_min,
    capacity_ln = log(capacity),

    ## a business with no stream record generates none of the groups
    across(c(organic, fibre, plastic, oils, durable), ~ replace_na(.x, 0L)),
    n_streams = replace_na(n_streams, 0L),

    volume = factor(volume_n,
                    levels = volume_codes$band_number,
                    labels = volume_codes$short_label, ordered = TRUE),

    ## the four-state typology used in section 6 of script 03
    typology = case_when(
      is.na(transfer) | is.na(willing_tr) ~ NA_character_,
      transfer == 1 & willing_tr == 1     ~ "Established",
      transfer == 1 & willing_tr == 0     ~ "Reluctant",
      transfer == 0 & willing_tr == 1     ~ "Latent",
      TRUE                                ~ "Detached"),
    typology = relevel(factor(typology), ref = "Detached")
  ) %>%
  select(business_id, transfer, willing_tr, receive, willing_rc,
         settlement, sector, food, micro, firm_age, firm_age_ln, owner,
         resp_age, female, tertiary, found_via, same_town,
         pays, problem, reduces, epr, knows,
         n_cont, capacity, capacity_ln,
         organic, fibre, plastic, oils, durable, n_streams, volume_n, volume,
         typology)

## the five group columns must all exist, even if a group were never reported
stopifnot(all(c("organic", "fibre", "plastic", "oils", "durable") %in% names(businesses)))

cat("analysis table:", nrow(businesses), "businesses x", ncol(businesses), "variables\n")
saveRDS(businesses, here("inputs", "analysis_df.rds"))
