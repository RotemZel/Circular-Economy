# 02_build_analysis_frame.R
#
# Builds the business-level table that scripts 03, 04 and 05 model and
# describe: one row per business, joining the survey answers to per-business
# summaries of waste streams and storage containers, and recoding the
# variables. It also prints the number of businesses by survey category and
# sector (Table A.2). The reference categories of the sector and typology
# factors are in settings.
#
# Reads:  inputs/waste_tidy.RData (the list waste_tidy, written by
#         01_make_waste_tidy.R)
#         inputs/codebook.xlsx (sheets sector_groups and owner_roles)
# Writes: inputs/analysis_df.rds (the table businesses)

settings <- list(
  # reference category of the sector factor
  sector_reference   = "Retail",
  # reference category of the transfer typology
  typology_reference = "Detached"
)

# 1. read the tidy survey tables and the codebook sheets ----

# the list waste_tidy: the survey, containers and streams tables from script 01
load(here::here("inputs", "waste_tidy.RData"))

# the codebook sheets, one table each; each table starts on row 3, below a note
# explaining it
codebook <- list(
  sector_groups = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                     sheet = "sector_groups", skip = 2),
  owner_roles   = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                     sheet = "owner_roles", skip = 2))

# 2. summarise the waste streams and the containers of each business ----

# one row per business with a stream record: a 0/1 indicator for each material
# group among its streams, and the number of different groups
materials <- waste_tidy$streams |>
  dplyr::summarise(organic  = as.integer(any(stream_group %in% "organic")),
                   fibre    = as.integer(any(stream_group %in% "fibre")),
                   plastic  = as.integer(any(stream_group %in% "plastic")),
                   oils     = as.integer(any(stream_group %in% "oils")),
                   durable  = as.integer(any(stream_group %in% "durable")),
                   n_groups = dplyr::n_distinct(stream_group, na.rm = TRUE),
                   .by = business_id)

# one row per business with a volume answer: the highest weekly volume band
# across its stream records
volume <- waste_tidy$streams |>
  dplyr::filter(!is.na(band_number)) |>
  dplyr::summarise(volume_n = max(band_number), .by = business_id)

# one row per business with a listed container size: the smallest listed
# container volume; "other" has no volume and is left out
container_volume <- waste_tidy$containers |>
  dplyr::filter(!is.na(litres)) |>
  dplyr::summarise(litres_min = min(litres), .by = business_id)

# 3. build the business-level table ----
# The survey answers are joined to the sector and to the three summaries. A
# business with no stream record gets 0 for every material group and for the
# number of groups. Storage capacity is the number of containers times the
# smallest container volume.

businesses <- waste_tidy$survey |>
  dplyr::left_join(codebook$sector_groups, by = "business_type") |>
  dplyr::left_join(materials, by = "business_id") |>
  dplyr::left_join(volume, by = "business_id") |>
  dplyr::left_join(container_volume, by = "business_id") |>
  tidyr::replace_na(list(organic = 0L, fibre = 0L, plastic = 0L, oils = 0L,
                         durable = 0L, n_groups = 0L)) |>
  dplyr::mutate(
    # the four outcomes
    transfer   = as.numeric(transfers_byproduct),
    willing_tr = as.numeric(willing_to_transfer),
    receive    = as.numeric(receives_byproduct),
    willing_rc = as.numeric(willing_to_receive),

    # the three towns, labelled A, B and C from the most to the least surveyed
    settlement = factor(city, labels = c("A", "B", "C")),

    # sector, with the reference category in settings
    sector = relevel(factor(sector), ref = settings$sector_reference),

    # firm and respondent characteristics
    micro       = as.integer(is_micro),
    firm_age    = years_midpoint,
    firm_age_ln = log(firm_age),
    owner       = dplyr::if_else(
      is.na(respondent_role), NA,
      as.integer(respondent_role %in% codebook$owner_roles$respondent_role)),
    resp_age    = respondent_age,
    female      = as.integer(respondent_sex == "Female"),
    tertiary    = as.integer(is_tertiary),

    # waste-management context
    pays    = as.numeric(pays_for_disposal),
    problem = as.numeric(perceives_disposal_problem),
    reduces = as.numeric(tries_to_reduce_waste),
    epr     = as.numeric(has_tamir_arrangement),
    knows   = as.numeric(knows_other_exchangers),

    # discovery item: whether the receiving business is in the same town
    same_town = receiver_same_town,

    # storage capacity
    capacity    = n_containers * litres_min,
    capacity_ln = log(capacity),

    # four-state typology of current transfer by willingness to transfer
    typology = dplyr::case_when(
      is.na(transfer) | is.na(willing_tr) ~ NA,
      transfer == 1 & willing_tr == 1     ~ "Established",
      transfer == 1 & willing_tr == 0     ~ "Reluctant",
      transfer == 0 & willing_tr == 1     ~ "Latent",
      TRUE                                ~ "Detached"),
    typology = relevel(factor(typology), ref = settings$typology_reference)
  ) |>
  dplyr::select(business_id, transfer, willing_tr, receive, willing_rc,
                settlement, sector, micro, firm_age, firm_age_ln, owner,
                resp_age, female, tertiary, found_via, same_town, refusal_reason,
                pays, problem, reduces, epr, knows,
                capacity, capacity_ln,
                organic, fibre, plastic, oils, durable, n_groups, volume_n,
                typology)

# 4. count the businesses by survey category and sector (Table A.2) ----

waste_tidy$survey |>
  dplyr::left_join(codebook$sector_groups, by = "business_type") |>
  dplyr::count(sector, business_type) |>
  print()

# 5. check and save the table ----

# the businesses without a sector: a business type that was not answered or is
# not in the codebook
print(c(businesses_without_sector = sum(is.na(businesses$sector))))

saveRDS(businesses, here::here("inputs", "analysis_df.rds"))
