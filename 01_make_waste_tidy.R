# 01_make_waste_tidy.R
#
# Turns the survey workbook into three tidy tables: survey (one row per
# business), containers (one row per container size a business reported) and
# streams (one row per stream answer, plus one row per stream record with
# no answer, so that its weekly volume counts). The workbook is the English
# translation of the Hebrew survey. Only the columns the analysis uses are
# read; every other column is skipped. The lookups (material groups,
# container volumes, volume bands, business size, business age and education)
# are read from the codebook, one sheet each; the other coding rules are in
# settings and in the comments of sections 2 to 4.
#
# Reads:  ../report_EN/analysis_reduced_ENG.xlsm (sheets survey_0 and Waste_1)
#         inputs/codebook.xlsx (sheets waste_streams, container_sizes,
#         waste_volume_bands, business_size, business_age and education_levels)
# Writes: inputs/waste_tidy.RData (the list waste_tidy)

settings <- list(
  # item 36 answers coded as the generator seeking the taker (a first-person
  # opening); the four patterns are tested in this order and the first match wins
  found_via_generator = "^i\\b|^we\\b",
  # item 36 answers coded as a social tie or word of mouth
  found_via_social    = "neighbourhood|word of mouth|truck driver",
  # item 36 answers coded as an online platform
  found_via_online    = "facebook|internet",
  # item 36 answers coded as the taker approaching the generator
  found_via_taker     =
    "approach|found (me|us)|called|came and asked|ask|search|looked for|offered|available",
  # answers to "If not: why not?" after item 72 (why the business would not take
  # in another business's by-products) coded as the quality of the material; the
  # three patterns are tested in this order, the first match wins, and an answer
  # that no pattern matches is coded "Other"
  refusal_quality     = "quality|clean|hygien",
  # the same answers coded as the material not being usable in the business
  refusal_cannot_use  = "use|suitable|relevant|\\bfit",
  # the same answers coded as no need or no benefit
  refusal_no_need     = "need|help|benefit|nothing|anything",
  # self-reported respondent ages below this are set to missing
  respondent_age_min  = 15,
  # self-reported respondent ages above this are set to missing
  respondent_age_max  = 90
)

# 1. read the codebook and list the survey questions the analysis uses ----

# the codebook sheets, one table each; each table starts on row 3, below a note
# explaining it
codebook <- list(
  waste_streams      = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                          sheet = "waste_streams", skip = 2),
  container_sizes    = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                          sheet = "container_sizes", skip = 2),
  waste_volume_bands = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                          sheet = "waste_volume_bands", skip = 2),
  business_size      = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                          sheet = "business_size", skip = 2),
  business_age       = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                          sheet = "business_age", skip = 2),
  education_levels   = readxl::read_excel(here::here("inputs", "codebook.xlsx"),
                                          sheet = "education_levels", skip = 2))

workbook <- here::here("..", "report_EN", "analysis_reduced_ENG.xlsm")

# the questions the analysis uses, as they head the workbook's columns, each
# with the short name its column gets here: first the questions of the business
# sheet, then those of the waste-stream sheet and the column that links each
# stream record to its business
questions <- c(
  record_id                  = "GlobalID",
  city                       = "2) City",
  business_type              = "5) Business classification",
  business_size_raw          = "6) Size of the business",
  business_age               = "7) How many years has the business existed?",
  respondent_role            = "8) Your role at the business",
  n_containers_raw           = "15) How many collection containers are there at the business?",
  container_sizes_raw        = "16) What is the size of the collection container (rubbish bin) at the business?",
  pays_for_disposal          = "19) Do you pay for waste disposal?",
  perceives_disposal_problem = "22) Do you feel you have a problem or challenge with waste disposal at the business? Yes / No",
  tries_to_reduce_waste      = "23) Do you try to reduce the quantity of waste at the business? Yes / No. If yes, by what means?",
  has_tamir_arrangement      = "24) Does the business have an arrangement with the Tamir corporation for waste recycling? Yes / No. If not, why?",
  transfers_byproduct        = "26) Do you transfer a by-product from your business to another business",
  receiver_same_town         = "29) Is the business it is transferred to located in the same locality?",
  found_via_text             = "36) How did you find the taking-in business?",
  receives_byproduct         = "44) Do you receive by-product materials from other businesses and use them as raw material at your business? Yes / No.",
  willing_to_transfer        = "65) Would you be willing to transfer your by-products as an input to other businesses?",
  willing_to_receive         = "71) Would you be willing to take in by-products of other businesses as raw materials for your business? Yes / No.",
  refusal_reason_text        = "If not: why not?",
  knows_other_exchangers     = "77) Do you know businesses in the locality that hand over by-products to other businesses or take in by-products from other businesses? Yes / No.",
  respondent_age_raw         = "79) Age: ______ years",
  respondent_sex_raw         = "80) Sex: Male / Female",
  respondent_education_raw   = "81) Education: up to 11 years of schooling / full secondary / post-secondary / academic",
  streams_raw                = "11) What are the main waste streams at the business?",
  volume_raw                 = "12) How much waste is there per week?",
  parent_record_id           = "ParentGlobalID")

# the question texts that head the columns of the two sheets, exactly as they
# appear in the workbook, read without any answers
headers <- list(
  survey_0 = names(readxl::read_excel(workbook, sheet = "survey_0", n_max = 0,
                                      .name_repair = "minimal")),
  Waste_1  = names(readxl::read_excel(workbook, sheet = "Waste_1", n_max = 0,
                                      .name_repair = "minimal")))

# 2. read the survey columns the analysis uses and give them proper types ----
# Only the columns whose question is listed in section 1 are read; every other
# column (addresses, contact details, coordinates, interview records, other
# free text) is skipped, so it is never read. A running business number is
# added; the workbook's record ID only links the stream records and is not
# saved. The ten yes/no answers are stacked in one column, coded TRUE when they
# start with y and FALSE when they start with n (anything else is missing), and
# put back in their own columns. A container count of "other" (more than three)
# is missing, and so is a respondent age that is not a number or lies outside
# the bounds in settings. Towns are ordered from the most to the least
# surveyed. The free-text answers to item 36 and to "If not: why not?" after
# item 72 are coded with the patterns in settings. The codebook gives each size
# answer its label and micro-business flag, each age band its midpoint in years,
# and each education answer its label and tertiary flag.

survey <- readxl::read_excel(workbook, sheet = "survey_0",
                             col_types = ifelse(headers$survey_0 %in% questions,
                                                "text", "skip")) |>
  dplyr::rename(dplyr::any_of(questions)) |>
  dplyr::mutate(business_id = dplyr::row_number()) |>
  tidyr::pivot_longer(c(pays_for_disposal, perceives_disposal_problem, tries_to_reduce_waste,
                        has_tamir_arrangement, transfers_byproduct, receives_byproduct,
                        willing_to_transfer, willing_to_receive, knows_other_exchangers,
                        receiver_same_town),
                      names_to = "yes_no_item", values_to = "yes_no_answer") |>
  dplyr::mutate(yes_no_answer = stringr::str_to_lower(stringr::str_trim(yes_no_answer)),
                yes_no_answer = dplyr::case_when(stringr::str_starts(yes_no_answer, "y") ~ TRUE,
                                                 stringr::str_starts(yes_no_answer, "n") ~ FALSE)) |>
  tidyr::pivot_wider(names_from = yes_no_item, values_from = yes_no_answer) |>
  dplyr::mutate(
    city            = forcats::fct_infreq(stringr::str_trim(city)),
    business_type   = stringr::str_trim(business_type),
    business_age    = stringr::str_trim(business_age),
    respondent_role = stringr::str_trim(respondent_role),

    n_containers = as.numeric(dplyr::na_if(stringr::str_trim(n_containers_raw), "other")),

    respondent_age = as.numeric(stringr::str_trim(respondent_age_raw)),
    respondent_age = dplyr::if_else(respondent_age < settings$respondent_age_min |
                                      respondent_age > settings$respondent_age_max,
                                    NA, respondent_age),

    respondent_sex = forcats::as_factor(stringr::str_trim(respondent_sex_raw)),

    found_text = stringr::str_to_lower(stringr::str_trim(found_via_text)),
    found_via  = factor(
      dplyr::case_when(
        stringr::str_detect(found_text, settings$found_via_generator) ~
          "Generator sought a taker",
        stringr::str_detect(found_text, settings$found_via_social) ~
          "Personal tie or word of mouth",
        stringr::str_detect(found_text, settings$found_via_online) ~
          "Online platform",
        stringr::str_detect(found_text, settings$found_via_taker) ~
          "Taker approached the generator"),
      levels = c("Taker approached the generator", "Generator sought a taker",
                 "Online platform", "Personal tie or word of mouth")),

    refusal_text   = stringr::str_to_lower(stringr::str_trim(refusal_reason_text)),
    refusal_reason = factor(
      dplyr::case_when(
        is.na(refusal_text) | refusal_text == "" ~ NA,
        stringr::str_detect(refusal_text, settings$refusal_quality) ~
          "Quality of the material",
        stringr::str_detect(refusal_text, settings$refusal_cannot_use) ~
          "Cannot be used in the business",
        stringr::str_detect(refusal_text, settings$refusal_no_need) ~
          "No need or no benefit",
        TRUE ~ "Other"),
      levels = c("No need or no benefit", "Cannot be used in the business",
                 "Quality of the material", "Other"))
  ) |>
  dplyr::left_join(codebook$business_size,    by = c("business_size_raw"        = "raw_label")) |>
  dplyr::left_join(codebook$business_age,     by = c("business_age"             = "raw_label")) |>
  dplyr::left_join(codebook$education_levels, by = c("respondent_education_raw" = "raw_label"))

# the businesses surveyed, the size, age and education answers that the
# codebook does not cover, the item-36 answers that no pattern matched, and the
# refusal answers coded "Other"
print(c(businesses          = nrow(survey),
        size_not_coded      = sum(!is.na(survey$business_size_raw) & is.na(survey$business_size)),
        age_not_coded       = sum(!is.na(survey$business_age) & is.na(survey$years_midpoint)),
        education_not_coded = sum(!is.na(survey$respondent_education_raw) & is.na(survey$education)),
        origin_not_coded    = sum(!is.na(survey$found_text) & is.na(survey$found_via)),
        refusal_other       = sum(survey$refusal_reason == "Other", na.rm = TRUE)))

# 3. build the containers table: one row per container size a business reported ----
# Item 16 allows several sizes in one comma-separated answer. Each size goes on
# its own row, empty answers are dropped, and each size is labelled from the
# codebook.

containers <- survey |>
  dplyr::select(business_id, container_sizes_raw) |>
  tidyr::separate_longer_delim(container_sizes_raw, delim = ",") |>
  dplyr::mutate(raw_token = stringr::str_to_lower(stringr::str_trim(container_sizes_raw))) |>
  dplyr::filter(!is.na(raw_token), raw_token != "") |>
  dplyr::left_join(codebook$container_sizes, by = "raw_token") |>
  dplyr::select(business_id, container_label, litres)

# the container answers that the codebook does not cover
print(c(containers_not_coded = sum(is.na(containers$container_label))))

# 4. build the streams table: one row per stream answer, and one row for each stream record with no answer ----
# Only the columns whose question is listed in section 1 are read: the waste
# streams, the weekly volume and the link to the business, which gives each
# stream record the number of its business. Stream records whose business is
# not in the survey are dropped. Each comma-separated stream answer goes on its
# own row and empty pieces are dropped; a record with no stream answer keeps one
# row, so that its weekly volume still counts. Each stream is labelled from the
# codebook ("other (specify)" has a label but no material group) together with
# its volume band.

streams <- readxl::read_excel(workbook, sheet = "Waste_1",
                              col_types = ifelse(headers$Waste_1 %in% questions,
                                                 "text", "skip")) |>
  dplyr::rename(dplyr::any_of(questions)) |>
  dplyr::left_join(dplyr::select(survey, business_id, record_id),
                   by = c("parent_record_id" = "record_id"))

# the stream records whose business is not in the survey
print(c(records_without_business = sum(is.na(streams$business_id))))

streams <- streams |>
  dplyr::filter(!is.na(business_id)) |>
  dplyr::mutate(streams_raw = dplyr::na_if(stringr::str_trim(streams_raw), ""),
                volume_raw  = stringr::str_trim(volume_raw)) |>
  tidyr::separate_longer_delim(streams_raw, delim = ",") |>
  dplyr::mutate(raw_token = stringr::str_to_lower(stringr::str_trim(streams_raw))) |>
  dplyr::filter(is.na(raw_token) | raw_token != "") |>
  dplyr::left_join(codebook$waste_streams,      by = "raw_token") |>
  dplyr::left_join(codebook$waste_volume_bands, by = c("volume_raw" = "raw_label")) |>
  dplyr::select(business_id, raw_token, stream_label, stream_group, volume_raw, band_number)

# the stream and volume answers that the codebook does not cover
print(c(streams_not_coded = sum(!is.na(streams$raw_token) & is.na(streams$stream_label)),
        volumes_not_coded = sum(!is.na(streams$volume_raw) & is.na(streams$band_number))))

# 5. save the columns script 02 uses ----
# Only the columns script 02 uses are kept; everything else, including the
# workbook's record IDs and the free text of item 36 and of the refusal
# question, is left out.

waste_tidy <- list(
  survey     = dplyr::select(survey, business_id, city, business_type, respondent_role,
                             pays_for_disposal, perceives_disposal_problem,
                             tries_to_reduce_waste, has_tamir_arrangement,
                             transfers_byproduct, receives_byproduct, willing_to_transfer,
                             willing_to_receive, knows_other_exchangers, receiver_same_town,
                             n_containers, respondent_age, respondent_sex, found_via,
                             refusal_reason, is_micro, years_midpoint, is_tertiary),
  containers = dplyr::select(containers, business_id, litres),
  streams    = dplyr::select(streams, business_id, stream_group, band_number))

save(waste_tidy, file = here::here("inputs", "waste_tidy.RData"))
