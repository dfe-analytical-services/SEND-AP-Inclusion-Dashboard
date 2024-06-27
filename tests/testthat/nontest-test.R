# Shorthand for frequently used tab names
LAC <- "Local area comparison"
RC <- "Regional comparison"
CT <- "Change over time"

listInputs <- c(
  "absence_la_filter",
  "absence_reg_filter",
  "autism_nat_bench_filter",
  "ccg_choice",
  "destinations_1618_la_bench_filter",
  "destinations_1618_la_bench_filter_two",
  "destinations_1618_la_time_filter",
  "destinations_1618_la_time_filter_two",
  "destinations_1618_la_type_filter",
  "destinations_1618_la_type_filter_two",
  "destinations_1618_reg_bench_filter",
  "destinations_1618_reg_bench_filter_two",
  "destinations_1618_reg_time_filter",
  "destinations_1618_reg_time_filter_two",
  "destinations_1618_reg_type_filter",
  "destinations_1618_reg_type_filter_two",
  "eyfsp_la_time_filter",
  "eyfsp_reg_time_filter",
  "eyfsp_reg_bench_filter",
  "eyfsp_reg_time_filter",
  "discontinued_la_time_filter",
  "ks1_phonics_la_filter",
  "ks1_phonics_reg_filter",
  "ks2_attainment_la_filter",
  "ks2_attainment_reg_filter",
  "ks4_attainment_la_bench_filter",
  "ks4_attainment_reg_bench_filter",
  "ks4_destinations_la_bench_filter",
  "ks4_destinations_la_time_filter",
  "ks4_destinations_la_type_filter",
  "ks4_destinations_reg_bench_filter",
  "ks4_destinations_reg_time_filter",
  "ks4_destinations_reg_type_filter",
  "la_choice",
  "level_choice",
  "link_to_la_dashboard",
  "mainstream_with_sen_la_filter",
  "mainstream_with_sen_reg_filter",
  "myregion_switch",
  "navlistPanel",
  "region_choice",
  "nhs_region_choice",
  "ofsted_table_choice",
  "percent_pupils_ehcp_la_filter",
  "percent_pupils_ehcp_reg_filter",
  "provider_types_la_bench_filter",
  "provider_types_la_time_filter",
  "provider_types_reg_bench_filter",
  "provider_types_reg_time_filter",
  "remove",
  "tabsetpanels_la",
  "tabsetpanels_reg"
)

# Lists of outputs for the Outcomes tab at LA level
outputs_la_outcomes_time <- c(
  "destinations_1618_la_time",
  "ks1_phonics_la_time",
  "ks2_attainment_la_time",
  "ks4_attainment_la_time",
  "la_ofsted_rating",
  "mentalhealth_ccg_time",
  "nhs_value_box_ccg_newest",
  "nhs_value_box_ccg_older",
  "discontinued_la_time"
)

outputs_la_outcomes_time_table <- c(
  "destinations_1618_la_time_table",
  "ks1_phonics_la_time_table",
  "ks2_attainment_la_time_table",
  "ks4_attainment_la_time_table",
  "mentalhealth_ccg_time_table",
  "discontinued_la_time_table"
)

outputs_la_outcomes_bench <- c(
  "destinations_1618_la_bench",
  "eyfsp_la_bench",
  "ks1_phonics_la_bench",
  "ks2_attainment_la_bench",
  "ks4_attainment_la_bench",
  "mentalhealth_ccg_bench",
  "discontinued_la_bench"
)

outputs_la_outcomes_bench_table <- c(
  "destinations_1618_la_bench_table",
  "ks1_phonics_la_bench_table",
  "ks2_attainment_la_bench_table",
  "ks4_attainment_la_bench_table",
  "mentalhealth_ccg_bench_table",
  "discontinued_la_bench_table"
)
# Lists of outputs for the Outcomes tab at region/England level
outputs_reg_outcomes_time <- c(
  "eyfsp_reg_time",
  "destinations_1618_reg_time",
  "ks1_phonics_reg_time",
  "ks2_attainment_reg_time",
  "ks4_attainment_reg_time",
  "reg_ofsted_rating",
  "disco_reg_time"
)

outputs_reg_outcomes_time_table <- outputs_reg_outcomes_time %>%
  str_replace(pattern = "_time", replacement = "_time_table")

outputs_reg_outcomes_bench <- outputs_la_outcomes_bench %>%
  str_replace(pattern = "_la_", replacement = "_reg_") %>%
  str_replace(pattern = "_ccg_", replacement = "_nat_")

outputs_reg_outcomes_bench_table <- outputs_reg_outcomes_bench %>%
  str_replace(pattern = "_bench", replacement = "_time_table")

# Lists of outputs for the Experiences tab at LA/CCG level

outputs_la_experiences_time <- c(
  "timeliness_la_time",
  "autism_ccg_time",
  "tribunals_la_time",
  "absence_la_time",
  "ks4_destinations_la_time"
)

outputs_la_experiences_time_table <- c(
  "timeliness_la_time_table",
  "autism_ccg_time_table",
  "tribunals_la_time_table",
  "absence_la_time_table",
  "ks4_destinations_la_time_table"
)

outputs_la_experiences_bench <- c(
  "timeliness_la_bench",
  "ch_prov_bench",
  "autism_ccg_bench",
  "tribunals_la_bench",
  "absence_la_bench",
  "ks4_destinations_la_bench"
)

outputs_la_experiences_bench_table <- outputs_la_experiences_bench %>%
  str_replace(pattern = "_bench", replacement = "_bench_table")

outputs_reg_experiences_time <- outputs_la_experiences_time %>%
  str_replace(pattern = "_la_", replacement = "_reg_") %>%
  str_replace(pattern = "_ccg_", replacement = "_nat_") %>%
  c("ch_nat_time")

outputs_reg_experiences_bench <- outputs_la_experiences_bench %>%
  str_replace(pattern = "_la_", replacement = "_reg_") %>%
  str_replace(pattern = "_ccg_", replacement = "_nat_") %>%
  str_replace(pattern = "_prov_", replacement = "_nat_")

outputs_reg_experiences_time_table <- outputs_reg_experiences_time %>%
  str_replace(pattern = "_time", replacement = "_time_table")
outputs_reg_experiences_bench_table <- outputs_reg_experiences_bench %>%
  str_replace(pattern = "_bench", replacement = "_bench_table")


# List of outputs for the Identification of need tab at LA level

outputs_la_identification_time <- c(
  "percent_pupils_ehcp_la_time",
  "ehcp_ageprofile_la_time",
  "mainstream_with_sen_la_time",
  "provider_types_la_time",
  "cin_la_time"
)

outputs_la_identification_bench <- c(
  "percent_pupils_ehcp_la_bench",
  "mainstream_with_sen_la_bench",
  "provider_types_la_bench",
  "cin_la_bench"
)

outputs_la_identification_time_table <- outputs_la_identification_time %>%
  str_replace(pattern = "_time", replacement = "_time_table")

outputs_la_identification_bench_table <- outputs_la_identification_bench %>%
  str_replace(pattern = "_bench", replacement = "_bench_table")

outputs_reg_identification_time <- outputs_la_identification_time %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_reg_identification_bench <- outputs_la_identification_bench %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_reg_identification_time_table <- outputs_la_identification_time_table %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_reg_identification_bench_table <- outputs_la_identification_bench_table %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_nat_summary <- c(
  "box_eyfsp",
  "box_ks1_phonics",
  "box_ks2_attainment",
  "box_ks4_attainment",
  "box_mentalhealth",
  "box_ofsted",
  "box_1618dest",
  "box_timeliness",
  "box_tribunals",
  "box_absence",
  "box_autism",
  "box_KS4dest",
  "box_statefunded",
  "box_mainstream",
  "box_special",
  "box_cin",
  "box_budget",
  "box_percap",
  "box_apcount_sf",
  "box_apcount_sa",
  "box_apcount_la",
  "box_uapcount_sa",
  "box_uapcount_la",
  "box_apchars",
  "box_apabsence",
  "box_apofsted"
)

outputs_la_ap_time <- c(
  "ap_counts_la_time",
  "ap_characteristics_la_time",
  "ap_absences_la_time",
  "ap_ofsted_la_time",
  "ap_uap_la_time"
)

outputs_la_ap_bench <- outputs_la_ap_time %>%
  str_replace(pattern = "_time", replacement = "_bench")

outputs_la_ap_time_table <- outputs_la_ap_time %>%
  str_replace(pattern = "_time", replacement = "_time_table")

outputs_la_ap_bench_table <- outputs_la_ap_bench %>%
  str_replace(pattern = "_bench", replacement = "_bench_table")

outputs_reg_ap_time <- outputs_la_ap_time %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_reg_ap_bench <- outputs_la_ap_bench %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_reg_ap_time_table <- outputs_la_ap_time_table %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

outputs_reg_ap_bench_table <- outputs_la_ap_bench_table %>%
  str_replace(pattern = "_la_", replacement = "_reg_")

app <- AppDriver$new(
  variant = platform_variant(), name = "Inclusion_Dashboard_v3",
  height = 1026, width = 1778, load_timeout = 10000000, expect_values_screenshot_args = FALSE, timeout = 1000000
) # disable screenshots in release version


test_that("Initial load test", {
  app$set_inputs(
    cookies = c("GA1.1.1768633568.1681211064", "granted", "GS1.1.1697116218.108.0.1697116314.0.0.0"),
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(time = 4)

  app$set_inputs(cookie_consent = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
  app$expect_values()
})

# 8/9. Check if Outcomes tables have changed (LA/bench) --------------------------------------------

app$set_inputs(
  panel_choice = "Local Areas",
  eyfsp_lab_toggle = "Table",
  phonics_lab_toggle = "Table",
  ks2_lab_toggle = "Table",
  ks4_lab_toggle = "Table",
  dest18_lab_toggle = "Table",
  mh_cgb_toggle = "Table",
  disco_lab_toggle = "Table"
)

Sys.sleep(time = 3)

test_that("Outcomes: benchmarking graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_bench_table
  )
})

app$set_inputs(myregion_switch = FALSE)

test_that("Outcomes: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_bench_table
  )
})

app$set_inputs(
  panel_choice = "England and Regions",
  region_choice = "London",
  eyfsp_reg_panel = RC,
  ks1_phonics_reg_panel = RC,
  ks2_attainment_reg_panel = RC,
  ks4_attainment_reg_panel = RC,
  destinations_1618_reg_panel = RC,
  mh_reg_panel = RC,
  disco_reg_panel = RC
)

Sys.sleep(4)

test_that("Outcomes, over time, charts, regions, comparison", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_bench
  )
})
