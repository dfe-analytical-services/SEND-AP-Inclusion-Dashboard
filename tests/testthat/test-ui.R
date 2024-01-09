library(tidyverse)

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


# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
app <- AppDriver$new(
  variant = platform_variant(), name = "Inclusion_Dashboard_v3",
  height = 1026, width = 1778, load_timeout = 10000000, expect_values_screenshot_args = FALSE, timeout = 100000
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

# 2. Check if Outcomes graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  la_choice = "Middlesbrough",
  tabsetpanels_la = "Outcomes"
)
Sys.sleep(time = 4)
app$set_inputs(
  ccg_choice = "NHS Tees Valley CCG" # have to do this one separately because otherwise it'll get wiped by the reactives
)
Sys.sleep(time = 5)
# app$wait_for_value(input = "navlistPanel", ignore = list("Homepage"))
# app$wait_for_value(input = "ccg_choice", ignore = list(NULL))

test_that("Outcomes: over time graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_time
  )
})

# 3. 16-18 destinations other mode
app$set_inputs(
  destinations_1618_la_time_filter_two = "All destination measures"
)

test_that("Outcomes: over time, 16-18 destinations all measures, LA level", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_la_time"
  )
})

# 4. Check if Outcomes tables have changed (LA/time) ----------------------------------------------
app$set_inputs(
  eyfsp_lat_toggle = "Table",
  phonics_lat_toggle = "Table",
  ks2_lat_toggle = "Table",
  ks4_lat_toggle = "Chart", # this is actually an error message but just to check it's there as it's not the default
  dest18_lat_toggle = "Table",
  mh_cgt_toggle = "Table",
  disco_lat_toggle = "Table"
)

Sys.sleep(time = 4)
# app$wait_for_value(input = "phonics_lat_toggle", ignore = list("Chart"))

test_that("Outcomes: over time tables, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_time_table
  )
})

# 5. 16-18 destinations other mode

app$set_inputs(
  destinations_1618_la_time_filter = "Identified LLDD (mainstream)",
  destinations_1618_la_time_filter_two = "Overall sustained destination (education, apprenticeship or employment)"
)
test_that("Outcomes: over time, 16-18 destinations all measures, LA level", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_la_time_table"
  )
})

# 6/7. Check if Outcomes graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Outcomes",
  la_choice = "Bedford",
  eyfsp_la_panel = LAC,
  ks2_attainment_la_panel = LAC,
  ks1_phonics_la_panel = LAC,
  ks4_attainment_la_panel = LAC,
  destinations_1618_la_panel = LAC,
  mentalhealth_ccg_panel = "Change over time (CCGs comparison)",
  disco_la_panel = LAC
)
Sys.sleep(time = 4)
app$set_inputs(
  ccg_choice = "NHS Bedfordshire, Luton and Milton Keynes CCG" # have to do this one separately because otherwise it'll get wiped by the reactives
)
Sys.sleep(time = 4)
test_that("Outcomes: benchmarking graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_bench
  )
})

app$set_inputs(
  myregion_switch = TRUE,
  destinations_1618_la_bench_filter_two = "All destination measures"
)
Sys.sleep(time = 4)
test_that("Outcomes: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_bench
  )
})

# 8/9. Check if Outcomes tables have changed (LA/bench) --------------------------------------------

app$set_inputs(
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

# 10. Check if 16-18 destination type graph has changed (LA/type) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Outcomes",
  la_choice = "Leicester",
  myregion_switch = FALSE,
  ks2_attainment_la_panel = LAC,
  ks1_phonics_la_panel = LAC,
  ks4_attainment_la_panel = LAC,
  destinations_1618_la_panel = "Provision type comparison"
)
Sys.sleep(time = 2)

test_that("Outcomes: 16-18 destinations by type, graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_la_type"
  )
})

# 11. Check if 16-18 destination type table has changed (LA/type) --------------------------------------------
app$set_inputs(dest18_typ_toggle = "Table")
Sys.sleep(time = 2)
test_that("Outcomes: 16-18 destinations by type, tables, LA level", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_la_type_table"
  )
})

# 12. Check if  Outcomes graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  tabsetpanels_reg = "Outcomes"
)
app$set_inputs(
  level_choice = "Regions",
  myregion_switch = FALSE
)
Sys.sleep(time = 3)
# app$wait_for_value(input = "level_choice", ignore = list(NULL, "England"))
app$set_inputs(region_choice = "East Midlands")
# app$wait_for_value(input = "region_choice", ignore = list(NULL))
# app$wait_for_value(input = "ks1_phonics_reg_time", ignore = list("Select region", "Select Region", NULL))

test_that("Outcomes: over time graphs, Region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_time
  )
})

# 13. Check if Outcomes tables have changed (region/time)
app$set_inputs(
  eyfsp_regt_toggle = "Table",
  phonics_regt_toggle = "Table",
  ks2_regt_toggle = "Table",
  ks4_regt_toggle = "Chart",
  dest18_regt_toggle = "Table",
  destinations_1618_reg_time_filter_two = "All destination measures", # we're alternating filter settings for this between charts and tables to check they both work without multiplying tests
  mh_regt_toggle = "Table",
  disco_regt_toggle = "Table"
)
Sys.sleep(time = 3)

test_that("Outcomes, over time, tables, regions, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_time_table
  )
})

# 14. Check if Outcomes graphs have changed (reg/bench) --------------------------------------------

app$set_inputs(
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

# 15. Check if Outcomes tables have changed (reg/bench) --------------------------------------------
app$set_inputs(
  eyfsp_regb_toggle = "Table",
  phonics_regb_toggle = "Table",
  ks2_regb_toggle = "Table",
  ks4_regb_toggle = "Table",
  dest18_regb_toggle = "Table",
  destinations_1618_reg_bench_filter_two = "Further education", # we're alternating filter settings for this between charts and tables to check they both work without multiplying tests
  mh_regb_toggle = "Table",
  disco_regb_toggle = "Table"
)

Sys.sleep(time = 10)

test_that("Outcomes, over time, tables, regions, comparison", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_bench_table
  )
})

# 16. Check if Outcomes graphs have changed (Eng/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  tabsetpanels_reg = "Outcomes"
)
Sys.sleep(time = 3)

test_that("Outcomes: benchmarking graphs, England level, charts, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_bench
  )
})

# 17. Check if Outcomes graphs have changed (Eng/time) --------------------------------------------

app$set_inputs(
  eyfsp_reg_panel = CT,
  ks1_phonics_reg_panel = CT,
  ks2_attainment_reg_panel = CT,
  ks4_attainment_reg_panel = CT,
  destinations_1618_reg_panel = CT,
  mh_reg_panel = CT,
  disco_reg_panel = CT
)
Sys.sleep(time = 4) # app$set_inputs waits for "a response" which is not very useful when you're updating four things, so wait a bit longer

test_that("Outcomes: benchmarking graphs, England level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_time
  )
})

# 18. Check if Outcomes tables have changed (Eng/time) --------------------------------------------
app$set_inputs(
  eyfsp_regt_toggle = "Table",
  phonics_regt_toggle = "Table",
  ks2_regt_toggle = "Table",
  ks4_regt_toggle = "Chart",
  dest18_regt_toggle = "Table",
  destinations_1618_reg_time_filter_two = "All destination measures", # we're alternating filter settings for this between charts and tables to check they both work without multiplying tests
  mh_regt_toggle = "Table",
  disco_regt_toggle = "Table"
)
Sys.sleep(time = 4)

test_that("Outcomes, over time, tables, regions, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_time_table
  )
})

# No need to check England bench tables because they're the same as the region bench tables

# 19. Check if 16-18 destination type graph has changed (region/type) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions",
  tabsetpanels_reg = "Outcomes"
)
Sys.sleep(time = 2)
app$set_inputs(
  destinations_1618_reg_panel = "SEN/LLDD provision type comparison"
)

Sys.sleep(time = 4)
test_that("Outcomes: 16-18 destinations by type, region level, graph", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_reg_type"
  )
})

# 20. Check if 16-18 destination type table has changed (region/type)  ------------------------------------------
app$set_inputs(
  dest18_regtyp_toggle = "Table"
)
Sys.sleep(time = 2)
test_that("Outcomes: 16-18 destinations by type, region level, table", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_reg_type_table"
  )
})

### EXPERIENCES ###

# 21. Check if Experiences graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Experiences",
  la_choice = "Stockton-on-Tees"
)

Sys.sleep(time = 2.5)
app$set_inputs(
  ccg_choice = "NHS Tees Valley CCG"
)
Sys.sleep(time = 2.5)

test_that("Experiences: over time graphs, LA/CCG level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_time
  )
})

# 22. Check if Experiences tables have changed (LA/time) --------------------------------------------
app$set_inputs(
  time_lat_toggle = "Table",
  trib_lat_toggle = "Table",
  aut_cgt_toggle = "Table",
  abs_lat_toggle = "Table",
  destks4_lat_toggle = "Table",
  ks4_destinations_la_time_filter_two = "All destination measures"
)

Sys.sleep(time = 5)
test_that("Experiences, over time, LA/CCG level, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_time_table
  )
})

# 23/24. Check if Experiences graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  myregion_switch = FALSE,
  tabsetpanels_la = "Experiences",
  la_choice = "Birmingham",
  timeliness_la_panel = LAC,
  tribunals_la_panel = LAC,
  autism_ccg_panel = "Sub-ICB area comparison",
  ch_la_panel = "Provider-level comparison",
  ks4_destinations_la_panel = LAC,
  absence_la_panel = LAC
)
Sys.sleep(time = 4)
app$set_inputs(ccg_choice = "NHS Birmingham and Solihull CCG")
Sys.sleep(time = 5)
test_that("Experiences: benchmarking graphs, LA/CCG level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench
  )
})

app$set_inputs(myregion_switch = TRUE)
Sys.sleep(time = 5)
test_that("Experiences: benchmarking graphs, LA/CCG level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench
  )
})

# 25/26. Check if Experiences tables have changed (LA/bench) -----------------------------------------------
app$set_inputs(
  time_lab_toggle = "Table",
  trib_lab_toggle = "Table",
  aut_cgb_toggle = "Table",
  abs_lab_toggle = "Table",
  destks4_lab_toggle = "Table",
  ch_prob_toggle = "Table"
)

Sys.sleep(time = 6)

test_that("Experiences, LA level, comparison, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench_table
  )
})

app$set_inputs(myregion_switch = FALSE)
Sys.sleep(time = 4)
test_that("Experiences, LA level, comparison, tables, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench_table
  )
})

# 27. Check if KS4 destination type graph has changed (LA/type) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Experiences",
  la_choice = "Camden",
  timeliness_la_panel = LAC,
  autism_ccg_panel = "CCG comparison",
  tribunals_la_panel = LAC,
  ks4_destinations_la_type_filter = "Employment",
  ks4_destinations_la_panel = "SEN provision type comparison"
)

Sys.sleep(time = 3)

app$set_inputs(
  ccg_choice = "NHS North Central London CCG",
)

Sys.sleep(time = 4)

test_that("Experiences: KS4 destinations by type, LA level", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_la_type"
  )
})

# 28. Check if KS4 destination type table has changed (LA/type) ---------------------------------------------

app$set_inputs(
  destks4_typ_toggle = "Table",
  ks4_destinations_la_time_filter_two = "Not sustained"
)

Sys.sleep(time = 4)

test_that("Experiences: KS4 destinations by type, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_la_type_table"
  )
})

# 29. Check if Experiences graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  tabsetpanels_reg = "Experiences",
  level_choice = "Regions",
  region_choice = "West Midlands"
)
Sys.sleep(time = 4)

test_that("Experiences: over time graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_time
  )
})

# 30. Check if Experiences tables have changed (region/time) ---------------------------------------------
app$set_inputs(
  time_regt_toggle = "Table",
  trib_regt_toggle = "Table",
  aut_nat_toggle = "Table",
  ch_nat_toggle = "Table",
  abs_regt_toggle = "Table",
  destks4_regt_toggle = "Table"
)
Sys.sleep(time = 4)
test_that("Experiences, over time tables, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_time_table
  )
})

# 31. Check if Experiences graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  tabsetpanels_reg = "Experiences",
  timeliness_reg_panel = RC,
  autism_reg_panel = "Provider-level comparison",
  ch_reg_panel = RC,
  tribunals_reg_panel = RC,
  ks4_destinations_reg_panel = RC,
  absence_reg_panel = RC
)
Sys.sleep(time = 4)
test_that("Experiences: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_bench
  )
})

# 32. Check if Experiences tables have changed (region/bench) -----------------------------------------------
app$set_inputs(
  time_regb_toggle = "Table",
  trib_regb_toggle = "Table",
  aut_nab_toggle = "Table",
  ch_nab_toggle = "Table",
  abs_regb_toggle = "Table",
  destks4_regb_toggle = "Table"
)
Sys.sleep(time = 5)
test_that("Experiences, region level, comparison, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_bench_table
  )
})

# 33. Check if KS4 destination type graph has changed (region/type) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  region_choice = "London",
  tabsetpanels_reg = "Experiences"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  timeliness_reg_panel = RC,
  autism_ccg_panel = "CCG comparison",
  tribunals_reg_panel = RC,
  ks4_destinations_reg_type_filter = "Employment",
  ks4_destinations_reg_panel = "SEN provision type comparison"
)

Sys.sleep(time = 2)

test_that("Experiences: KS4 destinations by type, region level", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_reg_type"
  )
})

# 34. Check if KS4 destination type table has changed (region/type) --------------------------------------------
app$set_inputs(
  destks4_regtyp_toggle = "Table"
)

Sys.sleep(time = 1)
test_that("Experiences: KS4 destinations by type, region level, tables", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_reg_type_table"
  )
})

# 35. Check England level (time/graphs)
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  tabsetpanels_reg = "Experiences",
  timeliness_reg_panel = CT,
  autism_reg_panel = "Change over time (England)",
  ch_reg_panel = CT,
  tribunals_reg_panel = CT,
  ks4_destinations_reg_panel = CT,
  absence_reg_panel = CT
)
Sys.sleep(time = 3)

test_that("Experiences: over time graphs, England level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_time
  )
})

# 36. Check if Experiences tables have changed (England/time) ---------------------------------------------
app$set_inputs(
  time_regt_toggle = "Table",
  trib_regt_toggle = "Table",
  aut_nat_toggle = "Table",
  ch_nat_toggle = "Table",
  abs_regt_toggle = "Table",
  destks4_regt_toggle = "Table"
)
Sys.sleep(time = 4)
test_that("Experiences, over time tables, England level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_time_table
  )
})

# 37. Check England level (bench/graphs)
app$set_inputs(
  navlistPanel = "England and Regions",
  tabsetpanels_reg = "Experiences",
  timeliness_reg_panel = RC,
  autism_reg_panel = "Provider-level comparison",
  ch_reg_panel = RC,
  tribunals_reg_panel = RC,
  ks4_destinations_reg_panel = RC,
  absence_reg_panel = RC
)
Sys.sleep(time = 4)
app$set_inputs(
  time_regt_toggle = "Chart",
  trib_regt_toggle = "Chart",
  aut_nat_toggle = "Chart",
  ch_nat_toggle = "Chart",
  abs_regt_toggle = "Chart",
  destks4_regt_toggle = "Chart"
)
test_that("Experiences: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_bench
  )
})
# No need to check bench tables as identical to reg level

### Financial sustainability ###

# 38. Check if Financial sustainability graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Financial sustainability",
  la_choice = "Hackney"
)
Sys.sleep(time = 4)

test_that("Financial sustainability: over time graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_time",
      "specialist_spend_la_time"
    )
  )
})

# 39. Check if Financial sustainability tables have changed (LA/time) --------------------------------------------
app$set_inputs(
  dsg_lat_toggle = "Table",
  spend_lat_toggle = "Table"
)

Sys.sleep(time = 4)

test_that("Financial sustainability: over time graphs, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_time_table",
      "specialist_spend_la_time_table"
    )
  )
})

# 40/41. Check if Financial sustainability graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Financial sustainability",
  la_choice = "Derby",
  dsg_deficit_la_panel = LAC,
  specialist_spend_la_panel = LAC,
  myregion_switch = FALSE
)
Sys.sleep(time = 4)

test_that("Financial sustainability: benchmarking graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench",
      "specialist_spend_la_bench"
    )
  )
})

app$set_inputs(myregion_switch = TRUE)
test_that("Financial sustainability: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench",
      "specialist_spend_la_bench"
    )
  )
})

# 42/43. Check if Financial sustainability tables have changed (LA/bench) --------------------------------------------
app$set_inputs(
  dsg_lab_toggle = "Table",
  spend_lab_toggle = "Table"
)

Sys.sleep(time = 4)
test_that("Financial sustainability: benchmarking, LA level, region switch, tables", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench_table",
      "specialist_spend_la_bench_table"
    )
  )
})

app$set_inputs(myregion_switch = FALSE)
Sys.sleep(time = 4)

test_that("Financial sustainability: benchmarking, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench_table",
      "specialist_spend_la_bench_table"
    )
  )
})

# 44. Check if Financial sustainability graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 2)
app$set_inputs(
  region_choice = "South West",
  tabsetpanels_reg = "Financial sustainability"
)
Sys.sleep(time = 5)
app$wait_for_idle(1500)

test_that("Financial sustainability: over time graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_time",
      "specialist_spend_reg_time"
    )
  )
})

# 45. Check if Financial sustainability tables have changed (region/time) --------------------------------------------
app$set_inputs(
  dsg_regt_toggle = "Table",
  spend_regt_toggle = "Table"
)

Sys.sleep(time = 4)

test_that("Financial sustainability: over time tables, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_time_table",
      "specialist_spend_reg_time_table"
    )
  )
})

# 46. Check if Financial sustainability graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  region_choice = "South East",
  myregion_switch = FALSE
)
Sys.sleep(time = 0.5)
app$set_inputs(
  tabsetpanels_reg = "Financial sustainability",
  dsg_deficit_reg_panel = RC,
  specialist_spend_reg_panel = RC
)
Sys.sleep(time = 4)

test_that("Financial sustainability: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_bench",
      "specialist_spend_reg_bench"
    )
  )
})

# 47. Check if Financial sustainability tables have changed (region/bench) --------------------------------------------
app$set_inputs(
  dsg_regb_toggle = "Table",
  spend_regb_toggle = "Table"
)
Sys.sleep(time = 0.5)

test_that("Financial sustainability: benchmarking tables, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_bench_table",
      "specialist_spend_reg_bench_table"
    )
  )
})

# 48. Check if Financial sustainability graphs have changed (England/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  tabsetpanels_reg = "Financial sustainability",
  dsg_deficit_reg_panel = CT,
  specialist_spend_reg_panel = CT
)
Sys.sleep(time = 0.5)
test_that("Financial sustainability: over time graphs, England level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_time",
      "specialist_spend_reg_time"
    )
  )
})

# 49. Check if Financial sustainability tables have changed (England/time) --------------------------------------------
app$set_inputs(
  dsg_regt_toggle = "Table",
  spend_regt_toggle = "Table"
)

Sys.sleep(time = 0.5)

test_that("Financial sustainability: over time tables, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_time_table",
      "specialist_spend_reg_time_table"
    )
  )
})

# 50. Check if Financial sustainability tables have changed (England/bench) --------------------------------------------
app$set_inputs(
  tabsetpanels_reg = "Financial sustainability",
  dsg_deficit_reg_panel = RC,
  specialist_spend_reg_panel = RC
)
Sys.sleep(time = 0.5)

test_that("Financial sustainability: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_bench",
      "specialist_spend_reg_bench"
    )
  )
})


### Identification of need ###

# 51. Check if Identification of need graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Identification of need",
  la_choice = "Norfolk"
)
Sys.sleep(time = 0.5)

test_that("Identification of need: over time graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_time
  )
})

# 52. Check if Identification of need tables have changed (LA/time) --------------------------------------------
app$set_inputs(
  ehcppc_lat_toggle = "Table",
  msen_lat_toggle = "Table",
  types_lat_toggle = "Table",
  age_lat_toggle = "Table",
  cin_lat_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Identification of need: over time tables, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_time_table
  )
})

# 53/54. Check if Identification of need graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Identification of need",
  la_choice = "Derby",
  percent_pupils_ehcp_la_panel = LAC,
  mainstream_with_sen_la_panel = LAC,
  provider_types_la_panel = LAC,
  cin_la_panel = LAC,
  myregion_switch = FALSE
)

Sys.sleep(time = 5)
test_that("Identification of need: benchmarking graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench
  )
})

app$set_inputs(myregion_switch = TRUE)
Sys.sleep(time = 1.5)
test_that("Identification of need: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench
  )
})

# 55/56. Check if Identification of need tables have changed (LA/bench) --------------------------------------------
app$set_inputs(
  ehcppc_lab_toggle = "Table",
  msen_lab_toggle = "Table",
  types_lab_toggle = "Table",
  cin_lab_toggle = "Table"
)

Sys.sleep(time = 4)

test_that("Identification of need, comparison, LA level, region switch, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench_table
  )
})
app$set_inputs(myregion_switch = FALSE)
Sys.sleep(time = 4)

test_that("Identification of need, comparison, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench_table
  )
})

# 57. Check if Identification of need graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  region_choice = "North West",
  tabsetpanels_reg = "Identification of need"
)
Sys.sleep(time = 4)

test_that("Identification of need: over time graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_time
  )
})

# 58. Check if Identification of need tables have changed (region/time) --------------------------------------------
app$set_inputs(
  ehcppc_regt_toggle = "Table",
  msen_regt_toggle = "Table",
  types_regt_toggle = "Table",
  age_regt_toggle = "Table",
  cin_regt_toggle = "Table"
)

Sys.sleep(time = 2)
test_that("Identification of need: over time, tables, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_time_table
  )
})

# 59. Check if Identification of need graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 2)
app$set_inputs(
  region_choice = "Yorkshire and the Humber",
  tabsetpanels_reg = "Identification of need",
  percent_pupils_ehcp_reg_panel = RC,
  mainstream_with_sen_reg_panel = RC,
  provider_types_reg_panel = RC,
  cin_reg_panel = RC,
  myregion_switch = FALSE
)
Sys.sleep(time = 2)
app$set_inputs(
  ehcppc_regt_toggle = "Chart",
  msen_regt_toggle = "Chart",
  types_regt_toggle = "Chart",
  age_regt_toggle = "Chart",
  cin_regt_toggle = "Chart"
)
Sys.sleep(time = 2)
test_that("Identification of need: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_bench
  )
})

# 60. Check if Identification of need tables have changed (region/bench) --------------------------------------------

app$set_inputs(
  ehcppc_regb_toggle = "Table",
  msen_regb_toggle = "Table",
  types_regb_toggle = "Table",
  cin_regb_toggle = "Table",
  age_regt_toggle = "Table"
)

Sys.sleep(time = 2.5)

Sys.sleep(time = 3)
test_that("Identification of need, regional comparison, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_bench_table
  )
})

# 61. Check if Identification of need graphs have changed (England/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England"
)
Sys.sleep(time = 3)
app$set_inputs(
  tabsetpanels_reg = "Identification of need"
)
Sys.sleep(time = 2.5)

test_that("Identification of need: over time graphs, England level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_time
  )
})

# 62. Check if Identification of need tables have changed (England/time) --------------------------------------------
app$set_inputs(
  ehcppc_regt_toggle = "Table",
  msen_regt_toggle = "Table",
  types_regt_toggle = "Table",
  age_regt_toggle = "Table",
  cin_regt_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Identification of need: over time, tables, England level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_time_table
  )
})

# 63. Check if Identification of need graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  tabsetpanels_reg = "Identification of need",
  percent_pupils_ehcp_reg_panel = RC,
  mainstream_with_sen_reg_panel = RC,
  provider_types_reg_panel = RC,
  cin_reg_panel = RC,
  myregion_switch = FALSE
)
Sys.sleep(time = 0.5)
test_that("Identification of need: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_bench
  )
})

### SUMMARIES ----------------------------------------------------------------------------------------

# 64. Check LA Summary panel ------------------------------------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Summary",
  la_choice = "Middlesbrough"
)
Sys.sleep(time = 4)

test_that("Summary, LA level", {
  app$expect_values(
    input = listInputs,
    output = "summary"
  )
})

# 65. Check LA Summary tables --------------------------------------------------------------------------
app$set_inputs(
  la_sum_toggle = "Table"
)
Sys.sleep(time = 4)

test_that("Summary: LA level, table", {
  app$expect_values(
    input = listInputs,
    output = c("la_summary_table", "ap_summary_table")
  )
})

# 66. Check national summary panel ------------------------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  tabsetpanels_reg = "Summary"
)
Sys.sleep(time = 0.5)

test_that("Summary: England level", {
  app$expect_values(
    input = listInputs,
    output = outputs_nat_summary
  )
})

# 67. Check regional summary panel ------------------------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions",
  region_choice = "North East",
  tabsetpanels_reg = "Summary"
)
Sys.sleep(time = 0.5)

test_that("Summary: Regions level", {
  app$expect_values(
    input = listInputs,
    output = outputs_nat_summary
  )
})

### ALTERNATIVE PROVISION -----------------------------------------------------------------------

# 68. Check if AP graphs have changed (LA/time) -------------------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  la_choice = "Lancashire",
  tabsetpanels_la = "Alternative provision"
)
app$wait_for_idle(1500)

Sys.sleep(time = 6)

test_that("AP: LA level, graphs, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_ap_time_table
  )
})

# 69. Check if AP tables have changed (LA/time) -------------------------------------------------------

app$set_inputs(
  ap_uap_lat_toggle = "Table",
  ap_ofsted_lat_toggle = "Table",
  ap_characteristics_lat_toggle = "Table",
  ap_absences_lat_toggle = "Table",
  ap_counts_lat_toggle = "Table"
)

Sys.sleep(time = 4)

test_that("AP: LA level, tables, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_ap_time_table
  )
})

# 70/71. Check if AP graphs have changed (LA/bench) -------------------------------------------------------

app$set_inputs(
  ap_counts_la_panel = LAC,
  ap_characteristics_la_panel = LAC,
  ap_ofsted_la_panel = LAC,
  ap_absences_la_panel = LAC,
  ap_counts_la_panel = LAC,
  ap_ofsted_la_bench_filter_two = "Overall effectiveness (% of schools)",
  myregion_switch = FALSE,
)
app$wait_for_idle()
Sys.sleep(time = 4)
test_that("AP: LA level, graphs, benchmarking", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_ap_bench
  )
})

app$set_inputs(
  myregion_switch = TRUE
)
app$wait_for_idle()
Sys.sleep(time = 4)
test_that("AP: LA level, graphs, benchmarking", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_ap_bench
  )
})

# 72/73. Check if AP tables have changed (LA/bench) -------------------------------------------------------

app$set_inputs(
  ap_uap_lab_toggle = "Table",
  ap_ofsted_lab_toggle = "Table",
  ap_characteristics_lab_toggle = "Table",
  ap_absences_lab_toggle = "Table",
  ap_counts_lab_toggle = "Table",
  myregion_switch = FALSE,
)
app$wait_for_idle()
Sys.sleep(time = 4)
test_that("AP: LA level, tables, benchmarking", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_ap_bench_table
  )
})

app$set_inputs(
  myregion_switch = TRUE
)
app$wait_for_idle()
Sys.sleep(time = 4)
test_that("AP: LA level, tables, benchmarking", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_ap_bench_table
  )
})

# 74. Check if AP graphs have changed (Region/time) -------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions",
  region_choice = "East of England",
  tabsetpanels_reg = "Alternative provision"
)
app$wait_for_idle()
Sys.sleep(time = 4)

test_that("AP: Region level, graphs, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_time
  )
})

# 75. Check if AP tables have changed (Region/time) -------------------------------------------------------
app$set_inputs(
  ap_uap_regt_toggle = "Table",
  ap_ofsted_regt_toggle = "Table",
  ap_characteristics_regt_toggle = "Table",
  ap_absences_regt_toggle = "Table",
  ap_counts_regt_toggle = "Table"
)
app$wait_for_idle()
Sys.sleep(time = 4)

test_that("AP: Region level, tables, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_time_table
  )
})

# 76. Check if AP graphs have changed (Region/bench) -------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions",
  region_choice = "North East",
  tabsetpanels_reg = "Alternative provision",
  ap_counts_reg_panel = RC,
  ap_characteristics_reg_panel = RC,
  ap_ofsted_reg_panel = RC,
  ap_absences_reg_panel = RC,
  ap_counts_reg_panel = RC,
  ap_ofsted_reg_bench_filter_two = "Overall effectiveness (% of schools)",
)

Sys.sleep(time = 4)

test_that("AP: Region level, graphs, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_bench
  )
})
# 77. Check if AP tables have changed (Region/bench) -------------------------------------------------------
app$set_inputs(
  ap_uap_regb_toggle = "Table",
  ap_ofsted_regb_toggle = "Table",
  ap_characteristics_regb_toggle = "Table",
  ap_absences_regb_toggle = "Table",
  ap_counts_regb_toggle = "Table"
)

Sys.sleep(time = 4)

test_that("AP: Region level, tables, benchmarking", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_bench_table
  )
})


# 78. Check if AP graphs have changed (England/time) -------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  tabsetpanels_reg = "Alternative provision"
)

Sys.sleep(time = 4)

test_that("AP: England level, graphs, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_time
  )
})
# 79. Check if AP tables have changed (England/time) -------------------------------------------------------
app$set_inputs(
  ap_uap_regt_toggle = "Table",
  ap_ofsted_regt_toggle = "Table",
  ap_characteristics_regt_toggle = "Table",
  ap_absences_regt_toggle = "Table",
  ap_counts_regt_toggle = "Table"
)

Sys.sleep(time = 4)

test_that("AP: England level, tables, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_time_table
  )
})

# 80. Check if AP graphs have changed (England/bench) -------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  tabsetpanels_reg = "Alternative provision",
  ap_counts_reg_panel = RC,
  ap_characteristics_reg_panel = RC,
  ap_ofsted_reg_panel = RC,
  ap_absences_reg_panel = RC,
  ap_counts_reg_panel = RC,
  ap_ofsted_reg_bench_filter_two = "Overall effectiveness (% of schools)",
)
app$wait_for_idle()
Sys.sleep(time = 8)

test_that("AP: England level, graphs, benchmarking", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_ap_bench
  )
})
