library(tidyverse)

# Shorthand for frequently used tab names
LAC <- "Local area comparison"
RC <- "Regional comparison"


listInputs <- c(
  "absence_la_filter",
  "absence_reg_filter",
  "autism_nat_bench_filter",
  "ccg_choice",
  "destinations_1618_la_bench_filter",
  "destinations_1618_la_time_filter",
  "destinations_1618_la_type_filter",
  "destinations_1618_reg_bench_filter",
  "destinations_1618_reg_time_filter",
  "destinations_1618_reg_type_filter",
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
  "nhs_value_box_ccg_older"
)

outputs_la_outcomes_time_table <- c(
  "destinations_1618_la_time_table",
  "ks1_phonics_la_time_table",
  "ks2_attainment_la_time_table",
  "ks4_attainment_la_time_table",
  "mentalhealth_ccg_time_table"
)

outputs_la_outcomes_bench <- c(
  "destinations_1618_la_bench",
  "ks1_phonics_la_bench",
  "ks2_attainment_la_bench",
  "ks4_attainment_la_bench",
  "mentalhealth_ccg_bench"
)

outputs_la_outcomes_bench_table <- c(
  "destinations_1618_la_bench_table",
  "ks1_phonics_la_bench_table",
  "ks2_attainment_la_bench_table",
  "ks4_attainment_la_bench_table",
  "mentalhealth_ccg_bench_table"
)
# Lists of outputs for the Outcomes tab at region/England level
outputs_reg_outcomes_time <- c(
  "destinations_1618_reg_time",
  "ks1_phonics_reg_time",
  "ks2_attainment_reg_time",
  "ks4_attainment_reg_time",
  "reg_ofsted_rating"
)

outputs_reg_outcomes_time_table <- c(
  "destinations_1618_reg_time_table",
  "ks1_phonics_reg_time_table",
  "ks2_attainment_reg_time_table",
  "ks4_attainment_reg_time_table"
)

outputs_reg_outcomes_bench <- c(
  "destinations_1618_reg_bench",
  "ks1_phonics_reg_bench",
  "ks2_attainment_reg_bench",
  "ks4_attainment_reg_bench",
  "mentalhealth_reg_bench"
)

outputs_reg_outcomes_bench_table <- c(
  "destinations_1618_reg_bench_table",
  "ks1_phonics_reg_bench_table",
  "ks2_attainment_reg_bench_table",
  "ks4_attainment_reg_bench_table",
  "mentalhealth_reg_bench_table"
)

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
  "autism_ccg_bench",
  "tribunals_la_bench",
  "absence_la_bench",
  "ks4_destinations_la_bench"
)

outputs_la_experiences_bench_table <- c(
  "timeliness_la_bench_table",
  "autism_ccg_bench_table",
  "tribunals_la_bench_table",
  "absence_la_bench_table",
  "ks4_destinations_la_bench_table"
)

outputs_reg_experiences_time <- outputs_la_experiences_time %>%
  str_replace(pattern = "_la_", replacement = "_reg_") %>%
  str_replace(pattern = "_ccg_", replacement = "_nat_")

outputs_reg_experiences_bench <- outputs_la_experiences_bench %>%
  str_replace(pattern = "_la_", replacement = "_reg_") %>%
  str_replace(pattern = "_ccg_", replacement = "_nat_")

outputs_reg_experiences_time_table <- outputs_reg_experiences_time %>%
  str_replace(pattern = "_time", replacement = "_time_table")
outputs_reg_experiences_bench_table <- outputs_reg_experiences_bench %>%
  str_replace(pattern = "_bench", replacement = "_bench_table")


# List of outputs for the Identification of Need tab at LA level

outputs_la_identification_time <- c(
  "percent_pupils_ehcp_la_time",
  "ehcp_ageprofile_la_time",
  "mainstream_with_sen_la_time",
  "provider_types_la_time"
)

outputs_la_identification_time_table <- outputs_la_identification_time %>%
  str_replace(pattern = "_time", replacement = "_time_table")


outputs_la_identification_bench <- c(
  "percent_pupils_ehcp_la_bench",
  "mainstream_with_sen_la_bench",
  "provider_types_la_bench"
)

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
  "box_budget"
)

# 1. Does it load  -------------------------------------------------------------------------------------------------------------------
app <- AppDriver$new(
  variant = platform_variant(), name = "Inclusion_Dashboard_v3",
  height = 1026, width = 1778, load_timeout = 10000000, expect_values_screenshot_args = FALSE
) # disable screenshots in release version


test_that("Initial load test", {
  app$set_inputs(
    cookies = c("GA1.1.1768633568.1681211064", "granted", "GS1.1.1697116218.108.0.1697116314.0.0.0"),
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(time = 2)

  app$set_inputs(cookie_consent = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
  app$expect_values()
})

# 2. Check if Outcomes graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  la_choice = "Middlesbrough",
  tabsetpanels_la = "Outcomes"
)
Sys.sleep(time = 1)
app$set_inputs(
  ccg_choice = "NHS Tees Valley CCG" # have to do this one separately because otherwise it'll get wiped by the reactives
)
Sys.sleep(time = 4)
# app$wait_for_value(input = "navlistPanel", ignore = list("Homepage"))
# app$wait_for_value(input = "ccg_choice", ignore = list(NULL))

test_that("Outcomes: over time graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_time
  )
})

# 3. Check if Outcomes tables have changed (LA/time) ----------------------------------------------
app$set_inputs(
  phonics_lat_toggle = "Table",
  ks2_lat_toggle = "Table",
  ks4_lat_toggle = "Table",
  dest18_lat_toggle = "Table",
  mh_cgt_toggle = "Table"
)
Sys.sleep(time = 2)
# app$wait_for_value(input = "phonics_lat_toggle", ignore = list("Chart"))

test_that("Outcomes: over time tables, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_time_table
  )
})

# 4/5. Check if Outcomes graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Outcomes",
  la_choice = "Bedford",
  ks2_attainment_la_panel = LAC,
  ks1_phonics_la_panel = LAC,
  ks4_attainment_la_panel = LAC,
  destinations_1618_la_panel = LAC,
  mentalhealth_ccg_panel = "Change over time (CCGs comparison)"
)

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

app$set_inputs(myregion_switch = TRUE)
Sys.sleep(time = 4)
test_that("Outcomes: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_outcomes_bench
  )
})

# 6/7. Check if Outcomes tables have changed (LA/bench) --------------------------------------------

app$set_inputs(
  phonics_lab_toggle = "Table",
  ks2_lab_toggle = "Table",
  ks4_lab_toggle = "Table",
  dest18_lab_toggle = "Table",
  mh_cgb_toggle = "Table"
)
Sys.sleep(time = 0.5)
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

# 8. Check if 16-18 destination type graph has changed (LA/type) --------------------------------------------
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
Sys.sleep(time = 0.5)

test_that("Outcomes: 16-18 destinations by type, graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_la_type"
  )
})

# 9. Check if 16-18 destination type table has changed (LA/type) --------------------------------------------
app$set_inputs(dest18_typ_toggle = "Table")
test_that("Outcomes: 16-18 destinations by type, tables, LA level", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_la_type_table"
  )
})

# 10. Check if  Outcomes graphs have changed (region/time) --------------------------------------------
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

# 11. Check if Outcomes tables have changed (region/time)
app$set_inputs(
  phonics_regt_toggle = "Table",
  ks2_regt_toggle = "Table",
  ks4_regt_toggle = "Table",
  dest18_regt_toggle = "Table",
  mh_regt_toggle = "Table"
)
Sys.sleep(time = 2)

test_that("Outcomes, over time, tables, regions, time", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_time_table
  )
})

# 12. Check if Outcomes tables have changed (Eng/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  tabsetpanels_reg = "Outcomes"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  ks2_attainment_reg_panel = RC,
  ks1_phonics_reg_panel = RC,
  ks4_attainment_reg_panel = RC,
  destinations_1618_reg_panel = RC
)
Sys.sleep(time = 2) # app$set_inputs waits for "a response" which is not very useful when you're updating four things, so wait a bit longer

test_that("Outcomes: benchmarking graphs, England level, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_bench_table
  )
})

# 13. Check if Outcomes graphs have changed (reg/bench) --------------------------------------------
app$set_inputs(
  phonics_regb_toggle = "Table",
  ks2_regb_toggle = "Table",
  ks4_regb_toggle = "Table",
  dest18_regb_toggle = "Table"
)
Sys.sleep(time = 2)

test_that("Outcomes, region level, comparison, charts", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_outcomes_bench
  )
})

# 14. Check if 16-18 destination type graph has changed (region/type) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions",
  tabsetpanels_reg = "Outcomes"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  ks2_attainment_reg_panel = RC,
  ks1_phonics_reg_panel = RC,
  ks4_attainment_reg_panel = RC,
  destinations_1618_reg_panel = "SEN/LLDD provision type comparison"
)

Sys.sleep(time = 0.5)
test_that("Outcomes: 16-18 destinations by type, LA level, graph", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_reg_type"
  )
})

# 15. Check if 16-18 destination type table has changed (region/type)  ------------------------------------------
app$set_inputs(
  dest18_regtyp_toggle = "Table"
)
Sys.sleep(time = 0.5)
test_that("Outcomes: 16-18 destinations by type, region level, table", {
  app$expect_values(
    input = listInputs,
    output = "destinations_1618_reg_type_table"
  )
})

### EXPERIENCES ###

# 16. Check if Experiences graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Experiences",
  la_choice = "Stockton-on-Tees"
)

Sys.sleep(time = 2)
app$set_inputs(
  ccg_choice = "NHS Tees Valley CCG"
)
Sys.sleep(time = 2)

test_that("Experiences: over time graphs, LA/CCG level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_time
  )
})

# 17. Check if Experiences tables have changed (LA/time) --------------------------------------------
app$set_inputs(
  time_lat_toggle = "Table",
  trib_lat_toggle = "Table",
  aut_cgt_toggle = "Table",
  abs_lat_toggle = "Table",
  destks4_lat_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Experiences, over time, LA/CCG level, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_time_table
  )
})

# 18/19. Check if Experiences graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  myregion_switch = FALSE,
  tabsetpanels_la = "Experiences",
  la_choice = "Birmingham",
  timeliness_la_panel = LAC,
  autism_ccg_panel = "CCG comparison",
  tribunals_la_panel = LAC,
  ks4_destinations_la_panel = LAC,
  absence_la_panel = LAC
)
Sys.sleep(time = 3)
app$set_inputs(ccg_choice = "NHS Birmingham and Solihull CCG")
Sys.sleep(time = 3)
test_that("Experiences: benchmarking graphs, LA/CCG level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench
  )
})

app$set_inputs(myregion_switch = TRUE)

test_that("Experiences: benchmarking graphs, LA/CCG level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench
  )
})

# 20/21. Check if Experiences tables have changed (LA/bench) -----------------------------------------------
app$set_inputs(
  time_lab_toggle = "Table",
  trib_lab_toggle = "Table",
  aut_cgb_toggle = "Table",
  abs_lab_toggle = "Table",
  destks4_lab_toggle = "Table"
)

Sys.sleep(time = 0.5)

test_that("Experiences, LA level, comparison, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench_table
  )
})

app$set_inputs(myregion_switch = FALSE)
test_that("Experiences, LA level, comparison, tables, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_experiences_bench_table
  )
})

# 22. Check if KS4 destination type graph has changed (LA/type) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Experiences",
  la_choice = "Camden",
  ccg_choice = "NHS North Central London CCG",
  timeliness_la_panel = LAC,
  autism_ccg_panel = "CCG comparison",
  tribunals_la_panel = LAC,
  ks4_destinations_la_type_filter = "Employment",
  ks4_destinations_la_panel = "SEN provision type comparison"
)

Sys.sleep(time = 0.5)

test_that("Experiences: KS4 destinations by type, LA level", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_la_type"
  )
})

# 23. Check if KS4 destination type table has changed (LA/type) ---------------------------------------------

app$set_inputs(destks4_typ_toggle = "Table")
test_that("Experiences: KS4 destinations by type, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_la_type_table"
  )
})

# 24. Check if Experiences graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  tabsetpanels_reg = "Experiences",
  level_choice = "Regions",
  region_choice = "East Midlands"
)
Sys.sleep(time = 2)

test_that("Experiences: over time graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_time
  )
})

# 25. Check if Experiences tables have changed (region/time) ---------------------------------------------
app$set_inputs(
  time_regt_toggle = "Table",
  trib_regt_toggle = "Table",
  aut_nat_toggle = "Table",
  abs_regt_toggle = "Table",
  destks4_regt_toggle = "Table"
)
Sys.sleep(time = 0.5)
test_that("Experiences, over time tables, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_time_table
  )
})

# 26. Check if Experiences graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  tabsetpanels_reg = "Experiences",
  timeliness_reg_panel = RC,
  autism_reg_panel = "Provider-level comparison",
  tribunals_reg_panel = RC,
  ks4_destinations_reg_panel = RC,
  absence_reg_panel = RC
)
Sys.sleep(time = 2)
test_that("Experiences: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_bench
  )
})

# 27. Check if Experiences tables have changed (region/bench) -----------------------------------------------
app$set_inputs(
  time_regb_toggle = "Table",
  trib_regb_toggle = "Table",
  aut_nab_toggle = "Table",
  abs_regb_toggle = "Table",
  destks4_regb_toggle = "Table"
)
Sys.sleep(time = 0.5)
test_that("Experiences, region level, comparison, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_experiences_bench_table
  )
})

# 28. Check if KS4 destination type graph has changed (region/type) --------------------------------------------
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

Sys.sleep(time = 0.5)

test_that("Experiences: KS4 destinations by type, region level", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_reg_type"
  )
})

# 29. Check if KS4 destination type table has changed (region/type) --------------------------------------------
app$set_inputs(
  destks4_regtyp_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Experiences: KS4 destinations by type, region level, tables", {
  app$expect_values(
    input = listInputs,
    output = "ks4_destinations_reg_type_table"
  )
})

### Financial Sustainability ###

# 30. Check if Financial Sustainability graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Financial Sustainability",
  la_choice = "Hackney"
)
Sys.sleep(time = 0.5)

test_that("Financial Sustainability: over time graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_time",
      "specialist_spend_la_time"
    )
  )
})

# 31. Check if Financial Sustainability tables have changed (LA/time) --------------------------------------------
app$set_inputs(
  dsg_lat_toggle = "Table",
  spend_lat_toggle = "Table"
)

Sys.sleep(time = 0.5)

test_that("Financial Sustainability: over time graphs, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_time_table",
      "specialist_spend_la_time_table"
    )
  )
})

# 32/33. Check if Financial Sustainability graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Financial Sustainability",
  la_choice = "Derby",
  dsg_deficit_la_panel = LAC,
  specialist_spend_la_panel = LAC,
  myregion_switch = FALSE
)
Sys.sleep(time = 0.5)

test_that("Financial Sustainability: benchmarking graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench",
      "specialist_spend_la_bench"
    )
  )
})

app$set_inputs(myregion_switch = TRUE)
test_that("Financial Sustainability: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench",
      "specialist_spend_la_bench"
    )
  )
})

# 34/35. Check if Financial Sustainability tables have changed (LA/bench) --------------------------------------------
app$set_inputs(
  dsg_lab_toggle = "Table",
  spend_lab_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Financial Sustainability: benchmarking, LA level, region switch, tables", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench_table",
      "specialist_spend_la_bench_table"
    )
  )
})

app$set_inputs(myregion_switch = FALSE)
Sys.sleep(time = 0.5)

test_that("Financial Sustainability: benchmarking, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_la_bench_table",
      "specialist_spend_la_bench_table"
    )
  )
})

# 36. Check if Financial Sustainability graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  region_choice = "South West",
  tabsetpanels_reg = "Financial Sustainability"
)
Sys.sleep(time = 0.5)

test_that("Financial Sustainability: over time graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_time",
      "specialist_spend_reg_time"
    )
  )
})

# 37. Check if Financial Sustainability tables have changed (region/time) --------------------------------------------
app$set_inputs(
  dsg_regt_toggle = "Table",
  spend_regt_toggle = "Table"
)

Sys.sleep(time = 0.5)

test_that("Financial Sustainability: over time tables, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_time_table",
      "specialist_spend_reg_time_table"
    )
  )
})

# 38. Check if Financial Sustainability graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "England",
  myregion_switch = FALSE
)
Sys.sleep(time = 0.5)
app$set_inputs(
  tabsetpanels_reg = "Financial Sustainability",
  dsg_deficit_reg_panel = RC
  # specialist_spend_reg_panel = RC tab doesn't currently exist
)
Sys.sleep(time = 0.5)

test_that("Financial Sustainability: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_bench"
      #  "specialist_spend_reg_bench" similarly this doesn't exist
    )
  )
})

# 39. Check if Financial Sustainability tables have changed (region/bench) --------------------------------------------
app$set_inputs(
  dsg_regb_toggle = "Table"
  # spend_regb_toggle = "Table" ...nor this
)
Sys.sleep(time = 0.5)

test_that("Financial Sustainability: benchmarking tables, region level", {
  app$expect_values(
    input = listInputs,
    output = c(
      "dsg_deficit_reg_bench_table"
      # "specialist_spend_reg_bench_table" this doesn't exist either...
    )
  )
})

### Identification of Need ###

# 40. Check if Identification of Need graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Identification of Need",
  la_choice = "Hackney"
)
Sys.sleep(time = 0.5)

test_that("Identification of Need: over time graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_time
  )
})

# 41. Check if Identification of Need tables have changed (LA/time) --------------------------------------------
app$set_inputs(
  ehcppc_lat_toggle = "Table",
  msen_lat_toggle = "Table",
  types_lat_toggle = "Table",
  age_lat_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Identification of Need: over time tables, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_time_table
  )
})

# 42/43. Check if Identification of Need graphs have changed (LA/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Identification of Need",
  la_choice = "Derby",
  percent_pupils_ehcp_la_panel = LAC,
  mainstream_with_sen_la_panel = LAC,
  provider_types_la_panel = LAC,
  myregion_switch = FALSE
)

Sys.sleep(time = 1.5)
test_that("Identification of Need: benchmarking graphs, LA level", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench
  )
})

app$set_inputs(myregion_switch = TRUE)
Sys.sleep(time = 1.5)
test_that("Identification of Need: benchmarking graphs, LA level, region switch", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench
  )
})

# 44/45. Check if Identification of Need tables have changed (LA/bench) --------------------------------------------
app$set_inputs(
  ehcppc_lab_toggle = "Table",
  msen_lab_toggle = "Table",
  types_lab_toggle = "Table"
)

Sys.sleep(time = 1.5)

test_that("Identification of Need, comparison, LA level, region switch, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench_table
  )
})
app$set_inputs(myregion_switch = FALSE)
Sys.sleep(time = 0.5)

test_that("Identification of Need, comparison, LA level, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_la_identification_bench_table
  )
})

# 46. Check if Identification of Need graphs have changed (region/time) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  region_choice = "North West",
  tabsetpanels_reg = "Identification of Need"
)
Sys.sleep(time = 0.5)

test_that("Identification of Need: over time graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_time
  )
})

# 47. Check if Identification of Need tables have changed (region/time) --------------------------------------------
app$set_inputs(
  ehcppc_regt_toggle = "Table",
  msen_regt_toggle = "Table",
  types_regt_toggle = "Table",
  age_regt_toggle = "Table"
)

Sys.sleep(time = 0.5)
test_that("Identification of Need: over time, tables, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_time_table
  )
})

# 48. Check if Identification of Need graphs have changed (region/bench) --------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions"
)
Sys.sleep(time = 0.5)
app$set_inputs(
  region_choice = "Yorkshire and the Humber",
  tabsetpanels_reg = "Identification of Need",
  percent_pupils_ehcp_reg_panel = RC,
  mainstream_with_sen_reg_panel = RC,
  provider_types_reg_panel = RC,
  myregion_switch = FALSE
)
Sys.sleep(time = 0.5)
test_that("Identification of Need: benchmarking graphs, region level", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_bench
  )
})

# 49. Check if Identification of Need tables have changed (region/bench) --------------------------------------------

app$set_inputs(
  ehcppc_regb_toggle = "Table",
  msen_regb_toggle = "Table",
  types_regb_toggle = "Table"
)

Sys.sleep(time = 0.5)

Sys.sleep(time = 0.5)
test_that("Identification of Need, regional comparison, tables", {
  app$expect_values(
    input = listInputs,
    output = outputs_reg_identification_bench_table
  )
})

# 50. Check LA Summary panel ------------------------------------------------------------------------
app$set_inputs(
  navlistPanel = "Local Areas",
  tabsetpanels_la = "Summary",
  la_choice = "Middlesbrough"
)
Sys.sleep(time = 2)

test_that("Summary, LA level", {
  app$expect_values(
    input = listInputs,
    output = "summary"
  )
})

# 51. Check LA Summary table --------------------------------------------------------------------------
app$set_inputs(
  la_sum_toggle = "Table"
)
Sys.sleep(time = 1)

test_that("Summary: LA level, table", {
  app$expect_values(
    input = listInputs,
    output = "la_summary_table"
  )
})

# 52. Check national summary panel ------------------------------------------------------------------------
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

# 53. Check regional summary panel ------------------------------------------------------------------------
app$set_inputs(
  navlistPanel = "England and Regions",
  level_choice = "Regions",
  tabsetpanels_reg = "Summary"
)
Sys.sleep(time = 0.5)

test_that("Summary: Regions level", {
  app$expect_values(
    input = listInputs,
    output = outputs_nat_summary
  )
})
