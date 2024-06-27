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
  "panel_choice",
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
  name = "SEND-AP-Inclusion-Dashboard",
  height = 1026, width = 1778, load_timeout = 1000000, expect_values_screenshot_args = TRUE, timeout = 100000
) # disable screenshots in release version

# watch the tests in browser for debugging
app$view()

test_that("Initial load test", {
  app$set_inputs(
    cookies = c("GA1.1.1768633568.1681211064", "granted", "GS1.1.1697116218.108.0.1697116314.0.0.0"),
    allow_no_input_binding_ = TRUE
  )
  Sys.sleep(time = 4)

  app$set_inputs(cookie_consent = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
  app$expect_values()
})

### Financial sustainability ###

# 38. Check if Financial sustainability graphs have changed (LA/time) --------------------------------------------
app$set_inputs(
  panel_choice = "Local Areas",
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
  panel_choice = "Local Areas",
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
  panel_choice = "England and Regions",
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
  panel_choice = "England and Regions",
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
  panel_choice = "England and Regions",
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
  panel_choice = "Local Areas",
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
  panel_choice = "Local Areas",
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
  panel_choice = "England and Regions",
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
  panel_choice = "England and Regions",
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
  panel_choice = "England and Regions",
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
  panel_choice = "England and Regions",
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
  panel_choice = "Local Areas",
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
  panel_choice = "England and Regions",
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
  panel_choice = "England and Regions",
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
