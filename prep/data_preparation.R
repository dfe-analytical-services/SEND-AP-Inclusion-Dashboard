# Script where we provide functions to read in the data file(s).

# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.

# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.

library(tidyverse)
library(collapse)
library(sf)
library(geojsonsf)
library(readODS)

# GENERAL-USE FILES AND FUNCTIONS ---------------------------------------------------------------------------------

## Local Authority Lookup Table


# To replicate:
#  Download a custom table in ODS format from:
# https://explore-education-statistics.service.gov.uk/data-tables/permalink/b5ad776d-bdb8-42d0-02e5-08daf870ec67
# Re-save it as XLSX

# Load in LA lookup table
la_region_lookup_raw <- rio::import("data/lookups/la_region_lookup_raw.xlsx", skip = 2)

# Give reasonable variable names, remove metrics, etc
la_region_lookup <- la_region_lookup_raw %>%
  rename(
    "region" = `...1`,
    "la_name" = `...2`
  ) %>%
  select(-`2021/22`) %>%
  drop_na(la_name) %>% # remove blank rows, comments etc
  fill(region) # the source data uses merged cells, fill the gaps in the region column


# CCG name lookup from https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2021-names-and-codes-in-england-1/explore
ccg_lookup <- rio::import("data/lookups/Clinical_Commissioning_Groups_(April_2021)_Names_and_Codes_in_England.csv") %>%
  select("nhs_code" = CCG21CDH, "nhs_name" = CCG21NM) %>%
  mutate(nhs_type = "Former CCG area")

# NHS Provider lookup from https://digital.nhs.uk/services/organisation-data-service/export-data-files/csv-downloads/other-nhs-organisations
provider_lookup <- rio::import("data/lookups/etr.csv") %>%
  select("nhs_code" = V1, "nhs_name" = V2) %>%
  mutate(nhs_type = "Provider")

# NHS Care trust provider lookup from https://digital.nhs.uk/services/organisation-data-service/export-data-files/csv-downloads/other-nhs-organisations
caretrusts_lookup <- read_csv("data/lookups/ect.csv", col_names = FALSE) %>%
  select("nhs_code" = X1, "nhs_name" = X2) %>%
  mutate(nhs_type = "Provider")

# NHS CCG to NHS Region lookup
# https://geoportal.statistics.gov.uk/datasets/ons::sub-icb-locations-to-integrated-care-boards-to-nhs-england-region-july-2022-lookup-in-england/explore
nhs_region_lookup <- rio::import("data/lookups/Sub_ICB_Locations_to_Integrated_Care_Boards_to_NHS_England_(Region)_(July_2022)_Lookup_in_England.csv") %>%
  select("nhs_code" = SICBL22CDH, "nhs_region" = NHSER22NM)

# Combine NHS CCG and provider lookups into one
nhs_lookup1 <- bind_rows(ccg_lookup, provider_lookup) %>%
  bind_rows(caretrusts_lookup)

# Add in regions to create one big NHS lookup
nhs_lookup <- left_join(nhs_lookup1, nhs_region_lookup) %>%
  mutate(nhs_region = coalesce(nhs_region, nhs_type)) # If it's a provider let's have it say provider instead of NA

# Create function to add a slash in time period variables to crate academic years
time_period_to_academic_year <- function(df) {
  df %>%
    ftransform(academic_year = paste0(substr(time_period, start = 1, stop = 4), "/", substr(time_period, start = 5, stop = 6)))
}

# OUTCOMES INDICATORS ---------------------------------------------------------------------------------

# % of pupils reaching expected standards in KS2
# Source: Key stage 2 attainment. File: Key stage 2 attainment by region, local authority and pupil characteristics
#  https://explore-education-statistics.service.gov.uk/data-catalogue/key-stage-2-attainment/2021-22

# The usual outcome here is rounded to the nearest % which is not so helpful for ranking. Instead I will recalculate it with one figure after the decimal point.

ks2_attainment <- rio::import("data/attainment_ks2/ks2_regional_local_authority_and_pupil_characteristics_2019_and_2022_revised.csv") %>%
  fsubset(characteristic_group == "SEN status" &
    gender == "Total" &
    characteristic != "No SEN" &
    characteristic != "SEN unclassified") %>%
  fselect(
    time_period, geographic_level,
    region_name, la_name,
    characteristic, pt_rwm_met_expected_standard,
    t_rwm_met_expected_standard, t_rwm_eligible_pupils
  ) %>%
  ftransform(`Percent meeting expected standards` = round(
    100 * (as.numeric(t_rwm_met_expected_standard) / as.numeric(t_rwm_eligible_pupils)),
    digits = 2
  )) %>%
  time_period_to_academic_year()


# Check the calculation is right
# ks2_attainment %>%
#   mutate(newvariable = round(`Percent meeting expected standards`, 0)) %>%
#   mutate(test = ifelse(as.numeric(pt_rwm_met_expected_standard) == newvariable,
#          yes = "Pass",
#          no = "Fail")) %>%
#   count(test)

# Only 4 cases fail, it appears the method used is to calculate it to one decimal place and round up,
# so in cases where it rounds to a .5, it falls foul of R's default rounding method being slightly different.
# However this was only used to check if the calculation of the metric was right, so this is no issue.

# KS1 phonics check
ks1_phonics <- rio::import("data/phonics/phonics_y1_regional_and_local_authority_f_2012_to_2022.csv") %>%
  fsubset(characteristic_group == "SEN status" &
    time_period >= 201516 &
    gender == "Total" &
    characteristic %in% c("EHC plan", "SEN support", "No SEN"))

# for unclear reasons, this section crashes with collapse verbs but works fine with dplyr ones
ks1_phonics_allSEN <- ks1_phonics %>%
  ungroup() %>%
  group_by(
    time_period, geographic_level,
    region_name, la_name
  ) %>%
  filter(characteristic != "No SEN") %>%
  summarise(
    tested_pupils = sum(as.numeric(t_phonics_y1_eligible_pupils), na.rm = T),
    expected_pupils = sum(as.numeric(t_phonics_y1_met_expected_standard), na.rm = T)
  ) %>%
  mutate(
    `Percent meeting expected standards in Y1` = 100 * expected_pupils / tested_pupils,
    characteristic = "All SEN"
  ) %>%
  select(
    time_period, geographic_level,
    region_name, la_name,
    characteristic, `Percent meeting expected standards in Y1`
  )

ks1_phonics <- ks1_phonics %>%
  fselect(
    time_period, geographic_level,
    region_name, la_name,
    characteristic, pt_phonics_y1_met_expected_standard
  ) %>%
  ftransform(`Percent meeting expected standards in Y1` = as.numeric(pt_phonics_y1_met_expected_standard)) %>%
  bind_rows(ks1_phonics_allSEN) %>%
  time_period_to_academic_year()

# KS4 attainment (attainment 8)
ks4_attainment <- rio::import("data/attainment_ks4/2122_lachar_data.csv") %>%
  fsubset(breakdown == "SEN description" &
    characteristic_free_school_meals == "Total" &
    characteristic_sen_status == "Total" &
    characteristic_sen_description %in% c("Any SEN", "No identified SEN") &
    characteristic_disadvantage == "Total" &
    characteristic_gender == "Total") %>%
  fselect(
    time_period, geographic_level,
    region_name, la_name,
    characteristic_sen_description,
    t_pupils, t_schools,
    avg_att8, avg_p8score,
    p8score_CI_low, p8score_CI_upp
  ) %>%
  time_period_to_academic_year() %>%
  ftransform("SEN provision" = recode(characteristic_sen_description,
    "Any SEN" = "Any SEN",
    "No identified SEN" = "No identified SEN"
  )) %>%
  mutate(
    "Average progress 8 score" = as.numeric(avg_p8score),
    "Progress 8 score (lower confidence interval)" = as.numeric(p8score_CI_low),
    "Progress 8 score (upper confidence interval)" = as.numeric(p8score_CI_upp)
  )


# KS4 (post-16) destination measures (non-national)
ks4_destinations <- rio::import("data/destinations_ks4/ks4_dm_ud_202021_la_prov.csv.gz") %>%
  fsubset(characteristic_group == "SEN Provision" &
    data_type == "Percentage" &
    institution_group == "State-funded mainstream & special schools" &
    characteristic %in% c("Identified SEN", "School Action/SEN support", "Statement of SEN/Education Health and Care plan")) %>%
  mutate(characteristic = recode(characteristic,
    "Statement of SEN/Education Health and Care plan" = "EHC plan or Statement",
    "School Action/SEN support" = "SEN support"
  )) %>%
  time_period_to_academic_year() %>%
  fsubset(academic_year > "2017/18") %>%
  pivot_longer(
    cols = c(all_work, appren, sfc, ssf, fe, other_edu, all_notsust, all_unknown),
    names_to = "destination_raw",
    values_to = "pupils"
  ) %>%
  mutate(
    `% of pupils` = round(as.numeric(pupils), 2),
    Destination = recode(destination_raw,
      "all_work" = "Employment",
      "appren" = "Apprenticeship",
      "sfc" = "Sixth form college",
      "ssf" = "School sixth form",
      "fe" = "Further education",
      "other_edu" = "Education (other)",
      "all_notsust" = "Not sustained",
      "all_unknown" = "Unknown"
    )
  ) %>%
  mutate(Destination = factor(Destination,
    ordered = TRUE,
    levels = c(
      "Unknown",
      "Not sustained",
      "Apprenticeship",
      "Employment",
      "Education (other)",
      "Further education",
      "School sixth form",
      "Sixth form college"
    )
  ))

# KS4 (post-16) destination measures (national)
ks4_destinations_nat <- rio::import("data/destinations_ks4/national/ks4_dm_ud_202021_nat_prov.csv") %>%
  fsubset(characteristic_group == "SEN Provision" &
    data_type == "Percentage" &
    institution_group == "State-funded mainstream & special schools" &
    characteristic %in% c("Identified SEN", "School Action/SEN support", "Statement of SEN/Education Health and Care plan")) %>%
  mutate(characteristic = recode(characteristic,
    "Statement of SEN/Education Health and Care plan" = "EHC plan or Statement",
    "School Action/SEN support" = "SEN support"
  )) %>%
  time_period_to_academic_year() %>%
  fsubset(academic_year > "2017/18") %>%
  mutate(
    appren = as.numeric(appren),
    all_work = as.numeric(all_work)
  ) %>%
  pivot_longer(
    cols = c(all_work, appren, sfc, ssf, fe, other_edu, all_notsust, all_unknown),
    names_to = "destination_raw",
    values_to = "pupils"
  ) %>%
  mutate(
    `% of pupils` = round(as.numeric(pupils), 2),
    Destination = recode(destination_raw,
      "all_work" = "Employment",
      "appren" = "Apprenticeship",
      "sfc" = "Sixth form college",
      "ssf" = "School sixth form",
      "fe" = "Further education",
      "other_edu" = "Education (other)",
      "all_notsust" = "Not sustained",
      "all_unknown" = "Unknown"
    )
  ) %>%
  mutate(Destination = factor(Destination,
    ordered = TRUE,
    levels = c(
      "Unknown",
      "Not sustained",
      "Apprenticeship",
      "Employment",
      "Education (other)",
      "Further education",
      "School sixth form",
      "Sixth form college"
    )
  ))



# NHS data

# TBC

# Local area inspection status
# As per ADCS spreadsheet here: https://adcs.org.uk/assets/documentation/SEND_Outcomes_Summary_www.xlsx
ofsted_raw <- rio::import("data/ofsted/SEND_Outcomes_Summary_www.xlsx", skip = 1)

# Check for mismatches
# ofsted_mismatched <- ofsted %>%
#      left_join(la_region_lookup, by = c("la_name" = "la_name")) %>% #Lots of LAs with different names...
#   filter(is.na(region)) %>%
#   distinct(LA)

# Clean up data: and rename LAs to match DfE data
ofsted <- ofsted_raw %>%
  # fsubset(LA != "Northamptonshire") %>% # Northamptonshire no longer exists but since W/N Northamptonshire haven't been inspected yet use it anyway
  mutate(la_name = recode(LA,
    "Stoke" = "Stoke-on-Trent",
    "Herefordshire" = "Herefordshire, County of",
    "Telford" = "Telford and Wrekin",
    "Hull" = "Kingston upon Hull, City of",
    "Durham" = "County Durham",
    "Bedford Borough" = "Bedford",
    "St Helens" = "St. Helens",
    "Newcastle" = "Newcastle upon Tyne",
    "NE Lincolnshire" = "North East Lincolnshire",
    "Kingston" = "Kingston upon Thames",
    "Lincolnshire CC" = "Lincolnshire",
    "Stockton on Tees" = "Stockton-on-Tees",
    "Bath and NE Somerset" = "Bath and North East Somerset",
    "Blackburn and Darwen" = "Blackburn with Darwen",
    "Derby City Council" = "Derby",
    "Derby City" = "Derby",
    "Bristol" = "Bristol, City of",
    "Leicesteshire" = "Leicestershire",
    "Newcastle City" = "Newcastle upon Tyne",
    "Birmingham CC" = "Birmingham",
    "Sunderland/TfChildren" = "Sunderland",
    "East Riding" = "East Riding of Yorkshire",
    "Nottingham City" = "Nottingham",
    "Kensington & Chelsea" = "Kensington and Chelsea"
  )) %>%
  left_join(la_region_lookup) %>%
  group_by(la_name) %>%
  arrange(desc(`Visit Date`), .by_group = TRUE) %>% # Sort within each LA by most recent visit
  slice(1) %>% # Take most recent only#
  mutate(
    `Publication Date` = lubridate::ymd(`Publication Date`),
    `Visit Date` = lubridate::ymd(`Visit Date`)
  ) %>% # Interpret column as date
  mutate(
    `Publication Date` = format(`Publication Date`, "%d %B %Y"),
    `Visit Date` = format(`Visit Date`, "%d %B %Y")
  ) %>% # Reformat to nicer date e.g. 21 June 2022

  # Now, to clean up the inspection outcomes column
  mutate(
    `Inspection Outcome` =
      case_when(
        Type == "Re SEND" & `Written Statement of Action` == "Yes" ~ "Written Statement of Action (after reinspection)",
        Type == "Re SEND" & `Written Statement of Action` == "No" |
          `Written Statement of Action` == "Removed" ~ "Previous Written Statement of Action Removed (after reinspection)",
        Type == "SEND" & `Written Statement of Action` == "Yes" ~ "Written Statement of Action",
        Type == "SEND" & `Written Statement of Action` == "No" ~ "No Written Statement of Action",
        `Written Statement of Action` %in% c("DfE to determine", "DFE to determine") ~ "DfE and NHS England to determine if WSoA required"
      )
  ) %>%
  mutate(`Inspection Outcome` = ifelse(test = la_name == "Tameside" & `Link to visit report` == "https://reports.ofsted.gov.uk/provider/44/80569",
    yes = "Written Statement of Action",
    no = `Inspection Outcome`
  )) %>% # Verdict was missing from original spreadsheet
  fselect(la_name, region, `Visit Date`, `Publication Date`, `Inspection Outcome`) %>%
  mutate(box_colour = case_when(
    `Inspection Outcome` %in% c(
      "Written Statement of Action",
      "Written Statement of Action (after reinspection)"
    ) ~ "maroon",
    `Inspection Outcome` %in% c(
      "No Written Statement of Action",
      "Previous Written Statement of Action Removed (after reinspection)"
    ) ~ "green",
    `Inspection Outcome` == "DfE and NHS England to determine if WSoA required" ~ "orange"
  ))

# Specify when ADCS spreadsheet has been updated here
ofsted_data_updated <- "Inspection reports data last updated 12th October 2022."

## ------------------------##
## EXPERIENCES INDICATORS ##
## ------------------------##

# SEN2_mi multipurpose file
# Source: Education, health and care plans. File 05 - Requests, assessments, discontinued plans, 20 week timeliness, mainstream to special transfers, mediation and tribunals
# https://explore-education-statistics.service.gov.uk/data-catalogue/education-health-and-care-plans/2022
sen2_mi <- read_csv("data/ehc_plans/sen2_mi.csv",
  col_types = cols(NoExc20weekRate = col_number())
)

# EHCP timeliness
ehcp_timeliness <- sen2_mi %>%
  fsubset(time_period > 2016) %>%
  fselect(time_period, geographic_level, region_code,
    region_name, old_la_code, new_la_code,
    la_name, NewallEHC,
    `% of EHCPs issued within 20 weeks` = NoExc20weekRate
  ) %>%
  ftransform(region_name = if_else(condition = geographic_level == "National",
    true = "England",
    false = region_name
  ))


# Overall absence rate (autumn and spring terms)
absence <- rio::import("data/absence/6_absence_2term_characteristics.csv.gz") %>%
  fsubset(school_type == "Total" &
    characteristic %in% c("SEN - SEN support", "SEN - Statement or EHCP")) %>%
  ftransform(characteristic = recode(characteristic,
    "SEN - SEN support" = "SEN support",
    "SEN - Statement or EHCP" = "EHCP or Statement"
  )) %>%
  ftransform(region_name = if_else(condition = geographic_level == "National",
    true = "England",
    false = region_name
  )) %>%
  fselect(time_period, geographic_level, region_name, new_la_code, la_name, characteristic, sess_overall_percent, sess_overall, sess_possible) %>%
  time_period_to_academic_year() %>%
  ftransform(`Overall absence %` = round(as.numeric(sess_overall_percent), 2))

absence_allSEN <- absence %>%
  ungroup() %>%
  fgroup_by(time_period, geographic_level, region_name, new_la_code, la_name) %>%
  fsummarise(
    sess_overall = sum(as.numeric(sess_overall), na.rm = T),
    sess_possible = sum(as.numeric(sess_possible), na.rm = T)
  ) %>%
  ftransform(
    `Overall absence %` = round(100 * sess_overall / sess_possible, 2),
    characteristic = "All SEN"
  ) %>%
  time_period_to_academic_year()

absence <- bind_rows(absence, absence_allSEN)

absence_regional <- rio::import("data/absence/6_absence_2term_characteristics.csv.gz") %>%
  fsubset(school_type == "Total" &
    characteristic %in% c("SEN - SEN support", "SEN - Statement or EHCP") &
    geographic_level %in% c("National", "Regional")) %>%
  ftransform(
    characteristic = recode(characteristic,
      "SEN - SEN support" = "SEN support",
      "SEN - Statement or EHCP" = "EHCP or Statement"
    ),
    region_name = ifelse(geographic_level == "National",
      yes = "England",
      no = region_name
    )
  ) %>%
  fselect(time_period,
    "region_old" = region_name,
    new_la_code,
    la_name,
    characteristic,
    sess_overall_percent,
    sess_overall,
    sess_possible
  ) %>%
  mutate(region_name = ifelse(test = region_old %in% c("Inner London", "Outer London"),
    yes = "London",
    no = region_old
  )) %>% # Collapse Inner/Outer London into one region
  group_by(time_period, characteristic, region_name) %>%
  summarise(
    sess_overall = sum(sess_overall),
    sess_possible = sum(sess_possible),
    sess_overall_percent = mean(sess_overall_percent)
  ) %>%
  rowwise() %>%
  mutate(overall_absence_percent = round(100 * (sess_overall / sess_possible), 5)) %>% # This perfectly reproduces the existing metric
  time_period_to_academic_year() %>%
  ftransform(`Overall absence %` = round(100 * sess_overall / sess_possible, 2))

absence_regional_allSEN <- absence_regional %>%
  group_by(time_period, academic_year, region_name) %>%
  summarise(
    sess_overall = sum(sess_overall),
    sess_possible = sum(sess_possible),
    sess_overall_percent = mean(sess_overall_percent)
  ) %>% # this metric is not apparently used for anything but for consistency and bind_rows...
  ftransform(
    overall_absence_percent = round(100 * (sess_overall / sess_possible), 5),
    `Overall absence %` = round(100 * sess_overall / sess_possible, 2),
    characteristic = "All SEN"
  )

absence_regional <- bind_rows(absence_regional, absence_regional_allSEN)
#### Test the calculations have worked
#
# # #Take all non-London values and compare
# absence_regional_test <- absence_regional %>%
#   filter(str_detect(region_name, "London", negate = TRUE)) %>%
#   arrange(region_name, academic_year, characteristic)
# #
# absence_test <- absence %>%
#   filter(geographic_level %in% c("National", "Regional")) %>%
#   filter(str_detect(region_name, "London", negate = TRUE)) %>%
#   arrange(region_name, academic_year, characteristic)
# #
# absence_test$sess_overall_percent == absence_regional_test$sess_overall_percent
# #
# # ##Compare total absences for London
# #Total absences for London in the "regional" data
# absence_regional_london_test <- absence_regional %>%
#   filter(str_detect(region_name, "London")) %>%
#   pull(sess_overall) %>%
#   sum()
# #Total absences for London in the original data
# absence_london_test <- absence %>%
#   filter(geographic_level == "Regional") %>%
#   filter(str_detect(region_name, "London")) %>%
#   pull(sess_overall) %>%
#   sum()
#
# #Check they are the same.
# absence_london_test == absence_regional_london_test

# Now we've confirmed the change to London region has worked we can remove non-LA data from the main dataset
absence <- absence %>%
  fsubset(geographic_level == "Local authority")


# SEND Tribunal appeal rate (https://www.gov.uk/government/statistics/tribunal-statistics-quarterly-january-to-march-2022)
tribunals_raw <- rio::import("data/tribunals/SEND_Appeal_Rate_Table_2021.ods", sheet = "SEND_2", skip = 5)
tribunals2 <- rio::import("data/tribunals/SEND_Appeal_Rate_Table_2021.ods", sheet = "SEND_2", skip = 3)


# Find range of years in column names
end_year <- max(as.numeric(colnames(tribunals2)), na.rm = TRUE)
start_year <- min(as.numeric(colnames(tribunals2)), na.rm = TRUE)

# Find number of years covered
n_years <- (end_year - start_year) + 1

# Create vector of years, with a blank for la_name, then repeat the sequence of years 3 times, sorted so you get e.g. 2014 three times in a row
years <- append("", sort(c(
  seq(from = start_year, to = end_year),
  seq(from = start_year, to = end_year),
  seq(from = start_year, to = end_year)
)))

# Create column names by repeating the three categories the required number of times
column_names <- append(
  "la_name",
  rep(
    x = c("appealsregistered_", "totalappealabledecisions_", "appealrate_"),
    times = n_years
  )
)
# Add years to the column names
colnames(tribunals_raw) <- paste0(column_names, years)

tribunals1 <- tribunals_raw %>%
  ftransform(la_name = str_remove_all(string = la_name, pattern = "[:digit:]")) %>%
  mutate(la_name = recode(la_name,
    "Durham" = "County Durham",
    "Herefordshire" = "Herefordshire, County of",
    "Kingston Upon Hull, City of" = "Kingston upon Hull, City of",
    "England Total" = "England"
  )) %>%
  pivot_longer(
    cols = (-la_name),
    names_to = c("metric", "year"),
    names_sep = "_",
    values_to = "value"
  )

tribunals <- tribunals1 %>%
  fsubset(metric == "appealrate" &
    year > 2016) %>%
  ftransform(`SEND Tribunal Appeal Rate` = round(100 * as.numeric(value), digits = 2)) %>%
  drop_na(`SEND Tribunal Appeal Rate`) %>%
  full_join(la_region_lookup) %>%
  mutate(
    geographic_level = ifelse(la_name == "England",
      yes = "National",
      no = "Local authority"
    ),
    region_name = ifelse(la_name == "England",
      yes = "England",
      no = region
    )
  )

# Tribunals data doesn't aggregate appeal rate at region level, so this code does that.
tribunals_reg <- tribunals1 %>%
  drop_na(value) %>%
  full_join(la_region_lookup) %>% # Add in region name
  filter(metric != "appealrate") %>% # Remove LA-level appeal rates
  mutate(region_name = ifelse(la_name == "England",
    yes = "England",
    no = region
  )) %>% # Add England as a region
  group_by(year, region_name, metric) %>%
  summarise(total_value = 100 * sum(as.numeric(value), na.rm = TRUE)) %>% # Add regional values together from LAs
  drop_na(region_name) %>% # Remove old LAs e.g. Poole
  pivot_wider(
    id_cols = c(region_name, year),
    names_from = metric,
    values_from = total_value
  ) %>% # Pivot table wider for computing values
  rowwise() %>% # Calculate on each row
  mutate(`SEND Tribunal Appeal Rate` = 100 * (appealsregistered / totalappealabledecisions)) %>% # Calculate appeal rate
  filter(year > 2016)


# 16-18 (post-19) destinations
destinations_1618 <- rio::import("data/destinations_1618/1618_dm_ud_202021_la_prov.csv") %>%
  fsubset(data_type == "Percentage" &
    cohort_level_group == "Total" &
    characteristic %in% c("Identified LLDD", "Identified SEN", "No identified LLDD", "No identified SEN")) %>%
  fselect(
    time_period, geographic_level, region_name, la_name, characteristic,
    characteristic_group, cohort, all_work, appren, he, fe, other_edu, all_notsust, all_unknown
  ) %>%
  time_period_to_academic_year() %>%
  pivot_longer(
    cols = c(all_work, appren, he, fe, other_edu, all_notsust, all_unknown),
    names_to = "destination_raw",
    values_to = "% of pupils"
  ) %>%
  mutate(`% of pupils` = as.numeric(`% of pupils`)) %>%
  mutate("Destination" = recode(destination_raw,
    "all_work" = "Employment",
    "appren" = "Apprenticeship",
    "fe" = "Further education",
    "he" = "Higher education",
    "other_edu" = "Education (other)",
    "all_notsust" = "Not sustained",
    "all_unknown" = "Unknown"
  )) %>%
  mutate(Destination = factor(Destination,
    ordered = TRUE,
    levels = c(
      "Unknown",
      "Not sustained",
      "Apprenticeship",
      "Employment",
      "Education (other)",
      "Further education",
      "Higher education"
    )
  ))


destinations_1618_nat <- rio::import("data/destinations_1618/national/1618_dm_ud_202021_nat_rev.csv") %>%
  fsubset(data_type == "Percentage" &
    cohort_level_group == "Total" &
    characteristic %in% c("Identified LLDD", "Identified SEN", "No identified LLDD", "No identified SEN")) %>%
  fselect(time_period, geographic_level, characteristic, characteristic_group, cohort, all_work, appren, he, fe, other_edu, all_notsust, all_unknown) %>%
  time_period_to_academic_year() %>%
  mutate(
    all_notsust = as.numeric(all_notsust),
    all_unknown = as.numeric(all_unknown)
  ) %>%
  pivot_longer(
    cols = c(all_work, appren, he, fe, other_edu, all_notsust, all_unknown),
    names_to = "destination_raw",
    values_to = "% of pupils"
  ) %>%
  mutate(`% of pupils` = as.numeric(`% of pupils`)) %>%
  mutate("Destination" = recode(destination_raw,
    "all_work" = "Employment",
    "appren" = "Apprenticeship",
    "fe" = "Further education",
    "he" = "Higher education",
    "other_edu" = "Education (other)",
    "all_notsust" = "Not sustained",
    "all_unknown" = "Unknown"
  )) %>%
  mutate(Destination = factor(Destination,
    ordered = TRUE,
    levels = c(
      "Unknown",
      "Not sustained",
      "Apprenticeship",
      "Employment",
      "Education (other)",
      "Further education",
      "Higher education"
    )
  ))


## -------------------------------------#
## FINANCIAL SUSTAINABILITY INDICATORS #
## -------------------------------------#
# DSG cumulative balance as a % of the total budget
# irritatingly the cumulative balance has to come from the section 251 return whereas the total budget is not listed
# in that report (the s251 "budget" is post-academy recoupment) so we need to join two separate documents

dsg_budget_24 <- rio::import("data/finance/dedicated-schools-grant.ods", sheet = "Allocations_2324", skip = 1)[, 1:7] %>%
  mutate(across(3:7, as.numeric),
    time_period = "202324"
  ) %>%
  drop_na(`Schools block (£s)`)
dsg_budget_23 <- rio::import("data/finance/dedicated-schools-grant.ods", sheet = "Allocations_2223", skip = 1)[, 1:7] %>%
  mutate(across(3:7, as.numeric),
    time_period = "202223"
  ) %>%
  drop_na(`Schools block (£s)`)
dsg_budget_22 <- rio::import("data/finance/dedicated-schools-grant.ods", sheet = "Allocations_2122", skip = 1)[, 1:7] %>%
  mutate(across(3:7, as.numeric),
    time_period = "202122"
  ) %>%
  drop_na(`Schools block (£s)`)
dsg_budget_21 <- rio::import("data/finance/dedicated-schools-grant.ods", sheet = "Allocations_2021", skip = 1)[, 1:7] %>%
  mutate(across(3:7, as.numeric),
    time_period = "202021"
  ) %>%
  drop_na(`Schools block (£s)`)
dsg_budget_20 <- rio::import("data/finance/dedicated-schools-grant.ods", sheet = "Allocations_1920", skip = 1)[, 1:7] %>%
  mutate(across(3:7, ~ 1000000 * as.numeric(.x)), # 2019-20 budget is in millions rather than £s so correct (we'll fix the names in a bit)
    time_period = "201920"
  ) %>%
  drop_na(`2019-20 schools block \n(£ million)`)
names(dsg_budget_20) <- names(dsg_budget_21)

dsg_budget <- bind_rows(dsg_budget_24, dsg_budget_23, dsg_budget_22, dsg_budget_21, dsg_budget_20)
names(dsg_budget) <- c("la_code", "la_name", names(dsg_budget)[c(1:5, 8)])
dsg_budget <- dsg_budget %>%
  mutate(la_name = case_when(
    is.na(la_name) ~ "", # fixing a bunch of typos etc pre-join
    la_name == "Bournemouth Christchurch and Poole" ~ "Bournemouth, Christchurch and Poole",
    la_name == "Bristol City of" ~ "Bristol, City of",
    la_name == "Herefordshire" ~ "Herefordshire, County of",
    la_name == "Durham" ~ "County Durham",
    la_name == "Kingston upon Hull City of" ~ "Kingston upon Hull, City of",
    TRUE ~ la_name
  )) %>%
  left_join(la_region_lookup) %>%
  mutate(region = case_when(
    la_code == "ENGLAND" ~ "England",
    la_code == "LONDON" ~ "London",
    la_name == "" ~ la_code,
    TRUE ~ region
  )) %>%
  select(-la_code) %>%
  distinct() # London is repeated for some reason, so this line should remove four rows, one for each year

dsg_deficit <- rio::import("data/finance/s251_alleducation_la_regional_national2.csv") %>%
  fsubset(time_period > 201819) %>%
  ftransform(time_period = as.character(time_period)) %>% # for join (and also this isn't really a number anyway)
  fsubset(category_of_expenditure ==
    "1.9.3 Dedicated Schools Grant carried forward to next year") %>%
  pivot_wider(
    names_from = category_of_expenditure,
    values_from = gross_expenditure
  )

dsg_london <- dsg_deficit %>%
  fsubset(region_name %in% c("Inner London", "Outer London") &
    geographic_level == "Regional") %>% # filter to Inner and Outer London to combine them
  group_by(
    time_period, time_identifier, geographic_level, country_name, country_code, la_name, old_la_code, new_la_code,
    main_category, early_years_establishments, primary_schools, secondary_schools, sen_and_special_schools, pupil_referral_units_and_alt_provision,
    post_16, income, net_expenditure, net_per_capita_expenditure
  ) %>%
  summarise(
    region_name = "London",
    region_code = "E12000007",
    `1.9.3 Dedicated Schools Grant carried forward to next year` = sum(`1.9.3 Dedicated Schools Grant carried forward to next year`, na.rm = T)
  )

dsg_deficit <- dsg_deficit %>%
  fsubset(!(region_name %in% c("Inner London", "Outer London") &
    geographic_level == "Regional")) %>% # not the ones in the previous table
  bind_rows(dsg_london) %>% # because adding it back %>%
  ftransform(region_name = case_when(region_name %in% c("Inner London", "Outer London") ~ "London",
    region_name == "" ~ "England",
    .default = region_name
  )) %>%
  inner_join(select(dsg_budget, la_name, region, time_period, `Total DSG allocation (£s)`), by = c("la_name", "region_name" = "region", "time_period")) %>%
  rowwise() %>%
  mutate(deficit = round(
    100 * (`1.9.3 Dedicated Schools Grant carried forward to next year` /
      `Total DSG allocation (£s)`), 2
  )) %>%
  ftransform(`DSG cumulative balance as a % of the total budget` = -deficit) %>%
  fselect(time_period, geographic_level, region_name, la_name, deficit, `DSG cumulative balance as a % of the total budget`) %>%
  ftransform(financial_year = paste0(substr(time_period, start = 1, stop = 4), "-", substr(time_period, start = 5, stop = 6)))

# Per capita gross spend on non-maintained and independent special provision
# From High Needs Benchmarking Tool

# import xlsx spreadsheet - this is a modified version of the original "High Needs Benchmarking Tool".
# To reproduce, select a year in the "Select Comparators" tab AND in the "All LA data" tab dropdowns.
# Select 2+ LAs in the "Select Comparators" tab, select Outturn and ensure all year selections are consistent.
# Then copy the whole sheet and paste as values in its own tab (tab names as below)
# The code brings in population figures that are not currently used. You may decide to calculate regional/national figures
# in which case the population figures will be needed

specialist_spend_201819 <- rio::import("data/finance/2021-22-High-Needs-LA-Benchmarking-Tool.xlsx",
  sheet = "values_201819",
  range = c("D6:CE165")
) %>% # Import table
  select(
    "la_name" = "...1",
    "pop2to18_2018-19" = "(ONS mid-2018 projection)",
    "specialschools_sf_2018-19" = "Special schools and academies (1)...69",
    "ap_sf_2018-19" = "PRUs and AP academies (1)...70",
    "specialschools_ind_2018-19" = "Special schools and academies (2)...77",
    "ap_ind_2018-19" = "PRUs and AP academies (2)...78"
  ) %>%
  fsubset(la_name != "Dorset (New, LA code 838)") %>% # This row is blank
  rowwise() %>%
  mutate(
    "independent_2018-19" = sum(as.numeric(`specialschools_ind_2018-19`), as.numeric(`ap_ind_2018-19`),
      na.rm = TRUE
    ), # add independent topup together for special and AP schools
    "state_2018-19" = sum(as.numeric(`specialschools_sf_2018-19`), as.numeric(`ap_sf_2018-19`), # add state sector together
      na.rm = TRUE
    ),
    "pop2to18_2018-19" = as.numeric(`pop2to18_2018-19`),
    "total_2018-19" = sum(`independent_2018-19`, `state_2018-19`)
  ) %>% # population column - not currently used
  drop_na(la_name, `state_2018-19`)


specialist_spend_201920 <- rio::import("data/finance/2021-22-High-Needs-LA-Benchmarking-Tool.xlsx",
  sheet = "values_201920",
  range = c("D6:CE165")
) %>%
  select(
    "la_name" = "...1",
    "pop2to18_2019-20" = "(ONS mid-2019 projection)",
    "specialschools_sf_2019-20" = "Special schools and academies (1)...69",
    "ap_sf_2019-20" = "PRUs and AP academies (1)...70",
    "specialschools_ind_2019-20" = "Special schools and academies (2)...77",
    "ap_ind_2019-20" = "PRUs and AP academies (2)...78"
  ) %>%
  fsubset(la_name != "Dorset") %>% # There is a blank row for "Dorset"
  mutate(la_name = recode(la_name,
    "Dorset (New, LA code 838)" = "Dorset"
  )) %>%
  rowwise() %>%
  mutate(
    "independent_2019-20" = sum(as.numeric(`specialschools_ind_2019-20`), as.numeric(`ap_ind_2019-20`),
      na.rm = TRUE
    ),
    "state_2019-20" = sum(as.numeric(`specialschools_sf_2019-20`), as.numeric(`ap_sf_2019-20`),
      na.rm = TRUE
    ),
    "pop2to18_2019-20" = as.numeric(`pop2to18_2019-20`),
    "total_2019-20" = sum(`independent_2019-20`, `state_2019-20`)
  ) %>%
  drop_na(la_name, `state_2019-20`)


specialist_spend_202021 <- rio::import("data/finance/2021-22-High-Needs-LA-Benchmarking-Tool.xlsx",
  sheet = "values_202021",
  range = c("D6:CE165")
) %>%
  select(
    "la_name" = "...1",
    "pop2to18_2020-21" = "(ONS mid-2020 projection)",
    "specialschools_sf_2020-21" = "Special schools and academies (1)...69",
    "ap_sf_2020-21" = "PRUs and AP academies (1)...70",
    "specialschools_ind_2020-21" = "Special schools and academies (2)...77",
    "ap_ind_2020-21" = "PRUs and AP academies (2)...78"
  ) %>%
  fsubset(la_name != "Dorset") %>% # There is a blank row for "Dorset"
  mutate(la_name = recode(la_name,
    "Dorset (New, LA code 838)" = "Dorset"
  )) %>%
  rowwise() %>%
  mutate(
    "independent_2020-21" = sum(as.numeric(`specialschools_ind_2020-21`), as.numeric(`ap_ind_2020-21`),
      na.rm = TRUE
    ),
    "state_2020-21" = sum(as.numeric(`specialschools_sf_2020-21`), as.numeric(`ap_sf_2020-21`),
      na.rm = TRUE
    ),
    "pop2to18_2020-21" = as.numeric(`pop2to18_2020-21`),
    "total_2020-21" = sum(`independent_2020-21`, `state_2020-21`)
  ) %>%
  drop_na(la_name, `state_2020-21`)



# Combine the three years into one table
specialist_spend_1 <- specialist_spend_201819 %>%
  left_join(specialist_spend_201920) %>%
  left_join(specialist_spend_202021) %>%
  select(la_name, starts_with("state"), starts_with("ind"), starts_with("total"))

specialist_spend <- specialist_spend_1 %>% # Cleaning up the ADCS' list of LAs to match our names
  mutate(la_name = recode(la_name,
    "Stoke" = "Stoke-on-Trent",
    "Herefordshire" = "Herefordshire, County of",
    "Cheshire West And Chester" = "Cheshire West and Chester",
    "Telford" = "Telford and Wrekin",
    "Hull" = "Kingston upon Hull, City of",
    "Durham" = "County Durham",
    "Bedford Borough" = "Bedford",
    "St Helens" = "St. Helens",
    "Newcastle" = "Newcastle upon Tyne",
    "NE Lincolnshire" = "North East Lincolnshire",
    "Kingston" = "Kingston upon Thames",
    "Lincolnshire CC" = "Lincolnshire",
    "Stockton on Tees" = "Stockton-on-Tees",
    "Bath and NE Somerset" = "Bath and North East Somerset",
    "Blackburn and Darwen" = "Blackburn with Darwen",
    "Derby City Council" = "Derby",
    "Derby City" = "Derby",
    "Bristol" = "Bristol, City of",
    "Leicesteshire" = "Leicestershire",
    "Newcastle City" = "Newcastle upon Tyne",
    "Birmingham CC" = "Birmingham",
    "Sunderland/TfChildren" = "Sunderland",
    "East Riding" = "East Riding of Yorkshire",
    "Nottingham City" = "Nottingham",
    "Kensington & Chelsea" = "Kensington and Chelsea",
    "Bournemouth, Christchurch & Poole" = "Bournemouth, Christchurch and Poole"
  )) %>%
  pivot_longer(
    cols = -la_name,
    names_to = c("category", "year"),
    names_sep = "_",
    values_to = "Spend per head"
  ) %>%
  left_join(la_region_lookup) %>%
  ftransform(`Spend per head` = round(`Spend per head`, 0)) %>%
  ftransform(category = recode(category,
    "independent" = "Independent or non-maintained",
    "state" = "State",
    "total" = "Total"
  )) %>%
  fsubset(la_name != "Dorset (New, LA code 838)") %>%
  distinct()




## -----------------------------------#
## IDENTIFICATION OF NEED INDICATORS #
## -----------------------------------#

# % of pupils with an EHCP
# Source: SEN in England. 01 - Pupils in all schools, by type of SEN provision - including independent schools and general hospital schools - 2016 to 2022
# https://explore-education-statistics.service.gov.uk/data-catalogue/special-educational-needs-in-england/2021-22
sen_phase_type <- read_csv("data/sen_in_england/sen_phase_type.csv", col_types = c(
  ehc_plan_percent = "numeric",
  sen_support_percent = "numeric"
))

percent_pupils_ehcp <- sen_phase_type %>%
  fsubset(phase_type_grouping == "Total" &
    type_of_establishment == "Total" &
    hospital_school == "Total") %>%
  fselect(
    time_period,
    geographic_level,
    region_name,
    la_name,
    new_la_code,
    ehc_plan_percent,
    sen_support_percent
  ) %>%
  pivot_longer(
    cols = c(ehc_plan_percent, sen_support_percent),
    names_to = "SEN provision",
    values_to = "% of pupils"
  ) %>%
  time_period_to_academic_year() %>%
  ftransform(
    `% of pupils` = round(`% of pupils`, 2),
    `SEN provision` = recode(`SEN provision`, ehc_plan_percent = "EHC plan", sen_support_percent = "SEN support"),
    region_name = if_else(condition = geographic_level == "National",
      true = "England",
      false = region_name
    )
  ) %>%
  fsubset(academic_year > "2016/17")

# Number of EHCPs by age group
# Source: Education, health and care plans. File: 01 - EHC plans - by age - January 2010 to 2022
# https://explore-education-statistics.service.gov.uk/data-catalogue/education-health-and-care-plans/2022
ehcp_ageprofile <- rio::import("data/ehc_plans/sen2_age_caseload.csv") %>%
  fsubset(characteristic_age != "Total") %>%
  mutate(
    `Number of EHCPs` = as.numeric(Total_ehc),
    `Age group` = factor(characteristic_age,
      ordered = TRUE,
      levels = c(
        "Age 20 to 25",
        "Age 16 to 19",
        "Age 11 to 15",
        "Age 5 to 10",
        "Under 5"
      )
    )
  )

# % pupils in a mainstream setting with SEN / an EHCP
# Source: SEN in England. 01 - Pupils in all schools, by type of SEN provision - including independent schools and general hospital schools - 2016 to 2022
# https://explore-education-statistics.service.gov.uk/data-catalogue/special-educational-needs-in-england/2021-22
mainstream_with_sen <- sen_phase_type %>%
  fsubset(phase_type_grouping %in% c(
    "State-funded nursery",
    "State-funded primary",
    "State-funded secondary"
  ) &
    type_of_establishment == "Total" &
    hospital_school == "Total") %>%
  # To combine the nursery, primary and secondary data, first pivot the data to be one row per LA/year
  pivot_wider(
    id_cols = c(region_name, la_name, time_period, geographic_level),
    names_from = phase_type_grouping, values_from = c(total_pupils, sen_support, ehc_plan)
  ) %>%
  # Ensure calculations are done by row
  rowwise() %>%
  # Add together the three totals for all three school types (using mutate as ftransform doesn't work with rowwise())
  mutate(
    total_pupils = sum(as.numeric(`total_pupils_State-funded secondary`),
      as.numeric(`total_pupils_State-funded primary`),
      as.numeric(`total_pupils_State-funded nursery`),
      na.rm = TRUE
    ),
    total_sensupport = sum(as.numeric(`sen_support_State-funded secondary`),
      as.numeric(`sen_support_State-funded primary`),
      as.numeric(`sen_support_State-funded nursery`),
      na.rm = TRUE
    ),
    total_ehc_plans = sum(as.numeric(`ehc_plan_State-funded secondary`),
      as.numeric(`ehc_plan_State-funded primary`),
      as.numeric(`ehc_plan_State-funded nursery`),
      na.rm = TRUE
    )
  ) %>%
  ftransform(
    ehc_percent = 100 * (total_ehc_plans / total_pupils), # Calculate EHC percentage
    sensupport_percent = 100 * (total_sensupport / total_pupils)
  ) %>% # Calculate SEN support percentage
  fselect(
    time_period,
    region_name,
    geographic_level,
    la_name,
    ehc_percent,
    sensupport_percent,
    total_pupils
  ) %>% # Get rid of unused variables
  pivot_longer(
    cols = c(ehc_percent, sensupport_percent),
    names_to = "SEN provision",
    values_to = "% of pupils"
  ) %>%
  mutate(`SEN provision` = recode(`SEN provision`,
    ehc_percent = "EHC plan",
    sensupport_percent = "SEN support"
  )) %>%
  time_period_to_academic_year() %>%
  mutate(`% of pupils` = round(`% of pupils`, 2)) %>%
  fsubset(academic_year >= "2017/18")

total_headcounts <- sen_phase_type %>%
  fsubset(type_of_establishment == "Total" &
    hospital_school == "Total" &
    phase_type_grouping == "Total") %>%
  fselect(time_period, geographic_level, region_name, la_name,
    "all_headcount" = total_pupils, "ehc_plan_headcount" = ehc_plan, "sen_support_headcount" = sen_support
  )





# Proportion of pupil population in AP and specialist settings
provider_types <- sen_phase_type %>%
  # Filter to total pupils and pupils in specialist settings
  fsubset(type_of_establishment == "Total" &
    hospital_school == "Total" &
    phase_type_grouping != "Total") %>%
  left_join(total_headcounts) %>% # Bring in total headcounts data for calculation of percentages
  fselect(-sen_support_percent, -ehc_plan_percent) %>% # To avoid confusion as these are %s within each provider type

  ftransform(
    total_pupils = as.numeric(total_pupils),
    ehc_plan = as.numeric(ehc_plan),
    sen_support = as.numeric(sen_support),
    all_headcount = as.numeric(all_headcount),
    sen_support_headcount = as.numeric(sen_support_headcount),
    ehc_plan_headcount = as.numeric(ehc_plan_headcount)
  ) %>%
  ftransform(
    all_percent = 100 * (total_pupils / all_headcount), # what % of all pupils go to this provision type
    ehc_plan_percent = 100 * (ehc_plan / ehc_plan_headcount), # what % of pupils with EHCPs go to this provision type
    sen_support_percent = 100 * (sen_support / sen_support_headcount)
  ) %>% # what % of pupils with SEN support go to this provision type
  time_period_to_academic_year() %>%
  pivot_longer(
    cols = c(all_percent, ehc_plan_percent, sen_support_percent),
    names_to = "Provision type",
    values_to = "percent"
  ) %>% # Bring the three percentages together under "Provision type"
  mutate(percent = round(percent, digits = 2)) %>%
  mutate(`Provision type` = recode(`Provision type`,
    all_percent = "all pupils",
    ehc_plan_percent = "pupils with EHC plans",
    sen_support_percent = "pupils on SEN support"
  )) %>%
  select(academic_year, time_identifier,
    geographic_level, region_name,
    la_name, new_la_code, phase_type_grouping,
    `Provision type`,
    "% of pupils (with SEN provision type)" = percent, academic_year
  ) %>%
  fsubset(academic_year >= "2017/18")


# The benchmarking graphs show a combined % in specialist provision, rather than split out AP from special and from independent.
# This next code creates a table that feeds those graphs
provider_types_grouped <- provider_types %>%
  mutate(`Grouped provider type` = ifelse(
    phase_type_grouping %in% c(
      "Independent school",
      "Pupil referral unit",
      "State-funded special school"
    ),
    yes = "Independent, alternative provision or special school",
    no = "Mainstream"
  )) %>%
  group_by(la_name, `Grouped provider type`, academic_year, `Provision type`, geographic_level, region_name) %>%
  summarise(`% in independent/AP/special` = sum(`% of pupils (with SEN provision type)`)) %>%
  # Having created a % figure for the new combined category we don't need the mainstream figures
  filter(`Grouped provider type` == "Independent, alternative provision or special school")

provider_types_nat <- provider_types %>% fsubset(geographic_level == "National")

provider_types_grouped_nat <- provider_types_nat %>%
  mutate(`Grouped provider type` = ifelse(
    phase_type_grouping %in% c(
      "Independent school",
      "Pupil referral unit",
      "State-funded special school"
    ),
    yes = "Independent, alternative provision or special school",
    no = "Mainstream "
  )) %>%
  group_by(la_name, `Grouped provider type`, academic_year, `Provision type`, geographic_level, region_name) %>%
  summarise(`% in independent/AP/special` = sum(`% of pupils (with SEN provision type)`)) %>%
  filter(`Grouped provider type` == "Independent, alternative provision or special school")


rm(sen_phase_type) # No longer need the big source file
gc() # Free up some RAM



### Autism Waiting Times
### Autism stats from here: https://digital.nhs.uk/supplementary-information/2022/autism-statistics-october-2021-to-september-2022-supplementary-information
# These two files have data for different months
AutismStatsDec22_Age <- rio::import("data/nhs/AutismStatsDec22_Age.csv.gz")
autism_supplementary <- rio::import("data/nhs/AutismStatsDec22_subICB.csv.gz")

autism_raw <- AutismStatsDec22_Age %>%
  bind_rows(autism_supplementary) %>%
  fsubset(PRIMARY_LEVEL != "UNKNOWN") %>% # Remove of data from unknown providers/commissioners
  fmutate(METRIC_VALUE = na_if(METRIC_VALUE, y = "*")) %>% # Remove censored data
  drop_na(METRIC_VALUE) %>% # Remove missing data
  # The overall metric is ASD19 but the various letter codes denote different age groups.
  # This combination seems to yield the least missing data (ASD19e for under 18s, ASD19g for over 18s)
  fsubset((METRIC == "ASD19e" & SECONDARY_LEVEL %in% c("Under 10", "10 to 17")) |
    (METRIC == "ASD19g" & SECONDARY_LEVEL == "18 to 24") |
    (METRIC == "ASD19e" & BREAKDOWN == "Age Group") |
    (METRIC == "ASD19g" & BREAKDOWN == "Age Group" & PRIMARY_LEVEL == "18 to 24")) %>% # Metric ASD19g is the same but for over 18s; secondary breakdown makes it 18-24 only
  ftransform(age_group = ifelse(test = BREAKDOWN == "Age Group",
    yes = paste0("Age: ", PRIMARY_LEVEL),
    no = paste0("Age: ", SECONDARY_LEVEL)
  )) %>% # If the breakdown is Age Group that won't be listed in secondary level, so need to bring it over.
  ftransform(
    `% with first appointment after more than 13 weeks` = as.numeric(METRIC_VALUE),
    `Age group` = factor(age_group, levels = c("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24"), ordered = TRUE)
  )


# left join in
autism <- autism_raw %>%
  left_join(nhs_lookup, by = c("PRIMARY_LEVEL" = "nhs_code")) %>%
  mutate(
    nhs_name = coalesce(nhs_name, PRIMARY_LEVEL_DESCRIPTION),
    nhs_type = coalesce(nhs_type, "Provider"),
    date = lubridate::dmy(REPORTING_PERIOD_START)
  ) %>%
  # Shorten the names of provider organisations to make the graph work better
  mutate(nhs_name = str_replace(nhs_name, "FOUNDATION TRUST", "FT")) %>%
  mutate(nhs_name = str_replace(nhs_name, "NHS TRUST", "NHST")) %>%
  mutate(nhs_name = str_replace(nhs_name, " AND ", " & ")) %>%
  mutate(nhs_name = str_replace(nhs_name, "SOUTH WEST", "SW"))

# Mental Health stats from MHSDS - "Time Series Data for selected MHSDS measures" CSV file
# https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics

mentalhealth_raw <- rio::import("data/nhs/MHSDS Time_Series_data_Apr_2016_NovPrf_2022.csv.gz") %>%
  fsubset(PRIMARY_LEVEL != "UNKNOWN" & # strip out "unknown" data
    MEASURE_ID == "MHS95" &
    BREAKDOWN %in% c("CCG of Residence", "Sub ICB of Residence", "England", "Region")) %>%
  ftransform(`Year ending` = lubridate::dmy(REPORTING_PERIOD_END))



mentalhealth <- mentalhealth_raw %>%
  mutate(BREAKDOWN2 = ifelse(BREAKDOWN %in% c("CCG of Residence", "Sub ICB of Residence"),
    yes = "CCG/Sub-ICB of Residence",
    no = BREAKDOWN
  )) %>% # Combine CCG and sub-ICB level, like the NHS PowerBI dashboard does
  left_join(nhs_lookup, by = c("PRIMARY_LEVEL" = "nhs_code")) %>% # to add region and make names formatted the same
  ftransform(`Number of children and young people` = as.numeric(MEASURE_VALUE)) %>%
  mutate(nhs_name = coalesce(nhs_name, PRIMARY_LEVEL_DESCRIPTION))



## CCG-level MH dataset
mentalhealth_ccg <- mentalhealth %>% fsubset(BREAKDOWN2 == "CCG/Sub-ICB of Residence")

most_recent_mentalhealth_label <- paste0(
  lubridate::month(max(mentalhealth_ccg$`Year ending`),
    label = TRUE, abbr = FALSE
  ),
  " ",
  lubridate::year(max(mentalhealth_ccg$`Year ending`))
)

year_ago_mentalhealth_label <- paste0(
  lubridate::month(max(mentalhealth_ccg$`Year ending`),
    label = TRUE, abbr = FALSE
  ),
  " ",
  lubridate::year(max(mentalhealth_ccg$`Year ending`)) - 1
)

# List of NHS regions for dropdown menu
nhs_region_list <- append("", mentalhealth %>%
  arrange(nhs_region) %>%
  drop_na(nhs_region) %>%
  pull(nhs_region) %>%
  unique())


local_authorities <- geojsonsf::geojson_sf("prep/Counties_and_Unitary_Authorities_(December_2022)_UK_BFC.geojson.gz") %>%
  sf::st_transform(crs = 27700)

sub_icb_locations <- geojsonsf::geojson_sf("prep/Sub_Integrated_Care_Board_Locations_(July_2022)_EN_BFC.geojson.gz") %>%
  sf::st_transform(crs = 27700)



# The below code gives a pure "which LAs overlap with which sub-ICB locations (CCGs)" answer
# However if a border is shared it counts this as overlap
# This results in too many matches being produced, including Scottish and Welsh LAs
#
# intersects_raw <- local_authorities %>%
#     sf::st_intersects(y = sub_icb_locations,
#                             sparse = TRUE)
#
# la_ccg_lookup_intersects <- intersects_raw %>%
#              as.data.frame() %>%
#              rename( "la_row" = row.id,
#                      "ccg_row" = col.id ) %>%
#   left_join(y = local_authorities,
#             by = c(la_row = "FID")) %>%
#   select(la_row, ccg_row, "la_name" = CTYUA22NM) %>%
#   left_join(y = sub_icb_locations,
#             by = c(ccg_row = "OBJECTID")) %>%
#   select(la_row, la_name, ccg_row, "subicb_long_name" = SICBL22NM) %>%
#   separate(col = "subicb_long_name",
#            into = c("icb_name", "nhs_code"),
#            sep = " - ") %>%
#   left_join(nhs_lookup)

# Function to shrink polygons, from stackoverflow

# NB size is given as a positive value
shrinkIfPossible <- function(sf, size) {
  # compute inward buffer
  sg <- sf::st_buffer(sf::st_geometry(sf), -size)

  # update geometry only if polygon is not degenerate
  sf::st_geometry(sf)[!sf::st_is_empty(sg)] <- sg[!sf::st_is_empty(sg)]

  # return updated dataset
  return(sf)
}


# Shrink the local authority boundaries by 100m to avoid excessive matches
la_geom_smaller <- shrinkIfPossible(local_authorities, 100)

intersects_raw_smaller <- la_geom_smaller %>%
  sf::st_intersects(
    y = sub_icb_locations,
    sparse = TRUE
  )

# create an LA to CCG lookup table
la_ccg_lookup <- intersects_raw_smaller %>%
  as.data.frame() %>%
  rename(
    "la_row" = row.id,
    "ccg_row" = col.id
  ) %>%
  left_join(
    y = local_authorities,
    by = c(la_row = "FID")
  ) %>%
  select(la_row, ccg_row, "la_name" = CTYUA22NM) %>%
  left_join(
    y = sub_icb_locations,
    by = c(ccg_row = "OBJECTID")
  ) %>%
  select(la_row, la_name, ccg_row, "subicb_long_name" = SICBL22NM) %>%
  separate(
    col = "subicb_long_name",
    into = c("icb_name", "nhs_code"),
    sep = " - "
  ) %>%
  left_join(nhs_lookup) %>%
  group_by(la_name) %>%
  mutate(count = n_distinct(nhs_name))

# manually add Northamptonshire, which has not been calculated because it no longer has any boundaries to calculate
# however it is obvious that since N and W Northamptonshire only overlap with NHS Northamptonshire CCG/ICB, Northamptonshire
# likewise will only overlap with it since it's the same area
Nhants <- la_ccg_lookup[la_ccg_lookup$la_name == "North Northamptonshire", ] %>% # Nhants will be identical
  mutate(la_name = "Northamptonshire")

la_ccg_lookup <- bind_rows(la_ccg_lookup, Nhants)

##### National stats for summary and England average lines

# Create a function to filter down to national level only as this will be done repeatedly
national_only <- function(df) {
  # Filter the dataframe
  df_filtered <- df[df$geographic_level == "National", ]

  # Assign the filtered dataframe to an object in the global environment
  return(df_filtered)
}

# and one to compare to the previous value for the same reason
# note that we can't rely on the previous value being the previous year (thanks Covid) which makes this more difficult
compare_to_previous <- function(df, col, time_column, ...) {
  column <- enquo(col)
  time_period <- enquo(time_column)
  df <- df %>%
    group_by(...) %>%
    arrange(!!time_period, .by_group = TRUE) %>% # may need another dynamic column in here if it turns out some dfs use different names
    mutate(pc_change = (100 * (!!column - lag(!!column)) / !!column)) %>%
    ungroup() # leaving the df grouped creates problems downstream

  return(df)
}

# Create england-only dataframes
eng_ks2_attainment <- national_only(ks2_attainment) %>% compare_to_previous(`Percent meeting expected standards`, time_period, characteristic)
eng_ks1_phonics <- national_only(ks1_phonics) %>% compare_to_previous(`Percent meeting expected standards in Y1`, time_period, characteristic)
eng_ks4_attainment <- national_only(ks4_attainment) # there's no previous data here to compare to currently - for unclear reasons P8 wasn't published in 2018/19
eng_ehcp_timeliness <- national_only(ehcp_timeliness) %>% compare_to_previous(`% of EHCPs issued within 20 weeks`, time_period)
eng_mentalhealth <- mentalhealth %>%
  fsubset(BREAKDOWN == "England") %>%
  compare_to_previous(`Number of children and young people`, `Year ending`)
eng_absence <- absence_regional %>%
  fsubset(region_name == "England") %>%
  compare_to_previous(`Overall absence %`, time_period, characteristic)
eng_tribunals <- national_only(tribunals) %>% compare_to_previous(`SEND Tribunal Appeal Rate`, year)
eng_dsg_deficit <- national_only(dsg_deficit) %>% compare_to_previous(`DSG cumulative balance as a % of the total budget`, time_period)
eng_percent_pupils_ehcp <- national_only(percent_pupils_ehcp) %>% compare_to_previous(`% of pupils`, time_period, `SEN provision`)
eng_autism <- autism %>%
  fsubset(BREAKDOWN == "Age Group") %>%
  compare_to_previous(`% with first appointment after more than 13 weeks`, date, nhs_name)
eng_ofsted <- count(ungroup(ofsted), `Inspection Outcome`)

# less straightforward summaries
# want an "all SEN" for this one which isn't there
eng_mainstream_with_sen <- national_only(mainstream_with_sen) %>%
  group_by(time_period, academic_year) %>%
  summarise(`% of pupils` = sum(`% of pupils`)) %>%
  mutate(`SEN provision` = "All SEN") %>%
  compare_to_previous(`% of pupils`, time_period)

eng_mainstream_with_sen_2 <- national_only(mainstream_with_sen) %>%
  compare_to_previous(`% of pupils`, time_period) %>%
  select(time_period, academic_year, `% of pupils`, `SEN provision`, pc_change)

eng_mainstream_with_sen <- bind_rows(eng_mainstream_with_sen, eng_mainstream_with_sen_2)
# There may be other metrics that can be calculated at England level

# grouping all specialist provider types
eng_provider_types <- national_only(provider_types) %>%
  fsubset(`Provision type` == "all pupils" &
    phase_type_grouping %in% c("Pupil referral unit", "Independent school", "Non-maintained special school")) %>% # specialist types
  group_by(academic_year) %>%
  fsummarise(`% of pupils (with SEN provision type)` = sum(`% of pupils (with SEN provision type)`)) %>%
  compare_to_previous(`% of pupils (with SEN provision type)`, academic_year)

source("prep/summary_prep.R")


save(ks2_attainment, ks1_phonics, ks4_attainment, # Initial Outcomes metrics
  mentalhealth, mentalhealth_ccg, most_recent_mentalhealth_label, year_ago_mentalhealth_label, # MH metrics
  ofsted, ofsted_data_updated, destinations_1618, destinations_1618_nat, # Final Outcomes metrics
  ehcp_timeliness, autism, tribunals, tribunals_reg, absence, absence_regional, ks4_destinations, ks4_destinations_nat, # Experiences metrics
  dsg_deficit, specialist_spend, # Financial Sustainability metrics
  ehcp_ageprofile, mainstream_with_sen, percent_pupils_ehcp, # Identification of Need metrics
  provider_types, provider_types_grouped, provider_types_nat, provider_types_grouped_nat, # specialist provider types metric
  nhs_region_list, la_region_lookup, nhs_lookup, la_ccg_lookup, # Lookups
  eng_absence, eng_autism, eng_dsg_deficit, eng_ehcp_timeliness, eng_ks1_phonics, eng_ks2_attainment, eng_ks4_attainment,
  eng_mentalhealth, eng_percent_pupils_ehcp, eng_tribunals, eng_mainstream_with_sen, eng_provider_types, eng_ofsted, summary_metrics,
  # box_ks2_attainment, sparkline_ks2_attainment,
  file = "data/prepared_data.Rdata"
)
