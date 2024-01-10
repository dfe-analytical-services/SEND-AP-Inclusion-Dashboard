# This script checks how many rows of data each metric has per year to see if anything is missing

# The number of rows education data should have is:
# 1 row for national (England)
# 8 rows for regional (London, SE, SW, E Midlands, W Midlands, Yorks & Humber, NE, NW)
# 149-151 LAs depending on year:
# In 2019 Bournemouth and Poole merged and annexed Christchurch from Dorset to form Bournemouth, Christchurch and Poole, decreasing the number of LAs by one
# In 2021 Northamptonshire split into North and West Northamptonshire, increasing the number of LAs by one
# In 2023 Cumbria split into Northumberland and Westmoreland and Furness (but this isn't in the dashboard yet so we're not checking for it)
# So in theory 158-160 rows in total

# There will be more rows in practice because of individual rows for different SEN types, measures etc
# BUT we don't care, because there'll always be those rows for England too - so all we need to check is the ratio England:Regions:LAs
# This will sometimes require combining the data frames if there's separate ones for England/Regions/LAs

# libraries
library(tidyverse)
library(collapse)
library(data.table)

# function - here we're assuming there's a "geographic level" column because I think all DFE data has it
# similarly we're assuming the LA name field is called la_name for the same reason
# but this is easy to parameterise later if necessary you just do what I've done with the time columne
rows_per_year <- function(df, time_column = time_period) {
  time_column <- enquo(time_column)
  entries_table <- ungroup(df) %>%
    filter(!la_name %in% small_LAs) %>%
    count(!!time_column, geographic_level) %>%
    pivot_wider(names_from = geographic_level, values_from = n) %>%
    # calc row ratios
    mutate(
      regions_per_eng = Regional / National,
      LAs_per_eng = `Local authority` / National
    )

  return(entries_table)
}

# some data needs some preparation - usually combining national/regional/local levels
# if the data doesn't have geographic level in it we have to do something like this otherwise we can usually just bind_rows()
combine_frames <- function(nat = NULL, reg = NULL, la = NULL, time = year) {
  time_column <- enquo(time)
  if (!is.null(nat)) {
    nat <- nat %>%
      mutate(
        geographic_level = "National",
        region_name = "England",
        la_name = NA_character_
      )
  }
  if (!is.null(reg)) {
    reg <- reg %>%
      mutate(
        geographic_level = "Regional",
        la_name = NA_character_
      )
  }
  if (!is.null(la) & !("geographic_level" %in% names(la))) { # only add the column if it's not already there - sometimes the "la" frame has national/regional data in it but not the other one
    la <- la %>%
      mutate(geographic_level = "Local authority")
  }
  out <- bind_rows(nat, reg, la) %>% # fortunately you can bind_rows a df with NULL so this works fine
    mutate(time_period = !!time_column)

  return(out)
}

destinations_1618_all <- bind_rows(destinations_1618, destinations_1618_nat) %>%
  fsubset(characteristic != "Specialist provision") # because they're only at national level so it screws up the ratios
tribunals_all <- combine_frames(nat = NULL, la = tribunals, reg = tribunals_reg)
# ks4 is particularly annoying as for some reason the numeric fields are strings only in the LA-level preventing a bind_rows
ks4_dest_check <- ks4_destinations %>%
  mutate(
    cohort = as.integer(cohort),
    education = as.numeric(education)
  )
ks4_dest_all <- bind_rows(ks4_dest_check, ks4_destinations_nat)
specialist_sp_all <- combine_frames(la = specialist_spend, reg = reg_specialist_spend, nat = nat_specialist_spend)
provider_types_all <- provider_types %>%
  mutate(time_period = academic_year)
provider_types_all_g <- provider_types_grouped %>%
  mutate(time_period = academic_year)
ap_absence_all <- bind_rows(sf_ap_absence, sf_ap_absence_regional)
ap_ofsted_check <- ap_ofsted_schl %>%
  mutate(time_period = Year)
# list of data frames to check (stolen from data_preparation.R)
metrics <- list(
  ks2_attainment, ks1_phonics, ks4_attainment, # Initial Outcomes metrics
  destinations_1618_all, discontinued_plans, eyfsp, # Final Outcomes metrics
  ehcp_timeliness, tribunals_all, absence, ks4_dest_all, # Experiences metrics
  dsg_deficit, specialist_sp_all, # Financial Sustainability metrics
  ehcp_ageprofile, mainstream_with_sen, cin_la, percent_pupils_ehcp, # Identification of Need metrics
  provider_types_all, provider_types_all_g, # specialist provider types metric
  ap_counts, uap_counts, ap_characteristics, sf_ap_absence, ap_ofsted_check
) # AP metrics

# name the list because it makes debugging and some later operations easier
names(metrics) <- c(
  "ks2_attainment", "ks1_phonics", "ks4_attainment", # Initial Outcomes metrics
  "destinations_1618_all", "discontinued_plans", "eyfsp", # Final Outcomes metrics
  "ehcp_timeliness", "tribunals_all", "absence", "ks4_dest_all", # Experiences metrics
  "dsg_deficit", "specialist_sp_all", # Financial Sustainability metrics
  "ehcp_ageprofile", "mainstream_with_sen", "cin_la", "percent_pupils_ehcp", # Identification of Need metrics
  "provider_types_all", "provider_types_all_g", # specialist provider types metric
  "ap_counts", "uap_counts", "ap_characteristics", "sf_ap_absence", "ap_ofsted_check"
)
row_check <- map(metrics, rows_per_year) %>%
  map2(.y = names(.), .f = \(x, y) mutate(x, time_period = as.character(time_period), df_name = y)) %>%
  bind_rows()
