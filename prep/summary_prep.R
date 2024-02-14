# This script needs to run every time any data is updated, so it's sourced in data_preparation.R
# It generates the summary graphic for each LA so they are ready for the dashboard to call on,
# rather than processing all the metrics on-the-fly.
# Note that it does not (currently) do anything relating to the England and regions summary

library(dplyr)
library(tidyr)
library(collapse)

# Create function to add rank numbers to an LA benchmarking dataframe, specifying the outcome to be ranked by

add_ranks_and_select <- function(df, metric_name, detail, outcome, year = time_period) {
  out <- sym(outcome)
  df <- df %>%
    drop_na(!!outcome) %>%
    ungroup() %>%
    group_by({{ year }})
  df <- arrange(df, desc(df[[outcome]]))
  df <- df %>%
    mutate(
      rank = row_number(),
      metric = metric_name,
      detail = detail
    ) %>%
    select(la_name, metric, detail, rank, {{ year }}, {{outcome}}) %>%
    ungroup() %>%
    arrange(desc({{ year }})) %>%
    group_by(la_name, metric, detail) %>%
    summarise(
      min_rank = min(rank),
      max_rank = max(rank),
      mean_rank = first(rank), 
      Outcome = first(!!out)
    )
}

ccg_add_ranks_and_select <- function(df, metric_name, detail, outcome) {
  df <- df %>%
    drop_na(!!outcome)
  df <- arrange(df, desc(df[[outcome]]))
  df <- df %>%
    mutate(
      rank = row_number(),
      metric = metric_name,
      detail = detail
    ) %>%
    select(nhs_name, metric, detail, rank, {{outcome}})
}

#count how many LAs had data, for AP where the answer is not just "150"
outof <- function(df, rank = mean_rank){
  df <- df %>% 
    mutate(out_of = if_else(is.na(mean_rank), NA_integer_, max(df$mean_rank)))
  
  return(df)
}

# in general, you do not want to do this if sourcing the script from data_preparation.R, since in this case any changes
# made by that script will be overwritten. If running this by itself however this has to be uncommented to load the data in
# load("data/prepared_data.Rdata")


# Process data inputs for inclusion in summary plot

## KS2 attainment (switchable)
summary_ks2_attainment <- ks2_attainment %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
  # time_period == max(time_period)) %>%
  split(.$characteristic) %>% 
  map(
  add_ranks_and_select, 
  "KS2 attainment",  # metric short name
  "% meeting expected standard at KS2", # metric detail 
  "Percent meeting expected standard", # outcome column
  time_period) %>%  # time column
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>% # this adds SEN status back in as a column since split() removes it
  bind_rows()


## EYFSP (switchable)
summary_eyfsp <- eyfsp %>%
  collapse::fsubset(geographic_level == "Local authority" & 
                    characteristic_type != "Unclassified") %>%
  # time_period == max(time_period)) %>%
  split(.$characteristic) %>% 
  map(
    add_ranks_and_select, 
    "EYFSP",  # metric short name
    "% with a good level of development", # metric detail 
    "gld_percentage", # outcome column
    time_period) %>%  # time column
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>% 
  bind_rows()

## KS1 phonics (switchable)
summary_ks1_phonics <- ks1_phonics %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
  # time_period == max(time_period)) %>%
  split(.$characteristic) %>% 
  map(
    add_ranks_and_select, 
    "Phonics screening check",  # metric short name
    "% meeting phonics standards in Y1", # metric detail 
    "Percent meeting expected standards in Y1", # outcome column
    time_period) %>%  # time column
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>% 
  bind_rows()


# KS4 Attainment (All SEN because EHCP for Prog8 doesn't really work)
summary_ks4_attainment <- ks4_attainment %>%
  collapse::fsubset(geographic_level == "Local authority" & 
                    `SEN provision` == "All SEN" & 
                     time_period == "202223" & # this will need manually updating, but the idea is we only want one year in the LA summary
                     !(la_name %in% small_LAs)) %>%
  add_ranks_and_select(
    outcome = "Average progress 8 score",
    metric_name = "KS4 attainment",
    detail = "Average progress 8 score (All SEN)"
  ) %>% 
  mutate(sen_status = "Not switchable")

# 16-18 destinations (Identified SEN / Identified LLDD)  #LLDD has higher numbers so choosing that breakdown
# % not sustained
summary_destinations_1618 <- destinations_1618 %>%
  collapse::fsubset(geographic_level == "Local authority" &
    characteristic == "Identified LLDD (mainstream)" &
    # time_period == max(time_period) &
    destination_raw == "all_notsust") %>%
  add_ranks_and_select(
    outcome = "% of pupils",
    metric_name = "16-18 destinations",
    detail = "% of pupils with self-identified learning difficulty, disability or physical illness in a non-sustained destination after 16-18 study"
  ) %>% 
  mutate(sen_status = "Not switchable")

summary_disco <- discontinued_plans %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
  add_ranks_and_select(
    outcome = "discontinued_schoolage",
    metric_name = "Discontinued EHC plans",
    detail = "Number of Education, Health and Care plans discontinued as needs met without a plan, for children of school age"
  ) %>% 
mutate(sen_status = "Not switchable")

# EHCP timeliness
summary_timeliness <- ehcp_timeliness %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
  mutate(year = as.numeric(year)) %>% 
  # time_period == max(time_period)) %>%
  add_ranks_and_select(
    outcome = "% of EHCPs issued within 20 weeks",
    metric_name = "EHCP timeliness",
    detail = "% of EHCPs issued within 20 weeks"
  ) %>% 
  mutate(sen_status = "Not switchable")

# Tribunal appeal rate
summary_tribunals <- tribunals %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
  # year == max(year)) %>%
  add_ranks_and_select(
    outcome = "SEND Tribunal Appeal Rate",
    metric_name = "Tribunal appeal rate",
    detail = "Rate of appeals to SEND Tribunal",
    year = year
  )  %>% 
  mutate(sen_status = "Not switchable")


# Absence (switchable)
summary_absence <- absence %>%
  mutate(sen = recode(characteristic,
    "EHCP or Statement" = "EHC plan"
  )) %>%
  collapse::fsubset(geographic_level == "Local authority" &
    `Absence measure` == "Overall") %>%
  # time_period == max(time_period)) %>%
  split(.$sen) %>% 
  map(add_ranks_and_select,
    "Absence",
    "Overall absence %", 
    "Percentage", 
    time_period
  )  %>% 
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>% 
  bind_rows()

# KS4 destinations
summary_ks4_destinations <- ks4_destinations %>%
  mutate(sen = case_match(characteristic,
    "EHC plan or Statement" ~ "EHC plan",
    "Identified SEN" ~ "All SEN", 
    .default = characteristic
  )) %>%
  collapse::fsubset(geographic_level == "Local authority" &
    destination_raw == "all_notsust") %>%
  # time_period == max(time_period)) %>%
  split(.$sen) %>% 
  map(add_ranks_and_select,
      "KS4 Destinations",
      "% of pupils in a non-sustained destination after KS4", 
      "% of pupils", 
      time_period
  )  %>% 
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>% 
  bind_rows()

# DSG deficit
summary_dsg_deficit <- dsg_deficit %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
  # time_period == max(time_period)) %>%
  add_ranks_and_select(
    outcome = "DSG cumulative balance as a % of the total income",
    metric_name = "DSG cumulative balance",
    detail = "DSG cumulative balance as a % of the total income"
  ) %>% 
  mutate(sen_status = "Not switchable")

# Autism waiting times (REMOVED - different scale since more LAs than CCGs)
# summary_autism <- autism %>%
#   collapse::fsubset(nhs_type == "Former CCG area" &
#                       age_group == "Age: 10 to 17") %>%
#   collapse::fsubset(date == max(date)) %>%
#   ccg_add_ranks_and_select(outcome = "% with first appointment after more than 13 weeks",
#                            metric_name = "Autism waiting times",
#                            detail = "% of 'suspected autism' referrals for children aged 10-17 with first appointment after more than 13 weeks")

# Specialist spend
summary_specialist_spend <- specialist_spend %>%
  collapse::fsubset( # year == max(year) &
    category == "Independent or non-maintained"
  ) %>%
  add_ranks_and_select(
    outcome = "Spend per head",
    metric_name = "Specialist spend per head",
    detail = "Spend per head on special schools and AP in the independent or non-maintained sector",
    year = year
  ) %>% 
  mutate(sen_status = "Not switchable")

# SEN provision
summary_percent_pupils_ehcp <- percent_pupils_ehcp %>%
  collapse::fsubset(geographic_level == "Local authority" &
    # time_period == max(time_period) &
    `SEN provision` == "EHC plan") %>%
  add_ranks_and_select(
    outcome = "% of pupils",
    metric_name = "% of pupils with EHC plans",
    detail = "% of pupils who have Education, Health and Care plans"
  ) %>% 
  mutate(sen_status = "Not switchable")

# Pupils in mainstream with SEN
summary_mainstream_with_sen <- mainstream_with_sen %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
      split(.$`SEN provision`) %>% 
  map(add_ranks_and_select, 
    "Pupils in mainstream with SEN",
    "% of pupils in mainstream schools who have EHC plans",
    "% of pupils",
    time_period
  ) %>% 
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>%
  bind_rows()

# Pupils in specialist
summary_provider_types <- provider_types_grouped %>%
  mutate(sen = recode(`Provision type`,
    "pupils with EHC plans" = "EHC plan",
    "pupils on SEN support" = "SEN support"
  )) %>%
  fsubset(geographic_level == "Local authority" &
    sen == "EHC plan") %>%
  # academic_year == max(academic_year)) %>%
  ungroup() %>%
  add_ranks_and_select(
    outcome = "% in independent/AP/special",
    metric_name = "Pupils in specialist settings",
    detail = "% of pupils with EHC plans who are in independent/AP/special settings",
    year = academic_year
  ) %>% 
  mutate(sen_status = "Not switchable")

# CIN Pupils with SEN
summary_cin_with_sen <- cin_la %>%
  collapse::fsubset(geographic_level == "Local authority" & 
                    social_care_group == "CINO at 31 March" ) %>%
  split(.$`SEN Provision`) %>% 
  map(
  add_ranks_and_select,
  "Children in need with SEN",
  "% of pupils in mainstream schools who have EHC plans",
  "Percentage of children",
  time_period
  ) %>% 
  map2(.y = names(.), .f = \(x, y) mutate(x, sen_status = y)) %>% 
  bind_rows()

# Number of pupils at state-funded AP
summary_ap_counts_sf <- ap_counts %>% 
  collapse::fsubset(geographic_level == "Local authority" & 
                    prov_type == "State-funded AP school") %>% 
  add_ranks_and_select("Number of pupils at state-funded AP", 
                       "Total number of pupils in state-funded Alternative Provision",
                       "Total", 
                       time_period) %>% 
  mutate(sen_status = "Not switchable")

summary_ap_counts_sf <- outof(summary_ap_counts_sf)

# Number of pupils at school-arranged AP
summary_ap_counts_sa <- ap_counts %>% 
  collapse::fsubset(geographic_level == "Local authority" & 
                      prov_type == "School arranged AP") %>% 
  add_ranks_and_select("Number of pupils at school-arranged AP", 
                       "Total number of pupils in school-arranged Alternative Provision",
                       "Total", 
                       time_period) %>% 
  mutate(sen_status = "Not switchable")

summary_ap_counts_sa <- outof(summary_ap_counts_sa)

# Number of placements at LA funded AP
summary_ap_counts_la <- ap_counts %>% 
  collapse::fsubset(geographic_level == "Local authority" & 
                      prov_type == "LA funded AP placements") %>% 
  add_ranks_and_select("Number of pupils at LA-funded AP", 
                       "Total number of pupils in LA-funded Alternative Provision",
                       "Total", 
                       time_period) %>% 
  mutate(sen_status = "Not switchable")

summary_ap_counts_la <- outof(summary_ap_counts_la)

# Number of pupils at school-arranged unregistered AP
summary_ap_counts_sa_u <- uap_counts %>% 
  collapse::fsubset(geographic_level == "Local authority" & 
                      prov_type == "School arranged unregistered AP pupils") %>% 
  group_by(time_period, la_name) %>% 
  summarise("Total" = sum(Total)) %>% 
  add_ranks_and_select("Number of pupils at unregistered school-arranged AP", 
                       "Total number of pupils in unregistered school-arranged Alternative Provision",
                       "Total", 
                       time_period) %>% 
  mutate(sen_status = "Not switchable")

summary_ap_counts_sa_u <- outof(summary_ap_counts_sa_u)

# Number of placements at LA funded unregisered AP
summary_ap_counts_la_u <- uap_counts %>% 
  collapse::fsubset(geographic_level == "Local authority" & 
                      prov_type == "LA funded unregistered AP placements") %>% 
  group_by(time_period, la_name) %>% 
  summarise("Total" = sum(Total)) %>% 
  add_ranks_and_select("Number of pupils at unregistered LA-funded AP", 
                       "Total number of pupils in unregistered LA funded Alternative Provision placements",
                       "Total", 
                       time_period) %>% 
  mutate(sen_status = "Not switchable")

summary_ap_counts_la_u <- outof(summary_ap_counts_la_u)

# % any SEN at state-funded AP
summary_ap_characteristics <- ap_characteristics %>% 
  collapse::fsubset(geographic_level == "Local authority" & 
                    prov_type == "State-funded AP school" &
                    Characteristic %in% c("All SEN",
                                          "EHC plan",
                                          "SEN Support")) %>%
  mutate(Characteristic = case_when(Characteristic %in% "SEN Support" ~ "SEN support",
                                    T ~ Characteristic)) %>%
  split(.$Characteristic) %>% 
  map(
    add_ranks_and_select,
    "% SEN in state-funded AP",
    "% of pupils in state-funded AP who have the selected SEN type",
    "% of pupils",
    time_period
  ) %>% 
  map2(.y = names(.), .f = \(x, y) mutate(x, Characteristic = y, out_of = if_else(is.na(mean_rank), NA_integer_, max(x$mean_rank)))) %>% 
  bind_rows()

# % any absence at state-funded AP
summary_ap_absence <- sf_ap_absence %>% 
  fsubset(geographic_level == "Local authority" &
          `Absence measure` == "Overall absence %") %>% 
  add_ranks_and_select("Overall absence %", 
                       "Overall absence rate in state-funded alternative provision",
                       "Percentage", 
                       time_period) %>% 
  mutate(sen_status = "Not switchable")

summary_ap_absence <- outof(summary_ap_absence)

# % good or outstanding state-funded Ofsted rating

# we're not actually using this - code was written in an exploratory manner but there's something like a 90-way tie for first with 
# 100% of schools good or outstanding so it's fairly meaningless 

# summary_ap_ofsted <- ap_ofsted_schl %>% 
#  fsubset(geographic_level == "Local authority" & 
#            school_type == "State-funded AP school" &
#          Measure == "Overall effectiveness (% of schools)") %>% 
#  fmutate(Mask = if_else(`Overall effectiveness` %in% c("Outstanding", "Good"), TRUE, FALSE)) %>% 
#  group_by(Year, Mask, la_name) %>% 
#  summarise(grand_total_schools = sum(grand_total_schools), 
#            Value = sum(Value)) %>% 
#  ungroup() %>% 
#  fsubset(Mask == TRUE) %>% 
#  add_ranks_and_select("% of Good/Outstanding AP schools", 
#                       "Percentage of AP schools rated Outstanding or Good by Ofsted", 
#                       "Value", 
#                       Year) %>% 
#  mutate(sen_status = "Not switchable")

# List the tables to combine
tables_to_bind <- list(
  summary_absence,
  summary_cin_with_sen,
  summary_destinations_1618,
  summary_disco,
  summary_dsg_deficit,
  summary_eyfsp,
  summary_ks1_phonics,
  summary_ks2_attainment,
  summary_ks4_attainment,
  summary_ks4_destinations,
  #   summary_autism, #Removed since there are fewer CCGs than local authorities so it would be misleading
  summary_percent_pupils_ehcp,
  summary_provider_types,
  summary_mainstream_with_sen,
  summary_specialist_spend,
  summary_timeliness,
  summary_tribunals
)

ap_tables <- list(
  summary_ap_characteristics, 
  summary_ap_counts_la, 
  summary_ap_counts_sa, 
  summary_ap_counts_la_u, 
  summary_ap_counts_sa_u, 
  summary_ap_absence, 
  summary_ap_counts_sf
)
outcomes_metrics <- c("Phonics screening check", "KS2 attainment", "KS4 attainment", "16-18 destinations", "EYFSP", "Discontinued EHC plans")
experiences_metrics <- c("EHCP timeliness", "Tribunal appeal rate", "Absence", "KS4 Destinations")
financialsustainability_metrics <- c("DSG cumulative balance", "Specialist spend per head")
identificationofneed_metrics <- c("% of pupils with EHC plans", "Pupils in mainstream with SEN", "Pupils in specialist settings", "Children in need with SEN")

# Combine tables into one
summary_metrics <- bind_rows(tables_to_bind) %>%
  # Add theme variable
  mutate(Theme = case_when(
    metric %in% outcomes_metrics ~ "Outcomes",
    metric %in% experiences_metrics ~ "Experiences",
    metric %in% financialsustainability_metrics ~ "Financial Sustainability",
    metric %in% identificationofneed_metrics ~ "Identification of Need",
    TRUE ~ "Error: theme not found"
  )) %>%
  mutate(Theme = factor(Theme, ordered = TRUE, levels = c(
    "Outcomes",
    "Experiences",
    "Financial Sustainability",
    "Identification of Need"
  )))

# separate table for AP due to some LAs not having APs within them meaning it's not out of 150. 
ap_summary_metrics <- bind_rows(ap_tables) %>% 
  mutate(Theme = "Alternative Provision")