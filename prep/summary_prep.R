# This script needs to run every time any data is updated, so it's sourced in data_preparation.R
# It generates the summary graphic for each LA so they are ready for the dashboard to call on,
# rather than processing all the metrics on-the-fly.
# Note that it does not (currently) do anything relating to the England and regions summary

library(tidyverse)
library(collapse)

#Create function to add rank numbers to an LA benchmarking dataframe, specifying the outcome to be ranked by

add_ranks_and_select <- function(df, metric_name, detail, outcome, year = time_period) {
  df <- df %>%
    drop_na(!!outcome) %>%
    ungroup() %>%
    group_by({{year}})
  df <- arrange(df, desc(df[[outcome]]))
  df <- df %>%
    mutate(rank = row_number(),
           metric = metric_name,
           detail = detail) %>% 
    select(la_name, metric, detail, rank, {{year}}) %>%
    ungroup() %>%
    arrange({{year}}) %>%
    group_by(la_name, metric, detail) %>%
    summarise(min_rank = min(rank),
           max_rank = max(rank),
           mean_rank = first(rank))
}

ccg_add_ranks_and_select <- function(df, metric_name, detail, outcome) {
  df <- df %>%
    drop_na(!!outcome)
  df <- arrange(df, desc(df[[outcome]]))
  df <- df %>%
    mutate(rank = row_number(),
           metric = metric_name,
           detail = detail) %>% 
    select(nhs_name, metric, detail, rank)}


#in general, you do not want to do this if sourcing the script from data_preparation.R, since in this case any changes
#made by that script will be overwritten. If running this by itself however this has to be uncommented to load the data in
#load("data/prepared_data.Rdata")


#Process data inputs for inclusion in summary plot

## KS2 attainment (EHC plan / All SEN / SEN support)
summary_ks2_attainment <- ks2_attainment %>%
  collapse::fsubset(geographic_level == "Local authority" &
                      characteristic ==  "All SEN") %>%
                      #time_period == max(time_period)) %>% 
  add_ranks_and_select(outcome = "Percent meeting expected standards",
                       metric_name = "KS2 attainment",
                       detail = "% meeting expected standards at KS2 (All SEN)", 
                       year = time_period)


## KS1 phonics (EHC plan / SEN support)
summary_ks1_phonics <- ks1_phonics_allSEN %>%
  collapse::fsubset(geographic_level == "Local authority" &
                      characteristic == "All SEN") %>%
                      #time_period == max(time_period)) %>% 
  add_ranks_and_select(outcome = "Percent meeting expected standards in Y1",
                       metric_name = "KS1 phonics",
                       detail = "% meeting expected phonics standards in Y1 (All SEN)") 
  

#KS4 Attainment (EHC plan / SEN support)
summary_ks4_attainment <- ks4_attainment %>%
  collapse::fsubset(geographic_level == "Local authority" &
                      #time_period == max(time_period) &
                      `SEN provision` == "Any SEN") %>% 
  add_ranks_and_select(outcome = "Average progress 8 score",
                       metric_name = "KS4 attainment",
                       detail = "Average progress 8 score (Any SEN)") 

#16-18 destinations (Identified SEN / Identified LLDD)  #LLDD has higher numbers so choosing that breakdown
#% not sustained
summary_destinations_1618 <- destinations_1618 %>%
  collapse::fsubset(geographic_level == "Local authority" &
                      characteristic == "Identified LLDD" &
                      #time_period == max(time_period) &
                      destination_raw == "all_notsust") %>% 
  add_ranks_and_select(outcome = "% of pupils",
                       metric_name = "16-18 destinations",
                       detail = "% of pupils with self-identified learning difficulty, disability or physical illness in a non-sustained destination after 16-18 study")

#EHCP timeliness
summary_timeliness <- ehcp_timeliness %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
                      #time_period == max(time_period)) %>% 
  add_ranks_and_select(outcome = "% of EHCPs issued within 20 weeks",
                       metric_name = "EHCP timeliness",
                       detail = "% of EHCPs issued within 20 weeks")

#Tribunal appeal rate
summary_tribunals <- tribunals %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
                      #year == max(year)) %>% 
add_ranks_and_select(outcome = "SEND Tribunal Appeal Rate",
                     metric_name = "Tribunal appeal rate",
                     detail = "Rate of appeals to SEND Tribunal", 
                     year = year)

#Absence (EHCP/SEN support)
summary_absence <- absence %>%
  mutate(sen = recode(characteristic,
                      "EHCP or Statement" = "EHC plan")) %>% 
  collapse::fsubset(geographic_level == "Local authority" &
                      sen == "EHC plan") %>%
                      #time_period == max(time_period)) %>% 
  add_ranks_and_select(outcome = "Overall absence %",
                       metric_name = "Absence",
                       detail = "Overall absence % (EHC plans)")

#KS4 destinations
summary_ks4_destinations <- ks4_destinations %>%
  mutate(sen = recode(characteristic, 
                      "EHC plan or Statement" = "EHC plan",
                      "Identified SEN" = "All SEN")) %>% 
  collapse::fsubset(geographic_level == "Local authority" &
                      sen == "EHC plan" &
                      destination_raw == "all_notsust") %>%
                     # time_period == max(time_period)) %>% 
  add_ranks_and_select(outcome = "% of pupils",
                       metric_name = "KS4 destinations",
                       detail = "% of pupils in a non-sustained destination after KS4")

#DSG deficit
summary_dsg_deficit <- dsg_deficit %>%
  collapse::fsubset(geographic_level == "Local authority") %>%
                      #time_period == max(time_period)) %>% 
  add_ranks_and_select(outcome = "DSG cumulative balance as a % of the total budget",
                       metric_name = "DSG cumulative balance",
                       detail = "DSG cumulative balance as a % of the total budget")

#Autism waiting times (REMOVED - different scale since more LAs than CCGs)
# summary_autism <- autism %>%
#   collapse::fsubset(nhs_type == "Former CCG area" & 
#                       age_group == "Age: 10 to 17") %>% 
#   collapse::fsubset(date == max(date)) %>% 
#   ccg_add_ranks_and_select(outcome = "% with first appointment after more than 13 weeks",
#                            metric_name = "Autism waiting times",
#                            detail = "% of 'suspected autism' referrals for children aged 10-17 with first appointment after more than 13 weeks")

#Specialist spend
summary_specialist_spend <- specialist_spend %>%
  collapse::fsubset(#year == max(year) &
                     category == "Independent or non-maintained") %>% 
  add_ranks_and_select(outcome = "Spend per head",
                       metric_name = "Specialist spend per head",
                       detail = "Spend per head on special schools and AP in the independent or non-maintained sector", 
                       year = year)

#SEN provision
summary_percent_pupils_ehcp <- percent_pupils_ehcp %>%
  collapse::fsubset(geographic_level == "Local authority" &
                      #time_period == max(time_period) &
                      `SEN provision` == "EHC plan") %>% 
add_ranks_and_select(outcome = "% of pupils",
                     metric_name = "% of pupils with EHC plans",
                     detail = "% of pupils who have Education, Health and Care plans")

#Pupils in mainstream with SEN
summary_mainstream_with_sen <- mainstream_with_sen %>%
  collapse::fsubset(geographic_level == "Local authority" &
                      #time_period == max(time_period) &
                      `SEN provision` == "EHC plan") %>% 
  add_ranks_and_select(outcome = "% of pupils",
                       metric_name = "Pupils in mainstream with SEN",
                       detail = "% of pupils in mainstream schools who have EHC plans")

#Pupils in specialist
summary_provider_types <- provider_types_grouped %>%
  mutate(sen = recode(`Provision type`,
                      "pupils with EHC plans" = "EHC plan",
                      "pupils on SEN support" = "SEN support")) %>% 
  fsubset(geographic_level == "Local authority" &
            sen == "EHC plan") %>% 
            #academic_year == max(academic_year)) %>% 
  ungroup() %>% 
  add_ranks_and_select(outcome = "% in independent/AP/special",
                       metric_name = "Pupils in specialist settings",
                       detail = "% of pupils with EHC plans who are in independent/AP/special settings", 
                       year = academic_year)


#List the tables to combine
tables_to_bind <- list(summary_absence,
     summary_destinations_1618,
     summary_dsg_deficit,
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
     summary_tribunals)

outcomes_metrics <- c("KS1 phonics", "KS2 attainment", "KS4 attainment", "16-18 destinations")
experiences_metrics <- c("EHCP timeliness", "Tribunal appeal rate", "Absence", "KS4 destinations")
financialsustainability_metrics <- c("DSG cumulative balance", "Specialist spend per head")
identificationofneed_metrics <- c("% of pupils with EHC plans", "Pupils in mainstream with SEN", "Pupils in specialist settings")

#Combine tables into one
 summary_metrics <- bind_rows(tables_to_bind) %>% 
#Add theme variable
   mutate(Theme = case_when(metric %in% outcomes_metrics ~ "Outcomes",
                           metric %in% experiences_metrics  ~ "Experiences",
                           metric %in% financialsustainability_metrics ~ "Financial Sustainability",
                           metric %in% identificationofneed_metrics ~ "Identification of Need",
                           TRUE ~ "Error: theme not found")) %>% 
   mutate(Theme = factor(Theme, ordered = TRUE, levels = c("Outcomes",
                                                           "Experiences",
                                                           "Financial Sustainability",
                                                           "Identification of Need")))
  

