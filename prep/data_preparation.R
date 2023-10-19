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
library(janitor)
library(openxlsx)
library(rvest)
library(httr)
library(stringr)
library(RCurl)
library(gsubfn)
library(forcats)

#==============================
# Pre-step
#==============================



# Run the file which requires some manual changes 
source("prep/data_preparation_manual_changes_required.R")

print("Although this file automates the downloading of data where possible, when updating you should progress step by step to check for key possible issues e.g., the underlying file changes name, stucture, or where it's published")


#------------------------------
# NHS general inputs 
#------------------------------

ymc_date_format <- "%Y-%m-%d"
months <- c(1:12)
current_year <- as.numeric(format(Sys.Date(), "%Y"))  
nhs_years <- c(current_year-1, current_year) # because in January, you want last year's publication 

#function that fixes capitalisation in some NHS records
simpleCap <- function(x) {
  s <- strsplit(tolower(x), " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#==============================
# OUTCOME INDICATORS
#==============================

#------------------------------
# Phonics (KS1) attainment (webscraping used)
#------------------------------

# url we want
phonics_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-1-and-phonics-screening-check-attainment"
# Extract the year the publication refers to
phonics_year_of_pub <- read_html(phonics_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
phonics_yr_start <- "2012" # Hardcoded as we publish since 2012
phonics_yr_end <- paste0("20",substr(phonics_year_of_pub, start = nchar(phonics_year_of_pub) -1, stop = nchar(phonics_year_of_pub) - 0))
# State the file with correct year we want
phonics_file_name <- paste0("phonics_y1_regional_and_local_authority_",phonics_yr_start,"_to_",phonics_yr_end, "_provisional",".csv")


# Save latest phonics data 
get_ees_data(
  url = phonics_url, 
  desired_file_name <- phonics_file_name,
  zip_subfolder_to_extract <- "data/", 
  output_dir <- "data/phonics"
)

# Import and wrangle the data 
ks1_phonics <- rio::import(paste0("data/", "phonics/", phonics_file_name)) %>%
  fsubset(breakdown_topic == "SEN status" &
    time_period >= 201516 &
    gender == "Total" &
    breakdown %in% c("EHC plan", "SEN support", "No SEN")) 


ks1_phonics_allSEN <- ks1_phonics %>%
  group_by(
    time_period, geographic_level,
    region_name, la_name
  ) %>%
  filter(breakdown != "No SEN") %>%
  summarise(across(c("t_phonics_y1_eligible_pupils", "t_phonics_y1_met_expected_standard"), ~sum(as.numeric(.x), na.rm = T))) %>%
  mutate(
    `Percent meeting expected standards in Y1` = round(100 * (t_phonics_y1_met_expected_standard / t_phonics_y1_eligible_pupils), 2),
    breakdown = "All SEN"
  ) %>%
  ungroup() %>%
  select(
    time_period, geographic_level,
    region_name, la_name,
    breakdown, `Percent meeting expected standards in Y1`,
    t_phonics_y1_met_expected_standard, 
    t_phonics_y1_eligible_pupils) %>%
rename(characteristic = breakdown)


ks1_phonics <- ks1_phonics %>%
  fselect(
    time_period,
    geographic_level,
    region_name, 
    la_name,
    breakdown, 
    pt_phonics_y1_met_expected_standard,
    t_phonics_y1_met_expected_standard,
    t_phonics_y1_eligible_pupils) %>%
  rename(characteristic = breakdown) %>%
  mutate(across(c(t_phonics_y1_eligible_pupils, t_phonics_y1_met_expected_standard), ~as.numeric(.x))) %>%
  ftransform(`Percent meeting expected standards in Y1` = round(100 * (t_phonics_y1_met_expected_standard / t_phonics_y1_eligible_pupils),2)) %>%
  bind_rows(ks1_phonics_allSEN) %>%
  time_period_to_academic_year() 
# Next 

#------------------------------
# Pupil destinations after 16-18 study - use revised unless only provisional are available 
#------------------------------
# https://explore-education-statistics.service.gov.uk/find-statistics/16-18-destination-measures

# Get meta data on latest published data 
dest_1618_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/16-18-destination-measures"
# Extract the year the publication refers to
dest_1618_year_of_pub <- read_html(dest_1618_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
dest_1618_name_start <- substr(dest_1618_year_of_pub, start = nchar(dest_1618_year_of_pub) -6, stop = nchar(dest_1618_year_of_pub) - 3)
dest_1618_name_end <- substr(dest_1618_year_of_pub, start = nchar(dest_1618_year_of_pub) -1, stop = nchar(dest_1618_year_of_pub) - 0)


# create a function that takes revised if available. If not, takes provisional stats. 
dest_1618_rev_or_prov <- function(region) {
  tryCatch(
    {
      get_ees_data(
        url = dest_1618_url, 
        desired_file_name <- paste0("1618_dm_ud_",dest_1618_name_start,dest_1618_name_end, "_", region, "_rev.csv"),
        zip_subfolder_to_extract <- "data/", 
        output_dir <- "data/destinations_1618"
      )
      print("If no warning, revised stats were used")
      
    },
    warning = function(warn) {
      # Handle the warning message here (e.g., print a message)
      cat("Warning:", conditionMessage(warn), "\n")
      print("use provisional stats as revised not yet available")
      
      get_ees_data(
        url = dest_1618_url, 
        desired_file_name <- paste0("1618_dm_ud_",dest_1618_name_start,dest_1618_name_end, "_", region, "_prov.csv") ,
        zip_subfolder_to_extract <- "data/", 
        output_dir <- "data/destinations_1618"
      )
      
    }
  )
  
}

# Save data to data folder - will be final or revised data, depending on the point in time this is run  
# The team have advised that the data doesn't tend to change between provisional and revised 
dest_1618_rev_or_prov(region = "la")
dest_1618_rev_or_prov(region = "nat")

# Import these files saved which start with pattern specified 
dir <- "data/destinations_1618"
dest_1618_to_import <- list.files(dir, pattern = paste0("1618_dm_ud_",dest_1618_name_start,dest_1618_name_end, "_"), full.names = TRUE)
dest_1618_ls <- list()

# Loop through the list of matching file names and read each file into R
for (file in dest_1618_to_import) {
  # Read the Excel file and store it in a data frame
  data <- rio::import(file)
  # Add the data frame to the list
  dest_1618_ls[[basename(file)]] <- data
}

# Next 

# Define a function for data wrangling
wrangle_1618_destinations <- function(data) {
  dest_1618_vars <- c("time_period", "geographic_level", "region_name", "la_name", "characteristic",
                     "characteristic_group", "cohort", "all_work", "appren", "he", "fe", "other_edu", "all_notsust", "all_unknown")
  data %>%
    fsubset(data_type == "Percentage" &
              cohort_level_group == "Total" &
              characteristic %in% c("Identified LLDD", "Identified SEN", "No identified LLDD", "No identified SEN")) %>%
    select(any_of(dest_1618_vars)) %>%
    time_period_to_academic_year() %>%
    mutate(across(c("all_work", "appren", "fe", "he", "other_edu", "all_notsust", "all_unknown"), ~as.numeric(.x))) %>% 
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
  

}

destinations_1618 <- wrangle_1618_destinations(dest_1618_ls$`1618_dm_ud_202021_la_rev.csv`)
destinations_1618_nat <- wrangle_1618_destinations(dest_1618_ls$`1618_dm_ud_202021_nat_rev.csv`)

# Next 

#==============================
# Mental Health stats from MHSDS
#==============================

# https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics
## This does three key things:
### 1. Goes to landing page where all publications are presented
### 2. Goes to the latest publication and extracts the time_series data 
### 3. wrangles the data 


### 1. Goes to landing page where all publications are presented & programatically works out the latest publication
# create empty vectors  
date_vector <- c()
link_vector <- c()

# Nested loop to combine and print
for (year in nhs_years) {
  for (month in months) {
    link <- paste0(
      "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics/performance-",
      getLowercaseMonthName(month), "-provisional-", getLowercaseMonthName(month+1), "-", year)
    
    date_str <- paste(year, month, "01", sep = "-") # Make start of the month 
    date <- as.Date(date_str)
    
    # Append the date and link to their respective vectors
    date_vector <- c(date_vector, date)
    link_vector <- c(link_vector, link)    
  }
}

# Format the 'Date' column in your dataframe
mh_web_df <- data.frame(date = format(as.Date(date_vector), format = ymc_date_format), Link = link_vector) %>%
  filter(date <= Sys.Date())

# Classify whether the link works or not 
mh_web_df$link_is_valid <- sapply(mh_web_df$Link, valid_url)

mh_web_df <- mh_web_df %>% filter(link_is_valid == T) 

# Get a var for publication date 
mh_web_df$publish_date <- sapply(mh_web_df$Link, get_nhs_date_pub)

# Get most recent publication
mh_web_df <- mh_web_df %>%
  mutate(
    check_date_format = substr(publish_date, start = 1, stop = 2)) %>%
  mutate(check_date_format = as.numeric(check_date_format)) %>%
  filter(!is.na(check_date_format)) %>%
  mutate(publish_date = as.character(publish_date)) %>% 
  mutate(publish_date = as.Date(publish_date, format = "%d %b %Y")) %>%
  mutate(date_diff_days = as.numeric(difftime(Sys.Date(), publish_date, units = "days"))) %>%
  # Some live links are created before the publication date, so remove this 
  filter(date_diff_days > 0) %>%
  # get the link which is closest to today (looking back in time)
  filter(date_diff_days == min(date_diff_days))


### 2. Goes to the latest publication and extracts the time_series data 
get_mh_nhs_link <- function(url) {
  page <- read_html(url) 
  
  link_elements <- page %>% html_nodes("a") 
  
  nhs_mh_links <- link_elements %>% html_attr("href") %>% as.data.frame() %>%
    rename(dwnld_link = 1) %>%
    filter(grepl(".csv", dwnld_link)) %>%
    distinct() 
}

# Get the time_series_data csv web link 
mh_time_series_link <- get_mh_nhs_link(paste(mh_web_df$Link)) %>%
  filter(str_detect(dwnld_link, "Time_Series_data"))

mh_time_series_link <- paste0(mh_time_series_link$dwnld_link)

mh_time_series_name <- paste0(substr(mh_time_series_link, start = nchar(mh_time_series_link) -40, stop = nchar(mh_time_series_link) -0)) 

# Save file as published 
GET(mh_time_series_link, write_disk(paste0("data/nhs/", mh_time_series_name), overwrite = TRUE))


### 3. wrangles the data 
mentalhealth_raw <- rio::import(paste0("data/nhs/", mh_time_series_name)) %>%
  fsubset(PRIMARY_LEVEL != "UNKNOWN" & # strip out "unknown" data
            MEASURE_ID == "MHS95" & # Number of CYP aged under 18 supported through NHS funded mental health with at least one contact 
            BREAKDOWN %in% c("CCG of Residence", "Sub ICB of Residence", "England", "Commissioning Region")) %>%
  ftransform(`Year ending` = lubridate::dmy(REPORTING_PERIOD_END))

mentalhealth <- mentalhealth_raw %>%
  mutate(BREAKDOWN2 = ifelse(BREAKDOWN %in% c("CCG of Residence", "Sub ICB of Residence"),
                             yes = "CCG/Sub-ICB of Residence",
                             no = BREAKDOWN
  )) %>% # Combine CCG and sub-ICB level, like the NHS PowerBI dashboard does
  left_join(nhs_lookup, by = c("PRIMARY_LEVEL" = "nhs_code")) %>% # to add region and make names formatted the same
  ftransform(`Number of children and young people` = as.numeric(MEASURE_VALUE)) %>%
  mutate(nhs_name = coalesce(nhs_name, PRIMARY_LEVEL_DESCRIPTION))

#rename all the regions of which many are a) IN ALL CAPS and b) have a spurious "COMMISSIONING REGION" at the end (we know it's a region)
mentalhealth_reg <- mentalhealth %>% 
  fsubset(BREAKDOWN2 == "Commissioning Region") %>% 
  mutate(nhs_name = unlist(map(gsub("COMMISSIONING REGION$", "", nhs_name), simpleCap)))

mentalhealth <- mentalhealth %>% 
  fsubset(BREAKDOWN2 != "Commissioning Region") %>% 
  bind_rows(mentalhealth_reg)

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

# Next 

#==============================
# EXPERIENCES INDICATORS
#==============================

#------------------------------
# Save all the SEN2 data files I want 
#------------------------------

# define list of sen2 files wanted 
sen2_ls <- c("sen2_age_caseload.csv",
             "sen2_mi.csv")

# loop through list and save the latest files 
for(i in sen2_ls) {
  get_ees_data(
    url = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans", # Define URL of publication
    # print what year this publication refers to (webscrapes caption)
    # Note that the "_" naming convention on the end was only introduced in 2021 
    desired_file_name <- i,
    # Data in EES folders are in sub folders (usually called "data" e.g. zip/data/"file.csv". We want to retrieve the final only. 
    zip_subfolder_to_extract <- "data/", 
    output_dir <- "data/ehc_plans/"
  )
}

# Next 

#------------------------------
# SEN2_mi multipurpose file
#------------------------------

# Source: Education, health and care plans. File 05 - Requests, assessments, discontinued plans, 20 week timeliness, mainstream to special transfers, mediation and tribunals
sen2_mi <- read_csv("data/ehc_plans/sen2_mi.csv",
  col_types = cols(NoExc20weekRate = col_number())
)

# EHCP timeliness
ehcp_timeliness <- sen2_mi %>%
  fsubset(time_period > 2016) %>%
  fselect(time_period, geographic_level, region_code,
    region_name, old_la_code, new_la_code,
    la_name, new_all_ehc,
    `% of EHCPs issued within 20 weeks` = no_exc_20week_rate
  ) %>%
  ftransform(region_name = if_else(condition = geographic_level == "National",
    true = "England",
    false = region_name
  )) %>%
  mutate(`% of EHCPs issued within 20 weeks`  = as.numeric(`% of EHCPs issued within 20 weeks` ))

# Next 

#------------------------------
# Absences data 
#------------------------------

abs_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms"
# Extract the year the publication refers to

get_ees_data(
  url = abs_url, 
  desired_file_name <- "6_absence_2term_characteristics.csv",
  zip_subfolder_to_extract <- "data/", 
  output_dir <- "data/absence"
)

# Overall absence rate (autumn and spring terms)
absence <- rio::import("data/absence/6_absence_2term_characteristics.csv") %>%
  fsubset(school_type == "Total" &
            characteristic %in% c("SEN - SEN support",
                                  "SEN - Statement or EHCP",
                                  "SEN - No SEN")) %>%
  ftransform(characteristic = recode(characteristic,
                                     "SEN - SEN support" = "SEN support",
                                     "SEN - Statement or EHCP" = "EHCP or Statement",
                                     "SEN - No SEN" = "No SEN"
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
  filter(characteristic != "No SEN") %>%
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


absence_regional <- rio::import("data/absence/6_absence_2term_characteristics.csv") %>%
  fsubset(school_type == "Total" &
            characteristic %in% c("SEN - SEN support",
                                  "SEN - Statement or EHCP",
                                  "SEN - No SEN") &
            geographic_level %in% c("National", "Regional")) %>%
  ftransform(
    characteristic = recode(characteristic,
                            "SEN - SEN support" = "SEN support",
                            "SEN - Statement or EHCP" = "EHCP or Statement",
                            "SEN - No SEN" = "No SEN"),
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
  ungroup() %>%
  filter(characteristic != "No SEN") %>%
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

# Next

#------------------------------
# KS4 attainment REVISED - attainment 8 (webscraping used)
#------------------------------
ks4_attainment_201819 <- rio::import(paste0("data/attainment_ks4/hardcoded/", "201819_LA_characteristics_data_including_sen_description.xlsx")) %>%
  select(-c("avg_att8_unrounded")) %>%
  rename( characteristic_ethnic_major = characteristic_Ethnic_major) %>%
  filter(
    characteristic_sen_description %in% c("Any SEN", "No identified SEN"),
    characteristic_gender == "Total",
    breakdown %in% c("Total", "SEN description")) # for whatever reason the data uses both of these for the SEN breakdown we want 

# url we want
ks4_att_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised"
# Extract the year the publication refers to
ks4_att_year_of_pub <- read_html(ks4_att_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
ks4_att_name_start <- substr(ks4_att_year_of_pub, start = nchar(ks4_att_year_of_pub) -4, stop = nchar(ks4_att_year_of_pub) - 3)
ks4_att_name_end <- substr(ks4_att_year_of_pub, start = nchar(ks4_att_year_of_pub) -1, stop = nchar(ks4_att_year_of_pub) - 0)
# State the file with correct year we want
ks4_att_file_name <- paste0(ks4_att_name_start, ks4_att_name_end, "_lachar_data_revised.csv")

# Save latest ks4 attainment data 
get_ees_data(
  url = ks4_att_url, 
  desired_file_name <- ks4_att_file_name,
  zip_subfolder_to_extract <- "data/", 
  output_dir <- "data/attainment_ks4"
)

# wrangle latest ks4 data 
ks4_attainment_new <- rio::import(paste0("data/attainment_ks4/", ks4_att_file_name)) %>%
  # Remove the year that we're addressing via the manual hardcoded data 
  filter(time_period != "201819") %>%
  fsubset(breakdown == "SEN description" &
            characteristic_free_school_meals == "Total" &
            characteristic_sen_status == "Total" &
            characteristic_sen_description %in% c("Any SEN", "No identified SEN") &
            characteristic_disadvantage == "Total" &
            characteristic_gender == "Total") 

# Bind and sort data 
ks4_attainment <- rbind(ks4_attainment_new, ks4_attainment_201819) %>%
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
  ) %>%
  # Add the below in case publication team add in 2018/19 later 
  distinct(.keep_all = T)

# Next 

#------------------------------
# KS4 (post-16) destination measures (webscraping used)
#------------------------------
# https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-destination-measures 

# Get meta data on latest publication 
dest_ks4_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-destination-measures"
# Extract the year the publication refers to
dest_ks4_year_of_pub <- read_html(dest_ks4_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
dest_ks4_name_start <- substr(dest_ks4_year_of_pub, start = nchar(dest_ks4_year_of_pub) -6, stop = nchar(dest_ks4_year_of_pub) - 3)
dest_ks4_name_end <- substr(dest_ks4_year_of_pub, start = nchar(dest_ks4_year_of_pub) -1, stop = nchar(dest_ks4_year_of_pub) - 0)

# create a function that takes revised if available. If not, takes provisional stats. 
dest_ks4_rev_or_prov <- function(region) {
  tryCatch(
    {
      get_ees_data(
        url = dest_ks4_url, 
        desired_file_name <- paste0("ks4_dm_ud_",dest_ks4_name_start,dest_ks4_name_end, "_", region, "_rev.csv"),
        zip_subfolder_to_extract <- "data/", 
        output_dir <- "data/destinations_ks4"
      )
      print("If no warning, revised stats were used")
      
    },
    warning = function(warn) {
      # Handle the warning message here (e.g., print a message)
      cat("Warning:", conditionMessage(warn), "\n")
      print("use provisional stats as revised not yet available")
      
      get_ees_data(
        url = dest_ks4_url, 
        desired_file_name <- paste0("ks4_dm_ud_",dest_ks4_name_start,dest_ks4_name_end, "_", region, "_prov.csv") ,
        zip_subfolder_to_extract <- "data/", 
        output_dir <- "data/destinations_ks4"
      )
      
    }
  )
  
}

## Save data using function  
dest_ks4_rev_or_prov(region = "la")
dest_ks4_rev_or_prov(region = "nat")

# Import these files saved which start with pattern specified 
dir <- "data/destinations_ks4"
dest_ks4_to_import <- list.files(dir, pattern = paste0("ks4_dm_ud_",dest_ks4_name_start,dest_ks4_name_end, "_"), full.names = TRUE)
dest_ks4_ls <- list()

# Loop through the list of matching file names and read each file into R
for (file in dest_ks4_to_import) {
  # Read the Excel file and store it in a data frame
  data <- rio::import(file)
  # Add the data frame to the list
  dest_ks4_ls[[basename(file)]] <- data
}

# Functionalise data wrangling as the same is done for LA and national data  
ks4_dest_wrangle_data <- function(data) {
  data %>% as.data.frame %>%
    fsubset(characteristic_group == "SEN Provision" &
              data_type == "Percentage" &
              institution_group == "State-funded mainstream & special schools" &
              characteristic %in% c("Identified SEN",
                                    "School Action/SEN support",
                                    "Statement of SEN/Education Health and Care plan",
                                    "No identified SEN")) %>%
    mutate(characteristic = recode(characteristic,
                                   "Statement of SEN/Education Health and Care plan" = "EHC plan or Statement",
                                   "School Action/SEN support" = "SEN support",
                                   "No identified SEN" = "No identified SEN" 
    )) %>%
    time_period_to_academic_year() %>%
    fsubset(academic_year > "2017/18") %>%
    mutate(
      across(c("all_work", "appren", "sfc", "ssf", "fe", "other_edu", "all_notsust", "all_unknown"), ~as.numeric(.x))
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
  
  
}

# Apply the function to both datasets
ks4_destinations <- ks4_dest_wrangle_data(data = as.data.frame(dest_ks4_ls$ks4_dm_ud_202021_la_rev.csv))
ks4_destinations_nat <- ks4_dest_wrangle_data(data = as.data.frame(dest_ks4_ls$ks4_dm_ud_202021_nat_rev.csv))

# Next 

#==============================
# FINANCIAL SUSTAINABILITY INDICATORS 
#==============================

#-----------------------------------------
# Local authority education surplus/deficit
#-----------------------------------------
# DSG cumulative balance as a % of the total budget
# This is derived from two separate data sources - see steps below 

# Webscrape list of deep links from latest year 
## Note that the historical files are published here, so all links are extracted 
url_lS_land <- list()
for(i in years_num) {
  start_yr <- paste0(i)
  end_yr <- i+1
  end_yr <-paste0(end_yr) # a bunch of these are 'nonsense' links because they're in the future  
  new_url <- paste0("https://skillsfunding.service.gov.uk/view-latest-funding/national-funding-allocations/DSG/", start_yr, "-to-", end_yr)
  url_lS_land <- c(url_lS_land, new_url)
}


# Create a dataframe of links to the files I want
url_list_file <- data.frame()
for (i in url_lS_land) {
  # Read the HTML content from the webpage
  page <- read_html(i)
  # Define the label you're looking for
  target_label <- "Download DSG allocations for all local authorities and regions"
  # Find the link element that contains the target label
  fund_link_ls <- page %>% 
    html_nodes(paste0("a:contains('", target_label, "')")) %>%
    html_attr("href")
  # Extract the relevant data and create a new data frame
  individual_df <- data.frame(link = fund_link_ls,
                              new_column = substr(fund_link_ls, nchar(fund_link_ls) - 12, nchar(fund_link_ls) - 0))
  # Append the new data frame to the main list
  url_list_file <- rbind(url_list_file, individual_df) 
  rm(individual_df)
}

# Keep the latest distinct files (one per year)
url_list_file <- url_list_file %>% 
  distinct(link) %>% 
  mutate(
    fy = substr(link, start = nchar(link) - 37, stop = nchar(link) - 31),
    year_ref = substr(link, start = nchar(link) - 34, stop = nchar(link) - 31),
    date_string = substr(link, start = nchar(link) - 9, stop = nchar(link) - 0)) %>%
  mutate(pub_date = as.POSIXct(date_string)) %>%
  mutate(march_pub_flag = ifelse(substr(pub_date, start = nchar(pub_date) -4, stop = nchar(pub_date) -3) %in% "03", 1, 0)) %>%
  filter(march_pub_flag == 1) %>%
  group_by(year_ref) %>%
  filter(pub_date == max(pub_date)) %>% ungroup() %>% 
  arrange(pub_date) %>% 
  distinct(link) 
url_list_file <- as.vector(unlist(url_list_file))

# Loop through URLs and import data
for(i in url_list_file){
  fund_link <- paste0("https://skillsfunding.service.gov.uk",i)
  
  # Extract month and year
  year <- paste0("20", substr(fund_link, start = nchar(fund_link) - 32, stop = nchar(fund_link) - 31)) # Use the FY as year ref
  # Extract strings for naming 
  month <- substr(fund_link, start = nchar(fund_link) - 4, stop = nchar(fund_link) - 3)
  day <- substr(fund_link, start = nchar(fund_link) - 1, stop = nchar(fund_link) - 0)
  date_string <- paste(year, month, day, sep = "-")
  fy <- substr(fund_link, start = nchar(fund_link) - 37, stop = nchar(fund_link) - 31)
  file_name <- paste0("dedicated-schools-grant_", fy, "_pub_date_", year, "_", month, "_", day,".ods") 
  download.file(fund_link, paste0("data/finance", "/", file_name), quiet = TRUE, mode = "wb")
  df_name <- paste0("dsg_budget_", year, "_", month, "_", day)
  print(fund_link)
  
  # Read the downloaded ODS file into R and wrangle data 
  ind_data <- readODS::read_ods(paste0("data/finance", "/", file_name), sheet = "Allocations_summary", skip = 1)[, 1:7] %>%
    mutate(across(3:7, as.numeric)) %>%
    mutate(
      time_period = paste0(as.numeric(year)-1, as.numeric(year)-2000), # derive name 
      pub_date = as.POSIXct(date_string)) %>%
    rename(`Schools block (£s)` = `Schools.block...s....3`,
           `Total DSG allocation (£s)` = `Total.DSG.allocation...s....7`) %>%
    drop_na(`Schools block (£s)`) %>%
    select(la_code = 1, la_name = 2, `Total DSG allocation (£s)`, time_period)
  assign(df_name, ind_data)
  rm(ind_data) 
  # Delete the raw files (remove this line if you want to check the data is pulled through correctly)
  unlink(paste0("data/finance", "/", file_name)) 
  
}

# Import 2019/20 data (it was published on a different area previously, so this is hard coded)
dsg_budget_20 <- rio::import("data/finance/hardcoded_data_keep/dedicated-schools-grant.ods", sheet = "Allocations_1920", skip = 1)[, 1:7] %>%
  mutate(across(3:7, ~ 1000000 * as.numeric(.x)), # 2019-20 budget is in millions rather than £s so correct (we'll fix the names in a bit)
         time_period = "201920"
  ) %>%
  rename(`Schools block (£s)` = `X2019.20.schools.block.....million....3`,
         `Total DSG allocation (£s)` = `X2019.20..total.DSG.allocation.....million....7`) %>%
  drop_na(`Schools block (£s)`) %>%
  select(la_code = 1, la_name = 2, `Total DSG allocation (£s)`, time_period)

# Asssign dfs with specified naming convention to a list
dsg_list <- mget(ls(pattern = "^dsg_budget"), inherits = TRUE)

# Create a time series of dsg budget 
dsg_budget <- dplyr::bind_rows(dsg_list) %>%
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
    la_code == "Yorkshire and the Humber" ~ "Yorkshire and The Humber",
    la_name == "" ~ la_code,
    TRUE ~ region
  )) %>%
  select(-la_code) %>%
  distinct() # London is repeated for some reason, so this line should remove four rows, one for each year

# Split and save data to excel (not used for further analysis)
split_dsg_by_year <- split(dsg_budget, dsg_budget$time_period)
# Create separate data frames with names like df_year1, df_year2, etc.
for (i in seq_along(split_dsg_by_year)) {
  assign(paste0("df_year", i), split_dsg_by_year[[i]], envir = .GlobalEnv)
}
# Save one data set per worksheet to excel 
wb <- createWorkbook()
data_frame_names <- names(dsg_list)
for (i in seq_along(dsg_list)) {
  sheet_name <- data_frame_names[i]  # Get the name of the data frame
  addWorksheet(wb, sheet_name)  # Add a new sheet with the data frame's name
  writeDataTable(wb, sheet_name, dsg_list[[i]])  # Write the data frame to the sheet
}
saveWorkbook(wb, paste0("data/finance/", "DSG_by_year_dr.xlsx"), overwrite = TRUE)


# Now, bring in s251 data and derive LA education surplus/deficit measure
s251_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/la-and-school-expenditure"
s251_year_of_pub  <- read_html(s251_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()

# State the file with correct year we want
s251_file_name <- paste0("s251_alleducation_la_regional_national2.csv")

# Save data 
get_ees_data(
  url = s251_url, 
  desired_file_name <- s251_file_name,
  zip_subfolder_to_extract <- "data/", 
  output_dir <- "data/finance"
)

# import and wrangle the data 
dsg_deficit <- rio::import(paste0("data/finance/", s251_file_name)) %>%
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
  bind_rows(dsg_london) %>% # because adding it back
  ftransform(region_name = case_when(region_name %in% c("Inner London", "Outer London") ~ "London",
                                     region_name == "" ~ "England",
                                     .default = region_name
  ))

dsg_deficit <- dsg_deficit %>%
  inner_join(select(dsg_budget, la_name, region, time_period, `Total DSG allocation (£s)`), by = c("la_name", "region_name" = "region", "time_period")) %>%
  rowwise() %>%
  mutate(deficit = round(
    100 * (`1.9.3 Dedicated Schools Grant carried forward to next year` /
       `Total DSG allocation (£s)`), 2
  )) %>%
  ftransform(`DSG cumulative balance as a % of the total budget` = -deficit) %>%
  fselect(time_period, geographic_level, region_name, la_name, deficit, `DSG cumulative balance as a % of the total budget`) %>%
  ftransform(financial_year = paste0(substr(time_period, start = 1, stop = 4), "-", substr(time_period, start = 5, stop = 6)))


write.xlsx(dsg_deficit, "data/finance/dsg_deficit_derived.xlsx")

# Next 

## -----------------------------------#
## IDENTIFICATION OF NEED INDICATORS #
## -----------------------------------#

# Save the latest SEN in England stats wanted 
sen_england <- c("sen_phase_type_.csv")

for(i in sen_england) {
  get_ees_data(
    url = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england", # Define URL of publication
    desired_file_name <- i,
    zip_subfolder_to_extract <- "data/", 
    output_dir <- "data/sen_in_england/"
  )
}

# % of pupils with an EHCP
# Source: SEN in England. 01 - Pupils in all schools, by type of SEN provision - including independent schools and general hospital schools - 2016 to 2022
# https://explore-education-statistics.service.gov.uk/data-catalogue/special-educational-needs-in-england/2021-22
sen_phase_type <- read_csv("data/sen_in_england/sen_phase_type_.csv", col_types = c(
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
ehcp_ageprofile <- rio::import("data/ehc_plans/sen2_age_caseload.csv") %>%
  rename(characteristic_age = age ) %>%
  fsubset(characteristic_age != "Total") %>%
  filter(ehcp_or_statement == "Total") %>% 
  mutate(
    `Number of EHCPs` = as.numeric(num_caseload),
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


# save setting type lists  
ls_specialist_setting_types <- c("Independent school",
                              "State-funded AP school",
                              "State-funded special school",
                              "Non-maintained special school")

ls_mainstream_setting_types <- c("State-funded nursery",
               "State-funded primary",
               "State-funded secondary",
               "Non-maintained special school")

# The benchmarking graphs show a combined % in specialist provision, rather than split out AP from special and from independent.
# This next code creates a table that feeds those graphs
provider_types_grouped <- provider_types %>%
  mutate(`Grouped provider type` = case_when(
    phase_type_grouping %in% ls_specialist_setting_types ~ "Independent, alternative provision or special school", 
    phase_type_grouping %in% ls_mainstream_setting_types ~  "Mainstream",
    T ~ "CHECK - names have changed year-on-year"
  )) %>%
  group_by(la_name, `Grouped provider type`, academic_year, `Provision type`, geographic_level, region_name) %>%
  summarise(`% in independent/AP/special` = sum(`% of pupils (with SEN provision type)`)) %>%
  # Having created a % figure for the new combined category we don't need the mainstream figures
  filter(`Grouped provider type` == "Independent, alternative provision or special school") %>%
  ungroup() 
  

provider_types_nat <- provider_types %>% fsubset(geographic_level == "National")

provider_types_grouped_nat <- provider_types_nat %>%
  mutate(`Grouped provider type` = case_when(
    phase_type_grouping %in% ls_specialist_setting_types ~ "Independent, alternative provision or special school", 
    phase_type_grouping %in% ls_mainstream_setting_types ~  "Mainstream",
    T ~ "CHECK - names have changed year-on-year"
  )) %>%
  group_by(la_name, `Grouped provider type`, academic_year, `Provision type`, geographic_level, region_name) %>%
  summarise(`% in independent/AP/special` = sum(`% of pupils (with SEN provision type)`)) %>%
  filter(`Grouped provider type` == "Independent, alternative provision or special school")

rm(sen_phase_type) # No longer need the big source file
gc() # Free up some RAM

# Next

#=================================
# Autism - waiting times 
#=================================
# This is a multi-step process 

### 1. Goes to landing page where all publications are presented
# create empty vectors  
date_vector <- c()
link_vector <- c()

# Nested loop to combine and print
for (year in nhs_years) {
  for (month in months) {
    link <- paste0(
      "https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics/",
      getLowercaseMonthName(month), "-", year-1, "-to-", getLowercaseMonthName(month-1), "-", year)
    
    date_str <- paste(year, month, "01", sep = "-")
    date <- as.Date(date_str)
    
    # Append the date and link to their respective vectors
    date_vector <- c(date_vector, date)
    link_vector <- c(link_vector, link)    
  }
}

# Format the 'Date' column in your dataframe 
aut_web_df <- data.frame(Date = format(as.Date(date_vector), format = ymc_date_format), Link = link_vector) %>%
  # remove future publication dates that have live links already 
  filter(Date <= Sys.Date())
# Classify whether the link works or not 
aut_web_df$link_is_valid <- sapply(aut_web_df$Link, valid_url)
aut_web_df <- aut_web_df %>% filter(link_is_valid == T) 
# Get a var for publication date 
aut_web_df$publish_date <- sapply(aut_web_df$Link, get_nhs_date_pub)
# Filter and sort - get most recent publication 
aut_web_df <- aut_web_df %>%
  mutate(
    check_date_format = substr(publish_date, start = 1, stop = 2)) %>%
  mutate(check_date_format = as.numeric(check_date_format)) %>%
  filter(!is.na(check_date_format)) %>%
  mutate(publish_date = as.character(publish_date)) %>% 
  mutate(publish_date = as.Date(publish_date, format = "%d %b %Y")) %>%
  mutate(date_diff_days = as.numeric(difftime(Sys.Date(), publish_date, units = "days"))) %>%
  # Some live links are created before the publication date, so remove this 
  filter(date_diff_days > 0) %>%
  # get the link which is closest to today (looking back in time)
  filter(date_diff_days == min(date_diff_days))


### 2. Goes to the latest publication and extracts the data 
get_aut_nhs_link <- function(url) {
  page <- read_html(url) 
  
  link_elements <- page %>% html_nodes("a") 
  
  nhs_aut_links <- link_elements %>% html_attr("href") %>% as.data.frame() %>%
    rename(dwnld_link = 1) %>%
    filter(grepl(".zip", dwnld_link)) %>%
    distinct() 
}

# Get files required 
aut_time_series_link <- get_aut_nhs_link(paste(aut_web_df$Link)) %>%
  # this should make no difference 
  filter(str_detect(dwnld_link, "AutismStats_"))

aut_time_series_link <- paste0(aut_time_series_link$dwnld_link)

for(i in aut_time_series_link) {
  download.file(paste0(i), paste0("data/nhs", "/","temp.zip"), quiet = TRUE, mode = "wb")
  zipF <- paste0("data/nhs", "/","temp.zip")
  outDir<-"data/nhs"
  unzip(zipF,exdir=outDir)
  unlink(zipF, recursive = TRUE)
  ls_files <- list.files(outDir, pattern = "")
}

delete_files <- function(pattern) {
  files <- list.files(outDir)
  to_be_deleted <- grep(paste0(pattern), files, value = T)
  file.remove(paste0("data/nhs/", to_be_deleted))
}

# What files are in my dir?
files <- list.files(outDir)

# Define the patterns of file to import 
aut_files_to_import <- c("_SubICB", "_England", "_Region", "_Provider")

# Use grep with the combined pattern to find matching files
aut_to_import <- list(grep(paste0(aut_files_to_import, collapse = "|"), files, value = TRUE))
aut_to_import <- unlist(aut_to_import)

aut_ts_df <- data.frame() # empty df

for(i in aut_to_import) { 
  individual_sheet <- read.csv(paste0("data/nhs/", i)) %>%
    mutate(source = paste0(i))
  print(i)
  aut_ts_df <- aut_ts_df %>% dplyr::bind_rows(individual_sheet)
}

# Delete csv files from folder 
for(i in c("DQ_", "_ICB", "temp", aut_files_to_import)) {
  delete_files(i)
}

### 3. wrangles the data 
# nhs_region_lookup <- rio::import("data/lookups/Sub_ICB_Locations_to_Integrated_Care_Boards_to_NHS_England_(Region)_(July_2022)_Lookup_in_England.csv") %>%
autism <- aut_ts_df %>%
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
  ) %>% 
  left_join(nhs_lookup, by = c("PRIMARY_LEVEL" = "nhs_code")) %>%
  mutate(REPORTING_PERIOD_START = as.character(REPORTING_PERIOD_START)) %>%
  mutate(
    nhs_name = coalesce(nhs_name, PRIMARY_LEVEL_DESCRIPTION),
    nhs_type = coalesce(nhs_type, "Provider"),
    date = lubridate::ymd(REPORTING_PERIOD_START)
  ) %>%
  # Shorten the names of provider organisations to make the graph work better
  mutate(nhs_name = str_replace(nhs_name, "FOUNDATION TRUST", "FT")) %>%
  mutate(nhs_name = str_replace(nhs_name, "NHS TRUST", "NHST")) %>%
  mutate(nhs_name = str_replace(nhs_name, " AND ", " & ")) %>%
  mutate(nhs_name = str_replace(nhs_name, "SOUTH WEST", "SW")) %>%
  # create var on aggregation levels (see BREAKDOWN var for details)
  mutate(data_level = case_when(BREAKDOWN %in% "Age Group" ~ "National",
                                BREAKDOWN %in% "CCG or Sub ICB Location - GP Practice or Residence; Age Group" ~ "CCG or Sub ICB",
                                BREAKDOWN %in% "Commissioning Region - GP Practice or Residence; Age Group" ~ "NHS Region",
                                BREAKDOWN %in% "Provider; Age Group" ~ "Provider",
                                T ~ "Check - error")) 

write.csv(autism, "data/nhs/autism_time_series_derived.csv")
# Next 

#================================
# Prep geog. files
#================================

local_authorities <- geojsonsf::geojson_sf("prep/Counties_and_Unitary_Authorities_(December_2022)_UK_BFC.geojson") %>%
  sf::st_transform(crs = 27700)

sub_icb_locations <- geojsonsf::geojson_sf("prep/Sub_Integrated_Care_Board_Locations_(July_2022)_EN_BFC.geojson") %>%
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

# This function filters down to only the national and regional data for the England/Regional summary panel
nat_and_reg <- function(df) {
  # Filter the dataframe
  df_filtered <- df[df$geographic_level %in% c("National", "Regional"), ]

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
    arrange(..., !!time_period) %>%
    mutate(pc_change = (100 * (!!column - lag(!!column, order_by = !!time_period)) / lag(!!column, order_by = !!time_period))) %>%
    ungroup()
  
  return(df)
}

# Create england-only dataframes
eng_ks2_attainment <- nat_and_reg(ks2_attainment) %>% compare_to_previous(`Percent meeting expected standards`, time_period, characteristic, region_name)
eng_ks1_phonics <- nat_and_reg(ks1_phonics) %>% compare_to_previous(`Percent meeting expected standards in Y1`, time_period, characteristic, region_name)
#this one is normal except that "last year" is 2018-19
eng_ks4_attainment <- nat_and_reg(ks4_attainment) %>% 
  mutate(region_name = if_else(geographic_level == "National", "England", region_name)) %>%  # the lack of an entry here is breaking compare_to_previous
  filter(time_period %in% c(201819, 202122)) %>% 
  compare_to_previous(`Average progress 8 score`, time_period, geographic_level, region_name, characteristic_sen_description)# there's no previous data here to compare to currently - for unclear reasons P8 wasn't published in 2018/19
eng_ehcp_timeliness <- nat_and_reg(ehcp_timeliness) %>% compare_to_previous(`% of EHCPs issued within 20 weeks`, time_period, region_name)
eng_mentalhealth <- mentalhealth %>%
  fsubset(BREAKDOWN %in% c("England", "Commissioning Region")) %>%
  compare_to_previous(`Number of children and young people`, `Year ending`, PRIMARY_LEVEL_DESCRIPTION)
eng_absence <- absence_regional %>%
  compare_to_previous(`Overall absence %`, time_period, characteristic, region_name)
eng_tribunals <- compare_to_previous(tribunals_reg, `SEND Tribunal Appeal Rate`, year, region_name)
eng_dsg_deficit <- nat_and_reg(dsg_deficit) %>% distinct() %>% compare_to_previous(`DSG cumulative balance as a % of the total budget`, time_period, region_name)
eng_percent_pupils_ehcp <- nat_and_reg(percent_pupils_ehcp) %>% compare_to_previous(`% of pupils`, time_period, `SEN provision`, region_name)
eng_autism <- autism %>%
  fsubset(BREAKDOWN == "Age Group") %>%
  compare_to_previous(`% with first appointment after more than 13 weeks`, date, nhs_name)
sum_ofsted <- ofsted %>% 
  filter(la_name != "Northamptonshire") %>% # because it doesn't actually exist at the moment
  mutate(WSoAPAP = case_when(`Priority action plan required? (new inspection framework)` == "Yes" ~ TRUE, 
                                  `Inspection outcome: written statement of action required? (previous inspection framwork)` == "Yes" ~ TRUE,
                                  `Revisit outcome: sufficient progress in addressing all significant weaknesses? (previous inspection framework)` != "Yes" ~ TRUE,
                                   .default = FALSE))
eng_ofsted <- count(ungroup(sum_ofsted), WSoAPAP, name = "Num_LAs") %>%
  fmutate(pc_LAs = Num_LAs/sum(Num_LAs))
reg_ofsted <- count(ungroup(sum_ofsted), region, WSoAPAP, name = "Num_LAs") %>% 
  group_by(region)  %>%
  fmutate(pc_LAs = Num_LAs/sum(Num_LAs))
reg_1618 <- nat_and_reg(destinations_1618) # for odd reasons the destinations metric comes in two files - national only and regional and local

# less straightforward summaries
# want an "all SEN" for this one which isn't there
eng_mainstream_with_sen <- nat_and_reg(mainstream_with_sen) %>%
  group_by(time_period, academic_year, region_name, geographic_level) %>%
  summarise(`% of pupils` = sum(`% of pupils`)) %>%
  mutate(`SEN provision` = "All SEN") %>%
  compare_to_previous(`% of pupils`, time_period, region_name)

eng_mainstream_with_sen_2 <- nat_and_reg(mainstream_with_sen) %>%
  compare_to_previous(`% of pupils`, time_period, `SEN provision`, region_name, geographic_level ) %>%
  select(time_period, academic_year, `% of pupils`, `SEN provision`, pc_change, region_name, geographic_level)

eng_mainstream_with_sen <- bind_rows(eng_mainstream_with_sen, eng_mainstream_with_sen_2)
# There may be other metrics that can be calculated at England level

# grouping all specialist provider types
eng_provider_types <- nat_and_reg(provider_types) %>%
  fsubset(`Provision type` == "all pupils" &
    phase_type_grouping %in% ls_specialist_setting_types) %>% # specialist types
  group_by(academic_year, region_name, geographic_level) %>%
  fsummarise(`% of pupils (with SEN provision type)` = sum(`% of pupils (with SEN provision type)`)) %>%
  compare_to_previous(`% of pupils (with SEN provision type)`, academic_year, region_name, geographic_level)

source("prep/summary_prep.R")

save(ks2_attainment, ks1_phonics, ks4_attainment, # Initial Outcomes metrics
  mentalhealth, mentalhealth_reg, mentalhealth_ccg, most_recent_mentalhealth_label, year_ago_mentalhealth_label, # MH metrics
  ofsted, ofsted_data_updated, destinations_1618, destinations_1618_nat, # Final Outcomes metrics
  ehcp_timeliness, autism, tribunals, tribunals_reg, absence, absence_regional, ks4_destinations, ks4_destinations_nat, # Experiences metrics
  dsg_deficit, specialist_spend, reg_specialist_spend, reg_specialist_spend_order, nat_specialist_spend, # Financial Sustainability metrics
  ehcp_ageprofile, mainstream_with_sen, percent_pupils_ehcp, # Identification of Need metrics
  provider_types, provider_types_grouped, provider_types_nat, provider_types_grouped_nat, # specialist provider types metric
  nhs_region_list, la_region_lookup, nhs_lookup, la_ccg_lookup, # Lookups
  eng_absence, eng_autism, eng_dsg_deficit, eng_ehcp_timeliness, eng_ks1_phonics, eng_ks2_attainment, eng_ks4_attainment,
  eng_mentalhealth, eng_percent_pupils_ehcp, eng_tribunals, eng_mainstream_with_sen, eng_provider_types, eng_ofsted, reg_ofsted, reg_1618, summary_metrics,
  # box_ks2_attainment, sparkline_ks2_attainment,
  file = "data/prepared_data.Rdata"
)

# End 