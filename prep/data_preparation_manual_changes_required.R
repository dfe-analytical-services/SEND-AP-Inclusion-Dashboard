library(rvest)
library(httr)
library(dplyr)
library(openxlsx)
library(lubridate)
library(tidyr)
library(data.table)

# This code is required because some files require manual changes, due to the way they're published 

print("This file requires some manual changes - predominately updating links")

#==============================
# Define inputs for use in code 
#==============================

years_num <- c(2015:2030) 

# GENERAL-USE FILES AND FUNCTIONS ---------------------------------------------------------------------------------

#------------------------------
# EES webscraping / downloading functions
#------------------------------

# Retrieve the 'Download all data (zip)' link which is on all Explore Education Statistics (EES) publication pages
ees_zip_link <- function(url){
  weblink <- RCurl::getURL(url)
  gsubfn::strapplyc(
    weblink, 
    '<h3 class=\"govuk-heading-s govuk-!-margin-bottom-2\"><a href=\"https://content.explore-education-statistics.service.gov.uk/api/releases/(.*)/files\" class=\"govuk-link', simplify = TRUE)
}

# This function webscrapes the zip file link, unzips the files, and extracts and saves the file(s) we want
get_ees_data <- function(
    # state URL of publication desired. Remove the ref year to automatically get the latest year e.g., https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england
  url, 
  # this is the name of the file you want available in the zip file retrievable from the 'Download all data (zip)' link on EES
  desired_file_name, 
  # State which folder WITHIN the zip downloaded contains the file you want 
  zip_subfolder_to_extract = "data/",
  # State filepath you want to save the file desired 
  output_dir = getwd()) {
  
  # State what year and publication is being used 
  publication_name <- read_html(url) %>% html_nodes(".govuk-heading-xl") %>% html_text()
  year_of_pub <- read_html(url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
  print(paste("Publication: ", publication_name))
  print(paste("Year: ", year_of_pub))
  
  # get url deep link
  url_name <- paste0("https://content.explore-education-statistics.service.gov.uk/api/releases/",ees_zip_link(url),'/files')
  print(url_name)
  
  # Download the ZIP 
  download.file(url_name, paste0(output_dir, "/","temp.zip"), quiet = TRUE, mode = "wb")
  
  # Extract the desired file to a temporary directory
  temp_extract_dir <- tempdir()
  unzip(paste0(output_dir,"/","temp.zip"), files = paste0(zip_subfolder_to_extract, desired_file_name), exdir = temp_extract_dir)
  
  # Move the extracted file to the output directory
  file.copy(file.path(temp_extract_dir, paste0(zip_subfolder_to_extract, desired_file_name)), 
            file.path(output_dir, desired_file_name), overwrite = T)
  
  # Clean up temporary files and directories
  unlink(temp_extract_dir, recursive = TRUE)
  unlink(paste0(output_dir, "/","temp.zip"))
}

#------------------------------
# General functions
#------------------------------

# Function for checking valid URL
valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}

# Converts numbers into month names 
getLowercaseMonthName <- function(numeric_month) {
  # Check if the input is within a valid month number range (1 to 12)
  if (numeric_month >= 1 && numeric_month <= 12) {
    # Use the month.name vector to get the month name and convert to lowercase
    return(tolower(month.name[numeric_month]))
  } else {
    return("Invalid month number")
  }
}

# Function for getting nhs publication date 
get_nhs_date_pub <- function(url) {
  read_html(url) %>% html_nodes(".nhsd-o-hero__meta-data-item-description") %>% html_text() %>%
    as.data.frame() %>%
    slice(1) %>%
    rename(pub_date_var = 1) %>%
    mutate(pub_date_var = trimws(pub_date_var)) 
}

# Create function to add a slash in time period variables to crate academic years
time_period_to_academic_year <- function(df) {
  df %>%
    ftransform(academic_year = paste0(substr(time_period, start = 1, stop = 4), "/", substr(time_period, start = 5, stop = 6)))
}

#------------------------------
# Create CCG --> Sub ICB --> Region NHS lookup
#------------------------------

# Get ETR and ECT data 
## https://digital.nhs.uk/services/organisation-data-service/export-data-files/csv-downloads/other-nhs-organisations
for(url in c("https://files.digital.nhs.uk/assets/ods/current/etr.zip", "https://files.digital.nhs.uk/assets/ods/current/ect.zip")) {
  download.file(paste0(url), paste0("data/lookups", "/","temp.zip"), quiet = TRUE, mode = "wb")
  zipF <- paste0("data/lookups", "/","temp.zip")
  outDir<-"data/lookups"
  unzip(zipF,exdir=outDir)
  unlink(zipF, recursive = TRUE)
}

unlink("data/lookups/etr.pdf")
unlink("data/lookups/ect.pdf")

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
  mutate(nhs_region = coalesce(nhs_region, nhs_type)) 

#-----------------------------------------
# LA lookup table 
#-----------------------------------------
# To replicate:
# Download a custom table in ODS format from:
# https://explore-education-statistics.service.gov.uk/data-tables/permalink/b5ad776d-bdb8-42d0-02e5-08daf870ec67
# Re-save it as XLSX# Load in LA lookup table

# Give reasonable variable names, remove metrics, etc
la_region_lookup <- rio::import("data/lookups/la_region_lookup_raw.xlsx", skip = 2) %>%
  rename(
    "region" = `...1`,
    "la_name" = `...2`
  ) %>%
  select(-`2021/22`) %>%
  drop_na(la_name) %>% # remove blank rows, comments etc
  fill(region) # the source data uses merged cells, fill the gaps in the region column


#------------------------------
# SEND Tribunal appeal rate (https://www.gov.uk/government/statistics/tribunal-statistics-quarterly-january-to-march-2022)
#------------------------------

print("The manual changed required for this tribunals code chunk is just updating the link")
# Update link  
trib_ods_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1161498/SEND_Appeal_Rate_Table_2022.ods"

# The rest is automated for this code chunk... 

# first year of data 
start_year <- 2014 
# latest year of data - do programatically 
end_year <- paste0(substr(trib_ods_url, start = nchar(trib_ods_url) -7, stop = nchar(trib_ods_url) -4))
trib_file_name_path <- paste0("data/tribunals/", "send_appeal_rate_table_", end_year, ".ods")
GET(trib_ods_url, write_disk(trib_file_name_path, overwrite = TRUE))
end_year <- as.numeric(end_year)

# Import the file you've just saved 
tribunals_raw <- rio::import(trib_file_name_path, sheet = "SEND_2", skip = 5)
tribunals2 <- rio::import(trib_file_name_path, sheet = "SEND_2", skip = 3)


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

#-----------------------------------------
# Per capita gross spend on non-maintained and independent special provision
#-----------------------------------------
print("This requires manual changes to be made")

# From High Needs Benchmarking Tool
## import xlsx spreadsheet - this is a modified version of the original "High Needs Benchmarking Tool".
## To reproduce, select a year in the "Select Comparators" tab AND in the "All LA data" tab dropdowns.
## Select 2+ LAs in the "Select Comparators" tab, select Outturn and ensure all year selections are consistent.
## Then copy the whole sheet and paste as values in its own tab (tab names as below)
## The code brings in population figures that are not currently used. You may decide to calculate regional/national figures
## in which case the population figures will be needed

specialist_spend_201819 <- rio::import("data/finance/2021-22-High-Needs-LA-Benchmarking-Tool.xlsx",
                                       sheet = "values_201819",
                                       range = c("B6:CE165")
) %>% # Import table
  select(
    region = 1,
    la_code = 2,
    "la_name" = 3,
    "pop2to18_2018-19" = "(ONS mid-2018 projection)",
    "specialschools_sf_2018-19" = "Special schools and academies (1)...71",
    "ap_sf_2018-19" = "PRUs and AP academies (1)...72",
    "specialschools_ind_2018-19" = "Special schools and academies (2)...79",
    "ap_ind_2018-19" = "PRUs and AP academies (2)...80"
  ) %>%
  filter(la_name != "Dorset (New, LA code 838)") %>% # This row is blank
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
  tidyr::drop_na(la_name, `state_2018-19`)


specialist_spend_201920 <- rio::import("data/finance/2021-22-High-Needs-LA-Benchmarking-Tool.xlsx",
                                       sheet = "values_201920",
                                       range = c("B6:CE165")
) %>%
  select(
    region = 1,
    la_code = 2,
    "la_name" = 3,
    "pop2to18_2019-20" = "(ONS mid-2019 projection)",
    "specialschools_sf_2019-20" = "Special schools and academies (1)...71",
    "ap_sf_2019-20" = "PRUs and AP academies (1)...72",
    "specialschools_ind_2019-20" = "Special schools and academies (2)...79",
    "ap_ind_2019-20" = "PRUs and AP academies (2)...80"
  ) %>%
  filter(la_name != "Dorset") %>% # There is a blank row for "Dorset"
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
  tidyr::drop_na(la_name, `state_2019-20`)


specialist_spend_202021 <- rio::import("data/finance/2021-22-High-Needs-LA-Benchmarking-Tool.xlsx",
                                       sheet = "values_202021",
                                       range = c("B6:CE165")) %>%
  select(
    region = 1,
    la_code = 2,
    "la_name" = 3,
    "pop2to18_2020-21" = "(ONS mid-2020 projection)",
    "specialschools_sf_2020-21" = "Special schools and academies (1)...71",
    "ap_sf_2020-21" = "PRUs and AP academies (1)...72",
    "specialschools_ind_2020-21" = "Special schools and academies (2)...79",
    "ap_ind_2020-21" = "PRUs and AP academies (2)...80"
  ) %>%
  filter(la_name != "Dorset") %>% # There is a blank row for "Dorset"
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
  tidyr::drop_na(la_name, `state_2020-21`)

# The HN Benchmarking tool for 2022-23 was broken while the dashboard was in production so its owner sent us this data file of what
# ought to be in there instead. Can be replaced by the benchmarking tool once that's updated
# note that in this case the data is raw outturn and not per-head so we need to divide through by population
specialist_spend_202122 <- rio::import("data/finance/Outturn data 202122.xlsx",
                                       #sheet = "values_202122",
                                       range = c("B6:DO165")) %>%
  select(
    region = 1,
    la_code = 2,
    "la_name" = 3,
    "pop2to18_2021-22" = "2-18 Population (ONS mid-2021 projection)",
    "specialschools_sf_2021-22" = "Special schools and academies (1)...103",
    "ap_sf_2021-22" = "PRUs and AP academies (1)...104",
    "specialschools_ind_2021-22" = "Special schools and academies (2)...111",
    "ap_ind_2021-22" = "PRUs and AP academies (2)...112"
  ) %>%
  filter(!(la_name %in% c("Dorset", "Bournemouth", "Poole", "Northamptonshire"))) %>% # blank rows due to LA reorgs
  mutate(la_name = recode(la_name,
                          "Dorset (New, LA code 838)" = "Dorset"
  )) %>%
  rowwise() %>%
  mutate(
    "pop2to18_2021-22" = as.numeric(`pop2to18_2021-22`),
    "independent_2021-22" = sum(as.numeric(`specialschools_ind_2021-22`), as.numeric(`ap_ind_2021-22`),
                                na.rm = TRUE
    ),
    "state_2021-22" = sum(as.numeric(`specialschools_sf_2021-22`), as.numeric(`ap_sf_2021-22`),
                          na.rm = TRUE
    ),
    "total_2021-22" = sum(`independent_2021-22`, `state_2021-22`))%>%
  tidyr::drop_na(la_name, `state_2021-22`)

# Combine the four years into one table
specialist_spend_1 <- specialist_spend_201819 %>%
  left_join(specialist_spend_201920) %>%
  left_join(specialist_spend_202021) %>%
  left_join(specialist_spend_202122) %>%
  select(la_name, starts_with(c("state", "ind", "total", "pop2to18")))

specialist_spend_1 <- specialist_spend_1 %>% # Cleaning up the ADCS' list of LAs to match our names
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
  )) 

specialist_spend_a <- specialist_spend_1 %>%
  select(-c(starts_with("pop2to18"))) %>%
  pivot_longer(
    cols = -c("la_name"),
    names_to = c("category", "year"),
    names_sep = "_",
    values_to = "Spend per head"
  ) 
specialist_spend_b <- specialist_spend_1 %>%
  select(starts_with(c("pop2to18", "la_name"))) %>%
  pivot_longer(
    cols = -c("la_name"),
    names_to = c("pop_est_var", "year"),
    names_sep = "_",
    values_to = "pop_est"
  ) 

specialist_spend <- specialist_spend_a %>%
  left_join(specialist_spend_b, by = c('year', 'la_name')) %>%
  mutate(total_spend = round(pop_est * `Spend per head`, 0)) %>%
  left_join(la_region_lookup) %>%
 # ftransform(`Spend per head` = round(`Spend per head`, 0)) %>%
  mutate(category = recode(category,
                               "independent" = "Independent or non-maintained",
                               "state" = "State",
                               "total" = "Total"
  )) %>%
  filter(la_name != "Dorset (New, LA code 838)") %>%
  distinct() %>%
  select(-c(pop_est_var))
 
nas <- specialist_spend %>%
  filter(is.na(`pop_est`) | pop_est ==0  )

# regional High Needs Benchmarking tool
reg_specialist_spend <- specialist_spend %>%
  group_by(region, category, year) %>%
  summarise(pop_est = sum(`pop_est`, na.rm = T),
            total_spend = sum(total_spend, na.rm = T)) %>%
  mutate(`Spend per head`= total_spend/ pop_est)

#for regional comparison graphs, want ordering by total spend per head
reg_specialist_spend_order <- reg_specialist_spend %>% 
  filter(category == "Total", 
         year == max(year), 
         !is.na(region)) %>% 
  arrange(`Spend per head`) %>%
  pull(region)

#and because it's for graph axis labels need to linebreak Yorkshire & the Humber
reg_specialist_spend_order[reg_specialist_spend_order == "Yorkshire and The Humber"] <- "Yorkshire and\nThe Humber"

# National High Needs Benchmarking tool
nat_specialist_spend <- specialist_spend %>%
  group_by(category, year) %>%
  summarise(pop_est = sum(`pop_est`, na.rm = T),
            total_spend = sum(total_spend, na.rm = T)) %>%
  mutate( `Spend per head`= total_spend/ pop_est)

rm(specialist_spend_a, specialist_spend_b, specialist_spend_201819, specialist_spend_201920, specialist_spend_202021, specialist_spend_202122)

# Next 

#------------------------------
# Local area inspection status - old framework 
#------------------------------
# https://www.gov.uk/government/collections/area-send-statistics 

# Save file as published 
ofsted_old_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1117636/Area_SEND_inspection_data_01_May_2016_to_31_August_2022.ods"
ofsted_old_file_name_path <- paste0("data/ofsted/", "Area_SEND_inspection_data_01_May_2016_to_31_August_2022.ods")
GET(ofsted_old_url, write_disk(ofsted_old_file_name_path, overwrite = TRUE))

# Save the last dataset on old insection framework
ofsted_official_31_Dec_22 <- rio::import(ofsted_old_file_name_path, sheet = "Area_SEND_up_to_31_Aug_2022", skip = 4) %>%
  select(la_code = Local.authority.code,
         la_name = Area.name,
         inspect_publish_date_old_framework = Inspection.publication.date,
         inspect_outcome_wsoa_old_framework = Inspection.outcome..written.statement.of.action.required.,
         revisit_outcome_prog_vs_weaknesses_old_framework = `Revisit.outcome..sufficient.progress.in.addressing.all.significant.weaknesses.`,
         revisit_publish_date_old_framework = `Revisit.publication.date`)

# Derive Dorset & Northamptonshire info from footnotes/ further info here: https://www.gov.uk/government/statistics/area-send-inspections-and-outcomes-in-england-as-at-31-august-2022/main-findings-area-send-inspections-and-outcomes-in-england-as-at-31-august-2022
old_rows <- data.frame(
  la_code = NA,
  la_name = c("Dorset", "Northamptonshire"),
  inspect_publish_date_old_framework = c("23/01/2017", "27/05/2017"),
  inspect_outcome_wsoa_old_framework = c("Yes", "No"),
  revisit_outcome_prog_vs_weaknesses_old_framework = c("Some", NA),
  revisit_publish_date_old_framework = c("11/02/2019", NA)
)

# Original + derived Dorset 
ofsted_official_31_Dec_22 <- rbind(ofsted_official_31_Dec_22, old_rows) %>%
  mutate(
    # recode so Ofsted data matches DfE naming convention 
    la_name = case_when(
      la_name %in% "Bristol" ~ "Bristol, City of",
      la_name %in% "Durham" ~ "County Durham",
      la_name %in% "Kingston upon Hull" ~ "Kingston upon Hull, City of",
      la_name %in% "St Helens" ~ "St. Helens",
      T ~ la_name))

# Next 

#------------------------------
# Local area inspection status - new framework 
#------------------------------

# Create function for finding ods link to MI data when you specify the link to the landing page 
get_ofsted_mi_link <- function(url) {

  page <- read_html(url) 

  link_elements <- page %>% html_nodes("a") 

  ofsted_mi_links <- link_elements %>% html_attr("href") %>% as.data.frame() %>%
    rename(dwnld_link = 1) %>%
    filter(grepl("https://assets.publishing.service.gov.uk/", dwnld_link)) %>%
    distinct() %>%
    as.vector()
}

# Get Ofsted MI data 
ofsted_mi <- get_ofsted_mi_link(url = paste0(
  # Standard part of link 
  "https://www.gov.uk/government/statistical-data-sets/area-send-inspections-and-outcomes-in-england-management-information", 
  # term/year part of link
  "-summer-term-2022-to-2023"))

# Import 
mi_ofsted_winter_2223 <- rio::import(paste0(ofsted_mi$dwnld_link),
                                     sheet = "All_ASEND_Jan_2023_to_Aug_2023",
                                     skip = 4) 
write.csv(mi_ofsted_winter_2223, paste0("data/ofsted/", "mi_ofsted_winter_2223.csv"))

# Wrangle / clean the data 
mi_oftsed_new_framework <- mi_ofsted_winter_2223  %>%
  select(la_name = Local.area.partnership, # ‘Local area partnership’ refers to those in education, health and care who are responsible for the strategic planning, commissioning, management, delivery and evaluation of arrangements for children and young people with SEND who live in a local area. A local area is the geographic footprint of a local authority.  
         inspect_publish_date_new_framework = Inspection.publication.date,
         inspection_outcome_new_framework = Inspection.outcome,
         priority_action_plan_required_new_framework = Priority.action.plan.required.) %>%
  mutate(
    # recode so Ofsted data matches DfE naming convention 
    la_name = case_when(
      la_name %in% "Bristol" ~ "Bristol, City of",
      la_name %in% "Durham" ~ "County Durham",
      la_name %in% "Kingston upon Hull" ~ "Kingston upon Hull, City of",
      la_name %in% "St Helens" ~ "St. Helens",
      T ~ la_name))

ofsted <- la_region_lookup %>%
  # Official stats - old framework 
  left_join(ofsted_official_31_Dec_22, by = 'la_name') %>%
  left_join(mi_oftsed_new_framework, by = 'la_name') %>%
  mutate(across(c("inspection_outcome_new_framework"), ~ifelse(is.na(.x), "Not yet inspected under new framework", .x))) %>%
  mutate(across(c(inspect_publish_date_old_framework, inspect_publish_date_new_framework), ~as.Date(.x, format = "%d/%m/%Y"))) %>% 
  mutate(summary_outcome = case_when(inspection_outcome_new_framework != "Not yet inspected under new framework" ~ paste(inspection_outcome_new_framework, "(new framework)"), 
                                    revisit_outcome_prog_vs_weaknesses_old_framework == "Yes" ~ "WSoA (previous framework); progress made on all weaknesses",
                                    revisit_outcome_prog_vs_weaknesses_old_framework %in% c("Some", "No") ~ "WSoA (previous framework); progress not made on all weaknesses",
                                    inspect_outcome_wsoa_old_framework == "Yes" ~ "Written Statement of Action (previous framework); not revisited",
                                    la_name %in% c("North Northamptonshire", "West Northamptonshire") ~ "Area not yet inspected",
                                    .default = "No Written Statement of Action (previous framework)"), 
        box_colour = case_when(
             summary_outcome %in% c(
               "WSoA (previous framework); progress not made on all weaknesses",
               "Written Statement of Action (previous framework); not revisited",
               "Widespread and/or systemic failings (new framework)"
             ) ~ "maroon",
             summary_outcome %in% c(
               "No Written Statement of Action (previous framework)",
               "Typically positive experiences and outcomes (new framework)",
               "WSoA (previous framework); progress made on all weaknesses"
             ) ~ "green",
             summary_outcome == "Inconsistent experiences and outcomes (new framework)" ~ "orange", 
             summary_outcome == "Area not yet inspected" ~ "black"
           )) %>% 
  rename(
    `Publication date (previous inspection framework)` = inspect_publish_date_old_framework,
    `Inspection outcome: written statement of action required? (previous inspection framwork)` = inspect_outcome_wsoa_old_framework,
    `Revisit outcome: sufficient progress in addressing all significant weaknesses? (previous inspection framework)` = revisit_outcome_prog_vs_weaknesses_old_framework,
    `Inspection outcome (new inspection framework)` = inspection_outcome_new_framework,
    `Priority action plan required? (new inspection framework)` = priority_action_plan_required_new_framework,
    `Inspection publication date (new inspection framework)` = inspect_publish_date_new_framework) 
 

# Specify when ADCS spreadsheet has been updated here
ofsted_data_updated <- "Inspection reports data last updated 11th December 2023."


# Next 

#=================================
# Community Health - waiting times 
#=================================
# This is a multi-step process 

### 1. Goes to landing page where all publications are presented
# create empty vectors  
date_vector <- c()
link_vector <- c()

# Nested loop to combine and print
#some string manipulation is required because of financial years being used 
for (year in 2022:2023) {
  for (monthnum in c(4:12, 1:3)) {
    if(monthnum < 10){
      url_month <- paste0("0", as.character(monthnum))
    } else {
      url_month <- as.character(monthnum)
    }
    financial_year <- if(monthnum > 5){  # this rather odd number is the combination of financial years and the lag in when publications happen
      paste(year, str_split_i(as.character(year + 1), "0", 2), sep = "-")
    } else {
      paste(year-1, str_split_i(as.character(year), "0", 2), sep = "-")
    }
    
    link <- paste0(
      "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/", year, "/", url_month, "/", 
      "Community-health-services-waiting-lists-", paste(financial_year, 
                                                        as.character(lubridate::month(ymd(010101) + months(monthnum-3), label = TRUE, abbr = FALSE)), sep = "-"), ".xlsx")
    
    date_str <- paste(year, monthnum, "01", sep = "-")
    date <- as.Date(date_str)
    
    # Append the date and link to their respective vectors
    date_vector <- c(date_vector, date)
    link_vector <- c(link_vector, link)    
  }
}

# Format the 'Date' column in your dataframe 
ch_web_df <- data.frame(Date = format(as.Date(date_vector), format = ymc_date_format), Link = link_vector) %>%
  # remove future publication dates that have live links already 
  filter(Date <= Sys.Date()) %>% 
  #manually add in some non-standard URLs
  mutate(Link = case_when(Date == ymd("2023-10-01") ~ gsub(".xlsx", "-revised.xlsx", Link),
                          Date == ymd("2023-06-01") ~ gsub(".xlsx", "-2.xlsx", Link), 
                          .default = Link))

non_standard <- ch_web_df[13,] %>%
  mutate(Link =  gsub("February", "January", Link), 
         Date = "2023-03-01")

ch_web_df <- bind_rows(ch_web_df[1:22,], non_standard) # we also don't want the original January data...
# Classify whether the link works or not 
ch_web_df$link_is_valid <- sapply(ch_web_df$Link, valid_url)
ch_web_df <- ch_web_df %>% filter(link_is_valid == T) 

# Each publication covers a month (at least for the breakdowns by waiting time we're interested in), so we need all of them

ch_time_series_link <- ch_web_df$Link
ch_dates <- ch_web_df$Date

#Since these are downloaded as Excel files rather than zip files we don't need to do the usual unzip/temp directory rigmarole
for(i in 1:length(ch_time_series_link)) {
  download.file(ch_time_series_link[i], paste0("data/nhs/", "community_health_", as.character(ch_dates[i]), ".xlsx"), quiet = TRUE, mode = "wb")
  outDir<-"data/nhs"
  ls_files <- list.files(outDir, pattern = ".xlsx")
}

#get contents
contents <- map(paste0("data/nhs/", ls_files), openxlsx::readWorkbook, sheet = "Contents")
actual_dates <- vector("character", length = length(contents))

for(i in 1:length(contents)){
  date_string <- unlist(strsplit(contents[[i]][3,], ", "))[2]
  actual_dates[i] <- date_string
}


# at this point a manual input is required - from the contents pages need to figure out which sheets are actually required
# Currently the intention is to look at people waiting > 18 weeks as the 12-18 week group is seen as the longest reasonable waiting time
# This means we want tables 4f and 4g, plus 3 for the national summary and 4 for the total size of the waiting list so can calculate proportions
sheets <- c("Table 3", "Table 4", "Table 4f", "Table 4g")
# the metrics we're using are (says Sophie) 
# speech and language therapy, occupational therapy, pediatrics, audiology (or as the tables call them)
# unfortunately there are variations in capitalisation (again), which we'll get around by listing both versions and using any_of()
relevant_columns <- c("...1", "...2", "level", "name", "Organisation Type", "Organisation type", "Organisation Name", "Organisation name", "(CYP) Audiology", "(CYP) Community paediatric service", "(CYP) Therapy interventions: Occupational therapy", "(CYP) Therapy interventions: occupational therapy", "(CYP) Therapy interventions: Speech and language", "(CYP) Therapy interventions: speech and language")
sum_relevant_rows <- c("(CYP) Audiology", "(CYP) Community paediatric service", "(CYP) Therapy interventions: Occupational therapy", "(CYP) Therapy interventions: occupational therapy", "(CYP) Therapy interventions: Speech and language", "(CYP) Therapy interventions: speech and language")
sum_relevant_columns <- c("...1", "Total waiting list", "Waiting >18-52 weeks", "Waiting >52 weeks")
ch_frames <- data.frame(file = paste0("data/nhs/", ls_files), date = actual_dates) %>% 
  cross_join(data.frame(sheet = sheets)) 

### 3. wrangles the data 
# nhs_region_lookup <- rio::import("data/lookups/Sub_ICB_Locations_to_Integrated_Care_Boards_to_NHS_England_(Region)_(July_2022)_Lookup_in_England.csv") %>%
ch_tables <- pmap(ch_frames[,c(1, 3)], import, skip = 4) # calling pmap with a data frame as an argument supplies each row as inputs in turn, so this is just an easy way to load all the tables

names(ch_tables) <- paste(ch_frames$sheet, ch_frames$date)
ch_summaries <- ch_tables[sapply(ch_tables, length) == 9] # summary tables are nine columns
ch_detail <- ch_tables[sapply(ch_tables, length) != 9] # region/LA tables aren't 

ch_summaries <- map(ch_summaries, select, any_of(sum_relevant_columns)) %>% 
                map(setnames, "...1", "Therapy") %>% 
                map(filter, Therapy %in% sum_relevant_rows) %>% 
                map2(.y = sub("Table 3", "", names(ch_summaries)), .f = \(x, y) mutate(x, Date = y)) %>% 
                map(.f = \(x) mutate(x, across(contains("aiting"), as.numeric))) %>% 
                map(.f = \(x) mutate(x, `Percentage waiting more than 18 weeks` = 100* (`Waiting >18-52 weeks` + `Waiting >52 weeks`) / `Total waiting list`))

eng_communityhealth <- bind_rows(ch_summaries) %>% 
  mutate(formattedDate = as.Date(paste("01", Date, sep = " "), format = "%d %B %Y"), 
         #fix how the therapies are formatted
         Therapy = gsub("\\(CYP\\) ", "", Therapy), # R doesn't parse the regex itself so the bracket needs to be escaped twice
         Therapy = gsub("Therapy interventions: ", "", Therapy))

eng_communityhealth$Therapy <- unlist(map(eng_communityhealth$Therapy, simpleCap))

ch_detailed <- map(ch_detail, select, any_of(relevant_columns)) %>% 
  map(setnames, c("...1", "...2"), c("Organisation Type", "Organisation Name"), skip_absent = TRUE) %>%
  map(setnames, c("level", "name"), c("Organisation Type", "Organisation Name"), skip_absent = TRUE) %>%
  map(setnames, c("(CYP) Therapy interventions: occupational therapy", "(CYP) Therapy interventions: speech and language"), c("(CYP) Therapy interventions: Occupational therapy", "(CYP) Therapy interventions: Speech and language"), skip_absent = TRUE) %>% # fix capitalisation
  map2(.y = names(ch_detail), .f = \(x, y) mutate(x, Category = case_when(grepl("^Table 4f", y) ~ "Waiting >18-52 weeks", 
                                                                          grepl("^Table 4g", y) ~ "Waiting >52 weeks", 
                                                                          grepl("^Table 4", y) ~ "Total waiting list", 
                                                                          .default = NA_character_))) %>% # this should cause warnings/errors later, which is useful since it should never happen 
  map2(.y = sub("Table 4\\D", "", names(ch_detail)), .f = \(x, y) mutate(x, Date = y)) %>% 
  map(.f = \(x) mutate(x, across(contains("aiting"), as.numeric))) 

la_communityhealth <- bind_rows(ch_detailed) %>%  # unfortunately this needs to be pivoted because it's currently the wrong way around - we want waiting times wide and services long
  mutate(Date = gsub("^ ", "", Date), 
         formattedDate = as.Date(paste("01", Date, sep = " "), format = "%d %B %Y")) %>% 
  pivot_longer(3:6, names_to = "Therapy", values_to = "Number_CYP") %>% 
  pivot_wider(names_from = Category, values_from = Number_CYP) %>% 
  mutate(`Percentage waiting more than 18 weeks` = 100*(`Waiting >18-52 weeks` + `Waiting >52 weeks`) / `Total waiting list`, 
         Therapy = gsub("\\(CYP\\) ", "", Therapy), # R doesn't parse the regex itself so the bracket needs to be escaped twice
         Therapy = gsub("Therapy interventions: ", "", Therapy))

la_communityhealth$Therapy <- unlist(map(la_communityhealth$Therapy, simpleCap))

# finally, we need to fix the organisation type column, which due to the vagaries of Excel only has the type in the first instance of each number
# to fix this we can "interpolate" type since the type of any given row is the non-NA type most immediately above it. 
# need to do this as a loop rather than with lead() and lag() since want R to fix each row sequentially 
for(i in 2:nrow(la_communityhealth)){
  if (is.na(la_communityhealth$`Organisation Type`[i])){
    la_communityhealth$`Organisation Type`[i] <- la_communityhealth$`Organisation Type`[i-1]
  }
}
write.csv(la_communityhealth, "data/nhs/la_ch_time_series_derived.csv")
write.csv(eng_communityhealth, "data/nhs/eng_ch_time_series_derived.csv")

# Next 

#------------------------------
# KS2 attainment, REVISED stats (webscraping used)
#------------------------------
# What manual changes need to be made? 
## You need to specify the latest year which has final stats, NOT provisional. 
## https://www.gov.uk/government/collections/statistics-key-stage-2 

print("MANUAL CHANGES: The only manual changes required for this metric are to make sure the revised stats are gathered, not provisional")

ks2_year_rev <- "2022-23" # !!! update this manually 

# The rest of this ks2 section is automated... 

# The usual outcome here is rounded to the nearest % which is not so helpful for ranking. Instead I will recalculate it with one figure after the decimal point.

# ks2 url we want
ks2_url <- paste0("https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment", "/", ks2_year_rev)
# Extract the year the publication refers to
KS2_year_of_pub <- read_html(ks2_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
ks2_yr_start <- "2019" # Hardcoded as the way KS2 was awarded changed in 2019 so shouldn't change 
ks2_yr_end <- paste0("20",substr(KS2_year_of_pub, start = nchar(KS2_year_of_pub) -1, stop = nchar(KS2_year_of_pub) - 0))
# State the file with correct year we want

ks2_file_name <- paste0("ks2_regional_local_authority_and_pupil_characteristics_",ks2_yr_start,"_to_",ks2_yr_end,"_revised.csv")

# Save latest ks2 data 
get_ees_data(
  url = ks2_url, 
  desired_file_name <- ks2_file_name,
  zip_subfolder_to_extract <- "data/", 
  output_dir <- "data/attainment_ks2"
)

# import and wrangle the data 
ks2_attainment <- rio::import(paste0("data/","attainment_ks2/", ks2_file_name)) %>%
  rename(
    characteristic_group = breakdown_topic,
    characteristic = breakdown) %>%
  fsubset(characteristic_group == "SEN status" &
            gender == "Total" &
            characteristic != "SEN unclassified") %>%
  fselect(
    time_period, geographic_level,
    region_name, la_name,
    characteristic, pt_rwm_met_expected_standard,
    t_rwm_met_expected_standard, t_rwm_eligible_pupils
  ) %>%
  ftransform(`Percent meeting expected standard` = round(
    100 * (as.numeric(t_rwm_met_expected_standard) / as.numeric(t_rwm_eligible_pupils)),
    digits = 2
  )) %>%
  time_period_to_academic_year()

# End

