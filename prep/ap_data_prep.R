
#==============================
# Schools, pupils, characteristics 
#==============================
# https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics

# Import data 
spc_url <- "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics"
spc_year_of_pub <- read_html(spc_url) %>% html_nodes(".govuk-caption-xl") %>% html_text()
print(spc_year_of_pub)

spc_ls <- c("spc_school_characteristics_.csv", # State funded AP
            "spc_pupils_fsm_ethnicity_yrgp.csv", # State funded AP
            "spc_school_ap_placement.csv", # School arranged AP
            "spc_ap_setting.csv", # placements at LA funded AP
            "ap_placements_ap_census.csv") # LA funded AP)
for(i in spc_ls) {  
get_ees_data(
  url = spc_url, 
  desired_file_name <- i,
  zip_subfolder_to_extract <- "data/", 
  output_dir <- "data/ap/")
}

#------------------------------
# Pupil numbers by type of AP
#------------------------------

# State funded AP 
spc_sfap <- rio::import(paste0("data/", "ap/", "spc_school_characteristics_.csv")) %>%
  filter(
    sex_of_school_description == "Total",
    phase_type_grouping == "State-funded AP school",
    type_of_establishment != "Total",
    denomination	== "Total",
    admissions_policy == "Total",
    urban_rural == "Total",
    academy_flag == "Total") %>%
  mutate(type_of_establishment = case_when(
    type_of_establishment %in% c("Pupil referral unit", "Pupil Referral Unit") ~ "Pupil Referral Unit",
    type_of_establishment %in% c("Free Schools - Alternative Provision", "Free schools alternative provision") ~ "Free Schools - Alternative Provision",
    T ~ type_of_establishment)) %>%
  rename(prov_type = phase_type_grouping,
         setting_type = type_of_establishment,
         academic_year = time_period,
         total_pupils = headcount_of_pupils) %>%
  mutate(academic_year = as.character(academic_year)) %>%
  select("academic_year",
         "geographic_level",
         "region_name",
         "old_la_code",
         "new_la_code",
         "la_name",
         "prov_type",
         "setting_type",
        # "number_of_schools",
         "total" = "total_pupils"
         #"headcount_total_girls",
         #"headcount_total_boys"
  ) 

# School arranged AP 
spc_sap <- rio::import(paste0("data/", "ap/", "spc_school_ap_placement.csv")) %>%
  filter(pupil_characteristic == "Total",
         characteristic_grouping == "Total",
         setting_type != "Total") %>%
  rename(academic_year = time_period,
         total_pupils = pupils) %>%
  mutate(prov_type = "School arranged AP") %>%
  select("academic_year",
         "geographic_level",
         "region_name",
         "old_la_code",
         "new_la_code",
         "la_name",
         "prov_type",
         "setting_type",
          "total" =  "total_pupils")

# LA funded AP 
spc_lfap <- rio::import(paste0("data/", "ap/", "spc_ap_setting.csv")) %>%
  mutate(prov_type = "LA funded AP placements") %>%
  select("academic_year" = "time_period",
         "geographic_level",
         "region_name",
         "old_la_code",
         "new_la_code",
         "la_name",
         "prov_type",
         "setting_type",
         "total" = "placements")

ap_counts <- rbind(spc_sfap, spc_sap, spc_lfap)
# LA arranged AP 

p <- df %>%
  # Insert dynamic filters here 
  fsubset(
    la_name == "Bristol, City of" &
    prov_type == "LA funded AP placements") %>%
  # State-funded AP school
  #School arranged AP
  # LA funded AP placements
  ggplot(aes(
    x = academic_year,
    y = total,
    group = prov_type,
    fill = setting_type,
    label = paste0(total)
    )) +
  geom_col(position = "stack") +
  geom_text(
    size = 3,
    position = position_stack(vjust = 0.5)
  ) +
  labs(
    x = "Academic year",
    fill = "AP setting type",
    label = "xxxx"
  ) +
  scale_fill_manual(values = dest_1618_palette)
  

#------------------------------
# Pupil characteristics in AP
#------------------------------


         




# Include age, SEN, ethnicity, and FSM

#==============================
# Outcomes 
#==============================

#------------------------------
# KS4 English and maths attainment
#------------------------------

#==============================
# Experiences 
#==============================
#------------------------------
# Ofsted rating (state-funded schools)
#------------------------------

#------------------------------
# Absence rate 
#------------------------------
