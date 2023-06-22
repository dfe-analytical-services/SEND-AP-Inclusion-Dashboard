# Script for creating an LA to Region Lookup Table

library(tidyverse)

# Download a custom table in ODS format from: https://explore-education-statistics.service.gov.uk/data-tables/permalink/b5ad776d-bdb8-42d0-02e5-08daf870ec67
# Re-save as XLSX
la_region_lookup_raw <- rio::import("sourcedata/lookups/la_region_lookup_raw.xlsx", skip = 2 ) 

la_region_lookup <- la_region_lookup_raw %>% 
  rename("region" = `...1`,
         "la_name" = `...2`) %>% #give human-readable neames
  select(-`2021/22`) %>%  #remove actual data as we're not interested in that for this
  drop_na(la_name) %>% #remove blank rows, comments etc
 fill(region) #the source data uses merged cells, fill the gaps in the region column

save(la_region_lookup, file = "sourcedata/lookups/la_region_lookup.Rdata")
