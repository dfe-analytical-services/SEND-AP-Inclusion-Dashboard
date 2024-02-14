# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest2))
shhh(library(diffviewer))
shhh(library(bslib))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(plotly))
shhh(library(DT))
shhh(library(rio))
shhh(library(collapse))
shhh(library(shinycssloaders))
shhh(library(ggrepel))
shhh(library(ggiraph))
shhh(library(geomtextpath))
shhh(library(lubridate))
shhh(library(stringr))
shhh(library(fresh))
shhh(library(shinyalert))
shhh(library(shinyscroll))
shhh(library(scales))
shhh(library(dfeshiny))
shhh(library(shinydashboard))
shhh(library(dplyr))
shhh(library(tidyr))
shhh(library(tibble))


# shhh(library(shinya11y)) #accessibility testing only in local version
# shhh(library(rlang))

# Source scripts and data---------------------------------------------------------------------------------

source("R/functions.R")
# source("R/support_links.R")
google_analytics_key <- "23W9Y1400B"
# Load in pre-processed data
load("data/prepared_data.Rdata")

# App-wide variables ---------------------------------------------------------------------------------

options(scipen = 99999)
# Useful Variables
spinner_type <- 1 # Defines the spinner style

# defines width of panels
graph_width <- 7
info_width <- 4

# reactive values used to control tab updates - get updated with where we're linking to
move_here <- reactiveVal("")
move_target <- reactiveVal("")
this_tab <- reactiveVal("")
trigger <- reactiveVal(FALSE)
no_dest_data <- reactiveVal(FALSE)

useShinyjs() # Activates shinyjs which allows toggling of visibility for some objects

# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

# site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/"
# site_overflow <- "https://department-for-education.shinyapps.io/dfe-shiny-template-overflow/"

# Theme and Graph settings ---------------------------------------------------------------------------------

title_font <- list(size = 14)

theme_set(theme_classic(base_size = 10))
theme_update(
  plot.title = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10),
  axis.title = element_text(size = 10)
)

# Default height of sparklines in pixels
sparkline_height <- 170

# Define months in text so they can be ordered in tables (R won't allow a date without a day, and if you order
# the text DT reorders it alphabetically)

month_order <- seq.Date(ymd("2017-01-01"), floor_date(Sys.Date(), "month"), "month") %>%
  format("%b %Y")

month_order <- factor(month_order, levels = month_order)
# Colours  ---------------------------------------------------------------------------------

# set colours for native R
af_darkblue <- "#12436D" # EHCP
af_turquoise <- "#28A197" # SEN support
af_darkpink <- "#801650" # All SEN
af_orange <- "#F46A25" # No SEN
af_grey <- "#3D3D3D" # My area
af_purple <- "#A285D1" # Other areas

af_palette <- c(af_darkblue, af_darkblue, af_darkblue, af_darkblue, af_turquoise, af_turquoise, af_turquoise, af_darkpink, af_darkpink, af_darkpink, af_darkpink, af_orange, af_orange, af_grey, af_purple, "#667585", "#9A8574", af_darkblue)

names(af_palette) <- c("EHC plan", "EHC plan or Statement", "Compulsory schooling age", "CINO at 31 March", "Above compulsory schooling age", "SEN support", "CLA 12 months at 31 March", "All SEN", "Identified SEN", "Identified SEN (mainstream)", "CPPO at 31 March", "No SEN", "No identified SEN", "My area", "Other areas", "Identified LLDD (mainstream)", "No identified LLDD", "Specialist provision")
af_gradient <- c("#0b2841", "#0e3657", "#12436d", "#41698a", "#718ea7")

# set same for summary boxes because shinydashboard doesn't natively allow user-defined colours, so have to hack the
# defaults using the fresh package
dfe_colours <- create_theme(
  adminlte_color(
    green = "#00857c", # was af_turquoise but not enough contrast for white text
    blue = af_darkblue,
    red = af_darkpink,
    orange = af_orange,
    black = af_grey,
    purple = "#7947b6", # was af_purple but not enough contrast for white text
    aqua = "#1d70b8"
  )
)


# focus_colours
focused <- "#12436D"
not_focused <- "#BFBFBF"


dest_palette <- c(
  "#CC7A88", "#B33E52",
  "#C7B8E6", "#967ACC",
  "#E6D2B8", "#CCAA7A", "#B3823E", "#B3823E", "#7a592a",
  "#12436D"
)

names(dest_palette) <- c("Unknown", "Not sustained", "Apprenticeship", "Employment", "Education (other)", "Further education", "Higher education", "School sixth form", "Sixth form college", "Overall sustained")

ap_palette <- c(
  "#12436d", "#B33E52",
  "#C7B8E6", "#967ACC",
  "#E6D2B8", "#CCAA7A", "#B3823E", "#7a592a"
)

ofsted_palette <- c("#73a773", "red", "red", "#73a773", "orange", "#73a773", "red", "white")
names(ofsted_palette) <- c(
  "No Written Statement of Action (previous framework)",
  "Written Statement of Action (previous framework); not revisited",
  "WSoA (previous framework); progress not made on all weaknesses",
  "WSoA (previous framework); progress made on all weaknesses",
  "Inconsistent experiences and outcomes (new framework)",
  "Typically positive experiences and outcomes (new framework)",
  "Widespread and/or systemic failings (new framework)",
  "Area not yet inspected"
)

nhs_palette <- c(af_darkblue, af_turquoise, af_darkpink, af_orange)
names(nhs_palette) <- c("Audiology", "Community Paediatric Service", "Occupational Therapy", "Speech And Language")

char_gradient_ap <- c("#0b2841", "#12436d", "#41698a", "#718ea7", "#28A197", "black")

# Input lists (LA/Regions) ---------------------------------------------------------------------------------

# List of regions
region_list <- absence_regional %>%
  ungroup() %>%
  select(region_name) %>%
  distinct() %>%
  arrange(region_name) %>%
  filter(region_name != "England") %>%
  mutate(ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and \nThe Humber", region_name)) %>% # newline to make axes labels read better
  pull(region_name)

# List of LAs - this is more fussy as it's split into separate lists
# which allows them to be in sub-sections of the menu
la_name_list_pre <- la_region_lookup %>%
  arrange(la_name) %>%
  pull(la_name)

# Add a blank
la_name_list <- c("", la_name_list_pre)

# Split the LA lookup into regions

split2 <- la_region_lookup %>%
  split(f = as.factor(.$region))

# Bring the split pieces of the LA lookup into the environment
list2env(split2, envir = .GlobalEnv)

# Create a function to turn each 2-part list into a single list with the name of the region as the variable name
clean_and_split <- function(input_df, obj_name) {
  input_df %>%
    select(la_name) %>%
    rename({{ obj_name }} := la_name)
}

# Clean up the regional lists
`North East` <- `North East` %>% clean_and_split(obj_name = "North East")
`North West` <- `North West` %>% clean_and_split(obj_name = "North West")
`South West` <- `South West` %>%
  clean_and_split(obj_name = "South West") %>%
  fsubset(`South West` != "Isles of Scilly") # Isles of Scilly removed so you can't select it as removed from benchmarking
`South East` <- `South East` %>% clean_and_split(obj_name = "South East")
`East of England` <- `East of England` %>% clean_and_split(obj_name = "East of England")
`Yorkshire and the Humber` <- `Yorkshire and The Humber` %>% clean_and_split(obj_name = "Yorkshire and the Humber")
`East Midlands` <- `East Midlands` %>% clean_and_split(obj_name = "East Midlands")
`West Midlands` <- `West Midlands` %>% clean_and_split(obj_name = "West Midlands")
`London` <- `London` %>%
  clean_and_split(obj_name = "London") %>%
  fsubset(London != "City of London") # City of London removed so you can't select it as removed from benchmarking

# LAs to be filtered for most benchmarking graphs because their small populations break things
small_LAs <- c("Isles of Scilly", "City of London")

# Input lists (NHS geographies) ---------------------------------------------------------------------------------

# List of NHS orgs
nhs_list_pre <- nhs_lookup %>%
  pull(nhs_name)

# Add a blank
nhs_list <- c("", nhs_list_pre)

# Split the LA lookup into regions

split2_nhs <- nhs_lookup %>%
  mutate(nhs_region = paste0(nhs_region, " (NHS)")) %>%
  split(f = as.factor(.$nhs_region))

# Bring the split pieces of the LA lookup into the environment
list2env(split2_nhs, envir = .GlobalEnv)


# Create a function to turn each 2-part list into a single list with the name of the region as the variable name
nhs_clean_and_split <- function(input_df, obj_name) {
  input_df %>%
    select(nhs_name) %>%
    rename({{ obj_name }} := nhs_name)
}

# Clean up the regional lists
`East of England (NHS)` <- `East of England (NHS)` %>%
  nhs_clean_and_split(obj_name = "East of England (NHS)")
`London (NHS)` <- `London (NHS)` %>%
  nhs_clean_and_split(obj_name = "London (NHS)")
`Midlands (NHS)` <- `Midlands (NHS)` %>%
  nhs_clean_and_split(obj_name = "Midlands (NHS)")
`North East and Yorkshire (NHS)` <- `North East and Yorkshire (NHS)` %>%
  nhs_clean_and_split(obj_name = "North East and Yorkshire (NHS)")
`North West (NHS)` <- `North West (NHS)` %>%
  nhs_clean_and_split(obj_name = "North West (NHS)")
`Provider (NHS)` <- `Provider (NHS)` %>%
  nhs_clean_and_split(obj_name = "Provider (NHS)")
`South East (NHS)` <- `South East (NHS)` %>%
  nhs_clean_and_split(obj_name = "South East (NHS)")
`South West (NHS)` <- `South West (NHS)` %>%
  nhs_clean_and_split(obj_name = "South West (NHS)")
