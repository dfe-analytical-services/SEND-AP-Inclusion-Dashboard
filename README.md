## Introduction

This is a prototype SEND Inclusion Dashboard, as promised in the SEND and AP Green Paper. It includes data from the Department for Education, NHS England and HM Courts and Tribunals service, presented to show changes over time and geographic variation at both local and regional levels.

Production dashboard: <https://rsconnect/rsc/content/8d936632-4898-441a-a2fd-a532c4b669a7/> Pre-Production dashboard: <https://rsconnect-pp/rsc/content/1ea3ef3c-0233-49ac-9317-29d8cfb69688/>

------------------------------------------------------------------------

## Requirements

### Software requirements (for running locally)

It may be possible to run the dashboard without any or all of the below; but we do not recommend it. These were used for development and are thus known to be working.

-   Installation of RStudio 2023.03.1+446 ("Cherry Blossom") Release or later
-   Installation of R 4.2.3 ("Shortstop Beagle") or higher
-   Installation of RTools 40 or higher

## Skills required (for troubleshooting and development)

-   Intermediate level R - see eg [Advanced R](https://adv-r.hadley.nz/) by Hadley Wickham
-   Particularly [R Shiny](https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/index.html)
-   To alter the "look and feel" of the dashboard some understanding of Bootstrap and CSS may be useful

## Data requirements

-   The dashboard stores the data cuts it uses locally (in the /data/ directory) for speed, so no specific access is required. The sources for each particular metric are linked in the "About this indicator" tabs on the right-hand side of the screen.
-   Note that the dashboard does not (yet) update itself automatically; this is a feature planned for the future once development on the API for Explore Education Statistics has reached a suitable stage.

------------------------------------------------------------------------

## How to use

### Running the app locally

1.  Clone or download the repo.

2.  Open the R project in R Studio.

3.  Run `renv::restore()` to install dependencies.

4.  Run `shiny::runApp()` to run the app locally.

### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest that test the app loads, that content appears correctly when different inputs are selected, and that tab content displays as expected. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function run_tests_locally() is created in the Rprofile script and is available in the RStudio console at all times to run both the unit and ui tests.

### Deployment

-   The app is deployed to the department's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.

------------------------------------------------------------------------

## How to Contribute

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only the SENDAP Analysis team can currently merge pull requests. Add isfraser and [placeholder for additional reviewers in the stats development team] as requested reviewers, and the team will review before merging.

------------------------------------------------------------------------

## Contact

To contact the maintainers of this dashboard email [the SENDAP Dashboard team](mailto::sendap.dashboard@education.gov.uk)
