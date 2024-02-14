# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#



ui <- function(input, output, session) {
  # Register Open Sans google font, or else the Summary graphic will appear with serif fonts and no label text#
  # gdtools::register_liberationsans()
  #  gdtools::register_gfont(family = "Open Sans")
  #  gdtools::liberationsansHtmlDependency()
  #  gdtools::addGFontHtmlDependency(family = "Open Sans")

  fluidPage(
    title = tags$head(
      tags$link(
        rel = "shortcut icon",
        href = "dfefavicon.png"
      ),
      # Add title for browser tabs
      tags$title("SEND and AP Dashboard")
    ),
    tags$html(lang = "en"),
    shinyjs::useShinyjs(),
    useShinydashboard(),
    use_shinyscroll(),
    # Setting up cookie consent based on a cookie recording the consent:
    dfe_cookie_script(),
    cookie_banner_ui("cookies", name = "SEND and AP Dashboard"),
    use_theme(dfe_colours),
    # use_tota11y(),  #only do this in dev version for accessibility testing
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "additional.css"
      )
    ),
    shinyGovstyle::header(
      main_text = "",
      main_link = "https://www.gov.uk/government/organisations/department-for-education",
      secondary_text = "SEND and AP dashboard",
      logo = "images/DfE_logo_landscape.png",
      logo_width = 150,
      logo_height = 32
    ),
    shinyGovstyle::banner(
      "beta banner",
      "beta",
      "This Dashboard is in development. We are still reviewing performance and reliability."
    ),
    shiny::navlistPanel(
      "",
      id = "navlistPanel",
      widths = c(2, 10),
      well = FALSE,
      homepage_panel(),
      regional_dashboard_panel(),
      dashboard_panel(),
      a11y_panel(),
      support_panel(
        team_email = "sendap.dashboard@education.gov.uk",
        repo_name = "SEND-AP-Inclusion-Dashboard",
        form_url = NULL,
        ees_publication = TRUE
      )
    ),
    tags$script(
      src = "script.js"
    ),
    footer(full = TRUE)
  )
}
