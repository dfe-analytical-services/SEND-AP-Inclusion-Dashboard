homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("SEND and AP Inclusion Dashboard Prototype"),
          br(),
          br()
        ),

        ## Left panel -------------------------------------------------------

        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "Contents Box",
                  h3("Introduction"),
                  p("This app is the prototype Special Educational Needs and Disabilities (SEND) and Alternative Provision (AP) Inclusion Dashboard."),
                  p("This dashboard presents published SEND and AP performance data across education, health and care (EHC) at an England, regional and local area level. This prototype has been developed as part of a user testing phase to help refine in readiness for public use later this year."),
                  p(strong("How to navigate the dashboard")),
                  p("The sidebar on the left can be used to navigate the dashboard. Each page has a summary tab which gives an overview of all metrics on one page. The tabs break down the metrics into themes of outcomes, experiences, identification of need, and financial sustainability. More detailed information is available in these tabs, including the option to make comparisons, or look at changes over time. The graphs should be read alongside the guidance text to support understanding."),
                  tags$ul(
                    tags$li(actionLink("link_to_engreg_dashboard", "England and regional (national) dashboard – provides a breakdown of metrics using time series and comparisons at an England and regional level.")),
                    tags$li(actionLink("link_to_la_dashboard", "Local areas dashboard – provides breakdowns for the selected area (Local Authority (LA) and Integrated Care Board) using time series and comparison function at local area level (see comment on local government unitarisation below).")),
                    tags$li(actionLink("link_to_accessibility", "Accessibility - provides the dashboard accessibility statement, compliance requirements, limitations, and the opportunity to feedback on the accessibility of the dashboard.")),
                    tags$li(actionLink("link_to_feedback", "Support and feedback – provides links for feedback and if you have any questions about the dashboard, or the data it contains. There is also a link to the GitHub repository if you wish to view the dashboard source code."))
                  ),
                  p("The dashboard will be kept up to date with the latest data shortly after it is released though there may be some lag. The metrics in the prototype will be updated for publication later this year and then reviewed on an annual basis."),
                  p("This dashboard is not all encompassing of all SEND and AP data - more information, along with methodological information, can be found on", a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics.")),
                  p(strong("Local Government Unitarisation")),
                  p("In 2021 Northamptonshire county area was split into two unitary authorities: West Northamptonshire and North Northamptonshire. This impacts data comparison pre and post 2021. We have made it possible to view data for Northamptonshire Local Authority to 2021 and both West and North Northamptonshire for 2021 onwards."),
                  p("This year, 2023, Cumbria will also be split into two unitary authorities and single unitary councils will be created in North Yorkshire and Somerset."),
                  p("As a result, whenever an LA is selected the dashboard’s ‘comparison’ tabs will show data for the last year that LA existed.")
                ),
                br()
              )
            )
          ),
        ),

        ## Right panel ------------------------------------------------------

        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Background Information")
              ),
              div(
                class = "panel-body",
                h3("Context and purpose"),
                p("The dashboard is part of the intended reforms to the SEND and AP system, as set out in the", a(href = "https://www.gov.uk/government/publications/send-and-alternative-provision-improvement-plan", "Improvement Plan"), ", which aims to improve outcomes for children and young people with SEND and in AP, improve experiences for families, and deliver financial sustainability."),
                p("The dashboard will eventually be fully accessible to the public and will present timely performance data across education, health and care. The dashboard will support better use of data in the system through:"),
                tags$ul(
                  tags$li("Improving public transparency"),
                  tags$li("Strengthening accountabilities for all those responsible for local delivery"),
                  tags$li("Enabling better decision making at a national and local level"),
                  tags$li("Supporting the development of local inclusion plans and driving self-improvement")
                ),
                p(
                  "The prototype has been developed with multiple users in mind, including LAs, SEND and AP providers, and parents and carers. During the",
                  a(href = "https://www.gov.uk/government/consultations/send-review-right-support-right-place-right-time", "Green Paper"),
                  "consultation we heard that careful consideration must be given to the dashboard before publication to ensure we develop a tool that helps us achieve our overall objectives, considers the various users in the system, and sets out relevant and useful metrics. The user testing phase will run from Spring 2023 through to the end of the year, when we will publish a first version of the dashboard. Development of the dashboard will continue with ongoing updates and iterations in response to user feedback."
                ),
                p(strong("Related Dashboards")),
                p("Users may also wish to look at the related dashboards below:"),
                tags$ul(
                  tags$li(a(href = "https://lginform.local.gov.uk/reports/view/send-research/local-area-send-report?mod-group=AllSingleTierAndCountyLaInCountry_England&mod-area=E92000001&mod-type=namedComparisonGroup", "LG Inform Dashboard")),
                  tags$li("Further Education (FE) Performance Dashboard – In development"),
                  tags$li("Children’s Social Care (CSC) Dashboard – In development")
                ),
                p("Regional Dashboards - links to be added")
              )
            )
          )
        )
      )
    )
  )
}


dashboard_panel <- function() {
  tabPanel(
    value = "la_dashboard",
    "Local Areas",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Compare local area SEND and AP indicators"),
        ),
        column(
          width = 12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              column(
                width = 12,
                pickerInput("la_choice",
                  label = h2("Select local authority to display graphs"),
                  choices = list(
                    "Please Select Local Authority" = "",
                    "East Midlands" = `East Midlands`,
                    "East of England" = `East of England`,
                    "London" = `London`,
                    "North East" = `North East`,
                    "North West" = `North West`,
                    "South East" = `South East`,
                    "South West" = `South West`,
                    "West Midlands" = `West Midlands`,
                    "Yorkshire and the Humber" = `Yorkshire and the Humber`
                  ),
                  choicesOpt = list(style = ("color: darkgrey")),
                  selected = NULL,
                  options = pickerOptions(
                    livemaxOptions = 1,
                    liveSearch = TRUE
                  )
                ),
                pickerInput("ccg_choice",
                  label = h3("Select NHS sub-ICB location (former CCG area) to display NHS graphs"),
                  choices = list(
                    "Please Select NHS area" = "",
                    "East of England" = `East of England (NHS)`,
                    "London" = `London (NHS)`,
                    "Midlands" = `Midlands (NHS)`,
                    "North East and Yorkshire" = `North East and Yorkshire (NHS)`,
                    "North West" = `North West (NHS)`,
                    "South East" = `South East (NHS)`,
                    "South West" = `South West (NHS)`
                  ),
                  selected = NULL,
                  options = pickerOptions(
                    livemaxOptions = 1,
                    liveSearch = TRUE
                  )
                ),
                materialSwitch(
                  inputId = "myregion_switch", label = "Compare to local areas in the same region only",
                  right = TRUE
                )
              )
            ),
            htmlOutput("la_changed")
          )
        ),
        column(
          width = 12,
          tabsetPanel(
            id = "tabsetpanels_la",
            tabPanel(
              "Summary",
              fluidRow(
                column(
                  width = 12,
                  h2("Local authority summary"),
                  box(
                    width = 12,
                    p("This graphic provides an overview of where the selected LA ranks among the largest 150 local authorities in England for each metric.
                                 The City of London and the Isles of Scilly are excluded from comparisons as their small populations confuse rate or average statistics.
                                   Being ranked higher or lower than average is not necessarily indicative of better/worse performance;
                                   however, it provides a starting point for understanding how different areas vary."),
                    p("Within each theme, the circles represent a metric on the dashboard, and the number in the circle is the selected LA's rank in the most current data for this metric.
                      Names of metrics are shown alongside the circle. The median (middle), highest and lowest LAs are shown by the horizontal lines.
                      Hover over a circle with your mouse for a more detailed description of the metric.
                      Where two metrics are very close (e.g. if the ranks are 64 and 65), one circle is moved to the side and labels may be removed, for clarity."),
                    radioGroupButtons(
                      "la_sum_toggle",
                      label = NULL,
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    withSpinner(
                      type = spinner_type,
                      ui_element = uiOutput("la_summary_tog")
                    )
                  )
                )
              ),
            ),
            tabPanel(
              "Outcomes",
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 1 phonics check"),
                  h3("% of pupils with SEN  meeting the expected standard in the phonics screening check in year 1"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks1_phonics_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "phonics_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_la_time_tog"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("ks1_phonics_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan", "SEN support", "No SEN"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "phonics_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_la_bench_tog", height = "120%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These are the first phonics screening check and key stage 1 attainment statistics since 2019, after assessments were cancelled in 2020 and 2021 due to the pandemic. These statistics cover the attainment of year 1 and year 2 pupils who took these assessments in summer 2022. These pupils experienced disruption to their learning during the pandemic. It includes all pupils who participated in the phonics screening check, those disapplied, those absent for the entire period of which the check could be administered and those without results due to maladministration. Pupils with missing or invalid results are not included in the calculations."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-1-and-phonics-screening-check-attainment/2021-22",
                      "Source: Key stage 1 and phonics screening check attainment."
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 2 attainment"),
                  h3("% of pupils with SEN reaching expected standards (in reading, writing and mathematics) at KS2"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks2_attainment_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ks2_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_la_time_tog", height = "110%")))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("ks2_attainment_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan", "SEN support"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "ks2_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics concern the attainment of year 6 pupils who took assessments in summer 2022. These pupils experienced disruption to their learning during the pandemic, particularly at the end of year 4 and in year 5. Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks. "),
                    p("Figures for 2022 are based on revised data. Figures for other years are based on final data. For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment",
                      "Source: Key stage 2 attainment."
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 4 attainment"),
                  h3("Average Progress 8 score, for pupils with and without SEN"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks4_attainment_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        htmlOutput("nhants_excuse"),
                        radioGroupButtons(
                          "ks4_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        htmlOutput("nhants_excuse_bench"),
                        selectInput("ks4_attainment_la_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Any SEN", "No identified SEN"),
                          selected = "Any SEN"
                        ),
                        radioGroupButtons(
                          "ks4_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Progress 8 aims to capture the progress that pupils in a school make from the end of primary school to the end of KS4. It is a type of value-added measure, which means that pupils’ results in up to 8 qualifications are compared to other pupils nationally with similar prior attainment."),
                    p("This academic year saw the return of the summer exam series, after they had been cancelled in 2020 and 2021 due to the impact of the COVID-19 pandemic, where alternative processes were set up to award grades (centre assessment grades, known as CAGs, and teacher assessed grades, known as TAGs). As part of the transition back to the summer exam series, adaptations were made to the exams (including advance information) and the approach to grading for 2022 exams broadly reflected a midpoint between results in 2019 and 2021. "),
                    p("Progress 8 scores in 2022 will also have been impacted by the changes to methodology in the way 2022 performance measures were calculated, as results achieved between January 2020 and August 2021 were not included in the measures."),
                    p(strong("Currently Progress 8 scores broken down by SEN description are not available for 2018/19, so no comparison over time is possible. These scores will be added in the near future.")),
                    tags$div(
                      "More information on these changes, and how Progress 8 is calculated, can be seen in the",
                      tags$a(
                        href = "https://www.gov.uk/government/news/guide-to-gcse-results-for-england-summer-2022",
                        "Guide to GCSE results for England, summer 2022."
                      )
                    ),
                    tags$a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1134998/Secondary_accountability_measures_2022_guidance_for_maintained_secondary_schools__academies_and_free_schools.pdf", "Secondary accountability measures: 2022 guidance for maintained secondary schools, academies and free schools (publishing.service.gov.uk)"),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised", "Source: Key stage 4 Performance")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupil destinations after 16-18 study"),
                  h3("% of children and young people withSEN post 16-18 in employment, training or higher education"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "destinations_1618_la_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("destinations_1618_la_time_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "Identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "dest18_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        htmlOutput("nhants_excuse_dest"),
                        selectInput("destinations_1618_la_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "Identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "dest18_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_la_bench_tog", height = "800px"))
                      ),
                      tabPanel(
                        "Provision type comparison",
                        htmlOutput("nhants_excuse_comp"),
                        selectInput("destinations_1618_la_type_filter",
                          label = "Select destination for comparison",
                          choices = list(
                            "Sustained employment destinations" = c(
                              "Employment",
                              "Apprenticeship"
                            ),
                            "Sustained education destinations" = c(
                              "Further education",
                              "Higher education",
                              "Education (other)"
                            ),
                            "Not sustained or unknown destinations" = c("Not sustained", "Unknown")
                          ),
                          selected = "Employment"
                        ),
                        radioGroupButtons(
                          "dest18_typ_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_la_type_tog", height = "600px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Destination measures show the percentage of students going to or remaining in an education, apprenticeship or employment destination in the academic year after completing 16 to 18 studies (finishing year 13, usually aged 18). The most recent data reports on students who completed this stage in the academic year 2019 to 2020 and identifies their main activity in the following year (2020 to 2021)."),
                    p("These official statistics show students continuing to education, apprenticeship or employment destinations in the year after completing 16 to 18 study in schools and colleges in England."),
                    strong("Note: \"Identified SEN\" refers to pupils who completed 16-18 study in schools (e.g. school sixth forms) who are identified as having special educational needs,
                       whereas \"Identified LLDD\" refers to pupils who completed 16-18 study in colleges (e.g. sixth form colleges) with a self-declared Learning Difficulty, Disability or health problem."),
                    p("In 2020/21, 13,753 pupils identified with SEN completed 16-18 study, compared to 91,897 with self-declared LLDD."),
                    p("The latest data was affected by COVID-19. For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/16-18-destination-measures",
                      "Source: 16-18 destinations. "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Mental Health service access"),
                  h3("Children and young people accessing NHS-funded mental health support"),
                  box(
                    width = 12,
                    valueBoxOutput(outputId = "nhs_value_box_ccg_newest", width = 5),
                    valueBoxOutput(outputId = "nhs_value_box_ccg_older", width = 5)
                  ),
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "mentalhealth_ccg_panel",
                      tabPanel(
                        "Change over time (National comparison)",
                        p(),
                        radioGroupButtons(
                          "mh_cgt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mentalhealth_ccg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Change over time (CCGs comparison)",
                        p(),
                        radioGroupButtons(
                          "mh_cgb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mentalhealth_ccg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This data shows the number of children accessing NHS-funded mental health services at least once in the previous (rolling) twelve-month period."),
                    p("Some providers' data are missing for some months in 2022 as a result of a cyber incident. Sub-Integrated Care Board (ICB) (formerly Clinical Commissioning Group (CCG) areas) breakdowns are not an accurate reflection of activity for Performance November 2022, although some ICBs/Sub ICBs/Commissioning Regions are impacted more than others."),
                    p("For full details and limitations, see the full NHS publication linked below."),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics",
                      "Source: Mental Health Services Monthly Statistics."
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Local Area SEND Inspection ratings"),
                  h3("Ratings from the last joint Ofsted/CQC inspection"),
                  box(
                    width = graph_width,
                    withSpinner(
                      type = spinner_type,
                      ui_element = valueBoxOutput("la_ofsted_rating", width = 12)
                    ),
                    p(ofsted_data_updated)
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This table shows the outcome of the most recent Local Area SEND area inspection. These inspections are carried out jointly by Ofsted and the Care Quality Commission (CQC)."),
                    tags$div(
                      "Source: ADSC collation of Ofsted/CQC reports from ",
                      tags$a(
                        href = "https://adcs.org.uk/inspection/article/send-inspection-outcomes-summary",
                        "adcs.org.uk/inspection/"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Alternative provision (AP) outcome measures"),
                  h3("Five proposed AP Metrics"),
                  box(
                    width = graph_width,
                    tags$ol(
                      tags$li("Improved Attendance"),
                      tags$li("Academic Attainment, with a focus on English and Maths"),
                      tags$li("Re-Integration into Mainstream Education"),
                      tags$li("Progression to sustainable post-16 destinations"),
                      tags$li("Effective AP outreach support for mainstream schools")
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The proposed new alternative provision metrics are still in development. When available, following testing, they will be displayed in this dashboard. These metrics were described in the 2023 SEND and AP Improvement Plan as part of a bespoke alternative provision performance framework. These will be designed to set robust standards, but the exact metrics are yet to be finalised."),
                  )
                )
              )
            ), # This close bracket is the end of the Outcomes panel.



            tabPanel(
              "Experiences",
              fluidRow(
                column(
                  width = 12,
                  h2("EHCP timeliness"),
                  h3("% EHCPs finalised within 20 weeks excluding exceptions"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "timeliness_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "time_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("timeliness_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        radioGroupButtons(
                          "time_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("timeliness_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The whole process of EHC needs assessment and EHC plan development, from the point when an assessment is requested (or a child or young person is brought to the local authority’s attention) until any final EHC plan is issued, must take no more than 20 weeks. The relevant legislation provides for certain exceptions to the time limits."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans in England (SEN2). "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("SEND Tribunal appeal rate"),
                  h3("% of total appealable decisions resulting in an appeal to the SEND Tribunal"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "tribunals_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "trib_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("tribunals_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        radioGroupButtons(
                          "trib_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("tribunals_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The appeal rate is based on Total Appealable Decisions, which is calculated from data collected by the DfE from the annual SEN2 data return, which is mandatory for local authorities to complete. The Total Appealable Decisions figure is calculated as the sum total of the following:"),
                    tags$ul(
                      tags$li("Number of initial requests for Education, Health and Care (EHC) assessments refused"),
                      tags$li("Number of assessments completed and a decision made not to issue an EHC plan"),
                      tags$li("Number with an EHC plan as at January each year"),
                      tags$li("Number of EHC plans ceased because the special educational needs of the child or young person are being met without an EHC plan")
                    ),
                    tags$div(
                      "For more information on the decision to publish a new appeal rate, see ",
                      tags$a(href = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/686233/send-consultation-response.pdf", "the response to the joint MoJ/DfE consulation feedback (link to GOV.uk).")
                    ),
                    tags$div(tags$a(
                      href = "https://www.gov.uk/government/statistics/tribunal-statistics-quarterly-january-to-march-2022",
                      "Source: Tribunal Statistics Quarterly."
                    ))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Autism waiting times (NHS)"),
                  h3("Proportion of patients with an open \"suspected autism\" referral receiving a first appointment after more than 13 weeks"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "autism_ccg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "aut_cgt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_ccg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "CCG comparison",
                        p(),
                        radioGroupButtons(
                          "aut_cgb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_ccg_bench_tog", height = "600px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Autism waiting times are only published by the NHS at Sub-ICB (former Clinical Commissioning Group (CCG) level) and national level, not ICB or region."),
                    p("Some months and local areas' data were affected by a cyber incident in 2022."),
                    p("Some age breakdowns or Sub-ICB-level data have been suppressed for privacy reasons, where the number of patients it relates to is very small. Because of this, the number of areas/providers with data for 18-25 year olds is lower than other age groups."),
                    p("Autism diagnosis activity also happens in the Community Services Dataset (CSDS); this metric currently only includes data from the Mental Health Services Dataset (MHSDS). "),
                    p("These are experimental statistics. This means that care should be taken when making comparisons as the metric is still being refined and definitions may change between years. "),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics",
                      "Source: Autism Waiting Time Statistics"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Absence"),
                  h3("Overall absence rate for pupils with SEN"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "absence_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "abs_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("absence_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHCP or Statement", "SEN support"),
                          selected = "All SEN"
                        ),
                        radioGroupButtons(
                          "abs_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("\"Absence\" refers to children who are absent for authorised and unauthorised reasons. During 2020/21 and 2021/22, this includes children who are absent with a positive COVID case – but does not include children who are isolating but have not had a confirmed positive case, for example as a contact."),
                    p("Totals include state-funded primary, secondary and special schools. Data for special schools is available from academic year 2016/17 onwards"),
                    p("For further detail, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms#explore-data-and-files",
                      "Source: Absence in schools in England (Autumn and Spring terms). "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 4 destinations"),
                  h3("Proportion of children and young people going to a positive destination post-16"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks4_destinations_la_panel",
                      tabPanel(
                        "Change over time",
                        htmlOutput("nhants_excuse_ks4t"),
                        selectInput("ks4_destinations_la_time_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "EHC plan or Statement", "SEN support"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "destks4_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        htmlOutput("nhants_excuse_ks4d"),
                        selectInput("ks4_destinations_la_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "EHC plan or Statement", "SEN support"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "destks4_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_la_bench_tog", height = "110%"))
                      ),
                      tabPanel(
                        "SEN provision type comparison",
                        htmlOutput("nhants_excuse_ks4c"),
                        selectInput("ks4_destinations_la_type_filter",
                          label = "Select destination for comparison",
                          choices = list(
                            "Sustained employment destinations" = c(
                              "Employment",
                              "Apprenticeship"
                            ),
                            "Sustained education destinations" = c(
                              "Further education",
                              "School sixth form",
                              "Sixth form college",
                              "Education (other)"
                            ),
                            "Not sustained or unknown destinations" = c("Not sustained", "Unknown")
                          ),
                          selected = "Employment"
                        ),
                        radioGroupButtons(
                          "destks4_typ_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_la_type_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These official statistics show the percentage of pupils continuing to a sustained education, apprenticeship or employment destination in England in the year after completing key stage 4 study (after year 11) from state-funded mainstream schools."),
                    p("To be counted in a destination, young people have to have sustained participation for a 6 month period in the destination year. "),
                    p("This dataset is affected by the COVID-19 disruption. Many employers and apprenticeship providers took on fewer individuals during the pandemic and so it is anticipated that sustained employment and apprenticeship destinations will be lower than for previous years."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-destination-measures", "Source: Key stage 4 destination measures")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Children awaiting placement"),
                  h3("Children and young people with EHC plans awaiting an education placement"),
                  box(width = graph_width),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This is a proposed metric that is currently under development."),
                    p("It will be added to this dashboard when the metric is published."),
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Use of mediation"),
                  h3("Number of mediation sessions held, and percentage of mediation sessions that were followed by an appeal to SEND Tribunal"),
                  box(width = graph_width),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This is a proposed metric that will be added to the dashboard during its next phase of development."),
                  )
                )
              )
            ),
            tabPanel(
              "Identification of Need",
              fluidRow(
                column(
                  width = 12,
                  h2("Proportions of pupils with EHCPs and with SEN support"),
                  h3("% of pupils in schools who have an Education, health and care (EHC) plan or are on SEN support"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "percent_pupils_ehcp_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ehcppc_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("percent_pupils_ehcp_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("percent_pupils_ehcp_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("EHC plan", "SEN support"),
                          selected = "EHC plan"
                        ),
                        radioGroupButtons(
                          "ehcppc_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("percent_pupils_ehcp_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These graphs show the percentage of all pupils in England with an education, health and care (EHC) plan or on SEN support in schools. This includes all state-funded nursery, primary, secondary and special schools, non-maintained special schools, pupil referral units and independent schools."),
                    p("Pupils with special educational needs are currently classified as follows:"),
                    tags$ul(
                      tags$li("SEN Support: Extra or different help is given from that provided as part of the school’s usual curriculum. The class teacher and special educational needs coordinator (SENCO) may receive advice or support from outside specialists. The pupil does not have an education, health and care plan."),
                      tags$li("Education, health and care (EHC) plan: A pupil has an EHC plan when a formal assessment has been made. A document is in place that sets out the child’s need and the extra help they should receive.")
                    ),
                    p("Note that children and young people not attending school (e.g. those at FE college or private nurseries) who have an EHC plan are not included in this graph."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils with special educational needs (SEN) in mainstream schools"),
                  h3("% of pupils in mainstream educational settings who have SEN, by SEN provision type"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "mainstream_with_sen_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "msen_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mainstream_with_sen_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("mainstream_with_sen_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("EHC plan", "SEN support"),
                          selected = "EHC plan"
                        ),
                        radioGroupButtons(
                          "msen_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(
                          type = spinner_type,
                          ui_element = uiOutput("mainstream_with_sen_la_bench_tog", height = "110%")
                        )
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Pupils with special educational needs (SEN) attend either mainstream or specialist education settings.
                    These graphs show what percentage of pupils in mainstream primary, secondary and nursery schools have an education,
                    health and care (EHC) plan or receive SEN support."),
                    p("It does not include pupils in special schools, alternative provision, independent schools or out of education."),
                    tags$a(
                      href = "% of pupils in mainstream educational settings with SEN",
                      "Source: Special educational needs in England"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Proportion of pupils in specialist settings"),
                  h3("% of pupils in specialist settings, by SEN provision type"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "provider_types_la_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("provider_types_la_time_filter",
                          label = "Show as a percentage of...",
                          choices = list("all pupils", "pupils with EHC plans", "pupils on SEN support"),
                          selected = "all pupils"
                        ),
                        radioGroupButtons(
                          "types_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("provider_types_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("provider_types_la_bench_filter",
                          label = "Show as a percentage of...",
                          choices = list("all pupils", "pupils with EHC plans", "pupils on SEN support"),
                          selected = "all pupils"
                        ),
                        radioGroupButtons(
                          "types_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("provider_types_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Most pupils in England attend 'mainstream' settings (state-funded nurseries, primary schools or secondary schools).
                    These graphs show the % of pupils that are in other 'specialist' settings: independent schools, alternative provision, non-maintained schools and state-funded special schools. "),
                    p("By default, the graph shows percentages of all pupils, with or without an identified special educational need.
                    The drop-down menu allows you to filter the graph to show the percentage of pupils with EHC plans or SEN support that are in these school types (for example, a value of 5% with EHC plans selected from the drop-down would mean that 5% of pupils with EHC plans were in that school type)."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Age of children and young people with EHC plans"),
                  h3("Number of children and young people by age group who have an Education, health and care (EHC) plan "),
                  box(
                    width = graph_width,
                    id = "ehcp_ageprofile_la_panel",
                    p(),
                    radioGroupButtons(
                      "age_lat_toggle",
                      label = NULL,
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    withSpinner(type = spinner_type, ui_element = uiOutput("ehcp_ageprofile_la_time_tog", height = "700px"))
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These graphs show the number of children and young people with an education, health and care (EHC) plan in different age groups in England, as at January of each year."),
                    p("These numbers are collected from Local Authorities, and unlike most graphs in this dashboard, they include children and young people both in and out of school."),
                    p("For more information, including what education provision children with EHC plans are currently in, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Children in need with SEN"),
                  h3("% of children in need, including children looked after by local authorities, who have an EHC plan or receive SEN support"),
                  box(width = graph_width),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This is a Children's Social Care metric that will be added in due course."),
                  )
                )
              )
            ),
            tabPanel(
              "Financial Sustainability",
              fluidRow(
                column(
                  width = 12,
                  h2("Local authority education surplus/deficit"),
                  h3("Dedicated Schools Grant (DSG) cumulative balance as a % of the total budget"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "dsg_deficit_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "dsg_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("dsg_deficit_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        radioGroupButtons(
                          "dsg_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("dsg_deficit_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The Dedicated Schools Grant (DSG) is funding given to Local Authorities to spend on supporting education, within legally defined limitations. One area of this is High Needs spend, which supports children with Education, Health and Care plans, among other things."),
                    p("These percentages show what proportion of the Dedicated Schools Grant funding a Local Authority has received was carried over into the following year."),
                    p("Where a percentage is negative, an LA has spent more than its allocated DSG (the overspend is carried forward to the next year). This is one indicator of the financial sustainability of a local SEND system."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/la-and-school-expenditure",
                      "Source: LA and school expenditure"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Spend per head on specialist provision"),
                  h3("Per capita gross spend on special schools and AP, by sector (state-funded or non-maintained/independent"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "specialist_spend_la_panel",
                      tabPanel(
                        "Change over time",
                        htmlOutput("nh_excuse_spendt"),
                        p(),
                        radioGroupButtons(
                          "spend_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("specialist_spend_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        htmlOutput("nh_excuse_spendc"),
                        p(),
                        radioGroupButtons(
                          "spend_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("specialist_spend_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This metric shows the amount of High Needs top-up funding spent within the state and independent/non-maintained sectors on special schools and alternative provision."),
                    p("It is taken from the High Needs Benchmarking tool generally used by Local Authorities."),
                    p("On average, independent and non-maintained provision is more expensive per head than provision in the state sector (e.g. state-funded special schools), and more children are in independent or non-maintained settings than previously."),
                    p(" However, independent special schools often cater for children and young people with very complex needs which increases the average cost."),
                    tags$a(
                      href = "https://www.gov.uk/government/publications/high-needs-benchmarking-tool",
                      "Source: High needs benchmarking tool"
                    )
                  )
                )
              )
            )
          ),
        )
      )
    )
  )

  # add box to show user input
}



regional_dashboard_panel <- function() {
  tabPanel(
    id = "toptab_engreg",
    value = "engreg_dashboard",
    "England and Regions",

    # Define UI for application that draws a histogram

    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Compare SEND and AP indicators for England and regions")
        ),
        column(
          width = 12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              column(
                width = 12,
                pickerInput("level_choice",
                  label = h2("Select England or region-level view"),
                  choices = c("England", "Regions"),
                  selected = "England",
                  options = pickerOptions(maxOptions = 1, )
                ),
                conditionalPanel(
                  condition = "input.level_choice == 'Regions' && input.tabsetpanels_reg != 'Summary'",
                  selectInput(
                    inputId = "region_choice",
                    label = "Choose region",
                    choices = region_list
                  )
                ),
                conditionalPanel(
                  condition = "input.tabsetpanels_reg == 'Summary'",
                  strong("Currently only England-level and LA-level summaries are available. Once a Region-level summary is created it will be selectable here.")
                )
              )
            )
          )
        ),
        column(
          width = 12,
          tabsetPanel(
            id = "tabsetpanels_reg",
            tabPanel(
              "Summary",
              column(
                width = 12,
                h2("England and Regions Summary"),
                box(
                  width = 12,
                  p("This page provides a snapshot of the latest data on one page for all metrics at an England level. A direction arrow indicates an increase or decrease in the latest value of a measure when compared with the previous period of data. The previous period in question is noted below the information on the size of the change.")
                )
              ),
              h2("Outcomes"),
              fluidRow(
                class = "summary-box",
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%", # manually specifying box height to achieve uniform box size
                  background = "blue",
                  h3("KS1 phonics"),
                  h4("% achieving expected standard in Y1 (all pupils with SEN)", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: KS1 phonics",
                    uiOutput("box_ks1_phonics"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "blue",
                  h3("KS2 attainment"),
                  h4("% achieving expected standards (all pupils with SEN)"),
                  div( # need a div to add hover over title
                    title = "Source: KS2 attainemnt",
                    uiOutput("box_ks2_attainment"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "blue",
                  h3("KS4 attainment"),
                  h4("Average progress 8 score (all pupils with SEN)"),
                  div( # need a div to add hover over title
                    title = "Source: KS4 attainment",
                    uiOutput("box_ks4_attainment"),
                  ),
                  # br(),
                )
              ),
              fluidRow(
                class = "summary-box",
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "blue",
                  h3("Mental Health services access"),
                  h4("Number of children and young people accessing NHS-funded support", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: NHS England",
                    uiOutput("box_mentalhealth"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "blue",
                  h3("Ofsted Ratings"),
                  br(),
                  h4("Percentage of LAs with no Written Statement of Action", .noWS = "after"),
                  br(),
                  div( # need a div to add hover over title
                    title = "Source: Ofsted",
                    uiOutput("box_ofsted"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "blue",
                  h3("16-18 Destinations"),
                  br(),
                  h4("Percentage of school leavers with sustained destination", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: 16-18 destinations",
                    uiOutput("box_1618dest"),
                  )
                )
              ),
              h2("Experiences"),
              fluidRow(
                class = "summary-box",
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "green",
                  h3("EHCP timeliness"),
                  # br(),
                  h4("Percentage of EHCPs issued within 20-week limit:", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: SEN2",
                    uiOutput("box_timeliness"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "green",
                  h3("SEND Tribunal appeal rate"),
                  # br(),
                  h4("Appeals to SEND tribunal as % of appealable decisions", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: HMCTS",
                    uiOutput("box_tribunals"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "green",
                  h3("Overall absence rate"),
                  # br(),
                  h4("Overall absence rate (all pupils with SEN)", .noWS = "after"),
                  # br(),
                  div( # need a div to add hover over title
                    title = "Source: Absence in schools in England",
                    uiOutput("box_absence"),
                  )
                )
              ),
              fluidRow(
                class = "summary-box",
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "green",
                  h3("Autism waiting times"),
                  h4("Proportion of children under 10 recieving a first appointment after > 13 weeks", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: Absence in schools in England",
                    uiOutput("box_autism"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "green",
                  h3("Destinations post-KS4"),
                  h4("Proportion of school leavers with a sustained destination", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: Absence in schools in England",
                    uiOutput("box_KS4dest"),
                  )
                ),
              ),
              h2("Identification of Need"),
              fluidRow(
                class = "summary-box",
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "red",
                  h3("Pupils with EHC plans in state-funded schools"),
                  h4("Percentage of pupils in state-funded schools with an EHCP", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: Special educational needs in England",
                    uiOutput("box_statefunded"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "red",
                  h3("Pupils with SEN in mainstream schools"),
                  h4("Percentage of pupils in mainstream settings with SEN", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: Special educational needs in England",
                    uiOutput("box_mainstream"),
                  )
                ),
                box(
                  class = "summary-box",
                  width = 4,
                  height = "100%",
                  background = "red",
                  h3("Pupils in special schools"),
                  br(),
                  h4("Percentage of pupils in specialist settings", .noWS = "after"),
                  # br(),
                  div( # need a div to add hover over title
                    title = "Source: Special educational needs in England",
                    uiOutput("box_special"),
                  )
                )
              ),
              h2("Financial Sustainability"),
              fluidRow(
                class = "summary-box",
                box(
                  class = "summary-box",
                  width = 4,
                  background = "black",
                  height = "100%",
                  h3("Combined Local Authority Education Deficit"),
                  h4("Dedicated Schools Grant cumulative balance as % of total budget", .noWS = "after"),
                  div( # need a div to add hover over title
                    title = "Source: LA and school expenditure",
                    uiOutput("box_budget"),
                  )
                )
              )
            ),
            tabPanel(
              "Outcomes",
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 1 phonics check"),
                  h3("% of pupils with SEN  meeting the expected standard in the phonics screening check in year 1"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks1_phonics_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "phonics_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks1_phonics_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan", "SEN support"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "phonics_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These are the first phonics screening check and key stage 1 attainment statistics since 2019, after assessments were cancelled in 2020 and 2021 due to the pandemic. These statistics cover the attainment of year 1 and year 2 pupils who took these assessments in summer 2022. These pupils experienced disruption to their learning during the pandemic. Includes all pupils who participated in the phonics screening check, those disapplied, those absent for the entire period of which the check could be administered and those without results due to maladministration. Pupils with missing, or invalid results are not included in the calculations."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-1-and-phonics-screening-check-attainment/2021-22",
                      "Source: Key stage 1 and phonics screening check attainment."
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key Stage 2 attainment"),
                  h3("% of pupils with SEN reaching expected standards (in reading, writing and mathematics) at KS2"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks2_attainment_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ks2_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks2_attainment_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan", "SEN support"),
                          selected = "All SEN"
                        ),
                        radioGroupButtons(
                          "ks2_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics cover the attainment of year 6 pupils who took assessments in summer 2022. These pupils experienced disruption to their learning during the pandemic, particularly at the end of year 4 and in year 5. Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks. "),
                    p("Figures for 2022 are based on revised data. Figures for other years are based on final data. For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment",
                      "Source: Key stage 2 attainment."
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 4 attainment"),
                  h3("Average Progress 8 score, for pupils with and without SEN"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks4_attainment_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ks4_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks4_attainment_reg_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Any SEN", "No identified SEN"),
                          selected = "Any SEN"
                        ),
                        radioGroupButtons(
                          "ks4_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Progress 8 aims to capture the progress that pupils in a school make from the end of primary school to the end of KS4. It is a type of value-added measure, which means that pupils’ results in up to 8 qualifications are compared to other pupils nationally with similar prior attainment."),
                    p("This academic year saw the return of the summer exam series, after they had been cancelled in 2020 and 2021 due to the impact of the COVID-19 pandemic, where alternative processes were set up to award grades (centre assessment grades, known as CAGs, and teacher assessed grades, known as TAGs). As part of the transition back to the summer exam series, adaptations were made to the exams (including advance information) and the approach to grading for 2022 exams broadly reflected a midpoint between results in 2019 and 2021. "),
                    p("Progress 8 scores in 2022 will also have been impacted by the changes to methodology in the way 2022 performance measures were calculated, as results achieved between January 2020 and August 2021 were not included in the measures."),
                    p(strong("Currently Progress 8 scores broken down by SEN description are not available for 2018/19, so no comparison over time is possible. These scores will be added in the near future.")),
                    tags$div(
                      "More information on these changes, and how Progress 8 is calculated, can be seen in the",
                      tags$a(
                        href = "https://www.gov.uk/government/news/guide-to-gcse-results-for-england-summer-2022",
                        "Guide to GCSE results for England, summer 2022."
                      )
                    ),
                    tags$a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1134998/Secondary_accountability_measures_2022_guidance_for_maintained_secondary_schools__academies_and_free_schools.pdf", "Secondary accountability measures: 2022 guidance for maintained secondary schools, academies and free schools (publishing.service.gov.uk)"),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised", "Source: Key stage 4 Performance")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupil destinations after 16-18 study"),
                  h3("% SEN post 16-18 in employment, training or higher education"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "destinations_1618_reg_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("destinations_1618_reg_time_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "Identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "dest18_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("destinations_1618_reg_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "Identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "dest18_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_reg_bench_tog", height = "650px"))
                      ),
                      tabPanel(
                        "SEN/LLDD provision type comparison",
                        selectInput("destinations_1618_reg_type_filter",
                          label = "Select destination for comparison",
                          choices = list(
                            "Sustained employment destinations" = c(
                              "Employment",
                              "Apprenticeship"
                            ),
                            "Sustained education destinations" = c(
                              "Further education",
                              "Higher education",
                              "Education (other)"
                            ),
                            "Not sustained or unknown destinations" = c("Not sustained", "Unknown")
                          ),
                          selected = "Employment"
                        ),
                        radioGroupButtons(
                          "dest18_regtyp_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_reg_type_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Destination measures show the percentage of students going to or remaining in an education, apprenticeship or employment destination in the academic year after completing 16 to 18 studies (finishing year 13, usually aged 18). The most recent data reports on students who completed this stage in the academic year 2019 to 2020 and identifies their main activity in the following year (2020 to 2021)."),
                    p("These official statistics show students continuing to education, apprenticeship or employment destinations in the year after completing 16 to 18 study in schools and colleges in England."),
                    strong("Note: \"Identified SEN\" refers to pupils who completed 16-18 study in schools (e.g. school sixth forms) who are identified as having special educational needs,
                       whereas \"Identified LLDD\" refers to pupils who completed 16-18 study in colleges (e.g. sixth form colleges) with a self-declared Learning Difficulty, Disability or health problem."),
                    p("In 2020/21, 13,753 pupils identified with SEN completed 16-18 study, compared to 91,897 with self-declared LLDD."),
                    p("The latest data was affected by COVID-19. For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/16-18-destination-measures",
                      "Source: 16-18 destinations. "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Mental Health service access"),
                  h3("Children and young people accessing NHS-funded mental health support"),
                  box(
                    width = graph_width,
                    conditionalPanel(
                      condition = "input.level_choice == 'Regions'",
                      selectInput("nhs_region_choice",
                        label = "Select NHS region for comparison (defaults to closest matching NHS region*)",
                        choices = nhs_region_list,
                        selected = ""
                      )
                    ),
                    tabPanel(
                      "Regional comparison over time",
                      radioGroupButtons(
                        "mh_regt_toggle",
                        label = NULL,
                        choices = c("Chart", "Table"),
                        selected = "Chart"
                      ),
                      withSpinner(type = spinner_type, ui_element = uiOutput("mentalhealth_reg_time_tog", height = "110%"))
                    ),
                    conditionalPanel(
                      condition = "input.level_choice == 'Regions'",
                      tags$div(
                        "NHS regions have different boundaries to those used by DfE. ",
                        tags$a(
                          href = "https://geoportal.statistics.gov.uk/documents/nhs-england-regions-april-2020-map-in-england-1/explore",
                          "The latest map is on the Open Geography Portal. "
                        )
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This data shows the number of children accessing NHS-funded mental health services at least once in the previous (rolling) twelve-month period."),
                    p("Some providers' data are missing for some months in 2022 as a result of a cyber incident. Sub-ICB (former CCG areas) breakdowns are not an accurate reflection of activity for Performance November 2022, although some ICBs/Sub ICBs/Commissioning Regions are impacted more than others."),
                    p("For full details and limitations, see the full NHS publication linked below."),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics",
                      "Source: Mental Health Services Monthly Statistics."
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Local Area SEND Inspection ratings"),
                  h3("Ratings from the last joint Ofsted/CQC inspection"),
                  box(
                    width = graph_width,
                    id = "ofsted_reg_panel",
                    strong("LA inspection outcomes in region/nation"),
                    selectInput("ofsted_table_choice",
                      label = "Select either a summary (number of LAs by category) or a full table of inspection results:",
                      choices = c("Summary", "Full table"),
                      selected = "Summary"
                    ),
                    withSpinner(type = spinner_type, ui_element = DTOutput("reg_ofsted_rating", height = "110%"))
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    strong("This table shows the outcome of the most recent LA SEND area inspection. These inspections are carried out jointly by Ofsted and the Care Quality Commission (CQC)."),
                    tags$div(
                      "Source: ADSC collation of Ofsted/CQC reports from ",
                      tags$a(
                        href = "https://adcs.org.uk/inspection/article/send-inspection-outcomes-summary",
                        "adcs.org.uk/inspection/"
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Alternative provision (AP) outcome measures"),
                  h3("Five proposed AP Metrics"),
                  box(
                    width = graph_width,
                    tags$ol(
                      tags$li("Improved Attendance"),
                      tags$li("Academic Attainment, with a focus on English and Maths"),
                      tags$li("Re-Integration into Mainstream Education"),
                      tags$li("Progression to sustainable post-16 destinations"),
                      tags$li("Effective AP outreach support for mainstream schools")
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The proposed new alternative provision metrics are still in development. When available, following testing, they will be displayed in this dashboard. These metrics were described in the 2023 SEND and AP Improvement Plan as part of a bespoke alternative provision performance framework. These will be designed to set robust standards, but the exact metrics are yet to be finalised."),
                  )
                )
              )
            ), # This close-bracket is the end of the Outcomes panel


            tabPanel(
              "Experiences",
              fluidRow(
                column(
                  width = 12,
                  h2("EHCP timeliness"),
                  h3("% EHCPs finalised within 20 weeks excluding exceptions"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "timeliness_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "time_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("timeliness_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        radioGroupButtons(
                          "time_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("timeliness_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The whole process of EHC needs assessment and EHC plan development, from the point when an assessment is requested (or a child or young person is brought to the local authority’s attention) until any final EHC plan is issued, must take no more than 20 weeks. The relevant legislation provides for exceptions to the time limits in certain situations."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans in England (SEN2). "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("SEND Tribunal appeal rate"),
                  h3("% of total appealable decisions resulting in an appeal to the SEND Tribunal"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "tribunals_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "trib_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("tribunals_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        radioGroupButtons(
                          "trib_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("tribunals_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The appeal rate is based on the Total Appealable Decisions, which is calculated from data collected by the DfE from the annual SEN2 data return, which is mandatory for local authorities to complete. The Total Appealable Decisions figure is calculated as the sum total of the following:"),
                    tags$ul(
                      tags$li("Number of initial requests for Education, Health and Care (EHC) assessments refused"),
                      tags$li("Number of assessments completed and a decision made not to issue an EHC plan"),
                      tags$li("Number with an EHC plan as at January each year"),
                      tags$li("Number of EHC plans ceased because the special educational needs of the child or young person are being met without an EHC plan")
                    ),
                    tags$div(
                      "For more information on the decision to publish a new appeal rate, see ",
                      tags$a(href = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/686233/send-consultation-response.pdf", "the response to the joint MoJ/DfE consulation feedback (link to GOV.uk).")
                    ),
                    tags$div(tags$a(
                      href = "https://www.gov.uk/government/collections/tribunals-statistics#tribunal-statistics-quarterly",
                      "Source: Tribunal Statistics Quarterly."
                    ))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Absence"),
                  h3("Overall absence rate for pupils with SEN"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "absence_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "abs_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("absence_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHCP or Statement", "SEN support"),
                          selected = "All SEN"
                        ),
                        radioGroupButtons(
                          "abs_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("\"Absence\" refers to children who are absent for authorised and unauthorised reasons. During 2020/21 and 2021/22, this includes children who are absent with a positive COVID case – but does not include children who are isolating but have not had a confirmed positive case, for example as a contact."),
                    p("Totals include state-funded primary, secondary and special schools. Data for special schools is available from academic year 2016/17 onwards"),
                    p("For further detail, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms#explore-data-and-files",
                      "Source: Absence in schools in England (Autumn and Spring terms). "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Autism waiting times (NHS)"),
                  h3("Proportion of patients with an open \"suspected autism\" referral receiving a first appointment after more than 13 weeks"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "autism_reg_panel",
                      tabPanel(
                        "Change over time (England)",
                        p(),
                        radioGroupButtons(
                          "aut_nat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_nat_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Provider-level comparison",
                        selectInput("autism_nat_bench_filter",
                          label = "Select age group for comparison",
                          choices = list("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24"),
                          selected = "Age: Under 10"
                        ),
                        radioGroupButtons(
                          "aut_nab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_nat_bench_tog", height = "800px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    strong("Indicator not available at regional level. National and provider-level breakdowns are provided."),
                    p("Some months and local areas' data were affected by a cyber incident in 2022."),
                    p("Some age breakdowns or CCG-level data have been suppressed for privacy reasons, where the number of patients it relates to is very small. Because of this, the number of areas/providers with data for 18-25 year olds is lower than other age groups."),
                    p("Autism diagnosis activity also happens in the Community Services Dataset (CSDS); this metric currently only includes data from the Mental Health Services Dataset (MHSDS). "),
                    p("These are experimental statistics. This means that care should be taken when making comparisons as the metric is still being refined and definitions may change between years. "),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics",
                      "Source: Autism Waiting Time Statistics"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 4 destinations"),
                  h3("Proportion of children and young people going to a positive destination post-16"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks4_destinations_reg_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("ks4_destinations_reg_time_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "EHC plan or Statement", "SEN support"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "destks4_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks4_destinations_reg_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "EHC plan or Statement", "SEN support"),
                          selected = "Identified SEN"
                        ),
                        radioGroupButtons(
                          "destks4_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_reg_bench_tog", height = "650px"))
                      ),
                      tabPanel(
                        "SEN provision type comparison",
                        selectInput("ks4_destinations_reg_type_filter",
                          label = "Select destination for comparison",
                          choices = list(
                            "Sustained employment destinations" = c(
                              "Employment",
                              "Apprenticeship"
                            ),
                            "Sustained education destinations" = c(
                              "Further education",
                              "School sixth form",
                              "Sixth form college",
                              "Education (other)"
                            ),
                            "Not sustained or unknown destinations" = c("Not sustained", "Unknown")
                          ),
                          selected = "Employment"
                        ),
                        radioGroupButtons(
                          "destks4_regtyp_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_reg_type_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These official statistics show the percentage of pupils continuing to a sustained education, apprenticeship or employment destination in England in the year after completing key stage 4 study (after year 11) from state-funded mainstream schools."),
                    p("To be counted in a destination, young people have to have sustained participation for a 6 month period in the destination year. "),
                    p("This dataset is affected by the COVID-19 disruption. Many employers and apprenticeship providers took on fewer individuals during the pandemic and so it is anticipated that sustained employment and apprenticeship destinations will be lower than for previous years."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-destination-measures", "Source: Key stage 4 destination measures")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Children awaiting placement"),
                  h3("Children and young people with EHC plans awaiting an education placement"),
                  box(width = graph_width),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This is a proposed metric that is currently under development."),
                    p("It will be added to this dashboard when the metric is published."),
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Use of mediation"),
                  h3("Number of mediation sessions held, and percentage of mediation sessions that were followed by an appeal to SEND Tribunal"),
                  box(width = graph_width),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This is a proposed metric that will be added to the dashboard during its next phase of development."),
                  )
                )
              )
            ),
            tabPanel(
              "Identification of Need",
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils in schools with SEN"),
                  h3("% of pupils in schools who have an Education, health and care (EHC) plan or who are receive SEN support"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "percent_pupils_ehcp_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ehcppc_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("percent_pupils_ehcp_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("percent_pupils_ehcp_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("EHC plan", "SEN support"),
                          selected = "EHC plan"
                        ),
                        radioGroupButtons(
                          "ehcppc_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("percent_pupils_ehcp_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These graphs show the percentage of all pupils in England with an education, health and care (EHC) plan. This includes all state-funded nursery, primary, secondary and special schools, non-maintained special schools, pupil referral units and independent schools."),
                    p("Pupils with special educational needs are currently classified as follows:"),
                    tags$ul(
                      tags$li("SEN Support: Extra or different help is given from that provided as part of the school’s usual curriculum. The class teacher and special educational needs coordinator (SENCO) may receive advice or support from outside specialists. The pupil does not have an education, health and care plan."),
                      tags$li("Education, health and care (EHC) plan: A pupil has an EHC plan when a formal assessment has been made. A document is in place that sets out the child’s need and the extra help they should receive.")
                    ),
                    p("Note that children and young people not attending school (e.g. those at FE college or private nurseries) who have an EHC plan are not included in this graph."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils with special educational needs (SEN) in mainstream schools"),
                  h3("% of pupils in mainstream educational settings who have SEN, by SEN provision type"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "mainstream_with_sen_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "msen_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mainstream_with_sen_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("mainstream_with_sen_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("EHC plan", "SEN support"),
                          selected = "EHC plan"
                        ),
                        radioGroupButtons(
                          "msen_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mainstream_with_sen_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    # strong("Note that for this metric, selecting  \"Inner London\" or \"Outer London\" will display statistics for London overall."), can't select Inner or Outer London anymore
                    p("Pupils with special educational needs (SEN) attend either mainstream or specialist education settings.
                    These graphs show what percentage of pupils in mainstream primary, secondary and nursery schools have an education,
                    health and care (EHC) plan or receive SEN support."),
                    p("It does not include pupils in special schools, alternative provision, independent schools or out of education."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Proportion of pupils in specialist settings"),
                  h3("% of pupils in specialist settings, by SEN provision type"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "provider_types_reg_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("provider_types_reg_time_filter",
                          label = "Show as a percentage of...",
                          choices = list("all pupils", "pupils with EHC plans", "pupils on SEN support"),
                          selected = "all pupils"
                        ),
                        radioGroupButtons(
                          "types_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("provider_types_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("provider_types_reg_bench_filter",
                          label = "Show as a percentage of...",
                          choices = list("all pupils", "pupils with EHC plans", "pupils on SEN support"),
                          selected = "all pupils"
                        ),
                        radioGroupButtons(
                          "types_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("provider_types_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Most pupils in England attend 'mainstream' settings (state-funded nurseries, primary schools or secondary schools).
                    These graphs show the % of pupils that are in other 'specialist' settings: independent schools, alternative provision, non-maintained schools and state-funded special schools. "),
                    p("By default, the graph shows percentages of all pupils, with or without an identified special educational need.
                    The drop-down menu allows you to filter the graph to show the percentage of pupils with EHC plans or SEN support that are in these school types (for example, a value of 5% with EHC plan selected from the drop-down would mean that 5% of pupils with EHC plans were in that school type)."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England "
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Age of children and young people with EHC plans"),
                  h3("Number of children and young people by age group who have an Education, health and care (EHC) plan "),
                  box(
                    width = graph_width,
                    p(),
                    radioGroupButtons(
                      "age_regt_toggle",
                      label = NULL,
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    withSpinner(type = spinner_type, ui_element = uiOutput("ehcp_ageprofile_reg_time_tog", height = "700px"))
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    # strong("Note that for this metric, selecting  \"Inner London\" or \"Outer London\" will display statistics for London overall."), can't select Inner or Outer London anymore
                    p("These graphs show the number of children and young people with an education, health and care (EHC) plan in different age groups in England, as at January of each year."),
                    p("These numbers are collected from Local Authorities, and unlike most graphs in this dashboard, they include children and young people both in and out of school."),
                    p("For more information, including what education provision children with EHC plans are currently in, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Children in need with SEN"),
                  h3("% of children in need, including children looked after by local authorities, who have an EHC plan or receive SEN support"),
                  box(width = graph_width),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This is a proposed Children's Social Care metric that is still in development.
                    When it is published, it will be also be displayed in this dashboard."),
                  )
                )
              )
            ),
            tabPanel(
              "Financial Sustainability",
              fluidRow(
                column(
                  width = 12,
                  h2("Local authority education surplus/deficit"),
                  h3("Dedicated Schools Grant (DSG) cumulative balance as a % of the total budget"),
                  box(
                    width = graph_width,
                    uiOutput("london_choice"),
                    tabsetPanel(
                      id = "dsg_deficit_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "dsg_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("dsg_deficit_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        radioGroupButtons(
                          "dsg_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("dsg_deficit_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The Dedicated Schools Grant (DSG) is funding given to Local Authorities to spend on supporting education, within legally defined limitations. One area of this is High Needs spend, which supports children with Education, Health and Care plans, among other things."),
                    p("These percentages show what proportion of the Dedicated Schools Grant funding a Local Authority has received was carried over into the following year."),
                    p("Where a percentage is negative, an LA has spent more than its allocated DSG (the overspend is carried forward to the next year). This is one indicator of the financial sustainability of a local SEND system."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/la-and-school-expenditure",
                      "Source: LA and school expenditure"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Spend per head on specialist provision"),
                  h3("Per capita gross spend on non-maintained and independent sector for SEN, AP and special schools"),
                  box(
                    width = graph_width,
                    #          withSpinner(type = spinner_type, ui_element = uiOutput("specialist_spend_reg_time_tog", height = "110%")))
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p(strong("Metric not yet available at regional or England level. This graph is currently present in the LA Dashboard, and will be added here once the data becomes available.")),
                    p("This metric shows the amount of High Needs top-up funding spent within the state and independent/non-maintained sectors on special schools and alternative provision."),
                    p("It is taken from the High Needs Benchmarking tool generally used by Local Authorities."),
                    p("On average, independent and non-maintained provision is more expensive per head than provision in the state sector (e.g. state-funded special schools), and more children are in independent or non-maintained settings than previously."),
                    p("However, independent special schools often cater for children and young people with very complex needs which increases the average cost."),
                    tags$a(
                      href = "https://www.gov.uk/government/publications/high-needs-benchmarking-tool",
                      "Source: High needs benchmarking tool"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
