homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("SEND and AP Dashboard"),
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
                h2("Context")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "Contents Box",
                  p("This is the Special Educational Needs and Disabilities (SEND) and Alternative Provision (AP) Dashboard."),
                  p("The dashboard is part of a wider set of reforms set out in the SEND and alternative provision improvement plan. The ", a(href = "https://www.gov.uk/government/publications/send-and-alternative-provision-improvement-plan", "SEND and alternative provision improvement plan", target = "_blank"), " aims to improve outcomes for children and young people with SEND and in AP, improve experiences for families, and deliver financial sustainability."),
                  p("The dashboard presents SEND and AP data on ‘system health’ and performance across education, health and care at a national, regional and local area level. The aim of the dashboard is to improve public transparency and accessibility, help to enable better decision-making at a national and local level and drive self-improvement across local areas."),
                  p("The dashboard is an iterative product that will evolve over time based on user feedback. This initial version focuses on improving the accessibility and transparency of published data to all users. It has been developed with multiple users in mind including: parents and carers, local authorities (LAs), health, SEND and AP partnerships (collective local partners who should be involved in strategic planning and commissioning) and providers (such as early years, schools and post-16 settings). This version has been developed based on feedback from the ", a(href = "https://www.gov.uk/government/consultations/send-review-right-support-right-place-right-time", "Green Paper consultation", target = "_blank"), " and engagement with potential users including thorough user testing of a prototype in Summer 2023."),
                  p("The dashboard could be used to help to identify best practices, allow LAs to benchmark nationally, regionally and locally, to monitor progress of reforms and changes made locally as well as wider implementation of reforms in SEND and AP."),
                  p("The dashboard should", strong(" not "), "be used in isolation either to make a summative judgement or for accountability purposes. The aim of the dashboard is to be a starting point to gather information easily and quickly. Using this as the only source of data intelligence in its current format would not give a true understanding of a local area’s circumstances."),
                  p(strong("Feedback")),
                  p("This dashboard is a prototype which has been published to support testing and further iterations. Please send any feedback or suggestions for improvements to ", a(href = "mailto:sendap.dashboard@education.gov.uk", "sendap.dashboard@education.gov.uk.")),
                )
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
                h2("User guide")
              ),
              div(
                class = "panel-body",
                p(strong("How to navigate the dashboard")),
                tags$ul(
                  tags$li("The sidebar on the left can be used to navigate to different parts of the dashboard including the ‘England and Regions’ page and the ‘Local Areas’ page."),
                  tags$li("Each of these pages contain the same metrics broken down into themed tabs of outcomes, experiences, identification of need and financial sustainability. There is also a separate tab for AP."),
                  tags$li("Each page has a summary tab which gives a high-level overview of all the metrics on one page. The themed tabs that follow contain the corresponding metrics with the option to look at changes over time and make comparisons nationally, regionally and locally. Metrics are available in graph or table format."),
                  tags$li("The ‘about this indicator’ text should be considered alongside each metric to support understanding of the contextual and methodological information."),
                  tags$li("The local areas dashboard provides breakdowns by upper tier local authority (LA) and sub-ICB (Integrated Care Board) regions. In 2023 there were 153 upper tier local authorities in England made up of 63 unitary authorities, 36 metropolitan districts, 33 London boroughs (including City of London) and 21 counties. You can find your local ICB", a(href = "https://www.nhs.uk/nhs-services/find-your-local-integrated-care-board/", " here.", target = "_blank")),
                  tags$li("More information on the support and provision for children and young people with SEND and their families can be found in your LA’s SEND Local Offer. These are published online by your LA.")
                ),
                p(strong("Data")),
                p("The dashboard draws on data that is collected nationally across England and provides a snapshot of data as of a specific date. The dashboard is published as", a(href = "https://osr.statisticsauthority.gov.uk/publication/guidance-on-producing-official-statistics-in-development/", " ‘official statistics in development’ "), "which replaces the ‘experimental statistics’ term used previously for publications of this nature.The dashboard contains data from multiple sources that are published at different times throughout the year."),
                p("The data in this dashboard is planned to be updated twice a year whilst the dashboard is still being developed and reviewed. This means that some of the data will not reflect the latest publication at certain times of the year and as a result, users should take extra care when using these metrics. Different metrics are published and updated at different frequencies the year, with some metrics being monthly whilst others are updated once a year."),
                p(strong("Local Government Unitarisation")),
                p("In 2021 Northamptonshire county area was split into two unitary authorities: West Northamptonshire and North Northamptonshire. This impacts data comparison pre and post 2021. We have made it possible to view data for Northamptonshire LA to 2021 and both West and North Northamptonshire for 2021 onwards."),
                p("In 2023, Cumbria was split into two unitary authorities and single unitary councils will be created in North Yorkshire and Somerset."),
                p("As a result, whenever an LA is selected the dashboard’s ‘comparison’ tabs will show data for the last year the county existed.")
              ),
            )
          )
        )
      )
    )
  )
}


dashboard_panel <- function() {
  tabPanel(
    "Local Areas",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Compare local area SEND and AP indicators"),
        ),
        column(
          width = 12,
          id = "lapickbox",
          div(
            id = "collapse-la",
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            box(
              collapsible = TRUE,
              width = 12,
              background = "aqua",
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
                    label = h3("Select NHS England sub-ICB location to display graphs"),
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
                  conditionalPanel(
                    condition = "input.tabsetpanels_la == 'Summary'",
                    selectInput(
                      inputId = "summary_sen_type_la",
                      label = "Choose SEN type used for Summary (applies to KS1, KS2, destination, absence and children in need metrics)",
                      choices = list("All SEN", "EHC plan", "SEN support")
                    )
                  ),
                  materialSwitch(
                    inputId = "myregion_switch", label = "Compare to local areas in the same region only",
                    right = TRUE
                  )
                )
              ),
              div(
                style = "text-indent: 30px margin-bottom: 5px",
                htmlOutput("la_changed")
              )
            )
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
                  h2("Local authority metric summary"),
                  box(
                    width = 12,
                    p("This graphic provides an overview of where the selected local authority (LA) ranks among the largest 150 local authorities in England for all metrics excluding health-related metrics.
                      The City of London and the Isles of Scilly are excluded from comparisons as their small populations confuse rate or average statistics."),
                    p("This graphic is not intended to rank LAs in terms of performance. Being ranked higher or lower is not necessarily indicative of better or worse performance. This graph should be used
                    as a starting point for discussions when comparing metrics as LAs face different local challenges with different cohorts of children and young people with SEND and in AP. An understanding
                    of each LA’s unique context is encouraged when exploring this data."),
                    p("The circles represent a metric on the dashboard, and the number in the circle is the selected LA's rank in the most current data for this metric. The median (middle), highest and lowest LAs are shown by the dotted vertical lines.
                      The horizontal lines indicate the highest and lowest rank achieved by that LA over the range of the data currently available (there are no historic lines for Progress 8, which is a relative in-year measure not designed for comparison and for EYFSP where there is only a single year of data).
                      Names of metrics are shown to the left of the graph.
                      Hover over a circle with your mouse for a more detailed description of the metric.
                      Where an LA's rank has been very consistent across the years no line may be visible; with the size of the circles used a rank difference of at least 5 is required for visibility"),
                    box(
                      width = 12,
                      radioGroupButtons(
                        "la_sum_toggle",
                        label = "Display data as:",
                        choices = c("Chart", "Table"),
                        selected = "Chart"
                      ),
                      withSpinner(
                        type = spinner_type,
                        ui_element = uiOutput("la_summary_tog")
                      ),
                    ),
                    h2("Alternative provision metric summary"),
                    box(
                      width = 12,
                      p("For alternative provision, graphical presentation is complicated by each measure having a different number of LAs for which data exists (LAs without APs of a particular type within their borders will not have data for that AP type).
                        For this reason the data is only presented in table format. The `out of` column for each metric indicates how many LAs have data for this measure)."),
                      withSpinner(
                        type = spinner_type,
                        ui_element = DTOutput("ap_summary_table")
                      )
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
                  h2("Early years foundation stage profile"),
                  h3("% of children with SEN achieving a good level of development"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "eyfsp_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "eyfsp_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("eyfsp_la_time_tog"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("eyfsp_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan", "SEN support", "No identified SEN"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "eyfsp_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("eyfsp_la_bench_tog", height = "120%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics report on teacher assessments of children’s development at the end of the early years foundation stage (EYFS), specifically the end of the academic year in which a child turns 5. This is typically the summer term of reception year. The assessment framework, or EYFS profile, consists of 17 early learning goals (ELGs) across 7 areas of learning."),
                    p("This graph covers the percentage of children with SEN with a good level of development. Specifically, the percentage of children who are at the expected level in the 12 ELGs within the 5 areas of learning relating to: communication and language; personal, social and emotional development; physical development; literacy; and mathematics."),
                    p("As part of the 2021/22 EYFS reforms introduced in September 2021, the EYFS profile was significantly revised. It is therefore not possible to directly compare assessment outcomes from 2021/22 with earlier years."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/early-years-foundation-stage-profile-results/2021-22",
                      "Source: Early years foundation stage profile results", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Phonics screening check in year 1"),
                  h3("% of pupils with SEN meeting the expected standard in the phonics screening check in year 1"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks1_phonics_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "phonics_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_la_time_tog"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("ks1_phonics_la_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan or Statement", "SEN support", "No SEN"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "phonics_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_la_bench_tog", height = "120%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics cover the attainment of year 1 pupils. Any year 2 pupils who did not meet the standard or did not take the phonics check in year 1, (re)- take the check in year 2 (but are not included here). Data is not available for 2020 and 2021 as assessments were cancelled in these years due to the COVID-19 pandemic."),
                    p("The data includes all pupils who participated in the phonics screening check, those disapplied, those absent for the entire period of which the check could be administered and those without results due to maladministration. Pupils with missing or invalid results are not included in the calculations."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-1-and-phonics-screening-check-attainment/2021-22",
                      "Source: Key stage 1 and phonics screening check attainment.", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 2 attainment"),
                  h3("% of pupils with SEN reaching expected standard in reading, writing and maths (combined) at KS2"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks2_attainment_la_panel", # HS: Metric name - no dependency (for la level - for region level server dependency for summary links)
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ks2_lat_toggle", # HS: Toggle (chart vs table) filter - server dependency
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_la_time_tog", height = "110%"))) # HS: Server dependency - creates the Chart or Table (depending on toggle value)
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("ks2_attainment_la_filter", # HS: Characteristic filter - server dependency
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan", "SEN support"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "ks2_lab_toggle", # HS: dependency on server
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_la_bench_tog", height = "110%")) # HS: dependency
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics cover the attainment of year 6 pupils, when most are age 11 in key stage 2 national curriculum assessments in England. Data is not available for 2020 and 2021 as assessments were cancelled in these years due to the COVID-19 pandemic."),
                    p("Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment",
                      "Source: Key stage 2 attainment.", target = "_blank"
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Table"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        htmlOutput("nhants_excuse_bench"),
                        selectInput("ks4_attainment_la_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "No identified SEN"),
                          selected = "All SEN"
                        ),
                        radioGroupButtons(
                          "ks4_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Progress 8 aims to capture the progress that pupils in a school make from the end of primary school to the end of KS4. It is a type of value-added measure, which means that pupils’ results in up to 8 qualifications are compared to other pupils nationally with similar prior attainment. Progress 8 is a relative in-year measure and is not designed for comparisons over time."),
                    p("We have not included breakdowns for SEN and Education, Health and Care (EHC) plans because children with EHC planPs may do fewer than 8 qualifications or be entered for qualifications that are not approved to count towards KS4 performance measures. This is likely to impact their Progress 8 scores."),
                    p("In 2022/23 there was a return to pre-pandemic standards for GCSEs, AS and A levels, with protection built into the grading process to recognise the disruption that students have faced. KS4 performance measures for 2022/23 that are based on qualification results reflect the return to pre-pandemic grading, and cannot be directly compared to measures from 2021/22, when a different grading approach was used."),
                    tags$div(
                      "For more information on how we calculate performance measures, and the factors affecting measures for 2022/23, please see the", a(href = "https://www.gov.uk/government/collections/school-and-college-performance-measures#key-stage-4-performance-measures", "KS4"), "technical guide."
                    ),
                    tags$a(href = "https://www.gov.uk/government/publications/progress-8-school-performance-measure", "Secondary accountability measures (including Progress 8 and Attainment 8)", target = "_blank"),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised", "Source: Key stage 4 Performance", target = "_blank")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupil destinations after 16-18 study"),
                  h3("% post 16-18 in employment, training or higher education for pupils identified with SEN or self-identified with learning difficulty or disability (LLDD)"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "destinations_1618_la_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("destinations_1618_la_time_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN (mainstream)", "No identified SEN", "Identified LLDD (mainstream)", "No identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        selectInput("destinations_1618_la_time_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained destination (education, apprenticeship or employment)",
                            "All destination measures"
                          ),
                          selected = "Overall sustained destination (education, apprenticeship or employment)"
                        ),
                        radioGroupButtons(
                          "dest18_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        htmlOutput("dest_1618_missing_time"),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        htmlOutput("nhants_excuse_dest"),
                        selectInput("destinations_1618_la_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN (mainstream)", "No identified SEN", "Identified LLDD (mainstream)", "No identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        selectInput("destinations_1618_la_bench_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "dest18_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_la_bench_tog", height = "800px")),
                        htmlOutput("dest_1618_missing_bench")
                      ),
                      tabPanel(
                        "Provision type comparison",
                        htmlOutput("nhants_excuse_comp"),
                        selectInput("destinations_1618_la_type_filter",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "dest18_typ_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_la_type_tog", height = "600px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Destination measures provide information on the success of schools and colleges in helping young people continue in education, apprenticeships or employment.  The release focuses on outcomes for state-funded mainstream schools and colleges."),
                    p("These statistics show how many students with special educational needs and with identified Learning difficulties or disabilities (LLDD) continue to sustained education, apprenticeship or employment destinations in the year after completing 16 to 18 study in schools and colleges in England. The make up of the cohort has changed this year compared to previous years and will impact comparisons over time. The way we decide when a student is at the end of 16 to 18 study has changed this year and comparisons to previous cohorts should be treated with caution (see publication for details)."),
                    strong("Note: \"Identified SEN\" refers to pupils who completed 16-18 study in schools (e.g. school sixth forms) who are identified as having special educational needs,
                       whereas \"Identified LLDD\" refers to pupils who completed 16-18 study in colleges (e.g. sixth form colleges) with a self-declared Learning Difficulty, Disability or health problem.
                            Destinations measures for special schools are only available at national level."),
                    p(),
                    p("In 2020/21, 13,753 pupils identified with SEN completed 16-18 study, compared to 91,897 with self-declared LLDD."),
                    p("Recent data is likely to have been impacted by COVID-19. The make up of the cohort has changed and will impact comparisons over time.  For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/16-18-destination-measures",
                      "Source: 16-18 destinations. ", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Mental health service access"),
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
                          label = "Display data as:",
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
                          label = "Display data as:",
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
                    p("A person is defined as accessing services if they have had at least one direct contact (i.e. where the patient was involved) or at least one instance of indirect activity in the previous 12 months."),
                    p("Some providers' data are missing for some months in 2022 as a result of a cyber incident. Sub-ICB breakdowns are not an accurate reflection of activity for performance in November 2022, although some ICBs/Sub ICBs/Commissioning Regions are impacted more than others."),
                    p("For full details and limitations, see the full NHS publication linked below."),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics",
                      "Source: Mental Health Services Monthly Statistics.", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Discontinued EHC Plans", id = "disco_la_panel"),
                  h3("Number of EHC plans discontinued - special educational needs being met without an EHC plan (compulsory school age and non-compulsory school age)"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      tabPanel(
                        "Change over time",
                        radioGroupButtons(
                          "disco_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("discontinued_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("discontinued_la_filter",
                          label = "Select age for discontinued plans",
                          choices = list("Compulsory school age", "Above compulsory school age"),
                          selected = "Compulsory school age"
                        ),
                        radioGroupButtons(
                          "disco_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("discontinued_la_bench_tog", height = "110%"))
                      ),
                    ),
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This data shows the number of education, health and care plans which are recorded in the annual SEN2 survey of local authorities (LA) as being discontinued as the child or young person's needs were being met without the need for an EHC plan."),
                    p("The publication is based on data collected in the SEN2 data collection. From 2023, the data collection changed from aggregated figures at LA level, to a person level collection. This has been a major change in approach and care should be taken when using this data. We expect the quality of the data returns to improve over time as the collection becomes established."),
                    p("Data on ceased plans changed from 2023 to include a more comprehensive set of reasons for plans ending. Data was collected for all plans that ended. Prior to 2023, data on ceased plans only covered children of compulsory school age. Care should be taken when comparing across this period."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, Health and Care Plans", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Ofsted/CQC Area SEND inspection outcomes"),
                  h3("Outcomes from the last joint Ofsted/CQC inspection"),
                  box(
                    width = graph_width,
                    withSpinner(
                      type = spinner_type,
                      ui_element = valueBoxOutput("la_ofsted_rating", width = 12)
                    ),
                    p(ofsted_data_updated)
                  ),
                  conditionalPanel(
                    condition = "input.la_choice == 'North Northamptonshire' || input.la_choice == 'West Northamptonshire'",
                    box(
                      width = graph_width,
                      textOutput("la_split_ofsted"),
                      br(),
                      withSpinner(
                        type = spinner_type,
                        ui_element = valueBoxOutput("previous_ofsted", width = 12)
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Ofsted and CQC jointly inspect local area partnerships to see how well they work together to improve the experiences and outcomes of children and young people with special educational needs and/or disabilities (SEND). The inspections are carried out in line with the ", a(href = "https://www.gov.uk/government/publications/area-send-framework-and-handbook", "area SEND inspection framework and handbook.", target = "_blank")),
                    p("From January 2023, all local area partnerships will be inspected on an ongoing basis under the new Area SEND framework. Local area partnerships are inspected at least once every 5 years."),
                    p("Not every area has been inspected under the new framework yet, therefore the dashboard presents the last available inspection outcome – whether that be under the new or previous framework."),
                    p(
                      "The outcomes listed in the dashboard pertain to either the previous framework:",
                      tags$ul(
                        tags$li("Written Statement of Action (WSoA) and no revisit (red)"),
                        tags$li("WSoA and progress not made against all significant weaknesses (red)"),
                        tags$li("WSoA but progress made against all weaknesses (green)"),
                        tags$li("No WSoA (green)")
                      ),
                      "or the new framework:",
                      tags$ul(
                        tags$li("Typically positive experiences and outcomes (green)"),
                        tags$li("Inconsistent experiences and outcomes (amber)"),
                        tags$li("Widespread/Systemic failings (red)"),
                        tags$li("No inspection (white)")
                      )
                    ),
                    p("Between the previous inspection framework and current framework, two local authorities (LA), Cumbria County Council and Northamptonshire County Council, divided into two separate authorities. The dashboard shows these as ‘not yet inspected’ although they were inspected under the previous framework on their previous County Council footprint. This outcome is presented alongside the selected LA."),
                    tags$a(
                      href = "https://www.gov.uk/government/collections/area-send-statistics",
                      "Source: Area SEND statistics ", target = "_blank"
                    )
                  )
                )
              )
            ), # This close bracket is the end of the Outcomes panel.



            tabPanel(
              "Experiences",
              fluidRow(
                column(
                  width = 12,
                  h2("EHC plan timeliness"),
                  h3("% EHC plans finalised within 20 weeks excluding exceptions"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "timeliness_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "time_lat_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("timeliness_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This metric shows the number of Education, Health and Care (EHC) plans issued within the 20-week statutory timeline."),
                    p("The Special Educational Needs and Disability Regulations 2014 set out the time limits for conducting an EHC needs assessment as required under section 36(11) of the\ Children and Families Act 2014. The whole process includes all of the required steps from the point when an assessment is requested (or a child or young person is brought to the local authority’s attention) until any final EHC plan is issued and must take no more than 20 weeks. The relevant legislation provides for exceptions to the time limits in certain situations."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans in England (SEN2).", target = "_blank"
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
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("tribunals_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The First-tier Tribunal Special Educational Needs and Disability (SEND) jurisdiction hears appeals against the decision of local authorities (LA) in England relating to Education, Health and Care (EHC) plans. The Tribunal can also hear appeals and make non-binding decisions about health and social care aspects of EHC plans, so long as there is also an education element."),
                    p("Appeals to the tribunal can be made in relation to a number of different decisions a LA would make relating to that system which include the refusal to assess a child with special educational needs. Further information is at:", a(href = "https://www.gov.uk/courts-tribunals/first-tier-tribunal-special-educational-needs-and-disability", "First-tier Tribunal (Special Educational Needs and Disability) - GOV.UK (www.gov.uk).")),
                    p("The appeal rate is based on Total Appealable Decisions, which is calculated from data collected by the DfE from the annual SEN2 data return, which is mandatory for LAs to complete. The Total Appealable Decisions figure is calculated as the sum total of the following:"),
                    tags$ul(
                      tags$li("Number of initial requests for EHC plan assessments refused"),
                      tags$li("Number of assessments completed and a decision made not to issue an EHC plan"),
                      tags$li("Number with an EHC plan as at January each year"),
                      tags$li("Number of EHC plans ceased because the special educational needs of the child or young person are being met without an EHC plan")
                    ),
                    tags$div(tags$a(
                      href = "https://www.gov.uk/government/collections/tribunals-statistics#tribunal-statistics-quarterly",
                      "Source: Tribunal Statistics Quarterly.", target = "_blank"
                    ))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Autism waiting times"),
                  h3("Percentage of patients with an open suspected autism referral in the month that has been open for at least 13 weeks or received a first appointment after more than 13 weeks"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "autism_ccg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "aut_cgt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_ccg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Sub-ICB area comparison",
                        p(),
                        radioGroupButtons(
                          "aut_cgb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_ccg_bench_tog", height = "600px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Autism assessments for children and adolescents happen in two types of NHS service, child and adolescent mental health (CAMH) services and community paediatric services which undertake neurodevelopmental assessment.  Only children referred to CAMHs services are included in these statistics, referrals and diagnoses of autism in child development services, which comprise the majority of autism referrals for young children in the UK, are out of scope these statistics."),
                    p("Autism waiting times are only published by the NHS at Sub-ICB (former Clinical Commissioning Group (CCG) level) and national level, not ICB or region."),
                    p("Some months and local areas' data were affected by a cyber incident in 2022."),
                    p("Some age breakdowns or Sub-ICB-level data have been suppressed for privacy reasons, where the number of patients it relates to is very small. Because of this, the number of areas/providers with data for 18-25 year olds is lower than other age groups."),
                    p("These are experimental statistics. This means that care should be taken when making comparisons as this metric is still being refined and definitions may change between years. "),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics",
                      "Source: Autism Waiting Time Statistics", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Children and young people community health service waiting times"),
                  h3("Percentage waiting for children and young people services for more than 18 weeks"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ch_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        p("This information is only available at regional, national ICB and provider levels. The areas that best correspond to local authorities are sub-ICB areas and the data is not published at this level."),
                        p("Provder-level comparison is available (for those providers which provided information)")
                      ),
                      tabPanel(
                        "Provider-level comparison",
                        p(),
                        selectInput("ch_prov_type_filter",
                          label = "Select waiting list for:",
                          choices = list("Audiology", "Occupational Therapy", "Community Paediatric Service", "Speech And Language"),
                          selected = "Overall"
                        ),
                        radioGroupButtons(
                          "ch_prob_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ch_prov_bench_tog", height = "600px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The Community Health Services SitRep collects monthly data on waiting lists and times for children and young people."),
                    p("This dashboard includes percentage on the waiting list who have been waiting for children and young people services for more than 18 weeks including for speech and language, occupational therapy, paediatric service and audiology."),
                    p("This graph does not suggest that 18 weeks is an acceptable length of wait - other waiting times below 18 weeks are available through the source linked below."),
                    p("Providers submit aggregate information for service lines, irrespective of the number of ICBs or regions they provide services under. Data is submitted at provider level and can be viewed here at regional and ICB level. Therefore variation in the number of providers submitting each month could cause variation in the reported waiting list so this should be taken into account if comparing across months."),
                    p("This publication contains management data which is collected on a rapid turnaround basis, allowing only minimal validation to be undertaken. These publications are not classified as Official Statistics."),
                    tags$a(
                      href = "https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/",
                      "Source: Community Health Services Waiting Lists", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Absence rate for pupils with SEN"),
                  h3("% of sessions missed due to absence broken down by authorised and unauthorised absence"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "absence_la_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("absence_la_auth_filter",
                          label = "Select absence type for comparison",
                          choices = list("Authorised", "Unauthorised", "Overall"),
                          selected = "Overall"
                        ),
                        p(),
                        radioGroupButtons(
                          "abs_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("absence_la_bench_auth_filter",
                          label = "Select absence type for comparison",
                          choices = list("Authorised", "Unauthorised", "Overall"),
                          selected = "Overall"
                        ),
                        selectInput("absence_la_sen_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHCP or Statement", "SEN support", "No SEN"),
                          selected = "All SEN"
                        ),
                        radioGroupButtons(
                          "abs_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Absence refers to children who are absent for authorised and unauthorised reasons. One session is equal to half a day. During 2020/21 and 2021/22, this included children who are absent with a positive COVID case – but did not include children who are isolating but have not had a confirmed positive case, for example as a contact."),
                    p("Every child has an equal right to a full time, suitable education. Attendance ambitions should be the same for all pupils. The Department recognises that some pupils with SEN may experience high absence rates: partly for unavoidable reasons (e.g., to attend a regular hospital appointment), but also partly where the barriers to regular attendance are ones that families and schools (and wider partners as needed) can work together to remove. Support to facilitate regular attendance should be in place for all pupils. The Department also recognises that the profile of reasons for absence in special schools can be different to the profile of reasons for pupils with SEN in mainstream schools. See ", a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1099677/Working_together_to_improve_school_attendance.pdf", "Working together to improve school attendance", target = "_blank"), "for more information"),
                    p("Totals include state-funded primary, secondary and special schools. Data for special schools is available from academic year 2016/17 onwards"),
                    p("For further detail, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms#explore-data-and-files",
                      "Source: Absence in schools in England (Autumn and Spring terms). ", target = "_blank"
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
                          choices = list("Identified SEN", "No identified SEN", "EHC plan or Statement", "SEN support"),
                          selected = "Identified SEN"
                        ),
                        selectInput("ks4_destinations_la_time_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained destination (education, apprenticeship or employment)",
                            "All destination measures"
                          ),
                          selected = "Overall sustained destination (education, apprenticeship or employment)"
                        ),
                        radioGroupButtons(
                          "destks4_lat_toggle",
                          label = "Display data as:",
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
                          choices = list("Identified SEN", "No identified SEN", "EHC plan or Statement", "SEN support"),
                          selected = "Identified SEN"
                        ),
                        selectInput("ks4_destinations_la_bench_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "destks4_lab_toggle",
                          label = "Display data as:",
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
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "destks4_typ_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_la_type_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These official statistics show the percentage of pupils continuing to an education, apprenticeship or employment destination in England in the year after completing key stage 4 study (after year 11) from state-funded mainstream and special schools."),
                    p("To be counted in a destination, young people have to have sustained participation for a 6 month period in the destination year. "),
                    p("This dataset is affected by the COVID-19 disruption. Many employers and apprenticeship providers took on fewer individuals during the pandemic and so it is anticipated that sustained employment and apprenticeship destinations will be lower than for previous years."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-destination-measures", "Source: Key stage 4 destination measures", target = "_blank")
                  )
                )
              ),
            ),
            tabPanel(
              "Identification of need",
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils in schools with SEN"),
                  h3("% of pupils in schools with SEN, by SEN provision type"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "percent_pupils_ehcp_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ehcppc_lat_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("percent_pupils_ehcp_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This graph shows the percentage of all pupils in schools in England with SEN, by type of SEN Provision. This includes pupils with SEN support or with an education, health and care (EHC) plan."),
                    p("The data includes all state-funded nursery, primary, secondary and special schools, non-maintained special schools, pupil referral units and independent schools (note that independent schools do not have a consistent definition of SEN)."),
                    p("Note that children and young people not attending school (e.g. those at FE college or private nurseries) who have an EHC plan are not included in this graph."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils with SEN in mainstream schools"),
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
                          label = "Display data as:",
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
                          label = "Display data as:",
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
                    p("This graph shows pupils who have SEN, by SEN provision type, that attend a mainstream school (state-funded nursery, primary and secondary)."),
                    tags$a(
                      href = "% of pupils in mainstream educational settings with SEN",
                      "Source: Special educational needs in England", target = "_blank"
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
                          label = "Display data as:",
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
                          label = "Display data as:",
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
                    p("By default, the graph shows the percentages of all pupils in a specialist setting.
                    The drop-down menu allows you to filter the graph by SEN provision type; e.g. a value of 5% with EHC plan selected from the drop-down would mean that 5% of all pupils with EHC plans were in that school type."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England ", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils in social care groups with SEN"),
                  h3("% of pupils with special educational needs (SEN) by social care group"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "cin_la_panel",
                      selectInput("cin_la_filter",
                        label = "Show Children who are identified with...",
                        choices = list(
                          "All SEN",
                          "SEN support",
                          "EHC plan"
                        ),
                        selected = "All SEN"
                      ),
                      tabPanel(
                        "Change over time",
                        radioGroupButtons(
                          "cin_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("cin_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("cin_sc_la_filter",
                          label = "Show Children who are....",
                          choices = list(
                            "CINO at 31 March",
                            "CLA 12 months at 31 March",
                            "CPPO at 31 March"
                          ),
                          selected = "CINO at 31 March"
                        ),
                        radioGroupButtons(
                          "cin_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("cin_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p(
                      "This graph shows the percentage of pupils with SEN, by SEN provision type and social care group. Social care groups included in this graph are:",
                      tags$ul(
                        tags$li("CLA 12 months - children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
                        tags$li("CPPO - children on a child protection plan, excluding children looked after."),
                        tags$li("CINO - children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed.")
                      )
                    ),
                    p("All references to these social care groups relate to the time point as at 31 March."),
                    p("Only data for social care groups that are available at local authority, regional and national level have been included. For further information on other groups please use the source below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england",
                      "Source: Outcomes for children in need, including children looked after by local authorities in England ", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Age of children and young people with EHC plans"),
                  h3("Number of children and young people by age group who have an EHC plan"),
                  box(
                    width = graph_width,
                    id = "ehcp_ageprofile_la_panel",
                    p(),
                    radioGroupButtons(
                      "age_lat_toggle",
                      label = "Display data as:",
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    withSpinner(type = spinner_type, ui_element = uiOutput("ehcp_ageprofile_la_time_tog", height = "700px"))
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These graphs show the number of children and young people with an education, health and care (EHC) plan or a statement by age group in England, as at January of each year."),
                    p("These numbers are collected from local authorities and includes children and young people both in and out of school from ages 0 to 25."),
                    p("For more information see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans", target = "_blank"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Financial sustainability",
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
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("dsg_deficit_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The Dedicated Schools Grant (DSG) is funding given to local authorities (LA) to spend on supporting education, within legally defined limitations. One area of this is High Needs spend, which supports children and young people with education, health and care plans, among other things."),
                    p("These percentages show what proportion of the DSG funding a LA has received and the DSG cumulative position into the following year aggregated to England/Regional or LA level depending on what tab and part of the dashboard is selected."),
                    p("Where a percentage is negative, an LA has spent more than its allocated DSG (the overspend is carried forward to the next year). This is one indicator of the financial sustainability of a local SEND system."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/la-and-school-expenditure",
                      "Source: LA and school expenditure", target = "_blank"
                    ),
                    tags$a(
                      href = "https://skillsfunding.service.gov.uk/view-latest-funding/national-funding-allocations/DSG/2023-to-2024",
                      "Source: Dedicated schools grant", target = "_blank"
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
                    tabsetPanel(
                      id = "specialist_spend_la_panel",
                      tabPanel(
                        "Change over time",
                        htmlOutput("nh_excuse_spendt"),
                        p(),
                        radioGroupButtons(
                          "spend_lat_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
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
                    p("It is taken from the High Needs Benchmarking tool generally used by local authorities."),
                    p("On average, independent and non-maintained provision is more expensive per head than provision in the state sector (e.g. state-funded special schools), and more children are in independent or non-maintained settings than previously."),
                    p(" However, independent special schools often cater for children and young people with very complex needs which increases the average cost."),
                    tags$a(
                      href = "https://www.gov.uk/government/publications/high-needs-benchmarking-tool",
                      "Source: High needs benchmarking tool", target = "_blank"
                    )
                  )
                )
              )
            ),

            # =======================
            # Alternative provision tab
            # =======================
            tabPanel(
              "Alternative provision",
              # Start the about text
              column(
                12,
                div(
                  div(
                    class = "panel panel-info",
                    div(
                      class = "panel-heading",
                      style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                      h2("Alternative provision: definition and data sources")
                    ),
                    div(
                      class = "panel-body",
                      tags$div(
                        title = "Contents Box",
                        h3("Definition of alternative provision"),
                        p("Alternative provision is defined in the ", a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/942014/alternative_provision_statutory_guidance_accessible.pdf", "Alternative Provision Statutory Guidance", target = "_blank"), " as education arranged by local authorities for pupils who, because of exclusion, illness or other reasons, would not otherwise receive suitable education; education arranged by schools for pupils on a suspension; and pupils being directed by schools to off-site provision to improve their behaviour.  These placements are typically for children unable to attend a mainstream or special school."),
                        h3("AP data collections"),
                        p("The Department for Education is making improvements to the way AP data is collected, published, and measured. In June 2023, we first published data on the use of alternative provision arranged by schools. The Department for Education is currently producing new bespoke alternative provision metrics to measure increased attendance, attainment, re-integration into mainstream education or progression to sustainable post-16 destinations. We expect these additional metrics to be included in the dashboard next year. This section includes statistics from the Department for Education on:"),
                        p(strong("1) State-funded alternative provision schools"), " - includes pupils in pupil referral units, AP academies and AP free schools and the data is collected through the school census. This includes placements arranged by local authorities and by schools."),
                        p(strong("2) School arranged alternative provision"), " - data on full and part time alternative provision placements arranged by schools collected in the school census. The department started collecting this data in January 2023, so data is not shown for previous years."),
                        p(strong("3) Local authority funded alternative provision"), " - placements in non-state-funded provision collected via the alternative provision census. This includes placements in non-state-funded provision when:"),
                        tags$ul(
                          tags$li("i) LAs arrange education for pupils who, because of exclusion, illness or other reasons, would not otherwise receive suitable education; using their Section 19 powers."),
                          tags$li("ii) LAs make educational provision for children and young people with Education, Health and Care plans under their duties in the Children and Families Act. To note, that many of these placements won’t meet the department's definition of alternative provision, as set out above, but we have included placements in the dashboard for transparency purposes.")
                        ),
                        p("Non-state-funded provision includes non-maintained special schools, independent schools, independent special schools, FE colleges and unregistered alternative provision settings. This may include some pupils who are included in the school census or school level annual school census, for example where the local authority is funding a placement in a non-maintained special school or independent special school."),
                        p("There are expected to be some overlaps between these data sets. This includes pupils attending pupil referral units, who also have other alternative provision either arranged by the school, or local authority funded alternative provision. It is therefore not advised to sum across the totals in each section."),
                        h3("User guidance"),
                        p("AP is a relatively small population compared to the overall school population. Therefore, users should be aware that when viewing certain local authority AP data, the percentages may be based on very small overall numbers and should be interpreted as such. In addition, some local authorities do not have any data for certain views. For example, if there is no state-funded AP in that local authority then there is no data to show for this provision type."),
                        p("The figures on numbers of pupils and placements shown below represent a snapshot at the point the data was collected in the alternative provision census (January) and school census (January). Therefore, the numbers of pupils and placements over a whole year will be a higher number than the snapshot figures shown."),
                        tags$ul(
                          tags$li("State-funded AP figures includes those enrolled at a state-funded AP school on census day (January)."),
                          tags$li("School arranged AP includes open placements on the school census day (January)."),
                          tags$li("LA funded AP includes open placements as at the alternative provision census day (January).")
                        ),
                      ),
                      br()
                    )
                  )
                ),
              ), # End of the about text

              fluidRow(
                column(
                  width = 12,
                  h2("Total pupils at an AP"),
                  h3("Total pupils by selected type of AP"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_count_la_panel", # This is a required argument and should be a unique identifier for the tabsetPanel(). It is used to reference and manipulate the tabset panel in your Shiny app
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_counts_la_time_filter",
                          label = "Select AP type",
                          choices = list("LA funded AP placements", "School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_counts_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_counts_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        selectInput("ap_counts_la_bench_filter",
                          label = "Select AP type",
                          choices = list("LA funded AP placements", "School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_counts_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_counts_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This publication provides statistics on the number of pupils at state-funded alternative provision schools (collected via the school census), school arranged alternative provision (collected via the school census), and LA arranged alternative provision (collected via the AP census).
School arranged alternative provision figures refer to pupils in school arranged alternative provision at January 2023.
State-funded alternative provision figures include pupils placed by schools and local authorities and whose sole or main registration is in a state-funded AP school at the Spring census.The number of dual subsidiary registrations at a state-funded AP will be added separately in 2024, when a time series dataset will become available.
Local authority funded placements in non state-funded provision figures refer to pupils with an open placement as at census date who have been recorded in the alternative provision census as being in a local authority funded alternative provision setting."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                      "Source: Schools, pupils and their characteristics: January"
                    )
                  )
                )
              ), # End of AP counts metric

              fluidRow(
                column(
                  width = 12,
                  h2("Characteristics of pupils at an AP"),
                  h3("% of pupils by selected characteristic at an AP"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_characteristics_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_characteristics_la_time_filter",
                          label = "Select pupil characteristic",
                          choices = list(
                            "All SEN",
                            "SEN status",
                            "Free school meal status",
                            "Age breakdown",
                            "Gender"
                          ),
                          selected = "All SEN"
                        ),
                        selectInput("ap_characteristics_la_time_filter_type",
                          label = "Select AP type",
                          choices = list("School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_characteristics_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_characteristics_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        selectInput("ap_characteristics_la_bench_filter",
                          label = "Select pupil characteristic",
                          choices = list(
                            "All SEN",
                            "SEN status",
                            "Free school meal status",
                            "Age breakdown",
                            "Gender"
                          ),
                          selected = "All SEN"
                        ),
                        selectInput("ap_characteristics_la_bench_filter_type",
                          label = "Select AP type",
                          choices = list("School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_characteristics_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_characteristics_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This publication provides statistics on the percentage of pupils at state-funded alternative provision schools, where pupils with dual subsidiary registration in state-funded AP schools are not included, and school arranged alternative provision (collected via the school census), by selected characteristic."),
                    p(
                      "Sources used:",
                      tags$a(
                        href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                        "Special educational needs in England (for state-funded AP data)"
                      ), " and ",
                      tags$a(
                        href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                        "Schools, pupils and their characteristics: January (for school arranged AP data)"
                      )
                    )
                  )
                )
              ), # end of the characteristics metric

              fluidRow(
                column(
                  width = 12,
                  h2("State-funded AP pupil absences"),
                  h3("Absence measures"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_absences_la_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ap_absences_lat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_absences_la_time_tog"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        selectInput("ap_absences_la_filter",
                          label = "Select absence measure for comparison",
                          choices = list(
                            "Overall absence %",
                            "Authorised absence %",
                            "Unauthorised absence %",
                            "% of persistent absentees - 10% or more sessions missed",
                            "% of persistent absentees - 50% or more sessions missed"
                          ),
                          selected = "Overall absence %"
                        ),
                        p(),
                        radioGroupButtons(
                          "ap_absences_lab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_absences_la_bench_tog", height = "120%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("'Absence' refers to children who are absent for authorised and unauthorised reasons. One session is equal to half a day. During 2020/21 and 2021/22, this includes children who are absent with a positive COVID case – but does not include children who are isolating but have not had a confirmed positive case, for example as a contact."),
                    p("Absence statistics refer to the enrolment level rather than pupil level. This means where a pupil has moved school throughout the year, they will be counted more than once as they have recorded attendance and absence at more than one school. Schools only record absence for the period a pupil is on roll at their school. This means that pupil enrolments who are persistently absent or severely absent in a short period of enrolment at a school will have very high rates. For further detail, see the source publication linked below."),
                    tags$a(
                      href = "https://www.gov.uk/government/collections/statistics-pupil-absence",
                      "Source: Pupil absence in schools in England: autumn and spring", target = "_blank"
                    )
                  )
                )
              ), # end of AP absences metric

              fluidRow(
                column(
                  width = 12,
                  h2("Ofsted most recent overall school effectiveness grade"),
                  h3("Most recent Ofsted overall effectiveness rating for state-funded schools, over time"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_ofsted_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_ofsted_la_time_filter", # One
                          label = "Select school type (these categories are mutually exclusive)",
                          choices = list(
                            "State-funded AP school",
                            "State-funded special school",
                            "Non-maintained special school",
                            "State-funded mainstream school"
                          ),
                          selected = "State-funded AP school"
                        ),
                        selectInput("ap_ofsted_la_time_filter_two", # two
                          label = "Select measure (be aware that '% of schools' may be based on very small numbers)",
                          choices = list(
                            "Overall effectiveness (number of schools)",
                            "Overall effectiveness (% of schools)"
                          ),
                          selected = "Overall effectiveness (number of schools)"
                        ),
                        radioGroupButtons(
                          "ap_ofsted_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_ofsted_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        selectInput("ap_ofsted_la_bench_filter", # One
                          label = "Select school type (these categories are mutually exclusive)",
                          choices = list(
                            "State-funded AP school",
                            "State-funded special school",
                            "Non-maintained special school",
                            "State-funded mainstream school"
                          ),
                          selected = "State-funded AP school"
                        ),
                        selectInput("ap_ofsted_la_bench_filter_two", # two
                          label = "Select measure (be aware that '% of schools' may be based on very small numbers)",
                          choices = list(
                            "Overall effectiveness (number of schools)",
                            "Overall effectiveness (% of schools)"
                          ),
                          selected = "Overall effectiveness (number of schools)"
                        ),
                        radioGroupButtons(
                          "ap_ofsted_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_ofsted_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This metric uses Ofsted state-funded school inspections and outcomes as at 31 August publication, which is published in the final quarter of the calendar year. For each year of data, the data uses both provisional data (used for the period January to August period) and revised data (for the September to December period of the previous calendar year). NULL values means that a school has not yet been inspected. Although the focus of this metric is for state-funded AP, users can also view overall effectiveness ratings for other state-funded school types for comparative purposes."),
                    tags$a(
                      href = "https://www.gov.uk/education/inspection-of-maintained-schools-and-academies",
                      "Source: Ofsted State-funded schools inspections and outcomes as at 31 August."
                    )
                  )
                )
              ), # end of the Ofsted metric

              fluidRow(
                column(
                  width = 12,
                  h2("Unregistered alternative provision (UAP) numbers"),
                  h3("UAP numbers at school arranged AP (total pupils) and LA-funded AP (total placements)"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_uap_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_uap_la_time_filter", # One
                          label = "Select provision type",
                          choices = list(
                            "LA funded unregistered AP placements",
                            "School arranged unregistered AP pupils"
                          ),
                          selected = "School arranged unregistered AP pupils"
                        ),
                        radioGroupButtons(
                          "ap_uap_lat_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_uap_la_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Local area comparison",
                        p(),
                        selectInput("ap_uap_la_bench_filter", # One
                          label = "Select provision type",
                          choices = list(
                            "LA funded unregistered AP placements",
                            "School arranged unregistered AP pupils"
                          ),
                          selected = "School arranged unregistered AP pupils"
                        ),
                        radioGroupButtons(
                          "ap_uap_lab_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_uap_la_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("For this indicator, unregistered provision is defined as placements at settings without a Unique Reference Number (URN). The Grand total unregistered AP (all setting types) bar represents the overall sum of unregistered AP across all setting types. For the 'Change over time view', each individual unregistered AP setting type's total is also displayed alongside the corresponding grand total. The unregistered AP setting types collected are 'Registered provider with UKPRN'; 'Non-maintained further education college'; 'One on one tuition'; 'Other unregistered provider'; 'Work based placement'; and 'Unknown'. This indicator uses statistics on the number of placements in non-school based unregistered alternative provision arranged by schools (collected via the school census) and by LAs collected via the AP census). Note that the number of placements figure can differ from the number of pupils figure as pupils can have multiple placements, so you cannot simply add school arranged unregistered AP pupils and LA funded unregistered AP placements to accurately identify the total number of pupils in UAP across both provision types."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                      "Source: Schools, pupils and their characteristics: January"
                    )
                  )
                )
              ), # end of UAP metric
            ), # This close-bracket is the end of the AP panel
          )
        )
      )
    )
  )
}



regional_dashboard_panel <- function() {
  tabPanel(
    id = "toptab_engreg",
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
          id = "engregbox",
          div(
            id = "collapse-engreg",
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            box(
              background = "aqua",
              collapsible = TRUE,
              width = 12,
              gov_row(
                column(
                  width = 12,
                  pickerInput("level_choice",
                    label = h2("Select England or region-level view"),
                    choices = list("Pick England or Regions" = "", "England", "Regions"),
                    selected = NULL,
                    options = pickerOptions(maxOptions = 1, )
                  ),
                  conditionalPanel(
                    condition = "input.level_choice == 'Regions'",
                    selectInput(
                      inputId = "region_choice",
                      label = "Choose region",
                      choices = region_list
                    )
                  ),
                  conditionalPanel(
                    condition = "input.tabsetpanels_reg == 'Summary'",
                    selectInput(
                      inputId = "summary_sen_type",
                      label = "Choose SEN type used for summary (applies to KS1, KS2, destination, absence and children in need metrics)",
                      choices = list("All SEN", "EHC plan", "SEN support")
                    ),
                  )
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
                  p("This page provides a snapshot of the latest data on one page for all metrics at a national or regional level depending on which option is selected."),
                  p("A direction arrow indicates an increase or decrease in the latest value of a measure when compared with the previous period of data. The previous period in question is noted below the information on the size of the change."),
                  p("Changes are expressed as percentage changes rather than percentage point changes; eg if a value is 2% this year after being 1% last year that is a 100% increase, not a 1% increase."),
                  p("Caution should be taken when looking at comparisons for Progress 8 where changes to the methodology and approach to grading has impacted comparisons. For destination measures, the make up of the cohort has changed and will impact comparisons over time (see sources for further information)."),
                )
              ),
              h2("Outcomes"),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_eyfsp_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%", # manually specifying box height to achieve uniform box size
                    background = "blue",
                    h3("Early Years Foundation Stage Profile"),
                    h4("% achieving good level of development", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: EYFSP",
                      uiOutput("box_eyfsp"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ks1_phonics_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%", # manually specifying box height to achieve uniform box size
                    background = "blue",
                    h3("Phonics screening check"),
                    h4("% achieving expected standard in Y1", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: KS1 phonics",
                      uiOutput("box_ks1_phonics"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ks2_attainment_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "blue",
                    h3("KS2 attainment"),
                    h4("% achieving expected standard"),
                    div( # need a div to add hover over title
                      title = "Source: KS2 attainemnt",
                      uiOutput("box_ks2_attainment"),
                    )
                  )
                )
              ),
              fluidRow(
                actionLink(
                  inputId = "link_ks4_attainment_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "blue",
                    h3("KS4 attainment"),
                    h4("Average progress 8 score (all pupils with SEN)"),
                    div( # need a div to add hover over title
                      title = "Source: KS4 attainment",
                      uiOutput("box_ks4_attainment"),
                    ),
                    br()
                  )
                ),
                class = "summary-box",
                actionLink(
                  inputId = "link_mh_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "blue",
                    h3("Mental Health services access"),
                    h4("Number of children and young people accessing NHS-funded support", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: NHS England",
                      uiOutput("box_mentalhealth"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ofsted_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "blue",
                    h3("OFSTED/CQC Area SEND Inspection Outcomes"),
                    # br(),
                    h4("Percentage of LAs with no Written Statement of Action or typically positive experiences", .noWS = "after"),
                    # br(),
                    div( # need a div to add hover over title
                      title = "Source: Ofsted",
                      uiOutput("box_ofsted"),
                    ),
                    br()
                  )
                )
              ),
              fluidRow(
                actionLink(
                  inputId = "link_1618_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "blue",
                    h3("16-18 Destinations"),
                    # br(),
                    h4("Percentage of school leavers (identified SEN) with sustained destination", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: 16-18 destinations",
                      uiOutput("box_1618dest"),
                    )
                  )
                ),
                class = "summary-box",
                actionLink(
                  inputId = "link_disco_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "blue",
                    h3("Discontinued EHC plans"),
                    h4("Number of EHC plans discontinued as needs met without plan:", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Education, Health and Care plans",
                      uiOutput("box_discontinued"),
                    )
                  )
                )
              ),
              h2("Experiences"),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_timeliness_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "green",
                    h3("EHC plan timeliness"),
                    br(),
                    h4("Percentage of EHC plans issued within 20-week limit:", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: SEN2",
                      uiOutput("box_timeliness"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_tribunals_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "green",
                    h3("SEND Tribunal appeal rate"),
                    # br(),
                    h4("Appeals to SEND tribunal as % of appealable decisions", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: HMCTS",
                      uiOutput("box_tribunals"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_absence_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "green",
                    h3("Overall absence rate"),
                    br(),
                    h4("Overall absence rate", .noWS = "after"),
                    # br(),
                    div( # need a div to add hover over title
                      title = "Source: Absence in schools in England",
                      uiOutput("box_absence"),
                    )
                  )
                )
              ),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_autism_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "green",
                    h3("Autism waiting times"),
                    h4("Proportion of children under 10 recieving a first appointment after more than 13 weeks (national data only, regional not published)", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Absence in schools in England",
                      uiOutput("box_autism"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ks4_destinations_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "green",
                    h3("Destinations post-KS4"),
                    h4("Proportion of school leavers (identified SEN) with a sustained destination", .noWS = "after"),
                    br(),
                    div( # need a div to add hover over title
                      title = "Source: Absence in schools in England",
                      uiOutput("box_KS4dest"),
                    )
                  )
                ),
              ),
              h2("Identification of need"),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_statefunded_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "red",
                    h3("Pupils with EHC plans in state-funded schools"),
                    h4("Percentage of pupils in state-funded schools with an EHC plans", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Special educational needs in England",
                      uiOutput("box_statefunded"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_mainstream_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "red",
                    h3("Pupils with SEN in mainstream schools"),
                    h4("Percentage of pupils in mainstream settings with SEN", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Special educational needs in England",
                      uiOutput("box_mainstream"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_special_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "red",
                    h3("Pupils in special schools"),
                    # br(),
                    h4("Percentage of pupils in specialist settings", .noWS = "after"),
                    # br(),
                    div( # need a div to add hover over title
                      title = "Source: Special educational needs in England",
                      uiOutput("box_special"),
                    )
                  )
                )
              ),
              fluidRow(
                actionLink(
                  inputId = "link_cin_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    height = "100%",
                    background = "red",
                    h3("Children in need with SEN"),
                    # br(),
                    h4("Percentage of children in need (excluding children on a child protection plan and children looked after) with SEN", .noWS = "after"),
                    # br(),
                    div( # need a div to add hover over title
                      title = "Source: Outcomes for children in need, including children looked after by local authorities in England",
                      uiOutput("box_cin"),
                    )
                  )
                )
              ),
              h2("Financial sustainability"),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_deficit_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "black",
                    height = "100%",
                    h3("Combined Local Authority Education Deficit"),
                    h4("Dedicated Schools Grant cumulative balance as % of total budget", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: LA and school expenditure",
                      uiOutput("box_budget"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_spend_reg_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "black",
                    height = "100%",
                    h3("Average spend per head on specialist provision"),
                    h4("Total per capita gross spend for SEN, AP and special schools", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: High needs benchmarking tool",
                      uiOutput("box_percap"),
                    )
                  )
                )
              ),
              h2("Alternative provision"),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_ap_sf_count_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Number of pupils at a state-funded AP"),
                    br(),
                    h4("All pupils attending state-funded alternative provision", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Schools, pupils and their characteristics",
                      uiOutput("box_apcount_sf"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ap_sa_count_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Number of pupils at school-arranged AP"),
                    h4("All pupils attending school-arranged alternative provision", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Schools, pupils and their characteristics",
                      uiOutput("box_apcount_sa"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ap_la_count_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Number of pupils at LA funded AP"),
                    h4("All pupils attending local authority funded alternative provision", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Schools, pupils and their characteristics",
                      uiOutput("box_apcount_la"),
                    )
                  )
                )
              ),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_uap_sa_count_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Number of pupils at school-arranged unregistered AP"),
                    h4("All pupils attending school-arranged unregistered alternative provision", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Schools, pupils and their characteristics",
                      uiOutput("box_uapcount_sa"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_uap_la_count_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Number of placements at LA funded unregistered AP"),
                    h4("All placements in local authority funded unregistered alternative provision", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Schools, pupils and their characteristics",
                      uiOutput("box_uapcount_la"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ap_sen_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Children with SEN in state-funded AP"),
                    br(),
                    h4("Percentage of children with selected SEN type in state-funded AP", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Special educational needs in England",
                      uiOutput("box_apchars"),
                    )
                  )
                )
              ),
              fluidRow(
                class = "summary-box",
                actionLink(
                  inputId = "link_ap_absence_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("Absence rate in state-funded AP"),
                    br(),
                    h4("Overall absence rate in state-funded AP", .noWS = "after"),
                    br(),
                    div( # need a div to add hover over title
                      title = "Source: Absence in schools in England",
                      uiOutput("box_apabsence"),
                    )
                  )
                ),
                actionLink(
                  inputId = "link_ap_ofsted_panel", width = "33%", class = "link-box",
                  box(
                    class = "summary-box",
                    width = 12,
                    background = "purple",
                    height = "100%",
                    h3("% State-funded AP schools good or outstanding"),
                    h4("Percentage of AP schools rated good or outstanding by Ofsted", .noWS = "after"),
                    div( # need a div to add hover over title
                      title = "Source: Ofsted reports",
                      uiOutput("box_apofsted"),
                    )
                  )
                ),
              )
            ),
            tabPanel(
              "Outcomes",
              fluidRow(
                column(
                  width = 12,
                  h2("Early years foundation stage profile"),
                  h3("% of children with SEN achieving a good level of development in the EYFSP"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "eyfsp_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "eyfsp_regt_toggle",
                          label = "Display data as:",
                          # choices = c("Chart", "Table"),
                          choiceNames = c(HTML('<label for="eyfsp_regt_toggle">Chart</label>'), HTML('<label for="eyfsp_regt_toggle">Table</label>')),
                          choiceValues = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("eyfsp_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("eyfsp_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHCP or Statement", "SEN support", "No identified SEN"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "eyfsp_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("eyfsp_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics report on teacher assessments of children’s development at the end of the early years foundation stage (EYFS), specifically the end of the academic year in which a child turns 5. This is typically the summer term of reception year. The assessment framework, or EYFS profile, consists of 17 early learning goals (ELGs) across 7 areas of learning."),
                    p("This graph covers the percentage of children with SEN with a good level of development. Specifically, the percentage of children who are at the expected level in the 12 ELGs within the 5 areas of learning relating to: communication and language; personal, social and emotional development; physical development; literacy; and mathematics."),
                    p("As part of the 2021/22 EYFS reforms introduced in September 2021, the EYFS profile was significantly revised. It is therefore not possible to directly compare assessment outcomes from 2021/22 with earlier years."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/early-years-foundation-stage-profile-results/2021-22",
                      "Source: Early years foundation stage profile results", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Phonics screening check in year 1"),
                  h3("% of pupils with SEN meeting the expected standard in the phonics screening check in year 1"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks1_phonics_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "phonics_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks1_phonics_reg_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHC plan or Statement", "SEN support", "No identified SEN"),
                          selected = "All SEN"
                        ),
                        p(),
                        radioGroupButtons(
                          "phonics_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks1_phonics_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics cover the attainment of year 1 pupils. Any year 2 pupils who did not meet the standard or did not take the phonics check in year 1, (re)- take the check in year 2 (but are not included here). Data is not available for 2020 and 2021 as assessments were cancelled in these years due to the COVID-19 pandemic."),
                    p("The data includes all pupils who participated in the phonics screening check, those disapplied, those absent for the entire period of which the check could be administered and those without results due to maladministration. Pupils with missing, or invalid results are not included in the calculations."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-1-and-phonics-screening-check-attainment/2021-22",
                      "Source: Key stage 1 and phonics screening check attainment.", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key Stage 2 attainment"),
                  h3("% of pupils with SEN reaching expected standard in reading, writing and mathematics (combined) at KS2"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks2_attainment_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ks2_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        div(withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_reg_time_tog", height = "110%")))
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks2_attainment_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These statistics cover the attainment of year 6 pupils, when most are age 11 in key stage 2 national curriculum assessments in England. Data is not available for 2020 and 2021 as assessments were cancelled in these years due to the COVID-19 pandemic."),
                    p("Writing teacher assessment and reading, writing and maths (combined) measures from 2018 onwards are not directly comparable to previous years due to changes in the writing teacher assessment frameworks."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-2-attainment",
                      "Source: Key stage 2 attainment.", target = "_blank"
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Table"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks4_attainment_reg_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "No identified SEN"),
                          selected = "All SEN"
                        ),
                        radioGroupButtons(
                          "ks4_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_attainment_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Progress 8 aims to capture the progress that pupils in a school make from the end of primary school to the end of KS4. It is a type of value-added measure, which means that pupils’ results in up to 8 qualifications are compared to other pupils nationally with similar prior attainment. Progress 8 is a relative in-year measure and is not designed for comparisons over time."),
                    p("We have not included breakdowns for SEN and Education, Health and Care (EHC) plans because children with EHC planPs may do fewer than 8 qualifications or be entered for qualifications that are not approved to count towards KS4 performance measures. This is likely to impact their Progress 8 scores."),
                    p("In 2022/23 there was a return to pre-pandemic standards for GCSEs, AS and A levels, with protection built into the grading process to recognise the disruption that students have faced. KS4 performance measures for 2022/23 that are based on qualification results reflect the return to pre-pandemic grading, and cannot be directly compared to measures from 2021/22, when a different grading approach was used."),
                    tags$div(
                      "For more information on how we calculate performance measures, and the factors affecting measures for 2022/23, please see the", a(href = "https://www.gov.uk/government/collections/school-and-college-performance-measures#key-stage-4-performance-measures", "KS4"), "technical guide."
                    ),
                    tags$a(href = "https://www.gov.uk/government/publications/progress-8-school-performance-measure", "Secondary accountability measures (including Progress 8 and Attainment 8)", target = "_blank"),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised", "Source: Key stage 4 Performance", target = "_blank")
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
                          choices = list("Identified SEN (mainstream)", "No identified SEN", "Identified LLDD (mainstream)", "No identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        selectInput("destinations_1618_reg_time_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained destination (education, apprenticeship or employment)",
                            "All destination measures"
                          ),
                          selected = "Overall sustained destination (education, apprenticeship or employment)"
                        ),
                        radioGroupButtons(
                          "dest18_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("destinations_1618_reg_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN (mainstream)", "No identified SEN", "Identified LLDD (mainstream)", "No identified LLDD"),
                          selected = "Identified SEN"
                        ),
                        selectInput("destinations_1618_reg_bench_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "dest18_regb_toggle",
                          label = "Display data as:",
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
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "dest18_regtyp_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("destinations_1618_reg_type_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Destination measures provide information on the success of schools and colleges in helping young people continue in education, apprenticeships or employment.  The release focuses on outcomes for state-funded mainstream schools and colleges."),
                    p("These statistics show how many students with special educational needs and with identified Learning difficulties or disabilities (LLDD) continue to sustained education, apprenticeship or employment destinations in the year after completing 16 to 18 study in schools and colleges in England. The make up of the cohort has changed this year compared to previous years and will impact comparisons over time. The way we decide when a student is at the end of 16 to 18 study has changed this year and comparisons to previous cohorts should be treated with caution."),
                    strong("Note: \"Identified SEN\" refers to pupils who completed 16-18 study in schools (e.g. school sixth forms) who are identified as having special educational needs,
                       whereas \"Identified LLDD\" refers to pupils who completed 16-18 study in colleges (e.g. sixth form colleges) with a self-declared Learning Difficulty, Disability or health problem.
                            Destinations measures for special schools are only available at national level."),
                    p(),
                    p("In 2020/21, 13,753 pupils identified with SEN completed 16-18 study, compared to 91,897 with self-declared LLDD."),
                    p("Recent data is likely to have been impacted by COVID-19. The make up of the cohort has changed and will impact comparisons over time.  For full details and limitations, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/16-18-destination-measures",
                      "Source: 16-18 destinations. ", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Mental health service access"),
                  h3("Number of children and young people accessing NHS funded secondary mental health, learning disabilities and autism services in England in the last 12 months"),
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
                    tabsetPanel(
                      id = "mh_reg_panel",
                      tabPanel(
                        "Change over time",
                        radioGroupButtons(
                          "mh_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mentalhealth_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        radioGroupButtons(
                          "mh_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("mentalhealth_reg_bench_tog", height = "110%"))
                      ),
                      conditionalPanel(
                        condition = "input.level_choice == 'Regions'",
                        tags$div(
                          "NHS regions have different boundaries to those used by DfE. ",
                          tags$a(
                            href = "https://geoportal.statistics.gov.uk/documents/nhs-england-regions-april-2020-map-in-england-1/explore",
                            "The latest map is on the Open Geography Portal. ", target = "_blank"
                          )
                        )
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This data shows the number of children accessing NHS-funded mental health services at least once in the previous (rolling) twelve-month period."),
                    p("A person is defined as accessing services if they have had at least one direct contact (i.e. where the patient was involved) or at least one instance of indirect activity in the previous 12 months."),
                    p("Some providers' data are missing for some months in 2022 as a result of a cyber incident. Sub-ICB breakdowns are not an accurate reflection of activity for performance in November 2022, although some ICBs/Sub ICBs/Commissioning Regions are impacted more than others."),
                    p("For full details and limitations, see the full NHS publication linked below."),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/mental-health-services-monthly-statistics",
                      "Source: Mental Health Services Monthly Statistics.", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Discontinued EHC plans", id = "disco_reg_panel"),
                  h3("Number of EHC plans discontinued - special educational needs being met without an EHC plan (compulsory school age and non-compulsory school age)"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      tabPanel(
                        "Change over time",
                        radioGroupButtons(
                          "disco_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("discontinued_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("discontinued_reg_filter",
                          label = "Select age for discontinued plans",
                          choices = list("Compulsory school age", "Above compulsory school age"),
                          selected = "Compulsory school age"
                        ),
                        radioGroupButtons(
                          "disco_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("discontinued_reg_bench_tog", height = "110%"))
                      ),
                    ),
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This data shows the number of education, health and care plans which are recorded in the annual SEN2 survey of local authorities (LA) as being discontinued as the child or young person's needs were being met without the need for an EHC plan."),
                    p("The publication is based on data collected in the SEN2 data collection. From 2023, the data collection changed from aggregated figures at LA level, to a person level collection. This has been a major change in approach and care should be taken when using this data. We expect the quality of the data returns to improve over time as the collection becomes established."),
                    p("Data on ceased plans changed from 2023 to include a more comprehensive set of reasons for plans ending. Data was collected for all plans that ended. Prior to 2023, data on ceased plans only covered children of compulsory school age. Care should be taken when comparing across this period."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, Health and Care Plans", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Ofsted/CQC Area SEND inspection outcomes", id = "ofsted_reg_panel"),
                  h3("Area SEND inspection outcome including outcomes from the previous and new framework"),
                  box(
                    width = graph_width,
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
                    p("Ofsted and CQC jointly inspect local area partnerships to see how well they work together to improve the experiences and outcomes of children and young people with special educational needs and/or disabilities (SEND). The inspections are carried out in line with the ", a(href = "https://www.gov.uk/government/publications/area-send-framework-and-handbook", "area SEND inspection framework and handbook.", target = "_blank")),
                    p("From January 2023, all local area partnerships will be inspected on an ongoing basis under the new Area SEND framework. Local area partnerships are inspected at least once every 5 years."),
                    p("Not every area has been inspected under the new framework yet, therefore the dashboard presents the last available inspection outcome – whether that be under the new or previous framework."),
                    p(
                      "The outcomes listed in the dashboard pertain to either the previous framework:",
                      tags$ul(
                        tags$li("Written Statement of Action (WSoA) and no revisit (red)"),
                        tags$li("WSoA and progress not made against all significant weaknesses (red)"),
                        tags$li("WSoA but progress made against all weaknesses (green)"),
                        tags$li("No WSoA (green)")
                      ),
                      "or the new framework:",
                      tags$ul(
                        tags$li("Typically positive experiences and outcomes (green)"),
                        tags$li("Inconsistent experiences and outcomes (amber)"),
                        tags$li("Widespread/Systemic failings (red)"),
                        tags$li("No inspection (white)")
                      ),
                    ),
                    p("Between the previous inspection framework and current framework, two local authorities (LA), Cumbria County Council and Northamptonshire County Council, divided into two separate authorities. The dashboard shows these as ‘not yet inspected’ although they were inspected under the previous framework on their previous County Council footprint. This outcome is presented alongside the selected LA."),
                    tags$a(
                      href = "https://www.gov.uk/government/collections/area-send-statistics",
                      "Source: Area SEND statistics ", target = "_blank"
                    )
                  )
                )
              )
            ), # This close-bracket is the end of the Outcomes panel


            tabPanel(
              "Experiences",
              fluidRow(
                column(
                  width = 12,
                  h2("Timeliness of EHC plans"),
                  h3("% EHC plans issued within 20 weeks excluding exceptions"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "timeliness_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "time_regt_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("timeliness_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This metric shows the number of Education, Health and Care (EHC) plans issued within the 20-week statutory timeline."),
                    p("The Special Educational Needs and Disability Regulations 2014 set out the time limits for conducting an EHC needs assessment as required under section 36(11) of the\ Children and Families Act 2014. The whole process includes all of the required steps from the point when an assessment is requested (or a child or young person is brought to the local authority’s attention) until any final EHC plan is issued and must take no more than 20 weeks. The relevant legislation provides for exceptions to the time limits in certain situations."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans in England (SEN2).", target = "_blank"
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
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("tribunals_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The First-tier Tribunal Special Educational Needs and Disability (SEND) jurisdiction hears appeals against the decision of local authorities (LA) in England relating to Education, Health and Care (EHC) plans. The Tribunal can also hear appeals and make non-binding decisions about health and social care aspects of EHC plans, so long as there is also an education element."),
                    p("Appeals to the tribunal can be made in relation to a number of different decisions a LA would make relating to that system which include the refusal to assess a child with special educational needs. Further information is at:", a(href = "https://www.gov.uk/courts-tribunals/first-tier-tribunal-special-educational-needs-and-disability", "First-tier Tribunal (Special Educational Needs and Disability) - GOV.UK (www.gov.uk).")),
                    p("The appeal rate is based on Total Appealable Decisions, which is calculated from data collected by the DfE from the annual SEN2 data return, which is mandatory for LAs to complete. The Total Appealable Decisions figure is calculated as the sum total of the following:"),
                    tags$ul(
                      tags$li("Number of initial requests for EHC plan assessments refused"),
                      tags$li("Number of assessments completed and a decision made not to issue an EHC plan"),
                      tags$li("Number with an EHC plan as at January each year"),
                      tags$li("Number of EHC plans ceased because the special educational needs of the child or young person are being met without an EHC plan")
                    ),
                    tags$div(tags$a(
                      href = "https://www.gov.uk/government/collections/tribunals-statistics#tribunal-statistics-quarterly",
                      "Source: Tribunal Statistics Quarterly.", target = "_blank"
                    ))
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Absence rate for pupils with SEN"),
                  h3("% of sessions missed due to absence broken down by authorised and unauthorised absence"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "absence_reg_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("absence_reg_auth_filter",
                          label = "Select absence type for comparison",
                          choices = list("Authorised", "Unauthorised", "Overall"),
                          selected = "Overall"
                        ),
                        p(),
                        radioGroupButtons(
                          "abs_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("absence_reg_sen_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("All SEN", "EHCP or Statement", "SEN support", "No SEN"),
                          selected = "All SEN"
                        ),
                        selectInput("absence_reg_auth_bench_filter",
                          label = "Select absence type for comparison",
                          choices = list("Authorised", "Unauthorised", "Overall"),
                          selected = "Overall"
                        ),
                        radioGroupButtons(
                          "abs_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("absence_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Absence refers to children who are absent for authorised and unauthorised reasons. During 2020/21 and 2021/22, this included children who are absent with a positive COVID case – but did not include children who are isolating but have not had a confirmed positive case, for example as a contact."),
                    p("Every child has an equal right to a full time, suitable education. Attendance ambitions should be the same for all pupils. The Department recognises that some pupils with SEN may experience high absence rates: partly for unavoidable reasons (e.g., to attend a regular hospital appointment), but also partly where the barriers to regular attendance are ones that families and schools (and wider partners as needed) can work together to remove. Support to facilitate regular attendance should be in place for all pupils. The Department also recognises that the profile of reasons for absence in special schools can be different to the profile of reasons for pupils with SEN in mainstream schools. See ", a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1099677/Working_together_to_improve_school_attendance.pdf", "Working together to improve school attendance", target = "_blank"), "for more information"),
                    p("Totals include state-funded primary, secondary and special schools. Data for special schools is available from academic year 2016/17 onwards"),
                    p("For further detail, see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england-autumn-and-spring-terms#explore-data-and-files",
                      "Source: Absence in schools in England (Autumn and Spring terms). ", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Autism waiting times"),
                  h3("Percentage of patients with an open suspected autism referral in the month that has been open for at least 13 weeks or received a first appointment after more than 13 weeks"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "autism_reg_panel",
                      tabPanel(
                        "Change over time (England)",
                        p(),
                        radioGroupButtons(
                          "aut_nat_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("autism_nat_bench_tog", height = "800px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("Indicator not available at regional level. National and provider-level breakdowns are provided."),
                    p("Autism assessments for children and adolescents happen in two types of NHS service, child and adolescent mental health (CAMH) services and community paediatric services which undertake neurodevelopmental assessment.  Only children referred to CAMHs services are included in these statistics, referrals and diagnoses of autism in child development services, which comprise the majority of autism referrals for young children in the UK, are out of scope these statistics."),
                    p("Some months and local areas' data were affected by a cyber incident in 2022."),
                    p("Some age breakdowns or Sub-ICB level data have been suppressed for privacy reasons, where the number of patients it relates to is very small. Because of this, the number of areas/providers with data for 18-25 year olds is lower than other age groups."),
                    p("These are experimental statistics. This means that care should be taken when making comparisons as this metric is still being refined and definitions may change between years."),
                    tags$a(
                      href = "https://digital.nhs.uk/data-and-information/publications/statistical/autism-statistics",
                      "Source: Autism Waiting Time Statistics", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Children and young people community health services waiting times"),
                  h3("Percentage waiting for children and young people services for more than 18 weeks"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ch_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ch_nat_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ch_nat_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ch_nat_service_filter",
                          label = "Select waiting times for",
                          choices = list("Audiology", "Occupational Therapy", "Community Paediatric Service", "Speech And Language"),
                          selected = "Audiology"
                        ),
                        radioGroupButtons(
                          "ch_nab_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ch_nat_bench_tog", height = "800px"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The Community Health Services SitRep collects monthly data on waiting lists and times for children and young people."),
                    p("This dashboard includes percentage on the waiting list who have been waiting for children and young people services for more than 18 weeks including for speech and language, occupational therapy, paediatric service and audiology."),
                    p("This graph does not suggest that 18 weeks is an acceptable length of wait - other waiting times below 18 weeks are available through the source linked below."),
                    p("Providers submit aggregate information for service lines, irrespective of the number of ICBs or regions they provide services under. Data is submitted at provider level and can be viewed here at regional and ICB level. Therefore variation in the number of providers submitting each month could cause variation in the reported waiting list so this should be taken into account if comparing across months."),
                    p("This publication contains management data which is collected on a rapid turnaround basis, allowing only minimal validation to be undertaken. These publications are not classified as Official Statistics."),
                    tags$a(
                      href = "https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/",
                      "Source: Community Health Services Waiting Lists", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Key stage 4 destinations"),
                  h3("% of pupils with SEN going to a sustained destination post-16"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ks4_destinations_reg_panel",
                      tabPanel(
                        "Change over time",
                        selectInput("ks4_destinations_reg_time_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "EHC plan or Statement", "SEN support", "No identified SEN"),
                          selected = "Identified SEN"
                        ),
                        selectInput("ks4_destinations_reg_time_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained destination (education, apprenticeship or employment)",
                            "All destination measures"
                          ),
                          selected = "Overall sustained destination (education, apprenticeship or employment)"
                        ),
                        radioGroupButtons(
                          "destks4_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ks4_destinations_reg_bench_filter",
                          label = "Select SEN provision type for comparison",
                          choices = list("Identified SEN", "EHC plan or Statement", "SEN support", "No identified SEN"),
                          selected = "Identified SEN"
                        ),
                        selectInput("ks4_destinations_reg_bench_filter_two",
                          label = "Select destination for comparison",
                          choices = list(
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "destks4_regb_toggle",
                          label = "Display data as:",
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
                            "Overall sustained",
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
                          selected = "Overall sustained"
                        ),
                        radioGroupButtons(
                          "destks4_regtyp_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ks4_destinations_reg_type_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("These official statistics show the percentage of pupils continuing to an education, apprenticeship or employment destination in England in the year after completing key stage 4 study (after year 11) from state-funded mainstream and special schools."),
                    p("To be counted in a destination, young people have to have sustained participation for a 6 month period in the destination year. "),
                    p("This dataset is affected by the COVID-19 disruption. Many employers and apprenticeship providers took on fewer individuals during the pandemic and so it is anticipated that sustained employment and apprenticeship destinations will be lower than for previous years."),
                    p("For full details and limitations, see the source publication linked below."),
                    tags$a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-destination-measures", "Source: Key stage 4 destination measures", target = "_blank")
                  )
                )
              ),
            ),
            tabPanel(
              "Identification of need",
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils in schools with SEN"),
                  h3("% of pupils in schools with SEN, by SEN provision type"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "percent_pupils_ehcp_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ehcppc_regt_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("percent_pupils_ehcp_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This graph shows the percentage of all pupils in schools in England with SEN, by type of SEN Provision. This includes pupils with SEN support or with an education, health and care (EHC) plan."),
                    p("The data includes all state-funded nursery, primary, secondary and special schools, non-maintained special schools, pupil referral units and independent schools (note that independent schools do not have a consistent definition of SEN)."),
                    p("Note that children and young people not attending school (e.g. those at FE college or private nurseries) who have an EHC plan are not included in this graph."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils with SEN in mainstream schools"),
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
                          label = "Display data as:",
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
                          label = "Display data as:",
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
                    p("This graph shows pupils who have SEN, by SEN provision type, that attend a mainstream school (state-funded nursery, primary and secondary)."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Pupils in social care groups with SEN"),
                  h3("% of pupils with special educational needs (SEN) by social care group"),
                  box(
                    width = graph_width,
                    selectInput("cin_reg_filter",
                      label = "Show Children who are identified with....",
                      choices = list(
                        "All SEN",
                        "SEN support",
                        "EHC plan"
                      ),
                      selected = "All SEN"
                    ),
                    tabsetPanel(
                      id = "cin_reg_panel",
                      tabPanel(
                        "Change over time",
                        radioGroupButtons(
                          "cin_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("cin_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("cin_sc_reg_filter",
                          label = "Show Children who are....",
                          choices = list(
                            "CINO at 31 March",
                            "CLA 12 months at 31 March",
                            "CPPO at 31 March"
                          ),
                          selected = "CINO at 31 March"
                        ),
                        radioGroupButtons(
                          "cin_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("cin_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p(
                      "This graph shows the percentage of pupils with SEN, by SEN provision type and social care group. Social care groups included in this graph are:",
                      tags$ul(
                        tags$li("CLA 12 months - children looked after (excludes children who are in respite care in their most recent episode during the reporting year)."),
                        tags$li("CPPO - children on a child protection plan, excluding children looked after."),
                        tags$li("CINO - children in need, excluding children on a child protection plan and children looked after. This includes children on child in need plans as well as other types of plan or arrangements. It also includes children awaiting a referral to be considered, an assessment to start or, for an assessment which has started, for the assessment to be completed.")
                      )
                    ),
                    p("All references to these social care groups relate to the time point as at 31 March."),
                    p("Only data for social care groups that are available at local authority, regional and national level have been included. For further information on other groups please use the source below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england",
                      "Source: Outcomes for children in need, including children looked after by local authorities in England ", target = "_blank"
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
                          label = "Display data as:",
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
                          label = "Display data as:",
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
                    p("By default, the graph shows the percentages of all pupils in a specialist setting.
                    The drop-down menu allows you to filter the graph by SEN provision type; e.g. a value of 5% with EHC plan selected from the drop-down would mean that 5% of all pupils with EHC plans were in that school type"),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england",
                      "Source: Special educational needs in England ", target = "_blank"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  h2("Age of children and young people with EHC plans"),
                  h3("Number of children and young people by age group who have an EHC plan"),
                  box(
                    width = graph_width,
                    p(),
                    radioGroupButtons(
                      "age_regt_toggle",
                      label = "Display data as:",
                      choices = c("Chart", "Table"),
                      selected = "Chart"
                    ),
                    withSpinner(type = spinner_type, ui_element = uiOutput("ehcp_ageprofile_reg_time_tog", height = "700px"))
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    # strong("Note that for this metric, selecting  \"Inner London\" or \"Outer London\" will display statistics for London overall."), can't select Inner or Outer London anymore
                    p("These graphs show the number of children and young people with an education, health and care (EHC) plan or a statement by age group in England, as at January of each year."),
                    p("These numbers are collected from local authorities and includes children and young people both in and out of school from ages 0 to 25."),
                    p("For more information see the source publication linked below."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/education-health-and-care-plans",
                      "Source: Education, health and care plans", target = "_blank"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Financial sustainability",
              fluidRow(
                column(
                  width = 12,
                  h2("Local authority end of year surplus/deficit"),
                  h3("Dedicated Schools Grant (DSG) cumulative balance as a % of the total budget"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "dsg_deficit_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "dsg_regt_toggle",
                          label = "Display data as:",
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
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("dsg_deficit_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("The Dedicated Schools Grant (DSG) is funding given to local authorities (LA) to spend on supporting education, within legally defined limitations. One area of this is High Needs spend, which supports children and young people with education, health and care plans, among other things."),
                    p("These percentages show what proportion of the DSG funding a LA has received and the DSG cumulative position into the following year aggregated to England/Regional or LA level depending on what tab and part of the dashboard is selected."),
                    p("Where a percentage is negative, an LA has spent more than its allocated DSG (the overspend is carried forward to the next year). This is one indicator of the financial sustainability of a local SEND system."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/la-and-school-expenditure",
                      "Source: LA and school expenditure", target = "_blank"
                    ),
                    tags$a(
                      href = "https://skillsfunding.service.gov.uk/view-latest-funding/national-funding-allocations/DSG/2023-to-2024",
                      "Source: Dedicated schools grant", target = "_blank"
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
                    tabsetPanel(
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "spend_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("specialist_spend_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        radioGroupButtons(
                          "spend_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("specialist_spend_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    # p(strong("Metric not yet available at regional or England level. This graph is currently present in the LA Dashboard, and will be added here once the data becomes available.")),
                    p("This metric shows the amount of High Needs top-up funding spent within the state and independent/non-maintained sectors on special schools and alternative provision."),
                    p("It is taken from the High Needs Benchmarking tool generally used by local authorities."),
                    p("On average, independent and non-maintained provision is more expensive per head than provision in the state sector (e.g. state-funded special schools), and more children are in independent or non-maintained settings than previously."),
                    p("However, independent special schools often cater for children and young people with very complex needs which increases the average cost."),
                    tags$a(
                      href = "https://www.gov.uk/government/publications/high-needs-benchmarking-tool",
                      "Source: High needs benchmarking tool", target = "_blank"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "Alternative provision",
              # Start the about text
              column(
                12,
                div(
                  div(
                    class = "panel panel-info",
                    div(
                      class = "panel-heading",
                      style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                      h2("Alternative provision: definition and data sources")
                    ),
                    div(
                      class = "panel-body",
                      tags$div(
                        title = "Contents Box",
                        h3("Definition of alternative provision"),
                        p("Alternative provision is defined in the ", a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/942014/alternative_provision_statutory_guidance_accessible.pdf", "Alternative Provision Statutory Guidance", target = "_blank"), " as education arranged by local authorities for pupils who, because of exclusion, illness or other reasons, would not otherwise receive suitable education; education arranged by schools for pupils on a suspension; and pupils being directed by schools to off-site provision to improve their behaviour.  These placements are typically for children unable to attend a mainstream or special school."),
                        h3("AP data collections"),
                        p("The Department for Education is making improvements to the way AP data is collected, published, and measured. In June 2023, we first published data on the use of alternative provision arranged by schools. The Department for Education is currently producing new bespoke alternative provision metrics to measure increased attendance, attainment, re-integration into mainstream education or progression to sustainable post-16 destinations. We expect these additional metrics to be included in the dashboard next year. This section includes statistics from the Department for Education on:"),
                        p(strong("1) State-funded alternative provision schools"), " - includes pupils in pupil referral units, AP academies and AP free schools and the data is collected through the school census. This includes placements arranged by local authorities and by schools."),
                        p(strong("2) School arranged alternative provision"), " - data on full and part time alternative provision placements arranged by schools collected in the school census. The department started collecting this data in January 2023, so data is not shown for previous years."),
                        p(strong("3) Local authority funded alternative provision"), " - placements in non-state-funded provision collected via the alternative provision census. This includes placements in non-state-funded provision when:"),
                        tags$ul(
                          tags$li("i) LAs arrange education for pupils who, because of exclusion, illness or other reasons, would not otherwise receive suitable education; using their Section 19 powers."),
                          tags$li("ii) LAs make educational provision for children and young people with Education, Health and Care plans under their duties in the Children and Families Act. To note, that many of these placements won’t meet the department's definition of alternative provision, as set out above, but we have included placements in the dashboard for transparency purposes.")
                        ),
                        p("Non-state-funded provision includes non-maintained special schools, independent schools, independent special schools, FE colleges and unregistered alternative provision settings. This may include some pupils who are included in the school census or school level annual school census, for example where the local authority is funding a placement in a non-maintained special school or independent special school."),
                        p("There are expected to be some overlaps between these data sets. This includes pupils attending pupil referral units, who also have other alternative provision either arranged by the school, or local authority funded alternative provision. It is therefore not advised to sum across the totals in each section."),
                        p("The figures on numbers of pupils and placements shown below represent a snapshot at the point the data was collected in the alternative provision census (January) and school census (January). Therefore, the numbers of pupils and placements over a whole year will be a higher number than the snapshot figures shown."),
                        tags$ul(
                          tags$li("State-funded AP figures includes those enrolled at a state-funded AP school on census day (January)."),
                          tags$li("School arranged AP includes open placements on the school census day (January)."),
                          tags$li("LA funded AP includes open placements as at the alternative provision census day (January).")
                        ),
                      ),
                      br()
                    )
                  )
                ),
              ), # End of the about text for AP

              fluidRow( # Start AP counts
                column(
                  width = 12,
                  h2("Total pupils at an AP"),
                  h3("Total pupils by selected type of AP"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_count_reg_panel", # This is a required argument and should be a unique identifier for the tabsetPanel(). It is used to reference and manipulate the tabset panel in your Shiny app
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_counts_reg_time_filter",
                          label = "Select AP type",
                          choices = list("LA funded AP placements", "School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_counts_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_counts_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        selectInput("ap_counts_reg_bench_filter",
                          label = "Select AP type",
                          choices = list("LA funded AP placements", "School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_counts_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_counts_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This publication provides statistics on the number of pupils at state-funded alternative provision schools (collected via the school census), school arranged alternative provision (collected via the school census), and LA arranged alternative provision (collected via the AP census).
School arranged alternative provision figures refer to pupils in school arranged alternative provision at January 2023.
State-funded alternative provision figures include pupils placed by schools and local authorities and whose sole or main registration is in a state-funded AP school at the Spring census. The number of dual subsidiary registrations at a state-funded AP will be added separately in 2024, when a time series dataset will become available.
Local authority funded placements in non state-funded provision figures refer to pupils with an open placement as at census date who have been recorded in the alternative provision census as being in a local authority funded alternative provision setting."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                      "Source: Schools, pupils and their characteristics: January"
                    )
                  )
                )
              ), # End of AP counts metric

              fluidRow(
                column(
                  width = 12,
                  h2("Characteristics of pupils at an AP"),
                  h3("% of pupils by selected characteristic at an AP"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_characteristics_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_characteristics_reg_time_filter",
                          label = "Select pupil characteristic",
                          choices = list(
                            "All SEN",
                            "SEN status",
                            "Free school meal status",
                            "Age breakdown",
                            "Gender"
                          ),
                          selected = "All SEN"
                        ),
                        selectInput("ap_characteristics_reg_time_filter_type",
                          label = "Select AP type",
                          choices = list("School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_characteristics_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_characteristics_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        selectInput("ap_characteristics_reg_bench_filter",
                          label = "Select pupil characteristic",
                          choices = list(
                            "All SEN",
                            "SEN status",
                            "Free school meal status",
                            "Age breakdown",
                            "Gender"
                          ),
                          selected = "All SEN"
                        ),
                        selectInput("ap_characteristics_reg_bench_filter_type",
                          label = "Select AP type",
                          choices = list("School arranged AP", "State-funded AP school"),
                          selected = "State-funded AP school"
                        ),
                        radioGroupButtons(
                          "ap_characteristics_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_characteristics_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This publication provides statistics on the percentage of pupils at state-funded alternative provision schools, where pupils with dual subsidiary registration in state-funded AP schools are not included, and school arranged alternative provision (collected via the school census), by selected characteristic."),
                    p(
                      "Sources used:",
                      tags$a(
                        href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                        "Special educational needs in England (for state-funded AP data)"
                      ), " and ",
                      tags$a(
                        href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                        "Schools, pupils and their characteristics: January (for school arranged AP data)"
                      )
                    )
                  )
                )
              ), # end of the characteristics metric


              fluidRow(
                column(
                  width = 12,
                  h2("State-funded AP pupil absences"),
                  h3("Absence measures"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_absences_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        radioGroupButtons(
                          "ap_absences_regt_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_absences_reg_time_tog"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        selectInput("ap_absences_reg_filter",
                          label = "Select absence measure for comparison",
                          choices = list(
                            "Overall absence %",
                            "Authorised absence %",
                            "Unauthorised absence %",
                            "% of persistent absentees - 10% or more sessions missed",
                            "% of persistent absentees - 50% or more sessions missed"
                          ),
                          selected = "Overall absence %"
                        ),
                        p(),
                        radioGroupButtons(
                          "ap_absences_regb_toggle",
                          label = "Display data as:",
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_absences_reg_bench_tog", height = "120%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("'Absence' refers to children who are absent for authorised and unauthorised reasons. During 2020/21 and 2021/22, this includes children who are absent with a positive COVID case – but does not include children who are isolating but have not had a confirmed positive case, for example as a contact."),
                    p("Absence statistics refer to the enrolment level rather than pupil level. This means where a pupil has moved school throughout the year, they will be counted more than once as they have recorded attendance and absence at more than one school. Schools only record absence for the period a pupil is on roll at their school. This means that pupil enrolments who are persistently absent or severely absent in a short period of enrolment at a school will have very high rates. For further detail, see the source publication linked below."),
                    tags$a(
                      href = "https://www.gov.uk/government/collections/statistics-pupil-absence",
                      "Source: Pupil absence in schools in England: autumn and spring", target = "_blank"
                    )
                  )
                )
              ), # end of AP absences metric

              fluidRow(
                column(
                  width = 12,
                  h2("Ofsted most recent overall school effectiveness grade"),
                  h3("Most recent Ofsted overall effectiveness rating for state-funded schools, over time"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_ofsted_reg_panel",
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_ofsted_reg_time_filter", # One
                          label = "Select school type (these categories are mutually exclusive)",
                          choices = list(
                            "State-funded AP school",
                            "State-funded special school",
                            "Non-maintained special school",
                            "State-funded mainstream school"
                          ),
                          selected = "State-funded AP school"
                        ),
                        selectInput("ap_ofsted_reg_time_filter_two", # two
                          label = "Select measure (be aware that '% of schools' may be based on very small numbers)",
                          choices = list(
                            "Overall effectiveness (number of schools)",
                            "Overall effectiveness (% of schools)"
                          ),
                          selected = "Overall effectiveness (number of schools)"
                        ),
                        radioGroupButtons(
                          "ap_ofsted_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_ofsted_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        selectInput("ap_ofsted_reg_bench_filter", # One
                          label = "Select school type (these categories are mutually exclusive)",
                          choices = list(
                            "State-funded AP school",
                            "State-funded special school",
                            "Non-maintained special school",
                            "State-funded mainstream school"
                          ),
                          selected = "State-funded AP school"
                        ),
                        selectInput("ap_ofsted_reg_bench_filter_two", # two
                          label = "Select measure (be aware that '% of schools' may be based on very small numbers)",
                          choices = list(
                            "Overall effectiveness (number of schools)",
                            "Overall effectiveness (% of schools)"
                          ),
                          selected = "Overall effectiveness (number of schools)"
                        ),
                        radioGroupButtons(
                          "ap_ofsted_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_ofsted_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("This metric uses Ofsted state-funded school inspections and outcomes as at 31 August publication, which is published in the final quarter of the calendar year. For each year of data, the data uses both provisional data (used for the period January to August period) and revised data (for the September to December period of the previous calendar year). NULL values means that a school has not yet been inspected. Although the focus of this metric is for state-funded AP, users can also view overall effectiveness ratings for other state-funded school types for comparative purposes."),
                    tags$a(
                      href = "https://www.gov.uk/education/inspection-of-maintained-schools-and-academies",
                      "Source: Ofsted State-funded schools inspections and outcomes as at 31 August."
                    )
                  )
                )
              ), # end of the Ofsted metric


              fluidRow(
                column(
                  width = 12,
                  h2("Unregistered alternative provision (UAP) numbers"),
                  h3("UAP numbers at school arranged AP (total pupils) and LA-funded AP (total placements)"),
                  box(
                    width = graph_width,
                    tabsetPanel(
                      id = "ap_uap_panel_reg",
                      tabPanel(
                        "Change over time",
                        p(),
                        selectInput("ap_uap_reg_time_filter", # One
                          label = "Select provision type",
                          choices = list(
                            "LA funded unregistered AP placements",
                            "School arranged unregistered AP pupils"
                          ),
                          selected = "School arranged unregistered AP pupils"
                        ),
                        radioGroupButtons(
                          "ap_uap_regt_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_uap_reg_time_tog", height = "110%"))
                      ),
                      tabPanel(
                        "Regional comparison",
                        p(),
                        selectInput("ap_uap_reg_bench_filter", # One
                          label = "Select provision type",
                          choices = list(
                            "LA funded unregistered AP placements",
                            "School arranged unregistered AP pupils"
                          ),
                          selected = "School arranged unregistered AP pupils"
                        ),
                        radioGroupButtons(
                          "ap_uap_regb_toggle",
                          label = NULL,
                          choices = c("Chart", "Table"),
                          selected = "Chart"
                        ),
                        withSpinner(type = spinner_type, ui_element = uiOutput("ap_uap_reg_bench_tog", height = "110%"))
                      )
                    )
                  ),
                  box(
                    width = info_width, title = "About this indicator", solidHeader = TRUE,
                    p("For this indicator, unregistered provision is defined as placements at settings without a Unique Reference Number (URN). The Grand total unregistered AP (all setting types) bar represents the overall sum of unregistered AP across all setting types. For the 'Change over time view', each individual unregistered AP setting type's total is also displayed alongside the corresponding grand total. The unregistered AP setting types collected are 'Registered provider with UKPRN'; 'Non-maintained further education college'; 'One on one tuition'; 'Other unregistered provider'; 'Work based placement'; and 'Unknown'. This indicator uses statistics on the number of placements in non-school based unregistered alternative provision arranged by schools (collected via the school census) and by LAs collected via the AP census). Note that the number of placements figure can differ from the number of pupils figure as pupils can have multiple placements, so you cannot simply add school arranged unregistered AP pupils and LA funded unregistered AP placements to accurately identify the total number of pupils in UAP across both provision types."),
                    tags$a(
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/school-pupils-and-their-characteristics",
                      "Source: Schools, pupils and their characteristics: January"
                    )
                  )
                )
              ), # end of UAP metric
            ) # End of overall AP panel
          )
        )
      )
    )
  )
}
