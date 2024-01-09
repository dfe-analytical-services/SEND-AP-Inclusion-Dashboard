a11y_panel <- function() {
  tabPanel(
    value = "access",
    "Accessibility",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Accessibility statement"),
          br("This accessibility statement applies to the SEND and AP Inclusion Dashboard.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
          h2("WCAG 2.1 compliance"),
          br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)", target = "_blank"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool", target = "_blank"), ", which detected a small number of accessibility issues as outlined below.
             Each page in this application has been audited for accessiblity with the page-snapshot functionality in the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool", target = "_blank"), ".
             This application does not fully pass the accessibility auditing, partially due to limitations in the software (R-Shiny) used to produce the dashboard. Specific reasons for this are outlined in the limitations section below, whilst to mitigate the accessibility challenges here, we also provide a link to the underlying data for each metric in the 'about this indicator' panel."),
          br("This app does however follow the following guidelines:"),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
            tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
          )),
          h2("Limitations"),
          br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
          tags$div(tags$ul(
            tags$li("Alternative text for interactive charts is limited to titles, although the data behind each chart is available in table format via the chart/table buttons."),
            tags$li("This dashboard uses R-Shiny components that have some issues with badly formatted aria identifiers."),
            tags$li("Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users.")
          )),
          h2("Browser compatibility"),
          br("The browsers used were Edge Chromium and Chrome as these are standard in the DfE and its agencies. The operating system used was Windows."),
          br("This statement was prepared and last updated on 18th October 2023."),
          h2("Feedback"),
          br(
            "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
            a(href = "mailto:sendap.dashboard@education.gov.uk", "sendap.dashboard@education.gov.uk")
          )
        )
      )
    )
  )
}

support_links <- function() {
  tabPanel(
    value = "support",
    "Support and feedback",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h2("Give us feedback"),
          "This dashboard is a new service that we will continue to develop with your feedback. Please send any feedback or suggestions for improvements to ",
          a(
            href = "mailto:sendap.dashboard@education.gov.uk",
            "sendap.dashboard@education.gov.uk", .noWS = c("after")
          ), ".",
          br(),
          "If you spot any errors or bugs while using this dashboard, please screenshot and email them to ",
          a(
            href = "mailto:sendap.dashboard@education.gov.uk",
            "sendap.dashboard@education.gov.uk", .noWS = c("after")
          ), ".",
          br(),
          h2("Find more information on the data"),
          "The majority of the data used to produce the dashboard, along with methodological information can be found on ",
          a(href = "https://explore-education-statistics.service.gov.uk/", "Explore Education Statistics", .noWS = c("after"), target = "_blank"),
          ".",
          "Some non-education data is also used; including healthcare information from NHS England. Each graph or table has its data source linked in the right-hand side panel. We are also pleased to make available",
          a(href = "not a link yet", "a more comprehensive list of all metrics and data sources."),
          br(),
          h2("Contact us"),
          "If you have questions about the dashboard or data within it, please contact us at ",
          a(href = "mailto:sendap.dashboard@education.gov.uk", "sendap.dashboard@education.gov.uk", .noWS = c("after"), target = "_blank"), br(),
          h2("See the source code"),
          "The source code for this dashboard is available in our ",
          a(href = "https://github.com/dfe-analytical-services/SEND-AP-Inclusion-Dashboard", "GitHub repository", .noWS = c("after"), target = "_blank"),
          ".",
          br()
        ),
        column(
          12,
          h2("Use of cookies"),
          textOutput("cookie_status"),
          actionButton("remove", "Reset cookie consent"),
        )
      )
    )
  )
}
