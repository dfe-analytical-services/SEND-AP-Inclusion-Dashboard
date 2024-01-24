# ---------------------------------------------------------
# This is the server file.
# This contains the code to generate the plots and table used in the dashboard.
# It is the "interactive" element, with pre-processing already done by data_preparation.R whenever necessary
# ---------------------------------------------------------

server <- function(input, output, session) {
  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Bookmarking ------------------------------------------------------------------------------
  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs using the list here:
  # we need to exclude stuff here or the app will crash with a maximum URI length error
  # excluding Plotly and DT save-state stuff that the end-user can't really control
  # this means we need a list of all the tables
  edu_metrics <- c(
    "destinations_1618", "ks1_phonics", "ks2_attainment", "ks4_attainment", "mentalhealth_ccg", "destinations_1618",
    "eyfsp", "ap_counts", "ap_characteristics", "timeliness", "tribunals", "absence", "ks4_destinations",
    "tribunals_reg_bench_table", "absence_reg_bench_table", "percent_pupils_ehcp", "ehcp_ageprofile",
    "mainstream_with_sen", "provider_types", "ap_absences", "ap_ofsted", "ap_uap", "ap_counts",
    "ap_counts", "discontinued"
  )
  other_tables <- c(
    "mentalhealth_ccg_time_table", "mentalhealth_ccg_bench_table", "mentalhealth_reg_time_table", "reg_ofsted_rating",
    "mentalhealth_reg_bench_table", "autism_ccg_time_table", "autism_nat_time_table", "autism_nat_bench_table",
    "ap_summary_table", "summary_table"
  )
  edu_tables <- unlist(map(edu_metrics, paste0, c("_la_time_table", "_la_bench_table", "_reg_time_table", "_reg_bench_table")))
  all_tables <- c(edu_tables, other_tables)
  tables_exclude <- unlist(map(all_tables, paste0, c("_rows_current", "_rows_all", "_rows_selected", "_columns_selected", "_state", "_search", "_cell_clicked", "_cells_selected")))
  setBookmarkExclude(c(
    "cookies", "link_to_app_content_tab", "cookieAccept", "cookieReject", "cookieLink", "hideAccept", "hideReject",
    "remove", ".clientValue-default-plotlyCrosstalkOpts", "plotly_hover-A", "plotly_afterplot-A", "plotly_relayout-A",
    "link_eyfsp_reg_panel", "link_ks1_phonics_reg_panel", "link_ks2_attainment_reg_panel", "link_ks4_attainment_reg_panel",
    "link_mh_reg_panel", "link_ofsted_reg_panel", "link_1618_reg_panel", "link_disco_reg_panel", "link_timeliness_reg_panel",
    "link_absence_reg_panel", "link_autism_reg_panel", "link_ks4_destinations_reg_panel", "link_statefunded_reg_panel",
    "link_mainstream_reg_panel", "link_special_reg_panel", "link_cin_reg_panel", "link_cin_reg_panel", "link_deficit_reg_panel",
    "link_spend_reg_panel", "link_ap_sf_count_panel", "link_ap_la_count_panel", "link_uap_sa_count_panel", "link_uap_la_count_panel",
    "link_ap_sen_panel", "link_ap_absence_panel", "link_ap_ofsted_panel", tables_exclude
  ))


  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })

  # Dynamic UI elements ---------------------------------------------------------------------------

  observeEvent(input$link_to_la_dashboard, {
    updateTabsetPanel(session, "navlistPanel", selected = "la_dashboard")
  })

  observeEvent(input$link_to_engreg_dashboard, {
    updateTabsetPanel(session, "navlistPanel", selected = "engreg_dashboard")
  })

  observeEvent(input$link_to_accessibility, {
    updateTabsetPanel(session, "navlistPanel", selected = "access")
  })

  observeEvent(input$link_to_feedback, {
    updateTabsetPanel(session, "navlistPanel", selected = "support")
  })




  # ==========================================
  # Generic functions used for viz
  # ==========================================

  format_axis_label <- function(label, max_length = 40) {
    formatted_label <- strwrap(label, width = max_length, simplify = TRUE)
    return(paste(formatted_label, collapse = "\n"))
  }

  # Generic bar chart plot function
  plot_reg_bench <- function(data, outcome_var, hover_label, label_y, region_choice_validated, national_average = NULL) {
    data <- data %>%
      mutate(
        chosen_region = case_when(
          region_name == region_choice_validated & input$level_choice == "Regions" ~ region_choice_validated,
          input$level_choice == "England" ~ "English regions",
          TRUE ~ "Other region"
        )
      ) %>%
      ggplot(aes(
        x = fct_reorder(region_name, outcome_var),
        y = outcome_var,
        text = paste0(
          Region, "\n", # always state region then line break
          hover_label, ":", outcome_var
        ),
        fill = chosen_region,
        label = ""
      )) +
      geom_col() +
      labs(
        x = "Regions in England",
        y = label_y,
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = af_purple), na.value = af_grey)
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }

    # Conditional English average line added if national_average created
    ## It doesn't make sense to add this line for national counts,
    if (!is.null(national_average) && is.data.frame(national_average)) {
      print("Yes, national_average exists and is a data frame.")
      # Include the code related to national_average
      data <- data +
        add_england_line_bench(national_average) +
        add_england_label_bench_reg(national_average, nudge = 3)
    } else {
      print("No valid national_average provided or it is not a data frame. Skipping related code.")
    }

    data <- ggplotly(data, tooltip = c("text")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )

    return(data) # plot this
  }


  # Generic LA bench chart plot function
  plot_la_bench <- function(data, outcome_var, hover_label, label_y, la_choice_validated, national_average = NULL) {
    data <- data %>%
      ggplot(aes(
        x = fct_reorder(la_name, !!sym(outcome_var)),
        y = !!sym(outcome_var),
        text = paste0(la_name, "\n", hover_label, ":", !!sym(outcome_var)),
        fill = chosen_la,
        label = ""
      )) +
      geom_col() +
      labs(
        x = data$rank_statement[data$la_name == la_choice_validated],
        y = label_y,
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

    # Conditional English average line added if national_average created
    if (!is.null(national_average) && is.data.frame(national_average)) {
      print("Yes, national_average exists and is a data frame.")
      # Include the code related to national_average
      data <- data +
        add_england_line_bench(national_average) +
        add_england_label_bench(national_average, input = input, nudge = 3)
    } else {
      print("No valid national_average provided or it is not a data frame. Skipping related code.")
    }

    data <- data %>%
      ggplotly(
        tooltip = c("text"),
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )

    return(data) # plot this
  }


  # Generic DT table plot function
  table_bench_time <- function(data, var_filter, outcome_var, var_filter_two = NULL, digits = 2) {
    data <- data %>%
      mutate(Region = region_name) %>%
      arrange(
        desc(`Academic year`),
        !!var_filter,
        !!var_filter_two
      ) %>%
      select(
        `Academic year`,
        Region,
        !!var_filter,
        !!var_filter_two,
        !!outcome_var
      )
    return(DTise(data, list(order = c(list(0, "desc"), list(2, "asc")))) %>%
      formatRound(!!outcome_var, digits))
  }



  # observers to turn the summary boxes into links
  # procedure uses three observers:
  # 1. Switches tabs, sets link target
  # 2. Fires on tab switch, copies target and triggers #3 (this one is necessary because otherwise the scroll fires before the tab switches)
  # 3. Scrolls to target and resets target variable

  # 1. needs to be different for all of the boxes as it has a different input parameter; for the other two we only need one observer each
  observeEvent(input$link_eyfsp_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "eyfsp_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("eyfsp_reg_panel")
  })

  observeEvent(input$link_ks1_phonics_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "ks1_phonics_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("ks1_attainment_reg_panel")
  })

  # the version for the other metrics where scrolling is required (the system )
  observeEvent(input$link_ks2_attainment_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "ks2_attainment_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("ks2_attainment_reg_panel")
  })


  observeEvent(input$link_ks4_attainment_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "ks4_attainment_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("ks4_attainment_reg_panel")
  })

  observeEvent(input$link_mh_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "mentalhealth_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("mh_reg_panel")
  })

  observeEvent(input$link_ofsted_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "ofsted_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("ofsted_reg_panel")
  })

  observeEvent(input$link_1618_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "destinations_1618_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("destinations_1618_reg_panel")
  })



  observeEvent(input$link_disco_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Outcomes")
    updateTabsetPanel(session, "disco_reg_panel", selected = "Change over time")
    this_tab("Outcomes")
    move_target("disco_reg_panel")
  })

  observeEvent(input$link_timeliness_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Experiences")
    updateTabsetPanel(session, "timeliness_reg_panel", selected = "Change over time")
    this_tab("Experiences")
    move_target("timeliness_reg_panel")
  })

  observeEvent(input$link_tribunals_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Experiences")
    updateTabsetPanel(session, "tribunals_reg_panel", selected = "Change over time")
    this_tab("Experiences")
    move_target("tribunals_reg_panel")
  })

  observeEvent(input$link_absence_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Experiences")
    updateTabsetPanel(session, "absence_reg_panel", selected = "Change over time")
    this_tab("Experiences")
    move_target("absence_reg_panel")
  })

  observeEvent(input$link_autism_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Experiences")
    updateTabsetPanel(session, "autism_reg_panel", selected = "Change over time")
    this_tab("Experiences")
    move_target("autism_reg_panel")
  })

  observeEvent(input$link_ks4_destinations_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Experiences")
    updateTabsetPanel(session, "ks4_destinations_reg_panel", selected = "Change over time")
    this_tab("Experiences")
    move_target("ks4_destinations_reg_panel")
  })

  observeEvent(input$link_statefunded_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of need")
    updateTabsetPanel(session, "percent_pupils_ehcp_reg_panel", selected = "Change over time")
    this_tab("Identification of need")
    move_target("percent_pupils_ehcp_reg_panel")
  })

  observeEvent(input$link_mainstream_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of need")
    updateTabsetPanel(session, "mainstream_with_sen_reg_panel", selected = "Change over time")
    this_tab("Identification of need")
    move_target("mainstream_with_sen_reg_panel")
  })

  observeEvent(input$link_special_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of need")
    updateTabsetPanel(session, "provider_types_reg_panel", selected = "Change over time")
    this_tab("Identification of need")
    move_target("provider_types_reg_panel")
  })


  observeEvent(input$link_cin_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of need")
    updateTabsetPanel(session, "cin_reg_panel", selected = "Change over time")
    this_tab("Identification of need")
    move_target("cin_reg_panel")
  })

  observeEvent(input$link_deficit_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Financial sustainability")
    updateTabsetPanel(session, "dsg_deficit_reg_panel", selected = "Change over time")
    this_tab("Financial sustainability")
    move_target("dsg_deficit_reg_panel")
  })

  observeEvent(input$link_spend_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Financial sustainability")
    updateTabsetPanel(session, "specialist_spend_reg_panel", selected = "Change over time")
    this_tab("Financial sustainability")
    move_target("specialist_spend_reg_panel")
  })

  observeEvent(input$link_ap_sf_count_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_count_reg_panel", selected = "Change over time")
    updateSelectInput(session, "ap_counts_reg_time_filter", selected = "State-funded AP school")
    this_tab("Alternative provision")
    move_target("ap_count_reg_panel")
  })

  observeEvent(input$link_ap_sa_count_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_count_reg_panel", selected = "Change over time")
    updateSelectInput(session, "ap_counts_reg_time_filter", selected = "School arranged AP")
    this_tab("Alternative provision")
    move_target("ap_count_reg_panel")
  })

  observeEvent(input$link_ap_la_count_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_count_reg_panel", selected = "Change over time")
    updateSelectInput(session, "ap_counts_reg_time_filter", selected = "LA funded AP placements")
    this_tab("Alternative provision")
    move_target("ap_count_reg_panel")
  })

  observeEvent(input$link_uap_sa_count_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_uap_panel_reg", selected = "Change over time")
    updateSelectInput(session, "ap_uap_reg_time_filter", selected = "School arranged unregistered AP pupils")
    this_tab("Alternative provision")
    move_target("ap_uap_panel_reg")
  })

  observeEvent(input$link_uap_la_count_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_uap_panel_reg", selected = "Change over time")
    updateSelectInput(session, "ap_uap_reg_time_filter", selected = "LA funded unregistered AP placements")
    this_tab("Alternative provision")
    move_target("ap_uap_panel_reg")
  })

  observeEvent(input$link_ap_ofsted_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_ofsted_reg_panel", selected = "Change over time")
    this_tab("Alternative provision")
    move_target("ap_ofsted_reg_panel")
  })

  observeEvent(input$link_ap_absence_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_absences_reg_panel", selected = "Change over time")
    this_tab("Alternative provision")
    move_target("ap_absences_reg_panel")
  })

  observeEvent(input$link_ap_sen_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Alternative provision")
    updateTabsetPanel(session, "ap_characteristics_reg_panel", selected = "Change over time")
    updateSelectInput(session, "ap_characteristics_reg_time_filter", selected = "SEN status")
    this_tab("Alternative provision")
    move_target("ap_characteristics_reg_panel")
  })

  # these are parts two and three of the observer chain discussed above
  # this code could definitely be improved - currently for some reason it runs twice, and the scroll actually happens on the second run
  # since it works and is not particularly slow, I am not going to spend longer fiddling with reactivity to fix this, but if performance
  # here becomes an issue it may need fixing
  observeEvent(
    {
      input$tabsetpanels_reg == this_tab()
    },
    {
      move_here(move_target())
      trigger(TRUE)
    },
    ignoreInit = TRUE
  )

  observeEvent(
    {
      trigger() == TRUE
    },
    {
      scroll(move_here())
      move_here("")
      trigger(FALSE)
    },
    ignoreInit = TRUE
  )


  # Render a region drop-down only if Regions level is selected
  output$region_choice_out <- renderUI({
    if (input$level_choice == "Regions") {
      selectInput(
        inputId = "region_choice",
        label = "Choose region",
        choices = region_list
      )
    } else {
      return(NULL)
    }
  })

  # Region to NHS region lookup
  # This updates the NHS region selectinput depending on what you've selected as the main region
  observeEvent(input$region_choice, {
    default_nhs_region <- case_when(
      input$region_choice %in% c("East Midlands", "West Midlands") ~
        "Midlands",
      input$region_choice %in% c("Inner London", "Outer London") ~
        "London",
      input$region_choice %in% c("North East", "Yorkshire and The Humber") ~
        "North East and Yorkshire",
      TRUE ~ input$region_choice
    )

    updateSelectInput(session, "nhs_region_choice", selected = default_nhs_region)
  })

  observeEvent(input$la_choice, {
    updatePickerInput(session, "ccg_choice",
      selected = character(0),
      choices =
        la_ccg_lookup %>%
          filter(la_name == input$la_choice) %>%
          pull(nhs_name)
    )
  })

  ## COOKIES DIALOGUE

  # output if cookie is unspecified
  observeEvent(input$cookies, {
    if (!(isTRUE(getOption("shiny.testmode")))) { # only bother with cookies outside of test mode, the popup breaks shinydriver
      if (!is.null(input$cookies)) {
        if (!("dfe_analytics" %in% names(input$cookies))) { # only pop-up if not already set and not in test mode

          shinyalert(
            inputId = "cookie_consent",
            title = "Cookie consent",
            text = "This site uses cookies to record traffic flow using Google Analytics",
            size = "s",
            closeOnEsc = TRUE,
            closeOnClickOutside = FALSE,
            html = FALSE,
            type = "",
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Accept",
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE
          )
        } else {
          msg <- list(
            name = "dfe_analytics",
            value = input$cookies$dfe_analytics
          )
          session$sendCustomMessage("analytics-consent", msg)
          if ("cookies" %in% names(input)) {
            if ("dfe_analytics" %in% names(input$cookies)) {
              if (input$cookies$dfe_analytics == "denied") {
                ga_msg <- list(name = paste0("_ga_", google_analytics_key))
                session$sendCustomMessage("cookie-remove", ga_msg)
              }
            }
          }
        }
      }
    }
  })

  observeEvent(input$cookie_consent, {
    msg <- list(
      name = "dfe_analytics",
      value = ifelse(input$cookie_consent, "granted", "denied")
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "denied") {
          ga_msg <- list(name = paste0("_ga_", google_analytics_key))
          session$sendCustomMessage("cookie-remove", ga_msg)
        }
      }
    }
  })

  observeEvent(input$remove, {
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools, this site uses cookies to identify numbers of unique users as part of Google Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      "Cookies consent has not been confirmed."
    }
  })


  #  output$cookie_status <- renderText(as.character(input$cookies))


  # OUTCOMES GRAPHS ---------------------------------------------------------------------------

  ### KS2 Attainment ####

  ## KS2 Attainment (LA/time)
  output$ks2_attainment_la_time <- renderPlotly({
    ## req(input$la_choice)
    validate(need(input$la_choice, message = "Please select a Local Authority"))

    ks2_attainment_la_time <- ks2_attainment %>%
      fsubset(la_name == input$la_choice) %>%
      AY_to_date(academic_year) %>%
      ggplot(aes(
        x = AY_date,
        y = `Percent meeting expected standard`,
        group = characteristic,
        colour = characteristic,
        text = paste(
          "Academic Year:", ay_date_to_ay(AY_date), "\n",
          "% meeting expected standard:", `Percent meeting expected standard`, "\n",
          "SEN status:", characteristic
        ),
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% of pupils meeting expected standard",
        x = "Academic year",
        colour = "Group"
      ) +
      scale_colour_manual(values = af_palette) +
      scale_x_date(breaks = c(ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")), labels = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23")) +
      coord_cartesian(ylim = c(0, NA))

    ks2_attainment_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks2_attainment_la_time_table <- renderDT({
    ks2_attainment_lat <- ks2_attainment %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(
        `Academic Year` = academic_year,
        `SEN Status` = characteristic,
        `Percent meeting expected standard`
      ) %>%
      arrange(
        `Academic Year`,
        `SEN Status`
      )
    return(DTise(
      ks2_attainment_lat,
      list(list(1, "asc"), list(0, "desc"))
    ) %>%
      formatRound("Percent meeting expected standard", 2))
  })


  ## KS2 Attainment (LA/bench)
  output$ks2_attainment_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, ks2_attainment)

    ks2_attainment_la_bench_data <- ks2_attainment %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$ks2_attainment_la_filter &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "Percent meeting expected standard")

    # Create rank statements to go on the X axis.
    ks2_attainment_la_bench_data$rank_statement <- rank_statement_fun(ks2_attainment_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )

    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks2_attainment %>%
      collapse::fsubset(characteristic == input$ks2_attainment_la_filter &
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = `Percent meeting expected standard`)

    # Create the plot
    ks2_attainment_la_bench <- ks2_attainment_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `Percent meeting expected standard`),
        y = `Percent meeting expected standard`,
        text = paste(
          la_name, "\n",
          "% meeting expected standard:", `Percent meeting expected standard`, "\n",
          "SEN status:", characteristic
        ),
        fill = chosen_la
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = ks2_attainment_la_bench_data$rank_statement[ks2_attainment_la_bench_data$la_name == input$la_choice],
        y = "% of pupils meeting expected standard",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

    # Pass plot into plotly
    ks2_attainment_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      # This code makes the dotted line a bit thinner.
      # You can use plotly_json() to examine the inner workings of plotly graphs to work out trace numbers
      restyle_england_line() %>% # Make the England line label align correctly and thin the line
      save_plot_button_only() %>%
      use_horizontal_legend()
  })

  output$ks2_attainment_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, ks2_attainment)

    ks2_attainment_lab <- ks2_attainment %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      })

    ks2_attainment_lab <- ks2_attainment_lab %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$ks2_attainment_la_filter &
        time_period == comparison_year) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local authority` = la_name,
        `SEN Status` = characteristic,
        `Percent meeting expected standard`
      ) %>%
      arrange(desc(`Percent meeting expected standard`))
    return(DTise(
      ks2_attainment_lab,
      list(list(3, "desc"))
    ) %>%
      formatRound("Percent meeting expected standard", 2))
  })

  ## KS2 Attainment (region/time)
  output$ks2_attainment_reg_time <- renderPlotly({
    ## req(input$level_choice)
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ks2_attainment_reg_time <- ks2_attainment %>%
        filter(
          region_name == input$region_choice,
          geographic_level == "Regional"
        ) %>%
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Percent meeting expected standard`,
          group = characteristic,
          colour = characteristic,
          text = paste(
            "Academic Year:", ay_date_to_ay(AY_date), "\n",
            "% meeting expected standard:", `Percent meeting expected standard`, "\n",
            "SEN status:", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils meeting expected standard",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_x_date(
          breaks = c(ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")),
          labels = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
        ) +
        coord_cartesian(ylim = c(0, NA))
    } else if (input$level_choice == "England") {
      ks2_attainment_reg_time <- ks2_attainment %>%
        filter(geographic_level == "National") %>%
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Percent meeting expected standard`,
          group = characteristic,
          colour = characteristic,
          text = paste(
            "Academic Year:", ay_date_to_ay(AY_date), "\n",
            "% meeting expected standard:", `Percent meeting expected standard`, "\n",
            "SEN status:", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils meeting expected standard",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_x_date(breaks = c(ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")), labels = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23")) +
        coord_cartesian(ylim = c(0, NA))
    }

    ks2_attainment_reg_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks2_attainment_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks2_attainment_ret <- ks2_attainment %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional") %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          `Percent meeting expected standard`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN Status`
        )

      return(DTise(
        ks2_attainment_ret,
        list(list(2, "asc"), list(0, "desc"))
      ) %>%
        formatRound("Percent meeting expected standard", 2))
    } else {
      ks2_attainment_et <- ks2_attainment %>%
        fsubset(geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN Status` = characteristic,
          `Percent meeting expected standard`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN Status`
        )
      return(DTise(
        ks2_attainment_et,
        list(list(2, "asc"), list(0, "desc"))
      ) %>%
        formatRound("Percent meeting expected standard", 1))
    }
  })

  ## KS2 Attainment (region/bench)
  output$ks2_attainment_reg_bench <- renderPlotly({
    # req(input$level_choice)
    # if(input$level_choice == "Regions") req(input$region_choice)

    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks2_attainment %>%
      collapse::fsubset(characteristic == input$ks2_attainment_reg_filter &
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = `Percent meeting expected standard`) %>%
      mutate(label = "England average")


    ks2_attainment_reg_bench1 <- ks2_attainment %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks2_attainment_reg_filter &
        time_period == max(time_period)) %>%
      mutate(
        chosen_region =
          if (input$level_choice == "Regions" & is.character(input$region_choice)) {
            case_when(
              region_name == input$region_choice ~ input$region_choice,
              TRUE ~ "Other regions"
            )
          } else {
            "English regions"
          }
      )

    ks2_attainment_reg_bench <- ks2_attainment_reg_bench1 %>%
      collapse::ftransform(
        region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
      ) %>%
      ggplot(aes(fct_reorder(region_name, `Percent meeting expected standard`),
        y = `Percent meeting expected standard`,
        text = paste(
          region_name, "\n",
          "% meeting expected standard:", `Percent meeting expected standard`, "\n",
          "SEN status:", characteristic
        ),
        fill = chosen_region
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 3) +
      labs(
        x = "Regions in England",
        y = "% of pupils meeting expected standard",
        fill = "Region"
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = 1
      ), legend.position = "none") +
      if (input$level_choice == "England") {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      } else {
        scale_fill_manual(values = c("Other regions" = not_focused), na.value = focused)
      }


    ks2_attainment_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>% # Make the England line label align correctly and thin the line
      use_horizontal_legend()
  })

  output$ks2_attainment_reg_bench_table <- renderDT({
    ks2_attainment_reb <- ks2_attainment %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks2_attainment_reg_filter &
        time_period == max(time_period)) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN Status` = characteristic,
        `Percent meeting expected standard`
      ) %>%
      arrange(
        `Academic Year`,
        desc(`Percent meeting expected standard`)
      )

    return(DTise(
      ks2_attainment_reb,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("Percent meeting expected standard", 2))
  })

  ### KS1 Phonics ####

  ## KS1 Phonics (LA/time)
  output$ks1_phonics_la_time <- renderPlotly({
    # req(input$la_choice)

    ks1_phonics_la_time <- ks1_phonics %>%
      fsubset(la_name == input$la_choice) %>%
      AY_to_date(academic_year) %>%
      ggplot(aes(
        x = AY_date,
        y = `Percent meeting expected standards in Y1`,
        group = characteristic,
        colour = characteristic,
        text = paste(
          "Academic Year:", ay_date_to_ay(AY_date), "\n",
          "% meeting expected standards in Y1:", `Percent meeting expected standards in Y1`, "\n",
          "SEN status:", characteristic
        ),
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% meeting expected standards in Y1",
        x = "Academic year",
        colour = "Group"
      ) +
      scale_colour_manual(values = c(af_darkpink, af_darkblue, af_orange, af_turquoise)) +
      scale_x_date(
        breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")),
        labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
      ) +
      coord_cartesian(ylim = c(0, NA))


    ks1_phonics_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks1_phonics_la_time_table <- renderDT({
    ks1_phonics_la_tt <- ks1_phonics %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(academic_year, la_name, characteristic, `Percent meeting expected standards in Y1`) %>%
      arrange(academic_year, characteristic)

    names(ks1_phonics_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      ks1_phonics_la_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("Percent meeting expected standards in Y1", 2))
  })

  ## KS1 Phonics (LA/bench)
  output$ks1_phonics_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, ks1_phonics)
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks1_phonics %>%
      collapse::fsubset(characteristic == if_else(input$ks1_phonics_la_filter == "EHC plan or Statement", "EHC plan", input$ks1_phonics_la_filter) & # hack because the historic data includes statements but the current does not
        time_period == comparison_year &
        geographic_level == "National") %>%
      mutate(outcome = `Percent meeting expected standards in Y1`)

    # Wrangle data into ggplot object
    ks1_phonics_la_bench_data <- ks1_phonics %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$ks1_phonics_la_filter &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "Percent meeting expected standards in Y1")

    # Create rank statements to go on the X axis.
    ks1_phonics_la_bench_data$rank_statement <- rank_statement_fun(ks1_phonics_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )

    ks1_phonics_la_bench <- ks1_phonics_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `Percent meeting expected standards in Y1`),
        y = `Percent meeting expected standards in Y1`,
        text = paste(
          la_name, "\n",
          "% meeting expected standards in Y1:", `Percent meeting expected standards in Y1`, "\n",
          "SEN status:", characteristic
        ),
        fill = chosen_la
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = ks1_phonics_la_bench_data$rank_statement[ks1_phonics_la_bench_data$la_name == input$la_choice],
        y = "% meeting expected standards in Y1",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    ks1_phonics_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$ks1_phonics_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, ks1_phonics)

    ks1_phonics_lab <- ks1_phonics %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      })

    ks1_phonics_lab <- ks1_phonics_lab %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$ks1_phonics_la_filter &
        time_period == comparison_year) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local authority` = la_name,
        `SEN Status` = characteristic,
        `Percent meeting expected standards in Y1`
      ) %>%
      arrange(desc(`Percent meeting expected standards in Y1`))
    return(DTise(
      ks1_phonics_lab,
      list(list(3, "desc"))
    ) %>%
      formatRound("Percent meeting expected standards in Y1", 2))
  })

  ## KS1 Phonics (region/time)
  output$ks1_phonics_reg_time <- renderPlotly({
    # req(input$level_choice)
    # if(input$level_choice == "Regions") req(input$region_choice)


    if (input$level_choice == "Regions") {
      validate(need(input$region_choice,
        message = "Please select a region"
      ))

      ks1_phonics_reg_time <- ks1_phonics %>%
        filter(
          region_name == input$region_choice,
          geographic_level == "Regional"
        ) %>%
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Percent meeting expected standards in Y1`,
          group = characteristic,
          colour = characteristic,
          text = paste(
            "Academic Year:", ay_date_to_ay(AY_date), "\n",
            "% meeting expected standards in Y1:", `Percent meeting expected standards in Y1`, "\n",
            "SEN status:", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils meeting expected standards in Y1",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = c(af_darkpink, af_darkblue, af_orange, af_turquoise)) +
        scale_x_date(
          breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")),
          labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
        ) +
        coord_cartesian(ylim = c(0, NA))
    } else {
      ks1_phonics_reg_time <- ks1_phonics %>%
        filter(geographic_level == "National") %>%
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Percent meeting expected standards in Y1`,
          group = characteristic,
          colour = characteristic,
          text = paste(
            "Academic Year:", ay_date_to_ay(AY_date), "\n",
            "% meeting expected standards in Y1:", `Percent meeting expected standards in Y1`, "\n",
            "SEN status:", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils meeting expected standards in Y1",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = c(af_darkpink, af_darkblue, af_orange, af_turquoise)) +
        scale_x_date(
          breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")),
          labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
        ) +
        coord_cartesian(ylim = c(0, NA))
    }

    ks1_phonics_reg_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      save_plot_button_only()
  })

  output$ks1_phonics_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks1_phonics_ret <- ks1_phonics %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional") %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          `Percent meeting expected standards in Y1`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN Status`
        )

      return(DTise(
        ks1_phonics_ret,
        list(list(2, "asc"), list(0, "desc"))
      ) %>%
        formatRound("Percent meeting expected standards in Y1", 2))
    } else {
      ks1_phonics_et <- ks1_phonics %>%
        fsubset(geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN Status` = characteristic,
          `Percent meeting expected standards in Y1`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN Status`
        )
      return(DTise(
        ks1_phonics_et,
        list(list(2, "asc"), list(0, "desc"))
      ) %>%
        formatRound("Percent meeting expected standards in Y1", 2))
    }
  })

  ## KS1 Phonics (region/bench)
  output$ks1_phonics_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks1_phonics %>%
      collapse::fsubset(characteristic == if_else(input$ks1_phonics_reg_filter == "EHC plan or Statement", "EHC plan", input$ks1_phonics_reg_filter) & # hack because the historic data includes statements but the current does not
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = `Percent meeting expected standards in Y1`)

    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    ks1_phonics_reg_bench <- ks1_phonics %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks1_phonics_reg_filter &
        time_period == max(time_period)) %>%
      mutate(chosen_region = case_when(
        region_name == region_choice_validated &
          input$level_choice == "Regions" ~ region_choice_validated,
        input$level_choice == "England" ~ "English regions",
        TRUE ~ "Other region"
      )) %>%
      collapse::ftransform(
        region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
      ) %>%
      ggplot(aes(fct_reorder(region_name, `Percent meeting expected standards in Y1`),
        y = `Percent meeting expected standards in Y1`,
        text = paste(
          region_name, "\n",
          "% meeting expected standards in Y1:", `Percent meeting expected standards in Y1`, "\n",
          "SEN status:", characteristic
        ),
        fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average) +
      labs(
        x = "Regions in England",
        y = "% of pupils meeting expected standards",
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }


    ks1_phonics_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$ks1_phonics_reg_bench_table <- renderDT({
    ks1_phonics_reb <- ks1_phonics %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks1_phonics_reg_filter &
        time_period == max(time_period)) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN Status` = characteristic,
        `Percent meeting expected standards in Y1`
      ) %>%
      arrange(
        `Academic Year`,
        desc(`Percent meeting expected standards in Y1`)
      )

    return(DTise(
      ks1_phonics_reb,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("Percent meeting expected standards in Y1", 2))
  })


  ### Early Years Foundation Stage Profile

  ## EYFSP (LA/time) ##

  output$eyfsp_la_time <- renderText("Since academic year 2021-22 was the first year of the new early years foundation stage profile assessment, comparisons over time cannot be made. This graph will appear once there is more than one year of time-series data")

  output$eyfsp_la_time_table <- renderText("Since academic year 2021-22 was the first year of the new early years foundation stage profile assessment, comparisons over time cannot be made. This table will appear once there is more than one year of time-series data")
  ## EYFSP (LA/bench) ##

  output$eyfsp_la_bench <- renderPlotly({
    validate(need(input$la_choice != "Northamptonshire", message = "No data for years prior to 2021-22 (so see North or West Northamptonshire for relevant results)"))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eyfsp %>%
      collapse::fsubset(characteristic_type == input$eyfsp_la_filter &
        #   time_period == comparison_year &   don't need this check until there's more than one year of data
        geographic_level == "National") %>%
      mutate(outcome = gld_percentage)

    # Wrangle data into ggplot object
    eyfsp_la_bench_data <- eyfsp %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic_type == input$eyfsp_la_filter) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "gld_percentage")

    # Create rank statements to go on the X axis.
    eyfsp_la_bench_data$rank_statement <- rank_statement_fun(eyfsp_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )

    eyfsp_la_bench <- eyfsp_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, gld_percentage),
        y = gld_percentage,
        text = paste(la_name, "\nPercentage of children with a good level of development:", gld_percentage),
        fill = chosen_la
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input, nudge = 0.5) +
      labs(
        x = eyfsp_la_bench_data$rank_statement[eyfsp_la_bench_data$la_name == input$la_choice],
        y = "% children with a good level of development",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    eyfsp_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$eyfsp_la_bench_table <- renderDT({
    eyfsp_la_bench_data <- eyfsp %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic_type == input$eyfsp_la_filter) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      })

    eyfsp_la_bt <- eyfsp_la_bench_data %>%
      fselect(
        `Academic Year` = academic_year,
        `Local authority` = la_name,
        `SEN provision` = characteristic_type,
        `% of children with good development` = gld_percentage
      ) %>%
      arrange(desc(`% of children with good development`))

    return(DTise(
      eyfsp_la_bt,
      list(list(3, "desc"))
    ) %>%
      formatRound("% of children with good development", 1))
  })

  ## EYFSP (Region/time) ##

  output$eyfsp_reg_time <- renderText("Since academic year 2021-22 was the first year of the new Early Years Foundation Stage Profile Assessment, comparisons over time cannot be made. This graph will appear once there is more than one year of time-series data")

  ## EYFSP (Region/bench) ##

  output$eyfsp_reg_bench <- renderPlotly({
    national_average <- eng_eyfsp %>%
      collapse::fsubset(characteristic_type == input$eyfsp_reg_filter &
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = gld_percentage)

    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    eyfsp_reg_bench <- eyfsp %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic_type == input$eyfsp_reg_filter &
        time_period == max(time_period)) %>%
      mutate(chosen_region = case_when(
        region_name == region_choice_validated &
          input$level_choice == "Regions" ~ region_choice_validated,
        input$level_choice == "England" ~ "English regions",
        TRUE ~ "Other region"
      )) %>%
      collapse::ftransform(
        region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
      ) %>%
      ggplot(aes(fct_reorder(region_name, gld_percentage),
        y = gld_percentage,
        text = paste(region_name, "\nPercentage of children with a good level of development:", gld_percentage),
        fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average) +
      labs(
        x = "Regions in England",
        y = "%  children with a good level of development",
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }


    eyfsp_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$eyfsp_reg_bench_table <- renderDT({
    eyfsp_reb <- eyfsp %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic_type == input$eyfsp_reg_filter &
        time_period == max(time_period)) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN Status` = characteristic_type,
        `% children with a good level of development` = gld_percentage
      ) %>%
      arrange(
        `Academic Year`,
        desc(`% children with a good level of development`)
      )

    return(DTise(
      eyfsp_reb,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("% children with a good level of development", 1))
  })

  ### KS4 Attainment ####

  ## KS4 Attainment(LA/time)
  output$ks4_attainment_la_time <- renderText({
    "Progress 8 is not intended as a metric for comparison of different cohorts across time, so this graph is not displayed. Historical data is available in table form."
  })

  # renderPlotly({
  # req(input$la_choice)
  #
  # ks4_attainment_la_time <- ks4_attainment %>%
  #  drop_na(`Average progress 8 score`) %>%
  #  fsubset(la_name == input$la_choice) %>%
  #  AY_to_date(academic_year) %>%
  #  ggplot(aes(
  #    x = AY_date,
  #    y = `Average progress 8 score`,
  #    ymin = `Progress 8 score (lower confidence interval)`,
  #    ymax = `Progress 8 score (upper confidence interval)`,
  #    group = `SEN provision`,
  #    colour = `SEN provision`,
  #    fill = `SEN provision`
  #  )) +
  #  geom_ribbon(alpha = 0.2, outline.type = "full") +
  #  geom_point() +
  #  geom_line() +
  #  labs(
  #    y = "Average progress 8 score",
  #    x = "Academic year"
  #  ) +
  #  scale_colour_manual(values = af_palette) +
  #  scale_fill_manual(values = af_palette) +
  # coord_cartesian(ylim = c(0, NA)) +
  #  scale_x_date(
  #    breaks = c(seq(ymd(paste(substr(min(ks4_attainment$academic_year), 1, 4), "09", "01", sep = "-")), ymd(paste(substr(max(ks4_attainment$academic_year), 1, 4), "09", "01", sep = "-")), "years")),
  #    labels = rev(unique(ks4_attainment$academic_year))
  #  )
  #  if (nrow(ks4_attainment_la_time$data) > 0) {
  #     ks4_attainment_la_time %>%
  #        ggplotly(
  #         tooltip = c("text", "y", "x", "colour")
  #        ) %>%
  #       save_plot_button_only() %>%
  #       use_horizontal_legend()
  #  }
  # })

  output$ks4_attainment_la_time_table <- renderDT({
    ks4_attainment_la_tt <- ks4_attainment %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(academic_year, la_name, `SEN provision`, `Average progress 8 score`, `Progress 8 score (lower confidence interval)`, `Progress 8 score (upper confidence interval)`) %>%
      arrange(academic_year, `SEN provision`)

    names(ks4_attainment_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      ks4_attainment_la_tt,
      list(list(0, "desc"), list(2, "asc"))
    ) %>%
      formatRound(columns = 4:6, 2))
  })

  ## KS4 Attainment(LA/bench)
  output$ks4_attainment_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_attainment))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks4_attainment %>%
      collapse::fsubset(`SEN provision` == input$ks4_attainment_la_bench_filter &
        time_period == comparison_year &
        geographic_level == "National") %>%
      ungroup() %>%
      mutate(outcome = `Average progress 8 score`)

    ks4_attainment_la_bench_data <- ks4_attainment %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year &
        `SEN provision` == input$ks4_attainment_la_bench_filter) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LAs"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "Average progress 8 score")
    if (nrow(ks4_attainment_la_bench_data) > 0) {
      # Create rank statements to go on the X axis.
      ks4_attainment_la_bench_data$rank_statement <- rank_statement_fun(ks4_attainment_la_bench_data,
        rank_col = rank,
        name_col = la_name,
        time_period = academic_year
      )

      ks4_attainment_la_bench <- ks4_attainment_la_bench_data %>%
        ggplot(aes(fct_reorder(la_name, `Average progress 8 score`),
          y = `Average progress 8 score`,
          ymin = `Progress 8 score (lower confidence interval)`,
          ymax = `Progress 8 score (upper confidence interval)`,
          group = `SEN provision`,
          text = paste0(la_name),
          colour = chosen_la
        )) +
        geom_pointrange(shape = 0) +
        add_england_line_bench(national_average) +
        # This graph needs a custom label since it has negative values
        geom_text(
          data = national_average,
          colour = af_grey,
          size = 3.5,
          inherit.aes = FALSE,
          nudge_y = 0.2,
          aes(
            x = ifelse(test = input$myregion_switch == TRUE,
              yes = 3,
              no = 20
            ),
            y = outcome,
            label = "England average"
          )
        ) +
        labs(
          x = ks4_attainment_la_bench_data$rank_statement[ks4_attainment_la_bench_data$la_name == input$la_choice],
          y = "Average Progress 8 score",
          colour = "Local Authority"
        ) +
        theme(axis.text.x = element_blank()) +
        scale_colour_manual(values = c("Other LAs" = not_focused), na.value = focused)

      ks4_attainment_la_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        restyle_england_line() %>% # Make the England line label align correctly and thin the line
        save_plot_button_only() %>%
        use_horizontal_legend()
    }
  })

  output$ks4_attainment_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, ks4_attainment)

    ks4_attainment_lab <- ks4_attainment %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      })

    ks4_attainment_lab <- ks4_attainment_lab %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        `SEN provision` == input$ks4_attainment_la_bench_filter &
        time_period == comparison_year) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local authority` = la_name,
        `SEN provision`,
        `Average progress 8 score`,
        `Progress 8 score (lower confidence interval)`,
        `Progress 8 score (upper confidence interval)`
      ) %>%
      arrange(desc(`Average progress 8 score`))
    return(DTise(
      ks4_attainment_lab,
      list(list(3, "desc"))
    ) %>%
      formatRound(columns = 4:6, 2))
  })

  ## KS4 Attainment(region/time)
  output$ks4_attainment_reg_time <- renderText({
    "Progress 8 is not intended as a metric for comparison of different cohorts across time, so this graph is not displayed. Historical data is available in table form."
  })

  # This code is likely to be useful later if the KS4 metric is switched to being attainment 8, but change over time is not
  # a meaningful concept for Progress 8 which is what it currently is

  # renderPlotly({
  # req(input$level_choice)
  # if(input$level_choice == "Regions") req(input$region_choice)

  #  if (input$level_choice == "Regions") {
  #    validate(need(input$region_choice, "Please select a region"))

  #    ks4_attainment_reg_time <- ks4_attainment %>%
  #      filter(
  #        region_name == input$region_choice,
  #        geographic_level == "Regional"
  #      ) %>%
  #      AY_to_date(academic_year) %>%
  #      drop_na(`Average progress 8 score`) %>% # Allows a ribbon to be drawn between years
  #      ggplot(aes(
  #        x = AY_date,
  #        y = `Average progress 8 score`,
  #        ymin = `Progress 8 score (lower confidence interval)`,
  #        ymax = `Progress 8 score (upper confidence interval)`,
  #        group = `SEN provision`,
  #        colour = `SEN provision`,
  #        fill = `SEN provision`
  #      )) +
  #      geom_ribbon(alpha = 0.2, outline.type = "full") +
  #      geom_point() +
  #      geom_line() +
  #      labs(
  #        y = "Average progress 8 score",
  #        x = "Academic year"
  #      ) +
  #      scale_colour_manual(values = af_palette) +
  #      scale_fill_manual(values = af_palette) +
  #       coord_cartesian(ylim = c(0, NA)) +
  #      scale_x_date(
  #        breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")),
  #        labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
  #      )
  #  } else {
  #    ks4_attainment_reg_time <- ks4_attainment %>%
  #      filter(geographic_level == "National") %>%
  #      drop_na(`Average progress 8 score`) %>% # Allows a ribbon to be drawn between years
  #      AY_to_date(academic_year) %>%
  #      ggplot(aes(
  #        x = AY_date,
  #        y = `Average progress 8 score`,
  #        ymin = `Progress 8 score (lower confidence interval)`,
  #        ymax = `Progress 8 score (upper confidence interval)`,
  #        group = `SEN provision`,
  #        colour = `SEN provision`,
  #        fill = `SEN provision`
  #      )) +
  #      geom_ribbon(alpha = 0.2, outline.type = "full") +
  #      geom_point() +
  #      geom_line() +
  #      labs(
  #        y = "Average progress 8 score",
  #        x = "Academic year"
  #      ) +
  #      scale_colour_manual(values = af_palette) +
  #      scale_fill_manual(values = af_palette) +
  #      scale_x_date(
  #        breaks = c(seq(ymd(paste(substr(min(ks4_attainment$academic_year), 1, 4), "09", "01", sep = "-")), ymd(paste(substr(max(ks4_attainment$academic_year), 1, 4), "09", "01", sep = "-")), "years")),
  #        labels = rev(unique(ks4_attainment$academic_year))
  #      )
  #  }


  #  ks4_attainment_reg_time %>%
  #    ggplotly(
  #      tooltip = c("text", "y", "x", "colour")
  #    ) %>%
  #    save_plot_button_only() %>%
  #    layout(
  #      legend = list(orientation = "h", y = -0.2),
  #      dragmode = FALSE
  #    )
  # })

  output$ks4_attainment_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks4_att_ret <- ks4_attainment %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional") %>%
        fselect(
          `Academic Year` = academic_year,
          `Region` = region_name,
          `SEN provision`,
          `Average progress 8 score`,
          `Progress 8 score (lower confidence interval)`,
          `Progress 8 score (upper confidence interval)`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN provision`
        )

      return(DTise(
        ks4_att_ret,
        list(list(2, "asc"), list(0, "desc"))
      ) %>%
        formatRound(columns = 4:6, 2))
    } else {
      ks4_att_et <- ks4_attainment %>%
        fsubset(geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN provision`,
          `Average progress 8 score`,
          `Progress 8 score (lower confidence interval)`,
          `Progress 8 score (upper confidence interval)`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN provision`
        )
      return(DTise(
        ks4_att_et,
        list(list(2, "asc"), list(0, "desc"))
      ) %>%
        formatRound(columns = 4:6, 2))
    }
  })

  ## KS4 Attainment(region/bench)
  output$ks4_attainment_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks4_attainment %>%
      collapse::fsubset(`SEN provision` == input$ks4_attainment_reg_bench_filter &
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = `Average progress 8 score`)


    ks4_attainment_reg_bench_basic <- ks4_attainment %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == input$ks4_attainment_reg_bench_filter &
        time_period == max(time_period))

    if (input$level_choice == "Regions" & is.character(input$region_choice)) {
      ks4_attainment_reg_bench_data <- ks4_attainment_reg_bench_basic %>%
        collapse::ftransform(chosen_region = ifelse(test = region_name == input$region_choice,
          yes = input$region_choice,
          no = "Other regions"
        ))
    } else {
      ks4_attainment_reg_bench_data <- ks4_attainment_reg_bench_basic %>%
        collapse::ftransform(chosen_region = "English regions")
    }


    ks4_attainment_reg_bench <- ks4_attainment_reg_bench_data %>%
      collapse::ftransform(region_name = ifelse(test = region_name == "Yorkshire and The Humber",
        yes = "Yorkshire and \n The Humber",
        no = region_name
      )) %>% # linewrap region to prevent chart exceeding box
      ggplot(aes(fct_reorder(region_name, `Average progress 8 score`),
        y = `Average progress 8 score`,
        ymin = `Progress 8 score (lower confidence interval)`,
        ymax = `Progress 8 score (upper confidence interval)`,
        group = `SEN provision`,
        text = paste0(
          "Average progress 8 score: ", `Average progress 8 score`, "\n",
          region_name
        ),
        colour = chosen_region
      )) +
      geom_pointrange(shape = 0) +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 0.02) +
      labs(
        x = "Regions in England",
        y = "Average progress 8 score"
      ) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), legend.position = "none") +
      if (input$level_choice == "Regions") {
        scale_colour_manual(values = c("Other regions" = not_focused), na.value = focused)
      } else {
        scale_colour_manual(values = c("English regions" = focused), na.value = focused)
      }


    ks4_attainment_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      # This code makes the dotted line a bit thinner.
      style(
        line = list(
          width = 1,
          color = "rgba(0,0,0,1)",
          dash = "dash",
          showlegend = "false"
        ),
        traces = c(4)
      ) %>%
      layout(
        legend = list(orientation = "h", y = -0.35, title = list(text = "")),
        dragmode = FALSE
      )
  })

  output$ks4_attainment_reg_bench_table <- renderDT({
    ks4_att_reb <- ks4_attainment %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == input$ks4_attainment_reg_bench_filter &
        time_period == max(time_period)) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN provision`,
        `Average progress 8 score`,
        `Progress 8 score (lower confidence interval)`,
        `Progress 8 score (upper confidence interval)`
      ) %>%
      arrange(
        `Academic Year`,
        desc(`Average progress 8 score`)
      )

    return(DTise(
      ks4_att_reb,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound(columns = 4:6, 2))
  })

  ### KS4 destinations ####

  ## KS4 destinations (LA/time)
  output$ks4_destinations_la_time <- renderPlotly({
    # req(input$la_choice)

    ks4_destinations_la_time <- ks4_destinations %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$ks4_destinations_la_time_filter) %>%
      mutate(
        textcol = if_else(Destination %in% c("Not sustained", "Overall sustained", "Sixth form college"), "white", "black"),
        Destination = factor(Destination, levels = names(dest_palette))
      ) %>%
      {
        ggplot(., aes(
          x = academic_year,
          y = `% of pupils`,
          group = Destination,
          fill = Destination,
          label = ifelse(test = `% of pupils` > 2.5,
            yes = paste0(round(`% of pupils`, 0), "%"),
            no = NA
          )
        )) +
          geom_col(position = "stack") +
          geom_text(
            size = 3,
            position = position_stack(vjust = 0.5),
            colour = .$textcol
          ) +
          labs(
            x = "Academic year",
            fill = "Destination",
            label = "% of pupils"
          ) +
          scale_fill_manual(values = c(
            "#CC7A88", "#B33E52",
            "#C7B8E6", "#967ACC",
            "#E6D2B8", "#CCAA7A", "#B3823E", "#7a592a"
          ))
      }

    ks4_destinations_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "v"),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$ks4_destinations_la_time_table <- renderDT({
    ks4_destinations_la_tt <- ks4_destinations %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$ks4_destinations_la_time_filter) %>%
      arrange(
        Destination,
        academic_year
      ) %>%
      fselect(
        academic_year,
        la_name,
        characteristic,
        Destination,
        `% of pupils`
      )

    names(ks4_destinations_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      ks4_destinations_la_tt,
      list(list(3, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 1))
  })

  ## KS4 destinations (LA/type)
  output$ks4_destinations_la_type <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_destinations))
    # req(input$la_choice)
    if (nrow(fsubset(ks4_destinations, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      ks4_destinations_la_type <- ks4_destinations %>%
        fsubset(la_name == input$la_choice &
          Destination == input$ks4_destinations_la_type_filter) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic
        )) +
        geom_point() +
        geom_line() +
        labs(
          x = "Academic year",
          colour = "Group"
        ) +
        scale_y_continuous(limits = c(0, 102)) +
        scale_colour_manual(values = af_palette)

      ks4_destinations_la_type %>%
        ggplotly(
          tooltip = c("text", "y", "x", "colour")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$ks4_destinations_la_type_table <- renderDT({
    ks4_destinations_la_typet <- ks4_destinations %>%
      fsubset(la_name == input$la_choice &
        Destination == input$ks4_destinations_la_type_filter) %>%
      arrange(
        academic_year,
        characteristic
      ) %>%
      fselect(
        academic_year,
        la_name,
        characteristic,
        Destination,
        `% of pupils`
      )

    names(ks4_destinations_la_typet)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      ks4_destinations_la_typet,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 1))
  })

  ### 16-18 destinations ####

  ## 16-18 destinations (LA/time)
  output$destinations_1618_la_time <- renderPlotly({
    # req(input$la_choice)

    destinations_1618_la_time <- destinations_1618_overall %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$destinations_1618_la_time_filter &
        measure_filter == input$destinations_1618_la_time_filter_two) %>%
      mutate(
        textcol = if_else(Destination %in% c("Not sustained", "Overall sustained", "Sixth form college"), "white", "black"),
        Destination = factor(Destination, levels = names(dest_palette))
      )

    if (nrow(destinations_1618_la_time) < 5) { # there should be a minimum of five rows as that's the number of years
      no_dest_data(TRUE)
    } else {
      no_dest_data(FALSE)
    }

    destinations_1618_la_time <- destinations_1618_la_time %>%
      {
        ggplot(., aes(
          x = academic_year,
          y = `% of pupils`,
          group = Destination,
          fill = Destination,
          label = ifelse(test = `% of pupils` > 2.5,
            yes = paste0(round(`% of pupils`, 0), "%"),
            no = NA
          ),
          text = paste0(
            "Academic Year: ", academic_year, "\n",
            "% of students: ", `% of pupils`, "\n",
            "Destination: ", Destination
          )
        )) +
          geom_col(position = "stack") +
          geom_text(
            size = 3,
            position = position_stack(vjust = 0.5),
            colour = .$textcol
          ) +
          labs(
            x = "Academic year",
            fill = "Destination",
            y = paste("% of students at destination")
          ) +
          scale_fill_manual(values = dest_palette)
      }

    destinations_1618_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "v"),
        dragmode = FALSE
      )
  })


  output$destinations_1618_la_time_table <- renderDT({
    destinations_1618_la_tt <- destinations_1618_overall %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$destinations_1618_la_time_filter &
        measure_filter == input$destinations_1618_la_time_filter_two) %>%
      arrange(
        Destination,
        academic_year
      ) %>%
      fselect(
        academic_year,
        la_name,
        characteristic,
        Destination,
        "% of students" = `% of pupils`
      )

    names(destinations_1618_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      destinations_1618_la_tt,
      list(list(2, "asc"), list(3, "desc"), list(0, "desc"))
    ) %>%
      formatRound("% of students", 1))
  })

  ## 16-18 destinations (LA/type)
  output$destinations_1618_la_type <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(destinations_1618_overall))

    ymax <- max(destinations_1618_overall$`% of pupils`)

    if (nrow(fsubset(destinations_1618_overall, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      destinations_1618_la_type <- destinations_1618_overall %>%
        fsubset(la_name == input$la_choice &
          Destination == input$destinations_1618_la_type_filter)

      destinations_1618_la_type <- destinations_1618_la_type %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic Year: ", academic_year, "\n",
            "% of students: ", `% of pupils`, "\n",
            "SEN status: ", characteristic
          )
        )) +
        geom_point() +
        geom_line() +
        labs(
          x = "Academic year",
          colour = "Group"
        ) +
        scale_y_continuous(limits = c(0, ymax)) +
        scale_colour_manual(values = af_palette)
      # facet_wrap(~characteristic_group)

      destinations_1618_la_type %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$destinations_1618_la_type_table <- renderDT({
    destinations_1618_la_typet <- destinations_1618_overall %>%
      fsubset(la_name == input$la_choice &
        Destination == input$destinations_1618_la_type_filter) %>%
      arrange(
        academic_year,
        characteristic
      ) %>%
      fselect(
        academic_year,
        la_name,
        characteristic,
        Destination,
        `% of pupils`
      )

    names(destinations_1618_la_typet)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      destinations_1618_la_typet,
      list(list(2, "asc"), list(3, "desc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 1))
  })



  ## 16-18 destinations (LA/bench)
  output$destinations_1618_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(destinations_1618_overall))

    national_average <- destinations_1618_nat_overall %>%
      collapse::fsubset(time_period == max(time_period) &
        geographic_level == "National" &
        characteristic == input$destinations_1618_la_bench_filter &
        Destination == input$destinations_1618_la_bench_filter_two) %>%
      mutate(outcome = `% of pupils`)

    # Select data
    destinations_1618_la_bench_data <- destinations_1618_overall %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$destinations_1618_la_bench_filter &
        Destination == input$destinations_1618_la_bench_filter_two &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "% of pupils")

    # get ranks
    destinations_1618_la_bench_data$rank_statement <- rank_statement_fun(destinations_1618_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )

    la_choice_validated <- if (is.null(input$la_choice)) {
      "Other LA"
    } else {
      input$la_choice
    }

    # Create graph
    chosen_la <- destinations_1618_la_bench_data %>% filter(la_name == input$la_choice)
    if (nrow(chosen_la) > 0) {
      plot_la_bench(
        data = destinations_1618_la_bench_data,
        outcome_var = "% of pupils",
        label_y = "% of students",
        la_choice_validated = la_choice_validated,
        hover_label = "% of students",
        national_average = national_average
      )
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })



  output$destinations_1618_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(destinations_1618_overall))

    destinations_1618_la_bt <- destinations_1618_overall %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$destinations_1618_la_bench_filter &
        Destination == input$destinations_1618_la_bench_filter_two &
        time_period == comparison_year) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice]
      } else {
        region_name != "none"
      }) %>%
      arrange(
        Destination,
        `% of pupils`
      ) %>%
      fselect(
        academic_year,
        la_name,
        characteristic,
        Destination,
        `% of students` = `% of pupils`
      )

    names(destinations_1618_la_bt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      destinations_1618_la_bt,
      list(list(3, "asc"), list(4, "desc"))
    ) %>%
      formatRound("% of students", 1))
  })

  ## 16-18 destinations (region/time)
  output$destinations_1618_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, "Please select a region"))
      validate(need(input$destinations_1618_reg_time_filter != "Specialist provision", "Data on special schools only available at national level"))
      destinations_1618_reg_time <- destinations_1618_overall %>%
        filter(
          characteristic == input$destinations_1618_reg_time_filter,
          measure_filter == input$destinations_1618_reg_time_filter_two,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        mutate(
          textcol = if_else(Destination %in% c("Not sustained", "Overall sustained"), "white", "black"),
          Destination = factor(Destination, levels = names(dest_palette))
        ) %>%
        {
          ggplot(., aes(
            x = academic_year,
            y = `% of pupils`,
            group = Destination,
            fill = Destination,
            label = ifelse(test = `% of pupils` > 2.5,
              yes = paste0(round(`% of pupils`, 0), "%"),
              no = NA
            ),
            text = paste0(
              "Academic Year: ", academic_year, "\n",
              "% of students: ", `% of pupils`, "\n",
              "Destination: ", Destination
            )
          )) +
            geom_col(position = "stack") +
            geom_text(
              size = 3,
              position = position_stack(vjust = 0.5),
              colour = .$textcol
            ) +
            labs(
              x = "Academic year",
              fill = "Destination",
              label = "% of students"
            ) +
            scale_fill_manual(values = dest_palette)
        }

      destinations_1618_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    } else {
      destinations_1618_reg_time <- destinations_1618_nat_overall %>%
        filter(
          characteristic == input$destinations_1618_reg_time_filter,
          measure_filter == input$destinations_1618_reg_time_filter_two
        ) %>%
        mutate(
          textcol = if_else(Destination %in% c("Not sustained", "Overall sustained"), "white", "black"),
          Destination = factor(Destination, levels = names(dest_palette))
        ) %>%
        {
          ggplot(., aes(
            x = academic_year,
            y = `% of pupils`,
            group = Destination,
            fill = Destination,
            label = ifelse(test = `% of pupils` > 2.5,
              yes = paste0(round(`% of pupils`, 0), "%"),
              no = NA
            ),
            text = paste0(
              "Academic Year: ", academic_year, "\n",
              "% of pupils: ", `% of pupils`, "\n",
              "Destination: ", Destination
            )
          )) +
            geom_col(position = "stack") +
            geom_text(
              size = 3,
              position = position_stack(vjust = 0.5),
              colour = .$textcol
            ) +
            labs(
              x = "Academic year",
              fill = "Destination",
              label = "% of students"
            ) +
            scale_fill_manual(values = dest_palette)
        }

      destinations_1618_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    }
  })

  output$destinations_1618_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      validate(need(input$destinations_1618_reg_time_filter != "Specialist provision", "Data on special schools only available at national level"))
      destinations_1618_reg_tt <- destinations_1618_overall %>%
        filter(
          characteristic == input$destinations_1618_reg_time_filter,
          measure_filter == input$destinations_1618_reg_time_filter_two,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          `Region` = region_name,
          `SEN Status` = characteristic,
          Destination,
          `% of students` = `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )

      return(DTise(
        destinations_1618_reg_tt,
        list(list(2, "asc"), list(3, "desc"), list
        (0, "desc"))
      ) %>%
        formatRound("% of students", 1))
    } else {
      destinations_1618_reg_tt <- destinations_1618_nat_overall %>%
        fsubset(characteristic == input$destinations_1618_reg_time_filter &
          measure_filter == input$destinations_1618_reg_time_filter_two &
          geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN Status` = characteristic,
          Destination,
          `% of students` = `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )
      return(DTise(
        destinations_1618_reg_tt,
        list(list(2, "asc"), list(3, "desc"), list(0, "desc"))
      ) %>%
        formatRound("% of students", 1))
    }
  })

  ## 16-18 destinations (region/type)
  output$destinations_1618_reg_type <- renderPlotly({
    ymax <- max(destinations_1618_overall$`% of pupils`)

    if (input$level_choice == "Regions") {
      req(input$region_choice)
      destinations_1618_reg_type <- destinations_1618_overall %>%
        filter(
          Destination == input$destinations_1618_reg_type_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        )

      destinations_1618_reg_type <- destinations_1618_reg_type %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic Year: ", academic_year, "\n",
            "% of students: ", `% of pupils`, "\n",
            "Characterisitic: ", characteristic
          )
        )) +
        geom_point() +
        geom_line() +
        labs(
          x = "Academic year",
          colour = "Group"
        ) +
        scale_y_continuous(limits = c(0, ymax)) +
        scale_colour_manual(values = af_palette)

      destinations_1618_reg_type %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      destinations_1618_reg_type <- destinations_1618_nat %>%
        filter(Destination == input$destinations_1618_reg_type_filter)

      ymax <- max(destinations_1618_reg_type$`% of pupils`)

      destinations_1618_reg_type <- destinations_1618_reg_type %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic Year: ", academic_year, "\n",
            "% of pupils: ", `% of pupils`, "\n",
            "Characterisitic: ", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of students",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_y_continuous(limits = c(0, ymax))

      destinations_1618_reg_type %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$destinations_1618_reg_type_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      destinations_1618_reg_typet <- destinations_1618_overall %>%
        fsubset(region_name == input$region_choice &
          Destination == input$destinations_1618_reg_type_filter &
          geographic_level == "Regional") %>%
        arrange(
          academic_year,
          characteristic
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          Destination,
          `% of students` = `% of pupils`
        )
    } else {
      destinations_1618_reg_typet <- destinations_1618_nat %>%
        fsubset(Destination == input$destinations_1618_reg_type_filter &
          geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN Status` = characteristic,
          Destination,
          `% of students` = `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )
    }

    return(DTise(
      destinations_1618_reg_typet,
      list(list(2, "asc"), list(3, "desc"), list(0, "desc"))
    ) %>%
      formatRound("% of students", 1))
  })

  ## 16-18 destinations (region/bench)
  output$destinations_1618_reg_bench <- renderPlotly({
    national_average <- destinations_1618_nat_overall %>%
      collapse::fsubset(geographic_level == "National" &
        characteristic == input$destinations_1618_reg_bench_filter &
        Destination == input$destinations_1618_reg_bench_filter_two &
        time_period == max(time_period)) %>%
      mutate(outcome = `% of pupils`)

    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    destinations_1618_reg_bench_s1 <- destinations_1618_overall %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$destinations_1618_reg_bench_filter &
        Destination == input$destinations_1618_reg_bench_filter_two &
        time_period == max(time_period)) %>%
      mutate(Region = region_name)

    destinations_1618_reg_bench <- plot_reg_bench(
      data = destinations_1618_reg_bench_s1,
      outcome_var = destinations_1618_reg_bench_s1$`% of pupils`,
      label_y = paste0("% of students"),
      region_choice_validated = paste0(region_choice_validated),
      hover_label = "% of students",
      national_average = national_average
    )
  })


  output$destinations_1618_reg_bench_table <- renderDT({
    destinations_1618_reg_bt <- destinations_1618_overall %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$destinations_1618_reg_bench_filter &
        Destination == input$destinations_1618_reg_bench_filter_two &
        academic_year == max(academic_year)) %>%
      arrange(
        academic_year,
        Destination,
        `% of pupils`
      ) %>%
      fselect(academic_year,
        Region = region_name,
        `Characteristic` = characteristic,
        Destination,
        `% of students` = `% of pupils`
      )

    return(DTise(
      destinations_1618_reg_bt,
      list(list(2, "asc"), list(3, "desc"), list(0, "desc"))
    ) %>%
      formatRound("% of students", 1))
  })

  ### Ofsted outcomes ####

  output$la_ofsted_rating <- renderValueBox({
    # req(input$la_choice)
    if (input$la_choice != "") {
      ofsted_la <- ofsted %>%
        fsubset(la_name == input$la_choice) %>%
        fmutate(`Publication Date` = if_else(is.na(`Inspection publication date (new inspection framework)`),
          `Publication date (previous inspection framework)`,
          `Inspection publication date (new inspection framework)`
        ))

      if (nrow(ofsted_la > 0 & ofsted_la$box_colour != "black")) {
        valueBox(
          color = ofsted_la$box_colour,
          value = tags$p(ofsted_la$summary_outcome, style = "font-size: 50%"),
          subtitle = paste("Report published: ", ofsted_la$`Publication Date`, ".")
        )
      } else {
        valueBox(
          color = "black",
          value = "LA not yet inspected",
          subtitle = "Recently-created LAs may not yet have been inspected"
        )
      }
    } else {
      valueBox(
        color = "black",
        value = "Please select LA in top menu",
        subtitle = "No LA currently selected"
      )
    }
  })

  output$previous_ofsted <- renderValueBox({
    previous_la <- case_when(input$la_choice %in% c("North Northamptonshire", "West Northamptonshire") ~ "Northamptonshire",
      input$la_choice %in% c("Northumberland", "Westmoreland and Furness") ~ "Cumbria",
      .default = ""
    )

    ofsted_old_la <- ofsted %>%
      fsubset(la_name == previous_la)
    if (nrow(ofsted_old_la > 0)) {
      previous_ofsted <-
        valueBox(
          color = ofsted_old_la$box_colour,
          value = tags$p(ofsted_old_la$summary_outcome, style = "font-size: 50%"),
          subtitle = paste("Report published: ", ofsted_old_la$`Publication date (previous inspection framework)`, ".")
        )
    } else {
      previous_ofsted <-
        valueBox(
          color = "red",
          value = "Error - This should never happen"
        )
    }
  })

  output$reg_ofsted_rating <- renderDataTable({
    # req(input$level_choice)
    # req(input$ofsted_table_choice)

    if (input$level_choice == "Regions" & input$ofsted_table_choice == "Full table") {
      req(input$region_choice)
      ofsted %>%
        fsubset(region == input$region_choice) %>%
        fmutate(`Publication Date` = case_when(!(is.na(`Inspection publication date (new inspection framework)`)) ~ `Inspection publication date (new inspection framework)`,
          !(is.na(revisit_publish_date_old_framework)) ~ as.Date(revisit_publish_date_old_framework, format = "%d/%m/%Y"),
          .default = `Publication date (previous inspection framework)`
        )) %>%
        select(
          `LA name` = la_name,
          `Region` = region,
          `Publication Date`,
          `Inspection Outcome` = summary_outcome
        ) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 10, dom = "tip")
        ) %>%
        DT::formatStyle("Inspection Outcome",
          fontWeight = "bold",
          color = "black",
          backgroundColor = styleEqual(
            levels = unique(ofsted$summary_outcome),
            values = ofsted_palette
          )
        )
    } else if (input$level_choice == "England" & input$ofsted_table_choice == "Full table") {
      ofsted %>%
        fmutate(`Publication Date` = case_when(!(is.na(`Inspection publication date (new inspection framework)`)) ~ `Inspection publication date (new inspection framework)`,
          !(is.na(revisit_publish_date_old_framework)) ~ as.Date(revisit_publish_date_old_framework, format = "%d/%m/%Y"),
          .default = `Publication date (previous inspection framework)`
        )) %>%
        select(
          `LA name` = la_name,
          `Region` = region,
          `Publication Date`,
          `Inspection Outcome` = summary_outcome
        ) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(pageLength = 10, dom = "tip")
        ) %>%
        DT::formatStyle("Inspection Outcome",
          fontWeight = "bold",
          color = "#161616",
          backgroundColor = styleEqual(
            levels = unique(ofsted$summary_outcome),
            values = ofsted_palette
          )
        )
    } else if (input$level_choice == "Regions" & input$ofsted_table_choice == "Summary") {
      req(input$region_choice)
      ofsted %>%
        fsubset(region == input$region_choice &
          la_name != "Northamptonshire") %>%
        group_by(summary_outcome) %>%
        count(name = "Number of LAs") %>%
        rename(`Inspection Outcome` = summary_outcome) %>%
        DT::datatable(
          rownames = FALSE,
          options = list(
            order = list(1, "desc"),
            pageLength = 10,
            dom = "t"
          )
        ) %>%
        DT::formatStyle("Inspection Outcome",
          fontWeight = "bold",
          color = "#161616",
          backgroundColor = styleEqual(
            levels = unique(ofsted$summary_outcome),
            values = ofsted_palette
          )
        )
    } else if (input$level_choice == "England" & input$ofsted_table_choice == "Summary") {
      ofsted %>%
        fsubset(la_name != "Northamptonshire") %>%
        group_by(summary_outcome) %>%
        count(name = "Number of LAs") %>%
        rename(`Inspection Outcome` = summary_outcome) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            order = list(1, "desc"),
            pageLength = 10,
            dom = "tB",
            buttons = list("copy", list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            ))
          )
        ) %>%
        DT::formatStyle("Inspection Outcome",
          fontWeight = "bold",
          color = "#161616",
          backgroundColor = styleEqual(
            levels = unique(ofsted$summary_outcome),
            values = ofsted_palette
          )
        )
    }
  })

  #### Discontinued Plans ####

  ## Discontinued Plans (LA/time)
  output$discontinued_la_time <- renderPlotly({
    # req(input$la_choice)

    disco_la_time <- discontinued_plans %>%
      pivot_longer(c(discontinued_older, discontinued_schoolage), names_prefix = "discontinued_", names_to = "Age", values_to = "Number of ceased plans") %>%
      fsubset(la_name == input$la_choice) %>%
      mutate(Age = if_else(Age == "older", "Above compulsory schooling age", "Compulsory schooling age")) %>%
      ggplot(aes(
        x = time_period,
        y = `Number of ceased plans`,
        group = Age,
        colour = Age,
        fill = Age,
        text = paste0(
          "Year: ", time_period, "\n",
          "Plans discontinued: ", `Number of ceased plans`, "\n"
        ),
      )) +
      geom_point() +
      geom_line() +
      labs(
        y = "Plans discontinued as needs met",
        x = "Year"
      ) +
      scale_colour_manual(values = af_palette) +
      scale_fill_manual(values = af_palette) +
      coord_cartesian(ylim = c(0, NA))


    if (nrow(disco_la_time$data) > 0) {
      disco_la_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        use_horizontal_legend()
    }
  })

  output$discontinued_la_time_table <- renderDT({
    discontinued_la_tt <- discontinued_plans %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(time_period, la_name, discontinued_schoolage, discontinued_older) %>%
      arrange(time_period)

    names(discontinued_la_tt)[1:4] <- c("Year", "Local Authority", "Plans discontinued (compulsory school age)", "Plans discontinued (above compulsory school age)")
    return(DTise(
      discontinued_la_tt,
      list(list(0, "desc"))
    ))
  })

  ## Discontinued Plans (LA/bench)
  output$discontinued_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, discontinued_plans)

    # Wrangle data into ggplot object
    discontinued_la_bench_data <- discontinued_plans %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      ))

    if (input$discontinued_la_filter == "Compulsory school age") {
      discontinued_la_bench_data <- discontinued_la_bench_data %>%
        mutate(ageclass = discontinued_schoolage)
    } else {
      discontinued_la_bench_data <- discontinued_la_bench_data %>%
        mutate(ageclass = discontinued_older)
    }

    discontinued_la_bench_data <- discontinued_la_bench_data %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "ageclass")

    # Create rank statements to go on the X axis.
    discontinued_la_bench_data$rank_statement <- rank_statement_fun(discontinued_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = time_period
    )

    discontinued_la_bench <- discontinued_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, ageclass),
        y = ageclass,
        text = paste0(
          la_name, "\n",
          "Plans discontinued: ", ageclass
        ),
        fill = chosen_la
      )) +
      geom_col() +
      labs(
        x = discontinued_la_bench_data$rank_statement[discontinued_la_bench_data$la_name == input$la_choice],
        y = "Number of plans discontinued as \nneeds met without plan",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    discontinued_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$discontinued_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, discontinued_plans)

    if (input$myregion_switch == TRUE) {
      region <- discontinued_plans %>%
        filter(la_name == input$la_choice) %>%
        pull(region_name) %>%
        unique()
      discontinued_lab <- discontinued_plans %>%
        filter(region_name == region)
    } else {
      discontinued_lab <- discontinued_plans %>%
        filter(region_name != "none")
    }

    discontinued_lab <- discontinued_lab %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year) %>%
      fselect(
        `Year` = time_period,
        `Local authority` = la_name,
        `Discontinued plans (school age)` = discontinued_schoolage,
        `Discontinued plans (above school age)` = discontinued_older
      )

    if (input$discontinued_la_filter == "Compulsory school age") {
      discontinued_lab <- discontinued_lab %>%
        arrange(desc(`Discontinued plans (school age)`))
      DTise_num <- 2
    } else {
      discontinued_lab <- discontinued_lab %>%
        arrange(desc(`Discontinued plans (above school age)`))
      DTise_num <- 3
    }

    return(DTise(
      discontinued_lab,
      list(list(DTise_num, "desc"))
    ))
  })

  ## Discontinued Plans (Region/time)
  output$discontinued_reg_time <- renderPlotly({
    # req(input$la_choice)
    if (input$level_choice == "Regions") {
      disco_reg_time_data <- discontinued_plans %>%
        pivot_longer(c(discontinued_older, discontinued_schoolage), names_prefix = "discontinued_", names_to = "Age", values_to = "Number of ceased plans") %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional") %>%
        mutate(Age = if_else(Age == "older", "Above compulsory schooling age", "Compulsory schooling age"))
    } else {
      disco_reg_time_data <- discontinued_plans %>%
        pivot_longer(c(discontinued_older, discontinued_schoolage), names_prefix = "discontinued_", names_to = "Age", values_to = "Number of ceased plans") %>%
        fsubset(region_name == "England") %>%
        mutate(Age = if_else(Age == "older", "Above compulsory schooling age", "Compulsory schooling age"))
    }

    disco_reg_time <- disco_reg_time_data %>%
      ggplot(aes(
        x = time_period,
        y = `Number of ceased plans`,
        group = Age,
        colour = Age,
        fill = Age,
        text = paste0(
          "Year: ", time_period, "\n",
          "Plans discontinued: ", `Number of ceased plans`, "\n"
        ),
      )) +
      geom_point() +
      geom_line() +
      labs(
        y = "Plans ceased as needs met",
        x = "Year"
      ) +
      scale_colour_manual(values = af_palette) +
      scale_fill_manual(values = af_palette) +
      coord_cartesian(ylim = c(0, NA))


    if (nrow(disco_reg_time$data) > 0) {
      disco_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        use_horizontal_legend()
    }
  })

  output$discontinued_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      discontinued_reg_tt <- discontinued_plans %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional")
    } else {
      discontinued_reg_tt <- discontinued_plans %>%
        fsubset(region_name == "England")
    }
    discontinued_reg_tt <- discontinued_reg_tt %>%
      arrange(time_period) %>%
      ftransform(
        Year = as.character(round(time_period, 0)), # stop it being 2021.00 etc
        discontinued_schoolage = as.character(round(discontinued_schoolage, 0)),
        discontinued_older = as.character(round(discontinued_older, 0))
      ) %>%
      fselect(Year, region_name, discontinued_schoolage, discontinued_older)

    names(discontinued_reg_tt)[1:4] <- c("Year", "Region", "Plans discontinued (compulsory school-age)", "Plans discontinued (above compulsory school-age)")
    return(DTise(
      discontinued_reg_tt,
      list(list(0, "desc"))
    ))
  })


  ## Discontinued Plans (Region/bench)
  output$discontinued_reg_bench <- renderPlotly({
    if (input$level_choice == "Regions" & !is.null(input$region_choice)) {
      discontinued_reg_bench_basic <- discontinued_plans %>%
        collapse::fsubset(geographic_level == "Regional" &
          time_period == max(time_period))

      if (input$discontinued_reg_filter == "Compulsory school age") {
        discontinued_reg_bench_basic <- discontinued_reg_bench_basic %>%
          mutate(`Number of plans discontinued as needs met without plan` = discontinued_schoolage)
      } else {
        discontinued_reg_bench_basic <- discontinued_reg_bench_basic %>%
          mutate(`Number of plans discontinued as needs met without plan` = discontinued_older)
      }

      discontinued_reg_bench <- discontinued_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region =
            case_when(
              region_name == input$region_choice ~ input$region_choice,
              TRUE ~ "Other region"
            ),
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ggplot(aes(fct_reorder(region_name, `Number of plans discontinued as needs met without plan`),
          y = `Number of plans discontinued as needs met without plan`,
          text = paste0(
            region_name, "\n",
            "Discontinued plans: ", `Number of plans discontinued as needs met without plan`
          ),
          fill = chosen_region
        )) +
        geom_col() +
        # Would normally add an England average here, but it's not really a meaningful concept in this graph since regional populations are different
        labs(
          x = "Regions in England",
          y = "Number of plans discontinued as needs met without plan",
          fill = "Region"
        ) +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ), legend.position = "none") +
        scale_fill_manual(
          values = c("Other region" = not_focused), na.value = focused
        )


      discontinued_reg_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line() %>%
        use_horizontal_legend()
    } else {
      # Pull in England data to its own dataframe, for creating the England average dotted line.
      # national_average <- eng_ehcp_timeliness %>%
      #   collapse::fsubset(time_period == max(time_period)) %>%
      #   mutate(outcome = `% of EHCPs issued within 20 weeks`)

      discontinued_reg_bench_basic <- discontinued_plans %>%
        collapse::fsubset(geographic_level == "Regional" &
          time_period == max(time_period))
      if (input$discontinued_reg_filter == "Compulsory school age") {
        discontinued_reg_bench_basic <- discontinued_reg_bench_basic %>%
          mutate(`Number of plans discontinued as needs met without plan` = discontinued_schoolage)
      } else {
        discontinued_reg_bench_basic <- discontinued_reg_bench_basic %>%
          mutate(`Number of plans discontinued as needs met without plan` = discontinued_older)
      }

      discontinued_reg_bench <- discontinued_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region = "English regions",
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ggplot(aes(fct_reorder(region_name, `Number of plans discontinued as needs met without plan`),
          y = `Number of plans discontinued as needs met without plan`,
          text = paste0(
            region_name, "\n",
            "Discontinued plans: ", `Number of plans discontinued as needs met without plan`
          ),
          fill = chosen_region
        )) +
        geom_col() +
        # Add England average dotted line and label
        # add_england_line_bench(national_average) +
        # add_england_label_bench_reg(national_average, nudge = 3.5) +
        labs(
          x = "Regions in England",
          y = "Number of plans discontinued as\n needs met without plan",
          fill = "Region"
        ) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            vjust = 0.5,
            hjust = 1
          ),
          legend.position = "none"
        ) +
        scale_fill_manual(
          values = c("English regions" = focused), na.value = focused
        )

      discontinued_reg_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line()
    }
  })

  output$discontinued_reg_bench_table <- renderDT({
    discontinued_reg_bt <- discontinued_plans %>%
      collapse::fsubset(geographic_level == "Regional" &
        time_period == max(time_period)) %>%
      select(
        `Year` = time_period,
        Region = region_name,
        `Discontinued plans (school age)` = discontinued_schoolage,
        `Discontinued plans (above school age)` = discontinued_older
      )

    if (input$discontinued_reg_filter == "Compulsory school age") {
      arrlist <- list(list(0, "desc"), list(2, "desc"))
    } else {
      arrlist <- list(list(0, "desc"), list(3, "desc"))
    }

    discontinued_reg_bt <- discontinued_reg_bt %>%
      ftransform(
        Year = round(Year, 0), # stop it being 2021.00 etc
        `Discontinued plans (school age)` = round(`Discontinued plans (school age)`, 0),
        `Discontinued plans (above school age)` = round(`Discontinued plans (above school age)`, 0)
      )

    return(DTise(
      discontinued_reg_bt,
      arrlist
    ))
  })

  # EXPERIENCES GRAPHS ---------------------------------------------------------------------------

  ### EHCP Timeliness ####

  ## EHCP Timeliness (LA/time)
  output$timeliness_la_time <- renderPlotly({
    timeliness_la_time <- ehcp_timeliness %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = time_period,
        y = `% of EHCPs issued within 20 weeks`,
        text = paste0(
          "Year: ", time_period, "\n",
          "% of EHC plans issued within 20 weeks: ", round(`% of EHCPs issued within 20 weeks`, 1)
        ),
        group = la_name
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% of EHCPs issued within 20 weeks\n (excluding exceptions)",
        x = "Calendar year"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_discrete(limits = unique(ehcp_timeliness$time_period))

    timeliness_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$timeliness_la_time_table <- renderDT({
    timeliness_la_tt <- ehcp_timeliness %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(time_period) %>%
      ftransform(
        Year = as.character(round(time_period, 0)) # stop it being 2021.00 etc
      ) %>%
      fselect(Year,
        `Local Authority` = la_name,
        `% of EHCPs issued within 20 weeks`
      )

    return(DTise(
      timeliness_la_tt,
      list(list(0, "desc"))
    ) %>%
      formatRound(columns = c("% of EHCPs issued within 20 weeks"), digits = 1))
  })

  ## EHCP Timeliness (LA/bench)
  output$timeliness_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ehcp_timeliness))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ehcp_timeliness %>%
      collapse::fsubset(time_period == comparison_year &
        geographic_level == "National") %>%
      mutate(outcome = `% of EHCPs issued within 20 weeks`)


    timeliness_la_bench_data <- ehcp_timeliness %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      # Add ranks
      add_ranks(outcome = "% of EHCPs issued within 20 weeks")

    # Create rank statements to go on the X axis.
    timeliness_la_bench_data$rank_statement <- rank_statement_fun(timeliness_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = time_period
    )


    timeliness_la_bench <- timeliness_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `% of EHCPs issued within 20 weeks`),
        y = `% of EHCPs issued within 20 weeks`,
        text = paste0(
          la_name, "\n",
          "% of EHC plans issued within 20 weeks: ", round(`% of EHCPs issued within 20 weeks`, 1)
        ),
        fill = chosen_la,
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = timeliness_la_bench_data$rank_statement[timeliness_la_bench_data$la_name == input$la_choice],
        y = "% of EHCPs issued within 20 weeks\n (excluding exceptions)",
        fill = "Local Authority"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    timeliness_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$timeliness_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ehcp_timeliness))

    timeliness_la_bt <- ehcp_timeliness %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year) %>%
      arrange(desc(`% of EHCPs issued within 20 weeks`)) %>%
      ftransform(
        Year = as.character(round(time_period, 0)) # stop it being 2021.00 etc
      )

    if (input$myregion_switch == TRUE) {
      timeliness_la_bt <- timeliness_la_bt %>%
        fsubset(region_name == region_name[la_name == input$la_choice][1])
    }

    timeliness_la_bt <- timeliness_la_bt %>%
      fselect(Year,
        `Local Authority` = la_name,
        `% of EHCPs issued within 20 weeks`
      )

    return(DTise(
      timeliness_la_bt,
      list(list(2, "desc"))
    ) %>%
      formatRound(columns = c("% of EHCPs issued within 20 weeks"), digits = 1))
  })

  ## EHCP Timeliness (region/time)
  output$timeliness_reg_time <- renderPlotly({
    if (input$level_choice == "Regions" &
      is.character(input$region_choice)) {
      timeliness2 <- ehcp_timeliness %>%
        filter(
          region_name == input$region_choice,
          geographic_level == "Regional"
        )
    } else {
      timeliness2 <- ehcp_timeliness %>%
        filter(region_name == "England")
    }

    timeliness_reg_time <- timeliness2 %>%
      ggplot(aes(
        x = time_period,
        y = `% of EHCPs issued within 20 weeks`,
        group = region_name,
        text = paste0(
          "Year: ", time_period, "\n",
          "% of EHC plans issued within 20 weeks: ", round(`% of EHCPs issued within 20 weeks`, 1)
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Calendar year",
        y = "% of EHCPs issued within 20 weeks\n (excluding exceptions)"
      ) +
      scale_y_continuous(limits = c(0, 100))

    timeliness_reg_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$timeliness_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      timeliness_reg_tt <- ehcp_timeliness %>%
        fsubset(
          geographic_level == "Regional" &
            region_name == input$region_choice
        ) %>%
        arrange(time_period) %>%
        ftransform(
          Year = as.character(round(time_period, 0))
        ) %>%
        fselect(Year,
          Region = region_name,
          `% of EHCPs issued within 20 weeks`
        )
    } else {
      timeliness_reg_tt <- ehcp_timeliness %>%
        fsubset(geographic_level == "National") %>%
        arrange(time_period) %>%
        ftransform(
          Region = "England",
          Year = as.character(round(time_period, 0)) # stop it being 2021.00 etc
        ) %>%
        fselect(
          Year,
          Region,
          `% of EHCPs issued within 20 weeks`
        )
    }
    return(DTise(
      timeliness_reg_tt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("% of EHCPs issued within 20 weeks", 1))
  })

  ## EHCP Timeliness (region/bench)
  output$timeliness_reg_bench <- renderPlotly({
    if (input$level_choice == "Regions" & !is.null(input$region_choice)) {
      # Turned this bit off for testing only
      # Pull in England data to its own dataframe, for creating the England average dotted line.
      national_average <- eng_ehcp_timeliness %>%
        collapse::fsubset(time_period == max(time_period) &
          geographic_level == "National") %>%
        mutate(outcome = `% of EHCPs issued within 20 weeks`)

      timeliness_reg_bench_basic <- ehcp_timeliness %>%
        collapse::fsubset(geographic_level == "Regional" &
          time_period == max(time_period)) %>%
        rename(
          "Percent of EHCPs issued within 20 weeks" =
            "% of EHCPs issued within 20 weeks"
        )

      timeliness_reg_bench <- timeliness_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region =
            case_when(
              region_name == input$region_choice ~ input$region_choice,
              TRUE ~ "Other region"
            ),
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ggplot(aes(fct_reorder(region_name, `Percent of EHCPs issued within 20 weeks`),
          y = `Percent of EHCPs issued within 20 weeks`,
          text = paste0(
            region_name, "\n",
            "% of EHC plans issued within 20 weeks: ", round(`Percent of EHCPs issued within 20 weeks`, 1)
          ),
          fill = chosen_region
        )) +
        geom_col() +
        # Add England average dotted line and label
        add_england_line_bench(national_average) +
        add_england_label_bench_reg(national_average, nudge = 3.5) +
        labs(
          x = "Regions in England",
          y = "% of EHCPs issued within 20 weeks\n (excluding exceptions)",
          fill = "Region"
        ) +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ), legend.position = "none") +
        scale_fill_manual(
          values = c("Other region" = not_focused), na.value = focused
        )


      timeliness_reg_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line() %>%
        use_horizontal_legend()
    } else {
      # Pull in England data to its own dataframe, for creating the England average dotted line.
      # national_average <- eng_ehcp_timeliness %>%
      #   collapse::fsubset(time_period == max(time_period)) %>%
      #   mutate(outcome = `% of EHCPs issued within 20 weeks`)

      timeliness_reg_bench_basic <- ehcp_timeliness %>%
        collapse::fsubset(geographic_level == "Regional" &
          time_period == max(time_period)) %>%
        rename(
          "Percent of EHCPs issued within 20 weeks" =
            "% of EHCPs issued within 20 weeks"
        )

      timeliness_reg_bench <- timeliness_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region = "English regions",
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ggplot(aes(fct_reorder(region_name, `Percent of EHCPs issued within 20 weeks`),
          y = `Percent of EHCPs issued within 20 weeks`,
          text = paste0(
            region_name, "\n",
            "% of EHC plans issued within 20 weeks: ", round(`Percent of EHCPs issued within 20 weeks`, 1)
          ),
          fill = chosen_region
        )) +
        geom_col() +
        # Add England average dotted line and label
        # add_england_line_bench(national_average) +
        # add_england_label_bench_reg(national_average, nudge = 3.5) +
        labs(
          x = "Regions in England",
          y = "% of EHCPs issued within 20 weeks\n (excluding exceptions)",
          fill = "Region"
        ) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            vjust = 0.5,
            hjust = 1
          ),
          legend.position = "none"
        ) +
        scale_fill_manual(
          values = c("English regions" = focused), na.value = focused
        )

      timeliness_reg_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line()
    }
  })

  output$timeliness_reg_bench_table <- renderDT({
    timeliness_reg_bt <- ehcp_timeliness %>%
      collapse::fsubset(geographic_level == "Regional" &
        time_period == max(time_period)) %>%
      arrange(desc(`% of EHCPs issued within 20 weeks`)) %>%
      ftransform(
        Year = as.character(round(time_period, 0)) # stop it being 2021.00 etc
      ) %>%
      fselect(Year,
        Region = region_name,
        `% of EHCPs issued within 20 weeks`
      )

    return(DTise(
      timeliness_reg_bt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("% of EHCPs issued within 20 weeks", 1))
  })

  ### Autism Waiting Times ####
  ## Autism Waiting Times (CCG/time)
  output$autism_ccg_time <- renderPlotly({
    # Check for valid CCG choice
    validate(need(input$ccg_choice, message = "Select NHS area to display this graph."))

    # Check if there is a valid output to be presented for this CCG
    if (input$ccg_choice %in% autism$nhs_name) {
      autism_ccg_time <- autism %>%
        fsubset(nhs_name == input$ccg_choice &
          nhs_type == "Former CCG area") %>%
        ggplot(aes(
          x = date,
          y = `% with first appointment after more than 13 weeks`,
          colour = `Age group`,
          group = `Age group`,
          text = paste0(
            "Date: ", format(date, "%b %Y"), "\n",
            "% of patients seen after more than 13 weeks: ", `% with first appointment after more than 13 weeks`
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of patients seen after more than 13 weeks",
          x = "Reporting dates"
        ) +
        scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink)) +
        coord_cartesian(ylim = c(0, NA))

      autism_ccg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        )
    } else {
      # If there is no data for this CCG display this message
      autism_ccg_time <- ggplot(data = NULL, aes(x = 1, y = 1)) +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()
        ) +
        geom_text(aes(label = "No data for this CCG on this metric."))

      autism_ccg_time %>%
        ggplotly() %>%
        save_plot_button_only()
    }
  })

  output$autism_ccg_time_table <- renderDT({
    validate(need(input$ccg_choice, message = "Select NHS area to display this graph."))

    if (input$ccg_choice %in% autism$nhs_name) {
      autism_ccg_tt <- autism %>%
        fsubset(nhs_name == input$ccg_choice &
          nhs_type == "Former CCG area") %>%
        ftransform(
          Month = factor(format(date, "%b %Y"), levels = month_order), # stop renderDT linewrapping the date
          `Age group` = factor(`Age group`, levels = c("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24"))
        ) %>%
        arrange(
          `Age group`,
          date
        ) %>%
        fselect(`Month`,
          `Former CCG area` = nhs_name,
          `Age group`,
          `% with first appointment\n after more than 13 weeks` = `% with first appointment after more than 13 weeks`
        )
    }
    return(DTise(
      autism_ccg_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% with first appointment\n after more than 13 weeks", 0))
  })

  ## Autism Waiting Times (CCG/bench)
  output$autism_ccg_bench <- renderPlotly({
    if (is.character(input$ccg_choice)) {
      # Pull in England data to its own dataframe, for creating the England average dotted line.
      national_average <- eng_autism %>%
        collapse::fsubset(date == max(date)) %>%
        mutate(outcome = `% with first appointment after more than 13 weeks`) %>%
        mutate(year = lubridate::year(date))

      autism_ccg_bench_data <- autism %>%
        collapse::fsubset(date == max(date)) %>%
        collapse::ftransform(chosen_ccg = ifelse(nhs_name == input$ccg_choice,
          yes = input$ccg_choice,
          no = "Other CCG"
        )) %>%
        filter(if (input$myregion_switch == TRUE) {
          nhs_region == nhs_region[nhs_name == input$ccg_choice][1]
        } else {
          nhs_region != "none"
        }) %>%
        mutate(year = lubridate::year(date)) %>%
        add_ranks(outcome = "% with first appointment after more than 13 weeks")

      # Create rank statement: this is not currently used for this graph because plotly is not happy about the 3 different
      # titles for the X axis.
      autism_ccg_bench_data$rank_statement <- rank_statement_fun(autism_ccg_bench_data,
        rank_col = rank,
        name_col = nhs_name,
        time_period = year,
        geog = "CCGs"
      )

      autism_ccg_bench <- autism_ccg_bench_data %>%
        ggplot(aes(
          x = tidytext::reorder_within(
            x = nhs_name,
            by = `% with first appointment after more than 13 weeks`,
            within = `Age group`
          ),
          y = `% with first appointment after more than 13 weeks`,
          text = paste0(
            "Sub-ICB Region: ", nhs_name, "\n",
            "% with first appointment after more than 13 weeks: ", `% with first appointment after more than 13 weeks`
          ),
          fill = chosen_ccg
        )) +
        geom_col() +
        # Add England average dotted line and label
        add_england_line_bench(national_average) +
        geom_text(
          size = 2.5,
          inherit.aes = FALSE,
          data = national_average,
          aes(
            x = ifelse(test = input[["myregion_switch"]] == TRUE,
              yes = 3,
              no = 20
            ),
            y = outcome + 10,
            label = "England average"
          )
        ) +
        labs(
          x = "Clinical Commissioning Groups (CCGs) in England",
          y = "% with first appointment after more than 13 weeks",
          fill = "Clinical Commissioning Group"
        ) +
        scale_y_continuous(limits = c(0, 100)) +
        theme(axis.text.x = element_blank()) +
        tidytext::scale_x_reordered() +
        scale_fill_manual(values = c("Other CCG" = not_focused), na.value = focused) +
        facet_wrap(~`Age group`,
          scales = "free_x",
          nrow = 3
        ) +
        theme(panel.border = element_rect(fill = NA))

      autism_ccg_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        # Change England average label alignment
        style(
          textposition = "right",
          traces = c(12)
        ) %>%
        # This code makes the dotted line a bit thinner.
        style(
          line = list(
            width = 1,
            color = "rgba(0,0,0,1)",
            dash = "dash",
            showlegend = "false"
          ),
          traces = c(11)
        ) %>%
        use_horizontal_legend() %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      autism_ccg_bench <- ggplot(data = NULL, aes(x = 1, y = 1)) +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()
        ) +
        geom_text(aes(label = "Select NHS area in the top menu to display this graph."))
    }
  })

  output$autism_ccg_bench_table <- renderDT({
    autism_ccg_b <- autism %>%
      collapse::fsubset(date == max(date)) %>%
      ftransform(`Age group` = factor(`Age group`, levels = c("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24"))) %>%
      filter(if (input$myregion_switch == TRUE) {
        nhs_region == nhs_region[nhs_name == input$ccg_choice][1]
      } else {
        nhs_region != "none"
      }) %>%
      arrange(
        `Age group`,
        `% with first appointment after more than 13 weeks`
      ) %>%
      fmutate(
        Month = format(date, "%B %Y"), # use month name to stop renderDT linewrapping the date
        `Sub-ICB Area` = map(nhs_name, simpleCap)
      ) %>%
      fselect(
        Month,
        `Sub-ICB Area`,
        `Age group`,
        `% with first appointment\n after more than 13 weeks` = `% with first appointment after more than 13 weeks`
      )

    return(DTise(
      autism_ccg_b,
      list(list(2, "asc"), list(3, "desc"))
    ) %>%
      formatRound("% with first appointment\n after more than 13 weeks", 0))
  })

  ## Autism Waiting Times (National/time)
  # No regional breakdown for this data - only national and provider

  output$autism_nat_time <- renderPlotly({
    autism_nat_time <- autism %>%
      fsubset(BREAKDOWN == "Age Group") %>%
      ggplot(aes(
        x = date,
        y = `% with first appointment after more than 13 weeks`,
        colour = `Age group`,
        group = `Age group`,
        text = paste0(
          "Date: ", format(date, "%b %Y"), "\n",
          "% with first appointment after more than 13 weeks: ", `% with first appointment after more than 13 weeks`
        ),
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% seen after more than 13 weeks",
        x = "Reporting dates"
      ) +
      scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink)) +
      coord_cartesian(ylim = c(0, NA))

    autism_nat_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$autism_nat_time_table <- renderDT({
    autism_nat_tt <- autism %>%
      fsubset(BREAKDOWN == "Age Group") %>%
      ftransform(
        Month = factor(format(date, "%b %Y"), levels = month_order), # stop renderDT linewrapping the date
        `Age group` = factor(`Age group`, levels = c("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24")),
        Region = "England"
      ) %>%
      arrange(
        `Age group`,
        date
      ) %>%
      fselect(
        `Month`,
        Region,
        `Age group`,
        `% with first appointment\n after more than 13 weeks` = `% with first appointment after more than 13 weeks` # insert linebreak for the table
      )

    return(DTise(
      autism_nat_tt,
      list(list(2, "desc"), list(0, "desc"))
    ) %>%
      formatRound("% with first appointment\n after more than 13 weeks", 0))
  })

  ## Autism Waiting Times (provider/bench)
  output$autism_nat_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_autism %>%
      collapse::fsubset(date == max(date) &
        `Age group` == input$autism_nat_bench_filter) %>%
      mutate(outcome = `% with first appointment after more than 13 weeks`) %>%
      mutate(year = lubridate::year(date))

    autism_nat_bench_data <- autism %>%
      collapse::fsubset(date == max(date) &
        data_level == "Provider" &
        `Age group` == input$autism_nat_bench_filter) %>%
      fmutate(nhs_name = as.factor(nhs_name))

    autism_nat_bench <- autism_nat_bench_data %>%
      ggplot(aes(
        y = fct_reorder(
          nhs_name,
          -`% with first appointment after more than 13 weeks`
        ),
        x = `% with first appointment after more than 13 weeks`,
        text = paste0(
          "Provider: ", nhs_name, "\n",
          "% with first appointment after more than 13 weeks: ", `% with first appointment after more than 13 weeks`
        )
      )) +
      geom_col(fill = af_purple) +
      geom_vline(
        linetype = "dashed",
        colour = af_grey,
        size = 0.5,
        data = national_average,
        aes(xintercept = outcome)
      ) +
      geom_text(
        data = national_average,
        colour = af_grey,
        size = 3.5,
        nudge_x = 15,
        inherit.aes = FALSE,
        aes(
          y = nrow(autism_nat_bench_data),
          x = abs(outcome * 1.05),
          label = "England average"
        )
      ) +
      labs(
        y = "NHS provider organisations in England",
        x = "% seen after more than 13 weeks"
      ) +
      scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, .1))) +
      theme(panel.border = element_rect(fill = NA))


    autism_nat_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$autism_nat_bench_table <- renderDT({
    autism_nat_b <- autism %>%
      collapse::fsubset(date == max(date) &
        nhs_type == "Provider" &
        `Age group` == input$autism_nat_bench_filter) %>%
      arrange(`% with first appointment after more than 13 weeks`) %>%
      fmutate(
        Month = factor(format(date, "%b %Y"), levels = month_order) # use month name to stop renderDT linewrapping the date
      ) %>%
      fselect(Month,
        Provider = nhs_name,
        `Age group`,
        `% with first appointment\n after more than 13 weeks` = `% with first appointment after more than 13 weeks`
      )

    return(DTise(
      autism_nat_b,
      list(list(0, "desc"), list(2, "desc"), list(3, "desc"))
    ) %>%
      formatRound("% with first appointment\n after more than 13 weeks", 0))
  })

  ### Community Health waiting times ###

  ## Community Health waiting times (LA/time)

  # Not currently possible, because sub-ICB region data non-existent. May do something at ICB level if time allows

  ## Community Health waiting times (Provider/bench)

  output$ch_prov_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_communityhealth %>%
      collapse::fsubset(formattedDate == max(formattedDate) &
        `Therapy` == input$ch_prov_type_filter) %>%
      mutate(outcome = `Percentage waiting more than 18 weeks`) %>%
      mutate(year = lubridate::year(formattedDate))

    ch_prov_data <- la_communityhealth %>%
      collapse::fsubset(formattedDate == max(formattedDate) &
        `Organisation Type` %in% c("Organisation", "Provider") &
        Therapy == input$ch_prov_type_filter) %>%
      drop_na(`Percentage waiting more than 18 weeks`)

    ch_prov_bench <- ch_prov_data %>%
      ggplot(aes(
        y = fct_reorder(
          `Organisation Name`,
          -`Percentage waiting more than 18 weeks`
        ),
        x = `Percentage waiting more than 18 weeks`,
        text = paste0(
          `Organisation Name`, "\n",
          "Percentage waiting more than 18 weeks: ", round(`Percentage waiting more than 18 weeks`, 1)
        )
      )) +
      geom_col(fill = af_purple) +
      geom_vline(
        linetype = "dashed",
        colour = af_grey,
        size = 0.5,
        data = national_average,
        aes(xintercept = outcome)
      ) +
      geom_text(
        data = national_average,
        colour = af_grey,
        size = 3.5,
        nudge_x = 15,
        inherit.aes = FALSE,
        aes(
          y = nrow(ch_prov_data) - 1,
          x = abs(outcome * 1.05),
          label = "England average"
        )
      ) +
      labs(
        y = paste("NHS provider organisations in England \nwhich provided data in", unique(ch_prov_data$Date)),
        x = "% waiting more than 18 weeks"
      ) +
      scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, .1))) +
      theme(panel.border = element_rect(fill = NA))


    ch_prov_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$ch_prov_bench_table <- renderDT({
    ch_prov_b <- la_communityhealth %>%
      collapse::fsubset(formattedDate == max(formattedDate) &
        `Organisation Type` %in% c("Organisation", "Provider") &
        Therapy == input$ch_prov_type_filter) %>%
      drop_na(`Percentage waiting more than 18 weeks`) %>%
      arrange(desc(`Percentage waiting more than 18 weeks`)) %>%
      fmutate(
        Provider = map(`Organisation Name`, simpleCap)
      ) %>%
      fselect(
        Date,
        Provider,
        Therapy,
        `Percentage waiting more than 18 weeks`
      )

    return(DTise(
      ch_prov_b,
      list(list(3, "desc"))
    ) %>%
      formatRound("Percentage waiting more than 18 weeks", 1))
  })

  ## Community Health Waiting Times (Region/time)

  output$ch_nat_time <- renderPlotly({
    # NHS stat, so the regions are different
    nhs_region_from_region <- case_when(input$region_choice %in% c("Yorkshire and The Humber", "North East") ~ "North East And Yorkshire",
      input$region_choice %in% c("East Midlands", "West Midlands") ~ "Midlands",
      input$region_choice == "East of England" ~ "East Of England",
      .default = input$region_choice
    )

    if (input$level_choice == "Regions" &
      is.character(input$region_choice)) {
      ch_data <- la_communityhealth %>%
        filter(
          toupper(`Organisation Name`) == toupper(nhs_region_from_region), # capitalisation prophylaxis
          `Organisation Type` == "Region"
        )
    } else {
      ch_data <- eng_communityhealth
    }

    ch_nat_time <- ch_data %>%
      ggplot(aes(
        x = formattedDate,
        y = `Percentage waiting more than 18 weeks`,
        colour = Therapy,
        group = Therapy,
        text = paste0(
          "Date: ", Date, "\n",
          Therapy, "\n",
          "% waiting more than 18 weeks: ", round(`Percentage waiting more than 18 weeks`, 1)
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Month",
        colour = "Service"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_date(date_labels = "%b %Y") +
      scale_colour_manual(values = nhs_palette)

    ch_nat_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$ch_nat_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      nhs_region_from_region <- case_when(input$region_choice %in% c("Yorkshire and The Humber", "North East") ~ "North East And Yorkshire",
        input$region_choice %in% c("East Midlands", "West Midlands") ~ "Midlands",
        input$region_choice == "East of England" ~ "East Of England",
        .default = input$region_choice
      )

      ch_nat_tt <- la_communityhealth %>%
        filter(
          toupper(`Organisation Name`) == toupper(nhs_region_from_region), # capitalisation prophylaxis
          `Organisation Type` == "Region"
        )
    } else {
      ch_nat_tt <- eng_communityhealth %>%
        mutate(`Organisation Name` = "England")
    }

    ch_nat_tt <- ch_nat_tt %>%
      arrange(Therapy, formattedDate) %>%
      ftransform(
        `Percentage waiting more than 18 weeks` = round(`Percentage waiting more than 18 weeks`, 1),
        Date = factor(format(formattedDate, "%b %Y"), levels = month_order),
        Region = simpleCap(`Organisation Name`)
      ) %>%
      fselect(Date,
        Region,
        Service = Therapy,
        `Percentage waiting more than 18 weeks`
      )

    return(DTise(
      ch_nat_tt,
      list(list(2, "desc"), list(0, "desc"))
    ) %>%
      formatRound("Percentage waiting more than 18 weeks", 1))
  })

  ## Community Health Waiting Times (Region/bench)

  output$ch_nat_bench <- renderPlotly({
    nhs_region_from_region <- NA_character_
    if (input$level_choice == "Regions" & !is.null(input$region_choice)) {
      nhs_region_from_region <- case_when(input$region_choice %in% c("Yorkshire and The Humber", "North East") ~ "North East And Yorkshire",
        input$region_choice %in% c("East Midlands", "West Midlands") ~ "Midlands",
        input$region_choice == "East of England" ~ "East Of England",
        .default = input$region_choice
      )
    }
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_communityhealth %>%
      collapse::fsubset(formattedDate == max(formattedDate) &
        Therapy == input$ch_nat_service_filter) %>%
      mutate(outcome = round(`Percentage waiting more than 18 weeks`, 1))

    ch_nat_bench_basic <- la_communityhealth %>%
      collapse::fsubset(`Organisation Type` == "Region" &
        formattedDate == max(formattedDate) &
        Therapy == input$ch_nat_service_filter)

    ch_nat_bench_basic$`Organisation Name` <- unlist(map(ch_nat_bench_basic$`Organisation Name`, simpleCap))

    ch_nat_bench <- ch_nat_bench_basic %>%
      collapse::ftransform(
        chosen_region =
          case_when(
            is.na(nhs_region_from_region) ~ "NHS regions", # if it wasn't set that means we're in England mode
            `Organisation Name` == nhs_region_from_region ~ nhs_region_from_region,
            .default = "Other region"
          ),
        `Organisation Name` = ifelse(`Organisation Name` == "North East And Yorkshire", "North East\nAnd Yorkshire", `Organisation Name`)
      ) %>%
      ggplot(aes(fct_reorder(`Organisation Name`, `Percentage waiting more than 18 weeks`),
        y = `Percentage waiting more than 18 weeks`,
        text = paste0(
          `Organisation Name`, "\n",
          "Percentage waiting more than 18 weeks: ", round(`Percentage waiting more than 18 weeks`, 1)
        ),
        fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 3.5) +
      labs(
        x = "Regions in England",
        y = "Percentage waiting more than 18 weeks\nfor selected service",
        fill = "Region"
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = 1
      ), legend.position = "none") +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other region" = not_focused), na.value = focused
        )
      } else {
        scale_fill_manual(
          values = c("NHS Regions" = focused), na.value = focused
        )
      }


    ch_nat_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$ch_nat_bench_table <- renderDT({
    ch_nat_bench_basic <- la_communityhealth %>%
      collapse::fsubset(`Organisation Type` == "Region" &
        formattedDate == max(formattedDate) &
        Therapy == input$ch_nat_service_filter)
    ch_nat_bench_basic$`Organisation Name` <- unlist(map(ch_nat_bench_basic$`Organisation Name`, simpleCap))

    ch_nat_bt <- ch_nat_bench_basic %>%
      arrange(desc(`Percentage waiting more than 18 weeks`)) %>%
      fselect(Date,
        Region = `Organisation Name`,
        Service = Therapy,
        `Percentage waiting more than 18 weeks`
      )

    return(DTise(
      ch_nat_bt,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("Percentage waiting more than 18 weeks", 1))
  })

  ### Mental Health Service access ####
  ## Mental Health Services access (CCG/time)
  output$mentalhealth_ccg_time <- renderPlotly({
    validate(need(input$ccg_choice, message = "Select NHS area to display this graph"))
    # Check if there is a valid output to be presented for this CCG
    if (is.numeric(mentalhealth$`Number of children and young people`[mentalhealth$nhs_name == input$ccg_choice & mentalhealth$`Year ending` == max(mentalhealth$`Year ending`)])) {
      mentalhealth_ccg_time <- mentalhealth %>%
        fsubset((nhs_name == input$ccg_choice &
          nhs_type == "Former CCG area") |
          PRIMARY_LEVEL == "England") %>%
        ggplot(aes(
          x = `Year ending`,
          y = `Number of children and young people`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Rolling year ending") +
        theme(axis.title.y = element_text(margin = margin(r = 5))) +
        facet_wrap(~nhs_name, scales = "free_y", nrow = 2) +
        scale_x_date(date_breaks = "4 months", date_labels = "%b %Y", date_minor_breaks = "1 month")
    } else {
      # If there is no data for this CCG display this message
      mentalhealth_ccg_time <- ggplot(data = NULL, aes(x = 1, y = 1)) +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()
        ) +
        geom_text(aes(label = "No data for this CCG on this metric."))
    }


    mentalhealth_ccg_time %>%
      ggplotly(
        tooltip = c("text", "y", "x")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$mentalhealth_ccg_time_table <- renderDT({
    validate(need(input$ccg_choice, message = "Select NHS area to display this table"))

    if (is.numeric(mentalhealth$`Number of children and young people`[mentalhealth$nhs_name == input$ccg_choice & mentalhealth$`Year ending` == max(mentalhealth$`Year ending`)])) {
      mentalhealth_ccg_tt <- mentalhealth %>%
        fsubset((nhs_name == input$ccg_choice &
          nhs_type == "Former CCG area") |
          PRIMARY_LEVEL == "England") %>%
        arrange(
          desc(nhs_name),
          `Year ending`
        ) %>%
        ftransform(
          `Year ending` = factor(format(`Year ending`, "%b %Y", levels = month_order)),
          `Number of children and young people` = round(`Number of children and young people`, 0),
          order = as.numeric(`Year ending`) # as.numeric on a factor gives you the index of the level, so we can use this to order
        ) %>%
        fselect(`Year ending`,
          `NHS Region` = nhs_name,
          `Number of children and young people`,
          order
        )
    }
    return(DTise(
      mentalhealth_ccg_tt,
      list(list(3, "desc")),
      hidden = 3
    ) %>% # sort on the order column but hide it
      formatRound("Number of children and young people", 0))
  })

  ## Mental Health Access (CCG/bench)
  output$mentalhealth_ccg_bench <- renderPlotly({
    mentalhealth_ccg_bench <- mentalhealth %>%
      fsubset(BREAKDOWN2 == "CCG/Sub-ICB of Residence") %>%
      collapse::ftransform(chosen_ccg = ifelse(nhs_name == input$ccg_choice,
        yes = input$ccg_choice,
        no = "Other CCG"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        nhs_region == nhs_region[nhs_name == input$ccg_choice][1]
      } else {
        nhs_region != "none"
      }) %>%
      ggplot(aes(
        x = `Year ending`,
        y = `Number of children and young people`,
        group = nhs_name,
        text = nhs_name,
        colour = chosen_ccg
      )) +
      geom_line(aes(alpha = chosen_ccg)) +
      labs(
        x = ifelse(input$myregion_switch == TRUE,
          yes = paste0(
            "Clinical Commissioning Groups in ",
            mentalhealth$nhs_region[mentalhealth$nhs_name == input$ccg_choice]
          ),
          no = paste0("All Clinical Commissioning Groups in England")
        ),
        y = "Number of children and young people",
        colour = "Clinical Commissioning Group",
        alpha = "Clinical Commissioning Group"
      ) +
      scale_colour_manual(
        values = c("Other CCG" = af_purple, chosen_ccg = af_grey),
        na.value = focused
      ) +
      coord_cartesian(ylim = c(0, NA)) +
      scale_alpha_manual(values = c("Other CCG" = 0.2))


    mentalhealth_ccg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$mentalhealth_ccg_bench_table <- renderDT({
    mentalhealth_ccg_bt <- mentalhealth %>%
      fsubset(BREAKDOWN2 == "CCG/Sub-ICB of Residence" &
        `Year ending` == max(`Year ending`)) %>%
      filter(if (input$myregion_switch == TRUE) {
        nhs_region == nhs_region[nhs_name == input$ccg_choice][1]
      } else {
        nhs_region != "none"
      }) %>%
      arrange(desc(`Number of children and young people`)) %>%
      fmutate(
        `Year ending` = format(`Year ending`, "%B %Y"),
        `Number of children and young people` = round(`Number of children and young people`, 0)
      ) %>% # use month name to stop renderDT linewrapping the date
      fselect(`Year ending`,
        `Former CCG area` = nhs_name,
        `Number of children and young people`
      )

    return(DTise(
      mentalhealth_ccg_bt,
      list(list(2, "desc"))
    ) %>%
      formatRound("Number of children and young people", 0))
  })

  ## Mental Health Access (region/time)
  output$mentalhealth_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      # req(input$nhs_region_choice)
      mentalhealth_reg_time_data <- mentalhealth %>%
        fsubset(BREAKDOWN2 %in% c("Commissioning Region") &
          `Year ending` > (ymd("2021-07-01"))) %>% # boundary changes make regions data before this point not comparable
        fsubset(nhs_name == input$nhs_region_choice)

      mentalhealth_reg_time <- mentalhealth_reg_time_data %>%
        ggplot(aes(
          x = `Year ending`,
          y = `Number of children and young people`,
          group = nhs_name,
          text = paste0(
            nhs_name, "\n",
            "Year ending: ", `Year ending`, "\n",
            "Number of children and young people: ", `Number of children and young people`
          )
        )) +
        geom_line() +
        labs(
          x = ifelse(input$myregion_switch == TRUE,
            yes = paste0(
              "Integrated Care Boards in ",
              mentalhealth$nhs_region[mentalhealth$nhs_name == input$ccg_choice]
            ),
            no = paste0("All Integrated Care Boards in England")
          ),
          y = "Number of children and young people",
          colour = "NHS Region",
          alpha = "NHS Region"
        ) +
        ylim(0, max(mentalhealth_reg_time_data$`Number of children and young people`)) + # y-axis should definitely start at zero for this graph
        if (input$level_choice == "Regions") {
          scale_colour_manual(
            values = c("Other NHS region" = af_purple), na.value = focused
          )
        } else {
          scale_colour_manual(values = c("English NHS regions" = af_purple), na.value = focused)
        }

      mentalhealth_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      mentalhealth_reg_eng_data <- mentalhealth %>%
        fsubset(BREAKDOWN2 == "England")

      mentalhealth_reg_eng <- mentalhealth_reg_eng_data %>%
        ggplot(aes(
          x = `Year ending`,
          y = `Number of children and young people`,
          group = nhs_name,
          text = paste0(
            nhs_name, "\n",
            "Year ending: ", `Year ending`, "\n",
            "Number of children and young people: ", `Number of children and young people`
          )
        )) +
        geom_line() +
        labs(
          x = "Start of month",
          y = "Number of children and young people"
        ) +
        ylim(0, max(mentalhealth_reg_eng_data$`Number of children and young people`))

      mentalhealth_reg_eng %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$mentalhealth_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      mentalhealth_reg_tt <- mentalhealth %>%
        fsubset(BREAKDOWN2 %in% c("Commissioning Region") &
          `Year ending` > (ymd("2021-07-01"))) %>% # boundary changes make regions data before this point not comparable
        fsubset(nhs_name == input$nhs_region_choice)
    } else {
      mentalhealth_reg_tt <- mentalhealth %>%
        fsubset(BREAKDOWN2 == "England")
    }

    mentalhealth_reg_tt <- mentalhealth_reg_tt %>%
      ftransform(
        `Year ending` = factor(format(`Year ending`, "%b %Y", levels = month_order)),
        `Number of children and young people` = round(`Number of children and young people`, 0),
        order = as.numeric(`Year ending`) # as.numeric on a factor gives you the index of the level, so we can use this to order
      ) %>%
      fselect(`Year ending`,
        `NHS Region` = nhs_name,
        `Number of children and young people`,
        order
      )

    return(DTise(
      mentalhealth_reg_tt,
      list(list(3, "desc")),
      hidden = 3
    ) %>% # sort on the order column but hide it
      formatRound("Number of children and young people", 0))
  })

  ## Mental Health Access (region/bench)

  output$mentalhealth_reg_bench <- renderPlotly({
    # req(input$nhs_region_choice)
    mentalhealth_reg_bench_data <- mentalhealth %>%
      fsubset(BREAKDOWN2 %in% c("Commissioning Region") &
        `Year ending` > (ymd("2021-07-01"))) %>%
      fsubset(`Year ending` == max(`Year ending`)) %>% # we have to do this in a second subset call because the regions data doesn't have the most recent month in it
      fmutate(chosen_region = if (input$level_choice == "England") {
        "English NHS regions"
      } else {
        if_else(nhs_name == input$nhs_region_choice,
          "Chosen region",
          "Other NHS region"
        )
      }) %>%
      fmutate(nhs_name = if_else(nhs_name == "North East And Yorkshire", "North East\nand Yorkshire", nhs_name))

    mentalhealth_reg_bench <- mentalhealth_reg_bench_data %>%
      ggplot(aes(
        x = fct_reorder(nhs_name, `Number of children and young people`),
        y = `Number of children and young people`,
        fill = chosen_region,
        text = paste0(
          nhs_name, "\n",
          "Number of children and young people: ", `Number of children and young people`
        )
      )) +
      geom_col() +
      labs(
        x = "NHS Regions",
        y = "Number of children and young people",
        colour = "NHS Region"
      ) +
      ylim(0, max(mentalhealth_reg_bench_data$`Number of children and young people`)) + # y-axis should definitely start at zero for this graph
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other NHS region" = not_focused), na.value = focused
        )
      } else {
        scale_fill_manual(values = c("English NHS regions" = focused), na.value = focused)
      }

    mentalhealth_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$mentalhealth_reg_bench_table <- renderDT({
    mentalhealth_reg_bt <- mentalhealth %>%
      fsubset(BREAKDOWN2 %in% c("Commissioning Region") &
        `Year ending` > (ymd("2021-07-01"))) %>%
      fsubset(`Year ending` == max(`Year ending`)) %>% # we have to do this in a second subset call because the regions data doesn't have the most recent month in it
      arrange(desc(`Number of children and young people`)) %>%
      ftransform(
        `Year ending` = format(`Year ending`, "%B %Y"),
        `Number of children and young people` = round(`Number of children and young people`, 0)
      ) %>% # use month name to stop renderDT linewrapping the date
      fselect(`Year ending`,
        `NHS Region` = nhs_name,
        `Number of children and young people`
      )

    return(DTise(
      mentalhealth_reg_bt,
      list(list(2, "desc"))
    ) %>%
      formatRound("Number of children and young people", 0))
  })

  output$nhs_value_box_ccg_newest <- renderValueBox({
    req(input$ccg_choice) # OK to use this for non-Plotly objects

    most_recent_mentalhealth_value <- mentalhealth_ccg$`Number of children and young people`[mentalhealth_ccg$`Year ending` == max(mentalhealth_ccg$`Year ending`) &

      mentalhealth_ccg$nhs_name == input$ccg_choice]

    valueBox(
      color = "blue",
      value = scales::comma(most_recent_mentalhealth_value),
      subtitle = paste(
        "Children and young people in contact with NHS-funded mental health services in the 12 months ending",
        most_recent_mentalhealth_label
      )
    )
  })

  output$nhs_value_box_ccg_older <- renderValueBox({
    req(input$ccg_choice)

    year_ago_mentalhealth_value <- mentalhealth_ccg$`Number of children and young people`[mentalhealth_ccg$`Year ending` ==

      (max(mentalhealth_ccg$`Year ending` - lubridate::years(1))) &
      mentalhealth_ccg$nhs_name == input$ccg_choice]

    valueBox(
      color = "purple",
      value = scales::comma(year_ago_mentalhealth_value),
      subtitle = paste(
        "Children and young people in contact with NHS-funded mental health services in the 12 months ending",
        year_ago_mentalhealth_label
      )
    )
  })

  ### Tribunal Appeals ####

  ## Tribunal Appeals (LA/time)
  output$tribunals_la_time <- renderPlotly({
    tribunals_la_time <- tribunals %>%
      fsubset(la_name == input$la_choice) %>%
      mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
      ggplot(aes(
        x = year,
        y = `SEND Tribunal Appeal Rate`,
        group = la_name,
        text = paste0(
          "Year: ", year, "\n",
          "Appeal Rate: ", `Appeal Rate`
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Calendar year",
        y = "SEND Tribunal appeal rate (%)"
      ) +
      scale_y_continuous(limits = c(0, 10))

    tribunals_la_time %>%
      ggplotly(
        tooltip = c(
          "text"
        )
      ) %>%
      # config(displayModeBar = FALSE) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$tribunals_la_time_table <- renderDT({
    tribunals_la_tt <- tribunals %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(year) %>%
      fselect(
        Year = year,
        `Local Authority` = la_name,
        `Appeal Rate (%)` = `SEND Tribunal Appeal Rate`
      )

    return(DTise(
      tribunals_la_tt,
      list(list(0, "desc"))
    ) %>%
      formatRound("Appeal Rate (%)", 2))
  })

  ## Tribunal Appeals (LA/bench)
  output$tribunals_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(tribunals), column = "year")
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_tribunals %>%
      collapse::fsubset(year == comparison_year &
        region_name == "England") %>%
      mutate(outcome = `SEND Tribunal Appeal Rate`) %>%
      mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%"))

    tribunals_la_bench_data <- tribunals %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        year == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "SEND Tribunal Appeal Rate", reverse = TRUE)

    # Create rank statements to go on the X axis.
    tribunals_la_bench_data$rank_statement <- rank_statement_fun(tribunals_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = year
    )

    tribunals_la_bench <- tribunals_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `SEND Tribunal Appeal Rate`),
        y = `SEND Tribunal Appeal Rate`,
        text = paste0(
          la_name, "\n",
          "SEND Tribunal Appeal Rate (%): ", round(`SEND Tribunal Appeal Rate`, 2)
        ),
        fill = chosen_la,
        label = `Appeal Rate`
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = tribunals_la_bench_data$rank_statement[tribunals_la_bench_data$la_name == input$la_choice],
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = "#BFBFBF"), na.value = "#12436D")


    tribunals_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      restyle_england_line() %>%
      save_plot_button_only() %>%
      use_horizontal_legend() %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$tribunals_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(tribunals), column = "year")

    tribunals_la_bt <- tribunals %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        year == comparison_year) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      arrange(`SEND Tribunal Appeal Rate`) %>%
      fselect(
        Year = year,
        `Local Authority` = la_name,
        `Appeal Rate (%)` = `SEND Tribunal Appeal Rate`
      )

    return(DTise(
      tribunals_la_bt,
      list(list(2, "desc"))
    ) %>%
      formatRound("Appeal Rate (%)", 2))
  })

  ## Tribunal Appeals (region/time)
  output$tribunals_reg_time <- renderPlotly({
    # req(input$level_choice)
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      tribunals_reg_time <- tribunals_reg %>%
        filter(region_name == case_when(
          input$region_choice %in% c("Inner London", "Outer London")
          ~ "London",
          !(input$region_choice %in% c("Inner London", "Outer London"))
          ~ input$region_choice,
          TRUE ~ "England"
        )) %>%
        mutate(`Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%")) %>%
        ggplot(aes(
          x = year,
          y = `SEND Tribunal Appeal Rate`,
          group = region_name,
          text = paste0(
            "Year: ", year, "\n",
            "SEND Tribunal Appeal Rate: ", `Appeal Rate`
          )
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Calendar year") +
        scale_y_continuous(limits = c(0, 10))

      tribunals_reg_time %>%
        ggplotly(tooltip = c("text")) %>%
        layout(yaxis = list(autorange = FALSE)) %>%
        save_plot_button_only() %>%
        use_horizontal_legend()
    } else {
      tribunals_reg_time <- tribunals %>%
        fsubset(geographic_level == "National") %>%
        mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
        ggplot(aes(
          x = year,
          y = `SEND Tribunal Appeal Rate`,
          group = geographic_level,
          text = paste0(
            "Year: ", year, "\n",
            "SEND Tribunal Appeal Rate: ", `Appeal Rate`
          )
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Calendar year") +
        scale_y_continuous(limits = c(0, 10))

      tribunals_reg_time %>%
        ggplotly(tooltip = c("text")) %>%
        layout(yaxis = list(autorange = FALSE)) %>%
        save_plot_button_only() %>%
        use_horizontal_legend()
    }
  })

  output$tribunals_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      tribunals_reg_tt <- tribunals_reg %>%
        filter(region_name == input$region_choice) %>%
        arrange(year) %>%
        fselect(
          Year = year,
          Region = region_name,
          `Appeal Rate (%)` = `SEND Tribunal Appeal Rate`
        )
    } else {
      tribunals_reg_tt <- tribunals_reg %>%
        fsubset(region_name == "England") %>%
        arrange(year) %>%
        fselect(
          Year = year,
          Region = region_name,
          `Appeal Rate (%)` = `SEND Tribunal Appeal Rate`
        )
    }
    return(DTise(
      tribunals_reg_tt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("Appeal Rate (%)", 2))
  })

  ## Tribunal Appeals (region/bench)
  # Regional data has its own object, tribunals_reg. National/LA are in tribunals
  output$tribunals_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_tribunals %>%
      collapse::fsubset(year == max(year) &
        region_name == "England") %>%
      ungroup() %>%
      mutate(outcome = `SEND Tribunal Appeal Rate`) %>%
      mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%"))


    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      tribunals_reg_bench <- tribunals_reg %>%
        collapse::fsubset(year == max(year)) %>%
        collapse::ftransform(chosen_region = ifelse(region_name == input$region_choice,
          yes = input$region_choice,
          no = "Other region"
        )) %>%
        collapse::ftransform(
          chosen_region = case_when(
            region_name == input$region_choice &
              input$level_choice == "Regions" ~ input$region_choice,
            input$level_choice == "England" ~ "English regions",
            TRUE ~ "Other region"
          ),
          `Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%"),
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ggplot(aes(fct_reorder(region_name, `SEND Tribunal Appeal Rate`),
          y = `SEND Tribunal Appeal Rate`,
          text = paste0(
            region_name, "\n",
            "SEND Tribunal Appeal Rate: ", `Appeal Rate`
          ),
          fill = chosen_region,
          label = `Appeal Rate`
        )) +
        geom_col() +
        # Add England average dotted line and label
        add_england_line_bench(national_average) +
        add_england_label_bench_reg(national_average, nudge = 0.15) +
        labs(
          x = "Regions in England",
          y = "SEND Tribunal Appeal Rate (%)",
          fill = "Region"
        ) +
        theme(
          axis.text.x = element_text(
            angle = 45,
            vjust = 0.5,
            hjust = 1
          ),
          legend.position = "none"
        ) +
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
    } else {
      validate(need(input$region_choice, message = "Please select a region"))
      tribunals_reg_bench <- tribunals_reg %>%
        collapse::fsubset(year == max(year)) %>%
        ungroup() %>%
        mutate(`Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%")) %>%
        ggplot(aes(
          x = fct_reorder(region_name, `SEND Tribunal Appeal Rate`),
          y = `SEND Tribunal Appeal Rate`,
          text = paste0(
            region_name, "\n",
            "SEND Tribunal Appeal Rate (%): ", round(`SEND Tribunal Appeal Rate`, 2)
          ),
          label = `Appeal Rate`
        )) +
        geom_col(fill = focused) +
        # Add England average dotted line and label
        add_england_line_bench(national_average) +
        add_england_label_bench_reg(national_average, nudge = 0.15) +
        labs(
          x = "Regions in England",
          y = "SEND Tribunal Appeal Rate (%)",
          fill = "English regions"
        ) +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ))
    }

    tribunals_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      restyle_england_line() %>%
      save_plot_button_only() %>%
      use_horizontal_legend()
  })

  output$tribunals_reg_bench_table <- renderDT({
    tribunals_reg_bt <- tribunals_reg %>%
      collapse::fsubset(year == max(year)) %>%
      ungroup() %>%
      arrange(`SEND Tribunal Appeal Rate`) %>%
      fselect(
        Year = year,
        Region = region_name,
        `Appeal Rate (%)` = `SEND Tribunal Appeal Rate`
      )

    return(DTise(
      tribunals_reg_bt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("Appeal Rate (%)", 2))
  })

  ### Absence ####
  ## SEN Absence (LA/time)
  output$absence_la_time <- renderPlotly({
    ylabel <- paste0("% of sessions missed due to absence (Autumn/Spring terms)")
    absence_la_time <- absence %>%
      fsubset(la_name == input$la_choice &
        `Absence measure` == input$absence_la_auth_filter) %>%
      ggplot(aes(
        x = academic_year,
        y = Percentage,
        group = characteristic,
        colour = characteristic,
        text = paste0(
          "Academic year: ", academic_year, "\n",
          "% sessions missed: ", round(Percentage, 1), "\n",
          "SEN status: ", characteristic
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = paste0(ylabel),
        x = "Academic year",
        colour = "Group"
      ) +
      scale_colour_manual(values = c(af_darkpink, af_darkblue, af_turquoise, af_orange)) +
      scale_y_continuous(limits = c(0, max(absence$Percentage)))


    absence_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE,
        yaxis = list(autorange = FALSE)
      )
  })

  output$absence_la_time_table <- renderDT({
    absence_la_tt <- absence %>%
      fsubset(la_name == input$la_choice &
        `Absence measure` == input$absence_la_auth_filter) %>%
      arrange(
        characteristic,
        `Absence measure`,
        desc(academic_year)
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `Absence measure`,
        `SEN Status` = characteristic,
        Percentage = round(Percentage, 1)
      )

    return(DTise(
      absence_la_tt,
      list(list(3, "desc"), list(0, "desc"))
    ) %>%
      formatRound("Percentage", 1))
  })

  ## SEN absence (LA/bench)
  output$absence_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(absence))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_absence %>%
      collapse::fsubset(characteristic == input$absence_la_sen_filter &
        `Absence measure` == input$absence_la_bench_auth_filter &
        time_period == comparison_year &
        region_name == "England") %>%
      ungroup() %>%
      mutate(outcome = Percentage)


    absence_la_bench_data <- absence %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$absence_la_sen_filter &
        `Absence measure` == input$absence_la_bench_auth_filter &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "Percentage", reverse = T)

    # Create rank statements to go on the X axis.
    absence_la_bench_data$rank_statement <- rank_statement_fun(absence_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )
    # Create plot
    ylabel <- paste0("% of sessions missed due to absence (Autumn/Spring terms)")

    absence_la_bench <- absence_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, Percentage),
        y = Percentage,
        text = paste0(
          la_name, "\n",
          "% sessions missed: ", round(Percentage, 1)
        ),
        fill = chosen_la
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = absence_la_bench_data$rank_statement[absence_la_bench_data$la_name == input$la_choice],
        y = paste0(ylabel),
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

    # Feed into plotly
    absence_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$absence_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(absence))

    absence_la_bt <- absence %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$absence_la_sen_filter &
        `Absence measure` == input$absence_la_bench_auth_filter &
        time_period == comparison_year) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      arrange(desc(Percentage)) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN Status` = characteristic,
        `Absence measure`,
        Percentage = round(Percentage, 1)
      )

    return(DTise(
      absence_la_bt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("Percentage", 1))
  })

  ## SEN absence (region/time)
  output$absence_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      absence_reg_time <- absence_regional %>%
        filter(
          region_name == input$region_choice,
          `Absence measure` == input$absence_reg_auth_filter
        ) %>%
        ggplot(aes(
          x = academic_year,
          y = Percentage,
          group = characteristic,
          colour = characteristic,
          label = paste0(
            "Academic year: ", academic_year, "\n",
            "% sessions missed: ", round(Percentage, 1), "\n",
            "SEN status: ", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of sessions missed due to absence \n(Autumn/Spring terms)",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = c(
          af_darkpink, af_darkblue, af_turquoise, af_orange
        )) +
        coord_cartesian(ylim = c(0, NA))
    } else {
      absence_reg_time <- absence_regional %>%
        filter(
          region_name == "England",
          `Absence measure` == input$absence_reg_auth_filter
        ) %>%
        ggplot(aes(
          x = academic_year,
          y = Percentage,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic year: ", academic_year, "\n",
            "% sessions missed: ", round(Percentage, 1), "\n",
            "SEN status: ", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of sessions missed due to absence \n(Autumn/Spring terms)",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = c(
          af_darkpink, af_darkblue, af_turquoise, af_orange
        )) +
        coord_cartesian(ylim = c(0, NA))
    }

    absence_reg_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$absence_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      absence_reg_tt <- absence_regional %>%
        filter(
          region_name == input$region_choice,
          `Absence measure` == input$absence_reg_auth_filter
        ) %>%
        arrange(
          characteristic,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `Absence measure`,
          `SEN Status` = characteristic,
          Percentage = round(Percentage, 1)
        )
    } else {
      absence_reg_tt <- absence_regional %>%
        fsubset(region_name == "England" &
          `Absence measure` == input$absence_reg_auth_filter) %>%
        arrange(
          characteristic,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `Absence measure`,
          `SEN Status` = characteristic,
          Percentage = round(Percentage, 1)
        )
    }
    return(DTise(
      absence_reg_tt,
      list(list(3, "desc"), list(0, "desc"))
    ) %>%
      formatRound("Percentage", 1))
  })


  ## SEN absence (region/bench)
  output$absence_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_absence %>%
      collapse::fsubset(characteristic == input$absence_reg_sen_filter &
        `Absence measure` == input$absence_reg_auth_bench_filter &
        time_period == max(time_period) &
        region_name == "England") %>%
      ungroup() %>%
      mutate(outcome = Percentage)


    absence_reg_bench_basic <- absence_regional %>%
      collapse::fsubset(
        characteristic == input$absence_reg_sen_filter &
          `Absence measure` == input$absence_reg_auth_bench_filter &
          time_period == max(time_period)
      ) %>%
      ungroup() # Needed or next line will fail

    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      absence_reg_bench_data <- absence_reg_bench_basic %>%
        collapse::ftransform(chosen_region = case_when(
          region_name == input$region_choice ~ input$region_choice,
          TRUE ~ "Other region"
        ))
    } else {
      absence_reg_bench_data <- absence_reg_bench_basic %>%
        ftransform(chosen_region = "English regions")
    }

    absence_reg_bench <- absence_reg_bench_data %>%
      collapse::ftransform(region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)) %>%
      ggplot(aes(fct_reorder(region_name, Percentage),
        y = Percentage,
        text = paste0(
          region_name, "\n",
          "% sessions missed: ", round(Percentage, 1), "\n"
        ),
        fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average) +
      labs(
        x = "Regions in England",
        y = "% of sessions missed due to absence (Autumn/Spring terms)",
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other region" = not_focused), na.value = focused
        )
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }

    # Feed into plotly
    absence_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$absence_reg_bench_table <- renderDT({
    absence_reg_bt <- absence_regional %>%
      collapse::fsubset(characteristic == input$absence_reg_sen_filter &
        `Absence measure` == input$absence_reg_auth_bench_filter &
        time_period == max(time_period)) %>%
      ungroup() %>%
      arrange(
        characteristic,
        `Absence measure`,
        desc(Percentage)
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN Status` = characteristic,
        `Absence measure`,
        Percentage = round(Percentage, 1)
      )

    return(DTise(
      absence_reg_bt,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("Percentage", 1))
  })



  ### KS4 destinations ####

  ## KS4 destinations (LA/time)
  output$ks4_destinations_la_time <- renderPlotly({
    ks4_destinations_la_time <- ks4_destinations_overall %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$ks4_destinations_la_time_filter &
        measure_filter == input$ks4_destinations_la_time_filter_two) %>%
      mutate(
        textcol = if_else(Destination %in% c("Not sustained", "Overall sustained", "Sixth form college"), "white", "black"),
        Destination = factor(Destination, levels = names(dest_palette))
      ) %>%
      {
        ggplot(., aes(
          x = academic_year,
          y = `% of pupils`,
          group = Destination,
          fill = Destination,
          text = paste0(
            la_name, "\n",
            "% of pupils: ", `% of pupils`, "\n",
            "Academic year: ", academic_year
          ),
          label = ifelse(test = `% of pupils` > 2.5,
            yes = paste0(round(`% of pupils`, 0), "%"),
            no = NA
          )
        )) +
          geom_col(position = "stack") +
          geom_text(
            size = 3,
            position = position_stack(vjust = 0.5),
            colour = .$textcol
          ) +
          labs(
            x = "Academic year",
            fill = "Destination",
            label = "% of pupils"
          ) +
          scale_fill_manual(values = dest_palette)
      }

    ks4_destinations_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "v"),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$ks4_destinations_la_time_table <- renderDT({
    ks4_destinations_la_tt <- ks4_destinations_overall %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$ks4_destinations_la_time_filter &
        measure_filter == input$ks4_destinations_la_time_filter_two) %>%
      arrange(
        Destination,
        academic_year
      ) %>%
      fselect(
        `Aademic year` = academic_year,
        `Local authority` = la_name,
        `SEN status` = characteristic,
        Destination,
        `% of pupils`
      )

    names(ks4_destinations_la_tt)[1:3] <- c("Academic Year", "Local Authority", "Characteristic")
    return(DTise(
      ks4_destinations_la_tt,
      list(list(3, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 0))
  })


  ## KS4 destinations (region/time)
  output$ks4_destinations_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks4_destinations_reg_time <- ks4_destinations_overall %>%
        filter(
          characteristic == input$ks4_destinations_reg_time_filter,
          measure_filter == input$ks4_destinations_reg_time_filter_two,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        mutate(
          textcol = if_else(Destination %in% c("Not sustained", "Overall sustained", "Sixth form college"), "white", "black"),
          Destination = factor(Destination, levels = names(dest_palette))
        ) %>%
        {
          ggplot(., aes(
            x = academic_year,
            y = `% of pupils`,
            group = Destination,
            fill = Destination,
            text = paste0(
              "Academic year: ", academic_year, "\n",
              "% of pupils: ", `% of pupils`, "\n",
              "Destination: ", Destination
            ),
            label = ifelse(test = `% of pupils` > 2.5,
              yes = paste0(round(`% of pupils`, 0), "%"),
              no = NA
            )
          )) +
            geom_col(position = "stack") +
            geom_text(
              size = 3,
              position = position_stack(vjust = 0.5),
              color = .$textcol
            ) +
            labs(
              x = "Academic year",
              fill = "Destination",
              label = "% of pupils"
            ) +
            scale_fill_manual(values = dest_palette)
        }

      ks4_destinations_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    } else {
      ks4_destinations_reg_time <- ks4_destinations_nat_overall %>%
        filter(characteristic == input$ks4_destinations_reg_time_filter &
          measure_filter == input$ks4_destinations_reg_time_filter_two) %>%
        mutate(
          textcol = if_else(Destination %in% c("Not sustained", "Overall sustained", "Sixth form college"), "white", "black"),
          Destination = factor(Destination, levels = names(dest_palette))
        ) %>%
        {
          ggplot(., aes(
            x = academic_year,
            y = `% of pupils`,
            group = Destination,
            fill = Destination,
            text = paste0(
              "Academic year: ", academic_year, "\n",
              "% of pupils: ", `% of pupils`, "\n",
              "Destination: ", Destination
            ),
            label = ifelse(test = `% of pupils` > 2.5,
              yes = paste0(round(`% of pupils`, 0), "%"),
              no = NA
            )
          )) +
            geom_col(position = "stack") +
            geom_text(
              size = 3,
              position = position_stack(vjust = 0.5),
              colour = .$textcol
            ) +
            labs(
              x = "Academic year",
              fill = "Destination",
              label = "% of pupils"
            ) +
            scale_fill_manual(values = dest_palette)
        }

      ks4_destinations_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    }
  })

  output$ks4_destinations_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks4_destinations_reg_tt <- ks4_destinations_overall %>%
        filter(
          characteristic == input$ks4_destinations_reg_time_filter,
          measure_filter == input$ks4_destinations_reg_time_filter_two,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        arrange(
          Destination,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        )
    } else {
      ks4_destinations_reg_tt <- ks4_destinations_nat_overall %>%
        filter(
          characteristic == input$ks4_destinations_reg_time_filter,
          measure_filter == input$ks4_destinations_reg_time_filter_two
        ) %>%
        arrange(
          Destination,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = country_name,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        )
    }
    return(DTise(
      ks4_destinations_reg_tt,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("% of pupils", 0))
  })



  ## KS4 destinations (LA/bench)
  output$ks4_destinations_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_destinations_overall))

    national_average <- ks4_destinations_nat_overall %>%
      collapse::fsubset(time_period == max(time_period) &
        geographic_level == "National" &
        characteristic == input$ks4_destinations_la_bench_filter &
        Destination == input$ks4_destinations_la_bench_filter_two) %>%
      mutate(outcome = `% of pupils`)

    ks4_destinations_la_bench_data <- ks4_destinations_overall %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$ks4_destinations_la_bench_filter &
        Destination == input$ks4_destinations_la_bench_filter_two &
        time_period == comparison_year) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice]
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "% of pupils")

    ks4_destinations_la_bench_data$rank_statement <- rank_statement_fun(ks4_destinations_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )


    la_choice_validated <- if (is.null(input$la_choice)) {
      "Other LA"
    } else {
      input$la_choice
    }

    # Create graph
    chosen_la <- ks4_destinations_la_bench_data %>% filter(la_name == input$la_choice)
    if (nrow(chosen_la) > 0) {
      plot_la_bench(
        data = ks4_destinations_la_bench_data,
        outcome_var = "% of pupils",
        label_y = "% of pupils",
        la_choice_validated = la_choice_validated,
        hover_label = "% of pupils",
        national_average = national_average
      )
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })

  output$ks4_destinations_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_destinations_overall))

    if (nrow(fsubset(ks4_destinations_overall, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      ks4_destinations_la_bt <- ks4_destinations_overall %>%
        collapse::fsubset(geographic_level == "Local authority" &
          !(la_name %in% small_LAs) &
          characteristic == input$ks4_destinations_la_bench_filter &
          Destination == input$ks4_destinations_la_bench_filter_two &
          time_period == comparison_year) %>%
        filter(if (input$myregion_switch == TRUE) {
          region_name == region_name[la_name == input$la_choice]
        } else {
          region_name != "none"
        }) %>%
        arrange(Destination) %>%
        fselect(
          `Academic Year` = academic_year,
          `Local Authority` = la_name,
          `SEN status` = characteristic,
          Destination,
          `% of pupils`
        )

      return(DTise(
        ks4_destinations_la_bt,
        list(list(3, "desc"), list(4, "desc"))
      ) %>%
        formatRound("% of pupils", 0))
    }
  })



  ## KS4 destinations (region/bench)
  output$ks4_destinations_reg_bench <- renderPlotly({
    national_average <- ks4_destinations_nat_overall %>%
      collapse::fsubset(geographic_level == "National" &
        characteristic == input$ks4_destinations_reg_bench_filter &
        Destination == input$ks4_destinations_reg_bench_filter_two &
        time_period == max(time_period)) %>%
      mutate(outcome = `% of pupils`)

    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    ks4_destinations_reg_bench_s1 <- ks4_destinations_overall %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks4_destinations_reg_bench_filter &
        Destination == input$ks4_destinations_reg_bench_filter_two &
        time_period == max(time_period)) %>%
      mutate(Region = region_name)

    ks4_destinations_reg_bench <- plot_reg_bench(
      data = ks4_destinations_reg_bench_s1,
      outcome_var = ks4_destinations_reg_bench_s1$`% of pupils`,
      label_y = paste0("% of pupils"),
      region_choice_validated = paste0(region_choice_validated),
      hover_label = "% of pupils",
      national_average = national_average
    )
  })

  output$ks4_destinations_reg_bench_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
    }
    ks4_dest_reg_bt <- ks4_destinations_overall %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks4_destinations_reg_bench_filter &
        Destination == input$ks4_destinations_reg_bench_filter_two &
        time_period == max(time_period)) %>%
      ungroup() %>%
      arrange(Destination) %>%
      fselect(
        `Academic Year` = academic_year,
        `Region` = region_name,
        `SEN status` = characteristic,
        Destination,
        `% of pupils`
      )

    return(DTise(
      ks4_dest_reg_bt,
      list(list(0, "desc"), list(4, "desc"))
    ) %>%
      formatRound("% of pupils", 0))
  })


  ## KS4 destinations (LA/type)
  output$ks4_destinations_la_type <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_destinations_overall))
    # req(input$la_choice)
    if (nrow(fsubset(ks4_destinations_overall, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      ks4_destinations_la_type <- ks4_destinations_overall %>%
        fsubset(la_name == input$la_choice &
          Destination == input$ks4_destinations_la_type_filter) %>%
        mutate(`Academic year` = academic_year) %>%
        ggplot(aes(
          x = `Academic year`,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic year: ", academic_year, "\n",
            "% of pupils: ", `% of pupils`, "\n",
            "Characteristic: ", characteristic
          )
        )) +
        geom_point() +
        geom_line() +
        labs(
          x = "Academic year",
          colour = "Group"
        ) +
        scale_y_continuous(limits = c(0, 102)) +
        scale_colour_manual(values = af_palette)

      ks4_destinations_la_type %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$ks4_destinations_la_type_table <- renderDT({
    ks4_destinations_la_typet <- ks4_destinations_overall %>%
      fsubset(la_name == input$la_choice &
        Destination == input$ks4_destinations_la_type_filter) %>%
      arrange(
        academic_year,
        characteristic
      ) %>%
      fselect(
        `Academic year` = academic_year,
        `Local authority` = la_name,
        `SEN Status` = characteristic,
        Destination,
        `% of pupils`
      )

    names(ks4_destinations_la_typet)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(DTise(
      ks4_destinations_la_typet,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 0))
  })



  ## KS4 destinations (region/type)
  output$ks4_destinations_reg_type <- renderPlotly({
    # req(input$level_choice)
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))


      ks4_destinations_reg_type <- ks4_destinations_overall %>%
        filter(
          Destination == input$ks4_destinations_reg_type_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic year: ", academic_year, "\n",
            "% of pupils: ", `% of pupils`, "\n",
            "Characteristic: ", characteristic
          )
        )) +
        geom_point() +
        geom_line() +
        labs(
          x = "Academic year",
          colour = "Group"
        ) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_colour_manual(values = af_palette)

      ks4_destinations_reg_type %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      ks4_destinations_reg_type <- ks4_destinations_nat_overall %>%
        filter(Destination == input$ks4_destinations_reg_type_filter) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic,
          text = paste0(
            "Academic year: ", academic_year, "\n",
            "% of pupils: ", `% of pupils`, "\n",
            "Characteristic: ", characteristic
          )
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_y_continuous(limits = c(0, 100))

      ks4_destinations_reg_type %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$ks4_destinations_reg_type_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ks4_destinations_reg_typet <- ks4_destinations_overall %>%
        filter(
          Destination == input$ks4_destinations_reg_type_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        arrange(
          academic_year,
          characteristic
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        )
    } else {
      ks4_destinations_reg_typet <- ks4_destinations_nat_overall %>%
        filter(Destination == input$ks4_destinations_reg_type_filter) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = country_name,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )
    }

    return(DTise(
      ks4_destinations_reg_typet,
      list(list(2, "desc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 0))
  })



  # FINANCIAL SUSTAINABILITY GRAPHS ---------------------------------------------------------------------------


  ### DSG deficit ####

  ## DSG deficit (LA/time)
  output$dsg_deficit_la_time <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(dsg_deficit))
    latest_dsg <- dsg_deficit %>%
      fsubset(time_period == comparison_year)

    min_scale <- min(dsg_deficit$`DSG cumulative balance as a % of the total income`)
    max_scale <- max(dsg_deficit$`DSG cumulative balance as a % of the total income`)

    dsg_deficit_la_time <- dsg_deficit %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = financial_year,
        y = `DSG cumulative balance as a % of the total income`,
        group = la_name,
        text = paste0(
          "Financial year: ", financial_year, "\n",
          "DSG cumulative balance as a % of the total income: ", `DSG cumulative balance as a % of the total income`
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Financial year",
        y = "DSG cumulative balance as \n % of the total income"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale))


    dsg_deficit_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$dsg_deficit_la_time_table <- renderDT({
    dsg_la_tt <- dsg_deficit %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(financial_year) %>%
      fselect(
        `Financial Year` = financial_year,
        `Local Authority` = la_name,
        `DSG cumulative balance as a % of the total income`
      )

    return(DTise(
      dsg_la_tt,
      list(list(0, "desc"))
    ) %>%
      formatRound("DSG cumulative balance as a % of the total income", 2))
  })

  ## DSG deficit (LA/bench)
  # For this one, the usual if/else filter refused to work, so instead wrapping the whole thing in an if/else.
  observe(if (input$myregion_switch == TRUE) {
    output$dsg_deficit_la_bench <- renderPlotly({
      comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(dsg_deficit))
      # Pull in England data to its own dataframe, for creating the England average dotted line.
      national_average <- eng_dsg_deficit %>%
        collapse::fsubset(time_period == comparison_year &
          geographic_level == "National") %>%
        ungroup() %>% # required before mutate here
        mutate(outcome = `DSG cumulative balance as a % of the total income`)

      chosen_region <- dsg_deficit$region_name[dsg_deficit$la_name == input$la_choice &
        dsg_deficit$time_period == comparison_year]

      dsg_deficit_la_bench_data <- dsg_deficit %>%
        collapse::fsubset(geographic_level == "Local authority" &
          !(la_name %in% small_LAs) &
          time_period == comparison_year &
          region_name == chosen_region) %>%
        collapse::ftransform(
          chosen_la = ifelse(la_name == input$la_choice,
            yes = input$la_choice,
            no = "Other LA"
          ),
          time_period = paste0(substr(as.character(time_period), 1, 4), "-", substr(as.character(time_period), 5, 6))
        )
      # Add ranks
      if (dsg_deficit_la_bench_data$deficit[dsg_deficit_la_bench_data$chosen_la == input$la_choice] > 0) {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total income", reverse = TRUE)
      } else {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total income", reverse = FALSE)
      } # count surpluses from the top and deficits from the bottom


      # Create rank statements to go on the X axis.
      dsg_deficit_la_bench_data$rank_statement <- rank_statement_fun(dsg_deficit_la_bench_data,
        rank_col = rank,
        name_col = la_name,
        time_period = time_period
      )

      dsg_deficit_la_bench_data <- dsg_deficit_la_bench_data %>%
        mutate(rank_statement = case_when(
          chosen_la != input$la_choice ~ NA_character_,
          deficit < 0 ~ gsub(pattern = ", out of", replacement = " largest surplus, out of", x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$chosen_la == input$la_choice]),
          deficit >= 0 ~ gsub(pattern = ", out of", replacement = " largest deficit, out of", x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$chosen_la == input$la_choice])
        ))





      dsg_deficit_la_bench <- dsg_deficit_la_bench_data %>%
        ggplot(aes(fct_reorder(la_name, `DSG cumulative balance as a % of the total income`),
          y = `DSG cumulative balance as a % of the total income`,
          text = paste0("DSG cumulative balance as a % of the total income: ", `DSG cumulative balance as a % of the total income`, "\n", la_name),
          fill = chosen_la
        )) +
        geom_col() +
        # Add England average dotted line and label
        add_england_line_bench(national_average) +
        geom_text(
          data = national_average,
          colour = af_grey,
          size = 3.5,
          inherit.aes = FALSE,
          nudge_y = 0.2,
          aes(
            x = ifelse(test = input$myregion_switch == TRUE,
              yes = 3,
              no = 20
            ),
            y = outcome,
            label = "England average"
          )
        ) +
        labs(
          x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$la_name == input$la_choice],
          y = "DSG cumulative balance as \n % of the total income"
        ) +
        theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

      dsg_deficit_la_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line() %>%
        use_horizontal_legend()
    })
  } else {
    output$dsg_deficit_la_bench <- renderPlotly({
      comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(dsg_deficit))
      # Pull in England data to its own dataframe, for creating the England average dotted line.
      national_average <- eng_dsg_deficit %>%
        collapse::fsubset(time_period == comparison_year &
          geographic_level == "National") %>%
        ungroup() %>% # required before mutate here
        mutate(outcome = `DSG cumulative balance as a % of the total income`)

      dsg_deficit_la_bench_data <- dsg_deficit %>%
        collapse::fsubset(geographic_level == "Local authority" &
          time_period == comparison_year) %>%
        collapse::ftransform(
          chosen_la = ifelse(la_name == input$la_choice,
            yes = input$la_choice,
            no = "Other LA"
          ),
          time_period = paste0(substr(as.character(time_period), 1, 4), "-", substr(as.character(time_period), 5, 6))
        )
      # Add ranks
      if (dsg_deficit_la_bench_data$deficit[dsg_deficit_la_bench_data$chosen_la == input$la_choice] > 0) {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total income", reverse = TRUE)
      } else {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total income", reverse = FALSE)
      } # count surpluses from the top and deficits from the bottom


      # Create rank statements to go on the X axis.
      dsg_deficit_la_bench_data$rank_statement <- rank_statement_fun(dsg_deficit_la_bench_data,
        rank_col = rank,
        name_col = la_name,
        time_period = time_period
      )

      dsg_deficit_la_bench_data <- dsg_deficit_la_bench_data %>%
        mutate(rank_statement = case_when(
          chosen_la != input$la_choice ~ NA_character_,
          deficit < 0 ~ gsub(pattern = ", out of", replacement = " largest surplus, out of", x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$chosen_la == input$la_choice]),
          deficit >= 0 ~ gsub(pattern = ", out of", replacement = " largest deficit, out of", x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$chosen_la == input$la_choice])
        ))

      dsg_deficit_la_bench <- dsg_deficit_la_bench_data %>%
        ggplot(aes(fct_reorder(la_name, `DSG cumulative balance as a % of the total income`),
          y = `DSG cumulative balance as a % of the total income`,
          text = la_name,
          fill = chosen_la
        )) +
        geom_col() +
        # Add England average dotted line and label
        add_england_line_bench(national_average) +
        geom_text(
          data = national_average,
          colour = af_grey,
          size = 3.5,
          inherit.aes = FALSE,
          nudge_y = 2,
          aes(
            x = ifelse(test = input$myregion_switch == TRUE,
              yes = 3,
              no = 20
            ),
            y = outcome,
            label = "England average"
          )
        ) +
        labs(
          x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$la_name == input$la_choice],
          y = "DSG cumulative balance as \n % of the total income",
          fill = "Local Authorities"
        ) +
        theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

      dsg_deficit_la_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line() %>%
        use_horizontal_legend()
    })
  })

  output$dsg_deficit_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(dsg_deficit))

    dsg_la_bt <- dsg_deficit %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs)) %>%
      ungroup() %>% # otherwise the next line breaks
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      arrange(`DSG cumulative balance as a % of the total income`) %>%
      fselect(
        `Financial Year` = financial_year,
        `Local Authority` = la_name,
        `DSG cumulative balance as a % of the total income`
      )

    return(DTise(
      dsg_la_bt,
      list(list(2, "desc"))
    ) %>%
      formatRound("DSG cumulative balance as a % of the total income", 2))
  })

  ## DSG deficit (region/time)
  output$dsg_deficit_reg_time <- renderPlotly({
    validate(need(input$region_choice, message = "Please select a region"))
    reg_dsg <- dsg_deficit %>%
      fsubset(geographic_level == "Regional")

    min_scale <- min(reg_dsg$`DSG cumulative balance as a % of the total income`)
    max_scale <- max(reg_dsg$`DSG cumulative balance as a % of the total income`)

    if (input$level_choice == "Regions") {
      dsg_deficit2 <- dsg_deficit %>% filter(
        geographic_level == "Regional",
        region_name == input$region_choice
      )
    } else {
      dsg_deficit2 <- dsg_deficit %>% filter(geographic_level == "National")
    } # End of "if region choice is valid"  if clause

    dsg_deficit_reg_time_p <- dsg_deficit2 %>%
      ggplot(aes(
        x = financial_year,
        y = `DSG cumulative balance as a % of the total income`,
        group = la_name,
        text = paste0(
          "Financial year: ", financial_year, "\n",
          "DSG cumulative balance as a % of the total income: ", `DSG cumulative balance as a % of the total income`
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Financial year",
        y = "DSG cumulative balance as \n % of the total income"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale))

    dsg_deficit_reg_time <- dsg_deficit_reg_time_p %>%
      ggplotly(tooltip = c("text")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
    dsg_deficit_reg_time
  })

  output$dsg_deficit_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      dsg_deficit_reg_tt <- dsg_deficit %>% filter(
        geographic_level == "Regional",
        region_name == input$region_choice
      )
    } else {
      dsg_deficit_reg_tt <- dsg_deficit %>%
        filter(geographic_level == "National") %>%
        fmutate(region_name = "England")
    }

    dsg_deficit_reg_tt <- dsg_deficit_reg_tt %>%
      distinct() %>%
      arrange(financial_year) %>%
      fselect(
        `Financial Year` = financial_year,
        Region = region_name,
        `DSG cumulative balance as a % of the total income`
      )

    return(DTise(
      dsg_deficit_reg_tt,
      list(list(0, "desc"))
    ) %>%
      formatRound("DSG cumulative balance as a % of the total income", 2))
  })

  ## DSG deficit (region/bench)
  output$dsg_deficit_reg_bench <- renderPlotly({
    validate(need(input$region_choice, message = "Please select a region"))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_dsg_deficit %>%
      collapse::fsubset(time_period == max(time_period) &
        geographic_level == "National") %>%
      ungroup() %>% # required before mutate here
      mutate(outcome = `DSG cumulative balance as a % of the total income`)

    dsg_deficit_reg_bench1 <- dsg_deficit %>%
      collapse::fsubset(geographic_level == "Regional" &
        time_period == max(time_period)) %>%
      ungroup()

    # First check region choice is selected
    if (input$level_choice == "Regions") {
      dsg_deficit_reg_bench2 <- dsg_deficit_reg_bench1 %>%
        mutate(chosen_region = ifelse(test = (region_name == input$region_choice),
          yes = input$region_choice,
          no = "Other region"
        ))
    } else {
      dsg_deficit_reg_bench2 <- dsg_deficit_reg_bench1 %>%
        mutate(chosen_region = "English regions")
    } # End of "if region choice is valid"  if clause

    dsg_deficit_reg_bench <- dsg_deficit_reg_bench2 %>%
      collapse::ftransform(region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)) %>%
      ggplot(aes(fct_reorder(region_name, `DSG cumulative balance as a % of the total income`),
        y = `DSG cumulative balance as a % of the total income`,
        text = paste0("DSG cumulative balance as a % of the total income: ", `DSG cumulative balance as a % of the total income`, "\n", region_name), fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      geom_text(
        data = national_average,
        colour = af_grey,
        size = 3.5,
        inherit.aes = FALSE,
        nudge_y = 0.5,
        aes(
          x = 9,
          y = outcome,
          label = "England average"
        )
      ) +
      labs(
        x = "Regions in England",
        y = "DSG cumulative balance as \n % of the total income",
        fill = "Region"
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = 1
      ), legend.position = "none") +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other region" = not_focused), na.value = focused
        )
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }

    dsg_deficit_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$dsg_deficit_reg_bench_table <- renderDT({
    dsg_reg_bt <- dsg_deficit %>%
      collapse::fsubset(geographic_level == "Regional" &
        time_period == max(time_period)) %>%
      ungroup() %>%
      distinct() %>%
      arrange(desc(`DSG cumulative balance as a % of the total income`)) %>%
      fselect(
        `Financial Year` = financial_year,
        Region = region_name,
        `DSG cumulative balance as a % of the total income`
      )

    return(DTise(
      dsg_reg_bt,
      list(list(2, "desc"))
    ) %>%
      formatRound("DSG cumulative balance as a % of the total income", 2))
  })

  ### Specialist Spend  ####

  ## Specialist Spend (LA/time)
  output$specialist_spend_la_time <- renderPlotly({
    latest_specialist_spend <- specialist_spend %>%
      fsubset(year == max(year))

    min_scale <- min(specialist_spend$`Spend per head`[specialist_spend$category == "Total"], na.rm = TRUE)
    max_scale <- max(specialist_spend$`Spend per head`, na.rm = TRUE)

    specialist_spend_la_time <- specialist_spend %>%
      fsubset(la_name == input$la_choice &
        category != "Total") %>%
      ggplot(aes(
        x = year,
        y = `Spend per head`,
        group = la_name,
        fill = category,
        text = paste0(
          "Financial year: ", year, "\n",
          "Spend per head: £", round(`Spend per head`, 0)
        )
      )) +
      geom_col(position = "stack") +
      labs(
        x = "Financial year",
        y = "Per capita spend on special schools and AP",
        fill = "School type"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale), labels = scales::dollar_format(prefix = "£")) +
      scale_fill_manual(values = c(af_darkblue, af_turquoise))


    specialist_spend_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$specialist_spend_la_time_table <- renderDT({
    spec_la_tt <- specialist_spend %>%
      fsubset(la_name == input$la_choice &
        category != "Total") %>%
      fmutate(`Spend per head` = round(`Spend per head`, 2)) %>%
      arrange(
        category,
        year
      ) %>%
      fselect(
        `Financial Year` = year,
        `Local Authority` = la_name,
        `Sector` = category,
        `Per capita spend on special schools and AP` = `Spend per head`
      )

    return(DTise(
      spec_la_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("Per capita spend on special schools and AP", 2))
  })

  ## Specialist Spend (reg/time)
  output$specialist_spend_reg_time <- renderPlotly({
    latest_specialist_spend <- ungroup(reg_specialist_spend) %>%
      fsubset(year == max(year))

    min_scale <- min(specialist_spend$`Spend per head`[specialist_spend$category == "Total"], na.rm = TRUE)
    max_scale <- max(specialist_spend$`Spend per head`, na.rm = TRUE)

    if (input$level_choice == "England") {
      specialist_spend_reg_data <- ungroup(nat_specialist_spend) %>%
        fsubset(category != "Total") %>%
        fmutate(region = "England") # this is just so you can plot both graphs with the same code
    } else {
      specialist_spend_reg_data <- ungroup(reg_specialist_spend) %>%
        fsubset(region == input$region_choice &
          category != "Total")
    }
    specialist_spend_reg_time <- ggplot(specialist_spend_reg_data, aes(
      x = year,
      y = `Spend per head`,
      group = region,
      fill = category,
      text = paste0(
        "Financial year: ", year, "\n",
        "Spend per head: £", round(`Spend per head`, 0)
      )
    )) +
      geom_col(position = "stack") +
      labs(
        x = "Financial year",
        y = "Per capita spend on special schools and AP",
        fill = "School type"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale), labels = scales::dollar_format(prefix = "£")) +
      scale_fill_manual(values = c(af_darkblue, af_turquoise))


    specialist_spend_reg_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$specialist_spend_reg_time_table <- renderDT({
    if (input$level_choice == "England") {
      spec_reg_tt <- ungroup(nat_specialist_spend) %>%
        fsubset(category != "Total") %>%
        fmutate(region = "England")
    } else {
      spec_reg_tt <- ungroup(reg_specialist_spend) %>%
        fsubset(region == input$region_choice &
          category != "Total")
    }

    spec_reg_tt <- spec_reg_tt %>%
      ungroup() %>%
      arrange(
        category,
        year
      ) %>%
      fmutate(`Per capita spend on special schools and AP` = round(`Spend per head`, digits = 2)) %>%
      fselect(
        `Financial Year` = year,
        `Region` = region,
        `Sector` = category,
        `Per capita spend on special schools and AP`
      )

    return(DTise(
      spec_reg_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("Per capita spend on special schools and AP", 2))
  })


  ## Specialist Spend (LA/bench)

  output$specialist_spend_la_bench <- renderPlotly({
    # req(input$la_choice)
    comparison <- specialist_spend %>%
      collapse::fsubset(la_name == input$la_choice) %>%
      drop_na(`Spend per head`) # because the dataset includes Northamptonshire in 2021-22 but has it as NAs
    comparison_year <- max(comparison$year, na.rm = T)
    chosen_region <- specialist_spend$region[specialist_spend$la_name == input$la_choice &
      specialist_spend$year == comparison_year &
      specialist_spend$category == "Total"]

    x_axis_title <- ifelse(test = input$myregion_switch == TRUE,
      yes = paste0(
        "Local authorities in ", chosen_region,
        " in ", comparison_year,
        " (dark bar is ",
        input$la_choice, ")"
      ),
      no = paste0(
        "Local authorities in England in ",
        comparison_year,
        " (dark bar is ", input$la_choice, ")"
      )
    )

    specialist_spend_la_bench <- specialist_spend %>%
      collapse::fsubset(year == comparison_year &
        if (input$myregion_switch == TRUE) {
          region == chosen_region
        } else {
          region != "TEST"
        }) %>%
      collapse::ftransform(chosen_la = case_when(
        la_name == input$la_choice ~
          input$la_choice,
        la_name != input$la_choice & category == "Independent or non-maintained" ~
          "Other LA (Ind/non-maintained spending)",
        la_name != input$la_choice & category == "State" ~
          "Other LA (State spending)",
        la_name != input$la_choice & category == "Total" ~
          "Other LA (Total spending)"
      )) %>%
      ggplot(aes(
        x = tidytext::reorder_within(
          x = la_name,
          by = `Spend per head`,
          within = category,
          sep = "*"
        ),
        y = `Spend per head`,
        text = la_name,
        fill = chosen_la
      )) +
      geom_col(position = "stack") +
      labs(x = x_axis_title) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c(
        "Other LA (Total spending)" = af_purple,
        "Other LA (State spending)" = af_darkblue,
        "Other LA (Ind/non-maintained spending)" = af_turquoise
      ), na.value = af_grey) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
      facet_wrap(~category, nrow = 3, scales = "free_x") +
      tidytext::scale_x_reordered() +
      theme(legend.position = "none")
    if (sum(specialist_spend_la_bench$data$`Spend per head`[specialist_spend_la_bench$data$la_name == input$la_choice] != 0) != 0) {
      specialist_spend_la_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        layout(dragmode = FALSE)
    }
  })

  output$specialist_spend_la_bench_table <- renderDT({
    comparison <- specialist_spend %>%
      collapse::fsubset(la_name == input$la_choice)
    comparison_year <- max(comparison$year, na.rm = T)
    chosen_region <- specialist_spend$region[specialist_spend$la_name == input$la_choice &
      specialist_spend$year == comparison_year &
      specialist_spend$category == "Total"]

    spend_la_bt <- specialist_spend %>%
      drop_na(`Spend per head`) %>%
      collapse::fsubset(year == comparison_year &
        if (input$myregion_switch == TRUE) {
          region == chosen_region
        } else {
          region != "TEST"
        }) %>%
      collapse::ftransform(chosen_la = case_when(
        la_name == input$la_choice ~
          input$la_choice,
        la_name != input$la_choice & category == "Independent or non-maintained" ~
          "Other LA (Ind/non-maintained spending)",
        la_name != input$la_choice & category == "State" ~
          "Other LA (State spending)",
        la_name != input$la_choice & category == "Total" ~
          "Other LA (Total spending)"
      )) %>%
      arrange(
        `category`,
        `Spend per head`
      ) %>%
      fselect(
        `Financial Year` = year,
        `Local Authority` = la_name,
        `Sector` = category,
        `Spend per head`
      )

    return(DTise(
      spend_la_bt,
      list(list(2, "asc"), list(3, "desc"))
    ) %>%
      formatRound(columns = "Spend per head", digits = 2))
  })

  ## Specialist Spend (Reg/bench)

  output$specialist_spend_reg_bench <- renderPlotly({
    # req(input$la_choice)

    if (input$level_choice == "Regions") {
      specialist_spend_reg_bench <- reg_specialist_spend %>%
        ungroup() %>%
        collapse::fsubset(year == max(year) &
          !(is.na(region))) %>%
        collapse::ftransform(chosen_region = case_when(
          region == input$region_choice ~
            input$region_choice,
          region != input$region_choice & category == "Independent or non-maintained" ~
            "Other Regions (Ind/non-maintained spending)",
          region != input$region_choice & category == "State" ~
            "Other Regions (State spending)",
          region != input$region_choice & category == "Total" ~
            "Other Regions (Total spending)"
        )) %>%
        ggplot(aes(
          x = tidytext::reorder_within(
            x = region,
            by = `Spend per head`,
            within = category,
            sep = "*"
          ),
          y = `Spend per head`,
          text = region,
          fill = chosen_region
        )) +
        geom_col(position = "stack") +
        labs(x = "Regions in England", fill = "Selected Region") +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ), legend.position = "bottom") +
        scale_fill_manual(values = c(
          "Other Regions (Total spending)" = af_purple,
          "Other Regions (State spending)" = af_darkblue,
          "Other Regions (Ind/non-maintained spending)" = af_turquoise
        ), na.value = af_grey) +
        scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
        facet_wrap(~category, nrow = 3, scales = "free_x") +
        tidytext::scale_x_reordered(labels = function(x) gsub("*.+$", "", x))
    } else {
      specialist_spend_reg_bench <- reg_specialist_spend %>%
        ungroup() %>%
        mutate(
          region = if_else(region == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region),
          region = factor(region, levels = reg_specialist_spend_order)
        ) %>%
        collapse::fsubset(year == max(year) &
          !(is.na(region))) %>%
        ggplot(aes(
          x = region,
          y = `Spend per head`,
          text = region,
          fill = category
        )) +
        geom_col(position = "stack") +
        labs(x = "Regions in England", fill = "Spend on") +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ), legend.position = "bottom") +
        scale_fill_manual(values = c(
          "Total" = af_purple,
          "State" = af_darkblue,
          "Independent or non-maintained" = af_turquoise
        ), na.value = af_grey) +
        scale_y_continuous(labels = scales::dollar_format(prefix = "£")) +
        facet_wrap(~category, nrow = 3)
    }


    if (sum(specialist_spend_reg_bench$data$`Spend per head`[specialist_spend_reg_bench$data$region == input$region_choice] != 0) != 0) {
      specialist_spend_reg_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.3),
          dragmode = FALSE
        )
    }
  })

  output$specialist_spend_reg_bench_table <- renderDT({
    comparison <- reg_specialist_spend %>%
      ungroup() %>%
      collapse::fsubset(year == max(year))


    spend_reg_bt <- comparison %>%
      collapse::fsubset(!is.na(region)) %>%
      arrange(
        `category`,
        `Spend per head`
      ) %>%
      fselect(
        `Financial Year` = year,
        `Region` = region,
        `Sector` = category,
        `Spend per head`
      )

    return(DTise(
      spend_reg_bt,
      list(list(2, "asc"), list(3, "desc"))
    ) %>%
      formatRound(columns = "Spend per head", digits = 2))
  })

  # IDENTIFICATION OF NEED GRAPHS ---------------------------------------------------------------------------


  ### % of pupils with EHCP/SEN support ####

  ## % of pupils with EHCP (LA/time)
  output$percent_pupils_ehcp_la_time <- renderPlotly({
    # req(input$la_choice)

    percent_pupils_ehcp_la_time <- percent_pupils_ehcp %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = academic_year,
        y = `% of pupils`,
        colour = `SEN provision`,
        group = `SEN provision`,
        text = paste0("Academic year: ", academic_year)
      )) +
      geom_line() +
      geom_point() +
      labs(x = "Academic year") +
      scale_colour_manual(values = af_palette) +
      coord_cartesian(ylim = c(0, NA))

    percent_pupils_ehcp_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$percent_pupils_ehcp_la_time_table <- renderDT({
    ehcp_ppc_la_tt <- percent_pupils_ehcp %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(
        `SEN provision`,
        academic_year
      ) %>%
      fselect(
        `academic_year` = academic_year,
        `Local Authority` = la_name,
        `SEN provision`,
        `% of pupils`
      )

    return(DTise(
      ehcp_ppc_la_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 2))
  })

  ## % of pupils with EHCP (LA/bench)
  output$percent_pupils_ehcp_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(percent_pupils_ehcp))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_percent_pupils_ehcp %>%
      collapse::fsubset(`SEN provision` == input$percent_pupils_ehcp_la_filter &
        time_period == comparison_year &
        geographic_level == "National") %>%
      mutate(outcome = `% of pupils`)

    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    percent_pupils_ehcp_la_bench_data <- percent_pupils_ehcp %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `SEN provision` == input$percent_pupils_ehcp_la_filter) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "none"
      }) %>%
      add_ranks(outcome = "% of pupils")

    # Create rank statements to go on the X axis.
    percent_pupils_ehcp_la_bench_data$rank_statement <- rank_statement_fun(percent_pupils_ehcp_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )

    # Create graph
    percent_pupils_ehcp_la_bench <- percent_pupils_ehcp_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `% of pupils`),
        y = `% of pupils`,
        text = paste0(
          "% of pupils: ", `% of pupils`, "\n",
          la_name
        ),
        fill = chosen_la
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        y = paste("% of pupils with", input$percent_pupils_ehcp_la_filter),
        x = percent_pupils_ehcp_la_bench_data$rank_statement[percent_pupils_ehcp_la_bench_data$la_name == input$la_choice],
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    percent_pupils_ehcp_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$percent_pupils_ehcp_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(percent_pupils_ehcp))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ppc_ehcp_la_bt <- percent_pupils_ehcp %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `SEN provision` == input$percent_pupils_ehcp_la_filter) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "none"
      }) %>%
      arrange(`% of pupils`) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN provision`,
        `% of pupils`
      )

    return(DTise(
      ppc_ehcp_la_bt,
      list(list(3, "desc"))
    ) %>%
      formatRound("% of pupils", 2))
  })

  ## % of pupils with EHCP (region/time)
  output$percent_pupils_ehcp_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      percent_pupils_ehcp_reg_time <- percent_pupils_ehcp %>%
        filter(
          geographic_level == "Regional",
          region_name == if_else(condition = input$region_choice %in% c("Inner London", "Outer London"),
            true = "London",
            false = input$region_choice
          )
        ) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = `SEN provision`,
          colour = `SEN provision`,
          text = paste0("Academic year: ", academic_year)
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette) +
        coord_cartesian(ylim = c(0, NA))
    } else {
      percent_pupils_ehcp_reg_time <- percent_pupils_ehcp %>%
        filter(geographic_level == "National") %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = `SEN provision`,
          colour = `SEN provision`,
          text = paste0("Academic year: ", academic_year)
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette) +
        coord_cartesian(ylim = c(0, NA))
    }

    percent_pupils_ehcp_reg_time %>%
      ggplotly(tooltip = c("text", "y", "colour")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$percent_pupils_ehcp_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      pp_ehcp_reg_tt <- percent_pupils_ehcp %>%
        filter(
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        arrange(
          `SEN provision`,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN provision`,
          `% of pupils`
        )
    } else {
      pp_ehcp_reg_tt <- percent_pupils_ehcp %>%
        filter(geographic_level == "National") %>%
        arrange(
          `SEN provision`,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN provision`,
          `% of pupils`
        )
    }
    return(DTise(
      pp_ehcp_reg_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 2))
  })

  ## % of pupils with EHCP (region/bench)
  output$percent_pupils_ehcp_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_percent_pupils_ehcp %>%
      collapse::fsubset(`SEN provision` == input$percent_pupils_ehcp_reg_filter &
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = `% of pupils`)

    percent_pupils_ehcp_reg_bench_basic <- percent_pupils_ehcp %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == input$percent_pupils_ehcp_reg_filter &
        time_period == max(time_period))

    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      percent_pupils_ehcp_reg_bench_data <- percent_pupils_ehcp_reg_bench_basic %>%
        collapse::ftransform(chosen_region = ifelse(region_name == input$region_choice,
          yes = input$region_choice,
          no = "Other region"
        ))
    } else {
      percent_pupils_ehcp_reg_bench_data <- percent_pupils_ehcp_reg_bench_basic %>%
        collapse::ftransform(chosen_region = "English regions")
    }


    percent_pupils_ehcp_reg_bench <- percent_pupils_ehcp_reg_bench_data %>%
      collapse::ftransform(region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)) %>%
      ggplot(aes(fct_reorder(region_name, `% of pupils`),
        y = `% of pupils`,
        text = region_name,
        fill = chosen_region,
        text = paste0(
          "% of pupils: ", `% of pupils`, "\n",
          region_name
        )
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 0.2) +
      labs(
        x = "Regions in England",
        y = paste("% of pupils with", input$percent_pupils_ehcp_reg_filter),
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other region" = not_focused), na.value = focused
        )
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }


    percent_pupils_ehcp_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      # This code makes the dotted line a bit thinner.
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$percent_pupils_ehcp_reg_bench_table <- renderDT({
    pp_ehcp_reg_bt <- percent_pupils_ehcp %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == input$percent_pupils_ehcp_reg_filter &
        time_period == max(time_period)) %>%
      ungroup() %>%
      arrange(`% of pupils`) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN provision`,
        `% of pupils`
      )

    return(DTise(
      pp_ehcp_reg_bt,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("% of pupils", 2))
  })

  ### EHCP Age profile ####

  ## EHCP age profile (LA/time)
  output$ehcp_ageprofile_la_time <- renderPlotly({
    # req(input$la_choice)

    ehcp_ageprofile_la_time <- ehcp_ageprofile %>%
      fsubset(la_name == input$la_choice & time_period > 2017) %>%
      ggplot(aes(
        x = time_period,
        y = `Number of EHCPs`,
        fill = `Age group`,
        text = paste0("Calendar year: ", time_period),
        label = ifelse(test = (`Number of EHCPs` / sum(`Number of EHCPs`, na.rm = TRUE) > 0.02),
          yes = str_remove(`Age group`, "Age "),
          no = NA
        )
      )) +
      geom_col(position = "stack", colour = "white") +
      geom_text(
        size = 3,
        colour = "white",
        position = position_stack(vjust = 0.5)
      ) +
      labs(x = "Calendar year") +
      scale_fill_manual(values = af_gradient) +
      scale_x_continuous(n.breaks = 8)

    ehcp_ageprofile_la_time %>%
      ggplotly(
        tooltip = c("y", "fill", "text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ehcp_ageprofile_la_time_table <- renderDT({
    ehcp_age_la_tt <- ehcp_ageprofile %>%
      fsubset(la_name == input$la_choice & time_period > 2017) %>%
      arrange(
        time_period,
        desc(`Age group`)
      ) %>%
      fselect(
        Year = time_period,
        `Local Authority` = la_name,
        `Age group`,
        `Number of EHCPs`
      )

    return(DTise(
      ehcp_age_la_tt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("Number of EHCPs", 0))
  })

  ## EHCP age profile (region/time)
  output$ehcp_ageprofile_reg_time <- renderPlotly({
    # req(input$level_choice)

    # if(input$level_choice == "Regions") req(input$region_choice)
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ehcp_ageprofile_reg_time <- ehcp_ageprofile %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional" &
          time_period > 2017) %>%
        ggplot(aes(
          x = time_period,
          y = `Number of EHCPs`,
          fill = `Age group`,
          text = paste0("Calendar year: ", time_period),
          label = str_remove(`Age group`, "Age")
        )) +
        geom_col(position = "stack", colour = "white") +
        geom_text(
          size = 3,
          colour = "white",
          position = position_stack(vjust = 0.5)
        ) +
        labs(x = "Calendar year") +
        scale_fill_manual(values = af_gradient) +
        scale_x_continuous(n.breaks = 8)
    } else {
      ehcp_ageprofile_reg_time <- ehcp_ageprofile %>%
        filter(geographic_level == "National") %>%
        ggplot(aes(
          x = time_period,
          y = `Number of EHCPs`,
          fill = `Age group`,
          text = paste0("Calendar year: ", time_period),
          label = ifelse(test = (`Number of EHCPs` / sum(`Number of EHCPs`, na.rm = TRUE) > 0.02),
            yes = str_remove(`Age group`, "Age "),
            no = NA
          )
        )) +
        geom_col(position = "stack", colour = "white") +
        labs(x = "Calendar year") +
        scale_fill_manual(values = af_gradient) +
        scale_x_continuous(n.breaks = 8)
    }

    ehcp_ageprofile_reg_time %>%
      ggplotly(tooltip = c("y", "fill", "text")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ehcp_ageprofile_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ehcp_age_reg_tt <- ehcp_ageprofile %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional" &
          time_period > 2017) %>%
        arrange(
          time_period,
          desc(`Age group`)
        ) %>%
        fmutate(`Number of EHCPs` = as.character(`Number of EHCPs`)) %>% # prevent 2dp nonsense
        fselect(
          Year = time_period,
          `Region` = region_name,
          `Age group`,
          `Number of EHCPs`
        )
    } else {
      ehcp_age_reg_tt <- ehcp_ageprofile %>%
        filter(geographic_level == "National" &
          time_period > 2017) %>%
        arrange(
          time_period,
          desc(`Age group`)
        ) %>%
        fmutate(
          region_name = "England"
        ) %>%
        fselect(
          Year = time_period,
          Region = region_name,
          `Age group`,
          `Number of EHCPs`
        )
    }
    return(DTise(
      ehcp_age_reg_tt,
      list(list(0, "desc"), list(2, "desc"))
    ) %>%
      formatRound("Number of EHCPs", 0))
  })

  ### % of mainstream pupils with SEN ####
  ## % of mainstream pupils with SEN (LA/time)
  output$mainstream_with_sen_la_time <- renderPlotly({
    # req(input$la_choice)

    mainstream_with_sen_la_time <- mainstream_with_sen %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = academic_year,
        y = `% of pupils`,
        colour = `SEN provision`,
        group = `SEN provision`,
        text = paste0("% of pupils: ", `% of pupils`, "\n", la_name)
      )) +
      geom_line() +
      geom_point() +
      labs(x = "Academic year") +
      scale_colour_manual(values = af_palette) +
      coord_cartesian(ylim = c(0, NA))

    mainstream_with_sen_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$mainstream_with_sen_la_time_table <- renderDT({
    msen_la_tt <- mainstream_with_sen %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(
        `SEN provision`,
        time_period
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN provision`,
        `% of pupils`
      )

    return(DTise(
      msen_la_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 2))
  })

  ## % of mainstream pupils with SEN (LA/bench)
  output$mainstream_with_sen_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(mainstream_with_sen))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_mainstream_with_sen %>%
      collapse::fsubset(`SEN provision` == input$mainstream_with_sen_la_filter &
        time_period == comparison_year &
        geographic_level == "National") %>%
      mutate(outcome = `% of pupils`)

    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    mainstream_with_sen_la_bench_data <- mainstream_with_sen %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `SEN provision` == input$mainstream_with_sen_la_filter) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "none"
      }) %>%
      # Add ranks
      add_ranks(outcome = "% of pupils")

    # Create rank statements to go on the X axis.
    mainstream_with_sen_la_bench_data$rank_statement <- rank_statement_fun(mainstream_with_sen_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )



    mainstream_with_sen_la_bench <- mainstream_with_sen_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `% of pupils`),
        y = `% of pupils`,
        text = la_name,
        fill = chosen_la
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        y = "% of pupils",
        x = mainstream_with_sen_la_bench_data$rank_statement[mainstream_with_sen_la_bench_data$la_name == input$la_choice],
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    mainstream_with_sen_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$mainstream_with_sen_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(mainstream_with_sen))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    msen_la_bt <- mainstream_with_sen %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `SEN provision` == input$mainstream_with_sen_la_filter) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "none"
      }) %>%
      arrange(`% of pupils`) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN provision`,
        `% of pupils`
      )

    return(DTise(
      msen_la_bt,
      list(3, "desc")
    ) %>%
      formatRound("% of pupils", 1))
  })

  ## % of mainstream pupils with SEN (region/time)
  output$mainstream_with_sen_reg_time <- renderPlotly({
    # req(input$level_choice)
    if (input$level_choice == "Regions") {
      req(input$region_choice)

      mainstream_with_sen_reg_time <- mainstream_with_sen %>%
        filter(
          geographic_level == "Regional",
          region_name == if_else(
            condition = (input$region_choice %in% c(
              "Inner London",
              "Outer London"
            )),
            true = "London",
            false = input$region_choice
          )
        ) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = `SEN provision`,
          colour = `SEN provision`,
          text = paste0("% of pupils: ", `% of pupils`, "\n", region_name)
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette) +
        coord_cartesian(ylim = c(0, NA))


      mainstream_with_sen_reg_time %>%
        ggplotly(tooltip = c("text")) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        )
    } else {
      mainstream_with_sen_reg_time <- mainstream_with_sen %>%
        filter(geographic_level == "National") %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = `SEN provision`,
          colour = `SEN provision`,
          text = paste0("Academic year: ", academic_year)
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette) +
        coord_cartesian(ylim = c(0, NA))


      mainstream_with_sen_reg_time %>%
        ggplotly(tooltip = c("text", "y", "colour")) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        )
    }
  })

  output$mainstream_with_sen_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      msen_reg_tt <- mainstream_with_sen %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional") %>%
        arrange(
          `SEN provision`,
          time_period
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          `Region` = region_name,
          `SEN provision`,
          `% of pupils`
        )
    } else {
      msen_reg_tt <- mainstream_with_sen %>%
        filter(geographic_level == "National") %>%
        arrange(
          `SEN provision`,
          time_period
        ) %>%
        fmutate(region_name = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN provision`,
          `% of pupils`
        )
    }
    return(DTise(
      msen_reg_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils", 1))
  })

  ## % of mainstream pupils with SEN(region/bench)
  output$mainstream_with_sen_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_mainstream_with_sen %>%
      collapse::fsubset(`SEN provision` == input$mainstream_with_sen_reg_filter &
        time_period == max(time_period) &
        geographic_level == "National") %>%
      mutate(outcome = `% of pupils`)

    mainstream_with_sen_reg_bench_basic <- mainstream_with_sen %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == input$mainstream_with_sen_reg_filter &
        time_period == max(time_period))

    if (input$level_choice == "Regions" & is.character(input$region_choice)) {
      mainstream_with_sen_reg_bench_data <- mainstream_with_sen_reg_bench_basic %>%
        collapse::ftransform(chosen_region = ifelse(region_name == input$region_choice,
          yes = input$region_choice,
          no = "Other region"
        ))
    } else {
      mainstream_with_sen_reg_bench_data <- mainstream_with_sen_reg_bench_basic %>%
        collapse::ftransform(chosen_region = "English regions")
    }

    mainstream_with_sen_reg_bench <- mainstream_with_sen_reg_bench_data %>%
      collapse::ftransform(region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)) %>%
      ggplot(aes(fct_reorder(region_name, `% of pupils`),
        y = `% of pupils`,
        text = region_name,
        fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 0.2) +
      labs(
        x = "Regions in England",
        y = paste("% of pupils with", input$mainstream_with_sen_reg_filter),
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
      } else {
        scale_fill_manual(values = focused)
      }

    mainstream_with_sen_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$mainstream_with_sen_reg_bench_table <- renderDT({
    msen_reg_bt <- mainstream_with_sen %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == input$mainstream_with_sen_reg_filter &
        time_period == max(time_period)) %>%
      ungroup() %>%
      arrange(`% of pupils`) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN provision`,
        `% of pupils`
      )

    return(DTise(
      msen_reg_bt,
      list(list(0, "desc"), list(3, "desc"))
    ) %>%
      formatRound("% of pupils", 1))
  })

  ### SEN of Children in Need ###

  ## SEN of Children in Need (LA/time) ##

  output$cin_la_time <- renderPlotly({
    cin_la_time_data <- cin_la %>%
      fsubset(geographic_level == "Local authority" &
        la_name == input$la_choice &
        `SEN Provision` == input$cin_la_filter)
    if (sum(is.na(cin_la_time_data$`Percentage of children`) > 0)) {
      caption <- "Some data points have been suppressed due to small numbers"
      margin_room <- 110
    } else {
      caption <- ""
      margin_room <- 0
    }

    cin_la_time <- ggplot(cin_la_time_data, aes(
      x = academic_year,
      y = `Percentage of children`,
      colour = social_care_group,
      group = social_care_group,
      text = paste0(
        "Social care group: ", social_care_group, "\n",
        "Academic year: ", academic_year
      )
    )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Academic year",
        colour = "Social Care Group"
      ) +
      scale_colour_manual(values = af_palette) +
      coord_cartesian(ylim = c(0, NA))

    cin_la_time %>%
      ggplotly(
        tooltip = c("y", "text")
      ) %>%
      save_plot_button_only() %>%
      add_annotations(
        x = 2.27,
        y = -0.37,
        text = caption,
        showarrow = FALSE,
        yref = "paper"
      ) %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE,
        margin = list(b = margin_room)
      )
  })

  output$cin_la_time_table <- renderDT({
    cin_la_tt <- cin_la %>%
      fsubset(geographic_level == "Local authority" &
        la_name == input$la_choice &
        `SEN Provision` == input$cin_la_filter) %>%
      arrange(
        `SEN Provision`,
        social_care_group,
        time_period
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN Provision`,
        `Social Care Group` = social_care_group,
        `Percentage of children`
      )

    return(DTise(
      cin_la_tt,
      list(list(2, "asc"), list(3, "asc"), list(0, "desc"))
    ) %>%
      formatRound("Percentage of children", 1))
  })

  ## SEN of Children in Need (LA/bench) ##

  output$cin_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(cin_la))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_cin %>%
      collapse::fsubset(`SEN Provision` == input$cin_la_filter &
        time_period == comparison_year &
        social_care_group == input$cin_sc_la_filter &
        geographic_level == "National") %>%
      mutate(outcome = `Percentage of children`)

    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    cin_la_bench_data <- cin_la %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `SEN Provision` == input$cin_la_filter &
        social_care_group == input$cin_sc_la_filter) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "none"
      }) %>%
      # Add ranks
      add_ranks(outcome = "Percentage of children")

    # Create rank statements to go on the X axis.
    cin_la_bench_data$rank_statement <- rank_statement_fun(cin_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )


    cin_la_bench <- cin_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `Percentage of children`),
        y = `Percentage of children`,
        fill = chosen_la,
        text = paste0(
          "Percentage of children: ", `Percentage of children`, "\n",
          la_name
        )
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        y = "Percentage of Children",
        x = cin_la_bench_data$rank_statement[cin_la_bench_data$la_name == input$la_choice],
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)


    cin_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$cin_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(cin_la))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    cin_la_bt <- cin_la %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `SEN Provision` == input$cin_la_filter &
        social_care_group == input$cin_sc_la_filter) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "none"
      }) %>%
      arrange(
        `SEN Provision`,
        social_care_group,
        time_period,
        desc(`Percentage of children`)
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN Provision`,
        `Social Care Group` = social_care_group,
        `Percentage of children`
      )

    return(DTise(
      cin_la_bt,
      list(list(2, "asc"), list(3, "asc"), list(0, "desc"), list(4, "desc"))
    ) %>%
      formatRound("Percentage of children", 1))
  })

  ## SEN of Children in Need (Region/time) ##

  output$cin_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      cin_reg_time_data <- cin_la %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          `SEN Provision` == input$cin_reg_filter)

      cin_reg_time <- ggplot(cin_reg_time_data, aes(
        x = academic_year,
        y = `Percentage of children`,
        colour = social_care_group,
        group = social_care_group,
        text = paste0(
          "Social care group: ", social_care_group, "\n",
          "Academic year: ", academic_year
        )
      )) +
        geom_line() +
        geom_point() +
        labs(
          x = "Academic year",
          colour = "Social Care Group"
        ) +
        scale_colour_manual(values = af_palette) +
        coord_cartesian(ylim = c(0, NA))
    } else {
      cin_reg_time_data <- cin_la %>%
        fsubset(geographic_level == "National" &
          `SEN Provision` == input$cin_reg_filter)

      cin_reg_time <- ggplot(cin_reg_time_data, aes(
        x = academic_year,
        y = `Percentage of children`,
        colour = social_care_group,
        group = social_care_group,
        text = paste0(
          "Social care group: ", social_care_group, "\n",
          "Academic year: ", academic_year
        )
      )) +
        geom_line() +
        geom_point() +
        labs(
          x = "Academic year",
          colour = "Social Care Group"
        ) +
        scale_colour_manual(values = af_palette) +
        coord_cartesian(ylim = c(0, NA))
    }

    cin_reg_time %>%
      ggplotly(
        tooltip = c("y", "text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$cin_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      cin_reg_time_data <- cin_la %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          `SEN Provision` == input$cin_reg_filter)
    } else {
      cin_reg_time_data <- cin_la %>%
        fsubset(geographic_level == "National" &
          `SEN Provision` == input$cin_reg_filter)
    }

    cin_reg_tt <- cin_reg_time_data %>%
      arrange(
        `SEN Provision`,
        social_care_group,
        time_period,
        desc(`Percentage of children`)
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Region` = region_name,
        `SEN Provision`,
        `Social Care Group` = social_care_group,
        `Percentage of children`
      )

    return(DTise(
      cin_reg_tt,
      list(list(2, "asc"), list(3, "asc"), list(0, "desc"), list(4, "desc"))
    ) %>%
      formatRound("Percentage of children", 1))
  })

  ## SEN of Children in Need (Region/bench) ##

  output$cin_reg_bench <- renderPlotly({
    national_average <- eng_cin %>%
      collapse::fsubset(`SEN Provision` == input$cin_reg_filter &
        time_period == max(time_period) &
        social_care_group == input$cin_sc_reg_filter &
        geographic_level == "National") %>%
      mutate(outcome = `Percentage of children`)

    cin_reg_bench_data <- cin_la %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN Provision` == input$cin_reg_filter &
        social_care_group == input$cin_sc_reg_filter &
        time_period == max(time_period)) %>%
      collapse::ftransform(
        chosen_region = ifelse(region_name == input$region_choice,
          yes = input$region_choice,
          no = "Other region"
        ),
        region_name = if_else(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
      )

    cin_reg_bench <- cin_reg_bench_data %>%
      ggplot(aes(
        x = fct_reorder(region_name, `Percentage of children`),
        y = `Percentage of children`,
        fill = chosen_region,
        text = paste0(
          "Percentage of children: ", `Percentage of children`, "\n",
          la_name
        )
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average) +
      labs(
        x = element_blank(),
        y = "Percentage of Children",
        fill = "Local Authority"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused) # we want them all to be purple for the England graph
      }

    cin_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$cin_reg_bench_table <- renderDT({
    cin_reg_bt <- cin_la %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN Provision` == input$cin_reg_filter &
        social_care_group == input$cin_sc_reg_filter &
        time_period == max(time_period)) %>%
      arrange(
        `SEN Provision`,
        social_care_group,
        time_period,
        desc(`Percentage of children`)
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Region` = region_name,
        `SEN Provision`,
        `Social Care Group` = social_care_group,
        `Percentage of children`
      )
    return(DTise(
      cin_reg_bt,
      list(list(2, "asc"), list(3, "asc"), list(0, "desc"), list(4, "desc"))
    ))
  })
  ### Specialist Provider types ####

  ## Provider types (LA/time)
  output$provider_types_la_time <- renderPlotly({
    ymax <- max(provider_types$`% of pupils (with SEN provision type)`)

    provider_types_la_time <- provider_types %>%
      filter(
        !(phase_type_grouping %in% c(
          "State-funded primary",
          "State-funded nursery",
          "State-funded secondary"
        )),
        la_name == input$la_choice,
        `Provision type` == input$provider_types_la_time_filter
      ) %>%
      ggplot(aes(
        x = academic_year,
        y = `% of pupils (with SEN provision type)`,
        group = phase_type_grouping,
        colour = phase_type_grouping,
        text = paste0(
          "% of pupils: ", `% of pupils (with SEN provision type)`, "\n",
          "School type: ", phase_type_grouping, "\n",
          "Academic year: ", academic_year
        )
      )) +
      geom_point() +
      geom_line() +
      labs(
        y = paste0("% of ", input$provider_types_la_time_filter),
        x = "Academic year",
        colour = "School type"
      ) +
      scale_colour_brewer(type = "qual", palette = 2) +
      scale_y_continuous(limits = c(0, ymax))


    provider_types_la_time %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$provider_types_la_time_table <- renderDT({
    types_la_tt <- provider_types %>%
      filter(
        !(phase_type_grouping %in% c(
          "State-funded primary",
          "State-funded nursery",
          "State-funded secondary"
        )),
        la_name == input$la_choice,
        `Provision type` == input$provider_types_la_time_filter
      ) %>%
      arrange(
        phase_type_grouping,
        academic_year
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        Group = phase_type_grouping,
        `% of pupils (with SEN provision type)`
      )

    return(DTise(
      types_la_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils (with SEN provision type)", 2))
  })

  ## Provider types (LA/bench)
  output$provider_types_la_bench <- renderPlotly({
    comparison <- provider_types_grouped %>%
      collapse::fsubset(la_name == input$la_choice)
    comparison_year <- max(comparison$academic_year, na.rm = T)

    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- provider_types_grouped_nat %>%
      ungroup() %>%
      fsubset(`Provision type` == input$provider_types_la_bench_filter &
        academic_year == comparison_year &
        geographic_level == "National") %>%
      mutate(outcome = `% in independent/AP/special`)

    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    provider_types_la_bench_data <- provider_types_grouped %>%
      fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        `Provision type` == input$provider_types_la_bench_filter &
        academic_year == comparison_year) %>%
      ungroup() %>% # The next line will fail if the tibble is still grouped
      mutate(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      fsubset(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "TEST"
      }) %>%
      # Add ranks
      add_ranks(outcome = "% in independent/AP/special")

    # Create rank statements to go on the X axis.
    provider_types_la_bench_data$rank_statement <- rank_statement_fun(provider_types_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )


    provider_types_la_bench <- provider_types_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `% in independent/AP/special`),
        y = `% in independent/AP/special`,
        fill = chosen_la,
        text = paste0(
          "% of pupils: ", `% in independent/AP/special`, "\n",
          la_name
        )
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input, nudge = 3) +
      labs(
        x = provider_types_la_bench_data$rank_statement[provider_types_la_bench_data$la_name == input$la_choice],
        y = "% of pupils",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused) +
      scale_y_continuous(limits = c(0, 100))

    provider_types_la_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$provider_types_la_bench_table <- renderDT({
    comparison <- provider_types_grouped %>%
      collapse::fsubset(la_name == input$la_choice)
    comparison_year <- max(comparison$academic_year, na.rm = T)
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    types_la_bt <- provider_types_grouped %>%
      fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        `Provision type` == input$provider_types_la_bench_filter &
        academic_year == comparison_year) %>%
      ungroup() %>% # The next line will fail if the tibble is still grouped
      fsubset(if (input$myregion_switch == TRUE) {
        region_name == chosen_region
      } else {
        region_name != "TEST"
      }) %>%
      arrange(`% in independent/AP/special`) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `Provision type`,
        `% in independent/AP/special`
      )

    return(DTise(
      types_la_bt,
      list(list(3, "desc"))
    )) %>%
      formatRound(columns = "% in independent/AP/special", digits = 1)
  })

  ## Provider types (region/time)
  output$provider_types_reg_time <- renderPlotly({
    ymax <- max(provider_types$`% of pupils (with SEN provision type)`)

    # req(input$level_choice)
    # if(input$level_choice == "Regions") req(input$region_choice)
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))


      provider_types_reg_time <- provider_types %>%
        fsubset(`Provision type` == input$provider_types_reg_time_filter &
          geographic_level == "Regional" &
          region_name == input$region_choice) %>%
        filter(!(phase_type_grouping %in% c(
          "State-funded primary",
          "State-funded nursery",
          "State-funded secondary"
        ))) %>% # Exclude mainstream settings
        ggplot(aes(
          x = academic_year,
          y = `% of pupils (with SEN provision type)`,
          group = phase_type_grouping,
          colour = phase_type_grouping,
          text = paste0(
            "% of pupils: ", `% of pupils (with SEN provision type)`, "\n",
            "School type: ", phase_type_grouping, "\n",
            "Academic year: ", academic_year
          )
        )) +
        geom_point() +
        geom_line() +
        labs(
          y = paste0("% of ", input$provider_types_reg_time_filter),
          x = "Academic year",
          colour = "School type"
        ) +
        scale_colour_brewer(type = "qual", palette = 2) +
        scale_y_continuous(limits = c(0, ymax))


      provider_types_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      provider_types_reg_time <- provider_types_nat %>%
        fsubset(`Provision type` == input$provider_types_reg_time_filter) %>%
        filter(!(phase_type_grouping %in% c(
          "State-funded primary",
          "State-funded nursery",
          "State-funded secondary"
        ))) %>% # Exclude mainstream settings
        ggplot(aes(
          x = academic_year,
          y = `% of pupils (with SEN provision type)`,
          group = phase_type_grouping,
          colour = phase_type_grouping,
          text = paste0(
            "% of pupils: ", `% of pupils (with SEN provision type)`, "\n",
            "School type: ", phase_type_grouping, "\n",
            "Academic year: ", academic_year
          )
        )) +
        geom_point() +
        geom_line() +
        labs(
          y = paste0("% of ", input$provider_types_reg_time_filter),
          x = "Academic year",
          colour = "School type"
        ) +
        scale_colour_brewer(type = "qual", palette = 2) +
        coord_cartesian(ylim = c(0, NA))


      provider_types_reg_time %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$provider_types_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      types_reg_tt <- provider_types %>%
        fsubset(`Provision type` == input$provider_types_reg_time_filter &
          geographic_level == "Regional" &
          region_name == input$region_choice) %>%
        filter(!(phase_type_grouping %in% c(
          "State-funded primary",
          "State-funded nursery",
          "State-funded secondary"
        ))) %>%
        arrange(
          phase_type_grouping,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          Group = phase_type_grouping,
          `% of pupils (with SEN provision type)`
        )
    } else {
      types_reg_tt <- provider_types %>%
        fsubset(`Provision type` == input$provider_types_reg_time_filter &
          geographic_level == "Regional" &
          region_name == input$region_choice) %>%
        filter(!(phase_type_grouping %in% c(
          "State-funded primary",
          "State-funded nursery",
          "State-funded secondary"
        ))) %>%
        arrange(
          phase_type_grouping,
          academic_year
        ) %>%
        fmutate(region_name = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          Group = phase_type_grouping,
          `% of pupils (with SEN provision type)`
        )
    }
    return(DTise(
      types_reg_tt,
      list(list(2, "asc"), list(0, "desc"))
    ) %>%
      formatRound("% of pupils (with SEN provision type)", 2))
  })

  ## Provider types (region/bench)
  output$provider_types_reg_bench <- renderPlotly({
    national_average <- provider_types_grouped_nat %>%
      ungroup() %>%
      fsubset(academic_year == max(academic_year) &
        `Provision type` == input$provider_types_reg_bench_filter &
        geographic_level == "National") %>%
      mutate(outcome = `% in independent/AP/special`)

    ymax <- max(provider_types_grouped_nat$`% in independent/AP/special`)


    provider_types_reg_bench_basic <- provider_types_grouped %>%
      collapse::fsubset(geographic_level == "Regional" &
        academic_year == max(academic_year) &
        `Grouped provider type` == "Independent, alternative provision or special school") %>%
      fsubset(`Provision type` == input$provider_types_reg_bench_filter)

    if (input$level_choice == "Regions" & is.character(input$region_choice)) {
      provider_types_reg_bench_data <- provider_types_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region = case_when(
            region_name == input$region_choice &
              input$level_choice == "Regions" ~
              input$region_choice,
            input$level_choice == "England" ~
              "English regions",
            TRUE ~ "Other region"
          ),
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ungroup()
    } else { # The graph will crash R if this tibble is not ungrouped
      provider_types_reg_bench_data <- provider_types_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region = "English regions",
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        ) %>%
        ungroup()
    } # The graph will crash R if this tibble is not ungrouped


    provider_types_reg_bench <- provider_types_reg_bench_data %>%
      ggplot(aes(
        x = fct_reorder(region_name, `% in independent/AP/special`),
        y = `% in independent/AP/special`,
        text = region_name,
        text = paste0(
          "% of pupils: ", `% in independent/AP/special`, "\n",
          region_name
        )
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average) +
      labs(
        x = "Regions in England",
        y = "% of pupils",
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        ),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }

    provider_types_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$provider_types_reg_bench_table <- renderDT({
    types_reg_bt <- provider_types_grouped %>%
      collapse::fsubset(geographic_level == "Regional" &
        academic_year == max(academic_year) &
        `Grouped provider type` == "Independent, alternative provision or special school") %>%
      fsubset(`Provision type` == input$provider_types_reg_bench_filter) %>%
      ungroup() %>%
      arrange(`% in independent/AP/special`) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `Provision type`,
        `% in independent/AP/special`
      )

    return(DTise(
      types_reg_bt,
      list(list(3, "desc"))
    ) %>%
      formatRound(columns = "% in independent/AP/special", digits = 1))
  })

  # ==========================================
  ## Generic functions for regional bench view
  # ==========================================

  # Generic bar chart plot function
  plot_reg_bench <- function(data, outcome_var, hover_label, label_y, region_choice_validated, national_average = NULL) {
    data <- data %>%
      mutate(
        chosen_region = case_when(
          region_name == region_choice_validated & input$level_choice == "Regions" ~ region_choice_validated,
          input$level_choice == "England" ~ "English regions",
          TRUE ~ "Other region"
        )
      ) %>%
      ggplot(aes(
        x = fct_reorder(region_name, outcome_var),
        y = outcome_var,
        text = paste0(
          Region, "\n", # always state region then line break
          hover_label, ":", outcome_var
        ),
        fill = chosen_region,
        label = ""
      )) +
      geom_col() +
      labs(
        x = "Regions in England",
        y = label_y,
        fill = "Region"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
        legend.position = "none"
      ) +
      if (input$level_choice == "Regions") {
        scale_fill_manual(values = c("Other region" = not_focused), na.value = focused)
      } else {
        scale_fill_manual(values = c("English regions" = focused), na.value = focused)
      }

    # Conditional English average line added if national_average created
    ## It doesn't make sense to add this line for national counts,
    if (!is.null(national_average) && is.data.frame(national_average)) {
      print("Yes, national_average exists and is a data frame.")
      # Include the code related to national_average
      data <- data +
        add_england_line_bench(national_average) +
        add_england_label_bench_reg(national_average, nudge = 3)
    } else {
      print("No valid national_average provided or it is not a data frame. Skipping related code.")
    }

    data <- ggplotly(data, tooltip = c("text")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )

    return(data) # plot this
  }

  # Generic DT table plot function
  table_bench_time <- function(data, var_filter, outcome_var, digits = 1) {
    data <- data %>%
      mutate(Region = region_name) %>%
      arrange(
        desc(`Academic year`),
        !!var_filter
      ) %>%
      select(
        `Academic year`,
        Region,
        !!var_filter,
        !!outcome_var
      )
    return(DTise(data, list(order = c(list(0, "desc"), list(3, "desc")))) %>%
      formatRound(4, digits)) # the outcome variable column will always be column 4 due to the select statement; avoiding having to mess around with non-standard evaluation
  }

  # ==========================
  # Alternative Provision tab (note to coders - don't call it AP in section headings, AP isn't searchable)
  # ==========================

  #--------------------------
  # AP counts
  #--------------------------

  # counts (time view) plot
  plot_ap_counts_time <- function(data) {
    # Create custom breaks for x-axis
    ap_counts_yrs <- ap_counts %>%
      arrange(time_period) %>%
      select(`Academic year`) %>%
      unique()

    data <- data %>%
      mutate(Total = round(Total, 0)) %>%
      ggplot(aes(
        x = `Academic year`,
        y = Total,
        group = `Type of AP`,
        fill = `Type of AP`,
        label = paste0(Total)
      )) +
      geom_col(position = "stack", show.legend = FALSE) +
      labs(
        x = "Academic year",
        fill = element_blank(),
        y = "Total pupils",
        label = ""
      ) +
      scale_fill_manual(values = ap_palette) +
      scale_x_discrete(limits = ap_counts_yrs$`Academic year`) +
      scale_y_continuous(labels = label_number())

    data <- ggplotly(data, tooltip = c("text", "y", "x", "colour")) %>%
      save_plot_button_only() %>%
      layout(
        showlegend = FALSE,
        dragmode = FALSE,
        yaxis = list(autorange = FALSE)
      )
  }

  # LA counts (time view) - table generic function
  table_ap_counts_time <- function(data, var) {
    data <- data %>%
      mutate(Region = region_name) %>%
      arrange(desc(`Academic year`), `Type of AP`) %>%
      select(
        `Academic year`,
        !!ensym(var),
        `Type of AP`,
        `Total pupils` = Total
      )
    return(DTise(data, c(list(0, "desc"), list(2, "asc"))) %>%
      formatRound(columns = "Total pupils", digits = 0))
  }


  ## LA time
  output$ap_counts_la_time <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ap_counts))
    ap_counts_la_type <- ap_counts %>%
      fsubset(la_name == input$la_choice &
        `Type of AP` == input$ap_counts_la_time_filter)

    if (nrow(ap_counts_la_type) > 0) {
      ap_counts_la_type <- ap_counts_la_type %>%
        plot_ap_counts_time()
    } else {
      ap_counts_la_type %>% validate_if_no_la_data()
    }
  })

  output$ap_counts_la_time_table <- renderDT({
    ap_counts_la_typet <- ap_counts %>%
      fsubset(la_name == input$la_choice &
        `Type of AP` == input$ap_counts_la_time_filter)

    if (nrow(ap_counts_la_typet) > 0) {
      ap_counts_la_typet <- ap_counts_la_typet %>%
        table_ap_counts_time(`Local authority`)
    } else {
      ap_counts_la_typet %>% validate_if_no_la_data()
    }
  })

  ## Region time plot
  output$ap_counts_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_counts_reg_type <- ap_counts %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          `Type of AP` == input$ap_counts_reg_time_filter) %>%
        plot_ap_counts_time()
    } else {
      ap_counts_reg_type <- ap_counts %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == input$ap_counts_reg_time_filter) %>%
        plot_ap_counts_time()
    }

    return(ap_counts_reg_type) # Return plot
  })

  output$ap_counts_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_counts_reg_type <- ap_counts %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          `Type of AP` == input$ap_counts_reg_time_filter) %>%
        table_ap_counts_time(`Region`)
    } else {
      ap_counts_reg_type <- ap_counts %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == input$ap_counts_reg_time_filter) %>%
        table_ap_counts_time(`Region`)
    }
  })

  ## AP counts (LA/bench)
  output$ap_counts_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ap_counts))
    if (nrow(fsubset(ap_counts, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      ap_counts_la_bench_data <- ap_counts %>%
        collapse::fsubset(geographic_level == "Local authority" &
          !(la_name %in% small_LAs) &
          `Type of AP` == input$ap_counts_la_bench_filter &
          time_period == comparison_year) %>%
        collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
          yes = input$la_choice,
          no = "Other LA"
        )) %>%
        filter(if (input$myregion_switch == TRUE) {
          region_name == region_name[la_name == input$la_choice]
        } else {
          region_name != "none"
        }) %>%
        add_ranks(outcome = "total")

      ap_counts_la_bench_data$rank_statement <- rank_statement_fun(ap_counts_la_bench_data,
        rank_col = rank,
        name_col = la_name,
        time_period = academic_year
      )

      chosen_la <- ap_counts_la_bench_data %>% filter(la_name == input$la_choice)

      if (nrow(chosen_la) > 0) {
        ap_counts_la_bench <- ggplot(ap_counts_la_bench_data, aes(
          x = fct_reorder(la_name, Total),
          y = Total,
          text = la_name,
          fill = chosen_la,
          label = ""
        )) +
          geom_col(position = "stack", show.legend = FALSE) +
          geom_text(
            size = 2.5,
            position = position_stack(vjust = 0.5)
          ) +
          labs(
            x = ap_counts_la_bench_data$rank_statement[ap_counts_la_bench_data$la_name == input$la_choice],
            y = "Total pupils",
            fill = "Local Authority"
          ) +
          theme(axis.text.x = element_blank()) +
          scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

        ap_counts_la_bench %>%
          ggplotly(
            tooltip = c("text", "y"),
          ) %>%
          save_plot_button_only() %>%
          layout(
            legend = list(orientation = "h", y = -0.2),
            dragmode = FALSE
          )
      } else {
        chosen_la %>% validate_if_no_la_data()
      }
    }
  })

  output$ap_counts_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ap_counts))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ap_counts_la_bt <- ap_counts %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `Type of AP` == input$ap_counts_la_bench_filter)

    chosen_la <- ap_counts_la_bt %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_counts_la_bt <- ap_counts_la_bt %>%
        filter(if (input$myregion_switch == TRUE) {
          region_name == chosen_region
        } else {
          region_name != "none"
        }) %>%
        arrange(desc(`Academic year`), desc(Total)) %>%
        fselect(
          `Academic year`,
          `Local authority`,
          `Type of AP`,
          `Total pupils` = Total
        ) %>%
        mutate(`Total pupils` = as.numeric(`Total pupils`))

      return(DTise(ap_counts_la_bt, list(0, 4)) %>%
        formatRound("Total pupils", 0)) # this has already been ordered into desc
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })


  ## AP counts (regional/bench)
  output$ap_counts_reg_bench <- renderPlotly({
    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    ap_counts_reg_bench_s1 <- ap_counts %>%
      collapse::fsubset(geographic_level == "Regional" &
        `Type of AP` == input$ap_counts_reg_bench_filter &
        time_period == max(time_period))

    ap_counts_reg_bench <- plot_reg_bench(
      data = ap_counts_reg_bench_s1,
      outcome_var = ap_counts_reg_bench_s1$Total,
      label_y = "Total pupils",
      region_choice_validated = paste0(region_choice_validated),
      hover_label = "Total pupils"
    )
  })

  output$ap_counts_reg_bench_table <- renderDT({
    ap_counts_reg_type <- ap_counts %>%
      collapse::fsubset(geographic_level == "Regional" &
        `Type of AP` == input$ap_counts_reg_bench_filter &
        time_period == max(time_period)) %>%
      table_bench_time(
        var_filter = quote(`Type of AP`),
        outcome_var = quote(Total),
        digits = 0
      )
  })


  # =========================
  # AP characteristics
  # =========================
  plot_ap_chars_time <- function(data) {
    # Create year breaks
    ap_chars_yrs <- ap_characteristics %>%
      arrange(time_period) %>%
      select(`Academic year`) %>%
      unique()

    data <- data %>%
      ggplot(aes(
        x = `Academic year`,
        y = `% of pupils`,
        group = `Characteristic`,
        fill = `Characteristic`,
        label = " ",
        text = paste0(
          "Academic year: ", `Academic year`, "\n",
          "% of pupils: ", `% of pupils`, "\n",
          "Characteristic: ", `Characteristic`, "\n"
        )
      )) +
      geom_col(show.legend = FALSE) +
      labs(
        x = "Academic year",
        fill = "",
        y = "% of pupils"
      ) +
      theme(legend.position = "bottom") +
      scale_y_continuous(limits = c(0, 101), labels = scales::percent_format(scale = 1)) +
      scale_x_discrete(limits = ap_chars_yrs$`Academic year`) +
      scale_fill_manual(values = char_gradient_ap)

    data %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  }


  table_ap_chars_time <- function(data, var) {
    data <- data %>%
      arrange(desc(`Academic year`), `Type of AP`, `Characteristic`) %>%
      select(
        `Academic year`,
        !!ensym(var), # `Local authority` or `Region`
        `Type of AP`,
        `Characteristic`,
        `% of pupils`,
        `Total pupils`
      )
    return(DTise(data, c(list(0, "desc"), list(2, "asc"), list(4, "asc"))) %>%
      formatRound("Total pupils", 0) %>%
      formatRound("% of pupils", 2))
  }

  output$ap_characteristics_la_time <- renderPlotly({
    ap_characteristics_la_time <- ap_characteristics %>%
      fsubset(
        la_name == input$la_choice &
          pupil_characteristic == input$ap_characteristics_la_time_filter &
          `Type of AP` == input$ap_characteristics_la_time_filter_type
      )

    if (nrow(ap_characteristics_la_time) > 0) {
      ap_characteristics_la_time <- ap_characteristics_la_time %>%
        plot_ap_chars_time()
    } else {
      ap_characteristics_la_time %>% validate_if_no_la_data()
    }
  })

  output$ap_characteristics_la_time_table <- renderDT({
    ap_characteristics_la_typet <- ap_characteristics %>%
      fsubset(
        la_name == input$la_choice &
          pupil_characteristic == input$ap_characteristics_la_time_filter &
          `Type of AP` == input$ap_characteristics_la_time_filter_type
      )

    if (nrow(ap_characteristics_la_typet) > 0) {
      ap_characteristics_la_typet <- ap_characteristics_la_typet %>%
        table_ap_chars_time(`Local authority`)
    } else {
      ap_characteristics_la_typet %>% validate_if_no_la_data()
    }
  })


  output$ap_characteristics_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_characteristics_reg_type <- ap_characteristics %>%
        fsubset(
          geographic_level == "Regional" &
            `Region` == input$region_choice &
            pupil_characteristic == input$ap_characteristics_reg_time_filter &
            `Type of AP` == input$ap_characteristics_reg_time_filter_type
        ) %>%
        plot_ap_chars_time()
    } else {
      ap_characteristics_reg_type <- ap_characteristics %>%
        fsubset(
          geographic_level == "National" &
            pupil_characteristic == input$ap_characteristics_reg_time_filter &
            `Type of AP` == input$ap_characteristics_reg_time_filter_type
        ) %>%
        plot_ap_chars_time()
    }
  })

  output$ap_characteristics_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_characteristics_reg_type <- ap_characteristics %>%
        fsubset(
          geographic_level == "Regional" &
            `Region` == input$region_choice &
            pupil_characteristic == input$ap_characteristics_reg_time_filter &
            `Type of AP` == input$ap_characteristics_reg_time_filter_type
        ) %>%
        table_ap_chars_time(Region)
    } else {
      ap_characteristics_reg_type <- ap_characteristics %>%
        fsubset(
          geographic_level == "National" &
            pupil_characteristic == input$ap_characteristics_reg_time_filter &
            `Type of AP` == input$ap_characteristics_reg_time_filter_type
        ) %>%
        table_ap_chars_time(Region)
    }
  })

  # AP characteristics (LA/bench)
  output$ap_characteristics_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ap_characteristics))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ap_characteristics_la_bench <- ap_characteristics %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year &
        pupil_characteristic == input$ap_characteristics_la_bench_filter &
        `Type of AP` == input$ap_characteristics_la_bench_filter_type) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      fsubset(`Region` == chosen_region)

    chosen_la <- ap_characteristics_la_bench %>% filter(la_name == input$la_choice)

    y_label <- paste0(
      "Local authorities in ",
      chosen_region,
      " in ",
      max(ap_characteristics$academic_year)
    )

    y_label <- format_axis_label(y_label)

    if (nrow(chosen_la) > 0) {
      ap_characteristics_la_bench <- ap_characteristics_la_bench %>%
        ggplot(aes(
          x = `% of pupils`,
          y = la_name,
          group = `Characteristic`,
          fill = `Characteristic`,
          label = " ",
          text = paste0(
            "Academic year: ", `Academic year`, "\n",
            "% of pupils: ", `% of pupils`, "\n",
            "Characteristic: ", `Characteristic`, "\n"
          )
        )) +
        geom_col(show.legend = FALSE) +
        labs(
          y =
            paste0(y_label),
          x = "% of pupils",
          fill = "Local Authority"
        ) +
        theme(legend.position = "bottom") +
        scale_x_continuous(limits = c(0, 102), labels = scales::percent_format(scale = 1)) +
        scale_fill_manual(values = char_gradient_ap)

      ap_characteristics_la_bench %>%
        ggplotly(
          tooltip = c("text"),
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(
            orientation = "h",
            traceorder = "reversed",
            y = -0.2
          ),
          dragmode = FALSE
        )
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })


  output$ap_characteristics_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ap_characteristics))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ap_characteristics_la_bt <- ap_characteristics %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year &
        pupil_characteristic == input$ap_characteristics_la_bench_filter &
        `Type of AP` == input$ap_characteristics_la_bench_filter_type)

    chosen_la <- ap_characteristics_la_bt %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_characteristics_la_bt <- ap_characteristics_la_bt %>%
        filter(if (input$myregion_switch == TRUE) {
          `Region` == chosen_region
        } else {
          `Region` != "none"
        }) %>%
        fselect(
          `Academic year`,
          `Local authority` = la_name,
          `Type of AP`,
          `Characteristic`,
          `% of pupils`,
          `Total pupils`
        ) %>%
        arrange(desc(`Academic year`), `Characteristic`, desc(`% of pupils`))

      return(DTise(
        ap_characteristics_la_bt,
        c(list(0, "desc"), list(3, "asc"), list(5, "desc"))
      ) %>%
        formatRound("% of pupils", 2) %>%
        formatRound("Total pupils", 0))
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })




  ## AP chars (regional/bench)
  output$ap_characteristics_reg_bench <- renderPlotly({
    ap_characteristics_reg_bench <- ap_characteristics %>%
      collapse::fsubset(geographic_level == "Regional" &
        pupil_characteristic == input$ap_characteristics_reg_bench_filter &
        `Type of AP` == input$ap_characteristics_reg_bench_filter_type &
        time_period == max(time_period)) %>%
      ggplot(aes(
        x = `% of pupils`,
        y = `Region`,
        group = `Characteristic`,
        fill = `Characteristic`,
        label = " ",
        text = paste0(
          "Academic year: ", `Academic year`, "\n",
          "% of pupils: ", `% of pupils`, "\n",
          "Characteristic: ", `Characteristic`, "\n"
        )
      )) +
      geom_col(show.legend = FALSE) +
      labs(
        y = "",
        x = "% of pupils",
        fill = "`Region`"
      ) +
      theme(legend.position = "bottom") +
      scale_x_continuous(limits = c(0, 102), labels = scales::percent_format(scale = 1)) +
      scale_fill_manual(values = char_gradient_ap)

    ap_characteristics_reg_bench %>%
      ggplotly(
        tooltip = c("text"),
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(
          orientation = "h",
          traceorder = "reversed",
          y = -0.2
        ),
        dragmode = FALSE
      )
  })

  output$ap_characteristics_reg_bench_table <- renderDT({
    ap_characteristics_reg_bt <- ap_characteristics %>%
      collapse::fsubset(geographic_level == "Regional" &
        pupil_characteristic == input$ap_characteristics_reg_bench_filter &
        `Type of AP` == input$ap_characteristics_reg_bench_filter_type &
        time_period == max(time_period)) %>%
      fselect(
        `Academic year`,
        `Region` = `Region`,
        `Type of AP`,
        `Characteristic`,
        `% of pupils`,
        `Total pupils`
      ) %>%
      fmutate(`Total pupils` = as.character(round(`Total pupils`, 0))) %>%
      arrange(desc(`Academic year`), `Characteristic`, desc(`% of pupils`))

    return(DTise(
      ap_characteristics_reg_bt,
      c(list(0, "desc"), list(3, "asc"), list(5, "desc"))
    ) %>%
      formatRound("% of pupils", 2) %>%
      formatRound("Total pupils", 0))
  })

  # =====================================
  # State-funded AP Absences
  # =====================================
  # Create custom breaks and labels - so line graph connects (nb: this will update when new years of data are added, so no hard coding)
  ap_absences_cust_breaks_labels <- sf_ap_absence %>%
    pivot_wider(names_from = academic_year, values_from = Percentage) %>%
    mutate(`2019/20` = NA) %>% # This is the Covid-affected year
    pivot_longer(
      cols = contains("/"),
      names_to = "academic_year",
      values_to = "Percentage"
    ) %>%
    select(academic_year) %>%
    AY_to_date(academic_year) %>%
    unique()


  plot_ap_absences_time <- function(data) {
    measure_name <- unique(data$`Absence measure`)[1]

    data <- data %>%
      AY_to_date(academic_year) %>%
      ggplot(aes(
        x = AY_date,
        y = Percentage,
        group = `Absence measure`,
        colour = `Absence measure`,
        text = paste0(
          "Academic year: ", paste0(ay_date_to_ay(`AY_date`)), "\n",
          "%: ", `Percentage`, "\n",
          "Absence measure: ", `Absence measure`
        )
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "%",
        x = "Academic year",
        colour = "Absence measure"
      ) +
      scale_colour_manual(values = af_gradient) +
      scale_x_date(
        breaks = ap_absences_cust_breaks_labels$AY_date,
        labels = ap_absences_cust_breaks_labels$academic_year
      ) +
      scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1))

    data %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  }

  table_ap_absences_time <- function(data, var) {
    data <- data %>%
      mutate(Region = Region, `Local authority` = la_name) %>%
      arrange(desc(time_period), `Absence measure`) %>%
      select(
        `Academic Year` = academic_year,
        !!ensym(var), # `Local authority` or `Region`
        `Grand total enrolments` = total_enrolments,
        `Absence measure`,
        `Percentage`
      ) %>%
      arrange(desc(`Academic Year`), `Absence measure`)

    return(DTise(data, c(list(0, "desc"), list(3, "asc"))) %>%
      formatRound("Percentage", 2) %>%
      formatRound("Grand total enrolments", 0))
  }



  ##   State-funded AP Absences (LA/time)
  output$ap_absences_la_time <- renderPlotly({
    ap_absences_la_time <- sf_ap_absence %>%
      fsubset(la_name == input$la_choice)

    if (nrow(ap_absences_la_time) > 0) {
      ap_absences_la_time <- ap_absences_la_time %>%
        plot_ap_absences_time()
    } else {
      ap_absences_la_time %>% validate_if_no_la_data()
    }
  })

  output$ap_absences_la_time_table <- renderDT({
    ap_absences_la_tt <- sf_ap_absence %>%
      fsubset(la_name == input$la_choice)

    if (nrow(ap_absences_la_tt) > 0) {
      ap_absences_la_tt <- ap_absences_la_tt %>%
        table_ap_absences_time(`Local authority`)
    } else {
      ap_absences_la_tt %>% validate_if_no_la_data()
    }
  })


  ##   State-funded AP Absences (regional/time)
  output$ap_absences_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_absences_reg_time <- sf_ap_absence %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice) %>%
        plot_ap_absences_time()
    } else {
      ap_absences_reg_time <- sf_ap_absence %>%
        fsubset(geographic_level == "National") %>%
        plot_ap_absences_time()
    }
  })


  output$ap_absences_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_absences_reg_time <- sf_ap_absence %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice) %>%
        table_ap_absences_time(`Region`)
    } else {
      ap_absences_reg_time <- sf_ap_absence %>%
        fsubset(geographic_level == "National") %>%
        table_ap_absences_time(`Region`)
    }
  })


  # Benchmarking SF AP Absences
  output$ap_absences_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, sf_ap_absence)
    chosen_region <- ifelse(grepl("London$", la_region_lookup$region[la_region_lookup$la_name == input$la_choice]),
      yes = "London",
      no = la_region_lookup$region[la_region_lookup$la_name == input$la_choice]
    )

    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- sf_ap_absence %>%
      collapse::fsubset(`Absence measure` == input$ap_absences_la_filter &
        time_period == comparison_year &
        geographic_level == "National") %>%
      mutate(
        outcome = Percentage,
        label = "England average"
      )

    # Wrangle data into ggplot object
    ap_absences_la_bench_data <- sf_ap_absence %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        `Absence measure` == input$ap_absences_la_filter &
        time_period == comparison_year)

    measure_name <- unique(ap_absences_la_bench_data$`Absence measure`)[1]
    measure_name <- format_axis_label(measure_name)

    chosen_la <- ap_absences_la_bench_data %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_absences_la_bench_data <- ap_absences_la_bench_data %>%
        collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
          yes = input$la_choice,
          no = "Other LA"
        )) %>%
        filter(if (input$myregion_switch == TRUE) {
          Region == chosen_region
        } else {
          Region != "none"
        }) %>%
        add_ranks(outcome = "Percentage")

      # Create rank statements to go on the X axis.
      ap_absences_la_bench_data$rank_statement <- rank_statement_fun(ap_absences_la_bench_data,
        rank_col = rank,
        name_col = la_name,
        time_period = academic_year
      )

      ap_absences_la_bench <- ap_absences_la_bench_data %>%
        ggplot(aes(fct_reorder(la_name, `Percentage`),
          y = `Percentage`,
          text = la_name,
          fill = chosen_la,
          label = `Percentage` # for hover label
        )) +
        geom_col() +
        add_england_line_bench(national_average) +
        add_england_label_bench(national_average, input = input) +
        labs(
          x = ap_absences_la_bench_data$rank_statement[ap_absences_la_bench_data$la_name == input$la_choice],
          y = paste0(measure_name),
          fill = "Local Authority"
        ) +
        theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

      ap_absences_la_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line() %>%
        use_horizontal_legend()
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })

  output$ap_absences_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, sf_ap_absence)
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]


    ap_absences_lab <- sf_ap_absence %>%
      filter(if (input$myregion_switch == TRUE) {
        Region == chosen_region
      } else {
        Region != "none"
      })

    ap_absences_lab <- ap_absences_lab %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        `Absence measure` == input$ap_absences_la_filter &
        time_period == comparison_year)

    chosen_la <- ap_absences_lab %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_absences_lab <- ap_absences_lab %>%
        arrange(desc(`Percentage`)) %>%
        fmutate(total_enrolments = as.character(round(total_enrolments, 0))) %>%
        fselect(
          `Academic Year` = academic_year,
          `Local authority` = la_name,
          `Grand total enrolments in LA` = total_enrolments,
          `Absence measure`,
          `Percentage`
        ) %>%
        return(DTise(
          ap_absences_lab,
          c(list(3, "desc"))
        ) %>%
          formatRound("Percentage", 2) %>%
          formatRound("Grand total enrolments in LA", 0))
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })


  ## AP absences (regional/bench)
  output$ap_absences_reg_bench <- renderPlotly({
    national_average <- sf_ap_absence %>%
      collapse::fsubset(geographic_level == "National" &
        `Absence measure` == input$ap_absences_reg_filter &
        time_period == max(time_period)) %>%
      mutate(outcome = Percentage)

    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    ap_absences_reg_bench_s1 <- sf_ap_absence %>%
      collapse::fsubset(geographic_level == "Regional" &
        `Absence measure` == input$ap_absences_reg_filter &
        time_period == max(time_period))

    measure_name <- unique(ap_absences_reg_bench_s1$`Absence measure`)[1]
    measure_name <- format_axis_label(measure_name)

    ap_absences_reg_bench <- plot_reg_bench(
      data = ap_absences_reg_bench_s1,
      outcome_var = ap_absences_reg_bench_s1$Percentage,
      label_y = paste0(measure_name),
      region_choice_validated = paste0(region_choice_validated),
      hover_label = "%",
      national_average = national_average
    )
  })

  output$ap_absences_reg_bench_table <- renderDT({
    ap_absences_reg_type <- sf_ap_absence %>%
      collapse::fsubset(geographic_level %in% c("Regional", "National") &
        `Absence measure` == input$ap_absences_reg_filter &
        time_period == max(time_period)) %>%
      table_bench_time(
        var_filter = quote(`Absence measure`),
        outcome_var = quote(Percentage)
      )
  })



  # ================================
  # Ofsted outcome (school)
  # ================================

  plot_ap_ofsted_time <- function(data) {
    measure_name <- unique(data$Measure)[1]

    data <- data %>%
      ggplot(aes(
        x = Year,
        y = Value,
        group = `Overall effectiveness`,
        fill = `Overall effectiveness`,
        label = " ",
        text = paste0(
          "Year: ", `Year`, "\n",
          "Ofsted effectiveness rating: ", `Overall effectiveness`, "\n",
          measure_name, ": ", `Value`
        )
      )) +
      geom_col(position = "stack") +
      geom_text(
        size = 3,
        position = position_stack(vjust = 0.5)
      ) +
      theme(legend.position = "bottom") +
      #  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
      scale_fill_manual(values = c(
        "Outstanding" = "#0b2841",
        "Good" = "#12436d",
        "Requires improvement" = "#41698a",
        "Inadequate" = "#718ea7",
        "NULL" = "black"
      )) +
      labs(
        x = "Year",
        fill = "Ofsted outcome",
        y = paste0(measure_name)
      )

    data %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  }

  table_ap_ofsted_time <- function(data, var) {
    data <- data %>%
      mutate(`Local authority` = la_name) %>%
      select(
        `Year`,
        !!ensym(var), # `Local authority` or `Region`
        `school_type`,
        `Overall effectiveness`,
        `Measure`,
        `Value`
      ) %>%
      arrange(desc(Year), Measure)
    return(DTise(data, c(list(3, "asc"), list(0, "desc"))))
  }


  # Time view
  output$ap_ofsted_la_time <- renderPlotly({
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ap_ofsted_la_time <- ap_ofsted_schl %>%
      fsubset(la_name == input$la_choice &
        school_type == input$ap_ofsted_la_time_filter &
        Measure == input$ap_ofsted_la_time_filter_two)

    if (nrow(ap_ofsted_la_time) > 0) {
      ap_ofsted_la_time <- ap_ofsted_la_time %>% plot_ap_ofsted_time()
    } else {
      ap_ofsted_la_time %>% validate_if_no_la_data()
    }
  })

  output$ap_ofsted_la_time_table <- renderDT({
    ap_ofsted_la_tt <- ap_ofsted_schl %>%
      fsubset(la_name == input$la_choice &
        school_type == input$ap_ofsted_la_time_filter &
        Measure == input$ap_ofsted_la_time_filter_two)

    if (nrow(ap_ofsted_la_tt) > 0) {
      ap_ofsted_la_tt <- ap_ofsted_la_tt %>% table_ap_ofsted_time(`Local authority`)
    } else {
      ap_ofsted_la_tt %>% validate_if_no_la_data()
    }
  })



  output$ap_ofsted_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_ofsted_reg_time <- ap_ofsted_schl %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          school_type == input$ap_ofsted_reg_time_filter &
          Measure == input$ap_ofsted_reg_time_filter_two) %>%
        plot_ap_ofsted_time()
    } else {
      ap_ofsted_reg_time <- ap_ofsted_schl %>%
        fsubset(geographic_level == "National" &
          school_type == input$ap_ofsted_reg_time_filter &
          Measure == input$ap_ofsted_reg_time_filter_two) %>%
        plot_ap_ofsted_time()
    }
  })



  output$ap_ofsted_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_ofsted_reg_type <- ap_ofsted_schl %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          school_type == input$ap_ofsted_reg_time_filter &
          Measure == input$ap_ofsted_reg_time_filter_two) %>%
        table_ap_ofsted_time(Region)
    } else {
      ap_ofsted_reg_type <- ap_ofsted_schl %>%
        fsubset(geographic_level == "National" &
          school_type == input$ap_ofsted_reg_time_filter &
          Measure == input$ap_ofsted_reg_time_filter_two) %>%
        table_ap_ofsted_time(Region)
    }
  })


  # AP Ofsted (LA/bench)
  output$ap_ofsted_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes_ofsted(input = input$la_choice, as.data.frame(ap_ofsted_schl))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    measure_name <- paste0(input$ap_ofsted_la_bench_filter_two)

    ap_ofsted_la_bench <- ap_ofsted_schl %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        Year == comparison_year &
        school_type == input$ap_ofsted_la_bench_filter &
        Measure == input$ap_ofsted_la_bench_filter_two)

    chosen_la <- ap_ofsted_la_bench %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_ofsted_la_bench <- ap_ofsted_la_bench %>%
        collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
          yes = input$la_choice,
          no = "Other LA"
        )) %>%
        filter(Region == chosen_region) %>%
        ggplot(aes(
          x = Value,
          y = fct_reorder(la_name, Value),
          group = `Overall effectiveness`,
          fill = `Overall effectiveness`,
          label = " ",
          text = paste0(
            "Year: ", `Year`, "\n",
            "LA: ", `la_name`, "\n",
            "Ofsted effectiveness rating: ", `Overall effectiveness`, "\n",
            measure_name, ": ", `Value`
          )
        )) +
        geom_col(show.legend = FALSE) +
        labs(
          y =
            paste0(
              "Local authorities in ",
              chosen_region,
              " (",
              max(ap_ofsted_schl$Year), ")"
            ),
          x = paste0(measure_name),
          fill = "Local Authority"
        ) +
        theme(legend.position = "bottom") +
        scale_fill_manual(values = char_gradient_ap)

      ap_ofsted_la_bench %>%
        ggplotly(
          tooltip = c("text")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(
            orientation = "h",
            traceorder = "reversed",
            y = -0.2
          ),
          dragmode = FALSE
        )
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })

  output$ap_ofsted_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes_ofsted(input = input$la_choice, as.data.frame(ap_ofsted_schl))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ap_ofsted_la_bt <- ap_ofsted_schl %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        Year == comparison_year &
        school_type == input$ap_ofsted_la_bench_filter &
        Measure == input$ap_ofsted_la_bench_filter_two)

    chosen_la <- ap_ofsted_la_bt %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_ofsted_la_bt <- ap_ofsted_la_bt %>%
        filter(if (input$myregion_switch == TRUE) {
          Region == chosen_region
        } else {
          Region != "none"
        }) %>%
        fselect("Year",
          "Local Authority" = "la_name",
          "School Type" = "school_type",
          "Overall effectiveness",
          "Measure",
          "Value"
        ) %>%
        arrange(desc(Year), Measure)

      return(DTise(
        ap_ofsted_la_bt,
        c(list(4, "asc"), list(3, "asc"), list(2, "asc"))
      ))
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })

  output$ap_ofsted_reg_bench <- renderPlotly({
    ap_ofsted_reg_bench <- ap_ofsted_schl %>%
      collapse::fsubset(geographic_level == "Regional" &
        school_type == input$ap_ofsted_reg_bench_filter &
        Measure == input$ap_ofsted_reg_bench_filter_two &
        Year == max(Year)) %>%
      ggplot(aes(
        x = Value,
        y = Region,
        group = `Overall effectiveness`,
        fill = `Overall effectiveness`,
        label = " ",
        text = paste0(
          "Year: ", `Year`, "\n",
          "Region: ", `Region`, "\n",
          "Ofsted effectiveness rating: ", `Overall effectiveness`, "\n",
          paste0(input$ap_ofsted_la_bench_filter_two), ": ", `Value`
        )
      )) +
      geom_col(show.legend = FALSE) +
      labs(
        y = " ",
        x = paste0(input$ap_ofsted_la_bench_filter_two),
        fill = "Region"
      ) +
      theme(legend.position = "bottom") +
      scale_fill_manual(values = char_gradient_ap)

    ap_ofsted_reg_bench %>%
      ggplotly(
        tooltip = c("text")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(
          orientation = "h",
          traceorder = "reversed",
          y = -0.2
        ),
        dragmode = FALSE
      )
  })


  output$ap_ofsted_reg_bench_table <- renderDT({
    ap_ofsted_reg_bt <- ap_ofsted_schl %>%
      collapse::fsubset(geographic_level == "Regional" &
        school_type == input$ap_ofsted_reg_bench_filter &
        Measure == input$ap_ofsted_reg_bench_filter_two &
        Year == max(Year)) %>%
      fselect("Year",
        "Region",
        "School Type" = "school_type",
        "Overall effectiveness",
        "Measure",
        "Value"
      ) %>%
      arrange(desc(Year), Measure)

    return(DTise(
      ap_ofsted_reg_bt,
      c(list(4, "asc"), list(3, "asc"), list(2, "asc"))
    ))
  })


  # =====================================================
  # Unregistered Alternative Provision (UAP) - counts
  # =====================================================

  plot_ap_uap_time <- function(data) {
    data <- data %>%
      mutate(
        Total = round(Total, 0),
        Region = Region
      ) %>%
      arrange(desc(Total)) %>%
      mutate(`Setting type` = factor(`Setting type`, levels = `Setting type`)) %>%
      ggplot(aes(
        x = `Academic year`,
        y = Total,
        fill = `Setting type`
      )) +
      geom_col(position = "dodge", show.legend = T) +
      labs(
        x = "Academic year",
        y = "Total",
        fill = " "
      ) +
      scale_fill_manual(
        values =
          c(
            "Grand total unregistered AP (all setting types)" = "#B33E52",
            "Non-maintained further education college" = "#0b2841",
            "One on one tuition" = "#0e3657",
            "Other unregistered provider" = "#12436d",
            "Registered provider with UKPRN" = "#41698a",
            "Work based placement" = "#718ea7",
            "Unknown " = "black"
          )
      ) +
      coord_flip()

    data %>%
      ggplotly(
        tooltip = c("text", "y", "colour", "x", "fill")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2, font = list(size = 11)),
        dragmode = FALSE,
        margin = list(b = 100) # Adjust the bottom margin as needed
      )
  }


  table_ap_uap_time <- function(data, var) {
    data <- data %>%
      mutate(
        Total = round(Total, 0),
        Region = Region
      ) %>%
      arrange(
        desc(`Academic year`),
        `Type of AP`,
        Total
      ) %>%
      select(
        `Academic year`,
        !!ensym(var), # `Local authority` or `Region`
        `Type of AP`,
        `Setting type`,
        Total
      )
    return(DTise(data, c(list(0, "desc"), list(2, "asc"))) %>%
      formatRound("Total", 0))
  }


  ## LA time
  output$ap_uap_la_time <- renderPlotly({
    ap_uap_la_type <- uap_counts %>%
      filter(
        la_name == input$la_choice,
        `Type of AP` == input$ap_uap_la_time_filter
      )

    if (nrow(ap_uap_la_type) > 0) {
      ap_uap_la_type <- ap_uap_la_type %>%
        plot_ap_uap_time()
    } else {
      ap_uap_la_type %>% validate_if_no_la_data()
    }
  })

  output$ap_uap_la_time_table <- renderDT({
    ap_uap_la_typet <- uap_counts %>%
      fsubset(la_name == input$la_choice &
        `Type of AP` == input$ap_uap_la_time_filter)

    if (nrow(ap_uap_la_typet) > 0) {
      ap_uap_la_typet <- ap_uap_la_typet %>%
        table_ap_uap_time(`Local authority`)
    } else {
      ap_uap_la_typet %>% validate_if_no_la_data()
    }
  })

  ## reg time
  output$ap_uap_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ap_uap_reg_type <- uap_counts %>%
        filter(
          Region == input$region_choice &
            geographic_level %in% "Regional" &
            `Type of AP` == input$ap_uap_reg_time_filter
        ) %>%
        plot_ap_uap_time()
    } else {
      ap_uap_reg_type <- uap_counts %>%
        filter(
          geographic_level %in% "National" &
            `Type of AP` == input$ap_uap_reg_time_filter
        ) %>%
        plot_ap_uap_time()
    }
  })

  output$ap_uap_reg_time_table <- renderDT({
    if (input$level_choice == "Regions") {
      ap_uap_reg_typet <- uap_counts %>%
        filter(
          Region == input$region_choice &
            geographic_level %in% "Regional" &
            `Type of AP` == input$ap_uap_reg_time_filter
        ) %>%
        table_ap_uap_time(`Region`)
    } else {
      ap_uap_reg_typet <- uap_counts %>%
        filter(
          geographic_level %in% "National" &
            `Type of AP` == input$ap_uap_reg_time_filter
        ) %>%
        table_ap_uap_time(`Region`)
    }
  })


  ## AP counts (LA/bench)

  output$ap_uap_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(uap_counts))
    ap_uap_la_bench_data <- uap_counts %>%
      collapse::fsubset(setting_type %in% "Grand total unregistered AP (all setting types)" &
        geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        `Type of AP` == input$ap_uap_la_bench_filter &
        time_period == comparison_year)

    chosen_la <- ap_uap_la_bench_data %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_uap_la_bench_data <- ap_uap_la_bench_data %>%
        collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
          yes = input$la_choice,
          no = "Other LA"
        )) %>%
        filter(if (input$myregion_switch == TRUE) {
          Region == Region[la_name == input$la_choice]
        } else {
          Region != "none"
        }) %>%
        add_ranks(outcome = "total")

      ap_uap_la_bench_data$rank_statement <- rank_statement_fun(ap_uap_la_bench_data,
        rank_col = rank,
        name_col = la_name,
        time_period = academic_year
      )

      ap_uap_la_bench <- ggplot(ap_uap_la_bench_data, aes(
        x = fct_reorder(la_name, Total),
        y = Total,
        text = la_name,
        fill = chosen_la,
        label = ""
      )) +
        geom_col(position = "stack", show.legend = FALSE) +
        geom_text(
          size = 2.5,
          position = position_stack(vjust = 0.5)
        ) +
        labs(
          x = ap_uap_la_bench_data$rank_statement[ap_uap_la_bench_data$la_name == input$la_choice],
          y = "Total",
          fill = "Local Authority"
        ) +
        theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = c("Other LA" = not_focused), na.value = focused)

      ap_uap_la_bench %>%
        ggplotly(
          tooltip = c("text", "y"),
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        )
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })



  output$ap_uap_la_bench_table <- renderDT({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(uap_counts))
    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ap_uap_la_bt <- uap_counts %>%
      collapse::fsubset(`Setting type` %in% "Grand total unregistered AP (all setting types)" &
        geographic_level == "Local authority" &
        time_period == comparison_year &
        !(la_name %in% small_LAs) &
        `Type of AP` == input$ap_uap_la_bench_filter)

    chosen_la <- ap_uap_la_bt %>% filter(la_name == input$la_choice)

    if (nrow(chosen_la) > 0) {
      ap_uap_la_bt <- ap_uap_la_bt %>%
        filter(if (input$myregion_switch == TRUE) {
          Region == chosen_region
        } else {
          Region != "none"
        }) %>%
        arrange(desc(`Academic year`), desc(Total)) %>%
        fselect(
          `Academic year`,
          `Local authority`,
          `Type of AP`,
          `Total` = Total
        )

      return(DTise(
        ap_uap_la_bt,
        c(list(0, "desc"), list(4, "desc"))
      ) %>%
        formatRound("Total", 0))
    } else {
      chosen_la %>% validate_if_no_la_data()
    }
  })


  output$ap_uap_reg_bench <- renderPlotly({
    region_choice_validated <- if (is.null(input$region_choice)) {
      "Other region"
    } else {
      input$region_choice
    }

    ap_uap_reg_bench_s1 <- uap_counts %>%
      collapse::fsubset(setting_type %in% "Grand total unregistered AP (all setting types)" &
        geographic_level == "Regional" &
        `Type of AP` == input$ap_uap_reg_bench_filter &
        time_period == max(time_period))

    ap_uap_reg_bench <- plot_reg_bench(
      data = ap_uap_reg_bench_s1,
      outcome_var = ap_uap_reg_bench_s1$Total,
      label_y = "Total",
      region_choice_validated = paste0(region_choice_validated),
      hover_label = "Total"
    )
  })


  output$ap_uap_reg_bench_table <- renderDT({
    ap_uap_reg_type <- uap_counts %>%
      collapse::fsubset(setting_type %in% "Grand total unregistered AP (all setting types)" &
        geographic_level == "Regional" &
        `Type of AP` == input$ap_uap_reg_bench_filter &
        time_period == max(time_period)) %>%
      mutate(`Grand total unregistered AP (all setting types)` = Total) %>%
      table_bench_time(
        var_filter = quote(`Type of AP`),
        outcome_var = quote(`Grand total unregistered AP (all setting types)`),
        digits = 0
      )
  })


  ########################## End of main tables ###############################################

  ## Summary

  output$summary <- renderGirafe({
    text_df <- data.frame(
      "metric" = c("Highest LA", "Median LA", "Lowest LA"),
      "detail" = c("Highest LA", "Median LA", "Lowest LA"),
      "y" = c(-1, 73, 153),
      "x" = c(0.5, 0.5, 0.5),
      "Theme" = c("Outcomes", "Outcomes", "Outcomes")
    ) %>%
      mutate(Theme = factor(Theme, ordered = TRUE, levels = c(
        "Outcomes",
        "Experiences",
        "Financial Sustainability",
        "Identification of Need"
      )))

    validate(need(input$la_choice, message = "Please choose an LA to display the graph"))

    metric_order <- tibble(metric = c(
      "EYFSP",
      "Phonics screening check",
      "KS2 attainment",
      "KS4 attainment",
      "16-18 destinations",
      "Discontinued EHC plans",
      "EHCP timeliness",
      "Tribunal appeal rate",
      "Absence",
      "KS4 Destinations",
      "% of pupils with EHC plans",
      "Pupils in mainstream with SEN",
      "Pupils in specialist settings",
      "Children in need with SEN",
      "DSG cumulative balance",
      "Specialist spend per head"
    )) %>%
      rownames_to_column(var = "Position") %>%
      mutate(Position = as.numeric(Position))

    summary_metrics %>%
      ungroup() %>%
      fsubset(la_name == input$la_choice &
        (sen_status == input$summary_sen_type_la | sen_status == "Not switchable")) %>% # sen status equal to the dropdown or not part of the switch
      arrange(Theme, desc(mean_rank)) %>%
      # x offsets achieved through look-up table since with the error bars they basically always overlap
      left_join(metric_order) %>%
      # Order the themes in the same way as the tabs
      mutate(Theme = factor(Theme, ordered = TRUE, levels = c(
        "Outcomes",
        "Experiences",
        "Identification of Need",
        "Financial Sustainability"
      ))) %>%
      ggplot(aes(
        x = mean_rank,
        xmin = min_rank,
        xmax = max_rank,
        y = fct_reorder(metric, -Position),
        label = metric,
        colour = Theme,
        fill = Theme,
        group = metric,
        tooltip = detail
      )) +
      #  High/low/median lines

      # geom_vline(aes(xintercept = 152), alpha = 0.3, linetype = "dotted") +
      geom_textvline(aes(label = "Lowest LA", xintercept = 150), alpha = 0.3, linetype = "dotted", fontface = "bold", hjust = 0.8) +
      geom_textvline(aes(label = "Median LA", xintercept = 76), alpha = 0.3, linetype = "dotted", fontface = "bold", hjust = 0.8) +
      geom_textvline(aes(label = "Highest LA", xintercept = 1), alpha = 0.3, linetype = "dotted", fontface = "bold", hjust = 0.8) +
      # geom_rect(aes(ymin = "16-18 destinations", ymax = "Absence", xmin = -30, xmax = -10), fill = af_darkblue, colour = "white") +
      # geom_rect(aes(ymin = "Absence", ymax = "% of pupils with SEN", xmin = -30, xmax = -10), fill = af_turquoise, colour = "white") +
      # geom_rect(aes(ymin = "% of pupils with SEN", ymax = "Pupils in specialist settings", xmin = -30, xmax = -10), colour = "white", fill = af_darkpink) +
      # geom_rect(aes(ymin = "Pupils in specialist settings", ymax = "Specialist spend per head", xmin = -30, xmax = -10), colour = "white", fill = af_orange) +
      # geom_textpath(data = text_df, aes(y = metric, x = y, label = Theme), colour = "white", halign = "right") +
      # Circles
      geom_point_interactive(
        shape = 16,
        size = 10, hover_nearest = TRUE
      ) +
      geom_linerange_interactive(linewidth = 1) +
      # Text with rank numbers to put on the circles
      geom_text_interactive(aes(label = mean_rank),
        hjust = 0.5,
        colour = "white",
        size = 4
      ) +
      # Metric labels
      # If these settings are changed for the following geom, there is a risk the text is not rendered on the server.
      # geom_text_repel_interactive(colour = "black",
      #                             size = 3,
      #                             alpha = 0.7,
      #                             direction = "both",
      #                             force = 2,
      #                             force_pull = 0.7,
      #                             point.padding = 12) +
      theme_bw() +
      theme(
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        # axis.ticks = element_blank(),
        # panel.border = element_blank()
      ) +
      scale_x_continuous(trans = "reverse", limits = c(160, -10), expand = expansion(add = 20)) +
      scale_y_discrete(expand = expansion(mult = 0.05)) +
      theme(legend.position = "bottom") +
      labs(
        x = "LA's rank within 150 LAs",
        title = "LA Metric Summary"
      ) +
      scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) +
      scale_fill_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) -> summary_plot

    girafe(ggobj = summary_plot, width_svg = 11, height_svg = 7, canvas_id = "la_summary")
  })

  # unused graphs removed because this file is quite long enough as it is and they can always be resurrected from version control

  # Summary table

  output$la_summary_table <- renderDT({
    summary_table <- summary_metrics %>%
      ungroup() %>%
      drop_na(mean_rank, Theme, metric, detail) %>%
      fsubset(la_name == input$la_choice &
        (sen_status == input$summary_sen_type_la | sen_status == "Not switchable")) %>%
      fmutate(
        Theme = factor(Theme, levels = c("Outcomes", "Experiences", "Identification of Need", "Financial Sustainability")),
        Detail = detail,
        sen_status = if_else(sen_status == "Not switchable", "NA or identified in Detail column", sen_status)
      ) %>%
      arrange(Theme) %>%
      select(
        `Local Authority` = la_name,
        Theme,
        `SEN Status` = sen_status,
        Metric = metric,
        Detail,
        Outcome,
        `LA Rank (most recent year)` = mean_rank, # ignore the column name, this is what it actually is
        `Highest LA rank` = min_rank,
        `Lowest LA rank` = max_rank
      )
    return(DTise(
      summary_table,
      list(list(1, "asc"), list(2, "asc"))
    ))
  })

  # Alternative Provision summary, which is separate due to not every LA necessarily appearing in all metrics and therefore the ranks not being out of 150
  output$ap_summary_table <- renderDT({
    ap_summary_table <- ap_summary_metrics %>%
      ungroup() %>%
      drop_na(mean_rank, Theme, metric, detail) %>%
      fsubset(la_name == input$la_choice &
        (sen_status == input$summary_sen_type_la | sen_status == "Not switchable")) %>%
      fmutate(
        sen_status = if_else(sen_status == "Not switchable", "NA or identified in Detail column", sen_status)
      ) %>%
      select(
        `Local Authority` = la_name,
        Theme,
        `SEN Status` = sen_status,
        Metric = metric,
        Detail = detail,
        Outcome,
        `LA Rank (most recent year)` = mean_rank, # ignore the column name, this is what it actually is
        `Out of` = out_of,
        `Highest LA rank` = min_rank,
        `Lowest LA rank` = max_rank
      )

    return(DTise(
      ap_summary_table,
      list(list(3, "asc"))
    ))
  })

  # England Summary ---------------------------------------------------------------------------

  # EYFSP
  output$box_eyfsp <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    validate(need(input$summary_sen_type, message = "Select SEN level"), errorClass = "summary-validation")
    df <- eng_eyfsp %>%
      drop_na(gld_percentage) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice) %>%
      fsubset(characteristic_type == input$summary_sen_type)

    eyfsp_data <- box_data(df, gld_percentage, academic_year)

    missing_box(
      df = eng_eyfsp,
      latest_value = eyfsp_data$latest_value,
      latest_timeperiod = eyfsp_data$latest_timeperiod,
      colour = "blue"
    )
  })
  # KS1 phonics
  output$box_ks1_phonics <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    validate(need(input$summary_sen_type, message = "Select SEN level"), errorClass = "summary-validation")
    df <- eng_ks1_phonics %>%
      drop_na(`Percent meeting expected standards in Y1`) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    latest_df <- df %>%
      filter(
        characteristic == input$summary_sen_type,
        academic_year == max(academic_year)
      )

    latest_timeperiod <- pull(latest_df, academic_year)
    latest_value <- pull(latest_df, `Percent meeting expected standards in Y1`)

    previous_df <- df %>%
      filter(
        characteristic == input$summary_sen_type,
        academic_year != max(academic_year)
      ) %>%
      filter(academic_year == max(academic_year))

    previous_timeperiod <- pull(previous_df, academic_year)
    previous_value <- pull(previous_df, `Percent meeting expected standards in Y1`)
    change <- pull(latest_df, pc_change)

    create_box(
      df = eng_ks1_phonics,
      latest_value = latest_value,
      latest_timeperiod = latest_timeperiod,
      previous_value = previous_value,
      previous_timeperiod = previous_timeperiod,
      add_percent_symbol = TRUE,
      colour = "blue"
    )
  })

  # sparklines no longer used as somewhat confusing and also took up too much space for the summary pane, but code
  # preserved in case we find a future use for them
  #  output$sparkline_ks1_phonics <- renderPlotly({

  # Create ggplot sparkline
  #    p1 <- eng_ks1_phonics %>%#
  #      filter(characteristic == "EHC plan") %>%
  #      rename("Academic year" = academic_year) %>%
  #      create_sparkline(outcome_var = `Percent meeting expected standards in Y1`,
  #                       time_var = `Academic year`,
  #                       yformat = "percent")
  #
  # Send to plotly
  #    p1 %>%
  #      ggplotly(height = sparkline_height,width = 300,
  #               tooltip = "text") %>%
  #      format_sparkline()
  # })




  # KS2 attainment
  output$box_ks2_attainment <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    validate(need(input$summary_sen_type, message = "Select SEN level"), errorClass = "summary-validation")
    df <- eng_ks2_attainment %>%
      drop_na(`Percent meeting expected standard`) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    latest_df <- df %>%
      filter(
        characteristic == input$summary_sen_type,
        academic_year == max(academic_year)
      )

    latest_timeperiod <- pull(latest_df, academic_year)
    latest_value <- pull(latest_df, `Percent meeting expected standard`)

    previous_df <- df %>%
      filter(
        characteristic == input$summary_sen_type,
        academic_year != max(academic_year)
      ) %>%
      filter(academic_year == max(academic_year))

    previous_timeperiod <- pull(previous_df, academic_year)
    previous_value <- pull(previous_df, `Percent meeting expected standard`)
    change <- pull(latest_df, pc_change)

    create_box(
      df = eng_ks2_attainment,
      latest_value = latest_value,
      latest_timeperiod = latest_timeperiod,
      previous_value = previous_value,
      previous_timeperiod = previous_timeperiod,
      add_percent_symbol = TRUE,
      colour = "blue"
    )
  })




  #  output$sparkline_ks2_attainment <- renderPlotly({
  #    #Create ggplot sparkline
  #    p1 <- eng_ks2_attainment %>%
  #      filter(characteristic == "All SEN") %>%
  #      rename("Academic year" = academic_year) %>%
  #      create_sparkline(outcome_var = `Percent meeting expected standards`,
  #                       time_var = `Academic year`,
  #                       yformat = "percent")
  #
  #    #Send to plotly
  #    p1 %>%
  #      ggplotly(height = sparkline_height,
  #               tooltip = "text") %>%
  #      format_sparkline()
  #  })


  # Mental Health support
  output$box_mentalhealth <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")

    nhs_region_from_region <- case_when(input$region_choice %in% c("Yorkshire and The Humber", "North East") ~ "North East And Yorkshire",
      input$region_choice %in% c("East Midlands", "West Midlands") ~ "Midlands",
      input$region_choice == "East of England" ~ "East Of England",
      .default = input$region_choice
    )

    # can't use geo_subset here because df has different colnames and we're also not using DfE regions
    if (input$level_choice == "England") {
      df <- eng_mentalhealth %>%
        drop_na(`Number of children and young people`) %>%
        fsubset(BREAKDOWN == "England")
    } else {
      df <- eng_mentalhealth %>%
        drop_na(`Number of children and young people`) %>%
        fsubset(BREAKDOWN == "Commissioning Region" &
          nhs_name == nhs_region_from_region)
    }

    box_data <- data.frame("x")

    box_data$latest_timeperiod <- paste0(
      lubridate::month(max(df$`Year ending`),
        label = TRUE, abbr = FALSE
      ),
      " ",
      lubridate::year(max(df$`Year ending`))
    )
    target_timeframe <- df %>%
      filter(`Year ending` <= (max(df$`Year ending`) - years(1)))

    box_data$previous_timeperiod <- paste0(
      lubridate::month(max(target_timeframe$`Year ending`),
        label = TRUE, abbr = FALSE
      ),
      " ",
      lubridate::year(max(target_timeframe$`Year ending`))
    )

    box_data$latest_value <- df$`Number of children and young people`[df$`Year ending` == max(df$`Year ending`)]

    box_data$previous_value <- df$`Number of children and young people`[df$`Year ending` == (max(target_timeframe$`Year ending`))]

    box_data$change <- df$pc_change[df$`Year ending` == max(df$`Year ending`)]

    create_box(
      df = eng_mentalhealth,
      latest_value = box_data$latest_value,
      latest_timeperiod = box_data$latest_timeperiod,
      previous_value = box_data$previous_value,
      previous_timeperiod = box_data$previous_timeperiod,
      add_percent_symbol = FALSE,
      colour = "blue"
    )
  })

  #  output$sparkline_mentalhealth <- renderPlotly({
  #
  #    #Create ggplot sparkline
  #    p1 <- eng_mentalhealth %>%
  #      drop_na(`Number of children and young people`) %>%
  #      create_sparkline(outcome_var = `Number of children and young people`,
  #                       time_var = `Year ending`,
  #                       yformat = "comma") +
  #      scale_x_date(date_breaks = "6 months",
  #                   date_labels = "%b %Y")
  #
  #    #Send to plotly
  #    p1 %>%
  #      ggplotly(height = sparkline_height,
  #               tooltip = "text") %>%
  #      format_sparkline()
  #  })

  # KS4 attainment
  output$box_ks4_attainment <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_ks4_attainment <- eng_ks4_attainment %>%
      drop_na(`Average progress 8 score`) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    latest_timeperiod <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "All SEN",
        academic_year == max(academic_year)
      ) %>%
      pull(academic_year)

    latest_value <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "All SEN",
        academic_year == max(academic_year)
      ) %>%
      pull(`Average progress 8 score`)

    previous_timeperiod <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "All SEN",
        academic_year != max(academic_year)
      )
    if (nrow(previous_timeperiod > 0)) {
      previous_timeperiod <- previous_timeperiod %>%
        filter(academic_year == max(academic_year)) %>%
        pull(academic_year)
    }

    previous_value <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "All SEN",
        academic_year != max(academic_year)
      )

    previous_value <- previous_value %>%
      filter(academic_year == max(academic_year)) %>%
      pull(`Average progress 8 score`)

    change <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "All SEN",
        academic_year == max(academic_year)
      ) %>%
      pull(pc_change)

    # currently the previous year of data isn't comparable to the present year due to covid effects so we aren't doing this comparison
    # but it should be reinstated once 2023/4 data becomes available
    # create_box(
    #  df = eng_ks4_attainment,
    #  latest_value = latest_value,
    #  latest_timeperiod = latest_timeperiod,
    #  previous_timeperiod = previous_timeperiod,
    #  change = change,
    #  add_percent_symbol = FALSE,
    #  colour = "blue",
    #  dp = 2
    # )

    missing_box(
      df = eng_ks4_attainment,
      latest_value = latest_value,
      latest_timeperiod = latest_timeperiod,
      text1 = "Previous year not",
      text2 = "comparable to current",
      colour = "blue"
    )
  })

  #  output$sparkline_ks4_attainment <- renderPlotly({
  #
  #    #Create ggplot sparkline
  #    p1 <- eng_ks4_attainment %>%
  #      drop_na(`Average progress 8 score`) %>%
  #      filter(`SEN provision` == "EHC plan") %>%
  #      create_sparkline(outcome_var = `Average progress 8 score`,
  #                       time_var = academic_year,
  #                       yformat = "notpercent") +
  #      scale_y_continuous(expand = expansion(mult = c(1,1)))
  #
  #    #Send to plotly
  #    p1 %>%
  #      ggplotly(height = sparkline_height,
  #               tooltip = "text") %>%
  #      format_sparkline()
  #  })

  output$box_ofsted <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      good_pc <- eng_ofsted[eng_ofsted$WSoAPAP == FALSE, ]$pc_LAs
    } else {
      good_pc <- ungroup(reg_ofsted) %>%
        fsubset(region == input$region_choice &
          WSoAPAP == FALSE) %>%
        pull(pc_LAs)
    }

    latest_value <- paste0(round(100 * good_pc, 1), "%")
    latest <- format(max(ofsted$`Inspection publication date (new inspection framework)`, na.rm = T), "%d %b %Y")

    # doing the ofsted box directly because it's different to all the other boxes
    tags$table(
      style = "background-color: blue",
      style = "color: white",
      tags$tr(
        tags$td(span(format(latest_value, big.mark = ",", trim = TRUE), # Latest value for metric in big font
          style = "font-size: 32px"
        )),
        tags$td(width = 15)
      ), # remove whitespace
      tags$tr(
        tags$td(span(paste0("Date of last inspection: ", latest),
          style = "font-size: 16px",
          .noWS = "before"
        ))
      )
    )
  })

  # destinations is also fiddly since the metric is "not these two categories"
  output$box_1618dest <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")

    # because the 16-18 destinations come in two separate files, one England and one Regional, this has to be done a bit differently
    if (input$level_choice == "England") {
      eng_1618 <- destinations_1618_nat %>%
        fsubset(characteristic == "Identified SEN (mainstream)")
    } else {
      eng_1618 <- reg_1618 %>%
        fsubset(characteristic == "Identified SEN (mainstream)") %>%
        geo_subset(lev = input$level_choice, reg = input$region_choice)
    }

    current <- eng_1618 %>%
      fsubset(time_period == max(time_period))

    current_year <- pull(current, academic_year)

    current_value <- current %>%
      fsubset(Destination %in% c("Not sustained", "Unknown")) %>%
      fsummarise(value = 100 - fsum(`% of pupils`)) %>%
      pull(value)
    previous_year <- eng_1618 %>%
      fsubset(time_period != max(time_period)) %>%
      fsubset(time_period == max(time_period)) %>%
      pull(academic_year)
    previous_value <- eng_1618 %>%
      fsubset(time_period != max(time_period)) %>%
      fsubset(time_period == max(time_period) &
        Destination %in% c("Not sustained", "Unknown")) %>%
      fsummarise(value = 100 - fsum(`% of pupils`)) %>%
      pull(value)
    change <- 100 * (current_value - previous_value) / previous_value

    create_box(
      df = eng_1618,
      latest_value = current_value,
      latest_timeperiod = current_year[1],
      previous_timeperiod = previous_year[1],
      previous_value = previous_value[1],
      add_percent_symbol = TRUE,
      colour = "blue"
    )
  })

  output$box_discontinued <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")

    # can't use geo_subset here because df has different colnames and we're also not using DfE regions
    if (input$level_choice == "England") {
      df <- eng_discontinued %>%
        drop_na(discontinued_schoolage) %>%
        fsubset(region_name == "England")
    } else {
      df <- eng_discontinued %>%
        drop_na(discontinued_schoolage) %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional")
    }

    box <- box_data(df, "discontinued_schoolage", time_period)

    create_box(
      df = eng_discontinued,
      latest_value = box$latest_value,
      latest_timeperiod = box$latest_timeperiod,
      previous_timeperiod = box$previous_timeperiod,
      previous_value = box$previous_value,
      add_percent_symbol = FALSE,
      colour = "blue",
      dp = 2
    )
  })

  output$box_timeliness <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_ehcp_timeliness <- eng_ehcp_timeliness %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    create_box(
      df = eng_ehcp_timeliness,
      latest_value = eng_ehcp_timeliness$`% of EHCPs issued within 20 weeks`[1],
      latest_timeperiod = eng_ehcp_timeliness$time_period[1],
      previous_timeperiod = eng_ehcp_timeliness$time_period[2],
      previous_value = eng_ehcp_timeliness$`% of EHCPs issued within 20 weeks`[2],
      add_percent_symbol = TRUE,
      colour = "lightblue"
    )
  })

  output$box_tribunals <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")

    if (input$level_choice == "England") {
      eng_tribunals <- eng_tribunals %>%
        fsubset(region_name == "England") %>%
        arrange(desc(year))
    } else {
      eng_tribunals <- eng_tribunals %>%
        fsubset(region_name == input$region_choice) %>%
        arrange(desc(year))
    }

    # then since the current year will be at the top...
    create_box(
      df = eng_tribunals,
      latest_value = eng_tribunals$`SEND Tribunal Appeal Rate`[1],
      latest_timeperiod = eng_tribunals$year[1],
      previous_timeperiod = eng_tribunals$year[2],
      previous_value = eng_tribunals$`SEND Tribunal Appeal Rate`[2],
      add_percent_symbol = TRUE,
      colour = "lightblue"
    )
  })

  output$box_absence <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")

    # sen column in this df isn't the same as others as it still contains the legacy "or statement" language
    sen_from_menu_sen <- if_else(input$summary_sen_type == "EHC plan", "EHCP or Statement", input$summary_sen_type)

    # this is yet another subtly different data structure - this time "England" is treated as a region
    if (input$level_choice == "England") {
      eng_absence <- eng_absence %>%
        fsubset(region_name == "England")
    } else {
      eng_absence <- eng_absence %>%
        fsubset(region_name == input$region_choice)
    }
    eng_absence <- eng_absence %>%
      arrange(desc(time_period)) %>%
      fsubset(characteristic == sen_from_menu_sen &
        `Absence measure` == "Overall")

    # then since the current year will be at the top...
    create_box(
      df = eng_absence,
      latest_value = eng_absence$Percentage[1],
      latest_timeperiod = eng_absence$academic_year[1],
      previous_timeperiod = eng_absence$academic_year[2],
      previous_value = eng_absence$Percentage[2],
      add_percent_symbol = TRUE,
      colour = "lightblue"
    )
  })

  output$box_autism <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_autism <- eng_autism %>%
      arrange(desc(date))

    # then since the current year will be at the top...
    create_box(
      df = eng_autism,
      latest_value = eng_autism$`% with first appointment after more than 13 weeks`[3],
      latest_timeperiod = eng_autism$REPORTING_PERIOD_END[3],
      previous_timeperiod = eng_autism$REPORTING_PERIOD_END[6], # three rows per year due to age groups
      previous_value = eng_autism$`% with first appointment after more than 13 weeks`[6],
      add_percent_symbol = TRUE,
      colour = "lightblue"
    )
  })

  # destinations again, has the same problem as before with national data not in the same file as regional
  output$box_KS4dest <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      eng_ks4 <- ks4_destinations_nat %>%
        fsubset(characteristic == "Identified SEN")
    } else {
      eng_ks4 <- ks4_destinations %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          characteristic == "Identified SEN")
    }
    current_year <- eng_ks4 %>%
      fsubset(time_period == max(time_period)) %>%
      pull(academic_year)
    current_value <- eng_ks4 %>%
      fsubset(time_period == max(time_period) &
        Destination %in% c("Not sustained", "Unknown")) %>%
      fsummarise(value = 100 - fsum(`% of pupils`)) %>%
      pull(value)
    previous_year <- eng_ks4 %>%
      fsubset(time_period != max(time_period)) %>%
      fsubset(time_period == max(time_period)) %>%
      pull(academic_year)
    previous_value <- eng_ks4 %>%
      fsubset(time_period != max(time_period)) %>%
      fsubset(time_period == max(time_period) &
        Destination %in% c("Not sustained", "Unknown")) %>%
      fsummarise(value = 100 - fsum(`% of pupils`)) %>%
      pull(value)
    change <- 100 * (current_value - previous_value) / previous_value
    create_box(
      df = eng_1618,
      latest_value = current_value,
      latest_timeperiod = current_year[1],
      previous_timeperiod = previous_year[1],
      previous_value = previous_value[1],
      add_percent_symbol = TRUE,
      colour = "lightblue"
    )
  })

  output$box_statefunded <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_percent_pupils_ehcp <- eng_percent_pupils_ehcp %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_percent_pupils_ehcp,
      latest_value = eng_percent_pupils_ehcp$`% of pupils`[1],
      latest_timeperiod = eng_percent_pupils_ehcp$academic_year[1],
      previous_timeperiod = eng_percent_pupils_ehcp$academic_year[3], # two rows per year due to EHCP/SEN Sup
      previous_value = eng_percent_pupils_ehcp$`% of pupils`[3],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })

  output$box_mainstream <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_mainstream_with_sen <- eng_mainstream_with_sen %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice) %>%
      fsubset(`SEN provision` == input$summary_sen_type)

    # then since the current year will be at the top...
    create_box(
      df = eng_mainstream_with_sen,
      latest_value = eng_mainstream_with_sen$`% of pupils`[1],
      latest_timeperiod = eng_mainstream_with_sen$academic_year[1],
      previous_timeperiod = eng_mainstream_with_sen$academic_year[2],
      previous_value = eng_mainstream_with_sen$`% of pupils`[2],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })

  output$box_special <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_provider_types <- eng_provider_types %>%
      arrange(desc(academic_year)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_provider_types,
      latest_value = eng_provider_types$`% of pupils (with SEN provision type)`[1],
      latest_timeperiod = eng_provider_types$academic_year[1],
      previous_timeperiod = eng_provider_types$academic_year[2],
      previous_value = eng_provider_types$`% of pupils (with SEN provision type)`[2],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })

  output$box_cin <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_cin <- eng_cin %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice) %>%
      fsubset(social_care_group == "CINO at 31 March" &
        `SEN Provision` == input$summary_sen_type) %>%
      time_period_to_academic_year()


    # then since the current year will be at the top...
    create_box(
      df = eng_cin,
      latest_value = eng_cin$`Percentage of children`[1],
      latest_timeperiod = eng_cin$time_period[1],
      previous_timeperiod = eng_cin$time_period[2],
      previous_value = eng_cin$`Percentage of children`[2],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })
  output$box_budget <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    eng_dsg_deficit <- eng_dsg_deficit %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_dsg_deficit,
      latest_value = eng_dsg_deficit$`DSG cumulative balance as a % of the total income`[1],
      latest_timeperiod = eng_dsg_deficit$financial_year[1],
      previous_timeperiod = eng_dsg_deficit$financial_year[2],
      previous_value = eng_dsg_deficit$`DSG cumulative balance as a % of the total income`[2],
      add_percent_symbol = TRUE,
      colour = "black"
    )
  })

  output$box_percap <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_percap <- nat_specialist_spend %>%
        ungroup() %>%
        arrange(desc(year)) %>%
        fsubset(category == "Total")
    } else {
      summary_percap <- ungroup(reg_specialist_spend) %>%
        arrange(desc(year)) %>%
        fsubset(region == input$region_choice &
          category == "Total")
    }
    # then since the current year will be at the top...
    create_box(
      df = summary_percap,
      latest_value = summary_percap$`Spend per head`[1],
      latest_timeperiod = summary_percap$year[1],
      previous_timeperiod = summary_percap$year[2],
      previous_value = summary_percap$`Spend per head`[2],
      add_percent_symbol = FALSE,
      money = TRUE,
      colour = "black"
    )
  })

  output$box_apcount_sf <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_sfap <- eng_ap_counts %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == "State-funded AP school")
    } else {
      summary_sfap <- ungroup(eng_ap_counts) %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          prov_type == "State-funded AP school") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    create_box(
      df = summary_sfap,
      latest_value = summary_sfap$total[1],
      latest_timeperiod = summary_sfap$academic_year[1],
      previous_timeperiod = summary_sfap$academic_year[2],
      previous_value = summary_sfap$total[2],
      add_percent_symbol = FALSE,
      money = FALSE,
      colour = "purple"
    )
  })

  output$box_apcount_la <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_sfap <- eng_ap_counts %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == "LA funded AP placements")
    } else {
      summary_sfap <- ungroup(eng_ap_counts) %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          prov_type == "LA funded AP placements") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    create_box(
      df = summary_sfap,
      latest_value = summary_sfap$total[1],
      latest_timeperiod = summary_sfap$academic_year[1],
      previous_timeperiod = summary_sfap$academic_year[2],
      previous_value = summary_sfap$total[2],
      add_percent_symbol = FALSE,
      money = FALSE,
      colour = "purple"
    )
  })

  output$box_apcount_sa <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_sfap <- eng_ap_counts %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == "School arranged AP")
    } else {
      summary_sfap <- ungroup(eng_ap_counts) %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          prov_type == "School arranged AP") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    missing_box(
      df = summary_sfap,
      latest_value = summary_sfap$total[1],
      latest_timeperiod = summary_sfap$academic_year[1],
      colour = "purple"
    )
  })

  output$box_uapcount_sa <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_sfuap <- eng_uap_counts %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == "School arranged unregistered AP pupils")
    } else {
      summary_sfuap <- ungroup(eng_uap_counts) %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          prov_type == "School arranged unregistered AP pupils") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    missing_box(
      df = summary_sfuap,
      latest_value = summary_sfuap$total[1],
      latest_timeperiod = summary_sfuap$academic_year[1],
      colour = "purple"
    )
  })

  output$box_uapcount_la <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_sfuap <- eng_uap_counts %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          `Type of AP` == "LA funded unregistered AP placements")
    } else {
      summary_sfuap <- ungroup(eng_uap_counts) %>%
        fsubset(geographic_level == "Regional" &
          Region == input$region_choice &
          prov_type == "LA funded unregistered AP placements") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    missing_box(
      df = summary_sfuap,
      latest_value = summary_sfuap$total[1],
      latest_timeperiod = summary_sfuap$academic_year[1],
      colour = "purple"
    )
  })

  output$box_apchars <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_sen_ap <- eng_ap_characteristics %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          Characteristic == input$summary_sen_type &
          `Type of AP` == "State-funded AP school")
    } else {
      summary_sen_ap <- ungroup(eng_ap_characteristics) %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          Characteristic == input$summary_sen_type &
          `Type of AP` == "State-funded AP school") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    create_box(
      df = summary_sen_ap,
      latest_value = summary_sen_ap$`% of pupils`[1],
      latest_timeperiod = summary_sen_ap$academic_year[1],
      previous_timeperiod = summary_sen_ap$academic_year[2],
      previous_value = summary_sen_ap$`% of pupils`[2],
      add_percent_symbol = TRUE,
      money = FALSE,
      colour = "purple"
    )
  })

  output$box_apabsence <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_abs_ap <- sf_ap_absence_regional %>%
        ungroup() %>%
        arrange(desc(time_period)) %>%
        fsubset(geographic_level == "National" &
          `Absence measure` == "Overall absence %")
    } else {
      summary_abs_ap <- ungroup(sf_ap_absence_regional) %>%
        fsubset(geographic_level == "Regional" &
          region_name == input$region_choice &
          `Absence measure` == "Overall absence %") %>%
        arrange(desc(time_period))
    }

    # then since the current year will be at the top...
    create_box(
      df = summary_abs_ap,
      latest_value = summary_abs_ap$Percentage[1],
      latest_timeperiod = summary_abs_ap$academic_year[1],
      previous_timeperiod = summary_abs_ap$academic_year[2],
      previous_value = summary_abs_ap$Percentage[2],
      add_percent_symbol = TRUE,
      money = FALSE,
      colour = "purple"
    )
  })

  output$box_apofsted <- renderUI({
    validate(need(input$level_choice, message = "Pick England or regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_ofsted_ap <- eng_ap_ofsted_schl %>%
        ungroup() %>%
        arrange(desc(Year)) %>%
        fsubset(Region == "England")
    } else {
      summary_ofsted_ap <- ungroup(eng_ap_ofsted_schl) %>%
        fsubset(Region == input$region_choice) %>%
        arrange(desc(Year))
    }

    # then since the current year will be at the top...
    create_box(
      df = summary_ofsted_ap,
      latest_value = summary_ofsted_ap$Value[1],
      latest_timeperiod = summary_ofsted_ap$Year[1],
      previous_timeperiod = summary_ofsted_ap$Year[2],
      previous_value = summary_ofsted_ap$Value[2],
      add_percent_symbol = TRUE,
      money = FALSE,
      colour = "purple"
    )
  })
  # CONDITIONAL TEXT -----------------------------------------------------------------------------

  # messages about Northamptonshire
  la_text <- reactive({
    case_when(
      input$la_choice %in% c("North Northamptonshire", "West Northamptonshire") ~ "North and West Northamptonshire were crearted out of the former Northamptonshire County Council on the 1st April 2021.
                                                                                         For data prior to that date, select Northamptonshire",
      input$la_choice == "Northamptonshire" ~ "Northamptonshire was divided into North and West Northamptonshire Unitary Authorities on 1st April 2021.
                                                     For data subsequent to that date, select North or West Northamptonshire",
      TRUE ~ ""
    )
  })

  northants_KS4 <- reactive({
    ifelse(test = input$la_choice == "Northamptonshire",
      yes = "<br>No Progress 8 data broken down by SEN description is available for years prior to 2021/22;
        Northamptonshire County Council ceased to exist before this point. For relevant data, see North or West Northamptonshire.",
      no = ""
    )
  })

  northants_dest <- reactive({
    ifelse(test = input$la_choice %in% c("North Northamptonshire", "West Northamptonshire"),
      yes = "<br>Destinations data is not yet available for years after 2021/22; at which point
        North and West Northamptonshire did not yet exist. For relevant data, see Northamptonshire.",
      no = ""
    )
  })

  northants_money <- reactive({
    ifelse(test = input$la_choice %in% c("North Northamptonshire", "West Northamptonshire"),
      yes = "<br>Spend per head data is not yet available for years after 2021/22; at which point
        North and West Northamptonshire did not yet exist. For relevant data, see Northamptonshire.",
      no = ""
    )
  })

  old_la_ofsted <- reactive({
    previous_la <- case_when(input$la_choice %in% c("North Northamptonshire", "West Northamptonshire") ~ "Northamptonshire",
      input$la_choice %in% c("Northumberland", "Westmoreland and Furness") ~ "Cumbria",
      .default = ""
    )
    old_la_ofsted <-
      ifelse(test = input$la_choice %in% c("North Northamptonshire", "West Northamptonshire"),
        yes = paste("Prior to the split this LA was part of", previous_la, "which had the following inspection outcome:"),
        no = ""
      )
  })

  missing_data_1618dest_time <- reactive({
    if (no_dest_data() == TRUE) {
      ifelse(test = input$destinations_1618_la_time_filter %in% c("Identified LLDD (mainstream)", "No identified LLDD"),
        yes = paste("Some local authorities have only returned data in some or all years for children who were identified SEN; try that option as an alternative"),
        no = paste("Some local authorities have only returned data in some or all years for children who were identified SEN; try that option as an alternative")
      )
    } else {
      ""
    }
  })

  missing_data_1618dest_bench <- reactive({
    ifelse(test = input$destinations_1618_la_bench_filter %in% c("Identified LLDD (mainstream)", "No identified LLDD"),
      yes = "Some local authorities only returned data for children who were identified SEN, so there are fewer than 150 LAs here",
      no = ""
    )
  })

  output$nhants_excuse <- renderText(northants_KS4())
  output$nhants_excuse_bench <- renderText(northants_KS4()) # because you can't have two outputs with the same ID
  output$la_changed <- renderText(la_text())
  output$nhants_excuse_dest <- renderText(northants_dest())
  output$nhants_excuse_comp <- renderText(northants_dest())
  output$nhants_excuse_ks4d <- renderText(northants_dest())
  output$nhants_excuse_ks4c <- renderText(northants_dest())
  output$nhants_excuse_ks4t <- renderText(northants_dest())
  output$nh_excuse_spendt <- renderText(northants_money())
  output$nh_excuse_spendc <- renderText(northants_money())
  output$la_split_ofsted <- renderText(old_la_ofsted())
  output$dest_1618_missing_time <- renderText(missing_data_1618dest_time())
  output$dest_1618_missing_bench <- renderText(missing_data_1618dest_bench())

  # CHART/TABLE TOGGLES --------------------------------------------------------------------

  output$eyfsp_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$eyfsp_lat_toggle == "Chart") {
      textOutput("eyfsp_la_time")
    } else {
      textOutput("eyfsp_la_time_table")
    }
  })

  output$eyfsp_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$eyfsp_lab_toggle == "Chart") {
      plotlyOutput("eyfsp_la_bench")
    } else {
      DTOutput("eyfsp_la_bench_table")
    }
  })
  output$ks1_phonics_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$phonics_lat_toggle == "Chart") {
      plotlyOutput("ks1_phonics_la_time")
    } else {
      DTOutput("ks1_phonics_la_time_table")
    }
  })

  output$ks1_phonics_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$phonics_lab_toggle == "Chart") {
      plotlyOutput("ks1_phonics_la_bench")
    } else {
      DTOutput("ks1_phonics_la_bench_table")
    }
  })

  output$ks2_attainment_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks2_lat_toggle == "Chart") {
      plotlyOutput("ks2_attainment_la_time")
    } else {
      DTOutput("ks2_attainment_la_time_table")
    }
  })

  output$ks2_attainment_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks2_lab_toggle == "Chart") {
      plotlyOutput("ks2_attainment_la_bench")
    } else {
      DTOutput("ks2_attainment_la_bench_table")
    }
  })

  output$ks4_attainment_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks4_lat_toggle == "Chart") {
      textOutput("ks4_attainment_la_time")
    } else {
      DTOutput("ks4_attainment_la_time_table")
    }
  })

  output$ks4_attainment_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks4_lab_toggle == "Chart") {
      plotlyOutput("ks4_attainment_la_bench")
    } else {
      DTOutput("ks4_attainment_la_bench_table")
    }
  })

  output$destinations_1618_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dest18_lat_toggle == "Chart") {
      plotlyOutput("destinations_1618_la_time")
    } else {
      DTOutput("destinations_1618_la_time_table")
    }
  })

  output$destinations_1618_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dest18_lab_toggle == "Chart") {
      plotlyOutput("destinations_1618_la_bench")
    } else {
      DTOutput("destinations_1618_la_bench_table")
    }
  })

  output$destinations_1618_la_type_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dest18_typ_toggle == "Chart") {
      plotlyOutput("destinations_1618_la_type")
    } else {
      DTOutput("destinations_1618_la_type_table")
    }
  })

  output$mentalhealth_ccg_time_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$mh_cgt_toggle == "Chart") {
      plotlyOutput("mentalhealth_ccg_time")
    } else {
      DTOutput("mentalhealth_ccg_time_table")
    }
  })

  output$mentalhealth_ccg_bench_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$mh_cgb_toggle == "Chart") {
      plotlyOutput("mentalhealth_ccg_bench")
    } else {
      DTOutput("mentalhealth_ccg_bench_table")
    }
  })

  output$mentalhealth_ccg_bench_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$mh_cgb_toggle == "Chart") {
      plotlyOutput("mentalhealth_ccg_bench")
    } else {
      DTOutput("mentalhealth_ccg_bench_table")
    }
  })

  output$timeliness_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$time_lat_toggle == "Chart") {
      plotlyOutput("timeliness_la_time")
    } else {
      DTOutput("timeliness_la_time_table")
    }
  })

  output$discontinued_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$disco_lat_toggle == "Chart") {
      plotlyOutput("discontinued_la_time")
    } else {
      DTOutput("discontinued_la_time_table")
    }
  })

  output$discontinued_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$disco_lab_toggle == "Chart") {
      plotlyOutput("discontinued_la_bench")
    } else {
      DTOutput("discontinued_la_bench_table")
    }
  })

  output$timeliness_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$time_lab_toggle == "Chart") {
      plotlyOutput("timeliness_la_bench")
    } else {
      DTOutput("timeliness_la_bench_table")
    }
  })

  output$tribunals_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$trib_lat_toggle == "Chart") {
      plotlyOutput("tribunals_la_time")
    } else {
      DTOutput("tribunals_la_time_table")
    }
  })

  output$tribunals_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$trib_lab_toggle == "Chart") {
      plotlyOutput("tribunals_la_bench")
    } else {
      DTOutput("tribunals_la_bench_table")
    }
  })

  output$autism_ccg_time_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$aut_cgt_toggle == "Chart") {
      plotlyOutput("autism_ccg_time")
    } else {
      DTOutput("autism_ccg_time_table")
    }
  })

  output$autism_ccg_bench_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$aut_cgb_toggle == "Chart") {
      plotlyOutput("autism_ccg_bench")
    } else {
      DTOutput("autism_ccg_bench_table")
    }
  })

  output$ch_prov_bench_tog <- renderUI({
    # validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$ch_prob_toggle == "Chart") {
      plotlyOutput("ch_prov_bench", height = "900px")
    } else {
      DTOutput("ch_prov_bench_table")
    }
  })

  output$absence_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$abs_lat_toggle == "Chart") {
      plotlyOutput("absence_la_time")
    } else {
      DTOutput("absence_la_time_table")
    }
  })

  output$absence_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$abs_lab_toggle == "Chart") {
      plotlyOutput("absence_la_bench")
    } else {
      DTOutput("absence_la_bench_table")
    }
  })

  output$ks4_destinations_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$destks4_lat_toggle == "Chart") {
      plotlyOutput("ks4_destinations_la_time")
    } else {
      DTOutput("ks4_destinations_la_time_table")
    }
  })

  output$ks4_destinations_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$destks4_lab_toggle == "Chart") {
      plotlyOutput("ks4_destinations_la_bench")
    } else {
      DTOutput("ks4_destinations_la_bench_table")
    }
  })

  output$ks4_destinations_la_type_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$destks4_typ_toggle == "Chart") {
      plotlyOutput("ks4_destinations_la_type")
    } else {
      DTOutput("ks4_destinations_la_type_table")
    }
  })

  output$percent_pupils_ehcp_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ehcppc_lat_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_la_time")
    } else {
      DTOutput("percent_pupils_ehcp_la_time_table")
    }
  })

  output$percent_pupils_ehcp_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ehcppc_lab_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_la_bench")
    } else {
      DTOutput("percent_pupils_ehcp_la_bench_table")
    }
  })

  output$mainstream_with_sen_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$msen_lat_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_la_time")
    } else {
      DTOutput("mainstream_with_sen_la_time_table")
    }
  })

  output$mainstream_with_sen_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$msen_lab_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_la_bench")
    } else {
      DTOutput("mainstream_with_sen_la_bench_table")
    }
  })

  output$cin_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$cin_lat_toggle == "Chart") {
      plotlyOutput("cin_la_time")
    } else {
      DTOutput("cin_la_time_table")
    }
  })

  output$cin_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$cin_lab_toggle == "Chart") {
      plotlyOutput("cin_la_bench")
    } else {
      DTOutput("cin_la_bench_table")
    }
  })
  output$provider_types_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$types_lat_toggle == "Chart") {
      plotlyOutput("provider_types_la_time")
    } else {
      DTOutput("provider_types_la_time_table")
    }
  })

  output$provider_types_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$types_lab_toggle == "Chart") {
      plotlyOutput("provider_types_la_bench")
    } else {
      DTOutput("provider_types_la_bench_table")
    }
  })

  output$ehcp_ageprofile_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$age_lat_toggle == "Chart") {
      plotlyOutput("ehcp_ageprofile_la_time")
    } else {
      DTOutput("ehcp_ageprofile_la_time_table")
    }
  })

  output$dsg_deficit_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dsg_lat_toggle == "Chart") {
      plotlyOutput("dsg_deficit_la_time")
    } else {
      DTOutput("dsg_deficit_la_time_table")
    }
  })

  output$dsg_deficit_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dsg_lab_toggle == "Chart") {
      plotlyOutput("dsg_deficit_la_bench")
    } else {
      DTOutput("dsg_deficit_la_bench_table")
    }
  })

  output$specialist_spend_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$spend_lat_toggle == "Chart") {
      plotlyOutput("specialist_spend_la_time")
    } else {
      DTOutput("specialist_spend_la_time_table")
    }
  })

  output$specialist_spend_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$spend_lab_toggle == "Chart") {
      plotlyOutput("specialist_spend_la_bench")
    } else {
      DTOutput("specialist_spend_la_bench_table")
    }
  })

  output$eyfsp_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$eyfsp_regt_toggle == "Chart") {
      textOutput("eyfsp_reg_time")
    } else {
      textOutput("eyfsp_reg_time_table")
    }
  })

  output$eyfsp_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$eyfsp_regb_toggle == "Chart") {
      plotlyOutput("eyfsp_reg_bench")
    } else {
      DTOutput("eyfsp_reg_bench_table")
    }
  })

  output$ks1_phonics_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$phonics_regt_toggle == "Chart") {
      plotlyOutput("ks1_phonics_reg_time")
    } else {
      DTOutput("ks1_phonics_reg_time_table")
    }
  })

  output$ks1_phonics_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$phonics_regb_toggle == "Chart") {
      plotlyOutput("ks1_phonics_reg_bench")
    } else {
      DTOutput("ks1_phonics_reg_bench_table")
    }
  })

  output$ks2_attainment_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ks2_regt_toggle == "Chart") {
      plotlyOutput("ks2_attainment_reg_time")
    } else {
      DTOutput("ks2_attainment_reg_time_table")
    }
  })

  output$ks2_attainment_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ks2_regb_toggle == "Chart") {
      plotlyOutput("ks2_attainment_reg_bench")
    } else {
      DTOutput("ks2_attainment_reg_bench_table")
    }
  })

  output$ks4_attainment_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ks4_regt_toggle == "Chart") {
      textOutput("ks4_attainment_reg_time")
    } else {
      DTOutput("ks4_attainment_reg_time_table")
    }
  })

  output$ks4_attainment_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ks4_regb_toggle == "Chart") {
      plotlyOutput("ks4_attainment_reg_bench")
    } else {
      DTOutput("ks4_attainment_reg_bench_table")
    }
  })

  output$destinations_1618_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$dest18_regt_toggle == "Chart") {
      plotlyOutput("destinations_1618_reg_time")
    } else {
      DTOutput("destinations_1618_reg_time_table")
    }
  })

  output$destinations_1618_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$dest18_regb_toggle == "Chart") {
      plotlyOutput("destinations_1618_reg_bench")
    } else {
      DTOutput("destinations_1618_reg_bench_table")
    }
  })

  output$destinations_1618_reg_type_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$dest18_regtyp_toggle == "Chart") {
      plotlyOutput("destinations_1618_reg_type")
    } else {
      DTOutput("destinations_1618_reg_type_table")
    }
  })

  output$mentalhealth_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$mh_regt_toggle == "Chart") {
      plotlyOutput("mentalhealth_reg_time")
    } else {
      DTOutput("mentalhealth_reg_time_table")
    }
  })

  output$mentalhealth_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$mh_regb_toggle == "Chart") {
      plotlyOutput("mentalhealth_reg_bench")
    } else {
      DTOutput("mentalhealth_reg_bench_table")
    }
  })

  output$discontinued_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$disco_regt_toggle == "Chart") {
      plotlyOutput("discontinued_reg_time")
    } else {
      DTOutput("discontinued_reg_time_table")
    }
  })

  output$discontinued_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$disco_regb_toggle == "Chart") {
      plotlyOutput("discontinued_reg_bench")
    } else {
      DTOutput("discontinued_reg_bench_table")
    }
  })
  output$timeliness_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$time_regt_toggle == "Chart") {
      plotlyOutput("timeliness_reg_time")
    } else {
      DTOutput("timeliness_reg_time_table")
    }
  })

  output$timeliness_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$time_regb_toggle == "Chart") {
      plotlyOutput("timeliness_reg_bench")
    } else {
      DTOutput("timeliness_reg_bench_table")
    }
  })

  output$tribunals_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$trib_regt_toggle == "Chart") {
      plotlyOutput("tribunals_reg_time")
    } else {
      DTOutput("tribunals_reg_time_table")
    }
  })

  output$tribunals_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$trib_regb_toggle == "Chart") {
      plotlyOutput("tribunals_reg_bench")
    } else {
      DTOutput("tribunals_reg_bench_table")
    }
  })

  output$autism_nat_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$aut_nat_toggle == "Chart") {
      plotlyOutput("autism_nat_time")
    } else {
      DTOutput("autism_nat_time_table")
    }
  })

  output$autism_nat_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$aut_nab_toggle == "Chart") {
      plotlyOutput("autism_nat_bench", height = "900px")
    } else {
      DTOutput("autism_nat_bench_table")
    }
  })

  output$ch_nat_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ch_nat_toggle == "Chart") {
      plotlyOutput("ch_nat_time")
    } else {
      DTOutput("ch_nat_time_table")
    }
  })

  output$ch_nat_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ch_nab_toggle == "Chart") {
      plotlyOutput("ch_nat_bench")
    } else {
      DTOutput("ch_nat_bench_table")
    }
  })

  output$absence_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$abs_regt_toggle == "Chart") {
      plotlyOutput("absence_reg_time")
    } else {
      DTOutput("absence_reg_time_table")
    }
  })

  output$absence_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$abs_regb_toggle == "Chart") {
      plotlyOutput("absence_reg_bench")
    } else {
      DTOutput("absence_reg_bench_table")
    }
  })

  output$ks4_destinations_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$destks4_regt_toggle == "Chart") {
      plotlyOutput("ks4_destinations_reg_time")
    } else {
      DTOutput("ks4_destinations_reg_time_table")
    }
  })

  output$ks4_destinations_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$destks4_regb_toggle == "Chart") {
      plotlyOutput("ks4_destinations_reg_bench")
    } else {
      DTOutput("ks4_destinations_reg_bench_table")
    }
  })

  output$ks4_destinations_reg_type_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$destks4_regtyp_toggle == "Chart") {
      plotlyOutput("ks4_destinations_reg_type")
    } else {
      DTOutput("ks4_destinations_reg_type_table")
    }
  })

  output$percent_pupils_ehcp_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ehcppc_regt_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_reg_time")
    } else {
      DTOutput("percent_pupils_ehcp_reg_time_table")
    }
  })

  output$percent_pupils_ehcp_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$ehcppc_regb_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_reg_bench")
    } else {
      DTOutput("percent_pupils_ehcp_reg_bench_table")
    }
  })

  output$mainstream_with_sen_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$msen_regt_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_reg_time")
    } else {
      DTOutput("mainstream_with_sen_reg_time_table")
    }
  })

  output$mainstream_with_sen_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$msen_regb_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_reg_bench")
    } else {
      DTOutput("mainstream_with_sen_reg_bench_table")
    }
  })

  output$cin_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$cin_regt_toggle == "Chart") {
      plotlyOutput("cin_reg_time")
    } else {
      DTOutput("cin_reg_time_table")
    }
  })

  output$cin_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$cin_regb_toggle == "Chart") {
      plotlyOutput("cin_reg_bench")
    } else {
      DTOutput("cin_reg_bench_table")
    }
  })

  output$provider_types_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$types_regt_toggle == "Chart") {
      plotlyOutput("provider_types_reg_time")
    } else {
      DTOutput("provider_types_reg_time_table")
    }
  })

  output$provider_types_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$types_regb_toggle == "Chart") {
      plotlyOutput("provider_types_reg_bench")
    } else {
      DTOutput("provider_types_reg_bench_table")
    }
  })

  output$ehcp_ageprofile_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$age_regt_toggle == "Chart") {
      plotlyOutput("ehcp_ageprofile_reg_time")
    } else {
      DTOutput("ehcp_ageprofile_reg_time_table")
    }
  })

  output$dsg_deficit_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$dsg_regt_toggle == "Chart") {
      plotlyOutput("dsg_deficit_reg_time")
    } else {
      DTOutput("dsg_deficit_reg_time_table")
    }
  })

  output$dsg_deficit_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$dsg_regb_toggle == "Chart") {
      plotlyOutput("dsg_deficit_reg_bench")
    } else {
      DTOutput("dsg_deficit_reg_bench_table")
    }
  })

  output$specialist_spend_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$spend_regt_toggle == "Chart") {
      plotlyOutput("specialist_spend_reg_time")
    } else {
      DTOutput("specialist_spend_reg_time_table")
    }
  })

  output$specialist_spend_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input$spend_regb_toggle == "Chart") {
      plotlyOutput("specialist_spend_reg_bench")
    } else {
      DTOutput("specialist_spend_reg_bench_table")
    }
  })

  output$la_summary_tog <- renderUI({
    if (input$la_sum_toggle == "Chart") {
      girafeOutput("summary", height = "800px")
    } else {
      DTOutput("la_summary_table")
    }
  })



  # =====================
  # Alternative provision - toggles
  # =====================

  #---------------------
  # Define ui functions
  #---------------------

  # Define a function for dynamic UI - Local authorities
  gen_la_ui <- function(toggle_input, # toggle named in dashboard_panel file
                        chart_output, # name of chart created in server file
                        table_output # name of table created in server file
  ) {
    validate(need(input$la_choice, message = "Please select LA in top menu in top menu"))
    if (input[[toggle_input]] == "Chart") {
      return(plotlyOutput(chart_output))
    } else {
      return(DTOutput(table_output))
    }
  }

  # Define a function for dynamic UI - regional/national view
  gen_reg_ui <- function(toggle_input, # toggle named in dashboard_panel file
                         chart_output, # name of chart created in server file
                         table_output # name of table created in server file
  ) {
    validate(need(input$level_choice, message = "Please select England or regional level"))
    if (input[[toggle_input]] == "Chart") {
      return(plotlyOutput(chart_output))
    } else {
      return(DTOutput(table_output))
    }
  }

  #---------------------
  # LA AP counts
  #---------------------

  output$ap_counts_la_time_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_counts_lat_toggle",
      chart_output = "ap_counts_la_time",
      table_output = "ap_counts_la_time_table"
    )
  })

  output$ap_counts_la_bench_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_counts_lab_toggle",
      chart_output = "ap_counts_la_bench",
      table_output = "ap_counts_la_bench_table"
    )
  })

  output$ap_counts_reg_time_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_counts_regt_toggle",
      chart_output = "ap_counts_reg_time",
      table_output = "ap_counts_reg_time_table"
    )
  })

  output$ap_counts_reg_bench_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_counts_regb_toggle",
      chart_output = "ap_counts_reg_bench",
      table_output = "ap_counts_reg_bench_table"
    )
  })

  #---------------------
  # AP characteristics
  #---------------------

  output$ap_characteristics_la_time_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_characteristics_lat_toggle",
      chart_output = "ap_characteristics_la_time",
      table_output = "ap_characteristics_la_time_table"
    )
  })

  output$ap_characteristics_la_bench_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_characteristics_lab_toggle",
      chart_output = "ap_characteristics_la_bench",
      table_output = "ap_characteristics_la_bench_table"
    )
  })


  output$ap_characteristics_reg_time_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_characteristics_regt_toggle",
      chart_output = "ap_characteristics_reg_time",
      table_output = "ap_characteristics_reg_time_table"
    )
  })

  output$ap_characteristics_reg_bench_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_characteristics_regb_toggle",
      chart_output = "ap_characteristics_reg_bench",
      table_output = "ap_characteristics_reg_bench_table"
    )
  })


  #---------------------
  # AP absences
  #---------------------

  output$ap_absences_la_time_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_absences_lat_toggle",
      chart_output = "ap_absences_la_time",
      table_output = "ap_absences_la_time_table"
    )
  })

  output$ap_absences_la_bench_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_absences_lab_toggle",
      chart_output = "ap_absences_la_bench",
      table_output = "ap_absences_la_bench_table"
    )
  })

  output$ap_absences_reg_time_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_absences_regt_toggle",
      chart_output = "ap_absences_reg_time",
      table_output = "ap_absences_reg_time_table"
    )
  })

  output$ap_absences_reg_bench_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_absences_regb_toggle",
      chart_output = "ap_absences_reg_bench",
      table_output = "ap_absences_reg_bench_table"
    )
  })

  #---------------------
  # AP Ofsted
  #---------------------
  output$ap_ofsted_la_time_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_ofsted_lat_toggle",
      chart_output = "ap_ofsted_la_time",
      table_output = "ap_ofsted_la_time_table"
    )
  })

  output$ap_ofsted_la_bench_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_ofsted_lab_toggle",
      chart_output = "ap_ofsted_la_bench",
      table_output = "ap_ofsted_la_bench_table"
    )
  })

  output$ap_ofsted_reg_time_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_ofsted_regt_toggle",
      chart_output = "ap_ofsted_reg_time",
      table_output = "ap_ofsted_reg_time_table"
    )
  })

  output$ap_ofsted_reg_bench_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_ofsted_regb_toggle",
      chart_output = "ap_ofsted_reg_bench",
      table_output = "ap_ofsted_reg_bench_table"
    )
  })

  #---------------------
  # AP UAP
  #---------------------
  output$ap_uap_la_time_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_uap_lat_toggle",
      chart_output = "ap_uap_la_time",
      table_output = "ap_uap_la_time_table"
    )
  })

  output$ap_uap_la_bench_tog <- renderUI({
    gen_la_ui(
      toggle_input = "ap_uap_lab_toggle",
      chart_output = "ap_uap_la_bench",
      table_output = "ap_uap_la_bench_table"
    )
  })

  output$ap_uap_reg_time_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_uap_regt_toggle",
      chart_output = "ap_uap_reg_time",
      table_output = "ap_uap_reg_time_table"
    )
  })

  output$ap_uap_reg_bench_tog <- renderUI({
    gen_reg_ui(
      toggle_input = "ap_uap_regb_toggle",
      chart_output = "ap_uap_reg_bench",
      table_output = "ap_uap_reg_bench_table"
    )
  })


  # MISC TEMPLATE CODE ---------------------------------------------------------------------------

  output$boxavgRevBal <- renderValueBox({
    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0("£", format(
        (reactiveRevBal() %>% filter(
          year == max(year),
          area_name == input$selectArea,
          school_phase == input$selectPhase
        ))$average_revenue_balance,
        big.mark = ","
      )),
      # add subtitle to explain what it's showing
      paste0("This is the latest value for the selected inputs"),
      color = "blue"
    )
  })

  output$boxpcRevBal <- renderValueBox({
    latest <- (reactiveRevBal() %>% filter(
      year == max(year),
      area_name == input$selectArea,
      school_phase == input$selectPhase
    ))$average_revenue_balance
    penult <- (reactiveRevBal() %>% filter(
      year == max(year) - 1,
      area_name == input$selectArea,
      school_phase == input$selectPhase
    ))$average_revenue_balance

    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0("£", format(latest - penult,
        big.mark = ","
      )),
      # add subtitle to explain what it's hsowing
      paste0("Change on previous year"),
      color = "blue"
    )
  })

  graphs_dependent_on_la_choice <- c(
    "ks2_attainment_la_time",
    "ks1_phonics_la_time",
    "ks4_attainment_la_time",
    "ofsted_la_ratings",
    "destinations_1618_la_time",
    "timeliness_la_time",
    "tribunals_la_time",
    "absence_la_time",
    "ks4_destinations_la_time",
    "dsg_deficit_la_time",
    "specialist_spend_la_time",
    "percent_pupils_ehcp_la_time",
    "mainstream_with_sen_la_time",
    "provider_types_la_time",
    "ehcp_ageprofile_la_time",
    "ks2_attainment_la_bench",
    "ks1_phonics_la_bench",
    "ks4_attainment_la_bench",
    "destinations_1618_la_bench",
    "timeliness_la_bench",
    "tribunals_la_bench",
    "absence_la_bench",
    "ks4_destinations_la_bench",
    "dsg_deficit_la_bench",
    "specialist_spend_la_bench",
    "percent_pupils_ehcp_la_time",
    "mainstream_with_sen_la_bench",
    "provider_types_la_bench",
    "ap_counts_la_time",
    "ap_counts_la_bench",
    "ap_characteristics_la_time",
    "ap_characteristics_la_bench",
    "ap_absences_la_time",
    "ap_absences_la_bench",
    "ap_ofsted_la_time",
    "ap_ofsted_la_bench",
    "ap_uap_la_time",
    "ap_uap_la_bench"
  )

  graphs_dependent_on_ccg_choice <- c(
    "autism_ccg_time",
    "mentalhealth_ccg_time",
    "autism_ccg_bench",
    "mentalhealth_ccg_bench"
  )

  england_menu_items <- c("level_choice", "region_choice_out")


  mytoggle_la <- function(id, condition) {
    shinyjs::toggle(
      id = id,
      condition = (input$la_choice != "" & !is.na(input$la_choice)),
      anim = TRUE,
      animType = "fade",
      time = 0.5
    )
  }

  mytoggle_ccg <- function(id, condition) {
    shinyjs::toggle(
      id = id,
      condition = (input$ccg_choice != "" & !is.na(input$ccg_choice)),
      anim = TRUE,
      animType = "fade",
      time = 0.5
    )
  }


  # mytoggle_englandmenus <- function(id, condition) {
  #    shinyjs::toggle(
  #      id = id,
  #      condition = (input$tabsetpanels_reg != "Summary"),
  #      anim = TRUE,
  #      animType = "fade",
  #      time = 0.5
  #    )
  #  }



  observe({
    #  purrr::map(england_menu_items, mytoggle_englandmenus)
    purrr::map(graphs_dependent_on_la_choice, mytoggle_la)
    purrr::map(graphs_dependent_on_ccg_choice, mytoggle_ccg)
  })




  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
