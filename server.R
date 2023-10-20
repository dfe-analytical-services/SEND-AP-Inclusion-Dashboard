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


  # observers to turn the summary boxes into links
  # procedure uses three observers:
  # 1. Switches tabs, sets link target
  # 2. Fires on tab switch, copies target and triggers #3 (this one is necessary because otherwise the scroll fires before the tab switches)
  # 3. Scrolls to target and resets target variable

  # 1. needs to be different for all of the boxes as it has a different input parameter; for the other two we only need one observer each
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
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of Need")
    updateTabsetPanel(session, "percent_pupils_ehcp_reg_panel", selected = "Change over time")
    this_tab("Identification of Need")
    move_target("percent_pupils_ehcp_reg_panel")
  })

  observeEvent(input$link_mainstream_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of Need")
    updateTabsetPanel(session, "mainstream_with_sen_reg_panel", selected = "Change over time")
    this_tab("Identification of Need")
    move_target("mainstream_with_sen_reg_panel")
  })

  observeEvent(input$link_special_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Identification of Need")
    updateTabsetPanel(session, "provider_types_reg_panel", selected = "Change over time")
    this_tab("Identification of Need")
    move_target("provider_types_reg_panel")
  })

  observeEvent(input$link_deficit_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Financial Sustainability")
    updateTabsetPanel(session, "dsg_deficit_reg_panel", selected = "Change over time")
    this_tab("Financial Sustainability")
    move_target("dsg_deficit_reg_panel")
  })

  observeEvent(input$link_spend_reg_panel, {
    updateTabsetPanel(session, "tabsetpanels_reg", selected = "Financial Sustainability")
    updateTabsetPanel(session, "specialist_spend_reg_panel", selected = "Change over time")
    this_tab("Financial Sustainability")
    move_target("specialist_spend_reg_panel")
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
        y = `Percent meeting expected standards`,
        group = characteristic,
        colour = characteristic
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% of pupils meeting expected standards",
        x = "Academic year",
        colour = "Group"
      ) +
      scale_colour_manual(values = af_palette) +
      scale_x_date(breaks = c(ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")), labels = c("2018/19", "2019/20", "2020/21", "2021/22", "2022/23"))

    ks2_attainment_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks2_attainment_la_time_table <- renderTable({
    ks2_attainment_lat <- ks2_attainment %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(
        `Academic Year` = academic_year,
        `SEN Status` = characteristic,
        `Percent meeting expected standards`
      ) %>%
      arrange(
        `Academic Year`,
        `SEN Status`
      )
    return(ks2_attainment_lat)
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
      add_ranks(outcome = "Percent meeting expected standards")

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
      mutate(outcome = `Percent meeting expected standards`)

    # Create the plot
    ks2_attainment_la_bench <- ks2_attainment_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `Percent meeting expected standards`),
        y = `Percent meeting expected standards`,
        text = la_name,
        fill = chosen_la
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = ks2_attainment_la_bench_data$rank_statement[ks2_attainment_la_bench_data$la_name == input$la_choice],
        y = "% of pupils meeting expected standards",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)

    # Pass plot into plotly
    ks2_attainment_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      # This code makes the dotted line a bit thinner.
      # You can use plotly_json() to examine the inner workings of plotly graphs to work out trace numbers
      restyle_england_line() %>% # Make the England line label align correctly and thin the line
      save_plot_button_only() %>%
      use_horizontal_legend()
  })

  output$ks2_attainment_la_bench_table <- renderTable({
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
        `Percent meeting expected standards`
      ) %>%
      arrange(desc(`Percent meeting expected standards`)) %>%
      return(ks2_attainment_lab)
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
          y = `Percent meeting expected standards`,
          group = characteristic,
          colour = characteristic
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils meeting expected standards",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_x_date(
          breaks = c(ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")),
          labels = c("2018/19", "2019/20", "2020/21", "2021/22")
        )
    } else if (input$level_choice == "England") {
      ks2_attainment_reg_time <- ks2_attainment %>%
        filter(geographic_level == "National") %>%
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Percent meeting expected standards`,
          group = characteristic,
          colour = characteristic
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of pupils meeting expected standards",
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_x_date(breaks = c(ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")), labels = c("2018/19", "2019/20", "2020/21", "2021/22"))
    }

    ks2_attainment_reg_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks2_attainment_reg_time_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks2_attainment_ret <- ks2_attainment %>%
        fsubset(region_name == input$region_choice &
          geographic_level == "Regional") %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          `Percent meeting expected standards`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN Status`
        )

      return(ks2_attainment_ret)
    } else {
      ks2_attainment_et <- ks2_attainment %>%
        fsubset(geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN Status` = characteristic,
          `Percent meeting expected standards`
        ) %>%
        arrange(
          `Academic Year`,
          `SEN Status`
        )
      return(ks2_attainment_et)
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
      mutate(outcome = `Percent meeting expected standards`) %>%
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
      ggplot(aes(fct_reorder(region_name, `Percent meeting expected standards`),
        y = `Percent meeting expected standards`,
        text = region_name,
        fill = chosen_region
      )) +
      geom_col() +
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 3) +
      labs(
        x = "Regions in England",
        y = "% of pupils meeting expected standards",
        fill = "Region"
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = 1
      ), legend.position = "none") +
      if (input$level_choice == "England") {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      } else {
        scale_fill_manual(values = c("Other regions" = af_purple), na.value = af_grey)
      }


    ks2_attainment_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>% # Make the England line label align correctly and thin the line
      use_horizontal_legend()
  })

  output$ks2_attainment_reg_bench_table <- renderTable({
    ks2_attainment_reb <- ks2_attainment %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks2_attainment_reg_filter &
        time_period == max(time_period)) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN Status` = characteristic,
        `Percent meeting expected standards`
      ) %>%
      arrange(
        `Academic Year`,
        desc(`Percent meeting expected standards`)
      )

    return(ks2_attainment_reb)
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
        colour = characteristic
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
        breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")),
        labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")
      )

    ks1_phonics_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks1_phonics_la_time_table <- renderTable({
    ks1_phonics_la_tt <- ks1_phonics %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(academic_year, la_name, characteristic, `Percent meeting expected standards in Y1`) %>%
      arrange(academic_year, characteristic)

    names(ks1_phonics_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(ks1_phonics_la_tt)
  })

  ## KS1 Phonics (LA/bench)
  output$ks1_phonics_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, ks1_phonics)
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks1_phonics %>%
      collapse::fsubset(characteristic == input$ks1_phonics_la_filter &
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
        text = la_name,
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
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)


    ks1_phonics_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$ks1_phonics_la_bench_table <- renderTable({
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
      arrange(desc(`Percent meeting expected standards in Y1`)) %>%
      return(ks1_attainment_lab)
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
          colour = characteristic
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
          breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")),
          labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")
        )
    } else {
      ks1_phonics_reg_time <- ks1_phonics %>%
        filter(geographic_level == "National") %>%
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Percent meeting expected standards in Y1`,
          group = characteristic,
          colour = characteristic
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
          breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")),
          labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")
        )
    }

    ks1_phonics_reg_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      save_plot_button_only()
  })

  output$ks1_phonics_reg_time_table <- renderTable({
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

      return(ks1_phonics_ret)
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
      return(ks1_phonics_et)
    }
  })

  ## KS1 Phonics (region/bench)
  output$ks1_phonics_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_ks1_phonics %>%
      collapse::fsubset(characteristic == input$ks1_phonics_reg_filter &
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
        text = region_name,
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
        scale_fill_manual(values = c("Other region" = af_purple), na.value = af_grey)
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }


    ks1_phonics_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$ks1_phonics_reg_bench_table <- renderTable({
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

    return(ks1_phonics_reb)
  })

  ### KS4 Attainment ####

  ## KS4 Attainment(LA/time)
  output$ks4_attainment_la_time <- renderPlotly({
    # req(input$la_choice)

    ks4_attainment_la_time <- ks4_attainment %>%
      drop_na(`Average progress 8 score`) %>%
      fsubset(la_name == input$la_choice) %>%
      AY_to_date(academic_year) %>%
      ggplot(aes(
        x = AY_date,
        y = `Average progress 8 score`,
        ymin = `Progress 8 score (lower confidence interval)`,
        ymax = `Progress 8 score (upper confidence interval)`,
        group = `SEN provision`,
        colour = `SEN provision`,
        fill = `SEN provision`
      )) +
      geom_ribbon(alpha = 0.2, outline.type = "full") +
      geom_point() +
      geom_line() +
      labs(
        y = "Average progress 8 score",
        x = "Academic year"
      ) +
      scale_colour_manual(values = af_palette) +
      scale_fill_manual(values = af_palette) +
      scale_x_date(
        breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")),
        labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")
      )
    if (nrow(ks4_attainment_la_time$data) > 0) {
      ks4_attainment_la_time %>%
        ggplotly(
          tooltip = c("text", "y", "x", "colour")
        ) %>%
        save_plot_button_only() %>%
        use_horizontal_legend()
    }
  })

  output$ks4_attainment_la_time_table <- renderTable({
    ks4_attainment_la_tt <- ks4_attainment %>%
      fsubset(la_name == input$la_choice) %>%
      fselect(academic_year, la_name, `SEN provision`, `Average progress 8 score`, `Progress 8 score (lower confidence interval)`, `Progress 8 score (upper confidence interval)`) %>%
      arrange(academic_year, `SEN provision`)

    names(ks4_attainment_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(ks4_attainment_la_tt)
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
          text = la_name,
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
        scale_colour_manual(values = c("Other LAs" = af_purple), na.value = af_grey)

      ks4_attainment_la_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        restyle_england_line() %>% # Make the England line label align correctly and thin the line
        save_plot_button_only() %>%
        use_horizontal_legend()
    }
  })

  output$ks4_attainment_la_bench_table <- renderTable({
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
      arrange(desc(`Average progress 8 score`)) %>%
      return(ks4_attainment_lab)
  })

  ## KS4 Attainment(region/time)
  output$ks4_attainment_reg_time <- renderPlotly({
    # req(input$level_choice)
    # if(input$level_choice == "Regions") req(input$region_choice)

    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, "Please select a region"))

      ks4_attainment_reg_time <- ks4_attainment %>%
        filter(
          region_name == input$region_choice,
          geographic_level == "Regional"
        ) %>%
        AY_to_date(academic_year) %>%
        drop_na(`Average progress 8 score`) %>% # Allows a ribbon to be drawn between years
        ggplot(aes(
          x = AY_date,
          y = `Average progress 8 score`,
          ymin = `Progress 8 score (lower confidence interval)`,
          ymax = `Progress 8 score (upper confidence interval)`,
          group = `SEN provision`,
          colour = `SEN provision`,
          fill = `SEN provision`
        )) +
        geom_ribbon(alpha = 0.2, outline.type = "full") +
        geom_point() +
        geom_line() +
        labs(
          y = "Average progress 8 score",
          x = "Academic year"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_fill_manual(values = af_palette) +
        scale_x_date(
          breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01"), ymd("2022-09-01")),
          labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
        )
    } else {
      ks4_attainment_reg_time <- ks4_attainment %>%
        filter(geographic_level == "National") %>%
        drop_na(`Average progress 8 score`) %>% # Allows a ribbon to be drawn between years
        AY_to_date(academic_year) %>%
        ggplot(aes(
          x = AY_date,
          y = `Average progress 8 score`,
          ymin = `Progress 8 score (lower confidence interval)`,
          ymax = `Progress 8 score (upper confidence interval)`,
          group = `SEN provision`,
          colour = `SEN provision`,
          fill = `SEN provision`
        )) +
        geom_ribbon(alpha = 0.2, outline.type = "full") +
        geom_point() +
        geom_line() +
        labs(
          y = "Average progress 8 score",
          x = "Academic year"
        ) +
        scale_colour_manual(values = af_palette) +
        scale_fill_manual(values = af_palette) +
        scale_x_date(
          breaks = c(ymd("2015-09-01"), ymd("2016-09-01"), ymd("2017-09-01"), ymd("2018-09-01"), ymd("2019-09-01"), ymd("2020-09-01"), ymd("2021-09-01")),
          labels = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22")
        )
    }


    ks4_attainment_reg_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ks4_attainment_reg_time_table <- renderTable({
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

      return(ks4_att_ret)
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
      return(ks4_att_et)
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
        text = region_name,
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
        scale_colour_manual(values = c("Other regions" = af_purple), na.value = af_grey)
      } else {
        scale_colour_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }


    ks4_attainment_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
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

  output$ks4_attainment_reg_bench_table <- renderTable({
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

    return(ks4_att_reb)
  })
  ### KS4 destinations ####

  ## KS4 destinations (LA/time)
  output$ks4_destinations_la_time <- renderPlotly({
    # req(input$la_choice)

    ks4_destinations_la_time <- ks4_destinations %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$ks4_destinations_la_time_filter) %>%
      ggplot(aes(
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
        position = position_stack(vjust = 0.5)
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

  output$ks4_destinations_la_time_table <- renderTable({
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
    return(ks4_destinations_la_tt)
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
        scale_y_continuous(limits = c(0, 100)) +
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

  output$ks4_destinations_la_type_table <- renderTable({
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
    return(ks4_destinations_la_typet)
  })

  ### 16-18 destinations ####

  ## 16-18 destinations (LA/time)
  output$destinations_1618_la_time <- renderPlotly({
    # req(input$la_choice)

    destinations_1618_la_time <- destinations_1618 %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$destinations_1618_la_time_filter) %>%
      ggplot(aes(
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
        position = position_stack(vjust = 0.5)
      ) +
      labs(
        x = "Academic year",
        fill = "Destination",
        label = "% of pupils"
      ) +
      scale_fill_manual(values = dest_1618_palette)

    destinations_1618_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "fill")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "v"),
        dragmode = FALSE
      )
  })

  output$destinations_1618_la_time_table <- renderTable({
    destinations_1618_la_tt <- destinations_1618 %>%
      fsubset(la_name == input$la_choice &
        characteristic == input$destinations_1618_la_time_filter) %>%
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

    names(destinations_1618_la_tt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(destinations_1618_la_tt)
  })

  ## 16-18 destinations (LA/type)
  output$destinations_1618_la_type <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(destinations_1618))
    # req(input$la_choice)
    if (nrow(fsubset(destinations_1618, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      destinations_1618_la_type <- destinations_1618 %>%
        fsubset(la_name == input$la_choice &
          Destination == input$destinations_1618_la_type_filter) %>%
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
        scale_y_continuous(limits = c(0, 100)) +
        scale_colour_manual(values = af_palette)
      # facet_wrap(~characteristic_group)

      destinations_1618_la_type %>%
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

  output$destinations_1618_la_type_table <- renderTable({
    destinations_1618_la_typet <- destinations_1618 %>%
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
    return(destinations_1618_la_typet)
  })

  # This one (LA 16-18 destination benchmarking) is a lot of information to get across.
  # Adding extra elements e.g. transparency clutters up the legend.
  # Only feasible option seems to restrict comparison across the region, which seems reasonable given
  # the density of the information.
  # Ideally a national average would also be added.

  ## 16-18 destinations (LA/bench)
  output$destinations_1618_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(destinations_1618))
    if (nrow(fsubset(destinations_1618, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      # req(input$la_choice)
      destinations_1618_la_bench <- destinations_1618 %>%
        collapse::fsubset(geographic_level == "Local authority" &
          !(la_name %in% small_LAs) &
          characteristic == input$destinations_1618_la_bench_filter &
          time_period == comparison_year) %>%
        collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
          yes = input$la_choice,
          no = "Other LA"
        )) %>%
        fsubset(region_name == region_name[la_name == input$la_choice]) %>%
        ggplot(aes(
          y = la_name,
          x = `% of pupils`,
          text = la_name,
          label = ifelse(test = `% of pupils` > 2.5,
            yes = paste0(round(`% of pupils`, 0), "%"),
            no = NA
          ),
          fill = Destination
        )) +
        geom_col() +
        geom_text(
          size = 2.5,
          position = position_stack(vjust = 0.5)
        ) +
        #   theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = dest_1618_palette) +
        scale_x_continuous(position = "top")

      # make y-axis label
      ylabel <- paste0(
        "Local authorities in ", destinations_1618$region_name[destinations_1618$la_name == input$la_choice],
        " in ", max(destinations_1618$academic_year)
      )[1]

      destinations_1618_la_bench %>%
        ggplotly(
          tooltip = c("text", "x"),
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(
            orientation = "h",
            traceorder = "reversed"
          ),
          dragmode = FALSE,
          yaxis = list(title = ylabel),
          xaxis = list(title = "% of students"),
          legend = list(title = "Destination")
        )
    }
  })

  output$destinations_1618_la_bench_table <- renderTable({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(destinations_1618))

    destinations_1618_la_bt <- destinations_1618 %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$destinations_1618_la_bench_filter &
        time_period == comparison_year &
        region_name == region_name[la_name == input$la_choice]) %>%
      arrange(
        Destination,
        `% of pupils`
      ) %>%
      fselect(
        academic_year,
        la_name,
        characteristic,
        Destination,
        `% of pupils`
      )

    names(destinations_1618_la_bt)[1:3] <- c("Academic Year", "Local Authority", "SEN Status")
    return(destinations_1618_la_bt)
  })

  ## 16-18 destinations (region/time)
  output$destinations_1618_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, "Please select a region"))
      destinations_1618_reg_time <- destinations_1618 %>%
        filter(
          characteristic == input$destinations_1618_reg_time_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        ggplot(aes(
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
          position = position_stack(vjust = 0.5)
        ) +
        labs(
          x = "Academic year",
          fill = "Destination",
          label = "% of pupils"
        ) +
        scale_fill_manual(values = dest_1618_palette)

      destinations_1618_reg_time %>%
        ggplotly(
          tooltip = c("text", "y", "x", "fill")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    } else {
      destinations_1618_reg_time <- destinations_1618_nat %>%
        filter(characteristic == input$destinations_1618_reg_time_filter) %>%
        ggplot(aes(
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
          position = position_stack(vjust = 0.5)
        ) +
        labs(
          x = "Academic year",
          fill = "Destination",
          label = "% of pupils"
        ) +
        scale_fill_manual(values = dest_1618_palette)

      destinations_1618_reg_time %>%
        ggplotly(
          tooltip = c("text", "y", "x", "fill")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    }
  })

  output$destinations_1618_reg_time_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      destinations_1618_reg_tt <- destinations_1618 %>%
        filter(
          characteristic == input$destinations_1618_reg_time_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          `Region` = region_name,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )

      return(destinations_1618_reg_tt)
    } else {
      destinations_1618_reg_tt <- destinations_1618_nat %>%
        fsubset(characteristic == input$destinations_1618_reg_time_filter &
          geographic_level == "National") %>%
        ftransform(Region = "England") %>%
        fselect(
          `Academic Year` = academic_year,
          Region,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )
      return(destinations_1618_reg_tt)
    }
  })
  ## 16-18 destinations (region/type)
  output$destinations_1618_reg_type <- renderPlotly({
    # req(input$level_choice)
    if (input$level_choice == "Regions") {
      req(input$region_choice)
      destinations_1618_reg_type <- destinations_1618 %>%
        filter(
          Destination == input$destinations_1618_reg_type_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
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
        scale_y_continuous(limits = c(0, 100)) +
        scale_colour_manual(values = af_palette)
      # facet_wrap(~characteristic_group)

      destinations_1618_reg_type %>%
        ggplotly(
          tooltip = c("text", "y", "x", "colour")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      destinations_1618_reg_type <- destinations_1618_nat %>%
        filter(Destination == input$destinations_1618_reg_type_filter) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic
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

      destinations_1618_reg_type %>%
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

  output$destinations_1618_reg_type_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      destinations_1618_reg_typet <- destinations_1618 %>%
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
          `% of pupils`
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
          `% of pupils`
        ) %>%
        arrange(
          `SEN Status`,
          `Academic Year`
        )
    }

    return(destinations_1618_reg_typet)
  })

  ## 16-18 destinations (region/bench)
  output$destinations_1618_reg_bench <- renderPlotly({
    # req(input$level_choice)
    # if(input$level_choice == "Regions") req(input$region_choice)
    destinations_1618_reg_bench1 <- destinations_1618 %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$destinations_1618_reg_bench_filter &
        time_period == max(time_period))

    if (input$level_choice == "Regions" & is.character(input$region_choice)) {
      destinations_1618_reg_bench_data <- destinations_1618_reg_bench1 %>%
        mutate(chosen_region = case_when(
          region_name == input$region_choice ~
            input$region_choice,
          TRUE ~ "Other region"
        ))
    } else {
      destinations_1618_reg_bench_data <- destinations_1618_reg_bench1 %>%
        mutate(chosen_region = "English regions")
    }

    destinations_1618_reg_bench <- destinations_1618_reg_bench_data %>%
      collapse::ftransform(
        region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
      ) %>%
      ggplot(aes(
        x = region_name,
        y = `% of pupils`,
        text = region_name,
        label = ifelse(test = `% of pupils` > 2.5,
          yes = paste0(round(`% of pupils`, 0), "%"),
          no = NA
        ),
        fill = Destination
      )) +
      geom_col() +
      geom_text(
        size = 2.5,
        position = position_stack(vjust = 0.5)
      ) +
      labs(
        y = "Region",
        x = "% of pupils",
        fill = "Destination"
      ) +
      scale_fill_manual(values = dest_1618_palette) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), legend.position = "right")


    destinations_1618_reg_bench %>%
      ggplotly(
        tooltip = c("text", "x", "fill")
      ) %>%
      config(
        displaylogo = FALSE,
        toImageButtonOptions = list(format = "svg"),
        modeBarButtons = list(list("toImage"))
      ) %>%
      layout(
        legend = list(orientation = "v"),
        dragmode = FALSE
      )
  })

  output$destinations_1618_reg_bench_table <- renderTable({
    destinations_1618_reg_bt <- destinations_1618 %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$destinations_1618_reg_bench_filter &
        academic_year == max(academic_year)) %>%
      arrange(
        academic_year,
        Destination,
        `% of pupils`
      ) %>%
      fselect(academic_year,
        Region = region_name,
        characteristic,
        Destination,
        `% of pupils`
      )

    names(destinations_1618_reg_bt)[1:3] <- c("Academic Year", "Region", "SEN Status")
    return(destinations_1618_reg_bt)
  })

  ### Ofsted ratings ####

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
          color = "#161616",
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
    }
  })

  # EXPERIENCES GRAPHS ---------------------------------------------------------------------------

  ### EHCP Timeliness ####

  ## EHCP Timeliness (LA/time)
  output$timeliness_la_time <- renderPlotly({
    timeliness_la_time <- ehcp_timeliness %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = time_period,
        y = `% of EHCPs issued within 20 weeks`
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% of EHCPs issued within 20 weeks",
        x = "Calendar year"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_x_discrete(limits = unique(ehcp_timeliness$time_period))

    timeliness_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$timeliness_la_time_table <- renderTable({
    timeliness_la_tt <- ehcp_timeliness %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(time_period) %>%
      ftransform(
        Year = as.character(round(time_period, 0)), # stop it being 2021.00 etc
        `% of EHCPs issued within 20 weeks` = as.character(round(`% of EHCPs issued within 20 weeks`, 1))
      ) %>%
      fselect(Year,
        `Local Authority` = la_name,
        `% of EHCPs issued within 20 weeks`
      )

    return(timeliness_la_tt)
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
        text = la_name,
        fill = chosen_la
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = timeliness_la_bench_data$rank_statement[timeliness_la_bench_data$la_name == input$la_choice],
        y = "% of EHCPs issued within 20 weeks",
        fill = "Local Authority"
      ) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)


    timeliness_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$timeliness_la_bench_table <- renderTable({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ehcp_timeliness))

    timeliness_la_bt <- ehcp_timeliness %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        time_period == comparison_year) %>%
      arrange(desc(`% of EHCPs issued within 20 weeks`)) %>%
      ftransform(
        Year = as.character(round(time_period, 0)), # stop it being 2021.00 etc
        `% of EHCPs issued within 20 weeks` = as.character(round(`% of EHCPs issued within 20 weeks`, 1))
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

    return(timeliness_la_bt)
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
        y = `% of EHCPs issued within 20 weeks`
      )) +
      geom_line() +
      geom_point() +
      labs(x = "Calendar year") +
      scale_y_continuous(limits = c(0, 100))

    timeliness_reg_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$timeliness_reg_time_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      timeliness_reg_tt <- ehcp_timeliness %>%
        fsubset(
          geographic_level == "Regional" &
            region_name == input$region_choice
        ) %>%
        arrange(time_period) %>%
        ftransform(
          Year = as.character(round(time_period, 0)), # stop it being 2021.00 etc
          `% of EHCPs issued within 20 weeks` = as.character(round(`% of EHCPs issued within 20 weeks`, 1))
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
          Year = as.character(round(time_period, 0)), # stop it being 2021.00 etc
          `% of EHCPs issued within 20 weeks` = as.character(round(`% of EHCPs issued within 20 weeks`, 1))
        ) %>%
        fselect(
          Year,
          Region,
          `% of EHCPs issued within 20 weeks`
        )
    }
    return(timeliness_reg_tt)
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
          text = region_name,
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
          values = c("Other region" = af_purple), na.value = af_grey
        )


      timeliness_reg_bench %>%
        ggplotly(
          tooltip = c("text", "y")
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
          text = region_name,
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
          values = c("English regions" = af_purple), na.value = af_grey
        )

      timeliness_reg_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line()
    }
  })

  output$timeliness_reg_bench_table <- renderTable({
    timeliness_reg_bt <- ehcp_timeliness %>%
      collapse::fsubset(geographic_level == "Regional" &
        time_period == max(time_period)) %>%
      arrange(desc(`% of EHCPs issued within 20 weeks`)) %>%
      ftransform(
        Year = as.character(round(time_period, 0)), # stop it being 2021.00 etc
        `% of EHCPs issued within 20 weeks` = as.character(round(`% of EHCPs issued within 20 weeks`, 1))
      ) %>%
      fselect(Year,
        Region = region_name,
        `% of EHCPs issued within 20 weeks`
      )

    return(timeliness_reg_bt)
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
          colour = `Age group`
        )) +
        geom_line() +
        geom_point() +
        labs(
          y = "% of patients seen after more than 13 weeks",
          x = "Reporting dates"
        ) +
        scale_colour_manual(values = c("dodgerblue", "forestgreen", "purple4"))


      autism_ccg_time %>%
        ggplotly(
          tooltip = c("text", "y", "x")
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

  output$autism_ccg_time_table <- renderTable({
    validate(need(input$ccg_choice, message = "Select NHS area to display this graph."))

    if (input$ccg_choice %in% autism$nhs_name) {
      autism_ccg_tt <- autism %>%
        fsubset(nhs_name == input$ccg_choice &
          nhs_type == "Former CCG area") %>%
        ftransform(
          Month = format(date, "%B %Y"), # stop renderTable linewrapping the date
          `Age group` = factor(`Age group`, levels = c("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24")),
          `% with first appointment\n after more than 13 weeks` = as.character(round(`% with first appointment after more than 13 weeks`, 1))
        ) %>%
        arrange(
          `Age group`,
          date
        ) %>%
        fselect(`Month`,
          `Former CCG area` = nhs_name,
          `Age group`,
          `% with first appointment\n after more than 13 weeks`
        )
    }
    return(autism_ccg_tt)
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
          text = nhs_name,
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
        scale_fill_manual(values = c("Other CCG" = af_purple), na.value = af_grey) +
        facet_wrap(~`Age group`,
          scales = "free_x",
          nrow = 3
        ) +
        theme(panel.border = element_rect(fill = NA))

      autism_ccg_bench %>%
        ggplotly(
          tooltip = c("text", "y")
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

  output$autism_ccg_bench_table <- renderTable({
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
      ftransform(
        Month = format(date, "%B %Y"), # use month name to stop renderTable linewrapping the date
        `% with first appointment\n after more than 13 weeks` = as.character(round(`% with first appointment after more than 13 weeks`, 1))
      ) %>%
      fselect(Month,
        `Former CCG area` = nhs_name,
        `Age group`,
        `% with first appointment\n after more than 13 weeks`
      )

    return(autism_ccg_b)
  })

  ## Autism Waiting Times (National/time)
  # No regional breakdown for this data - only national and provider

  output$autism_nat_time <- renderPlotly({
    autism_nat_time <- autism %>%
      fsubset(BREAKDOWN == "Age Group") %>%
      ggplot(aes(
        x = date,
        y = `% with first appointment after more than 13 weeks`,
        colour = `Age group`
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% seen after more than 13 weeks",
        x = "Reporting dates"
      ) +
      scale_colour_manual(values = c("dodgerblue", "forestgreen", "purple4"))

    autism_nat_time %>%
      ggplotly(
        tooltip = c("text", "y", "x")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$autism_nat_time_table <- renderTable({
    autism_nat_tt <- autism %>%
      fsubset(BREAKDOWN == "Age Group") %>%
      ftransform(
        Month = format(date, "%B %Y"), # stop renderTable linewrapping the date
        `Age group` = factor(`Age group`, levels = c("Age: Under 10", "Age: 10 to 17", "Age: 18 to 24")),
        `% with first appointment\n after more than 13 weeks` = as.character(round(`% with first appointment after more than 13 weeks`, 1)),
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
        `% with first appointment\n after more than 13 weeks`
      )

    return(autism_nat_tt)
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
        nhs_type == "Provider" &
        `Age group` == input$autism_nat_bench_filter)

    autism_nat_bench <- autism_nat_bench_data %>%
      ggplot(aes(
        y = fct_reorder(
          nhs_name,
          -`% with first appointment after more than 13 weeks`
        ),
        x = `% with first appointment after more than 13 weeks`,
        text = nhs_name
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
        tooltip = c("text", "x")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$autism_nat_bench_table <- renderTable({
    autism_nat_b <- autism %>%
      collapse::fsubset(date == max(date) &
        nhs_type == "Provider" &
        `Age group` == input$autism_nat_bench_filter) %>%
      arrange(`% with first appointment after more than 13 weeks`) %>%
      fmutate(
        Month = format(date, "%B %Y"), # use month name to stop renderTable linewrapping the date
        `% with first appointment\n after more than 13 weeks` = as.character(round(`% with first appointment after more than 13 weeks`, 1))
      ) %>%
      fselect(Month,
        Provider = nhs_name,
        `Age group`,
        `% with first appointment\n after more than 13 weeks`
      )

    return(autism_nat_b)
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

  output$mentalhealth_ccg_time_table <- renderTable({
    validate(need(input$ccg_choice, message = "Select NHS area to display this graph"))

    if (is.numeric(mentalhealth$`Number of children and young people`[mentalhealth$nhs_name == input$ccg_choice & mentalhealth$`Year ending` == max(mentalhealth$`Year ending`)])) {
      mentalhealth_ccg_tt <- mentalhealth %>%
        fsubset((nhs_name == input$ccg_choice &
          nhs_type == "Former CCG area") |
          PRIMARY_LEVEL == "England") %>%
        arrange(
          desc(nhs_name),
          `Year ending`
        ) %>%
        fmutate(
          `Year ending` = format(`Year ending`, "%B %Y"),
          `Number of children and young people` = as.character(round(`Number of children and young people`), 0)
        ) %>%
        fselect(`Year ending`,
          `Former CCG area` = nhs_name,
          `Number of children and young people`
        )
    }
    return(mentalhealth_ccg_tt)
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
        na.value = af_grey
      ) +
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

  output$mentalhealth_ccg_bench_table <- renderTable({
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
        `Number of children and young people` = as.character(round(`Number of children and young people`, 0))
      ) %>% # use month name to stop renderTable linewrapping the date
      fselect(`Year ending`,
        `Former CCG area` = nhs_name,
        `Number of children and young people`
      )

    return(mentalhealth_ccg_bt)
  })

  ## Mental Health Access (region/bench)
  output$mentalhealth_reg_bench <- renderPlotly({
    if (input$level_choice == "Regions") {
      # req(input$nhs_region_choice)
      mentalhealth_reg_bench <- mentalhealth %>%
        fsubset(BREAKDOWN2 %in% c("Commissioning Region") &
          `Year ending` > (ymd("2021-07-01"))) %>% # boundary changes make regions data before this point not comparable
        collapse::ftransform(chosen_region = case_when(
          tolower(nhs_name) == tolower(input$nhs_region_choice) ~ input$nhs_region_choice, # the NHS seems to change the capitalisation of the region names on a regular basis
          nhs_name == "England" ~ "England",
          input$level_choice == "England" ~ "English NHS regions",
          TRUE ~ "Other NHS region"
        )) %>%
        ggplot(aes(
          x = `Year ending`,
          y = `Number of children and young people`,
          group = nhs_name,
          text = nhs_name,
          colour = chosen_region
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
        if (input$level_choice == "Regions") {
          scale_colour_manual(
            values = c("Other NHS region" = af_purple), na.value = af_grey
          )
        } else {
          scale_colour_manual(values = c("English NHS regions" = af_purple), na.value = af_grey)
        }

      mentalhealth_reg_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      mentalhealth_reg_eng <- mentalhealth %>%
        fsubset(BREAKDOWN2 == "England") %>%
        ggplot(aes(
          x = `Year ending`,
          y = `Number of children and young people`,
          group = nhs_name,
          text = nhs_name
        )) +
        geom_line() +
        labs(
          x = "Start of month",
          y = "Number of children and young people"
        )
      mentalhealth_reg_eng %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$mentalhealth_reg_bench_table <- renderTable({
    mentalhealth_reg_bt <- mentalhealth %>%
      fsubset(BREAKDOWN2 %in% c("Commissioning Region") &
        `Year ending` > (ymd("2021-07-01"))) %>%
      fsubset(`Year ending` == max(`Year ending`)) %>% # we have to do this in a second subset call because the regions data doesn't have the most recent month in it
      arrange(desc(`Number of children and young people`)) %>%
      ftransform(
        `Year ending` = format(`Year ending`, "%B %Y"),
        `Number of children and young people` = as.character(round(`Number of children and young people`, 0))
      ) %>% # use month name to stop renderTable linewrapping the date
      fselect(`Year ending`,
        `NHS Region` = nhs_name,
        `Number of children and young people`
      )

    return(mentalhealth_reg_bt)
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
        label = `Appeal Rate`
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
          "label",
          "x",
          "colour"
        )
      ) %>%
      # config(displayModeBar = FALSE) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$tribunals_la_time_table <- renderTable({
    tribunals_la_tt <- tribunals %>%
      fsubset(la_name == input$la_choice) %>%
      mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
      arrange(year) %>%
      fselect(
        Year = year,
        `Local Authority` = la_name,
        `Appeal Rate`
      )

    return(tribunals_la_tt)
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
        text = la_name,
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
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)


    tribunals_la_bench %>%
      ggplotly(
        tooltip = c("label", "x")
      ) %>%
      restyle_england_line() %>%
      save_plot_button_only() %>%
      use_horizontal_legend() %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$tribunals_la_bench_table <- renderTable({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(tribunals), column = "year")

    tribunals_la_bt <- tribunals %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        year == comparison_year) %>%
      mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      arrange(`SEND Tribunal Appeal Rate`) %>%
      fselect(
        Year = year,
        `Local Authority` = la_name,
        `Appeal Rate`
      )

    return(tribunals_la_bt)
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
        mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
        ggplot(aes(
          x = year,
          y = `SEND Tribunal Appeal Rate`,
          group = region_name,
          label = `Appeal Rate`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Calendar year") +
        scale_y_continuous(limits = c(0, 10))

      tribunals_reg_time %>%
        ggplotly(tooltip = c("label", "x")) %>%
        config(displayModeBar = FALSE) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      tribunals_reg_time <- tribunals %>%
        fsubset(geographic_level == "National") %>%
        mutate(`Appeal Rate` = paste0(`SEND Tribunal Appeal Rate`, "%")) %>%
        ggplot(aes(
          x = year,
          y = `SEND Tribunal Appeal Rate`,
          group = geographic_level,
          label = `Appeal Rate`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Calendar year") +
        scale_y_continuous(limits = c(0, 10))

      tribunals_reg_time %>%
        ggplotly(tooltip = c("label", "x")) %>%
        config(displayModeBar = FALSE) %>%
        layout(yaxis = list(autorange = FALSE))
    }
  })

  output$tribunals_reg_time_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      tribunals_reg_tt <- tribunals_reg %>%
        filter(region_name == input$region_choice) %>%
        fmutate(`Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%")) %>%
        arrange(year) %>%
        fselect(
          Year = year,
          Region = region_name,
          `Appeal Rate`
        )
    } else {
      tribunals_reg_tt <- tribunals_reg %>%
        fsubset(region_name == "England") %>%
        arrange(year) %>%
        fmutate(`Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%")) %>%
        fselect(
          Year = year,
          Region = region_name,
          `Appeal Rate`
        )
    }
    return(tribunals_reg_tt)
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
          text = region_name,
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
        scale_fill_manual(values = c("Other region" = af_purple), na.value = af_grey)
    } else {
      validate(need(input$region_choice, message = "Please select a region"))
      tribunals_reg_bench <- tribunals_reg %>%
        collapse::fsubset(year == max(year)) %>%
        ungroup() %>%
        mutate(`Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%")) %>%
        ggplot(aes(
          x = fct_reorder(region_name, `SEND Tribunal Appeal Rate`),
          y = `SEND Tribunal Appeal Rate`,
          text = region_name,
          label = `Appeal Rate`
        )) +
        geom_col(fill = af_purple) +
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
        tooltip = c("text", "label")
      ) %>%
      restyle_england_line() %>%
      save_plot_button_only() %>%
      use_horizontal_legend()
  })

  output$tribunals_reg_bench_table <- renderTable({
    tribunals_reg_bt <- tribunals_reg %>%
      collapse::fsubset(year == max(year)) %>%
      ungroup() %>%
      mutate(`Appeal Rate` = paste0(round(`SEND Tribunal Appeal Rate`, 2), "%")) %>%
      arrange(`SEND Tribunal Appeal Rate`) %>%
      fselect(
        Year = year,
        Region = region_name,
        `Appeal Rate`
      )

    return(tribunals_reg_bt)
  })

  ### Absence ####
  ## SEN Absence (LA/time)
  output$absence_la_time <- renderPlotly({
    absence_la_time <- absence %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = academic_year,
        y = `Overall absence %`,
        group = characteristic,
        colour = characteristic
      )) +
      geom_line() +
      geom_point() +
      labs(
        y = "% of sessions missed due to absence\n(Autumn/Spring terms)",
        x = "Academic year",
        colour = "Group"
      ) +
      scale_colour_manual(values = c(af_darkpink, af_darkblue, af_turquoise, af_orange)) +
      scale_y_continuous(limits = c(0, max(absence$`Overall absence %`)))


    absence_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE,
        yaxis = list(autorange = FALSE)
      )
  })

  output$absence_la_time_table <- renderTable({
    absence_la_tt <- absence %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(
        characteristic,
        academic_year
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN Status` = characteristic,
        `Overall absence %`
      )

    return(absence_la_tt)
  })

  ## SEN absence (LA/bench)
  output$absence_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(absence))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_absence %>%
      collapse::fsubset(characteristic == input$absence_la_filter &
        time_period == comparison_year &
        region_name == "England") %>%
      ungroup() %>%
      mutate(outcome = `Overall absence %`)


    absence_la_bench_data <- absence %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$absence_la_filter &
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
      add_ranks(outcome = "Overall absence %", reverse = T)

    # Create rank statements to go on the X axis.
    absence_la_bench_data$rank_statement <- rank_statement_fun(absence_la_bench_data,
      rank_col = rank,
      name_col = la_name,
      time_period = academic_year
    )
    # Create plot
    absence_la_bench <- absence_la_bench_data %>%
      ggplot(aes(fct_reorder(la_name, `Overall absence %`),
        y = `Overall absence %`,
        text = la_name,
        fill = chosen_la
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        x = absence_la_bench_data$rank_statement[absence_la_bench_data$la_name == input$la_choice],
        y = "% of sessions missed due to absence\n(Autumn/Spring terms)",
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)

    # Feed into plotly
    absence_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$absence_la_bench_table <- renderTable({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(absence))

    absence_la_bt <- absence %>%
      collapse::fsubset(geographic_level == "Local authority" &
        !(la_name %in% small_LAs) &
        characteristic == input$absence_la_filter &
        time_period == comparison_year) %>%
      filter(if (input$myregion_switch == TRUE) {
        region_name == region_name[la_name == input$la_choice][1]
      } else {
        region_name != "none"
      }) %>%
      arrange(`Overall absence %`) %>%
      fselect(
        `Academic Year` = academic_year,
        `Local Authority` = la_name,
        `SEN Status` = characteristic,
        `Overall absence %`
      )

    return(absence_la_bt)
  })

  ## SEN absence (region/time)
  output$absence_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      absence_reg_time <- absence_regional %>%
        filter(region_name == input$region_choice) %>%
        ggplot(aes(
          x = academic_year,
          y = `Overall absence %`,
          group = characteristic,
          colour = characteristic
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
        ))
    } else {
      absence_reg_time <- absence_regional %>%
        filter(region_name == "England") %>%
        ggplot(aes(
          x = academic_year,
          y = `Overall absence %`,
          group = characteristic,
          colour = characteristic
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
        ))
    }

    absence_reg_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$absence_reg_time_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      absence_reg_tt <- absence_regional %>%
        filter(region_name == input$region_choice) %>%
        arrange(
          characteristic,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          `Overall absence %`
        )
    } else {
      absence_reg_tt <- absence_regional %>%
        fsubset(region_name == "England") %>%
        arrange(
          characteristic,
          academic_year
        ) %>%
        fselect(
          `Academic Year` = academic_year,
          Region = region_name,
          `SEN Status` = characteristic,
          `Overall absence %`
        )
    }
    return(absence_reg_tt)
  })


  ## SEN absence (region/bench)
  output$absence_reg_bench <- renderPlotly({
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_absence %>%
      collapse::fsubset(characteristic == input$absence_reg_filter &
        time_period == max(time_period) &
        region_name == "England") %>%
      ungroup() %>%
      mutate(outcome = `Overall absence %`)


    absence_reg_bench_basic <- absence_regional %>%
      collapse::fsubset(
        characteristic == input$absence_reg_filter &
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
      ggplot(aes(fct_reorder(region_name, `Overall absence %`),
        y = `Overall absence %`,
        text = region_name,
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
          values = c("Other region" = af_purple), na.value = af_grey
        )
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }

    # Feed into plotly
    absence_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$absence_reg_bench_table <- renderTable({
    absence_reg_bt <- absence_regional %>%
      collapse::fsubset(characteristic == input$absence_reg_filter &
        time_period == max(time_period)) %>%
      ungroup() %>%
      arrange(`Overall absence %`) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        `SEN Status` = characteristic,
        `Overall absence %`
      )

    return(absence_reg_bt)
  })

  ### KS4 Destinations ####
  ## KS4 destinations (LA/bench)
  output$ks4_destinations_la_bench <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_destinations))
    if (nrow(fsubset(ks4_destinations, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      # req(input$la_choice)
      ks4_destinations_la_bench <- ks4_destinations %>%
        collapse::fsubset(geographic_level == "Local authority" &
          !(la_name %in% small_LAs) &
          characteristic == input$ks4_destinations_la_bench_filter &
          time_period == comparison_year) %>%
        collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
          yes = input$la_choice,
          no = "Other LA"
        )) %>%
        fsubset(region_name == region_name[la_name == input$la_choice]) %>%
        ggplot(aes(
          y = la_name,
          x = `% of pupils`,
          text = la_name,
          fill = Destination,
          label = ifelse(test = `% of pupils` > 2.5,
            yes = paste0(round(`% of pupils`, 0), "%"),
            no = NA
          )
        )) +
        geom_col() +
        geom_text(
          size = 2.5,
          position = position_stack(vjust = 0.5)
        ) +
        labs(
          y = # ifelse(input$myregion_switch == TRUE, yes = # this switch was implemented to change the legend if "only compare to LAs in same region" box was checked
            paste0(
              "Local authorities in ", # but this graph only compares to the same region even if it isn't checked, so no need for it
              ks4_destinations$region_name[ks4_destinations$la_name == input$la_choice][1],
              " in ",
              max(ks4_destinations$academic_year)
            ),
          #    no = paste0("All local authorities in England",
          #                " in ",
          #                max(ks4_destinations$academic_year))),
          x = "% of pupils",
          fill = "Local Authority"
        ) +
        theme(axis.title.x = element_text(margin = margin(t = 1))) +
        scale_fill_manual(values = dest_ks4_palette)


      ks4_destinations_la_bench %>%
        ggplotly(
          tooltip = c("text", "x"),
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
    }
  })

  output$ks4_destinations_la_bench_table <- renderTable({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(ks4_destinations))

    if (nrow(fsubset(ks4_destinations, la_name == input$la_choice & time_period == comparison_year)) > 0) {
      # req(input$la_choice)
      ks4_destinations_la_bt <- ks4_destinations %>%
        collapse::fsubset(geographic_level == "Local authority" &
          !(la_name %in% small_LAs) &
          characteristic == input$ks4_destinations_la_bench_filter &
          time_period == comparison_year) %>%
        fsubset(region_name == region_name[la_name == input$la_choice]) %>%
        arrange(Destination) %>%
        fselect(
          `Academic Year` = academic_year,
          `Local Authority` = la_name,
          `SEN Status` = characteristic,
          Destination,
          `% of pupils`
        )

      return(ks4_destinations_la_bt)
    }
  })

  ## KS4 destinations (region/time)
  output$ks4_destinations_reg_time <- renderPlotly({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks4_destinations_reg_time <- ks4_destinations %>%
        filter(
          characteristic == input$ks4_destinations_reg_time_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
        ggplot(aes(
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
          position = position_stack(vjust = 0.5)
        ) +
        labs(
          x = "Academic year",
          fill = "Destination",
          label = "% of pupils"
        ) +
        scale_fill_manual(values = dest_ks4_palette)

      ks4_destinations_reg_time %>%
        ggplotly(
          tooltip = c("text", "y", "x", "fill")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    } else {
      ks4_destinations_reg_time <- ks4_destinations_nat %>%
        filter(characteristic == input$ks4_destinations_reg_time_filter) %>%
        ggplot(aes(
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
          position = position_stack(vjust = 0.5)
        ) +
        labs(
          x = "Academic year",
          fill = "Destination",
          label = "% of pupils"
        ) +
        scale_fill_manual(values = dest_ks4_palette)

      ks4_destinations_reg_time %>%
        ggplotly(
          tooltip = c("text", "y", "x", "fill")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "v"),
          dragmode = FALSE
        )
    }
  })

  output$ks4_destinations_reg_time_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))
      ks4_destinations_reg_tt <- ks4_destinations %>%
        filter(
          characteristic == input$ks4_destinations_reg_time_filter,
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
      ks4_destinations_reg_tt <- ks4_destinations_nat %>%
        filter(characteristic == input$ks4_destinations_reg_time_filter) %>%
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
    return(ks4_destinations_reg_tt)
  })

  ## KS4 destinations (region/type)
  output$ks4_destinations_reg_type <- renderPlotly({
    # req(input$level_choice)
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))


      ks4_destinations_reg_type <- ks4_destinations %>%
        filter(
          Destination == input$ks4_destinations_reg_type_filter,
          geographic_level == "Regional",
          region_name == input$region_choice
        ) %>%
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
        scale_y_continuous(limits = c(0, 100)) +
        scale_colour_manual(values = af_palette)

      ks4_destinations_reg_type %>%
        ggplotly(
          tooltip = c("text", "y", "x", "colour")
        ) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        ) %>%
        layout(yaxis = list(autorange = FALSE))
    } else {
      ks4_destinations_reg_type <- ks4_destinations_nat %>%
        filter(Destination == input$ks4_destinations_reg_type_filter) %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = characteristic,
          colour = characteristic
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

  output$ks4_destinations_reg_type_table <- renderTable({
    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ks4_destinations_reg_typet <- ks4_destinations %>%
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
      ks4_destinations_reg_typet <- ks4_destinations_nat %>%
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

    return(ks4_destinations_reg_typet)
  })

  ## KS4 destinations (region/bench)
  output$ks4_destinations_reg_bench <- renderPlotly({
    ks4_destinations_reg_bench_basic <- ks4_destinations %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks4_destinations_reg_bench_filter &
        time_period == max(time_period))

    if (input$level_choice == "Regions") {
      validate(need(input$region_choice, message = "Please select a region"))

      ks4_destinations_reg_bench_data <- ks4_destinations_reg_bench_basic %>%
        collapse::ftransform(chosen_region = ifelse(region_name == input$region_choice,
          yes = input$region_choice,
          no = "Other region"
        ))
    } else {
      ks4_destinations_reg_bench_data <- ks4_destinations_reg_bench_basic %>%
        collapse::ftransform(
          chosen_region = "English regions",
          region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
        )
    }

    ks4_destinations_reg_bench <- ks4_destinations_reg_bench_data %>%
      ggplot(aes(
        y = region_name,
        x = `% of pupils`,
        text = region_name,
        fill = Destination,
        label = ifelse(test = `% of pupils` > 2.5,
          yes = paste0(round(`% of pupils`, 0), "%"),
          no = NA
        )
      )) +
      geom_col() +
      geom_text(
        size = 2.5,
        position = position_stack(vjust = 0.5)
      ) +
      labs(
        y = "Region",
        x = "% of pupils",
        fill = "Destination"
      ) +
      #   theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = dest_ks4_palette)

    ks4_destinations_reg_bench %>%
      ggplotly(
        tooltip = c("text", "x", "fill")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(
          orientation = "h",
          traceorder = "reversed"
        ),
        dragmode = FALSE
      )
  })

  output$ks4_destinations_reg_bench_table <- renderTable({
    ks4_dest_reg_bt <- ks4_destinations %>%
      collapse::fsubset(geographic_level == "Regional" &
        characteristic == input$ks4_destinations_reg_bench_filter &
        time_period == max(time_period)) %>%
      ungroup() %>%
      arrange(
        Destination,
        desc(`% of pupils`)
      ) %>%
      fselect(
        `Academic Year` = academic_year,
        Region = region_name,
        Destination,
        `% of pupils`
      )

    return(ks4_dest_reg_bt)
  })

  # FINANCIAL SUSTAINABILITY GRAPHS ---------------------------------------------------------------------------


  ### DSG deficit ####

  ## DSG deficit (LA/time)
  output$dsg_deficit_la_time <- renderPlotly({
    comparison_year <- fix_la_changes(input = input$la_choice, as.data.frame(dsg_deficit))
    latest_dsg <- dsg_deficit %>%
      fsubset(time_period == comparison_year)

    min_scale <- min(dsg_deficit$`DSG cumulative balance as a % of the total budget`)
    max_scale <- max(dsg_deficit$`DSG cumulative balance as a % of the total budget`)

    dsg_deficit_la_time <- dsg_deficit %>%
      fsubset(la_name == input$la_choice) %>%
      ggplot(aes(
        x = financial_year,
        y = `DSG cumulative balance as a % of the total budget`,
        group = la_name
      )) +
      geom_line() +
      geom_point() +
      labs(
        x = "Financial year",
        y = "% surplus (if positive) or deficit (if negative)"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale))


    dsg_deficit_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$dsg_deficit_la_time_table <- renderTable({
    dsg_la_tt <- dsg_deficit %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(financial_year) %>%
      fselect(
        `Financial Year` = financial_year,
        `Local Authority` = la_name,
        `DSG cumulative balance as a % of the total budget`
      )

    return(dsg_la_tt)
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
        mutate(outcome = `DSG cumulative balance as a % of the total budget`)

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
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total budget", reverse = TRUE)
      } else {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total budget", reverse = FALSE)
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
        ggplot(aes(fct_reorder(la_name, `DSG cumulative balance as a % of the total budget`),
          y = `DSG cumulative balance as a % of the total budget`,
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
        labs(x = dsg_deficit_la_bench_data$rank_statement[dsg_deficit_la_bench_data$la_name == input$la_choice]) +
        theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)

      dsg_deficit_la_bench %>%
        ggplotly(
          tooltip = c("text", "y")
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
        mutate(outcome = `DSG cumulative balance as a % of the total budget`)

      dsg_deficit_la_bench_data <- dsg_deficit %>%
        collapse::fsubset(geographic_level == "Local authority" &
          time_period == comparison_year) %>%
        collapse::ftransform(
          chosen_la = ifelse(la_name == input$la_choice,
            yes = input$la_choice,
            no = "Other LAs"
          ),
          time_period = paste0(substr(as.character(time_period), 1, 4), "-", substr(as.character(time_period), 5, 6))
        )
      # Add ranks
      if (dsg_deficit_la_bench_data$deficit[dsg_deficit_la_bench_data$chosen_la == input$la_choice] > 0) {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total budget", reverse = TRUE)
      } else {
        dsg_deficit_la_bench_data <- add_ranks(dsg_deficit_la_bench_data, outcome = "DSG cumulative balance as a % of the total budget", reverse = FALSE)
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
        ggplot(aes(fct_reorder(la_name, `DSG cumulative balance as a % of the total budget`),
          y = `DSG cumulative balance as a % of the total budget`,
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
          fill = "Local Authorities"
        ) +
        theme(axis.text.x = element_blank()) +
        scale_fill_manual(values = c("Other LAs" = af_purple), na.value = af_grey)

      dsg_deficit_la_bench %>%
        ggplotly(
          tooltip = c("text", "y")
        ) %>%
        save_plot_button_only() %>%
        restyle_england_line() %>%
        use_horizontal_legend()
    })
  })

  output$dsg_deficit_la_bench_table <- renderTable({
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
      arrange(`DSG cumulative balance as a % of the total budget`) %>%
      fselect(
        `Financial Year` = financial_year,
        `Local Authority` = la_name,
        `DSG cumulative balance as a % of the total budget`
      )

    return(dsg_la_bt)
  })

  ## DSG deficit (region/time)
  output$dsg_deficit_reg_time <- renderPlotly({
    validate(need(input$region_choice, message = "Please select a region"))
    reg_dsg <- dsg_deficit %>%
      fsubset(geographic_level == "Regional")

    min_scale <- min(reg_dsg$`DSG cumulative balance as a % of the total budget`)
    max_scale <- max(reg_dsg$`DSG cumulative balance as a % of the total budget`)

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
        y = `DSG cumulative balance as a % of the total budget`,
        group = la_name
      )) +
      geom_line() +
      geom_point() +
      labs(x = "Financial year") +
      scale_y_continuous(limits = c(min_scale, max_scale))

    dsg_deficit_reg_time <- dsg_deficit_reg_time_p %>%
      ggplotly(tooltip = c("text", "y", "x")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
    dsg_deficit_reg_time
  })

  output$dsg_deficit_reg_time_table <- renderTable({
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
        `DSG cumulative balance as a % of the total budget`
      )

    return(dsg_deficit_reg_tt)
  })

  ## DSG deficit (region/bench)
  output$dsg_deficit_reg_bench <- renderPlotly({
    validate(need(input$region_choice, message = "Please select a region"))
    # Pull in England data to its own dataframe, for creating the England average dotted line.
    national_average <- eng_dsg_deficit %>%
      collapse::fsubset(time_period == max(time_period) &
        geographic_level == "National") %>%
      ungroup() %>% # required before mutate here
      mutate(outcome = `DSG cumulative balance as a % of the total budget`)

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
      ggplot(aes(fct_reorder(region_name, `DSG cumulative balance as a % of the total budget`),
        y = `DSG cumulative balance as a % of the total budget`,
        text = region_name,
        fill = chosen_region
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
        fill = "Region"
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = 1
      ), legend.position = "none") +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other region" = af_purple), na.value = af_grey
        )
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }

    dsg_deficit_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$dsg_deficit_reg_bench_table <- renderTable({
    dsg_reg_bt <- dsg_deficit %>%
      collapse::fsubset(geographic_level == "Regional" &
        time_period == max(time_period)) %>%
      ungroup() %>%
      distinct() %>%
      arrange(desc(`DSG cumulative balance as a % of the total budget`)) %>%
      fselect(
        `Financial Year` = financial_year,
        Region = region_name,
        `DSG cumulative balance as a % of the total budget`
      )

    return(dsg_reg_bt)
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
        text = paste0("Spend per head: £", round(`Spend per head`, 0))
      )) +
      geom_col(position = "stack") +
      labs(
        x = "Financial year",
        y = "Per capita spend on special schools and AP",
        fill = "School type"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale), labels = scales::dollar_format(prefix = "£")) +
      scale_fill_manual(values = c("#4d8264", "#1A99F9"))


    specialist_spend_la_time %>%
      ggplotly(
        tooltip = c("text", "x")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$specialist_spend_la_time_table <- renderTable({
    spec_la_tt <- specialist_spend %>%
      fsubset(la_name == input$la_choice &
        category != "Total") %>%
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

    return(spec_la_tt)
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
      text = paste0("Spend per head: £", round(`Spend per head`, 0))
    )) +
      geom_col(position = "stack") +
      labs(
        x = "Financial year",
        y = "Per capita spend on special schools and AP",
        fill = "School type"
      ) +
      scale_y_continuous(limits = c(min_scale, max_scale), labels = scales::dollar_format(prefix = "£")) +
      scale_fill_manual(values = c("#4d8264", "#1A99F9"))


    specialist_spend_reg_time %>%
      ggplotly(
        tooltip = c("text", "x")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$specialist_spend_reg_time_table <- renderTable({
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
      fselect(
        `Financial Year` = year,
        `Region` = region,
        `Sector` = category,
        `Per capita spend on special schools and AP` = round(`Spend per head`, digits = 1)
      )

    return(spec_reg_tt)
  })


  ## Specialist Spend (LA/bench)

  output$specialist_spend_la_bench <- renderPlotly({
    # req(input$la_choice)
    comparison <- specialist_spend %>%
      collapse::fsubset(la_name == input$la_choice)
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
        "Other LA (State spending)" = "#1A99F9",
        "Other LA (Ind/non-maintained spending)" = "#4d8264"
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

  output$specialist_spend_la_bench_table <- renderTable({
    comparison <- specialist_spend %>%
      collapse::fsubset(la_name == input$la_choice)
    comparison_year <- max(comparison$year, na.rm = T)
    chosen_region <- specialist_spend$region[specialist_spend$la_name == input$la_choice &
      specialist_spend$year == comparison_year &
      specialist_spend$category == "Total"]

    spend_la_bt <- specialist_spend %>%
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

    return(spend_la_bt)
  })

  ## Specialist Spend (Reg/bench)

  output$specialist_spend_reg_bench <- renderPlotly({
    # req(input$la_choice)

    if (input$level_choice == "Regions") {
      specialist_spend_reg_bench <- reg_specialist_spend %>%
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
        ungroup() %>%
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
          "Other Regions (State spending)" = "#1A99F9",
          "Other Regions (Ind/non-maintained spending)" = "#4d8264"
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
          "State" = "#1A99F9",
          "Independent or non-maintained" = "#4d8264"
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

  output$specialist_spend_reg_bench_table <- renderTable({
    comparison <- reg_specialist_spend %>%
      collapse::fsubset(year == max(year))


    spend_reg_bt <- comparison %>%
      collapse::fsubset(!is.na(region)) %>%
      ungroup() %>%
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

    return(spend_reg_bt)
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
        group = `SEN provision`
      )) +
      geom_line() +
      geom_point() +
      labs(x = "Academic year") +
      scale_colour_manual(values = af_palette)

    percent_pupils_ehcp_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$percent_pupils_ehcp_la_time_table <- renderTable({
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

    return(ehcp_ppc_la_tt)
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
        text = la_name,
        fill = chosen_la
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench(national_average, input = input) +
      labs(
        y = "% of pupils with an EHC plan",
        x = percent_pupils_ehcp_la_bench_data$rank_statement[percent_pupils_ehcp_la_bench_data$la_name == input$la_choice],
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)


    percent_pupils_ehcp_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$percent_pupils_ehcp_la_bench_table <- renderTable({
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

    return(ppc_ehcp_la_bt)
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
          colour = `SEN provision`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette)
    } else {
      percent_pupils_ehcp_reg_time <- percent_pupils_ehcp %>%
        filter(geographic_level == "National") %>%
        ggplot(aes(
          x = academic_year,
          y = `% of pupils`,
          group = `SEN provision`,
          colour = `SEN provision`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette)
    }

    percent_pupils_ehcp_reg_time %>%
      ggplotly(tooltip = c("text", "y", "x")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$percent_pupils_ehcp_reg_time_table <- renderTable({
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
    return(pp_ehcp_reg_tt)
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
        fill = chosen_region
      )) +
      geom_col() +
      # Add England average dotted line and label
      add_england_line_bench(national_average) +
      add_england_label_bench_reg(national_average, nudge = 0.2) +
      labs(
        x = "Regions in England",
        y = "% of pupils with an EHC plan",
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
          values = c("Other region" = af_purple), na.value = af_grey
        )
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }


    percent_pupils_ehcp_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      # This code makes the dotted line a bit thinner.
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$percent_pupils_ehcp_reg_bench_table <- renderTable({
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

    return(pp_ehcp_reg_bt)
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
        tooltip = c("text", "y", "x", "fill")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ehcp_ageprofile_la_time_table <- renderTable({
    ehcp_age_la_tt <- ehcp_ageprofile %>%
      fsubset(la_name == input$la_choice & time_period > 2017) %>%
      arrange(
        time_period,
        desc(`Age group`)
      ) %>%
      fmutate(`Number of EHCPs` = as.character(`Number of EHCPs`)) %>% # prevent 2dp nonsense
      fselect(
        Year = time_period,
        `Local Authority` = la_name,
        `Age group`,
        `Number of EHCPs`
      )

    return(ehcp_age_la_tt)
  })

  ## EHCP age profile (LA/bench) - currently not used
  output$ehcp_ageprofile_la_bench <- renderPlotly({
    # req(input$la_choice)

    chosen_region <- la_region_lookup$region[la_region_lookup$la_name == input$la_choice]

    ehcp_ageprofile_la_bench <- ehcp_ageprofile %>%
      collapse::fsubset(geographic_level == "Local authority" &
        time_period == max(time_period)) %>%
      collapse::ftransform(chosen_la = ifelse(la_name == input$la_choice,
        yes = input$la_choice,
        no = "Other LA"
      )) %>%
      fsubset(region_name == chosen_region) %>%
      ggplot(aes(
        x = fct_reorder(la_name, `Number of EHCPs`),
        y = `Number of EHCPs`,
        text = la_name,
        fill = chosen_la
      )) +
      geom_col(position = "stack", colour = "white") +
      labs(
        y = "% of pupils with an EHC plan",
        x = ifelse(input$myregion_switch == TRUE,
          yes = paste0("Local authorities in ", chosen_region, " in ", max(percent_pupils_ehcp$academic_year)),
          no = paste0("All local authorities in England", " in ", max(percent_pupils_ehcp$academic_year))
        ),
        fill = "Local Authority"
      ) +
      theme(axis.text.x = element_blank()) +
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)

    ehcp_ageprofile_la_bench %>%
      ggplotly(
        tooltip = c("text", "y", "fill")
      ) %>%
      config(displayModeBar = FALSE) # %>%
    #  layout(legend = list(orientation = "h", y = -0.2),
    #         dragmode = FALSE )
  })

  # if this graph is ever used again, it will need a table, the code for which would go here

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
          # label = ifelse(test = (`Number of EHCPs` / sum(`Number of EHCPs`, na.rm = TRUE) < 0.02),
          #                yes = str_remove(`Age group`, "Age "),
          #                no = NA))) +
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
      ggplotly(tooltip = c("text", "y", "x")) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$ehcp_ageprofile_reg_time_table <- renderTable({
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
          region_name = "England",
          `Number of EHCPs` = as.character(`Number of EHCPs`)
        ) %>% # prevent 2dp nonsense
        fselect(
          Year = time_period,
          Region = region_name,
          `Age group`,
          `Number of EHCPs`
        )
    }
    return(ehcp_age_reg_tt)
  })


  ## EHCP age profile (region/bench) - also not used currently
  output$ehcp_ageprofile_reg_bench <- renderPlotly({
    ehcp_ageprofile_reg_bench <- ehcp_ageprofile %>%
      collapse::fsubset(geographic_level == "Regional" &
        `SEN provision` == "EHC plan" &
        time_period == max(time_period)) %>%
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
        region_name = ifelse(region_name == "Yorkshire and The Humber", "Yorkshire and\nThe Humber", region_name)
      ) %>%
      ggplot(aes(fct_reorder(region_name, `% of pupils`),
        y = `% of pupils`,
        text = region_name,
        fill = chosen_region
      )) +
      geom_col() +
      labs(
        x = "Regions in England",
        y = "% of pupils with an EHC plan",
        fill = "Region"
      ) +
      theme(axis.text.x = element_text(
        angle = 45,
        vjust = 0.5,
        hjust = 1
      ), legend.position = "none") +
      if (input$level_choice == "Regions") {
        scale_fill_manual(
          values = c("Other region" = af_purple), na.value = af_grey
        )
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }

    ehcp_ageprofile_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  # if this graph is ever used again, it will need a table, the code for which would go here

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
        group = `SEN provision`
      )) +
      geom_line() +
      geom_point() +
      labs(x = "Academic year") +
      scale_colour_manual(values = af_palette)

    mainstream_with_sen_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      )
  })

  output$mainstream_with_sen_la_time_table <- renderTable({
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

    return(msen_la_tt)
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
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey)


    mainstream_with_sen_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$mainstream_with_sen_la_bench_table <- renderTable({
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

    return(msen_la_bt)
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
          colour = `SEN provision`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette)

      mainstream_with_sen_reg_time %>%
        ggplotly(tooltip = c("text", "y", "x")) %>%
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
          colour = `SEN provision`
        )) +
        geom_line() +
        geom_point() +
        labs(x = "Academic year") +
        scale_colour_manual(values = af_palette)

      mainstream_with_sen_reg_time %>%
        ggplotly(tooltip = c("text", "y", "x")) %>%
        save_plot_button_only() %>%
        layout(
          legend = list(orientation = "h", y = -0.2),
          dragmode = FALSE
        )
    }
  })

  output$mainstream_with_sen_reg_time_table <- renderTable({
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
    return(msen_reg_tt)
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
        y = "% of pupils with an EHC plan",
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
        scale_fill_manual(values = c("Other region" = af_purple), na.value = af_grey)
      } else {
        scale_fill_manual(values = af_purple)
      }

    mainstream_with_sen_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$mainstream_with_sen_reg_bench_table <- renderTable({
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

    return(msen_reg_bt)
  })

  ### Specialist Provider types ####

  ## Provider types (LA/time)
  output$provider_types_la_time <- renderPlotly({
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
        colour = phase_type_grouping
      )) +
      geom_point() +
      geom_line() +
      labs(
        y = paste0("% of ", input$provider_types_la_time_filter),
        x = "Academic year",
        colour = "Group"
      ) +
      scale_colour_brewer(type = "qual", palette = 2)

    provider_types_la_time %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only() %>%
      layout(
        legend = list(orientation = "h", y = -0.2),
        dragmode = FALSE
      ) %>%
      layout(yaxis = list(autorange = FALSE))
  })

  output$provider_types_la_time_table <- renderTable({
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

    return(types_la_tt)
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
        text = la_name,
        fill = chosen_la
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
      scale_fill_manual(values = c("Other LA" = af_purple), na.value = af_grey) +
      scale_y_continuous(limits = c(0, 100))

    provider_types_la_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$provider_types_la_bench_table <- renderTable({
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

    return(types_la_bt)
  })

  ## Provider types (region/time)
  output$provider_types_reg_time <- renderPlotly({
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
          colour = phase_type_grouping
        )) +
        geom_point() +
        geom_line() +
        labs(
          y = paste0("% of ", input$provider_types_reg_time_filter),
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_brewer(type = "qual", palette = 2)

      provider_types_reg_time %>%
        ggplotly(
          tooltip = c("text", "y", "x", "colour")
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
          colour = phase_type_grouping
        )) +
        geom_point() +
        geom_line() +
        labs(
          y = paste0("% of ", input$provider_types_reg_time_filter),
          x = "Academic year",
          colour = "Group"
        ) +
        scale_colour_brewer(type = "qual", palette = 2)

      provider_types_reg_time %>%
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

  output$provider_types_reg_time_table <- renderTable({
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
    return(types_reg_tt)
  })

  ## Provider types (region/bench)
  output$provider_types_reg_bench <- renderPlotly({
    national_average <- provider_types_grouped_nat %>%
      ungroup() %>%
      fsubset(academic_year == max(academic_year) &
        `Provision type` == input$provider_types_reg_bench_filter &
        geographic_level == "National") %>%
      mutate(outcome = `% in independent/AP/special`)


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
        fill = chosen_region
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
        scale_fill_manual(values = c("Other region" = af_purple), na.value = af_grey)
      } else {
        scale_fill_manual(values = c("English regions" = af_purple), na.value = af_grey)
      }

    provider_types_reg_bench %>%
      ggplotly(
        tooltip = c("text", "y")
      ) %>%
      save_plot_button_only() %>%
      restyle_england_line() %>%
      use_horizontal_legend()
  })

  output$provider_types_reg_bench_table <- renderTable({
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

    return(types_reg_bt)
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
    x_lut <- tibble(
      metric = c(
        "Absence", "16-18 destinations", "DSG cumulative balance", "KS1 phonics",
        "KS2 attainment", "KS4 attainment", "KS4 destinations", "% of pupils with EHC plans",
        "Pupils in specialist settings", "Pupils in mainstream with SEN", "Specialist spend per head",
        "EHCP timeliness", "Tribunal appeal rate"
      ),
      x = c(-0.3, -0.3, -0.3, -0.1, 0.1, 0.3, -0.1, -0.3, 0, 0.3, 0.3, 0.1, 0.3)
    )



    summary_metrics %>%
      ungroup() %>%
      fsubset(la_name == input$la_choice) %>%
      arrange(Theme, desc(mean_rank)) %>%
      # x offsets achieved through look-up table since with the error bars they basically always overlap
      left_join(x_lut) %>%
      # Order the themes in the same way as the tabs
      mutate(Theme = factor(Theme, ordered = TRUE, levels = c(
        "Outcomes",
        "Experiences",
        "Identification of Need",
        "Financial Sustainability"
      ))) %>%
      ggplot(aes(
        y = mean_rank,
        x = x,
        label = metric,
        colour = Theme,
        group = x,
        tooltip = detail
      )) +
      #  High/low/median lines
      geom_hline_interactive(yintercept = 1, alpha = 0.3, linetype = "dotted") +
      geom_hline_interactive(yintercept = 150, alpha = 0.3, linetype = "dotted") +
      geom_hline_interactive(
        yintercept = 75,
        linetype = "dotted", alpha = 0.3
      ) +
      # High/low/median line labels
      geom_text_interactive(
        colour = af_grey,
        size = 3,
        data = text_df,
        mapping = aes(
          x = x + 0.3,
          y = y
        )
      ) +
      # Circles
      geom_point_interactive(
        shape = 16,
        size = 5.5, hover_nearest = TRUE
      ) +
      # geom_linerange_interactive(aes(ymax = max_rank, # feature in development, to be added later
      #                               ymin = min_rank,
      #                               alpha = 0.5)) +
      # Text with rank numbers to put on the circles
      geom_text_interactive(aes(label = mean_rank),
        hjust = 0.5,
        colour = "white",
        size = 2
      ) +
      # Metric labels
      # If these settings are changed for the following geom, there is a risk the text is not rendered on the server.
      geom_text_repel_interactive(
        colour = "black",
        size = 3,
        alpha = 0.7,
        direction = "both",
        force = 1.5,
        force_pull = 0.7,
        min.segment.length = 0.001,
        point.padding = 10,
        seed = if_else(isTRUE(getOption("shiny.testmode")), 23, NA) # fixed seed for test mode so the graph is always the same
      ) +
      theme_bw() +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_reverse(
        limits = c(160, -9),
        breaks = seq(from = 160, to = 1, by = -20),
        expand = c(0, 0)
      ) +
      theme(legend.position = "none") +
      labs(
        y = "LA's rank within all 152 LAs",
        title = "LA Metric Summary"
      ) +
      scale_x_continuous(limits = c(-1.5, 1.5)) +
      scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) +
      facet_wrap(~Theme, ncol = 4, scales = "fixed") -> summary_plot

    girafe(ggobj = summary_plot, width_svg = 11, height_svg = 7, canvas_id = "la_summary")
  })

  output$summary_polar <- renderGirafe({
    text_df <- data.frame(
      "metric" = c(
        "KS2 attainment",
        "KS4 destinations",
        "Pupils in mainstream with SEN",
        "DSG cumulative balance"
      ),
      "y" = c(-20, -20, -20, -20),
      "Theme" = c("Outcomes", "Experiences", "Identification of Need", "Financial Sustainability"),
      "detail" = c("Outcomes", "Experiences", "Identification of Need", "Financial Sustainability")
    )

    validate(need(input$la_choice, message = "Please choose an LA to display the graph"))
    dot_x <- 1

    area_choice <- if (is.character(input$ccg_choice)) {
      list(input$la_choice, input$ccg_choice)
    } else {
      input$la_choice
    }

    summary_metrics %>%
      ungroup() %>%
      drop_na(mean_rank, Theme, metric, detail) %>%
      fsubset(la_name == la_choice) %>%
      arrange(Theme, desc(mean_rank)) %>%
      # If two metrics are very close, offset one to the side
      mutate(difference_from_previous = abs(lag(mean_rank) - mean_rank)) %>%
      mutate(x = case_when(
        difference_from_previous < 3 ~ dot_x + 0.2,
        TRUE ~ dot_x
      )) %>%
      mutate(difference_from_next = abs(lead(mean_rank) - mean_rank)) %>%
      mutate(overlap = ifelse(difference_from_next < 3 | difference_from_previous < 3,
        yes = "Overlap",
        no = "No Overlap"
      )) %>%
      mutate(overlap = replace_na(overlap, replace = "No Overlap")) %>%
      # Order the themes in the same way as the tabs
      mutate(Theme = factor(Theme, ordered = TRUE, levels = c(
        "Outcomes",
        "Experiences",
        "Identification of Need",
        "Financial Sustainability"
      ))) %>%
      ggplot(aes(
        y = mean_rank,
        ymin = min_rank,
        ymax = max_rank,
        x = fct_reorder(metric, as.numeric(Theme)),
        label = metric,
        colour = Theme,
        fill = Theme,
        group = metric,
        tooltip = detail
      )) +
      #  High/low/median lines

      geom_hline(aes(yintercept = 152), alpha = 0.3, linetype = "dotted") +
      geom_texthline(aes(label = "Median LA", yintercept = 76), alpha = 0.3, linetype = "dotted", hjust = 0.8) +
      geom_texthline(aes(label = "Highest LA", yintercept = 1), alpha = 0.3, linetype = "dotted", hjust = 0.8) +
      geom_rect(aes(xmin = "16-18 destinations", xmax = "Absence", ymin = -30, ymax = -10), fill = af_darkblue, colour = "white") +
      geom_rect(aes(xmin = "Absence", xmax = "% of pupils with SEN", ymin = -30, ymax = -10), fill = af_turquoise, colour = "white") +
      geom_rect(aes(xmin = "% of pupils with SEN", xmax = "Pupils in specialist settings", ymin = -30, ymax = -10), colour = "white", fill = af_darkpink) +
      geom_rect(aes(xmin = "Pupils in specialist settings", xmax = "Specialist spend per head", ymin = -30, ymax = -10), colour = "white", fill = af_orange) +
      geom_textpath(data = text_df, aes(x = metric, y = y, label = Theme), colour = "white", halign = "right") +
      geom_segment(
        aes(
          x = fct_reorder(metric, as.numeric(Theme)),
          xend = fct_reorder(metric, as.numeric(Theme)),
          y = 152, yend = rank
        )
      ) +
      # Circles
      geom_point_interactive(
        shape = 16,
        size = 5.5, hover_nearest = TRUE
      ) +
      geom_errorbar_interactive(linewidth = 2, alpha = 0.25) +
      # Text with rank numbers to put on the circles
      geom_text_interactive(aes(label = rank),
        hjust = 0.5,
        colour = "white",
        size = 2
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
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()
      ) +
      scale_y_continuous(trans = "reverse", limits = c(160, -40), expand = expansion(add = 20)) +
      theme(legend.position = "none") +
      labs(
        y = "LA's rank within largest 150 LAs",
        title = "LA Metric Summary"
      ) +
      coord_curvedpolar(start = 11) +
      # scale_x_continuous(limits = c(0, 2)) +
      scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) +
      scale_fill_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) -> summary_polar
    girafe(ggobj = summary_polar, width_svg = 11, height_svg = 7)
  })


  # Plotly version of the summary graphic (labels don't self-repel)
  # No longer used but may be useful at some point if ggiraph proves unreliable
  output$summary_plotlyver <- renderPlotly({
    Theme <- c("Outcomes", "Experiences", "Financial Sustainability", "Identification of Need")
    rank_1 <- c(65, 102, 30, 142)
    rank_2 <- c(4, 84, 45, 92)
    rank_3 <- c(54, 98, NA, 130)
    rank_4 <- c(64, 62, NA, 87)
    rank_5 <- c(122, 68, NA, 110)
    rank_6 <- c(99, NA, NA, NA)

    df <- data.frame(Theme, rank_1, rank_2, rank_3, rank_4, rank_5, rank_6)

    df_long1 <- pivot_longer(df,
      cols = -Theme,
      names_to = "number",
      values_to = "rank"
    ) %>%
      mutate(Metric = paste0(Theme, substr(number, start = 6, stop = 6))) %>%
      drop_na(number)

    dot_x <- 0.2

    df_long2 <- df_long1 %>%
      group_by(Theme) %>%
      arrange(rank) %>%
      mutate(Theme = factor(Theme, ordered = TRUE, levels = c(
        "Outcomes",
        "Experiences",
        "Financial Sustainability",
        "Identification of Need"
      ))) %>%
      mutate(difference_from_previous = abs(lag(rank) - rank)) %>%
      mutate(x = case_when(
        difference_from_previous < 2 ~ dot_x + 0.2,
        TRUE ~ dot_x
      )) %>%
      mutate(difference_from_next = abs(lead(rank) - rank)) %>%
      mutate(overlap = ifelse(difference_from_next < 2 | difference_from_previous < 2,
        yes = "Overlap",
        no = "No Overlap"
      )) %>%
      mutate(overlap = replace_na(overlap, replace = "No Overlap"))

    df_long2 %>%
      ggplot(aes(
        y = rank,
        x = x,
        label = Metric,
        colour = Theme,
        group = x
      )) +
      # geom_line()+
      geom_point(
        shape = 16,
        size = 4
      ) +
      geom_hline(yintercept = 1, alpha = 0.5) +
      geom_hline(yintercept = 152, alpha = 0.5) +
      geom_hline(
        yintercept = 76,
        linetype = "dotted"
      ) +
      geom_text(aes(label = rank),
        hjust = 0.5,
        colour = "black",
        size = 3
      ) +
      theme_bw() +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_y_reverse(
        limits = c(160, -9),
        breaks = seq(from = 160, to = 1, by = -20),
        expand = c(0, 0)
      ) +
      theme(legend.position = "none") +
      geom_text(aes(y = 155, x = 0.5, label = "Lowest LA"), colour = "grey30") +
      geom_text(aes(y = 74, x = 0.5, label = "Median LA"), colour = "grey30") +
      geom_text(aes(y = -1, x = 0.5, label = "Highest LA"), colour = "grey30") +
      labs(
        y = "LA's rank within all 152 LAs",
        title = "LA Metric Summary"
      ) +
      geom_text(
        size = 3,
        hjust = 0,
        inherit.aes = FALSE,
        aes(
          label = ifelse(test = overlap == "Overlap",
            yes = "",
            no = Metric
          ),
          x = 1,
          y = rank
        )
      ) +
      scale_x_continuous(limits = c(0.2, 1.8)) +
      scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) +
      facet_wrap(~Theme, ncol = 4, scales = "fixed") +
      labs(subtitle = "The LA's rank for each metric is shown in the number in each circle. \n
           Higher or lower may or may not be better.") -> summary_plot

    summary_plot %>%
      ggplotly(
        tooltip = c("text", "y", "x", "colour")
      ) %>%
      save_plot_button_only()
  })

  # Summary table

  output$la_summary_table <- renderTable({
    summary_table <- summary_metrics %>%
      ungroup() %>%
      drop_na(mean_rank, Theme, metric, detail) %>%
      fsubset(la_name == input$la_choice) %>%
      fmutate(
        Theme = factor(Theme, levels = c("Outcomes", "Experiences", "Identification of Need", "Financial Sustainability")),
        Metric = if_else(detail == "Average progress 8 score (Any SEN)", "Average progress 8 score (All SEN)", detail)
      ) %>%
      arrange(Theme) %>%
      select(
        `Local Authority` = la_name,
        Metric,
        Theme,
        `LA Rank (most recent year)` = mean_rank
      ) # ignore the column name, this is what it actually is
    return(summary_table)
  })

  # England Summary ---------------------------------------------------------------------------

  # KS1 phonics
  output$box_ks1_phonics <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
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
      change = change,
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
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    validate(need(input$summary_sen_type, message = "Select SEN level"), errorClass = "summary-validation")
    df <- eng_ks2_attainment %>%
      drop_na(`Percent meeting expected standards`) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    latest_df <- df %>%
      filter(
        characteristic == input$summary_sen_type,
        academic_year == max(academic_year)
      )

    latest_timeperiod <- pull(latest_df, academic_year)
    latest_value <- pull(latest_df, `Percent meeting expected standards`)

    previous_df <- df %>%
      filter(
        characteristic == input$summary_sen_type,
        academic_year != max(academic_year)
      ) %>%
      filter(academic_year == max(academic_year))

    previous_timeperiod <- pull(previous_df, academic_year)
    previous_value <- pull(previous_df, `Percent meeting expected standards`)
    change <- pull(latest_df, pc_change)

    create_box(
      df = eng_ks2_attainment,
      latest_value = latest_value,
      latest_timeperiod = latest_timeperiod,
      previous_timeperiod = previous_timeperiod,
      change = change,
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
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")

    nhs_region_from_region <- case_when(input$region_choice %in% c("Yorkshire and The Humber", "North East") ~ "North East And Yorkshire",
      input$region_choice %in% c("East Midlands", "West Midlands") ~ "Midlands",
      input$region_choice == "East of England" ~ "East Of England", # sigh
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
      previous_timeperiod = box_data$previous_timeperiod,
      change = box_data$change,
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
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_ks4_attainment <- eng_ks4_attainment %>%
      drop_na(`Average progress 8 score`) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    latest_timeperiod <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "Any SEN",
        academic_year == max(academic_year)
      ) %>%
      pull(academic_year)

    latest_value <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "Any SEN",
        academic_year == max(academic_year)
      ) %>%
      pull(`Average progress 8 score`)

    previous_timeperiod <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "Any SEN",
        academic_year != max(academic_year)
      )
    if (nrow(previous_timeperiod > 0)) {
      previous_timeperiod <- previous_timeperiod %>%
        filter(academic_year == max(academic_year)) %>%
        pull(academic_year)
    }

    previous_value <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "Any SEN",
        academic_year != max(academic_year)
      )

    previous_value <- previous_value %>%
      filter(academic_year == max(academic_year)) %>%
      pull(`Average progress 8 score`)

    change <- eng_ks4_attainment %>%
      filter(
        `SEN provision` == "Any SEN",
        academic_year == max(academic_year)
      ) %>%
      pull(pc_change)

    create_box(
      df = eng_ks4_attainment,
      latest_value = latest_value,
      latest_timeperiod = latest_timeperiod,
      previous_timeperiod = previous_timeperiod,
      change = change,
      add_percent_symbol = FALSE,
      colour = "blue",
      dp = 2
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
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
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
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")

    # because the 16-18 destinations come in two separate files, one England and one Regional, this has to be done a bit differently
    if (input$level_choice == "England") {
      eng_1618 <- destinations_1618_nat %>%
        fsubset(characteristic == "Identified SEN")
    } else {
      eng_1618 <- reg_1618 %>%
        fsubset(characteristic == "Identified SEN") %>%
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
      change = change,
      add_percent_symbol = TRUE,
      colour = "blue"
    )
  })

  output$box_timeliness <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_ehcp_timeliness <- eng_ehcp_timeliness %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    create_box(
      df = eng_ehcp_timeliness,
      latest_value = eng_ehcp_timeliness$`% of EHCPs issued within 20 weeks`[1],
      latest_timeperiod = eng_ehcp_timeliness$time_period[1],
      previous_timeperiod = eng_ehcp_timeliness$time_period[2],
      change = eng_ehcp_timeliness$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "green"
    )
  })

  output$box_tribunals <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")

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
      change = eng_tribunals$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "green"
    )
  })

  output$box_absence <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")

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
      fsubset(characteristic == sen_from_menu_sen)

    # then since the current year will be at the top...
    create_box(
      df = eng_absence,
      latest_value = eng_absence$`Overall absence %`[1],
      latest_timeperiod = eng_absence$academic_year[1],
      previous_timeperiod = eng_absence$academic_year[2],
      change = eng_absence$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "green"
    )
  })

  output$box_autism <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_autism <- eng_autism %>%
      arrange(desc(date))

    # then since the current year will be at the top...
    create_box(
      df = eng_autism,
      latest_value = eng_autism$`% with first appointment after more than 13 weeks`[3],
      latest_timeperiod = eng_autism$REPORTING_PERIOD_END[3],
      previous_timeperiod = eng_autism$REPORTING_PERIOD_END[6], # three rows per year due to age groups
      change = eng_autism$pc_change[3],
      add_percent_symbol = TRUE,
      colour = "green"
    )
  })

  # destinations again, has the same problem as before with national data not in the same file as regional, sigh
  output$box_KS4dest <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
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
      change = change,
      add_percent_symbol = TRUE,
      colour = "green"
    )
  })

  output$box_statefunded <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_percent_pupils_ehcp <- eng_percent_pupils_ehcp %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_percent_pupils_ehcp,
      latest_value = eng_percent_pupils_ehcp$`% of pupils`[1],
      latest_timeperiod = eng_percent_pupils_ehcp$academic_year[1],
      previous_timeperiod = eng_percent_pupils_ehcp$academic_year[3], # two rows per year due to EHCP/SEN Sup
      change = eng_percent_pupils_ehcp$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })

  output$box_mainstream <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_mainstream_with_sen <- eng_mainstream_with_sen %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_mainstream_with_sen,
      latest_value = eng_mainstream_with_sen$`% of pupils`[1],
      latest_timeperiod = eng_mainstream_with_sen$academic_year[1],
      previous_timeperiod = eng_mainstream_with_sen$academic_year[3], # two rows per year due to EHCP/SEN Sup
      change = eng_mainstream_with_sen$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })

  output$box_special <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_provider_types <- eng_provider_types %>%
      arrange(desc(academic_year)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_provider_types,
      latest_value = eng_provider_types$`% of pupils (with SEN provision type)`[1],
      latest_timeperiod = eng_provider_types$academic_year[1],
      previous_timeperiod = eng_provider_types$academic_year[2],
      change = eng_provider_types$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "orange"
    )
  })

  output$box_budget <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    eng_dsg_deficit <- eng_dsg_deficit %>%
      arrange(desc(time_period)) %>%
      geo_subset(lev = input$level_choice, reg = input$region_choice)

    # then since the current year will be at the top...
    create_box(
      df = eng_dsg_deficit,
      latest_value = eng_dsg_deficit$`DSG cumulative balance as a % of the total budget`[1],
      latest_timeperiod = eng_dsg_deficit$financial_year[1],
      previous_timeperiod = eng_dsg_deficit$financial_year[2],
      change = eng_dsg_deficit$pc_change[1],
      add_percent_symbol = TRUE,
      colour = "black"
    )
  })

  output$box_percap <- renderUI({
    validate(need(input$level_choice, message = "Pick England or Regional-level summary"), errorClass = "summary-validation")
    if (input$level_choice == "England") {
      summary_percap <- ungroup(nat_specialist_spend) %>%
        arrange(desc(year)) %>%
        fsubset(category == "Total") %>%
        ftransform(pc_change = (100 * (`Spend per head` - lead(`Spend per head`)) / lead(`Spend per head`)))
    } else {
      summary_percap <- ungroup(reg_specialist_spend) %>%
        arrange(desc(year)) %>%
        fsubset(region == input$region_choice &
          category == "Total") %>%
        ftransform(pc_change = (100 * (`Spend per head` - lead(`Spend per head`)) / lead(`Spend per head`)))
    }
    # then since the current year will be at the top...
    create_box(
      df = summary_percap,
      latest_value = summary_percap$`Spend per head`[1],
      latest_timeperiod = summary_percap$year[1],
      previous_timeperiod = summary_percap$year[2],
      change = summary_percap$pc_change[1],
      add_percent_symbol = FALSE,
      money = TRUE,
      colour = "black"
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


  # CHART/TABLE TOGGLES --------------------------------------------------------------------

  output$ks1_phonics_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$phonics_lat_toggle == "Chart") {
      plotlyOutput("ks1_phonics_la_time")
    } else {
      tableOutput("ks1_phonics_la_time_table")
    }
  })

  output$ks1_phonics_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$phonics_lab_toggle == "Chart") {
      plotlyOutput("ks1_phonics_la_bench")
    } else {
      tableOutput("ks1_phonics_la_bench_table")
    }
  })

  output$ks2_attainment_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks2_lat_toggle == "Chart") {
      plotlyOutput("ks2_attainment_la_time")
    } else {
      tableOutput("ks2_attainment_la_time_table")
    }
  })

  output$ks2_attainment_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks2_lab_toggle == "Chart") {
      plotlyOutput("ks2_attainment_la_bench")
    } else {
      tableOutput("ks2_attainment_la_bench_table")
    }
  })

  output$ks4_attainment_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks4_lat_toggle == "Chart") {
      plotlyOutput("ks4_attainment_la_time")
    } else {
      tableOutput("ks4_attainment_la_time_table")
    }
  })

  output$ks4_attainment_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ks4_lab_toggle == "Chart") {
      plotlyOutput("ks4_attainment_la_bench")
    } else {
      tableOutput("ks4_attainment_la_bench_table")
    }
  })

  output$destinations_1618_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dest18_lat_toggle == "Chart") {
      plotlyOutput("destinations_1618_la_time")
    } else {
      tableOutput("destinations_1618_la_time_table")
    }
  })

  output$destinations_1618_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dest18_lab_toggle == "Chart") {
      plotlyOutput("destinations_1618_la_bench")
    } else {
      tableOutput("destinations_1618_la_bench_table")
    }
  })

  output$destinations_1618_la_type_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dest18_typ_toggle == "Chart") {
      plotlyOutput("destinations_1618_la_type")
    } else {
      tableOutput("destinations_1618_la_type_table")
    }
  })

  output$mentalhealth_ccg_time_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$mh_cgt_toggle == "Chart") {
      plotlyOutput("mentalhealth_ccg_time")
    } else {
      tableOutput("mentalhealth_ccg_time_table")
    }
  })

  output$mentalhealth_ccg_bench_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$mh_cgb_toggle == "Chart") {
      plotlyOutput("mentalhealth_ccg_bench")
    } else {
      tableOutput("mentalhealth_ccg_bench_table")
    }
  })

  output$timeliness_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$time_lat_toggle == "Chart") {
      plotlyOutput("timeliness_la_time")
    } else {
      tableOutput("timeliness_la_time_table")
    }
  })

  output$timeliness_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$time_lab_toggle == "Chart") {
      plotlyOutput("timeliness_la_bench")
    } else {
      tableOutput("timeliness_la_bench_table")
    }
  })

  output$tribunals_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$trib_lat_toggle == "Chart") {
      plotlyOutput("tribunals_la_time")
    } else {
      tableOutput("tribunals_la_time_table")
    }
  })

  output$tribunals_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$trib_lab_toggle == "Chart") {
      plotlyOutput("tribunals_la_bench")
    } else {
      tableOutput("tribunals_la_bench_table")
    }
  })

  output$autism_ccg_time_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$aut_cgt_toggle == "Chart") {
      plotlyOutput("autism_ccg_time")
    } else {
      tableOutput("autism_ccg_time_table")
    }
  })

  output$autism_ccg_bench_tog <- renderUI({
    validate(need(input$ccg_choice, message = "Please select NHS area in top menu"))
    if (input$aut_cgb_toggle == "Chart") {
      plotlyOutput("autism_ccg_bench")
    } else {
      tableOutput("autism_ccg_bench_table")
    }
  })

  output$absence_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$abs_lat_toggle == "Chart") {
      plotlyOutput("absence_la_time")
    } else {
      tableOutput("absence_la_time_table")
    }
  })

  output$absence_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$abs_lab_toggle == "Chart") {
      plotlyOutput("absence_la_bench")
    } else {
      tableOutput("absence_la_bench_table")
    }
  })

  output$ks4_destinations_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$destks4_lat_toggle == "Chart") {
      plotlyOutput("ks4_destinations_la_time")
    } else {
      tableOutput("ks4_destinations_la_time_table")
    }
  })

  output$ks4_destinations_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$destks4_lab_toggle == "Chart") {
      plotlyOutput("ks4_destinations_la_bench")
    } else {
      tableOutput("ks4_destinations_la_bench_table")
    }
  })

  output$ks4_destinations_la_type_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$destks4_typ_toggle == "Chart") {
      plotlyOutput("ks4_destinations_la_type")
    } else {
      tableOutput("ks4_destinations_la_type_table")
    }
  })

  output$percent_pupils_ehcp_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ehcppc_lat_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_la_time")
    } else {
      tableOutput("percent_pupils_ehcp_la_time_table")
    }
  })

  output$percent_pupils_ehcp_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$ehcppc_lab_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_la_bench")
    } else {
      tableOutput("percent_pupils_ehcp_la_bench_table")
    }
  })

  output$mainstream_with_sen_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$msen_lat_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_la_time")
    } else {
      tableOutput("mainstream_with_sen_la_time_table")
    }
  })

  output$mainstream_with_sen_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$msen_lab_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_la_bench")
    } else {
      tableOutput("mainstream_with_sen_la_bench_table")
    }
  })

  output$provider_types_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$types_lat_toggle == "Chart") {
      plotlyOutput("provider_types_la_time")
    } else {
      tableOutput("provider_types_la_time_table")
    }
  })

  output$provider_types_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$types_lab_toggle == "Chart") {
      plotlyOutput("provider_types_la_bench")
    } else {
      tableOutput("provider_types_la_bench_table")
    }
  })

  output$ehcp_ageprofile_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$age_lat_toggle == "Chart") {
      plotlyOutput("ehcp_ageprofile_la_time")
    } else {
      tableOutput("ehcp_ageprofile_la_time_table")
    }
  })

  output$dsg_deficit_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dsg_lat_toggle == "Chart") {
      plotlyOutput("dsg_deficit_la_time")
    } else {
      tableOutput("dsg_deficit_la_time_table")
    }
  })

  output$dsg_deficit_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$dsg_lab_toggle == "Chart") {
      plotlyOutput("dsg_deficit_la_bench")
    } else {
      tableOutput("dsg_deficit_la_bench_table")
    }
  })

  output$specialist_spend_la_time_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$spend_lat_toggle == "Chart") {
      plotlyOutput("specialist_spend_la_time")
    } else {
      tableOutput("specialist_spend_la_time_table")
    }
  })

  output$specialist_spend_la_bench_tog <- renderUI({
    validate(need(input$la_choice, message = "Please select LA in top menu"))
    if (input$spend_lab_toggle == "Chart") {
      plotlyOutput("specialist_spend_la_bench")
    } else {
      tableOutput("specialist_spend_la_bench_table")
    }
  })

  output$ks1_phonics_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$phonics_regt_toggle == "Chart") {
      plotlyOutput("ks1_phonics_reg_time")
    } else {
      tableOutput("ks1_phonics_reg_time_table")
    }
  })

  output$ks1_phonics_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$phonics_regb_toggle == "Chart") {
      plotlyOutput("ks1_phonics_reg_bench")
    } else {
      tableOutput("ks1_phonics_reg_bench_table")
    }
  })

  output$ks2_attainment_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$ks2_regt_toggle == "Chart") {
      plotlyOutput("ks2_attainment_reg_time")
    } else {
      tableOutput("ks2_attainment_reg_time_table")
    }
  })

  output$ks2_attainment_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$ks2_regb_toggle == "Chart") {
      plotlyOutput("ks2_attainment_reg_bench")
    } else {
      tableOutput("ks2_attainment_reg_bench_table")
    }
  })

  output$ks4_attainment_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$ks4_regt_toggle == "Chart") {
      plotlyOutput("ks4_attainment_reg_time")
    } else {
      tableOutput("ks4_attainment_reg_time_table")
    }
  })

  output$ks4_attainment_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$ks4_regb_toggle == "Chart") {
      plotlyOutput("ks4_attainment_reg_bench")
    } else {
      tableOutput("ks4_attainment_reg_bench_table")
    }
  })

  output$destinations_1618_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$dest18_regt_toggle == "Chart") {
      plotlyOutput("destinations_1618_reg_time")
    } else {
      tableOutput("destinations_1618_reg_time_table")
    }
  })

  output$destinations_1618_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$dest18_regb_toggle == "Chart") {
      plotlyOutput("destinations_1618_reg_bench")
    } else {
      tableOutput("destinations_1618_reg_bench_table")
    }
  })

  output$destinations_1618_reg_type_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$dest18_regtyp_toggle == "Chart") {
      plotlyOutput("destinations_1618_reg_type")
    } else {
      tableOutput("destinations_1618_reg_type_table")
    }
  })

  output$mentalhealth_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$mh_regt_toggle == "Chart") {
      plotlyOutput("mentalhealth_reg_bench")
    } else {
      tableOutput("mentalhealth_reg_bench_table")
    }
  })

  output$timeliness_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$time_regt_toggle == "Chart") {
      plotlyOutput("timeliness_reg_time")
    } else {
      tableOutput("timeliness_reg_time_table")
    }
  })

  output$timeliness_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$time_regb_toggle == "Chart") {
      plotlyOutput("timeliness_reg_bench")
    } else {
      tableOutput("timeliness_reg_bench_table")
    }
  })

  output$tribunals_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$trib_regt_toggle == "Chart") {
      plotlyOutput("tribunals_reg_time")
    } else {
      tableOutput("tribunals_reg_time_table")
    }
  })

  output$tribunals_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$trib_regb_toggle == "Chart") {
      plotlyOutput("tribunals_reg_bench")
    } else {
      tableOutput("tribunals_reg_bench_table")
    }
  })

  output$autism_nat_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$aut_nat_toggle == "Chart") {
      plotlyOutput("autism_nat_time")
    } else {
      tableOutput("autism_nat_time_table")
    }
  })

  output$autism_nat_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$aut_nab_toggle == "Chart") {
      plotlyOutput("autism_nat_bench")
    } else {
      tableOutput("autism_nat_bench_table")
    }
  })

  output$absence_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$abs_regt_toggle == "Chart") {
      plotlyOutput("absence_reg_time")
    } else {
      tableOutput("absence_reg_time_table")
    }
  })

  output$absence_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$abs_regb_toggle == "Chart") {
      plotlyOutput("absence_reg_bench")
    } else {
      tableOutput("absence_reg_bench_table")
    }
  })

  output$ks4_destinations_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$destks4_regt_toggle == "Chart") {
      plotlyOutput("ks4_destinations_reg_time")
    } else {
      tableOutput("ks4_destinations_reg_time_table")
    }
  })

  output$ks4_destinations_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$destks4_regb_toggle == "Chart") {
      plotlyOutput("ks4_destinations_reg_bench")
    } else {
      tableOutput("ks4_destinations_reg_bench_table")
    }
  })

  output$ks4_destinations_reg_type_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$destks4_regtyp_toggle == "Chart") {
      plotlyOutput("ks4_destinations_reg_type")
    } else {
      tableOutput("ks4_destinations_reg_type_table")
    }
  })

  output$percent_pupils_ehcp_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$ehcppc_regt_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_reg_time")
    } else {
      tableOutput("percent_pupils_ehcp_reg_time_table")
    }
  })

  output$percent_pupils_ehcp_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$ehcppc_regb_toggle == "Chart") {
      plotlyOutput("percent_pupils_ehcp_reg_bench")
    } else {
      tableOutput("percent_pupils_ehcp_reg_bench_table")
    }
  })

  output$mainstream_with_sen_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$msen_regt_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_reg_time")
    } else {
      tableOutput("mainstream_with_sen_reg_time_table")
    }
  })

  output$mainstream_with_sen_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$msen_regb_toggle == "Chart") {
      plotlyOutput("mainstream_with_sen_reg_bench")
    } else {
      tableOutput("mainstream_with_sen_reg_bench_table")
    }
  })

  output$provider_types_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$types_regt_toggle == "Chart") {
      plotlyOutput("provider_types_reg_time")
    } else {
      tableOutput("provider_types_reg_time_table")
    }
  })

  output$provider_types_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$types_regb_toggle == "Chart") {
      plotlyOutput("provider_types_reg_bench")
    } else {
      tableOutput("provider_types_reg_bench_table")
    }
  })

  output$ehcp_ageprofile_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$age_regt_toggle == "Chart") {
      plotlyOutput("ehcp_ageprofile_reg_time")
    } else {
      tableOutput("ehcp_ageprofile_reg_time_table")
    }
  })

  output$dsg_deficit_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$dsg_regt_toggle == "Chart") {
      plotlyOutput("dsg_deficit_reg_time")
    } else {
      tableOutput("dsg_deficit_reg_time_table")
    }
  })

  output$dsg_deficit_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$dsg_regb_toggle == "Chart") {
      plotlyOutput("dsg_deficit_reg_bench")
    } else {
      tableOutput("dsg_deficit_reg_bench_table")
    }
  })

  output$specialist_spend_reg_time_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$spend_regt_toggle == "Chart") {
      plotlyOutput("specialist_spend_reg_time")
    } else {
      tableOutput("specialist_spend_reg_time_table")
    }
  })

  output$specialist_spend_reg_bench_tog <- renderUI({
    validate(need(input$level_choice, message = "Please select England or Regional level"))
    if (input$spend_regb_toggle == "Chart") {
      plotlyOutput("specialist_spend_reg_bench")
    } else {
      tableOutput("specialist_spend_reg_bench_table")
    }
  })

  output$la_summary_tog <- renderUI({
    if (input$la_sum_toggle == "Chart") {
      girafeOutput("summary", height = "800px")
    } else {
      tableOutput("la_summary_table")
    }
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
    "provider_types_la_bench"
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
