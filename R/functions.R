# Functions
# This script contains the functions used in the dashboard
# it is called on by global.R



# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts (comes as standard with dashboard template)

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("Helper scripts")
  message("----------------------------------------")
  help_scripts <- eval(styler::style_dir("R/", filetype = "r")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, help_scripts, test_scripts)
  return(script_changes)
}

# Text and number formatting functions ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# Round a number to 1 digit, add commas, add "plus/minus" symbols
format_pm <- function(x) {
  strNum <- round(x, digits = 1)
  strNum <- format(abs(strNum), big.mark = ",", trim = TRUE)
  strNum <- paste0(ifelse(x < 0, "-", ""), strNum)
}

# format academic years from time periods
time_period_to_academic_year <- function(df) {
  df %>%
    ftransform(academic_year = paste0(substr(time_period, start = 1, stop = 4), "/", substr(time_period, start = 5, stop = 6)))
}

# capitalise the first letter of words for titles etc
# "acro" contains acronyms which should be all caps
# "minor" contains conjunctions etc which should be lower case
simpleCap <- function(x, except = c("nhs", "ft", "nhst", "ccg", "hcrg"), minor = c("and", "of")) {
  s <- strsplit(tolower(x), " ")[[1]]
  out <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " "
  )
  for (word in except) {
    out <- gsub(word, toupper(word), out, ignore.case = TRUE)
  }
  for (word in minor) {
    out <- gsub(word, tolower(word), out, ignore.case = TRUE)
  }
  return(out)
}

# England Summary helper functions ----------------------------------------------------------------------------

# Function for creating sparklines
create_sparkline <- function(df, outcome_var, time_var, yformat) {
  # Capture the time and outcome variables from df
  outcome_var <- enquo(outcome_var)
  time_var <- enquo(time_var)

  # Create character variables of the name of the time variable and outcome variable
  time_name <- deparse(substitute(!!time_var)) %>%
    gsub("_", " ", .) %>%
    gsub("(^[[:alpha:]])", "\\U\\1", ., perl = TRUE) %>%
    str_remove(pattern = "~")

  outcome_name <- deparse(substitute(outcome_var)) %>%
    gsub("_", " ", .) %>%
    gsub("(^[[:alpha:]])", "\\U\\1", ., perl = TRUE) %>%
    str_remove(pattern = "~")

  # Find first and last x-axis labels
  min_break <- df %>%
    pull(!!time_var) %>%
    min()

  max_break <- df %>%
    pull(!!time_var) %>%
    max()

  # Draw a line plot and create a label for the tooltips
  sparkline <- df %>%
    ggplot(aes(
      x = !!time_var,
      y = !!outcome_var,
      group = 1,
      text = paste0(
        time_name, ": ",
        !!time_var, "<br>",
        outcome_name, ": ",
        !!outcome_var, "<br>"
      )
    )) +
    geom_area(outline.type = "full", fill = "white") +
    geom_path(lineend = "round") +
    theme_classic() +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      panel.background = element_rect(fill = "#f3f2f1"),
      plot.background = element_rect(fill = "#f3f2f1"),
      plot.margin = grid::unit(c(0, 1, 0, 0), "cm"),
      axis.ticks.length = unit(0, "pt")
    ) +
    scale_x_discrete(breaks = c(min_break, max_break))

  # If the format is specified as percent, add a % symbol
  if (yformat == "percent") {
    sparkline <- sparkline +
      scale_y_continuous(labels = scales::percent_format(scale = 1), expand = expansion(mult = c(0.05, 0.5)))
  } else {
    sparkline <- sparkline + scale_y_continuous(expand = expansion(mult = c(0.05, 0.5)))
  }
} # end of function

# Function to format the sparkline
format_sparkline <- function(p) {
  # Set margins (taken from LSIP dashboard)
  # m <- list(
  #  l = 0, r = 4, b = 0, t = 0, pad = 0 )

  p %>%
    layout(
      p = p,
      #        margin = m,
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    ) %>% # disable zooming
    config(
      p = p,
      displayModeBar = FALSE
    ) %>%
    style(hoverinfo = "none", traces = c(0, 1, 2))
}

# Generate an up, down or right arrow to symbolise change
arrow_function <- function(current, previous) {
  value <- (current - previous) / previous
  if (value > 0.05) {
    return("\U2191")
  } else if (value < -0.05) {
    return("\U2193")
  } else {
    return("\U2192")
  }
}

# make the data for the England/Region summaries, assuming the df is shaped the way they normally are
box_data <- function(df, value_column, time_column) {
  value <- enquo(value_column)
  time_period <- enquo(time_column)

  latest <- df %>%
    filter(
      !!time_period == max(!!time_period)
    )

  latest_value <- pull(latest, !!value)
  latest_timeperiod <- pull(latest, !!time_period)
  change <- pull(latest, pc_change)

  previous <- df %>%
    filter(!!time_period != max(!!time_period)) %>%
    filter(!!time_period == max(!!time_period))

  previous_value <- pull(previous, !!value)
  previous_timeperiod <- pull(previous, !!time_period)

  return(list(latest_value = latest_value, latest_timeperiod = latest_timeperiod, previous_value = previous_value, previous_timeperiod = previous_timeperiod, change = change))
}

# Create the info box for the England summary
create_box <- function(df, latest_value, previous_value, latest_timeperiod, previous_timeperiod, add_percent_symbol, money = FALSE, colour, dp = 1) {
  # If user specifies to add a % or not
  if (add_percent_symbol == TRUE) {
    latest <- paste0(round(latest_value, dp), "%")
    previous <- paste0(format_pm(previous_value), "%")
  } else if (money == TRUE) {
    latest <- paste("£", as.character(round(latest_value, 0), sep = " "))
    previous <- paste0("£", as.character(round(previous_value, 0), sep = " "))
  } else {
    latest <- round(latest_value, dp)
    previous <- format_pm(previous_value)
  }

  tags$table(
    style = paste0("background-color:", colour),
    style = "color: white",
    tags$tr(
      tags$td(span(paste0(latest_timeperiod, " value:  "),
        style = "font-size: 16px;",
        .noWS = c("before")
      )), # remove whitespace),
      tags$td(width = 15),
      tags$td(span("Previously:",
        style = "font-size: 16px"
      ))
    ),
    tags$tr(
      tags$td(span(
        paste0(
          format(latest, big.mark = ",", trim = TRUE), # Latest value for metric in big font
          " (", arrow_function(latest_value, previous_value), ")"
        ),
        style = "font-size: 32px"
      )),
      tags$td(span(" ")),
      tags$td(span(
        paste0(
          previous # Change direction arrow
        ), # Change figure
        style = "font-size: 21px",
        .noWS = c("before")
      ))
    ), # remove whitespace
    tags$tr(
      tags$td(),
      tags$td(),
      tags$td(span(
        paste0(
          "in ",
          previous_timeperiod
        ),
        style = "font-size: 14px",
        .noWS = "before"
      ))
    )
  )
}

# Create the info box for the England summary in the case we don't have any "previous" data to compare to
missing_box <- function(df, latest_value, latest_timeperiod, colour, text1 = "No previous data", text2 = "available for comparison") {
  tags$table(
    style = paste0("background-color:", colour),
    style = "color: white",
    tags$tr(
      tags$td(width = 100, span(paste0(latest_timeperiod, " value:  "),
        style = "font-size: 16px;",
        .noWS = c("before")
      )), # remove whitespace),
      tags$td(width = 15),
      tags$td(span(text1,
        style = "font-size: 16px"
      ))
    ),
    tags$tr(
      tags$td(span(format(latest_value, big.mark = ",", trim = TRUE), # Latest value for metric in big font
        style = "font-size: 32px"
      )),
      tags$td(width = 15),
      tags$td(span(text2,
        style = "font-size: 16px",
        .noWS = c("before")
      ))
    ), # remove whitespace
    tags$tr(
      # tags$td(width = 15, height = "1em"),
      tags$td(span(" "))
    )
  )
}

# Function for subsetting summary boxes to the England/regions dropdown
geo_subset <- function(df, lev, reg) {
  if (lev == "England") {
    df <- df %>%
      fsubset(geographic_level == "National")
  } else {
    df <- df %>%
      fsubset(geographic_level == "Regional" &
        region_name == reg)
  }
  return(df)
}

# General dashboard plotting functions ----------------------------------------------------------------------------

# Set plotly to only display a "save plot as image" button, no other buttons
save_plot_button_only <- function(p) {
  plotly::config(
    p = p,
    displaylogo = FALSE,
    toImageButtonOptions = list(format = "png"),
    modeBarButtons = list(list("toImage"))
  )
}

# Change legend to horizontal in plotly
use_horizontal_legend <- function(p) {
  layout(
    p = p,
    legend = list(orientation = "h", y = -0.2),
    dragmode = FALSE
  )
}

# Set all the standard options for a DT table
DTise <- function(df, order, hidden = NULL) {
  output <- DT::datatable(df,
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      order = order,
      columnDefs = list(list(visible = FALSE, targets = hidden)),
      pageLength = 20,
      dom = "lftBp",
      buttons = list("copy", list(
        extend = "collection",
        buttons = c("csv", "excel", "pdf"),
        text = "Download"
      ))
    )
  )
  return(output)
}

# England average lines and labels for benchmarking graphs ----------------------------------------------------------------------------

# Add an England line to the benchmarking graphs
add_england_line_bench <- function(df) {
  geom_hline(
    linetype = "dashed",
    colour = af_grey,
    size = 0.5,
    data = df,
    aes(yintercept = .data[["outcome"]])
  )
}

# Label the England line on benchmarking graphs
add_england_label_bench <- function(df, input, nudge = 0.25) {
  geom_text(
    data = df,
    colour = af_grey,
    size = 3.5,
    inherit.aes = FALSE,
    nudge_y = nudge,
    aes(
      x = ifelse(test = input[["myregion_switch"]] == TRUE,
        yes = 3,
        no = 20
      ),
      label = "England average",
      y = abs(.data[["outcome"]] * 1.05), # Set the label 5% higher than the line; better than nudge_y since scales differ
      text = paste("England average: ", .data[["outcome"]]) # Customize the text for the hover label
    )
  )
}


# Regional version of label function with label set further to left
add_england_label_bench_reg <- function(df, nudge = 0.5) {
  geom_text(
    data = df,
    colour = af_grey,
    size = 3.5,
    nudge_y = nudge,
    inherit.aes = FALSE,
    aes(
      x = 2,
      y = .data[["outcome"]],
      label = "England average",
      text = paste("England average: ", .data[["outcome"]])
    )
  )
}

# Thin out the England average line (for benchmarking graphs)
restyle_england_line <- function(p) {
  # Change England average label alignment
  style(
    p = p,
    textposition = "right",
    traces = c(4)
  ) %>%
    # This code makes the dotted line a bit thinner.
    style(
      p = p,
      line = list(
        width = 1,
        color = "rgba(0,0,0,1)",
        dash = "dash",
        showlegend = "false"
      ),
      traces = c(3)
    )
}

# Create function to add rank numbers to an LA benchmarking dataframe, specifying the outcome to be ranked by
add_ranks <- function(df, outcome, reverse = FALSE) {
  df <- df %>%
    drop_na(!!outcome)
  df <- if (!!reverse == FALSE) {
    arrange(df, desc(df[[outcome]]))
  } else {
    arrange(df, df[[outcome]])
  }
  df <- df %>%
    mutate(rank = row_number())
}

# Create function to generate a generic statement about an LA's ranking within a region or the country
rank_statement_fun <- function(df, rank_col, name_col, time_period, geog = "LAs") {
  rank_col <- enquo(rank_col)
  name_col <- enquo(name_col)
  time_period <- enquo(time_period)
  n <- nrow(df)
  df %>%
    mutate(
      statement = case_when(
        !!rank_col == 1 ~ paste0(!!name_col, " is the highest out of ", n, " ", geog, " in ", max(!!time_period)),
        !!rank_col == n ~ paste0(!!name_col, " is the lowest out of ", n, " ", geog, " in ", max(!!time_period)),
        !!rank_col %% 10 == 1 & !!rank_col != 11 ~ paste0(!!name_col, " is ranked ", !!rank_col, "st, out of ", n, " ", geog, " in ", max(!!time_period)),
        !!rank_col %% 10 == 2 & !!rank_col != 12 ~ paste0(!!name_col, " is ranked ", !!rank_col, "nd, out of ", n, " ", geog, " in ", max(!!time_period)),
        !!rank_col %% 10 == 3 & !!rank_col != 13 ~ paste0(!!name_col, " is ranked ", !!rank_col, "rd, out of ", n, " ", geog, " in ", max(!!time_period)),
        TRUE ~ paste0(!!name_col, " is ranked ", !!rank_col, "th, out of ", n, " ", geog, " in ", max(!!time_period))
      )
    ) %>%
    pull(statement)
}

# function to adjust comparison local authorities which have split/joined. Right now this is just Northamptonshire splitting into West and North, but will need it
# in future for Cumbria -> Westmoreland & Furness/Northumberland and doubtless others
# what it's supposed to do is change the year the benchmark graph filters to
fix_la_changes <- function(df, input, column = "time_period") {
  LA <- {{ input }}
  affected_LAs <- c("Northamptonshire", "Cumbria")
  if (LA %in% affected_LAs) {
    last_year <- max(df[df$la_name == LA, {{ column }}], na.rm = T)

    return(last_year)
  } else {
    years <- as.numeric(unlist((df[{{ column }}])))
    return(max(years, na.rm = T)) # function only needs to do anything clever if the selected LA is actually one that has changed
  }
}


fix_la_changes_ofsted <- function(df, input, column = "Year") {
  LA <- {{ input }}
  affected_LAs <- c("Northamptonshire", "Cumbria")
  if (LA %in% affected_LAs) {
    last_year <- max(df[df$la_name == LA, {{ column }}], na.rm = T)

    return(last_year)
  } else {
    years <- as.numeric(unlist((df[{{ column }}])))
    return(max(years, na.rm = T)) # function only needs to do anything clever if the selected LA is actually one that has changed
  }
}

# create a date column from an AY, for time series graphs with missing data (so that eg 2018/19 isn't next to 2021/22 if Covid years are missing)
AY_to_date <- function(df, AY_col) {
  col <- enquo(AY_col)
  df <- df %>%
    dplyr::mutate(AY_date = as.Date(paste0("1-9-", substr(!!col, 1, 4)), format = "%d-%m-%Y")) # apparently collapse::ftransform doesn't do NSE the same way so this doesn't work with it
  return(df)
}


# Create a LA validation warning message (as some LAs don't have data for certain combination of user selected filters [LA and the specific measure])
validate_if_no_la_data <- function(data) {
  validate(
    need(!is.null(data) && nrow(data) > 0, "There doesn't appear to be data for this LA and measure.")
  )
}

# Takes first four characters from a YYYY date structure and makes it cleanly label as academic years e.g. "2018-10-10" becomes "2018/19"
## Required for some line graphs
ay_date_to_ay <- function(input_str) {
  # Extract the first four numbers from the input
  first_four_numbers <- substr(gsub("[^0-9]", "", input_str), 1, 4)

  # Create the formatted string "YYYY/YY"
  formatted_string <- paste0(first_four_numbers, "/", substr(as.numeric(first_four_numbers) + 1, 3, 4))

  # Add the specific date "2017-09-01"
  result <- paste0(formatted_string)

  return(result)
}

format_axis_label <- function(label, max_length = 40) {
  formatted_label <- strwrap(label, width = max_length, simplify = TRUE)
  return(paste(formatted_label, collapse = "\n"))
}

# useShinydashboard function, which prevents the footer colouration bleeding into the main body
useShinydashboard <- function() {
  if (!requireNamespace(package = "shinydashboard")) {
    message("Package 'shinydashboard' is required to run this function")
  }
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(tags$div(class = "main-sidebar", style = "display: none;"), value = deps)
}
