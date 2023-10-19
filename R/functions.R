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
  strNum <- paste0(ifelse(x < 0, "-", "+"), strNum)
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
arrow_function <- function(value) {
  if (value > 0.09) {
    return("\U2191")
  } else if (value < 0.09) {
    return("\U2193")
  } else {
    return("\U2192")
  }
}

# Create the info box for the England summary
create_box <- function(df, latest_value, change, latest_timeperiod, previous_timeperiod, add_percent_symbol, money = FALSE, colour, dp = 1) {
  # If user specifies to add a % or not
  if (add_percent_symbol == TRUE) {
    latest_value <- paste0(round(latest_value, dp), "%")
    change_formatted <- paste0(format_pm(change), "%")
  } else if (money == TRUE) {
    latest_value <- paste0("£", as.character(round(latest_value, 0), sep = " "))
    change_formatted <- paste(format_pm(change), "%", sep = " ") # the change is always a percentage change
  } else {
    latest_value <- round(latest_value, dp)
    change_formatted <- paste(format_pm(change), "%", sep = " ") # the change is always a percentage change
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
      tags$td(span("Change:",
        style = "font-size: 16px"
      ))
    ),
    tags$tr(
      tags$td(span(format(latest_value, big.mark = ",", trim = TRUE), # Latest value for metric in big font
        style = "font-size: 32px"
      )),
      tags$td(span(" ")),
      tags$td(span(
        paste0(
          arrow_function(change), # Change direction arrow
          " ",
          change_formatted
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
          "since ",
          previous_timeperiod
        ),
        style = "font-size: 14px",
        .noWS = "before"
      ))
    )
  )
}

# Create the info box for the England summary in the case we don't have any "previous" data to compare to
missing_box <- function(df, latest_value, latest_timeperiod, colour) {
  tags$table(
    style = paste0("background-color:", colour),
    style = "color: white",
    tags$tr(
      tags$td(width = 100, span(paste0(latest_timeperiod, " value:  "),
        style = "font-size: 16px;",
        .noWS = c("before")
      )), # remove whitespace),
      tags$td(width = 15),
      tags$td(span("No previous data ",
        style = "font-size: 16px"
      ))
    ),
    tags$tr(
      tags$td(span(format(latest_value, big.mark = ",", trim = TRUE), # Latest value for metric in big font
        style = "font-size: 32px"
      )),
      tags$td(width = 15),
      tags$td(span("available for comparison",
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
add_england_label_bench <- function(df, input, nudge = 0) {
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
      y = abs(.data[["outcome"]] * 1.05), # Set the label 5% higher than the line; better than nudge_y since scales differ
      label = "England average"
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
      x = 2, y = .data[["outcome"]],
      label = "England average"
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
  affected_LAs <- c("Northamptonshire")
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
