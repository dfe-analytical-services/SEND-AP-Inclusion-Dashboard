dot_x <- 1

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

summary_metrics %>%
  ungroup() %>%
  drop_na(mean_rank, Theme, metric, detail) %>%
  fsubset(la_name == "Camden") %>%
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
    x = mean_rank,
    xmin = min_rank,
    xmax = max_rank,
    y = fct_reorder(metric, -as.numeric(Theme)),
    label = metric,
    colour = Theme,
    fill = Theme,
    group = metric,
    tooltip = detail
  )) +
  #  High/low/median lines

  geom_vline(aes(xintercept = 152), alpha = 0.3, linetype = "dotted") +
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
    x = "LA's rank within largest 150 LAs",
    title = "LA Metric Summary"
  ) +
  scale_colour_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange)) +
  scale_fill_manual(values = c(af_darkblue, af_turquoise, af_darkpink, af_orange))
