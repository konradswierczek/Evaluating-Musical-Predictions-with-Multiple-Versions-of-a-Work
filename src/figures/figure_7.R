tool_levels <- levels(factor(df_var$tool))

df_points <- df_var |>
  group_by(tool, feature) |>
  summarize(mean_ratio = mean(ratio), .groups = "drop") |>
  mutate(
    tool = factor(tool, levels = tool_levels),
    tool_numeric = case_when(
      tool == "Essentia" ~ 1250,
      tool == "Librosa" ~ 1000,
      tool == "MIRtoolbox" ~ 750
    )
  )  # numeric y values for plotting

df_eme_var |>
  separate_wider_delim(
    feature,
    delim = ".",
    names = c("category", "feature", "derivative", "who_knows"),
    too_few = "align_start"
  ) |>
  filter(category != "metadata") |>
  mutate(
    category = fct_relevel(
      category,
      "lowlevel",
      "tonal",
      "rhythm"
    )
  ) |>
  ggplot(
    aes(
      x = mean_ratio
    )
  ) +
  geom_density(
    aes(
      mean_ratio,
      after_stat(count),
      fill = category
    ),
    alpha = 0.5,
    position = "stack"
  ) +
  geom_point(
    data = df_points,
    aes(
      x = mean_ratio,
      y = tool_numeric,         # numeric y-axis
      shape = feature,
      colour = tool
    ),
    size = 7.5,
    stroke = 1.5
  ) +
  #coord_cartesian(ylim = c(0.5, 6)) +
  scale_colour_manual(values = colours_tool) +
  scale_fill_manual(values = c("white", "black", "gray")) +
  scale_x_continuous(
    limits = c(0, 1.5),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5)
  ) +
  scale_y_continuous(           # manually relabel y-axis with tool names
    breaks = c(750, 1000, 1250),
    labels = tool_levels,
    limits = c(0, 2010)
  ) +
  scale_shape_manual(
    values = c(
      17,
      18,
      0,
      1
    )
  ) +
  guides(
    colour = "none"
  ) +
  labs(
    x = "Mean Variation Between Versions (VBV)",
    shape = "Feature",
    fill = "Essentia Category"
  ) +
  theme_maple() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.box = "vertical"
  )

ggsave(
  "img/figure_7.png",
  width = 8.5,
  height = 7.5
)
