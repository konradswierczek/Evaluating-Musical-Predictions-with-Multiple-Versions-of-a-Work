library(ggplot2)
library(MAPLEemo)

# Example
df_var |>
  discrete_histogram(
    val_col = "ratio",
    group_cols = c("feature", "tool")
  ) |>
  ggplot(
    aes(
      x = x_mid,
      y = y,
      label = key,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = mode
    )
  ) +
  geom_vline(
    xintercept = 1,
    colour = "lightgray",
    linetype = "dashed"
  ) +
  geom_vline(
    aes(xintercept = mean_ratio),
    colour = "#2d2d2d"
  ) +
  geom_rect() +
  geom_text(
    size   = 2.5,
    colour = "white"
  ) +
  facet_grid(
    feature ~ tool
  ) +
  scale_fill_manual(values = colours_mode) +
  labs(
    x = "Mean Variation Between Versions (VBV)",
    y = "Number of Preludes",
    fill = "Nominal Mode"
  ) +
  theme_maple() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(
  "img/figure_4.png",
  width = 8.5,
  height = 7.5
)  
