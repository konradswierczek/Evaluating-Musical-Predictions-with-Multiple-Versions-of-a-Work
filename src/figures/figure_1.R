# src/figures/figure_1.R
# TODO: Add the caption here.
# ============================================================================ #
# Imports.
library(ggplot2)
library(ggforce)
library(MAPLEemo)
library(grid)
library(gridExtra)
library(dplyr)

# Reproducibility.
set.seed(1618)

# ============================================================================ #
# Set up data.
levels <- c(
  "High Accuracy\nHigh Precision", 
  "Low Accuracy\nHigh Precision", 
  "High Accuracy\nLow Precision", 
  "Low Accuracy\nLow Precision"
)

# Define rings dataset with ordered facet labels.
rings <- expand.grid(
  x0 = 0,
  y0 = 0,
  r = c(3, 6, 9, 12, 15),
  facet_label = levels
)
# Ensure facet_label is a factor corresponding to levels.
rings$facet_label <- factor(
  rings$facet_label,
  levels = levels
)

# Create the points dataset in order.
points <- data.frame(
  x = c(
    rnorm(10, mean = 0, sd = 1), # Team A: High Accuracy, High Precision
    rnorm(10, mean = 7.5, sd = 1), # Team B: Low Accuracy, High Precision
    rnorm(10, mean = 0, sd = 2.5), # Team C: High Accuracy, Low Precision
    rnorm(10, mean = 5, sd = 5) # Team D: Low Accuracy, Low Precision
  ),
  y = c(
    rnorm(10, mean = 0, sd = 1), # Team A
    rnorm(10, mean = 7.5, sd = 1), # Team B
    rnorm(10, mean = 0, sd = 2.5), # Team C
    rnorm(10, mean = 3, sd = 5)  # Team D
  ),
  facet_label = factor(c(
    rep("High Accuracy\nHigh Precision", 10), # Team A
    rep("Low Accuracy\nHigh Precision", 10), # Team B
    rep("High Accuracy\nLow Precision", 10), # Team C
    rep("Low Accuracy\nLow Precision", 10) # Team D
  ), levels = levels),
  team = factor(c(
    rep("A", 10),
    rep("B", 10),
    rep("C", 10),
    rep("D", 10)
  ), levels = c("A", "B", "C", "D"))
)

# Create a separate dataset for team labels (to place them at the top-left of each facet).
team_labels <- points |>
  distinct(facet_label, team) |>
  mutate(x = -14, y = 14)

# ============================================================================ #
# Plot it.
# Front of the target.
p1 <- ggplot() +
  geom_circle(
    data = rings,
    aes(x0 = x0, y0 = y0, r = r),
    fill = NA,
    color = "black",
    size = 1
  ) +
  geom_point(
    data = points, aes(x = x, y = y),
    color = "red"
  ) +
  geom_text(
    data = team_labels,
    aes(x = x, y = y, label = team),
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +
  coord_fixed() +
  facet_wrap(
    ~ facet_label, 
    strip.position = "bottom", 
    nrow = 2
  ) +
  theme_maple() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  ) +
  labs(
    title = "Targets from teams A-D (front)​"
  )

# Back of the target.
p2 <- ggplot() +
  geom_circle(
    data = rings,
    aes(
      x0 = x0,
      y0 = y0,
      r = r
    ),
    fill = NA,
    color = "white",
    size = 1
  ) +
  geom_point(
    data = points,
    aes(
      x = x,
      y = y
    ),
    color = "red"
  ) +
  geom_text(
    data = team_labels,
    aes(
      x = x,
      y = y,
      label = team
    ),
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +
  coord_fixed() +
  facet_wrap(
    ~facet_label,
    strip.position = "bottom",
    nrow = 2
  ) +
  theme_maple() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12)
  ) +
  labs(
    title = "Targets from teams A-D (back)​"
  )

# Arrange the two plots side by side.
p3 <- grid.arrange(
  p1,
  p2,
  ncol = 2
)

# Save the output.
ggsave(
  "img/figure_1.png",
  p3,
  width = 14,
  height = 7
)

# ============================================================================ #
# = #