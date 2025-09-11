# src/figures/figure_6.R
# ============================================================================ #
library(dplyr)
library(ggplot2)
library(gridExtra)
library(boot)

load("data/inference.RData")

mean_func <- function(x, indices) {
  mean(x[indices])
}

# ============================================================================ #

figure_6 <- grid.arrange(
  df_var_ins |>
    group_by(
      feature,
      tool,
      instrument,
    ) |>
    summarize(
      mean = mean(ratio),
      boot = boot.ci(
        boot(
          ratio,
          mean_func,
          R = 1000
        ),
        type = "bca"
      )$bca
    ) |>
    ggplot(
      aes(
        x = instrument,
        y = mean,
        col = instrument
      )
    ) +
    geom_point(
      aes(
        shape = feature
      ),
      size = 1.5
    ) +
    geom_errorbar(
      aes(
        ymin = boot[, 4],
        ymax = boot[, 5]
      ),
      width = 0.2
    )  +
    facet_grid(
      tool ~ feature
    ) +
    theme_maple() +
    theme(
      legend.position = "none"
    ) +
    labs(
      y = "Variation Between Versions (VBV)",
      x = "Instrument",
      title = "A"
    ) +
    scale_y_continuous(
      breaks = c(
        0,
        0.25,
        0.5,
        0.75,
        1
      ),
      limits = c(
        0,
        1.15
      )
    ) +
    scale_colour_manual(
      values = c(
        "purple2",
        "sienna2"
      )
    ) +
    scale_shape_manual(
      values = c(
        17,
        18,
        0,
        1
      )
    ),

  df_var_mode |>
    group_by(
      feature,
      tool,
      mode,
    ) |>
    summarize(
      mean = mean(ratio),
      boot = boot.ci(
        boot(
          ratio,
          mean_func,
          R = 1000
        ),
        type = "bca"
      )$bca
    ) |>
    ggplot(
      aes(
        x = mode,
        y = mean,
        col = mode
      )
    ) +
    geom_point(
      aes(
        shape = feature
      ),
      size = 1.5
    ) +
    geom_errorbar(
      aes(
        ymin = boot[, 4],
        ymax = boot[, 5]
      ),
      width = 0.2
    )  +
    facet_grid(
      tool ~ feature
    ) +
    guides(
      shape = guide_legend(
        title = "Feature"
      ),
      colour = "none"
    ) +
    theme_maple() +
    theme(
      legend.position = "bottom",
      legend.key.spacing = unit(0.5, "cm")
    ) +
    labs(
      y = "Variation Between Versions (VBV)",
      x = "Nominal Mode",
      title = "B"
    ) +
    scale_y_continuous(
      breaks = c(
        0,
        0.25,
        0.5,
        0.75,
        1
      ),
      limits = c(
        0,
        1.15
      )
    ) +
    scale_colour_manual(values = mode_cols) +
    scale_shape_manual(
      values = c(
        17,
        18,
        0,
        1
      )
    ) +
    guides(
      shape = guide_legend(
        title = "Feature",
        nrow = 1
      )
    ),
  ncol = 1,
  heights = c(0.475, 0.55)
)

# ============================================================================ #
ggsave(
  figure_6,
  filename = "img/figure_6.png",
  width = 8.5,
  height = 11
)

# ============================================================================ #
