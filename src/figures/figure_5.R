# src/figures/figure_5.R
# ============================================================================ #
library(dplyr)
library(ggplot2)
library(gridExtra)
library(boot)
library(ggpubr)

load("data/inference.RData")

mean_func <- function(x, indices) {
  mean(x[indices])
}
# ============================================================================ #

df_boot <- df_var |>
  group_by(
    feature,
    tool
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
  )

figure_5 <- grid.arrange(
  df_boot |>
    ungroup() |>
    ggplot(
      aes(
        x = tool,
        y = mean,
        colour = tool
      )
    ) +
    geom_point(
      aes(
        shape = feature
      ),
      size = 3.5
    ) +
    geom_errorbar(
      aes(
        ymin = boot[, 4],
        ymax = boot[, 5]
      ),
      width = 0.2,
      position = position_dodge(0.9)
    )  +
    geom_bracket(
      data = df_res_tool_significant,
      aes(
        xmin = group1,
        xmax = group2,
        y.position = y,
        label = sig
      ),
      colour = "#999595",
      tip.length = 0
    ) +
    facet_wrap(
      ~ feature,
      nrow = 1
    ) +
    labs(
      y = "Variation Between Versions (VBV)",
      title = "A"
    ) +
    scale_colour_manual(values = tool_cols) +
    scale_shape_manual(
      values = c(
        17,
        18,
        0,
        1
      )
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
        1.1
      )
    ) +
    theme_maple() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = 30,
        hjust = 1
      )
    ),
  df_boot |>
    ggplot(
      aes(
        x = feature,
        y = mean,
        colour = tool
      )
    ) +
    geom_point(
      aes(
        shape = feature
      ),
      size = 3.5
    ) +
    geom_errorbar(
      aes(
        ymin = boot[, 4],
        ymax = boot[, 5]
      ),
      width = 0.2,
      position = position_dodge(0.9)
    )  +
    geom_bracket(
      data = df_res_feature_significant,
      aes(
        xmin = group1,
        xmax = group2,
        label = sig,
        y.position = y
      ),
      colour = "#999595",
      tip.length = 0,
      vjust = 0.75
    ) +
    facet_wrap(. ~ tool, nrow = 1) +
    guides(
      shape = guide_legend(
        title = "Feature",
        nrow = 1,
        byrow = TRUE
      ),
      colour = guide_legend(
        title = "Tool",
        nrow = 1,
        byrow = TRUE
      )
    ) +
    theme_maple() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = 30,
        hjust = 1
      ),
      legend.position.inside = c(0.5, 0.15),
      legend.key.spacing = unit(0.5, "cm")
    ) +
    labs(
      y = "Variation Between Versions (VBV)",
      title = "B"
    ) +
    scale_colour_manual(values = tool_cols) +
    scale_shape_manual(
      values = c(
        17,
        18,
        0,
        1
      )
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
        1.1
      )
    ),
  heights = c(0.5, 0.65)
)

# ============================================================================ #
ggsave(
  figure_5,
  filename = "img/figure_5.png",
  width = 8.5,
  height = 11
)

# ============================================================================ #
