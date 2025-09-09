df_raw |>
  filter(
    extractor %in% c(
      "mirtoolbox_mode_dirGomezMIRtoolbox",
      "mirtoolbox_mode_stftGomezMIRtoolbox"
    )
  ) |>
  spread(extractor, val) |>
  ggplot(
    aes(
      x = mirtoolbox_mode_dirGomezMIRtoolbox,
      y = mirtoolbox_mode_stftGomezMIRtoolbox
    )
  ) +
  geom_point() +
  theme_maple() +
  labs(
    x = "MIRtoolbox mirmode",
    y = "MIRtoolbox Chroma Features with mirmode Reproduction"
  )

ggsave(
  filename = "img/figure_8.png",
  width = 8.5,
  height = 8.5
)
