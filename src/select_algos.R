# src/select_algos.R
# ============================================================================ #
library(dplyr)

# ============================================================================ #
df_mse <- left_join(
  df_tidy |>
    filter(albumID == "flatMIDI"),
  df_midi |>
  mutate(
    feature = case_when(
      feature == "tempo" ~ "Tempo (BPM)",
      feature == "mode" ~ "Relative Mode",
      feature == "onsets" ~ "Onsets (#)"
    ),
    feature = as.factor(feature),
    feature = fct_relevel(
      feature,
      'Relative Mode',
      "Onsets (#)",
      'Tempo (BPM)'
    )
  ) |>
  rename(midi_val = val),
  by = c("pieceID", "feature")
) |>
  group_by(tool, method, feature) |>
  summarise(mse = mean((midi_val - val)^2, na.rm = TRUE)) |>
  arrange(feature, tool, mse) |>
  filter(
    method != "dirGomezMIRtoolbox"
  ) |>
  ungroup()

# ============================================================================ #
df_subset <- df_tidy |>
  semi_join(
    df_mse |>
      group_by(tool, feature) |>
      slice_min(order_by = mse, n = 1) |>
      select(-mse)
  ) |>
  filter(
    !albumID %in% c(
      "flatMIDI",
      "bachAshkenazy2006Deadpan",
      "bachDemaria2014Deadpan",
      "bachNewman1973"
    )
  )

# ============================================================================ #
