library(MAPLEemo)
# Calculate relative variation and add last labels.
df_var <- df_subset |>
  group_by(feature, tool, method) |>
  relative_variation(
    pieceID,
    val
  ) |>
  left_join(
    df_metadata_pieces |>
      select(-pieceNumber),
    by = "pieceID"
  ) |>
  group_by(feature, tool) |>
  mutate(mean_ratio = mean(ratio)) |>
  ungroup()
# ============================================================================ #
# Suggestion of R3: calculate VBV based on versions/one album.
df_var2 <- df_subset |>
  group_by(feature, tool, method, albumID) |>
  relative_variation(
    pieceID,
    val
  ) |>
  left_join(
    df_metadata_pieces |>
      select(-pieceNumber),
    by = "pieceID"
  ) |>
  group_by(feature, tool) |>
  mutate(mean_ratio = mean(ratio)) |>
  ungroup()
# ============================================================================ #

df_eme_var <- df_eme |>
  filter(
    setCode == "bach-1",
    !albumID %in% c(
      "flatMIDI",
      "bachAshkenazy2006Deadpan",
      "bachDemaria2014Deadpan",
      "bachNewman1973"
    )
  ) |>
  group_by(feature) |>
  relative_variation(
    pieceID,
    val
  ) |>
  left_join(
    df_metadata_pieces |>
      select(-pieceNumber),
    by = "pieceID"
  ) |>
  group_by(feature) |>
  summarize(mean_ratio = mean(ratio)) |>
  ungroup()
