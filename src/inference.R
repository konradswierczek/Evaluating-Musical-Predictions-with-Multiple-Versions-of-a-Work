# src/inference.R
# ============================================================================ #
library(MAPLEemo)
library(dplyr)

set.seed(1618)
# ============================================================================ #
df_main_effects <- df_var |>
  ungroup() |>
  factorial_permutation_anova("tool", "feature", "ratio", n = 10000)

# ============================================================================ #
df_res_tool <- tibble()
for (f in unique(df_var$feature)) {
  df_res_tool <- bind_rows(df_res_tool, df_var |>
    filter(feature == f) |>
    mutate(feature = as.character(feature)) |>
    pairwise_permutation_tests("tool", "ratio") |>
    mutate(feature = f)
  )
}
df_res_tool <- df_res_tool |>
  mutate(adjusted_p_value = p.adjust(p_value, method = "BH"))

df_res_tool_significant <- df_res_tool |>
  filter(
    adjusted_p_value < 0.05
  ) |>
  mutate(
    y = case_when(
      group1 == "Essentia" & group2 == "Librosa" ~ 0.95,
      group1 == "Essentia" & group2 == "MIRtoolbox" ~ 1.05,
      group1 == "Librosa" & group2 == "MIRtoolbox" ~ 0.8,
    ),
    sig = case_when(
      adjusted_p_value < 0.001 ~ "***",
      adjusted_p_value < 0.01 ~ "**",
      adjusted_p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# ============================================================================ #
df_res_feature <- tibble()
for (t in unique(df_var$tool)) {
  df_res_feature <- bind_rows(df_res_feature, df_var |>
    filter(tool == t) |>
    mutate(feature = as.character(feature)) |>
    pairwise_permutation_tests("feature", "ratio") |>
    mutate(tool = t)
  )
}
df_res_feature <- df_res_feature |>
  mutate(adjusted_p_value = p.adjust(p_value, method = "BH"))

df_res_feature_significant <- df_res_feature |>
  filter(
    adjusted_p_value < 0.05
  ) |>
  mutate(
    y = case_when(
      group1 %in% c(
        "Relative Mode",
        "Spectral Centroid (Hz)"
      ) & group2 %in% c(
        "Relative Mode",
        "Spectral Centroid (Hz)"
      ) ~ 1.1,
      group1 %in% c(
        "Relative Mode",
        "Onsets (#)"
      ) & group2 %in% c(
        "Relative Mode",
        "Onsets (#)"
      ) ~ 1,
      group1 %in% c(
        "Onsets (#)",
        "Spectral Centroid (Hz)"
      ) & group2 %in% c(
        "Onsets (#)",
        "Spectral Centroid (Hz)"
      ) ~ 0.35,
      group1 %in% c(
        "Tempo (BPM)",
        "Relative Mode"
      ) & group2 %in% c(
        "Tempo (BPM)",
        "Relative Mode"
      ) ~ 0.78,
      group1 %in% c(
        "Tempo (BPM)",
        "Onsets (#)"
      ) & group2 %in% c(
        "Tempo (BPM)",
        "Onsets (#)"
      ) ~ 0.25,
      group1 %in% c(
        "Tempo (BPM)",
        "Spectral Centroid (Hz)"
      ) & group2 %in% c(
        "Tempo (BPM)",
        "Spectral Centroid (Hz)"
      ) ~ 0.65,
      .default = 1
    ),
    sig = case_when(
      adjusted_p_value < 0.001 ~ "***",
      adjusted_p_value < 0.01 ~ "**",
      adjusted_p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# ============================================================================ #
df_var_ins <- df_subset |>
  left_join(
    df_metadata_albums |> select(albumID, instrument)
  ) |>
  group_by(feature, tool, method, instrument) |>
  relative_variation(
    pieceID,
    val
  )

df_main_effects_ins <- df_var_ins |>
  ungroup() |>
  permutation_test_anova("instrument", "ratio", n = 10000)

df_res_ins <- tibble()
df_var_ins <- df_var_ins |>
  mutate(
    feature_tool = paste(feature, tool)
  )
for (ft in unique(df_var_ins$feature_tool)) {
  df_res_ins <- bind_rows(df_res_ins, df_var_ins |>
    filter(feature_tool == ft) |>
    mutate(feature = as.character(feature)) |>
    pairwise_permutation_tests("instrument", "ratio") |>
    mutate(feature_tool = ft)
  )
}

df_res_ins_significant <- df_res_ins |>
  filter(
    p_value < 0.05
  ) |>
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# ============================================================================ #
df_var_mode <- df_subset |>
  left_join(
    df_metadata_pieces |> select(pieceID, mode)
  ) |>
  group_by(feature, tool, method, mode) |>
  relative_variation(
    pieceID,
    val
  )

df_main_effects_mode <- df_var |>
  ungroup() |>
  permutation_test_anova("mode", "ratio", n = 10000)

df_res_mode <- tibble()
df_var_mode <- df_var_mode |>
mutate(feature_tool = paste(feature, tool, sep = " "))
for (ft in unique(df_var_mode$feature_tool)) {
  df_res_mode <- bind_rows(df_res_mode, df_var_mode |>
    filter(feature_tool == ft) |>
    mutate(feature = as.character(feature)) |>
    pairwise_permutation_tests("mode", "ratio") |>
    mutate(feature_tool = ft)
  )
}

df_res_mode_significant <- df_res_mode |>
  filter(
    p_value < 0.05
  ) |>
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# ============================================================================ #
save(
  df_main_effects, df_res_tool, df_res_feature, df_res_ins, df_res_mode,
  file = "data/inference.RData"
)

# ============================================================================ #
