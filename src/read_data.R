# src/read_data.R
# Read in and prepare all data for further analysis.
# ============================================================================ #
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

# ============================================================================ #
# Read data.
df_raw <- read_csv("data/main_data.csv")
df_midi <- read_csv("data/midi_data.csv")
df_metadata_albums <- read_csv("data/metadata_albums.csv")
df_metadata_pieces <- read_csv("data/metadata_pieces.csv")
df_eme <- read_csv("data/eme_data.csv")

# ============================================================================ #
df_tidy <-  df_raw |>
  separate_wider_delim(
    extractor,
    delim = "_",
    names = c(
      "tool",
      "feature",
      "method"
    )
  ) |>
  mutate(
    tool = case_when(
      tool == "mirtoolbox" ~ "MIRtoolbox",
      .default = str_to_title(tool)
    ),
    feature = case_when(
      feature == "tempo" ~ "Tempo (BPM)",
      feature == "mode" ~ "Relative Mode",
      feature == "centroid" ~ "Spectral Centroid (Hz)",
      feature == "onsets" ~ "Onsets (#)",
    ),
    feature = as.factor(feature),
    feature = fct_relevel(
      feature,
      'Relative Mode',
      "Onsets (#)",
      'Tempo (BPM)',
      'Spectral Centroid (Hz)'
    )
  )
