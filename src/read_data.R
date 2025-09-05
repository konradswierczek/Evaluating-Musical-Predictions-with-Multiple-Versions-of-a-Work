# src/read_data.R
# Read in and prepare all data for further analysis.
# ============================================================================ #
library(readr)
library(intMAPLEemo)
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
df_eme <- read_csv("data/essentiaMusicExtractor.csv") |>
  select(
    "metadata.tags.file_name",
    where(is.double)
  ) |>
  mutate(
    setCode = sapply(
      metadata.tags.file_name, 
      function(x) helper_translate_filename(x)$setCode
    ),
    pieceID = sapply(
      metadata.tags.file_name,
      function(x) helper_translate_filename(x)$pieceID
    ),
    pieceID = gsub("-", "_", pieceID),
    albumID = sapply(
      metadata.tags.file_name,
      function(x) helper_translate_filename(x)$albumID
    )
  ) |>
  pivot_longer(
    cols = -c("setCode", "pieceID", "albumID", "metadata.tags.file_name"),
    names_to = "feature",
    values_to = "val"
  ) |>
  select(-metadata.tags.file_name)
write_csv(df_eme, "data/eme_data.csv")

# ============================================================================ #
df_tidy <-  df_raw |>
  filter(
    extractor %in% c(
      "essentia_tempo_percival", "mirtoolbox_tempo_metre", "librosa_tempo_org",
      "essentia_onsets_rms", "mirtoolbox_onsets_org", "librosa_onsets_org",
      "essentia_mode_cqtGomezMIRtoolbox", "mirtoolbox_mode_stftGomezMIRtoolbox", "librosa_mode_censGomezMIRtoolbox",
      "essentia_centroid_org", "mirtoolbox_centroid_org", "librosa_centroid_org"
    ),
    !albumID %in% c(
      "flatMIDI",
      "bachAshkenazy2006Deadpan",
      "bachDemaria2014Deadpan",
      "bachNewman1973"
    )
  ) |>
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
