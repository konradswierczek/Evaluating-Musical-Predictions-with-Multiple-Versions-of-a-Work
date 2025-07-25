# src/read_data.R
# Read in and prepare all data for further analysis.
# ============================================================================ #
library(readr)
library(dplyr)
library(intMAPLEemo)
library(purrr)
library(tidyr)

# ============================================================================ #
# Read data from multiple files and consolidate into one long tibble.
df_raw <- list.files(
  "data",
  pattern = "_",
  full.names = TRUE
) |>
  map_dfr(
    ~ read_csv(.x) |>
      select(-1) |>
      pivot_longer(
        cols = -filename,
        names_to = "feature",
        values_to = "val"
      )
  ) |>
  mutate(
    setCode = sapply(
      filename,
      function(x) helper_translate_filename(x)$setCode
    ),
    pieceID = sapply(
      filename,
      function(x) helper_translate_filename(x)$pieceID
    ),
    pieceID = gsub("-", "_", pieceID),
    albumID = sapply(
      filename,
      function(x) helper_translate_filename(x)$albumID
    )
  )

# ============================================================================ #
# load in data from MIDI analysis.
df_midi <- read_csv("data/midi.csv")

# ============================================================================ #
# Load in Essentia Music Extractor Data.
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
  filter(
    setCode == "bach-1",
    !albumID %in% c(
      "flatMIDI",
      "bachAshkenazy2006Deadpan",
      "bachDemaria2014Deadpan",
      "bachNewman1973"
    )
  ) |>
  pivot_longer(
    cols = -c("setCode", "pieceID", "albumID", "metadata.tags.file_name"),
    names_to = "feature",
    values_to = "val"
  )

# ============================================================================ #
