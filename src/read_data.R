# src/read_data.R
# Read in and prepare all data for further analysis.
# ============================================================================ #
library(readr)
library(intMAPLEemo)
library(dplyr)
library(tidyr)

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
  )

# ============================================================================ #
