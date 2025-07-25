library(readr)
library(dplyr)
library(tidyr)
library(intMAPLEemo)

df_raw_edited <- df_raw |>
  mutate(
    albumID = case_when(
      albumID == "flatMIDI" ~ "flatMIDI",
      albumID == "bachAshk2006" ~ "bachAshkenazy2006",
      albumID == "bachAshk2006Deadpan" ~ "bachAshkenazy2006Deadpan",
      albumID == "bachBarenboim2006" ~ "bachBarenboim2003",
      albumID == "bachDemaria2015" ~ "bachDemaria2014",
      albumID == "bachDemaria2015Deadpan" ~ "bachDemaria2014Deadpan",
      albumID == "bachDemus1970" ~ "bachDemus1956",
      albumID == "bachFischer1934" ~ "bachFischer1934",
      albumID == "bachGalling1964" ~ "bachGalling1964",
      albumID == "bachGould1971" ~ "bachGould1971",
      albumID == "bachGulda" ~ "bachGulda1973",
      albumID == "bachHamilton1964" ~ "bachHamilton1964",
      albumID == "bachKirkpatrick1963" ~ "bachKirkpatrick1959",
      albumID == "bachLandowska1951" ~ "bachLandowska1951",
      albumID == "bachLeonhardt1973" ~ "bachLeonhardt1973",
      albumID == "bachMartins1964" ~ "bachMartins1981",
      albumID == "bachNewman1973" ~ "bachNewman1973",
      albumID == "bachNewman2001harpsichord" ~ "bachNewman2000harpsichord",
      albumID == "bachNewman2001piano" ~ "bachNewman2001piano",
      albumID == "bachRichter1970" ~ "bachRichter1973",
      albumID == "bachTureck1953" ~ "bachTureck1953"
    )
  ) |>
  separate_wider_delim(
    feature,
    names = c("feature", "mid"),
    delim = "_"
  ) |>
  separate_wider_delim(
    mid,
    names = c("tool", "method"),
    delim = "-"
  ) |>
  mutate(
    feature = case_when(
      feature == "bpm" ~ "tempo",
      feature == "mirmode" ~ "mode",
      .default = feature
    ),
    method = case_when(
      method == "std" ~ "org",
      method == "sftf" ~ "stft",
      .default = method
    ),
    method = case_when(
      feature == "mode" ~ paste0(method, "GomezMIRtoolbox"),
      .default = method
    ),
    tool = case_when(
      tool == "MIRtoolbox" ~ "mirtoolbox",
      .default = tool
    ),
    extractor = paste(
      tool,
      feature,
      method,
      sep = "_"
    )
  ) |>
  select(-c("filename", "feature", "tool", "method")) |>
  mutate(
    extractor = case_when(
      extractor == "mirtoolbox_mode_orgGomezMIRtoolbox" ~ "mirtoolbox_mode_stftGomezMIRtoolbox",
      extractor == "librosa_tempo_onsets" ~ "librosa_tempo_org",
      extractor == "essentia_onsets_phase" ~ "essentia_onsets_complexphase",
      .default = extractor
    )
  )

df_metadata_pieces <- df_raw |>
  select(pieceID, setCode) |>
  distinct() |>
  mutate(
    key = pretty_pieceID(pieceID, setCode, "%keyName%"),
    mode = pretty_pieceID(pieceID, setCode, "%mode%"),
    pieceNumber = pretty_pieceID(pieceID, setCode, "%pieceNumber%")
  )

df_metadata_albums <- df_raw |>
  select(albumID, setCode) |>
  distinct() |>
  mutate(
    yearRecorded = pretty_albumID(albumID, setCode, "%yearRecorded%"),
    yearReleased = pretty_albumID(albumID, setCode, "%yearReleased%"),
    instrument = pretty_albumID(albumID, setCode, "%performedOn%"),
    recordLabel = pretty_albumID(albumID, setCode, "%recordLabel%"),
    fullName = pretty_albumID(albumID, setCode, "%fullName%")
  )

df_midi <- read_csv("data/midi.csv") |>
  select(-c("filename", "performer", "instrument"))

write_csv(df_raw_edited, "data/main_data.csv")
write_csv(df_metadata_albums, "data/metadata_albums.csv")
write_csv(df_metadata_pieces, "data/metadata_pieces.csv")
write_csv(df_midi, "data/midi_data.csv")
