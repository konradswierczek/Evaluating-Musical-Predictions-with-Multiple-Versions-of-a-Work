library(RMariaDB)

con <- dbConnect(
  MariaDB(),
  user = "maple_read",
  password = "maple",
  dbname = "emotional_piano_project",
  host = "130.113.231.85"
)

# Define query.
query <- paste(
  "SELECT",
  "mir.val, mir.extractor, files.setCode, files.albumID, files.pieceID",
  "FROM mir",
  "LEFT JOIN files USING (filename)",
  "LEFT JOIN extractors USING (extractor)",
  "WHERE mir.type = 'midi'",
  "AND files.setCode = 'bach-1'",
  "AND extractors.tag = 'latest'",
  sep = "\n"
)

# Pull the data from database.
df_raw_sql <- dbGetQuery(con, query) %>%
  tibble()
dbDisconnect(con)

df_raw_sql |>
  filter(
    extractor %in% c(
      "pyramidi_tempo_og",
      "pyramidi_mirmode_music21GomezMIRtoolbox",
      "pyramidi_mirmode_pyramidiGomezMIRtoolbox",
      "autoSDC_attacks_org"
    )
  ) |>
  mutate(
    feature = case_when(
      extractor == "pyramidi_tempo_og" ~ "tempo",
      extractor == "pyramidi_mirmode_music21GomezMIRtoolbox" ~ "mode",
      extractor == "pyramidi_mirmode_pyramidiGomezMIRtoolbox" ~ "mode_alt",
      extractor == "autoSDC_attacks_org" ~ "onsets"
    ),
    albumID = "flatMIDI"
  ) |>

read_csv("data/tempos.csv")

con <- dbConnect(
  MariaDB(),
  user = "maple_read",
  password = "maple",
  dbname = "emotional_piano_project",
  host = "130.113.231.85"
)

# Define query.
query <- paste(
  "SELECT",
  "mir.val, mir.extractor, files.setCode, files.albumID, files.pieceID",
  "FROM mir",
  "LEFT JOIN files USING (filename)",
  "LEFT JOIN extractors USING (extractor)",
  "WHERE mir.type = 'manipulations'",
  "AND files.setCode = 'bach-1'",
  "AND mir.filename LIKE '%_NA_NA_NA%'",
  "AND extractors.tag = 'latest'",
  sep = "\n"
)


# Pull the data from database.
df_raw_sql2 <- dbGetQuery(con, query) %>%
  tibble()
dbDisconnect(con)

left_join(
  df_raw_sql2 |>
    filter(
      extractor %in% c(
        "essentia_centroid_org",
        "essentia_mode_cqtGomezMIRtoolbox",
        "essentia_mode_stftGomezMIRtoolbox",
        "essentia_onsets_complex",
        "essentia_onsets_complexphase",
        "essentia_onsets_flux",
        "essentia_onsets_hfc",
        "essentia_onsets_melflux",
        "essentia_onsets_rms",
        "essentia_tempo_degara",
        "essentia_tempo_multifeature",
        "essentia_tempo_percival",
        "librosa_centroid_org",
        "librosa_mode_censGomezMIRtoolbox",
        "librosa_mode_cqtGomezMIRtoolbox",
        "librosa_mode_stftGomezMIRtoolbox",
        "librosa_mode_vqtGomezMIRtoolbox",
        "librosa_onsets_org",
        "librosa_tempo_org",
        "mirtoolbox_centroid_org",
        "mirtoolbox_mode_stftGomezMIRtoolbox",
        "mirtoolbox_onsets_org",
        "mirtoolbox_tempo_classical",
        "mirtoolbox_tempo_metre"
      )extractor", "albumID", "setCode")
) |>
  separate_wider_delim(
    extractor,
    ) |>
    mutate(albumID = "flatMIDI"),
  df_raw |> filter(albumID == "flatMIDI"),
  by = c("pieceID", "extractor", "albumID", "setCode")
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
  ggplot(
    aes(
      x = val.x,
      y = val.y,
      colour = method
    )
  ) +
  geom_point() +
  facet_wrap(
    tool ~ feature,
    scales = "free"
  ) +
  labs(
    x = "Baseline Files From Transformations Paper",
    y = "flatMIDI files from VBV Paper"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )"


con <- dbConnect(
  MariaDB(),
  user = "maple_read",
  password = "maple",
  dbname = "emotional_piano_project",
  host = "130.113.231.85"
)

# Define query.
query <- paste(
  "SELECT",
  "mir.val, mir.extractor, files.setCode, files.albumID, files.pieceID",
  "FROM mir",
  "LEFT JOIN files USING (filename)",
  "LEFT JOIN extractors USING (extractor)",
  "WHERE mir.type = 'manipulations'",
  "AND files.setCode = 'bach-1'",
  "AND mir.filename LIKE '%_NA_NA_NA%'",
  "AND extractors.tag = 'latest'",
  sep = "\n"
)


# Pull the data from database.
df_raw_sql2 <- dbGetQuery(con, query) %>%
  tibble()
dbDisconnect(con)



