# EVALUATING MUSICAL PREDICTIONS WITH MULTIPLE VERSIONS OF A WORK

Data and reproducible analysis for "Evaluating Musical Predictions with Multiple Versions of a Work" by Konrad Swierczek & Michael Schutz accepted in Music & Science. This repository uses renv. More details coming soon.

This analyses for this project are fully reproducible using docker:

```
docker run -d \
  -v ./:/home/rstudio/app \
  -w /home/rstudio/app \
  -p 8787:8787 \
  -p 3838:3838 \
  -e PASSWORD=vbv \
  --name variation-between-versions \
  rocker/verse:4.5.0

```

Due to copyright, we cannot provide the audio files to make the original extractions reproducible.

## Shiny App

```
Rscript app.R
```
