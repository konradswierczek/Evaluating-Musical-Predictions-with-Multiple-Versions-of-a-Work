# app.R
# ============================================================================ #
library(shiny)
library(dplyr)
library(ggplot2)
library(MAPLEemo)

options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)

source("src/read_data.R")

text_size <- 20
title <- "Musical Feature Evaluation with Versions"

# ============================================================================ #
ui <- tagList(
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      }
      .full-width-header {
        width: 100%;
        height: 60px;
        line-height: 60px;
        background-color: #70747c;
        color: white;
        padding: 0 24px;
        display: flex;
        justify-content: flex-start;  /* <-- change here */
        align-items: center;
        box-sizing: border-box;
        gap: 16px; /* spacing between items */
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        font-weight: 500;
        font-size: 1rem;
      }
      .full-width-header h2 {
        margin: 0;
        font-family: inherit;
        font-weight: inherit;
      }
    ")),
    tags$title(title)
  ),
  div(
    class = "full-width-header",
    style = "gap: 16px;",  # spacing between items

    # Maple Lab logo
    a(
      href = "https://maplelab.net",
      target = "_blank",
      img(
        src = "https://maplelabs.info/wp-content/uploads/2018/05/final_head-darker-leaf-png-300x300.png",
        height = "50px",
        style = "display:block;"
      )
    ),

    h2(title, style = "margin: 0; font-size: 2.25rem; line-height: 60px; font-weight: 500;"),

    # GitHub icon
    a(
      href = "https://github.com/konradswierczek/variation-between-versions",
      target = "_blank",
      img(
        src = "https://images.seeklogo.com/logo-png/30/2/github-logo-png_seeklogo-304612.png",
        height = "30px",
        style = "display:block;"
      )
    ),

    # Book icon (Bootstrap SVG)
    a(
      href = "https://journals.sagepub.com/home/mns",
      target = "_blank",
      img(
        src = "https://cdn.jsdelivr.net/npm/bootstrap-icons/icons/book.svg",
        height = "30px",
        style = "display:block;"
      )
    ),

    # Website icon (globe)
    a(
      href = "https://konradswierczek.ca",
      target = "_blank",
      img(
        src = "https://cdn.jsdelivr.net/npm/bootstrap-icons/icons/globe.svg",
        height = "30px",
        style = "display:block;"
      )
    )
  ),

  # Small spacer so content doesn't touch header
  tags$div(style = "height: 20px;"),

  # Main page content with normal Shiny layout
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(
          "feature",
          "Select a Musical Property",
          choices = NULL
        ),
        selectInput(
          "tool",
          "Select an Analysis Tool",
          choices = NULL
        ),
        selectInput(
          "method",
          "Select an Extraction Method",
          choices = NULL
        ),
        wellPanel(
          h4("Information"),
          p(
            "Data from 'Musical Feature Evaluation with Versions', a manuscript accepted in Music & Science. ",
            "Visit ",
            a(
              href = "https://github.com/konradswierczek/variation-between-versions",
              "github.com/konradswierczek/variation-between-versions",
              target = "_blank"
            ),
            " for more information.",
            "Select one of the three music content analysis tools and four features we analyzed. Select an algorithm method. The first plot shows the Variation Between Versions for each of the 24 preludes from Bach's Well Tempered Clavier Book 1 across 17 notable performances. Click on any of the preludes to see the predicted values for each album."
          )
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          column(
            width = 6,
            div(
              style = "aspect-ratio: 1 / 1; width: 100%;",
              plotOutput(
                "dotplot",
                width = "100%",
                height = "100%",
                click = clickOpts(id = "plot_click")
              )
            )
          ),
          column(
            width = 6,
            div(
              style = "aspect-ratio: 1 / 1; width: 100%;",
              plotOutput(
                "albums",
                width = "100%",
                height = "100%",
                click = clickOpts(id = "album_click")
              )
            )
          )
        ),
        uiOutput("score")
      )
    )
  )
)

# ============================================================================ #
server <- function(input, output, session) {
  updateSelectInput(
    session,
    "feature",
    choices = df_tidy |>
      pull(feature) |>
      unique()
  )
  updateSelectInput(
    session,
    "tool",
    choices = df_tidy |>
      pull(tool) |>
      unique()
  )
  observe({
    req(input$feature, input$tool)
    updateSelectInput(
      session,
      "method",
      choices = df_tidy |>
        filter(
          feature == input$feature,
          tool == input$tool
        ) |>
        pull(method) |>
        unique()
    )
  })

  df_var <- reactive({
    df_tidy |>
      filter(
        feature == input$feature,
        tool == input$tool,
        method == input$method
      ) |>
      relative_variation(
        pieceID,
        val
      ) |>
      left_join(
        df_metadata_pieces |>
          select(-pieceNumber),
        by = "pieceID"
      ) |>
      mutate(mean_ratio = mean(ratio)) |>
      discrete_histogram(
        val_col = "ratio"
      )
  })

  pieceID <- reactiveVal("M0")
  observeEvent(input$plot_click, {
    req(input$plot_click)
    df <- df_var()
    closest_point <- nearPoints(
      df,
      input$plot_click,
      xvar = "x_mid",
      yvar = "y",
      threshold = 30, # Adjust if clicks are not responsive enough (centered around the actual data point).
      maxpoints = 1
    )
    pieceID(closest_point$pieceID)
  })

  output$dotplot <- renderPlot({
    df_var()|>
      ggplot(
        aes(
          x = x_mid,
          y = y,
          label = key,
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = mode
        )
      ) +
      geom_vline(
        xintercept = 1,
        colour = "lightgray",
        linetype = "dashed"
      ) +
      geom_vline(
        aes(xintercept = mean_ratio),
        colour = "#2d2d2d"
      ) +
      geom_rect(
        colour = "black"
      ) +
      geom_text(
        size   = text_size * 0.75,
        colour = "white"
      ) +
      scale_fill_manual(values = colours_mode) +
      labs(
        x = "Mean Variation Between Versions (VBV)",
        y = "Number of Preludes",
        fill = "Nominal Mode",
        title = paste0(input$tool, ": ", input$feature, " [", input$method, "]")
      ) +
      theme_maple() +
      theme(
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = text_size),
        axis.text = element_text(size = text_size * 0.8),
        plot.title = element_text(size = text_size)
      )
  })

  output$albums <- renderPlot({
    df_tidy |>
      filter(
        feature == input$feature,
        tool == input$tool,
        method == input$method,
        pieceID == pieceID()
      ) |>
      discrete_histogram(
        val_col = "val"
      ) |>
      left_join(
        df_metadata_pieces |>
          select(-pieceNumber),
        by = "pieceID"
      ) |>
      left_join(
        df_metadata_albums,
        by = "albumID"
      ) |>
      ggplot(
        aes(
          x = x_mid,
          y = y,
          label = paste(fullName, "\n", yearReleased),
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax
        )
      ) +
      geom_rect(
        colour = "black"
      ) +
      geom_text(
        size   = text_size * 0.25,
        colour = "white"
      ) +
      labs(
        x = paste0("Predicted Value: ", input$feature),
        y = "Number of Albums",
        fill = "Nominal Mode",
        title = paste0(
          "Prelude in ",
          df_metadata_pieces |>
            filter(pieceID == pieceID()) |>
            mutate(key = paste(key, mode)) |>
            pull(key)
        )
      ) +
      theme_maple() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = text_size),
        axis.text = element_text(size = text_size * 0.8),
        plot.title = element_text(size = text_size)
      )
  })

  output$score <- renderUI({
    img(src = paste0("bach-1_", pieceID(), "_snippet.png"), width = "100%")
  })
}

# ============================================================================ #
shinyApp(ui = ui, server = server)

# ============================================================================ #
