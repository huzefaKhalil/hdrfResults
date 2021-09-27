resultsDownloaderUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel(title = "Data Downloader", windowTitle = "Data Downloader"),
    box(
      width = 12,
      title = "Data Selection",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      
      multiColTweak,
      
      # Row to display the input check boxes
      fluidRow(selectDataRow(id)),
      
      # Row to display the input for comparisons
      fluidRow(selectComparisonRow(id, moduleText = "meta")),
      
      # Row to run the analysis
      fluidRow(
        br(),
        column(
          width = 4,
          shinyjs::disabled(
            downloadButton(ns("download"),
                           label = "Download Data")
          )
        )
      )
    )
  )
}