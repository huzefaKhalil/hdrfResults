
resultsViewerUI <- function(id, tData, maxGenes = 100) {
  ns <- NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    titlePanel(title = "HDRF Gene Explorer", windowTitle = "HDRF Gene Explorer"),
    box(
      width = 12,
      title = "Data Selection",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      
      multiColTweak,
      
      # Row to display the input check boxes
      fluidRow(selectDataRow(id, tData)),
      
      # Row to display the input for comparisons
      fluidRow(selectComparisonRow(id)),
      
      # Row to display the input for genes
      fluidRow(selectGenesRow(id, tData, maxGenes)),
      
      # Row to run the analysis
      fluidRow(
        column(width = 2,
               shinyjs::disabled(
                 actionButton(inputId = ns("runAnalysis"),
                              label = "View Results")
               )),
        column(width = 2,
               shinyjs::disabled(
                 downloadButton(outputId = ns("downloadData"),
                                label = "Download Data")
               ))
      )
    ),
    
    # Display the output graphs
    box(
      style = "padding: 15px;",
      width = 12,
      title = "Result",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      # TODO: make this TRUE and add code to uncollapse it when the button is pressed.
      
      # Row to display the heatmap and volcano plot
      fluidRow(
        box(
          width = 12,
          title = "Heatmap",
          column(
            width = 8,
            plotly::plotlyOutput(ns("heatmap"), height = "auto")
          ),
          column(
            width = 4,
            fluidRow(
              htmlOutput(ns("colInfo"), inline = TRUE)
            ),
            fluidRow((plotly::plotlyOutput(ns("volcano"), height = "auto")))
          )
          
          
          #verbatimTextOutput(ns("heatmapCode"))
        )
      ),
      
      # Row to display the forest plot
      fluidRow(
        box(
          width = 12,
          title = "Forest plot",
          box(
            width = 12,
            column(width = 3,
                   selectizeInput(
                     inputId = ns("plotGene"),
                     label = "Choose gene to plot",
                     multiple = FALSE,
                     choices = NULL,
                     selected = NULL
                   )),
            column(width = 3,
                   offset = 3,
                   list(br(),
                        shinyjs::disabled(
                          downloadButton(ns("saveCurrent"),
                                         label = "Save Current Plot")
                        ))),
            column(width = 3,
                   list(br(),
                        shinyjs::disabled(
                          downloadButton(ns("saveAll"),
                                         label = "Save All Plots")
                        )))
          ),
          plotOutput(ns("forest"))
          #verbatimTextOutput(ns("forestPlotCode"))
        )
      )
    )
  )
}

