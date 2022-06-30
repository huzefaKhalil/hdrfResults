rrhoUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    titlePanel(title = "Rank Rank Hypergeometric Overlap", windowTitle = "RRHO"),
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
      fluidRow(selectComparisonRow(id, selectAll = FALSE, moduleText = "rrho")),
      
      fluidRow(
        br(),
        column(
          width = 6,
          radioButtons(
            inputId = ns("statistic"),
            label = h4("Test Statistic"),
            choices = c("Hypergeometric Test p-value" = "hyper" ,
                        "Log Odds Ratio" = "fisher"),
            selected = "hyper",
            inline = FALSE
          )
        )
      ),
      
      # Row to run the analysis
      fluidRow(
        br(),
        column(
          width = 3,
          shinyjs::disabled(
            actionButton(inputId = ns("runAnalysis"),
                         label = "Run RRHO")
          )
        )
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
      
      # Row to display the heatmap and venn diagrams
      fluidRow(
        column(
          width = 4,
          h3("Enrichment Plot")
        ),
        column(
          width = 3,
          offset = 5,
          br(),
          shinyjs::disabled(
            downloadButton(ns("saveEnrichment"),
                           label = "Save Plot")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 10,
          plotOutput(ns("enrichment"), height = "600px")
        )
      ),
      
      # Row to display the forest plot
      fluidRow(
        column(
          width = 4,
          h3("Gene overlap plots")
        ),
        column(
          width = 3,
          offset = 5,
          br(),
          shinyjs::disabled(
            downloadButton(ns("saveGeneList"),
                           label = "Save Genes Overlap Lists")
          )
        )
      ),
      fluidRow(
        
      ),
      
      fluidRow(
        column(
          width = 4,
          h4("Concordant Gene Overlap")
        )
      ),
      fluidRow(
        column(
          width = 6,
          plotOutput(ns("upUpPlot"))
        ),
        column(
          width = 6,
          plotOutput(ns("downDownPlot"))
        )
      ),
      
      fluidRow(
        column(
          width = 4,
          h4("Discordant Gene Overlap")
        )
      ),
      fluidRow(
        column(
          width = 6,
          plotOutput(ns("upDownPlot"))
        ),
        column(
          width = 6,
          plotOutput(ns("downUpPlot"))
        )
      )
      
      #verbatimTextOutput(ns("forestPlotCode"))
      
    )
  )
}