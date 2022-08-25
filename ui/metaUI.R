
metaAnalysisUI <- function(id, tData) {
  ns <- NS(id)
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    titlePanel(title = "Meta-Analysis", windowTitle = "Meta-Analysis"),
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
      fluidRow(selectComparisonRow(id, moduleText = "meta")),
      
      fluidRow(
        br(),
        column(
          width = 4,
          radioButtons(
            inputId = ns("statistic"),
            label = h4("Effect Size Statistic"),
            choices = c("Cohen's D" = "cohensD" ,
                        "Hedge's G" = "hedgesG" ),
            selected = "hedgesG",
            inline = TRUE
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
                         label = "Run Meta-Analysis")
          )
        ),
        column(
          width = 5,
          shinyWidgets::progressBar(
            id = ns("metaProgress"),
            value = 0,
            display_pct = TRUE,
            title = "Awaiting Selection"
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
      
      # Row to display the heatmap and volcano plot
      fluidRow(
        box(
          width = 12,
          title = "Volcano Plot",
          column(
            width = 10,
            plotly::plotlyOutput(ns("volcano"), height = "auto")
          )
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
                   offset = 1,
                   list(br(),
                        shinyjs::disabled(
                          downloadButton(ns("saveCurrent"),
                                         label = "Save Current Plot")
                        ))),
            column(width = 3,
                   offset = 1,
                   list(br(),
                        shinyjs::disabled(
                          downloadButton(ns("saveResult"),
                                         label = "Save Meta-Analysis Results")
                        )))
          ),
          plotOutput(ns("forest"))
          #verbatimTextOutput(ns("forestPlotCode"))
        )
      )
    )
  )
}