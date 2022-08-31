# let's load the UI libraries we will need
library(shiny)
library(shinydashboard)
library(plotly)
library(future)
library(promises)
library(ipc)

plan(multiprocess)

#
# some global variables we need to set
sqlFile <- "resources/hdrfData.sqlite"
rdsFile <- "resources/hdrfLite.rds"
pathwayFile <- "resources/c2.cp.c5.v7.1.gmt.rds"

maxGenes <- 100 # the number of max genes which can be selected for the viewer

# These are the files which have various needed functions and which we need to source
# first the Util functions
source("util/hdrfUtil.R", local = TRUE)
source("util/rrhoUtil.R", local = TRUE)

# then the UI functions
source("ui/hdrfUI.R", local = TRUE)
source("ui/resultsViewerUI.R", local = TRUE)
source("ui/resultsDownloaderUI.R", local = TRUE)
source("ui/metaUI.R", local = TRUE)
source("ui/rrhoUI.R", local = TRUE)
source("ui/animalModelsUI.R", local = TRUE)
source("ui/statModelUI.R", local = TRUE)

# finally the modules themselves
source("modules/resultsViewerModule.R", local = TRUE)
source("modules/resultsDownloaderModule.R", local = TRUE)
source("modules/metaModule.R", local = TRUE)
source("modules/rrhoModule.R", local = TRUE)

options(future.globals.maxSize = 2 * 1024^3)

tData <- loadData()

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  
  # Application title
  dashboardHeader(title = "HDRF Gene Explorer", titleWidth = "320px"),
  
  # setup the menu
  dashboardSidebar(sidebarMenuOutput("menu")),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "mInfo", animalModelsUI(id = "animalModelInfo")),
      tabItem(tabName = "sInfo", statModelUI(id = "statModelInfo")),
      tabItem(tabName = "viewer", resultsViewerUI(id = "viewer", tData = tData, maxGenes = maxGenes)),
      tabItem(tabName = "downloader", resultsDownloaderUI(id = "downloader", tData = tData)),
      tabItem(tabName = "meta", metaAnalysisUI(id = "meta", tData = tData)),
      tabItem(tabName = "rrho", rrhoUI(id = "rrho", tData = tData))
    )
  )
)

# Define server logic
server <- function(input, output) {
  # load the data
  #tData <- loadData()
  # the reactive Vals for the various modules are defined here and passed to them.
  resVals <- reactiveValues(selectedHdrf = NULL,
                            availableHdrf = NULL)
  resSelectedData <- reactiveValues()
  
  rrhoVals <- reactiveValues(selectedHdrf = NULL,
                             availableHdrf = NULL)
  rrhoSelectedData <- reactiveValues()
  rrhoRes <- reactiveValues()
  
  dVals <- reactiveValues(selectedHdrf = NULL,
                          availableHdrf = NULL)
  
  metaVals <- reactiveValues(selectedHdrf = NULL,
                             availableHdrf = NULL)
  metaRes <- reactiveValues()
  
  output$menu <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Animal Models", tabName = "mInfo"),
      shinydashboard::menuItem("Stats Methods", tabName = "sInfo"),
      shinydashboard::menuItem("Results Viewer", tabName = "viewer"),
      shinydashboard::menuItem("Data Download", tabName = "downloader"),
      shinydashboard::menuItem("Meta Analysis", tabName = "meta"),
      shinydashboard::menuItem("RRHO", tabName = "rrho"),
      id = "tabs"
    )
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "viewer") {
      resultsViewerMS("viewer", tData = tData, resVals, resSelectedData)
    } else if (input$tabs == "downloader") {
      resultsDownloaderMS("downloader", tData = tData, dVals)
    } else if (input$tabs == "meta") {
      metaAnalysisMS("meta", tData = tData, metaVals, metaRes)
    } else if (input$tabs == "rrho") {
      rrhoMS("rrho", tData = tData, rrhoVals, rrhoSelectedData, rrhoRes)
    }
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
