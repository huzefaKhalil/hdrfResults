# let's load the UI libraries we will need
library(shiny)
library(shinydashboard)
library(plotly)

# some global variables we need to set
sqlFile <- "resources/hdrfData.sqlite"
rdsFile <- "resources/hdrfLite.rds"
pathwayFile <- "resources/c2.cp.c5.v7.1.gmt.rds"

maxGenes <- 100 # the number of max genes which can be selected for the viewer

# These are the files which have various needed functions and which we need to source
# first the Util functions
source("util/hdrfUtil.R", local = TRUE)
source("util/metaUtil.R", local = TRUE)
source("util/rrhoUtil.R", local = TRUE)

# then the UI functions
source("ui/hdrfUI.R", local = TRUE)
source("ui/resultsViewerUI.R", local = TRUE)
source("ui/resultsDownloaderUI.R", local = TRUE)
source("ui/metaUI.R", local = TRUE)
source("ui/rrhoUI.R", local = TRUE)

# finally the modules themselves
source("modules/resultsViewerModule.R", local = TRUE)
source("modules/resultsDownloaderModule.R", local = TRUE)
source("modules/metaModule.R", local = TRUE)
source("modules/rrhoModule.R", local = TRUE)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "purple",
    
    # Application title
    dashboardHeader(title = "HDRF Results Viewer", titleWidth = "320px"),
    
    # setup the menu
    dashboardSidebar(sidebarMenuOutput("menu")),
    
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "mInfo", h2("Model Information (Todo)")),
            tabItem(tabName = "sInfo", h2("Statistical Information (Todo)")),
            tabItem(tabName = "viewer", resultsViewerUI(id = "viewer")),
            tabItem(tabName = "downloader", resultsDownloaderUI(id = "downloader")),
            tabItem(tabName = "meta", metaAnalysisUI(id = "meta")),
            tabItem(tabName = "rrho", rrhoUI(id = "rrho"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # load the data
    tData <- loadData()
    
    output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Model Information", tabName = "mInfo"),
            shinydashboard::menuItem("Stat Information", tabName = "sInfo"),
            shinydashboard::menuItem("Results Viewer", tabName = "viewer"),
            shinydashboard::menuItem("Data Download", tabName = "downloader"),
            shinydashboard::menuItem("Meta Analysis", tabName = "meta"),
            shinydashboard::menuItem("RRHO", tabName = "rrho"),
            id = "tabs"
        )
    })
    
    observeEvent(input$tabs, {
        if (input$tabs == "viewer") {
            resultsViewerMS("viewer", tData = tData)
        } else if (input$tabs == "downloader") {
            resultsDownloaderMS("downloader", tData = tData)
        } else if (input$tabs == "meta") {
            metaAnalysisMS("meta", tData = tData)
        } else if (input$tabs == "rrho") {
            rrhoMS("rrho", tData = tData)
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
