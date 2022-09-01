

metaAnalysisMS <- function(id, tData, metaVals, metaRes) {
  
  moduleServer(
    id,
    
    function(input, output, session) {
      # save directory for the meta analysis
      saveDir <- file.path("resources/metaRes")
      
      # now for ipc and futures stuff
      queue <- shinyQueue()
      queue$consumer$start(250)  # Execute signal ever 250milliseconds
      
      metaOutput <- reactiveVal()    # this holds the output of the meta-analysis
      geneList <- reactiveVal()
      geneOn <- reactiveVal()
      theData <- reactiveVal()
      
      # this method updates the comparisons which are available to the user
      updateSelection <- function() {
        req(input$treatment)
        req(input$timepoint)
        req(input$region)
        req(input$sex)
        req(input$model)
        
        hd <- getComparison(
          tData$hdrf,
          treatment = input$treatment,
          timepoint = input$timepoint,
          region = input$region,
          sex = input$sex,
          model = input$model,
          species = input$species
        )
        
        # set the available comparisons
        if (!is.null(metaVals$selectedHdrf)) {
          ids <- getIds(metaVals$selectedHdrf)
          ids <- gsub("^-", "", ids)
          hd <- removeComparison(hd, ids)
        }
        
        # set the available Hdrf
        metaVals$availableHdrf <- hd
        
        # shinyWidgets::updateMultiInput(session,
        #                   "comparisons",
        #                   choices = comp)
        updateSelectHdrf(session, "availableComparisons", hd)
      }
      
      # select comparisons. This method moves selected comparisons from available to selected
      sComp <- function(sIds) {
        # so, here we have to update both text boxes. These are:
        # input$availableComparisons and input$selectedComparisons
        # also have to update metaVals$availableHdrf and metaVals$selectedHdrf
        
        # set the selected hdrf and available hdrf
        metaVals$availableHdrf <- removeComparison(metaVals$availableHdrf,
                                                   sIds)
        
        if (!is.null(metaVals$selectedHdrf)) {
          sIds <- c(sIds, getIds(metaVals$selectedHdrf))
        }
        
        metaVals$selectedHdrf <- getComparisonById(tData$hdrf,
                                                   sIds)
        
        # now, update the selectInput boxes
        updateSelectHdrf(session, "availableComparisons", metaVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", metaVals$selectedHdrf)
      }
      
      # The logic to select comparisons.
      observe({
        req(input$treatment,
            input$timepoint,
            input$region,
            input$sex,
            input$model)
        updateSelection()
      })
      
      #****************************************************
      #* This code is for selecting comparisons and unselecting them
      #****************************************************
      
      #enable button to select comparison
      observe({
        shinyjs::disable(id = "selectComparisons")
        req(input$availableComparisons)
        shinyjs::enable("selectComparisons")
      })
      
      # enable button to unselect comparison
      observe({
        shinyjs::disable("unSelectComparisons")
        req(input$selectedComparisons)
        shinyjs::enable("unSelectComparisons")
      })
      
      # enable button to select all comparison
      observe({
        shinyjs::disable("selectAllComparisons")
        req(metaVals$availableHdrf)
        shinyjs::enable("selectAllComparisons")
      })
      
      # enable button to reverse selected comparison
      observe({
        shinyjs::disable("reverseComparisons")
        req(input$selectedComparisons)
        shinyjs::enable("reverseComparisons")
      })
      
      # when the button is pressed to select comparisons
      # move the selected comparisons to the other text box
      shinyjs::onclick("selectComparisons", {
        # step one, get the ids being selected
        sIds <- input$availableComparisons
        sComp(sIds)
      })
      
      shinyjs::onclick("selectAllComparisons", {
        # step one, get the ids being selected
        sIds <- getIds(metaVals$availableHdrf)
        sComp(sIds)
      })
      
      shinyjs::onclick("unSelectComparisons", {
        # get the ids being moved back
        sIds <- input$selectedComparisons
        
        # now update the selected hdrfs
        metaVals$selectedHdrf <- removeComparison(metaVals$selectedHdrf, sIds)
        
        # We don't handles reversed comparisons here... if they are unselected, they will
        # loose their reversed status
        sIds <- gsub("^-", "", sIds)
        
        # now move these to available Ids
        if (!is.null(metaVals$availableHdrf))
          aIds <- c(sIds, getIds(metaVals$availableHdrf))
        else
          aIds <- sIds
        
        # now update the hdrfs
        metaVals$availableHdrf <- getComparisonById(tData$hdrf, aIds)
        
        # now update the selectInputs
        updateSelectHdrf(session, "availableComparisons", metaVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", metaVals$selectedHdrf)
      })
      
      # reverse the selected comparisons
      shinyjs::onclick("reverseComparisons", {
        metaVals$selectedHdrf <- reverseComparison(metaVals$selectedHdrf,
                                                   input$selectedComparisons)
        updateSelectHdrf(session, "selectedComparisons", metaVals$selectedHdrf)
      })
      
      #****************************************************
      #* Update the progress bar
      observe({
        req(geneOn(), geneList())

        val <- which(geneList() == geneOn()) / length(geneList()) * 100
        
        shinyWidgets::updateProgressBar(
          session,
          id = "metaProgress",
          value = val,
          title = "Running Meta-Analysis"
        )
        
      })
      
      #****************************************************
      
      # now we get to the part which runs the meta-analysis. This first part just gets the data.
      observe({
        shinyjs::disable("runAnalysis")
        req(metaVals$selectedHdrf)
        shinyjs::enable("runAnalysis")
      })
      
      # run the meta analysis!
      shinyjs::onclick("runAnalysis", {
        shinyjs::disable("runAnalysis")
        
        theData(NULL)

        sIds <- getIds(metaVals$selectedHdrf)
        nSelected <- length(sIds)
        
        # TODO
        # 1. Check if the meta-analysis exists
        fName <- file.path(saveDir, paste0(digest::digest(sIds), ".csv"))
        
        shinyWidgets::updateProgressBar(
          session,
          id = "metaProgress",
          value = 0,
          title = "Loading Data"
        )
        
        sIds <- getComparisonById(tData$hdrf, sIds)
        
        # 3. If the file doesn't exist, let's get the data
        future({
          
          tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
          theData <- flattenDge(sIds, conn = tempConn)
          DBI::dbDisconnect(tempConn)
          
          # first, split it by id
          theData <- split(theData, by = "id")
          
          theData     # actually return this
          
        }, seed = TRUE) %...>% theData
        
        NULL  # return something while the data is loading is running
        
      })
      
      # run the meta-analysis calculations!
      observe({
        req(theData())

        metaOutput(NULL)
        geneOn(NULL)
        geneList(NULL)

        if (input$statistic == "cohensD") {
          st <- "cohensD"
          vt <- "varD"
        } else {
          st <- "hedgesG"
          vt <- "varG"
        }
        
        theData <- theData()
        geneList(names(theData))
        
        future({
          mapply(function(x, nx) {
            
            queue$producer$fireAssignReactive("geneOn", nx)
            
            if (nrow(x) < (nSelected*3/4)) return(NULL) #make sure at least 75% x are present
            
            tryCatch({
              tOut <- metafor::rma(x[[st]], x[[vt]], method = "REML")
              
              return(data.frame(id = nx,
                                estimate = tOut$b[1,1],
                                se = tOut$se,
                                pval = tOut$pval,
                                zval = tOut$zval))
            }, error = function(e) return(NULL), finally = {})
            
          }, theData, names(theData), SIMPLIFY = FALSE)
          
        }, seed = TRUE) %...>% metaOutput
        
        NULL # return something while meta-analysis is running
      })
      
      # now we have the output, let's format it into the finished data.table
      observe({
        
        req(metaOutput())
        geneList(NULL)
        geneOn(NULL)
        
        shinyWidgets::updateProgressBar(
          session,
          id = "metaProgress",
          value = 100,
          title = "Finishing up - Generating results"
        )
        
        metaOutput <- data.table::rbindlist(metaOutput()[lengths(metaOutput()) != 0])
        metaOutput$fdr <- p.adjust(metaOutput$pval, method = "fdr")
        metaOutput$logPval <- -log10(metaOutput$pval)
        metaOutput <- merge(metaOutput,
                            tData$hdrf@ids[, c("id", "compound.symbol")],
                            all.x = TRUE, all.y = FALSE, by = "id")
        metaOutput$hoverText <- paste0("Symbol: ", metaOutput$compound.symbol, "<br>",
                                       "Estimate: ", format(metaOutput$estimate, digits = 4), "<br>",
                                       "Adjusted P-value:", format(metaOutput$fdr, digits = 4))
        metaOutput <- metaOutput[order(metaOutput$pval), ]
        
        metaRes$metaOutput <- metaOutput
        metaRes$data <- theData()
        
        shinyWidgets::updateProgressBar(
          session,
          id = "metaProgress",
          value = 100,
          title = "All Done!"
        )
        
        theData(NULL)
        metaOutput(NULL)
        shinyjs::enable("saveResult")
        shinyjs::enable("runAnalysis")
      })
      
      observe({
        req(metaRes$metaOutput)
        
        updateSelectizeInput(
          session,
          inputId = "plotGene",
          choices = setNames(metaRes$metaOutput$id, metaRes$metaOutput$compound.symbol),
          selected = metaRes$metaOutput$id[1],
          server = FALSE
        )
      })
      
      output$volcano <- renderPlotly({
        req(metaRes$metaOutput)
        
        plot_ly(
          data = metaRes$metaOutput,
          type = "scattergl",
          x = ~ estimate,
          y = ~ logPval,
          color = ifelse(metaRes$metaOutput$fdr <= 0.05, "FDR <= 0.05", "FDR > 0.05"),
          colors = ifelse(metaRes$metaOutput$fdr <= 0.05, "red", "black"),
          showlegend = TRUE,
          hoverinfo = "text",
          key = ~ id,
          text = ~ hoverText,
          source = "volcano",
          mode = "markers",
          marker = list(size = 4, opacity = 0.8)
        ) %>%
          layout(
            xaxis = list(title = "Meta-Analysis Estimate"),
            yaxis = list(title = "-Log(p-value)")
          ) %>%
          event_register("plotly_click")
      })
      
      clickedPoint <- reactive({
        event_data(event = "plotly_click", source = "volcano")
      })
      
      observeEvent(clickedPoint(), {
        
        gene <- clickedPoint()$key
        
        updateSelectizeInput(
          session,
          inputId = "plotGene",
          choices = setNames(metaRes$metaOutput$id, metaRes$metaOutput$compound.symbol),
          selected = clickedPoint()$key,
          server = FALSE
        )
      })
      
      # get the actual forest plot
      observe({
        req(input$plotGene)
        req(metaRes$data)
        
        # generate the forest plot
        gene <- input$plotGene
        sData <- metaRes$data[[gene]]
        
        metaRes$plot <- smoothForest(
          sData,
          metaStatistic = TRUE,
          includeModel = TRUE,
          includeName = TRUE,
          includeRegion = TRUE,
          includeEstimate = TRUE,
          includeTreatment = FALSE,
          includeSex = FALSE,
          includePval = TRUE,
          orderBy = "estimate",
          estimate = input$statistic,
          fontSize = 3.5,
          ids = tData$hdrf@ids
        )[[1]]
        
        shinyjs::enable("saveCurrent")
      })
      
      output$forest <- renderPlot({
        req(metaRes$plot)
        
        metaRes$plot
      }, height = function() {
        req(input$plotGene)
        req(metaRes$data)
        return(200 + 19 * nrow(metaRes$data[[input$plotGene]]))
      })
      
      output$saveCurrent <- downloadHandler(
        filename = function() {
          paste0("Forest plot - ",
                 metaRes$metaOutput$compound.symbol[metaRes$metaOutput$id == input$plotGene],
                 ".svg")
        },
        content = function(file) {
          height <- 200 + 19 * nrow(metaRes$data[[input$plotGene]])
          svg(file, width = 1024/72, height = height/72)
          print(metaRes$plot)
          dev.off()
        },
        contentType = "image/svg+xml"
      )
      
      output$saveResult <- downloadHandler(
        filename = "metaAnalysisResult.csv",
        content = function(file) {
          toWrite <- metaRes$metaOutput[, c("compound.symbol", "estimate", "se", "pval", "fdr")]
          names(toWrite) <- c("Symbol", "Estimate", "SE", "Pvalue", "FDR")
          data.table::fwrite(toWrite, file)
        }
      )
      
    }
  )
  
}