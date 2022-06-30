
metaAnalysisMS <- function(id, tData) {
  
  moduleServer(
    id,
    
    function(input, output, session) {
      # load the data
      
      withProgress({
        incProgress(0.1, message = "Loading...")
        
        vals <- reactiveValues(selectedHdrf = NULL,
                               availableHdrf = NULL)
        selectedData <- reactiveValues()
        metaRes <- reactiveValues()
        
        incProgress(0.4, message = "Loading...")
        
        # save directory for the meta analysis
        saveDir <- file.path("resources/metaRes")
        
        populateUI(session, tData)
        
        incProgress(0.8, message = "Loading...")
        
      })
      
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
        if (!is.null(vals$selectedHdrf)) {
          ids <- getIds(vals$selectedHdrf)
          ids <- gsub("^-", "", ids)
          hd <- removeComparison(hd, ids)
        }
        
        # set the available Hdrf
        vals$availableHdrf <- hd
        
        # shinyWidgets::updateMultiInput(session,
        #                   "comparisons",
        #                   choices = comp)
        updateSelectHdrf(session, "availableComparisons", hd)
      }
      
      # select comparisons. This method moves selected comparisons from available to selected
      sComp <- function(sIds) {
        # so, here we have to update both text boxes. These are:
        # input$availableComparisons and input$selectedComparisons
        # also have to update vals$availableHdrf and vals$selectedHdrf
        
        # set the selected hdrf and available hdrf
        vals$availableHdrf <- removeComparison(vals$availableHdrf,
                                               sIds)
        
        if (!is.null(vals$selectedHdrf)) {
          sIds <- c(sIds, getIds(vals$selectedHdrf))
        }
        
        vals$selectedHdrf <- getComparisonById(tData$hdrf,
                                               sIds)
        
        # now, update the selectInput boxes
        updateSelectHdrf(session, "availableComparisons", vals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", vals$selectedHdrf)
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
        req(vals$availableHdrf)
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
      observeEvent(input$selectComparisons, {
        # step one, get the ids being selected
        sIds <- input$availableComparisons
        sComp(sIds)
      })
      
      observeEvent(input$selectAllComparisons, {
        # step one, get the ids being selected
        sIds <- getIds(vals$availableHdrf)
        sComp(sIds)
      })
      
      observeEvent(input$unSelectComparisons, {
        # get the ids being moved back
        sIds <- input$selectedComparisons
        
        # now update the selected hdrfs
        vals$selectedHdrf <- removeComparison(vals$selectedHdrf, sIds)
        
        # We don't handles reversed comparisons here... if they are unselected, they will
        # loose their reversed status
        sIds <- gsub("^-", "", sIds)
        
        # now move these to available Ids
        if (!is.null(vals$availableHdrf))
          aIds <- c(sIds, getIds(vals$availableHdrf))
        else
          aIds <- sIds
        
        # now update the hdrfs
        vals$availableHdrf <- getComparisonById(tData$hdrf, aIds)
        
        # now update the selectInputs
        updateSelectHdrf(session, "availableComparisons", vals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", vals$selectedHdrf)
      })
      
      # reverse the selected comparisons
      observeEvent(input$reverseComparisons, {
        vals$selectedHdrf <- reverseComparison(vals$selectedHdrf,
                                               input$selectedComparisons)
        updateSelectHdrf(session, "selectedComparisons", vals$selectedHdrf)
      })
      
      #****************************************************
      
      # now we get to the part which runs the meta-analysis
      observe({
        shinyjs::disable("runAnalysis")
        req(vals$selectedHdrf)
        shinyjs::enable("runAnalysis")
      })
      
      # run the meta analysis!
      #observeEvent(input$runAnalysis, {
      shinyjs::onclick("runAnalysis", {
        shinyjs::disable("runAnalysis")
        
        sIds <- getIds(vals$selectedHdrf)
        nSelected <- length(sIds)
        
        # 1. Check if the meta-analysis exists
        fName <- file.path(saveDir, paste0(digest::digest(sIds), ".csv"))
        
        # 2. If it exists, return the data
        if (file.exists(fName)) {
          
        } else {
          
          shinyWidgets::updateProgressBar(
            session,
            id = "metaProgress",
            value = 0,
            title = "Loading Data"
          )
          
          # 3. If the file doesn't exist, let's get the data
          sIds <- getComparisonById(tData$hdrf, sIds)
          
          tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
          theData <- flattenDge(sIds, conn = tempConn)
          DBI::dbDisconnect(tempConn)

          # first, split it by id
          theData <- split(theData, by = "id")
          
          if (input$statistic == "cohensD") {
            st <- "cohensD"
            vt <- "varD"
          } else {
            st <- "hedgesG"
            vt <- "varG"
          }
          
          # now apply over them.. let's make it parallel with mclapply
          totalDone <- 0
          total <- length(theData)
          
          # make the cluster
          #pboptions(type = "none")
          metaRes$data <- theData
          
          # output <- parallel::mcmapply(function(x, nx) {
          metaOutput <- mapply(function(x, nx) {
            totalDone <<- totalDone + 1
            
            shinyWidgets::updateProgressBar(
              session,
              id = "metaProgress",
              value = ((totalDone / total) * 100),
              title = "Running Meta-Analysis"
            )
            
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
          #mc.cores = getOption("mc.cores", 4L))
          
          shinyWidgets::updateProgressBar(
            session,
            id = "metaProgress",
            value = 100,
            title = "Finishing up"
          )
          
          metaOutput <- data.table::rbindlist(metaOutput[lengths(metaOutput) != 0])
          metaOutput$fdr <- p.adjust(metaOutput$pval, method = "BH")
          metaOutput$logPval <- -log10(metaOutput$pval)
          metaOutput <- merge(metaOutput,
                              tData$hdrf@ids[, c("id", "compound.symbol")],
                              all.x = TRUE, all.y = FALSE, by = "id")
          metaOutput$hoverText <- paste0("Symbol: ", metaOutput$compound.symbol, "<br>",
                                         "Estimate: ", format(metaOutput$estimate, digits = 4), "<br>",
                                         "Adjusted P-value:", format(metaOutput$fdr, digits = 4))
          metaOutput <- metaOutput[order(metaOutput$pval), ]
          
          metaRes$metaOutput <- metaOutput
          
          shinyWidgets::updateProgressBar(
            session,
            id = "metaProgress",
            value = 100,
            title = "All Done!"
          )
          
          shinyjs::enable("saveResult")
          shinyjs::enable("runAnalysis")
        }
        
      })
      
      observe({
        req(metaRes$metaOutput)
        
        updateSelectizeInput(
          session,
          inputId = "plotGene",
          choices = setNames(metaRes$metaOutput$id, metaRes$metaOutput$compound.symbol),
          selected = metaRes$metaOutput$id[1],
          server = TRUE
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
          server = TRUE
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