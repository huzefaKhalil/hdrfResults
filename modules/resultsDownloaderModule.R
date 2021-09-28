

resultsDownloaderMS <- function(id, tData) {
  moduleServer(
    id,
               
    function(input, output, session) {
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
        vals$availableHdrf <-
          removeComparison(vals$availableHdrf,
                           sIds)
        
        if (!is.null(vals$selectedHdrf)) {
          sIds <- c(sIds, getIds(vals$selectedHdrf))
        }
        
        vals$selectedHdrf <-
          getComparisonById(tData$hdrf,
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
      
      # enable button to select comparison
      observe({
        shinyjs::disable("selectComparisons")
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
        vals$selectedHdrf <-
          removeComparison(vals$selectedHdrf, sIds)
        
        # We don't handles reversed comparisons here... if they are unselected, they will
        # loose their reversed status
        sIds <- gsub("^-", "", sIds)
        
        # now move these to available Ids
        if (!is.null(vals$availableHdrf))
          aIds <- c(sIds, getIds(vals$availableHdrf))
        else
          aIds <- sIds
        
        # now update the hdrfs
        vals$availableHdrf <-
          getComparisonById(tData$hdrf, aIds)
        
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
      
      observe({
        shinyjs::disable("download")
        req(vals$selectedHdrf)
        shinyjs::enable("download")
      })
      
      output$download <- downloadHandler(
        filename = "HDRFData.zip",
        content = function(file) {
          withProgress({
            incProgress(0.1, message = "Loading Data...")
            
            dr <- tempdir()
            # get selected data
            sComp <- printComparison(vals$selectedHdrf)
            
            incProgress(0.2, message = "Loading Data...")
            
            tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
            
            outDT <- lapply(names(sComp), function(x) {
              temp <- getComparisonById(vals$selectedHdrf, x)
              flattenDge(temp, conn = tempConn)
            })
            
            DBI::dbDisconnect(tempConn)
            
            incProgress(0.5, message = "Writing Data...")
            
            outFiles <- sapply(outDT, function(x) {
              fn <- paste0(dr,
                           "/",
                           x$name[1],
                           "_",
                           x$region[1],
                           "_",
                           x$comparison[1],
                           ".csv")
              x <-
                x[, c("ensembl",
                      "symbol",
                      "logFC",
                      "se",
                      "t",
                      "pvalue",
                      "fdr")]
              data.table::fwrite(x, fn)
              return(fn)
            })
            
            incProgress(0.8, message = "Compressing Data...")
            
            zip(zipfile = file,
                files = outFiles,
                flags = "-j")
            
          })
        }
      )
    })
  
}
