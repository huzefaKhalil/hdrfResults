
resultsDownloaderMS <- function(id, tData, dVals) {
  moduleServer(
    id,
    
    function(input, output, session) {

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
        if (!is.null(dVals$selectedHdrf)) {
          ids <- getIds(dVals$selectedHdrf)
          ids <- gsub("^-", "", ids)
          hd <- removeComparison(hd, ids)
        }
        
        # set the available Hdrf
        dVals$availableHdrf <- hd
        
        # shinyWidgets::updateMultiInput(session,
        #                   "comparisons",
        #                   choices = comp)
        updateSelectHdrf(session, "availableComparisons", hd)
      }
      
      # select comparisons. This method moves selected comparisons from available to selected
      sComp <- function(sIds) {
        # so, here we have to update both text boxes. These are:
        # input$availableComparisons and input$selectedComparisons
        # also have to update dVals$availableHdrf and dVals$selectedHdrf
        
        # set the selected hdrf and available hdrf
        dVals$availableHdrf <-
          removeComparison(dVals$availableHdrf,
                           sIds)
        
        if (!is.null(dVals$selectedHdrf)) {
          sIds <- c(sIds, getIds(dVals$selectedHdrf))
        }
        
        dVals$selectedHdrf <-
          getComparisonById(tData$hdrf,
                            sIds)
        
        # now, update the selectInput boxes
        updateSelectHdrf(session, "availableComparisons", dVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", dVals$selectedHdrf)
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
        req(dVals$availableHdrf)
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
        sIds <- getIds(dVals$availableHdrf)
        sComp(sIds)
      })
      
      shinyjs::onclick("unSelectComparisons", {
        # get the ids being moved back
        sIds <- input$selectedComparisons
        
        # now update the selected hdrfs
        dVals$selectedHdrf <-
          removeComparison(dVals$selectedHdrf, sIds)
        
        # We don't handles reversed comparisons here... if they are unselected, they will
        # loose their reversed status
        sIds <- gsub("^-", "", sIds)
        
        # now move these to available Ids
        if (!is.null(dVals$availableHdrf))
          aIds <- c(sIds, getIds(dVals$availableHdrf))
        else
          aIds <- sIds
        
        # now update the hdrfs
        dVals$availableHdrf <-
          getComparisonById(tData$hdrf, aIds)
        
        # now update the selectInputs
        updateSelectHdrf(session, "availableComparisons", dVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", dVals$selectedHdrf)
      })
      
      # reverse the selected comparisons
      shinyjs::onclick("reverseComparisons", {
        dVals$selectedHdrf <- reverseComparison(dVals$selectedHdrf,
                                                input$selectedComparisons)
        updateSelectHdrf(session, "selectedComparisons", dVals$selectedHdrf)
      })
      
      observe({
        shinyjs::disable("download")
        req(dVals$selectedHdrf)
        shinyjs::enable("download")
      })
      
      output$download <- downloadHandler(
        filename = "HDRFData.zip",
        content = function(file) {
          withProgress({
            incProgress(0.1, message = "Loading Data...")
            
            dr <- tempdir()
            # get selected data
            sComp <- printComparison(dVals$selectedHdrf)
            
            incProgress(0.2, message = "Loading Data...")
            
            tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
            
            outDT <- lapply(names(sComp), function(x) {
              temp <- getComparisonById(dVals$selectedHdrf, x)
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
                      "fdr",
                      "cohensD",
                      "varD",
                      "hedgesG",
                      "varG")]
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
