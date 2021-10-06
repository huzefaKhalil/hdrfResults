

resultsViewerMS <- function(id, tData) {
  moduleServer(
    
    id,
               
    function(input, output, session) {
                 # load the data
      withProgress({
        incProgress(0.1, message = "Loading...")
        #tData <- loadData()
        
        vals <- reactiveValues(selectedHdrf = NULL,
                               availableHdrf = NULL)
        selectedData <- reactiveValues()
        
        populateUI(session, tData)
        
        incProgress(0.3, message = "Loading...")
        
        Sys.sleep(0.2)
        incProgress(0.7, message = "Loading...")
        
        Sys.sleep(0.2)
        incProgress(1, message = "Loading...")
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
      
      # these check for changes to the selection and updae available comparisons
      # observeEvent(input$treatment, updateSelection())
      # observeEvent(input$timepoint, updateSelection())
      # observeEvent(input$region, updateSelection())
      # observeEvent(input$sex, updateSelection())
      # observeEvent(input$model, updateSelection())
      # observeEvent(input$species, updateSelection())
      observe({
        req(input$treatment,
            input$timepoint,
            input$region,
            input$sex,
            input$model)
        updateSelection()
      })
      
      # initiates the genes when pathways are selected
      observeEvent(input$pathways, {
        updateSelectizeInput(
          session,
          inputId = "genes",
          choices = setNames(tData$hdrf@ids$id, tData$hdrf@ids$compound.symbol),
          selected = tData$pathways[[input$pathways]]$id,
          server = TRUE
        )
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
      
      #****************************************************
      
      observe({
        shinyjs::disable("uploadGenes")
        req(input$upGenes)
        shinyjs::enable("uploadGenes")
      })
      
      observe({
        shinyjs::disable("runAnalysis")
        req(vals$selectedHdrf, input$genes)
        shinyjs::enable("runAnalysis")
      })
      
      observe({
        shinyjs::disable("downloadData")
        req(vals$selectedHdrf, input$genes)
        shinyjs::enable("downloadData")
      })
      
      # get uploaded genes
      #### IMPORTANT!!!
      #### THIS WILL CHANGE IF HUMAN (OR ANYOTHER SPECIES) DATA IS ADDED
      observeEvent(input$uploadGenes, {
        genes <- strsplit(input$upGenes, "\n")[[1]]
        ids <- tData$hdrf@ids
        
        # let's get the rows we need
        if (grepl("^ENSM", genes[1]))
          colToCheck <- "mouse.ensembl"
        else if (grepl("^ENSR", genes[1]))
          colToCheck <- "rat.ensembl"
        else
          colToCheck <- "compound.symbol"
        
        if (length(genes) > maxGenes)
          genes <- genes[1:maxGenes]
        
        rowsToGet <-
          pmatch(tolower(genes), tolower(ids[[colToCheck]]))
        
        # get the ones which were not recognized
        naGenes <- sum(is.na(rowsToGet))
        if (naGenes > 0)
          shinyjs::alert(paste(
            naGenes,
            "genes not found.",
            (length(rowsToGet) - naGenes),
            "were added."
          ))
        
        rowsToGet <- na.omit(rowsToGet)
        
        if (length(rowsToGet) > 0) {
          updateSelectizeInput(
            session,
            inputId = "genes",
            choices = setNames(tData$hdrf@ids$id, tData$hdrf@ids$compound.symbol),
            selected = tData$hdrf@ids$id[rowsToGet],
            server = TRUE
          )
        }
      })
      
      #############
      # When the user presses the "View Results" Button, gathering all the actions here so as to get the progress bar
      #############
      observeEvent(input$runAnalysis, {
        shinyjs::disable("runAnalysis")
        shinyjs::disable("downloadData")
        
        tryCatch({
          withProgress({
            incProgress(0.1, detail = "Fetching data")
            
            sIds <- getIds(vals$selectedHdrf)
            
            # make sure there are no duplicated compound symbols here
            sGenes <-
              input$genes[!duplicated(tData$hdrf@ids[input$genes]$compound.symbol)]
            
            # get selected data
            selectedData$sData <-
              isolate({
                
                tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
                theData <- flattenDge(vals$selectedHdrf, geneId = sGenes, conn = tempConn)
                DBI::dbDisconnect(tempConn)
                
                theData
              })
            
            incProgress(0.2, detail = "Fetching data")
            
            # get the columns for all comparisons
            selectedData$pvalData <-
              isolate({
                
                tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)

                theData <- getColumnData(
                  tData$hdrf,
                  column = "pvalue",
                  comparisonID = sIds,
                  conn = tempConn
                )
                
                DBI::dbDisconnect(tempConn)
                
                theData
              })
            
            incProgress(0.3, detail = "Fetching data")
            
            selectedData$estimateData <-
              isolate({
                
                tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
                
                theData <- getColumnData(
                  tData$hdrf,
                  column = "logFC",
                  comparisonID = sIds,
                  conn = tempConn
                )
                
                DBI::dbDisconnect(tempConn)
                
                theData
              })
            
            incProgress(0.6, detail = "Plotting data")
            
            # make the forest plots
            selectedData$forestPlots <- isolate(
              smoothForest(
                selectedData$sData,
                metaStatistic = TRUE,
                includeModel = TRUE,
                includeName = TRUE,
                includeRegion = TRUE,
                includeEstimate = TRUE,
                includeTreatment = FALSE,
                includeSex = FALSE,
                includePval = TRUE,
                orderBy = "estimate",
                estimate = "hedgesG",
                fontSize = 3.5,
                ids = tData$hdrf@ids
              )
            )
            
            incProgress(0.8, detail = "Plottting data")
            
            selectedData$heatmapr <-
              isolate(heatmap.3(selectedData$sData, ids = tData$hdrf@ids))
            
            # set the gene to plot
            updateSelectizeInput(
              session,
              inputId = "plotGene",
              choices = selectedData$heatmapr$matrix$rows,
              selected = selectedData$heatmapr$matrix$rows[1],
              server = TRUE
            )
            
          }, message = "HDRF Results")
        }, finally = {
          shinyjs::enable("runAnalysis")
          shinyjs::enable("downloadData")
          
          shinyjs::enable("saveCurrent")
          shinyjs::enable("saveAll")
        })
      })
      
      # to download the data
      output$downloadData <- downloadHandler(
        filename = "hdrfDataDownload.csv",
        content = function(file) {

          tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
          tData <- flattenDge(vals$selectedHdrf, geneId = input$genes, conn = tempConn)
          DBI::dbDisconnect(tempConn)

          data.table::fwrite(tData[,-c("id", "comparisonID")], file)
        }
      )
      
      # render the heatmap
      output$heatmap <- renderPlotly({
        req(selectedData$heatmapr)
        
        heatmaply::heatmaply(
          selectedData$heatmapr,
          plot_method = "plotly",
          showticklabels = c(FALSE, TRUE),
          margins = c(0, 50, NA, 0)
        ) %>%
          event_register("plotly_click")
      })
      
      # code for capturing the click
      clickedPoint <- reactive({
        event_data(event = "plotly_click", source = "A")
      })
      
      # this checks for a click on the heatmap and changes the forest plot to that gene
      observeEvent(clickedPoint(), {
        gene <- selectedData$heatmapr$matrix$rows[clickedPoint()$y]
        
        updateSelectizeInput(
          session,
          inputId = "plotGene",
          choices = selectedData$heatmapr$matrix$rows,
          selected = selectedData$heatmapr$matrix$rows[clickedPoint()$y],
          server = TRUE
        )
      })
      
      # when a point is clicked.. print the experiment info
      output$colInfo <- renderUI({
        req(selectedData$sData, clickedPoint())
        
        comparison <-
          selectedData$heatmapr$matrix$cols[clickedPoint()$x]
        rowsToGet <-
          grepl(comparison, selectedData$sData$comparisonID)
        
        info <- selectedData$sData[rowsToGet,]
        HTML(
          paste0(
            "Experiment: ",
            info$name[1],
            "<br>",
            "Model: ",
            info$model[1],
            " - ",
            info$species[1],
            " - ",
            info$region[1],
            "<br>",
            "Comparison: ",
            info$comparison[1],
            "<br>",
            "Treatment: ",
            info$treatment[1],
            "<br>",
            "Highlighted Gene: ",
            selectedData$heatmapr$matrix$rows[clickedPoint()$y],
            "<br>"
          )
        )
      })
      
      # print the volcano plot
      output$volcano <- renderPlotly({
        req(selectedData$pvalData,
            selectedData$estimateData,
            clickedPoint())
        comparison <-
          selectedData$heatmapr$matrix$cols[clickedPoint()$x]
        gene <-
          selectedData$heatmapr$matrix$rows[clickedPoint()$y]
        
        # let's get the data
        pv <- selectedData$pvalData
        lfc <- selectedData$estimateData
        
        plotData <- data.frame(
          symbol = pv[["symbol"]],
          pval = -log(pv[[comparison]]),
          estimate = lfc[[comparison]]
        )
        plotData <- na.omit(plotData)
        plotData$color <-
          ifelse(
            !(plotData$symbol %in% selectedData$heatmapr$matrix$rows),
            "black",
            ifelse(gene != plotData$symbol, "green", "red")
          )
        
        plotly::plot_ly(
          data = plotData,
          type = "scattergl",
          x = ~ estimate,
          y = ~ pval,
          #color = ifelse(grepl(gene, plotData$symbol), "selected", "other"),
          color = ~ color,
          colors = c("black", "green", "red"),
          size = ifelse(grepl(gene, plotData$symbol), 6, 2),
          sizes = c(10, 50),
          showlegend = FALSE,
          hoverinfo = "text",
          text = plotData$symbol,
          source = "volcano",
          mode = "markers",
          marker = list(opacity = 0.8)
        ) %>%
          layout(
            xaxis = list(title = "Log Fold-Change"),
            yaxis = list(title = "-Log(p-value)")
          )
      })
      
      # forestPlots <- metaReactive2({
      #   req(sData())
      #   # lets activate the previous / next / save buttons
      #   shinyjs::enable("saveCurrent")
      #   shinyjs::enable("saveAll")
      #
      #   metaExpr(
      #     smoothForest(
      #       ..(sData()),
      #       includeModel = TRUE,
      #       includeName = TRUE,
      #       includeRegion = TRUE,
      #       includeEstimate = TRUE,
      #       includeTreatment = FALSE,
      #       includeSex = FALSE,
      #       includePval = TRUE,
      #       orderBy = "estimate",
      #       estimate = "hedgesG",
      #       ids = tData$hdrf@ids
      #     )
      #   )
      #
      # })
      
      
      # change the plot when the plot counter moves
      observeEvent(input$plotGene, {
        output$forest <-renderPlot({
          req(selectedData$forestPlots)
          
          selectedData$forestPlots[[input$plotGene]]
        },
        height = function() {
          req(selectedData$heatmapr)
          return(200 + 19 * length(selectedData$heatmapr$matrix$cols))
        }
        )
      })
      
      # code to save the plots
      output$saveCurrent <- downloadHandler(
        filename = function() {
          paste0("Forest plot - ", input$plotGene, ".svg")
        },
        content = function(file) {
          height <- 200 + 19 * length(selectedData$heatmapr$matrix$cols)
          svg(file, width = 1024/72, height = height/72)
          print(selectedData$forestPlots[[input$plotGene]])
          dev.off()
        },
        contentType = "image/svg+xml"
      )
      
      output$saveAll <- downloadHandler(
        filename = "ForestPlots.zip",
        content = function(file) {
          dr <- tempdir()
          height <-
            200 + 19 * length(selectedData$heatmapr$matrix$cols)
          fns <- Map(function(p, np) {
            fn <- paste0(dr, "/", np, ".svg")
            svg(fn, width = 1024/72, height = height/72)
            print(selectedData$forestPlots[[input$plotGene]])
            dev.off()
            return(fn)
          },
          selectedData$forestPlots,
          names(selectedData$forestPlots))
          
          zip(zipfile = file,
              files = fns,
              flags = "-j")
        },
        contentType = "application/zip"
      )
      
      
      
      # observe({
      #   shinyjs::disable("downloadReport")
      #   req(sData())
      #   shinyjs::enable("downloadReport")
      # })
      
      # heatmapCode <- reactive({
      #   req(sData())
      #   ec <- newExpansionContext()
      #   # substitute reading from a file for the actual logic on server
      #   ec$substituteMetaReactive(hdrf, function() {
      #     metaExpr(readr::read_rds("hdrfData.rds"))
      #   })
      #
      #   # expand code for the plot
      #   expandChain(output$heatmap(),
      #               .expansionContext = ec)
      # })
      #
      # output$heatmapCode <- renderPrint({
      #   heatmapCode()
      # })
      
      # forestPlotCode <- reactive({
      #   req(forestPlots())
      #   ec <- newExpansionContext()
      #   # substitute reading from a file for the actual logic on server
      #   ec$substituteMetaReactive(hdrf, function() {
      #     metaExpr(readr::read_rds("hdrfData.rds"))
      #   })
      #
      #   # expand code for the plot
      #   expandChain(output$forest(),
      #               .expansionContext = ec)
      # })
      
      # output$forestPlotCode <- renderPrint({
      #   forestPlotCode()
      # })
      #
      #   output$downloadReport <- downloadHandler(
      #     "results.zip",
      #     content = function(zipFilePath) {
      #       files2Include <- c(
      #         bundleAnalysisUtilScript("hdrf", "results"), # bundle utility R script file
      #         file.path(
      #           MBNI_SHINY_RESOURCES_DIR,
      #           "hdrf",
      #           "results",
      #           "hdrfData.rds"
      #         )
      #       )
      #
      #       buildRmdBundle(
      #         report_template = getAnalysisReportTemplatePath("hdrf", "results"),
      #         output_zip_path = zipFilePath,
      #         vars = list(
      #           heatmap_code = heatmapCode(),
      #           forest_plot_code = forestPlotCode()
      #         ),
      #         include_files = files2Include,
      #         render_args = list(output_format = "all")
      #       )
      #     }
      #   )
    })
}
