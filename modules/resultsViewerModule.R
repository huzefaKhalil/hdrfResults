

resultsViewerMS <- function(id, tData, resVals, resSelectedData) {
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      # create vals to be used with future ipc
      sData <- reactiveVal()
      pvalData <- reactiveVal()
      estimateData <- reactiveVal()
      forestPlots <- reactiveVal()
      heatmapr <- reactiveVal()

      # create the shinyQueue for the ipc
      queue <- shinyQueue()
      queue$consumer$start(250)   # check for stuff every 250ms
      
      withProgress({
        incProgress(0.2, message = "Loading...")
        
        if (is.null(input$genes)) {
          updateSelectizeInput(
            session,
            inputId = "genes",
            choices = setNames(tData$hdrf@ids$id, tData$hdrf@ids$compound.symbol),
            server = TRUE
          )
        }
        
        incProgress(0.6, message = "Loading...")
        
        if (input$pathways == "") {
          updateSelectizeInput(
            session,
            inputId = "pathways",
            choices = c("", names(tData$pathways)),
            selected = NULL,
            server = FALSE
          )
        }
        
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
        if (!is.null(resVals$selectedHdrf)) {
          ids <- getIds(resVals$selectedHdrf)
          ids <- gsub("^-", "", ids)
          hd <- removeComparison(hd, ids)
        }
        
        # set the available Hdrf
        resVals$availableHdrf <- hd
        
        # shinyWidgets::updateMultiInput(session,
        #                   "comparisons",
        #                   choices = comp)
        updateSelectHdrf(session, "availableComparisons", hd)
      }
      
      # select comparisons. This method moves selected comparisons from available to selected
      sComp <- function(sIds) {
        # so, here we have to update both text boxes. These are:
        # input$availableComparisons and input$selectedComparisons
        # also have to update resVals$availableHdrf and resVals$selectedHdrf
        
        # set the selected hdrf and available hdrf
        resVals$availableHdrf <-
          removeComparison(resVals$availableHdrf,
                           sIds)
        
        if (!is.null(resVals$selectedHdrf)) {
          sIds <- c(sIds, getIds(resVals$selectedHdrf))
        }
        
        resVals$selectedHdrf <-
          getComparisonById(tData$hdrf,
                            sIds)
        
        # now, update the selectInput boxes
        updateSelectHdrf(session, "availableComparisons", resVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", resVals$selectedHdrf)
      }
      
      # these check for changes to the selection and updae available comparisons
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
        req(resVals$availableHdrf)
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
        sIds <- getIds(resVals$availableHdrf)
        sComp(sIds)
      })
      
      shinyjs::onclick("unSelectComparisons", {
        # get the ids being moved back
        sIds <- input$selectedComparisons
        
        # now update the selected hdrfs
        resVals$selectedHdrf <-
          removeComparison(resVals$selectedHdrf, sIds)
        
        # We don't handles reversed comparisons here... if they are unselected, they will
        # loose their reversed status
        sIds <- gsub("^-", "", sIds)
        
        # now move these to available Ids
        if (!is.null(resVals$availableHdrf))
          aIds <- c(sIds, getIds(resVals$availableHdrf))
        else
          aIds <- sIds
        
        # now update the hdrfs
        resVals$availableHdrf <-
          getComparisonById(tData$hdrf, aIds)
        
        # now update the selectInputs
        updateSelectHdrf(session, "availableComparisons", resVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", resVals$selectedHdrf)
      })
      
      # reverse the selected comparisons
      shinyjs::onclick("reverseComparisons", {
        resVals$selectedHdrf <- reverseComparison(resVals$selectedHdrf,
                                                  input$selectedComparisons)
        updateSelectHdrf(session, "selectedComparisons", resVals$selectedHdrf)
      })
      
      #****************************************************
      
      observe({
        shinyjs::disable("uploadGenes")
        req(input$upGenes)
        shinyjs::enable("uploadGenes")
      })
      
      observe({
        shinyjs::disable("runAnalysis")
        req(resVals$selectedHdrf, input$genes)
        shinyjs::enable("runAnalysis")
      })
      
      observe({
        shinyjs::disable("downloadData")
        req(resVals$selectedHdrf, input$genes)
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
      shinyjs::onclick("runAnalysis", {
        #shinyjs::reset("runAnalysis")
        shinyjs::disable("runAnalysis")
        shinyjs::disable("downloadData")
        
        sData(NULL)
        pvalData(NULL)
        estimateData(NULL)
        forestPlots(NULL)
        heatmapr(NULL)
        
        progress <- AsyncProgress$new(session, value = 0.01, message = "HDRF Results", detail = "Fetching Data")

        # get the selected ids
        sIds <- getIds(resVals$selectedHdrf)
        sHdrf <- resVals$selectedHdrf
        
        # make sure there are no duplicated compound symbols here
        sGenes <- input$genes[!duplicated(tData$hdrf@ids[input$genes]$compound.symbol)]

        future({
          progress$set(value = 0.1, message = "HDRF Results", detail = "Fetching data")
          
          # get selected data from the db
          tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
          sData <- flattenDge(sHdrf, geneId = sGenes, conn = tempConn)
          #DBI::dbDisconnect(tempConn)
          
          # fire off theData
          queue$producer$fireAssignReactive("sData", sData)
          
          progress$set(value = 0.2, message = "HDRF Results", detail = "Fetching data")
          
          # now get the pval data and estimate data. This is necessary for the forest plots on the side
          
          #tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
          theData <- getColumnData(
            tData$hdrf,
            column = "pvalue",
            comparisonID = sIds,
            conn = tempConn
          )
          #DBI::dbDisconnect(tempConn)
          
          progress$set(value = 0.4, message = "HDRF Results", detail = "Fetching data")
          
          queue$producer$fireAssignReactive("pvalData", theData)
          
          theData <- getColumnData(
            tData$hdrf,
            column = "logFC",
            comparisonID = sIds,
            conn = tempConn
          )
          
          queue$producer$fireAssignReactive("estimateData", theData)
          
          progress$set(value = 0.6, message = "HDRF Results", detail = "Generating Plots")
          
          # close the DB connection
          DBI::dbDisconnect(tempConn)
          
          # now the plots
          forestPlots <- smoothForest(
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
            estimate = "hedgesG",
            fontSize = 3.5,
            ids = tData$hdrf@ids
          )
          
          queue$producer$fireAssignReactive("forestPlots", forestPlots)
          
          progress$set(value = 0.8, message = "HDRF Results", detail = "Generating Plots")
          
          heatmapr <- heatmap.3(sData, ids = tData$hdrf@ids)
          
          progress$close()
          
          heatmapr
          
        }, seed = TRUE) %...>% heatmapr
        
        NULL
      })
      
      # now an observe to make sure all the stuff is done and to populate the graphs and all
      observe({
        req(sData(), pvalData(), estimateData(), forestPlots(), heatmapr())
        
        resSelectedData$sData <- sData()
        resSelectedData$pvalData <- pvalData()
        resSelectedData$estimateData <- estimateData()
        resSelectedData$forestPlots <- forestPlots()
        resSelectedData$heatmapr <- heatmapr()
        
        # set the gene to plot
        updateSelectizeInput(
          session,
          inputId = "plotGene",
          choices = resSelectedData$heatmapr$matrix$rows,
          selected = resSelectedData$heatmapr$matrix$rows[1],
          server = FALSE
        )
        
        #enable the buttons
        shinyjs::enable("runAnalysis")
        shinyjs::enable("downloadData")
        
        shinyjs::enable("saveCurrent")
        shinyjs::enable("saveAll")
      })
      
      # to download the data
      output$downloadData <- downloadHandler(
        filename = "hdrfDataDownload.csv",
        content = function(file) {
          
          tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
          tData <- flattenDge(resVals$selectedHdrf, geneId = input$genes, conn = tempConn)
          DBI::dbDisconnect(tempConn)
          
          data.table::fwrite(tData[,-c("id", "comparisonID")], file)
        }
      )
      
      # render the heatmap
      output$heatmap <- renderPlotly({
        req(resSelectedData$heatmapr)
        
        heatmaply::heatmaply(
          resSelectedData$heatmapr,
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
        gene <- resSelectedData$heatmapr$matrix$rows[clickedPoint()$y]
        
        updateSelectizeInput(
          session,
          inputId = "plotGene",
          choices = resSelectedData$heatmapr$matrix$rows,
          selected = resSelectedData$heatmapr$matrix$rows[clickedPoint()$y],
          server = FALSE
        )
      })
      
      # when a point is clicked.. print the experiment info
      output$colInfo <- renderUI({
        req(resSelectedData$sData, clickedPoint())
        
        comparison <-
          resSelectedData$heatmapr$matrix$cols[clickedPoint()$x]
        rowsToGet <-
          grepl(comparison, resSelectedData$sData$comparisonID)
        
        info <- resSelectedData$sData[rowsToGet,]
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
            resSelectedData$heatmapr$matrix$rows[clickedPoint()$y],
            "<br>"
          )
        )
      })
      
      # print the volcano plot
      output$volcano <- renderPlotly({
        req(resSelectedData$pvalData,
            resSelectedData$estimateData,
            clickedPoint())
        comparison <-
          resSelectedData$heatmapr$matrix$cols[clickedPoint()$x]
        gene <-
          resSelectedData$heatmapr$matrix$rows[clickedPoint()$y]
        
        # let's get the data
        pv <- resSelectedData$pvalData
        lfc <- resSelectedData$estimateData
        
        plotData <- data.frame(
          symbol = pv[["symbol"]],
          pval = -log(pv[[comparison]]),
          estimate = lfc[[comparison]]
        )
        plotData <- na.omit(plotData)
        plotData$color <-
          ifelse(
            !(plotData$symbol %in% resSelectedData$heatmapr$matrix$rows),
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

      # change the plot when the plot counter moves
      observeEvent(input$plotGene, {
        output$forest <-renderPlot({
          req(resSelectedData$forestPlots)
          
          resSelectedData$forestPlots[[input$plotGene]]
        },
        height = function() {
          req(resSelectedData$heatmapr)
          return(200 + 19 * length(resSelectedData$heatmapr$matrix$cols))
        }
        )
      })
      
      # code to save the plots
      output$saveCurrent <- downloadHandler(
        filename = function() {
          paste0("Forest plot - ", input$plotGene, ".svg")
        },
        content = function(file) {
          height <- 200 + 19 * length(resSelectedData$heatmapr$matrix$cols)
          svg(file, width = 1024/72, height = height/72)
          print(resSelectedData$forestPlots[[input$plotGene]])
          dev.off()
        },
        contentType = "image/svg+xml"
      )
      
      output$saveAll <- downloadHandler(
        filename = "ForestPlots.zip",
        content = function(file) {
          dr <- tempdir()
          height <-
            200 + 19 * length(resSelectedData$heatmapr$matrix$cols)
          fns <- Map(function(p, np) {
            fn <- paste0(dr, "/", np, ".svg")
            svg(fn, width = 1024/72, height = height/72)
            print(p)
            dev.off()
            return(fn)
          },
          resSelectedData$forestPlots,
          names(resSelectedData$forestPlots))
          
          zip(zipfile = file,
              files = fns,
              flags = "-j")
        },
        contentType = "application/zip"
      )
    })
}
