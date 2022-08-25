

rrhoMS <- function(id, tData) {
  
  moduleServer(
    id,
    
    function(input, output, session) {
      
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
        if (!is.null(rrhoVals$selectedHdrf)) {
          ids <- getIds(rrhoVals$selectedHdrf)
          ids <- gsub("^-", "", ids)
          hd <- removeComparison(hd, ids)
        }
        
        # set the available Hdrf
        rrhoVals$availableHdrf <- hd
        
        # shinyWidgets::updateMultiInput(session,
        #                   "comparisons",
        #                   choices = comp)
        updateSelectHdrf(session, "availableComparisons", hd)
      }
      
      # select comparisons. This method moves selected comparisons from available to selected
      sComp <- function(sIds) {
        # so, here we have to update both text boxes. These are:
        # input$availableComparisons and input$selectedComparisons
        # also have to update rrhoVals$availableHdrf and rrhoVals$selectedHdrf
        
        # select only the first two..
        if (length(sIds) > 2) {
          sIds <- sIds[1:2]
        }
        
        # set the selected hdrf and available hdrf
        rrhoVals$availableHdrf <- removeComparison(rrhoVals$availableHdrf,
                                                   sIds)
        
        if (!is.null(rrhoVals$selectedHdrf)) {
          sIds <- c(sIds, getIds(rrhoVals$selectedHdrf))
        }
        
        rrhoVals$selectedHdrf <- getComparisonById(tData$hdrf,
                                                   sIds)
        
        # now, update the selectInput boxes
        updateSelectHdrf(session, "availableComparisons", rrhoVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", rrhoVals$selectedHdrf)
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
        
        if (is.null(rrhoVals$selectedHdrf) || length(printComparison(rrhoVals$selectedHdrf)) < 2)
          shinyjs::enable("selectComparisons")
        
      })
      
      # enable button to unselect comparison
      observe({
        shinyjs::disable("unSelectComparisons")
        req(input$selectedComparisons)
        shinyjs::enable("unSelectComparisons")
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
      
      # shinyjs::onclick("selectAllComparisons", {
      #   # step one, get the ids being selected
      #   sIds <- getIds(rrhoVals$availableHdrf)
      #   sComp(sIds)
      # })
      
      shinyjs::onclick("unSelectComparisons", {
        #browser()
        # get the ids being moved back
        sIds <- input$selectedComparisons
        
        # now update the selected hdrfs
        
        rrhoVals$selectedHdrf <- removeComparison(rrhoVals$selectedHdrf, sIds)
        
        # We don't handles reversed comparisons here... if they are unselected, they will
        # loose their reversed status
        sIds <- gsub("^-", "", sIds)
        
        # now move these to available Ids
        if (!is.null(rrhoVals$availableHdrf))
          aIds <- c(sIds, getIds(rrhoVals$availableHdrf))
        else
          aIds <- sIds
        
        # now update the hdrfs
        rrhoVals$availableHdrf <- getComparisonById(tData$hdrf, aIds)
        
        # now update the selectInputs
        updateSelectHdrf(session, "availableComparisons", rrhoVals$availableHdrf)
        updateSelectHdrf(session, "selectedComparisons", rrhoVals$selectedHdrf)
      })
      
      # reverse the selected comparisons
      shinyjs::onclick("reverseComparisons", {
        rrhoVals$selectedHdrf <- reverseComparison(rrhoVals$selectedHdrf,
                                                   input$selectedComparisons)
        updateSelectHdrf(session, "selectedComparisons", rrhoVals$selectedHdrf)
      })
      
      #****************************************************
      
      # now we get to the part which runs the meta-analysis
      observe({
        req(rrhoVals$selectedHdrf)
        shinyjs::disable("runAnalysis")
        
        if (length(printComparison(rrhoVals$selectedHdrf)) == 2)
          shinyjs::enable("runAnalysis")
      })
      
      #observeEvent(input$runAnalysis, {
      shinyjs::onclick("runAnalysis", {
        shinyjs::disable("runAnalysis")
        
        tryCatch({
          withProgress({
            
            incProgress(0.1, detail = "Fetching data")
            
            sIds <- getIds(rrhoVals$selectedHdrf)
            sIds <- getComparisonById(tData$hdrf, sIds)
            
            # get selected data
            rrhoSelectedData$sData <-
              isolate({
                tempConn <- DBI::dbConnect(RSQLite::SQLite(), tData$conn)
                theData <- flattenDge(sIds, conn = tempConn)
                DBI::dbDisconnect(tempConn)
                theData
              })
            
            rrhoSelectedData$comps <- printComparison(rrhoVals$selectedHdrf)
            
            incProgress(0.2, detail = "Fetching data")
            
            inData <- list(rrhoSelectedData$sData[comparisonID == names(rrhoSelectedData$comps)[1]],
                           rrhoSelectedData$sData[comparisonID == names(rrhoSelectedData$comps)[2]])
            
            inData <- lapply(inData, function(x) {
              temp <- data.frame(x$id, -log10(x$pvalue) * sign(x$logFC),
                                 stringsAsFactors = FALSE)
              # remove duplicates
              temp[!duplicated(temp[, 2]), ]
            })
            
            commonGenes <- lapply(inData, function(x) x[, 1])
            commonGenes <- intersect(commonGenes[[1]], commonGenes[[2]])
            
            # get the common genes
            list1 <- inData[[1]][inData[[1]][, 1] %in% commonGenes, ]
            list2 <- inData[[2]][inData[[2]][, 1] %in% commonGenes, ]
            
            incProgress(0.3, detail = "Running RRHO")
            
            rrhoRes$resList <- RRHO2(list1, list2,
                                     method = input$statistic,
                                     log10.ind = TRUE)
            rrhoRes$geneList <- list(
              "Up.Up" = tData$hdrf@ids[rrhoRes$resList$geneLists$UpUp]$compound.symbol,
              "Down.Down" = tData$hdrf@ids[rrhoRes$resList$geneLists$DownDown]$compound.symbol,
              "Up.Down" = tData$hdrf@ids[rrhoRes$resList$geneLists$UpDown]$compound.symbol,
              "Down.Up" = tData$hdrf@ids[rrhoRes$resList$geneLists$DownUp]$compound.symbol
            )
            # convert to df
            maxN <- max(sapply(rrhoRes$geneList, length))
            rrhoRes$geneList <- lapply(rrhoRes$geneList, function(x) {
              length(x) <- maxN
              return(x)
            })
            rrhoRes$geneList <- do.call(cbind, rrhoRes$geneList)
            
            incProgress(0.8, detail = "Generating Plots")
            
            rast <- raster::raster(rrhoRes$resList$hypermat)
            rPoints <- data.frame(raster::rasterToPoints(rast))
            
            jet.colors <- colorRampPalette(c("#00007F", "blue",
                                             "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00",
                                             "red", "#7F0000"))
            legendLab <- ifelse(input$statistic == "fisher", "-log(odds ratio)", "-log(pvalue)")
            
            rrhoRes$enrichmentPlot <- 
              ggplot(data = rPoints) + 
              geom_tile(aes(x = 1-y, y = x, fill = layer)) + 
              scale_fill_gradientn(colors = jet.colors(101),
                                   guide = guide_colorbar(title = legendLab,
                                                          title.position = "left",
                                                          title.theme = element_text(angle = 90),
                                                          title.hjust = 0.5,
                                                          title.vjust = 0.5,
                                                          barheight = grid::unit(0.5, "npc"))) +
              coord_equal() +
              theme_bw() +
              annotate("text", x = 0.05, y = -0.05, label = "Up", size = 6) +
              annotate("text", x = 0.5, y = -0.05, label = rrhoSelectedData$comps[1], size = 6) +
              annotate("text", x = 0.95, y = -0.05, label = "Down", size = 6) +
              annotate("text", x = -0.05, y = 0.05, label = "Up", angle = 90, size = 6) +
              annotate("text", x = -0.05, y = 0.5, label = rrhoSelectedData$comps[2], angle = 90, size = 6) +
              annotate("text", x = -0.05, y = 0.95, label = "Down", angle = 90, size = 6) +
              theme(panel.grid = element_blank(),
                    panel.border = element_blank(),
                    axis.line = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank(),
                    axis.title = element_blank())
            
            incProgress(0.9, detail = "Generating Plots")
            
            vList <- list("Upregulated Genes" = rrhoRes$resList$ConcordantVenn$UpUp,
                          "Downregulated Genes" = rrhoRes$resList$ConcordantVenn$DownDown,
                          "Up Down" = rrhoRes$resList$DiscordantVenn$UpDown,
                          "Down Up" = rrhoRes$resList$DiscordantVenn$DownUp)
            
            rrhoRes$vennPlots <- Map(function(x, nx) {
              
              pTemp <- VennDiagram::draw.pairwise.venn(
                x$area1,
                x$area2,
                x$cross.area,
                category = rrhoSelectedData$comps,
                scaled = TRUE, lwd = 0,
                fill = c("cornflowerblue", "darkorchid1"),
                ind = FALSE, cat.dist = 0.03,
                cex = 1, cat.cex = 1.2, cat.pos = c(200, 160), ext.text = FALSE
              )
              
              tTemp <- grid::textGrob(nx, y=unit(0.92,"npc"), 
                                      vjust=0, gp=grid::gpar(fontsize=15))
              pTemp <- grid::gTree(children=grid::gList(pTemp, tTemp))
              
              return(pTemp)
              
            }, vList, names(vList))
            
            ## todo the venn diagrams
          }, message = "RRHO")
        }, finally = {
          shinyjs::enable("runAnalysis")
          shinyjs::enable("saveEnrichment")
          shinyjs::enable("saveGeneList")
        })
        
      })
      
      output$enrichment <- renderPlot({
        req(rrhoRes$enrichmentPlot)
        rrhoRes$enrichmentPlot
      }, height = 600)
      
      output$upUpPlot <- renderPlot({
        grid::grid.draw(req(rrhoRes$vennPlots$`Upregulated Genes`))
      }, height = 400)
      
      output$downDownPlot <- renderPlot({
        grid::grid.draw(req(rrhoRes$vennPlots$`Downregulated Genes`))
      }, height = 400)
      
      output$upDownPlot <- renderPlot({
        grid::grid.draw(req(rrhoRes$vennPlots$`Up Down`))
      }, height = 400)
      
      output$downUpPlot <- renderPlot({
        grid::grid.draw(req(rrhoRes$vennPlots$`Down Up`))
      }, height = 400)
      
      # download helpers
      output$saveEnrichment <- downloadHandler(
        filename = "Enrichment_plot.svg",
        content = function(file) {
          svg(file, width = 720/72, height = 600/72)
          print(rrhoRes$enrichmentPlot)
          dev.off()
        },
        contentType = "image/svg+xml"
      )
      
      output$saveGeneList <- downloadHandler(
        filename = "RRHO_Overlaps.csv",
        content = function(file) {
          data.table::fwrite(rrhoRes$geneList, file)
        },
        contentType = "text/csv"
      )
      
    }
  )
}
