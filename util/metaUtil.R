# source(file.path("modules", "analyses", "hdrf", "hdrf_util.R"))
# source(file.path("modules", "analyses", "hdrf", "hdrf_ui.R"))

# this is the method which actually performs the meta-analysis. However,
# if we need the counter updated, we should put the heavy lifting in the
# server function on the module..
metaAnalysis <- function(session, sIds, tData, statistic = "hedgesG") {
  
  # 1. Check if the meta-analysis exists
  saveDir <- file.path("resources/metaRes")
  fName <- file.path(saveDir, paste0(digest::digest(sIds), ".csv"))
  
  # 2. If it exists, return the data
  if (file.exists(fName)) {
    
    return()
  }
  
  # 3. If the file doesn't exist, let's get the data
  sIds <- getComparisonById(tData$hdrf, sIds)
  theData <- flattenDge(sIds, conn = tData$conn)
  
  # first, split it by id
  theData <- split(theData, by = "id")
  
  if (statistic == "cohensD") {
    st <- "cohensD"
    vt <- "varD"
  } else {
    st <- "hedgesG"
    vt <- "varG"
  }
  
  # now apply over them.. let's make it parallel with mclapply
  totalDone <- 0
  total <- length(theData)
  output <- parallel::mcmapply(function(x, nx) {
    totalDone <<- totalDone + 1
    
    shinyWidgets::updateProgressBar(
      session,
      id = "metaProgress",
      value = totalDone / total
    )
    
    tryCatch({
      tOut <- metafor::rma(x[[st]], x[[vt]], method = "HE")
      
      return(data.frame(id = nx,
                        estimate = tOut$b[1,1],
                        se = tOut$se,
                        pval = tOut$pval,
                        zval = tOut$zval))
    }, error = function(e) return(NULL), finally = {})
    
  }, theData, names(theData),
  SIMPLIFY = FALSE,
  mc.cores = getOption("mc.cores", 4L))
  
  output <- data.table::rbindlist(output[lengths(output) != 0])
  output$fdr <- p.adjust(output$pval, method = "BH")
  
  return(output)
  
}