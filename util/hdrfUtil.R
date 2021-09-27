# Includes code common to ALL HDRF modules. Might make sense to keep it together.

#library(DBI)
#library(RSQLite)
library(dgeResults) # use this a lot... need it loaded as it provides classes of the object types we are working with

loadData <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), sqlFile)
  
  hdrf <- readRDS(rdsFile)
  
  pathways <- readRDS(pathwayFile)
  
  return(list("conn" = conn, "hdrf" = hdrf, "pathways" = pathways))
}

# populates the UI, namesly the checkboxes and selectInput vals.
populateUI <- function(session, tData) {
  
  updateCheckboxGroupInput(
    session,
    inputId = "species",
    choices = tools::toTitleCase(getSpecies(tData$hdrf)),
    selected = tools::toTitleCase(getSpecies(tData$hdrf))
  )
  
  updateCheckboxGroupInput(
    session,
    inputId = "sex",
    choices = tools::toTitleCase(getSex(tData$hdrf)),
    selected = "Male"
  )
  
  updateCheckboxGroupInput(
    session,
    inputId = "model",
    choices = getModel(tData$hdrf),
    selected = getModel(tData$hdrf)
  )
  
  updateCheckboxGroupInput(
    session,
    inputId = "region",
    choices = getRegion(tData$hdrf),
    selected = c("HPC", "vHPC")
  )
  
  updateCheckboxGroupInput(
    session,
    inputId = "timepoint",
    choices = getTimepoint(tData$hdrf),
    selected = "None"
  )
  
  updateCheckboxGroupInput(
    session,
    inputId = "treatment",
    choices = getTreatment(tData$hdrf),
    selected = "None"
  )
  
  updateSelectizeInput(
    session,
    inputId = "genes",
    choices = setNames(tData$hdrf@ids$id, tData$hdrf@ids$compound.symbol),
    server = TRUE
  )
  
  updateSelectizeInput(
    session,
    inputId = "pathways",
    choices = c("", names(tData$pathways)),
    selected = NULL
    #server = TRUE
  )
}

# This method takes comparisons and produce a printable list similar to 
comparisonToList <- function(comp) {
  
  # let's split it first
  outDT <- data.table::data.table()
  outDT$id <- names(comp)
  
  # split the name and assign it to the first column
  splitRes <- data.table::tstrsplit(comp, " \\(")
  outDT$comparison <- splitRes[[1]]
  
  theNames <- splitRes[[2]]
  # remove the last bracket
  
  theNames <- gsub("\\)", "", theNames)
  
  labs <- data.table::tstrsplit(theNames, ", ")
  outDT$exp <- labs[[1]]
  outDT$region <- labs[[2]]
  outDT$treatment <- labs[[3]]
  outDT$timepoint <- labs[[4]]
  outDT$id <- names(comp)
  
  # now, split into spearate lists
  outList <- split(outDT, by = "exp")
  outList <- lapply(outList, function(x) {
    
    out <- x$id
    outNames <- paste(x$region, x$treatment, x$timepoint, sep = ", ")
    outNames <- gsub(", None", "", outNames)
    outNames <- paste0("(", outNames, ")")
    
    names(out) <- paste(x$comparison, outNames)
    return(out)
  })
  
  return(outList)
}

# method to update a selectInput box, given a box and hdrf data object
updateSelectHdrf <- function(session, boxId, hd) {
  
  if (is.null(hd)) {
    comp <- ""
    names(comp) <- ""
  } else {
    temp <- printComparison(hd,
                            name = TRUE,
                            region = TRUE,
                            treatment = TRUE,
                            timepoint = TRUE)
    comp <- comparisonToList(temp)
  }
  
  updateSelectInput(session, boxId, choice = comp)
}

# add a tooltip to checkbox choices
choicePopover <-
  function(id,
           choice,
           title,
           content,
           placement = "bottom",
           trigger = "hover",
           options = NULL) {
    options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options, content)
    options = paste0("{'", paste(
      names(options),
      options,
      sep = "': '",
      collapse = "', '"
    ), "'}")
    bsTag <- shiny::tags$script(shiny::HTML(
      paste0(
        "
        $(document).ready(function() {
        setTimeout(function() {
        $('input', $('#",
        id,
        "')).each(function(){
        if(this.getAttribute('value') == '",
        choice,
        "') {
        opts = $.extend(",
        options,
        ", {html: true});
        $(this.parentElement).popover('destroy');
        $(this.parentElement).popover(opts);
        }
        })
        }, 500)
        });
        "
      )
    ))
    htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
  }

# This sets code to have checkboxGroupInput in multiple columns
multiColTweak <- list(tags$head(tags$style(
  HTML(
    "
    .multicol2 {
    
    -webkit-column-count: 2; /* Chrome, Safari, Opera */
    -moz-column-count: 2;    /* Firefox */
    column-count: 2;
    column-gap: 15px;
    -moz-column-fill: auto;
    -column-fill: auto;
    }
    ",
    ".multicol1 {
    
    -webkit-column-count: 1; /* Chrome, Safari, Opera */
    -moz-column-count: 1;    /* Firefox */
    column-count: 1;
    -moz-column-fill: auto;
    -column-fill: auto;
    }"
  )
)))
