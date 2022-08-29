
#########################################################
# UI code for HDRF
#########################################################

# These set up the initial check boxes
speciesControl <- function(id, tData) {
  ns <- NS(id)
  list(
    h4("Species"),
    tags$div(
      align = "left",
      class = 'multicol1',
      checkboxGroupInput(
        inputId = ns("species"),
        label = NULL,
        choices = tools::toTitleCase(getSpecies(tData$hdrf)),
        selected = tools::toTitleCase(getSpecies(tData$hdrf)),
        inline = FALSE
      )
    )
  )
}

sexControl <- function(id, tData) {
  ns <- NS(id)
  list(
    h4("Sex"),
    tags$div(
      align = "left",
      class = 'multicol1',
      checkboxGroupInput(
        inputId = ns("sex"),
        label = NULL,
        choices = tools::toTitleCase(getSex(tData$hdrf)),
        selected = "Male",
        inline = FALSE
      )
    )
  )
}


modelsControl <- function(id, tData) {
  ns <- NS(id)
  list(
    h4("Models"),
    tags$div(
      align = "left",
      class = 'multicol1',
      checkboxGroupInput(
        inputId = ns("model"),
        label = NULL,
        choices = getModel(tData$hdrf),
        selected = getModel(tData$hdrf),
        inline = FALSE
      )
    )
    # Cannot use this because we are updating the choices server side...
    # ,
    # choicePopover(
    #   id = ns("model"),
    #   choice = "GRov",
    #   title = "GRov",
    #   content = paste("Mice with Glucocorticoid Receptor Overexpression.",
    #                   "Includes mice with Life-time GRov (LTGRov) and",
    #                   "Early-life GRov (ELGRov).")
    # ),
    # choicePopover(
    #   id = ns("model"),
    #   choice = "bHR-bLR",
    #   title = "bHR-bLR",
    #   content = paste("Selectively bred rat line, bred for low (bLR) and high (bHR) locomoter activity.",
    #                   "Experiments also includes comparisons between bHR, bLR and outbreds (bIR).")
    # ),
    # choicePopover(
    #   id = ns("model"),
    #   choice = "FSL",
    #   title = "FSL",
    #   content = "Selectively bred rat line, Flinder\\'s Sensitive (FSL) and Flinder\\'s Resistant (FRL)."
    # ),
    # choicePopover(
    #   id = ns("model"),
    #   choice = "Cort",
    #   title = "Cort",
    #   content = "Mice with CORT stress treatment."
    # ),
    # choicePopover(
    #   id = ns("model"),
    #   choice = "CSDS",
    #   title = "CSDS",
    #   content = paste("Mice with Chronic Social Defeat Stress. In most experiments, these are",
    #                   "categorized as either Susceptible or Reslient and then compared with Controls.")
    # ),
    # choicePopover(
    #   id = ns("model"),
    #   choice = "Enrichment",
    #   title = "Environmental Enrichment",
    #   content = paste("Mice with either environmentally enrichmed housing or standard housing.")
    # ),
    # choicePopover(
    #   id = ns("model"),
    #   choice = "BDNF",
    #   title = "BDNF",
    #   content = "BDNF Val66Met"
    # )
  )
}

regionsControl <- function(id, tData) {
  ns <- NS(id)
  list(
    h4("Brain Region"),
    tags$div(
      align = 'left',
      class = 'multicol2',
      checkboxGroupInput(
        inputId = ns("region"),
        label = NULL,
        inline = FALSE,
        choices = getRegion(tData$hdrf),
        selected = getRegion(tData$hdrf)
      )
    )
  )
}

timepointControl <- function(id, tData) {
  ns <- NS(id)
  list(
    h4("Timepoints"),
    tags$div(
      align = "left",
      class = 'multicol1',
      checkboxGroupInput(
        inputId = ns("timepoint"),
        label = NULL,
        choices = getTimepoint(tData$hdrf),
        selected = "None",
        inline = FALSE
      )
    )
  )
}

treatmentControl <- function(id, tData) {
  ns <- NS(id)
  list(
    h4("Drug Treatment"),
    tags$div(
      align = 'left',
      class = 'multicol2',
      checkboxGroupInput(
        inputId = ns("treatment"),
        label = NULL,
        inline = FALSE,
        choices = getTreatment(tData$hdrf),
        selected = "None"
      )
    )
  )
}

############################################
# Setup actual rows of input
############################################

# This generates the first row with all the check boxes
selectDataRow <- function(id, tData) {
  ns <- NS(id)   # don't need it here...
  tagList(
    #column(width = 1, speciesControl),
    
    #column(width = 1, sexControl),
    
    #column(width = 1, speciesSexHousingControl),
    column(width = 2, speciesControl(id, tData), sexControl(id, tData)),
    
    #column(width = 3, experimentControl),
    
    column(width = 2, modelsControl(id, tData)),
    
    column(width = 2, regionsControl(id, tData)),
    
    column(width = 3, treatmentControl(id, tData)),
    
    column(width = 3, timepointControl(id, tData))
  )
}

# This generates the second row to select the comparisons
selectComparisonRow <- function(id, selectAll = TRUE, moduleText = "results") {
  ns <- NS(id)
  
  mText <- switch(moduleText,
                  meta = paste(
                    "Press the Reverse Comparison button to reverse the selected comparison.",
                    "Run the meta-analysis after selecting the comparisons."
                  ),
                  rrho = paste(
                    "Press the Reverse Comparison button to reverse the selected comparison.",
                    "You can select a maximum of two comparisons."
                  ),
                  paste(
                    "Press the Reverse Comparison button to reverse the selected comparison.",
                    "After selecting the comparisons, please select or upload the needed genes (max 100) or select a pathway."
                  )
  )
  
  if (selectAll) {
    button <- shinyjs::disabled(
      actionButton(
        inputId = ns("selectAllComparisons"),
        label = "Select All Comparisons"
      )
    )
  } else {
    button <- NULL
  }
  tagList(
    column(
      width = 4,
      selectInput(
        inputId = ns("availableComparisons"),
        label = h4("Available Comparisons"),
        multiple = TRUE,
        size = 10,
        selectize = FALSE,
        choices = NULL
      ),
      br(),
      button
    ),
    column(
      width = 1,
      
      br(), br(), br(), br(), br(),
      shinyjs::disabled(
        actionButton(inputId = ns("selectComparisons"),
                     label = NULL,
                     icon = icon("arrow-right"))
      ),
      br(), br(),
      shinyjs::disabled(
        actionButton(inputId = ns("unSelectComparisons"),
                     label = NULL,
                     icon = icon("arrow-left"))
      )
    ),
    column(
      width = 4,
      selectInput(
        inputId = ns("selectedComparisons"),
        label = h4("Selected Comparisons"),
        multiple = TRUE,
        size = 10,
        selectize = FALSE,
        choices = NULL
      ),
      br(),
      shinyjs::disabled(
        actionButton(
          inputId = ns("reverseComparisons"),
          label = "Reverse Comparisons"
        )
      )
    ),
    column(
      width = 3,
      offset = 0,
      list(
        br(),
        p("To view results, select desired comparisons on the left."),
        p(mText)
      )
    )
  )
}

# this generates the gene selector boxes
selectGenesRow <- function(id, tData, maxGenes) {
  ns <- NS(id)
  tagList(
    column(
      width = 3,
      selectizeInput(
        inputId = ns("genes"),
        label = h3("Choose Genes"),
        multiple = TRUE,
        choices = NULL,
        selected = NULL,
        options = list(maxItems = maxGenes)
      )
    ),
    column(width = 1, h3("or")),
    column(
      width = 3,
      textAreaInput(
        inputId = ns("upGenes"),
        label = h3("Upload Genes"),
        value = "",
        rows = 3,
        placeholder = "Paste genes (either symbols or Ensembl IDs), one per line."
      ),
      shinyjs::disabled(
        actionButton(inputId = ns("uploadGenes"),
                     label = "Upload Genes")
      )
    ),
    column(width = 1, h3("or")),
    column(
      width = 3,
      selectizeInput(
        inputId = ns("pathways"),
        label = h3("Choose Pathway"),
        multiple = FALSE,
        choices = NULL,
        selected = NULL
      )
    )
  )
}
