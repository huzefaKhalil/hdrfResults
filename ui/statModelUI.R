
statModelUI <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    titlePanel(title = "Methods Information", windowTitle = "Methods Information"),
    
    box(
      width = 8,
      title = "Preprocessing",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      p("In order to ensure comparability across experiments, we started with the raw data where possible. Each experiment was run through the same quality control and annotation pipeline. For RNASeq experiments, ", a("fastp", href = "https://github.com/OpenGene/fastp"), " was used for quality control and ", a("STAR", href = "https://github.com/alexdobin/STAR"), " was used for alignment. Finally, quantification was done using the ", a("subread", href="http://subread.sourceforge.net/"), " package."),
      
      br(),
      
      p("Once the quality control, alignment were done, the DGE analysis was conducted using ", a("limma-voom", href="https://bioconductor.org/packages/release/bioc/html/limma.html"), ". ")
      
    )

  )
}