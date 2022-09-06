
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
      
      p("In order to ensure comparability across experiments, we started with the raw data. Each experiment was run through the same quality control and annotation pipeline. For RNASeq experiments, ",
        a("fastp", href = "https://github.com/OpenGene/fastp"), " was used for quality control and ",
        a("STAR", href = "https://github.com/alexdobin/STAR"), " was used for alignment. Finally, quantification was done using the ",
        a("subread", href="http://subread.sourceforge.net/"), " package. For microarray experiments, RMA normalization was done."),
      br(),
      
      p("The mouse genome used for alignment was GRCm38 and the Rat genome was Rnor_6. Ensembl annotation was used for consistency. The database was constructed across species by combining across gene symbol. In case of different genes symbols, homologous genes were found and in such cases, the symbol name in this database contains both the mouse and rat symbol.")
    ),
    
    box(
      width = 8,
      title = "Differential Gene Expression Analysis",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      p("Once the quality control, alignment were done, the DGE analysis was conducted using ",
        a("limma-voom", href="https://bioconductor.org/packages/release/bioc/html/limma.html"), ".")
      
    ),
    
    box(
      width = 8,
      title = "Effect Size Calculation",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      p("The LogFC we get from the limma-voom workflow is an empirical Bayes estimate. The methodology to caluculate an effect size estimate from this statistic is described in ",
        a("Marot, et al (2009)", href="https://academic.oup.com/bioinformatics/article/25/20/2692/192916"), ".",
        "There are two effect size estimates we calculate: Cohen's d and Hedge's g.",
        "The latter is more conservative and corrects for small sample size.")
      
    ),
    
    box(
      width = 8,
      title = "Meta-Analysis Methodology",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      p("The meta-analysis estimate is calculated using the method of inverse variance using the ",
        a("metafor", href="https://metafor-project.org"), " R package.")
      
    ),
    
    box(
      width = 8,
      title = "Rank Rank Hypergeometric Overlap",
      status = "success",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,
      
      p("The RRHO method is useful for comparing the gene overlap between any two comparisons.",
      "The methodology is described in ", a("Cahill et al (2018)", href = "https://doi.org/10.1038/s41598-018-27903-2"), ".")
      
    )

  )
}