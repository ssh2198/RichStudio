library(shiny)

enrichTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel("Enrich",
                   br(),
                   textInput(ns('header_input'), "Header", value="geneID"),
                   selectInput(ns('anntype_select'), "Select annotation source", c("GO", "KEGG")),
                   selectInput(ns('keytype_select'), "Select keytype", c("ACCNUM", "ALIAS", "ENSEMBL", "ENSEMBLPROT", "ENSEMBLTRANS",
                                                                     "ENTREZID", "ENZYME", "EVIDENCE", "EVIDENCEALL", "FLYBASE",
                                                                     "FLYBASECG", "FLYBASEPROT", "GENENAME", "GO", "GOALL", "MAP",
                                                                     "ONTOLOGY", "ONTOLOGYALL", "PATH", "PMID", "REFSEQ", "SYMBOL",
                                                                     "UNIGENE", "UNIPROT"), selected="SYMBOL"),
                   selectInput(ns('ont_select'), "Select ontology", c("BP", "MF", "CC")),
                   actionButton(ns('enrich_deg'), "Enrich")
          ),
          tabPanel("Cluster")
        )
      ),
      mainPanel(
        h3("Uploaded Files"),
        tabsetPanel(
          tabPanel("DEG",
             br(),
             selectInput(ns('deg_to_enrich'), "Select DEGs to enrich", choices=NULL, multiple=TRUE),
             p('Enriched DEGs will appear in "Rich Result" tab', style="color:grey")
          ),
          tabPanel("Rich Result",
            br(),
            selectInput(ns('rr_view'), "Select rich results", choices=NULL, multiple=TRUE),
            actionButton(ns('delete_rr'), "Delete selected rich results"),
            br(),
            br(),
            selectInput(ns('rr_table_select'), "Select rich result to view", choices=NULL),
            DT::dataTableOutput(ns('rr_table'))
          )
        )
      )
    )
  )
}

enrichTabServer <- function(id, u_degnames, u_degpaths) {
  moduleServer(id, function(input, output, session) {
    
  })
}