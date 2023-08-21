library(shiny)
library(shinydashboard)

source('shiny_enrich.R')
source('cluster_hmap_func.R')


enrichTabUI <- function(id, tabName) {
  ns <- NS(id)
  tabItem(tabName = tabName,
    # ENRICH TAB CONTENTS
    fluidRow(
      column(width = 4,
        box(title="Enrich", width=12, status="primary", solidHeader=TRUE,
          br(),
          selectInput(ns('selected_degs'), "Select DEG sets to enrich", choices=NULL, multiple=TRUE),
          textInput(ns('header_input'), "Header", value="geneID"),
          selectInput(ns('anntype_select'), "Select annotation source", c("GO", "KEGG", "Reactome", "KEGGM")),
          selectInput(ns('keytype_select'), "Select keytype", 
                      c("ACCNUM", "ALIAS", "ENSEMBL", "ENSEMBLPROT", "ENSEMBLTRANS",
                        "ENTREZID", "ENZYME", "EVIDENCE", "EVIDENCEALL", "FLYBASE",
                        "FLYBASECG", "FLYBASEPROT", "GENENAME", "GO", "GOALL", "MAP",
                        "ONTOLOGY", "ONTOLOGYALL", "PATH", "PMID", "REFSEQ", "SYMBOL",
                        "UNIGENE", "UNIPROT"), selected="SYMBOL"),
          selectInput(ns('ont_select'), "Select ontology", c("BP", "MF", "CC")),
          selectInput(ns('species_select'), "Select species", c('anopheles', 'arabidopsis', 'bovine', 'celegans', 'canine', 'fly', 'zebrafish',
                                                                'ecoli', 'chicken', 'human', 'mouse', 'rhesus', 'malaria', 'chipm', 'rat',
                                                                'toxoplasma', 'sco', 'pig', 'yeast', 'xenopus'), selected='human'),
          
          actionButton(ns('enrich_deg'), "Enrich")
        )
      ),
      column(width = 8,
        h3("Enrichment Result Visualization"),
        tabsetPanel(
          tabPanel("Table",
          
          ),
          tabPanel("Bar Plot",
          
          ),
          tabPanel("Dot Plot",
            
          )
        )
      )
    ),
    tags$head(
      tags$style(
        HTML(".box-title { font-size: 20px; }")  # Adjust the font size as needed
      )
    )
  )
}


enrichTabServer <- function(id, u_degnames, u_degdfs, u_rrnames, u_rrdfs, u_clusnames, u_clusdfs, u_cluslists) {
  
  moduleServer(id, function(input, output, session) {
    
    # create reactive objs to make accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels) 
    u_degdfs_reactive <- reactive(u_degdfs) 
    u_rrnames_reactive <- reactive(u_rrnames$labels) 
    u_rrdfs_reactive <- reactive(u_rrdfs)
    u_clusnames_reactive <- reactive(u_clusnames$labels)
    u_clusdfs_reactive <- reactive(u_clusdfs)
    u_cluslists_reactive <- reactive(u_cluslists)
    
    # update select inputs based on # file inputs
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_degs', choices=u_degnames_reactive())
      
    })
    
    
    # <!----- ENRICH LOGIC -----!>
    # enrich selected degs
    observeEvent(input$enrich_deg, {
      req(input$selected_degs)
      
      for (i in seq_along(input$selected_degs)) {
        x <- u_degdfs[[input$selected_degs[i]]]
        
        # enrich
        df <- shiny_enrich(x=x, header=as.character(input$header_input), species=input$species_select,
                           anntype=as.character(input$anntype_select), keytype=as.character(input$keytype_select), ontology=as.character(input$ont_select))
        print(paste("Done enriching", input$selected_degs[i]))
        lab <- input$selected_degs[i]
        
        u_rrdfs[[lab]] <- df@result # set u_rrdfs
        u_rrnames$labels <- c(u_rrnames$labels, lab) # set u_rrnames 
      }
      print("Done enriching all DEG sets")
    })
    
  })
}