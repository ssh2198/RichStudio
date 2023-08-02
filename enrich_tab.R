library(shiny)
source('shiny_enrich.R')

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
            selectInput(ns('keytype_select'), "Select keytype", 
                        c("ACCNUM", "ALIAS", "ENSEMBL", "ENSEMBLPROT", "ENSEMBLTRANS",
                          "ENTREZID", "ENZYME", "EVIDENCE", "EVIDENCEALL", "FLYBASE",
                          "FLYBASECG", "FLYBASEPROT", "GENENAME", "GO", "GOALL", "MAP",
                          "ONTOLOGY", "ONTOLOGYALL", "PATH", "PMID", "REFSEQ", "SYMBOL",
                          "UNIGENE", "UNIPROT"), selected="SYMBOL"),
             selectInput(ns('ont_select'), "Select ontology", c("BP", "MF", "CC")),
             actionButton(ns('enrich_deg'), "Enrich")
          ),
          tabPanel("Cluster",
            br(),
            selectInput(ns('cluster_by'), "Cluster by", c("Mean Pvalue", "Median Pvalue", "Min Pvalue", "Mean Padj", "Median Padj", "Min Padj")),
            numericInput(ns('cutoff'), "Cutoff", value=.5, min=0, max=1),
            numericInput(ns('overlap'), "Overlap", value=.5, min=0, max=1),
            numericInput(ns('min_size'), "Overlap", value=2, min=0),
            actionButton(ns('cluster'), "Cluster selection")
          )
        )
      ),
      mainPanel(
        h3("Uploaded Files"),
        tabsetPanel(
          tabPanel("DEG",
             br(),
             selectInput(ns('selected_degs'), "Select DEGs to enrich", choices=NULL, multiple=TRUE),
             p('Enriched DEGs will appear in "Rich Result" tab', style="color:grey"),
             br(),
             selectInput(ns('deg_table_select'), "Select DEG to view", choices=NULL),
             DT::dataTableOutput(ns('deg_table'))
          ),
          tabPanel("Rich Result",
            br(),
            selectInput(ns('selected_rrs'), "Select rich results", choices=NULL, multiple=TRUE),
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

enrichTabServer <- function(id, u_degnames, u_degpaths, u_rrnames, u_rrdfs) {
  moduleServer(id, function(input, output, session) {
    # create reactive objs to make accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels) 
    u_degpaths_reactive <- reactive(reactiveValuesToList(u_degpaths)) 
    u_rrnames_reactive <- reactive(u_rrnames$labels) 
    u_rrdfs_reactive <- reactive(u_rrdfs)
    
    # enrich selected degs
    observeEvent(input$enrich_deg, {
      req(input$selected_degs)
      x <- read.delim(u_degpaths()[[input$selected_degs]], header=TRUE, sep='\t')
      
      # enrich
      df <- shiny_enrich(x=x, header=as.character(input$header_input), 
                         anntype=as.character(input$anntype_select), keytype=as.character(input$keytype_select), ontology=as.character(input$ont_select))
      lab <- input$selected_degs
      
      u_rrdfs[[lab]] <- df@result # set u_rrdfs
      u_rrnames$labels <- c(u_rrnames$labels, lab) # set u_rrnames 
    })
    
    # update select inputs based on # file inputs
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_degs', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_rrs', choices=u_rrnames_reactive())
    })
    
    # reactively update which deg table is read based on selection
    deg_to_table <- reactive ({
      req(input$deg_table_select)
      df <- read.csv(u_degpaths()[[input$deg_table_select]], 
                     header=TRUE,
                     sep='\t')
      return(df)
    })
    
    # output deg table
    output$deg_table = DT::renderDataTable({
      deg_to_table()
    })
  })
}