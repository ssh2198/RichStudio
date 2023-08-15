library(shiny)

source('shiny_enrich.R')
source('cluster_hmap_func.R')


enrichTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # ENRICH TAB CONTENTS
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel("Enrich",
            br(),
            textInput(ns('header_input'), "Header", value="geneID"),
            selectInput(ns('anntype_select'), "Select annotation source", c("GO", "KEGG", "Reactome", "KEGGM")),
            selectInput(ns('keytype_select'), "Select keytype", 
                        c("ACCNUM", "ALIAS", "ENSEMBL", "ENSEMBLPROT", "ENSEMBLTRANS",
                          "ENTREZID", "ENZYME", "EVIDENCE", "EVIDENCEALL", "FLYBASE",
                          "FLYBASECG", "FLYBASEPROT", "GENENAME", "GO", "GOALL", "MAP",
                          "ONTOLOGY", "ONTOLOGYALL", "PATH", "PMID", "REFSEQ", "SYMBOL",
                          "UNIGENE", "UNIPROT"), selected="SYMBOL"),
             selectInput(ns('ont_select'), "Select ontology", c("BP", "MF", "CC")),
             selectInput(ns('species_select'), "Select species", c("Human", "Mouse", "Rat")),
             actionButton(ns('enrich_deg'), "Enrich")
          ),
          tabPanel("Cluster",
            br(),
            textInput(ns('cluster_name'), "Name", value="Test group"),
            selectInput(ns('cluster_by'), "Cluster by", c("Mean Pvalue", "Median Pvalue", "Min Pvalue", "Mean Padj", "Median Padj", "Min Padj")),
            numericInput(ns('cutoff'), "Cutoff", value=.5, min=0, max=1),
            numericInput(ns('overlap'), "Overlap", value=.5, min=0, max=1),
            numericInput(ns('min_size'), "Min size", value=2, min=0),
            actionButton(ns('cluster'), "Cluster")
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
            actionButton(ns('delete_rr'), "Delete selection"),
            br(),
            br(),
            selectInput(ns('rr_table_select'), "Select rich result to view", choices=NULL),
            DT::dataTableOutput(ns('rr_table'))
          ),
          tabPanel("Cluster Result",
            br(),
            p("Clustered genesets will appear here", style="color:grey"),
            selectInput(ns('selected_clus'), "Select cluster results", choices=NULL, multiple=TRUE),
            actionButton(ns('delete_clus'), "Delete selection"),
            br(),
            br(),
            selectInput(ns('clus_table_select'), "Select cluster result to view", choices=NULL),
            DT::dataTableOutput(ns('clus_table'))
          )
        )
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
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_rrs', choices=u_rrnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'rr_table_select', choices=u_rrnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_clus', choices=u_clusnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'clus_table_select', choices=u_clusnames_reactive())
    })
    
    
    # <!----- DEG / ENRICH LOGIC -----!>
    # enrich selected degs
    observeEvent(input$enrich_deg, {
      req(input$selected_degs)
      
      for (i in seq_along(input$selected_degs)) {
        x <- u_degdfs[[input$selected_degs[i]]]
        
        # enrich
        df <- shiny_enrich(x=x, header=as.character(input$header_input), 
                           anntype=as.character(input$anntype_select), keytype=as.character(input$keytype_select), ontology=as.character(input$ont_select))
        print(paste("Done enriching", input$selected_degs[i]))
        lab <- input$selected_degs[i]
        
        u_rrdfs[[lab]] <- df@result # set u_rrdfs
        u_rrnames$labels <- c(u_rrnames$labels, lab) # set u_rrnames 
      }
      print("Done enriching all DEG sets")
    })
    
    # reactively update which deg table is read based on selection
    deg_to_table <- reactive ({
      req(input$deg_table_select)
      df <- u_degdfs[[input$deg_table_select]]
      return(df)
    })
    
    # output deg table
    output$deg_table = DT::renderDataTable({
      deg_to_table()
    })
    
    
    # <!----- RICH RESULT FILE MANAGEMENT -----!>
    # when rr delete button clicked
    observeEvent(input$delete_rrs, {
      req(input$selected_rrs) # Make sure DEG selected
      
      # remove selected files from u_rrdfs and u_rrnames 
      u_rrdfs <- setdiff(names(u_rrdfs), input$selected_rrs)
      u_rrnames$labels <- setdiff(u_rrnames$labels, input$selected_rrs)
    })
    
    # reactively update which rr table is read based on selection
    rr_to_table <- reactive ({
      req(input$rr_table_select)
      df <- u_rrdfs[[input$rr_table_select]]
      return(df)
    })
    
    # output rr table
    output$rr_table = DT::renderDataTable({
      rr_to_table()
    })
    
    
    # <!----- CLUSTERING LOGIC -----!>
    # clustering
    observeEvent(input$cluster, {
      req(input$selected_rrs) # Require rich result selection
      req(input$cluster_name) # Require cluster name
      
      genesets <- list()
      gs_names <- c()
      for (i in seq_along(input$selected_rrs)) {
        tmp <- input$selected_rrs[i]
        genesets <- c(genesets, list(u_rrdfs[[tmp]]))
      }
      names(genesets) <- input$selected_rrs
      gs_names <- names(genesets)
      
      merged_gs <- merge_genesets(genesets)
      print("done merge")
      clustered_gs <- cluster(merged_gs=merged_gs, cutoff=input$cutoff, overlap=input$overlap, minSize=input$min_size)
      print("done clustering")
      cluster_list <- get_cluster_list(clustered_gs=clustered_gs, merged_gs=merged_gs, gs_names=gs_names) # get cluster info
      final_data <- hmap_prepare(clustered_gs, gs_names=gs_names) # final data
      final_data <- change_finaldata_valueby(final_data=final_data, cluster_list=cluster_list, value_by="mean")
      
      # store in reactive
      lab <- input$cluster_name
      u_clusdfs[[lab]] <- final_data # set u_clusdfs
      u_cluslists[[lab]] <- cluster_list # set u_cluslists
      u_clusnames$labels <- c(u_clusnames$labels, lab) # set u_clusnames
    })
    
    # reactively update which cluster table is read based on selection
    clus_to_table <- reactive ({
      req(input$clus_table_select)
      df <- u_clusdfs[[input$clus_table_select]]
      return(df)
    })
    
    # output rr table
    output$clus_table = DT::renderDataTable({
      clus_to_table()
    })
  })
}