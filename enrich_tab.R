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
        h3("File View"),
        tabsetPanel(
          # deg set view tab
          tabPanel("DEG Sets",
            br(),
            selectInput(ns('selected_degs'), "Select DEG sets to enrich", choices=NULL, multiple=TRUE),
            p('Enriched DEG sets will appear in "Rich Result" tab', style="color:grey"),
            hr(),
            h4("Table view/export"),
            fluidRow(
              column(4,
                selectInput(ns('deg_table_select'), "Select DEG set", choices=NULL),
              ),
              column(4,
                selectInput(ns('deg_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
              )
            ),
            
            DT::dataTableOutput(ns('deg_table')),
            br(),
            downloadButton(ns("download_deg"), "Download"),
            br()
          ),
          # rich result view tab
          tabPanel("Rich Results",
            br(),
            p("Enriched DEG sets and uploaded enrichment results will appear here", style="color:grey"),
            selectInput(ns('selected_rrs'), "Select rich results", choices=NULL, multiple=TRUE),
            actionButton(ns('delete_rr'), "Delete selection"),
            hr(),
            h4("Table view/export"),
            fluidRow(
              column(4,
                selectInput(ns('rr_table_select'), "Select rich result", choices=NULL),
              ),
              column(4,
                selectInput(ns('rr_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
              )
            ),
            DT::dataTableOutput(ns('rr_table')),
            br(),
            downloadButton(ns("download_rr"), "Download"),
            br()
          ),
          tabPanel("Cluster Result",
            br(),
            p("Clustered enrichment results will appear here", style="color:grey"),
            selectInput(ns('selected_clus'), "Select cluster results", choices=NULL, multiple=TRUE),
            actionButton(ns('delete_clus'), "Delete selection"),
            hr(),
            h4("Table view/export"),
            fluidRow(
              column(4,
                selectInput(ns('clus_table_select'), "Select cluster result", choices=NULL),
              ),
              column(4,
                selectInput(ns('clus_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
              )
            ),
            DT::dataTableOutput(ns('clus_table')),
            br(),
            downloadButton(ns("download_clus"), "Download"),
            br()
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
    
    # download deg
    output$download_deg <- downloadHandler(
      filename = function() {
        req(input$deg_table_select)
        paste0(input$deg_table_select, input$deg_export_type)
      },
      content = function(file) {
        ext_type <- input$deg_export_type
        if (ext_type == ".txt") {
          write.table(u_degdfs[[input$deg_table_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_degdfs[[input$deg_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_degdfs[[input$deg_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
    
    
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
    
    # download rr
    output$download_rr <- downloadHandler(
      filename = function() {
        req(input$rr_table_select)
        paste0(input$rr_table_select, input$rr_export_type)
      },
      content = function(file) {
        ext_type <- input$rr_export_type
        if (ext_type == ".txt") {
          write.table(u_rrdfs[[input$rr_table_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_rrdfs[[input$rr_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_rrdfs[[input$rr_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
    
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
    
    # when cluster delete button clicked
    observeEvent(input$delete_clus, {
      req(input$selected_clus) # Make sure cluster result selected
      
      # remove selected files from u_clusdfs, u_cluslists, u_clusnames 
      u_clusdfs <- setdiff(names(u_clusdfs), input$selected_clus)
      u_cluslists <- setdiff(names(u_cluslists), input$selected_clus)
      u_clusnames$labels <- setdiff(u_clusnames$labels, input$selected_clus)
    })
    
    # reactively update which cluster table is read based on selection
    clus_to_table <- reactive ({
      req(input$clus_table_select)
      df <- u_clusdfs[[input$clus_table_select]]
      return(df)
    })
    
    # output cluster result table
    output$clus_table = DT::renderDataTable({
      clus_to_table()
    })
    
    # download cluster result
    output$download_clus <- downloadHandler(
      filename = function() {
        req(input$clus_table_select)
        paste0(input$clus_table_select, input$clus_export_type)
      },
      content = function(file) {
        ext_type <- input$clus_export_type
        if (ext_type == ".txt") {
          write.table(u_clusdfs[[input$clus_table_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_clusdfs[[input$clus_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_clusdfs[[input$clus_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
  })
}