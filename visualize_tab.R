library(shiny)
library(plotly)

source('rr_cluster.R')
source('make_heatmap.R')


visualizeTabUI <- function(id, tabName) {
  ns <- NS(id)
  tabItem(tabName = tabName,
    tabsetPanel(
      tabPanel("Cluster Result Heatmap",
        br(),
        box(title = "Cluster Heatmap", status = "primary", width = 12, collapsible = TRUE,
          p("Select cluster result to view comprehensive heatmap displaying values for each cluster"),
          selectInput(ns("clusdf_select"), "Select cluster result", choices=NULL, multiple=FALSE),
          fluidRow(
            column(4,
              selectInput(ns('big_value_type'), "Select value to show", choices=c("Padj", "Pvalue"))
            ),
            column(4,
              selectInput(ns('value_by'), "Calculate value by", choices=c("mean", "median", "min", "max"))
            )
          )
        ),
        br(),
        box(title = "Cluster Heatmap", status = "primary", width = 12,
          solidHeader = TRUE,
          plotlyOutput(ns('clusdf_hmap')),
        ),
        br(),
        box(title = "Term Heatmap", status = "primary", width = 12, collapsible = TRUE,
          p("Select individual clusters to view heatmap of values for each term in cluster"),
          selectInput(ns("indiv_clus_select"), "Select individual clusters", choices=NULL, multiple=TRUE),
          selectInput(ns('small_value_type'), "Select value to show", choices=c("Padj", "Pvalue"))
        ),
        br(),
        box(title = "Term Heatmap", status = "primary", width = 12,
          solidHeader = TRUE,
          plotlyOutput(ns('indiv_clus_hmap'))
        )
      ),
      tabPanel("Enrichment Result Heatmap",
        br(),
        tabBox(title = "Edit Heatmap", id="edit_hmap_box", width = 12,
          tabPanel("Add",
            selectInput(ns("rr_add_select"), "Select enrichment result", choices=NULL, multiple=FALSE),
            fluidRow(
              column(4,
                numericInput(ns('top_rr_terms'), "Select top ? terms", value = 20, min=0, max=100)
              ),
              column(4,
                selectInput(ns('rr_top_value_by'), "Select top terms by", choices=c("Padj", "Pvalue"))
              )
            ),
            DT::dataTableOutput(ns('rr_select_table')),
            actionButton(ns('add_rr'), "Add terms")
          ),
          tabPanel("Delete",
            selectInput(ns("rr_delete_select"), "Select enrichment result", choices=NULL, multiple=FALSE),
            selectInput(ns("rr_term_delete_select"), "Select terms to delete", choices=NULL, multiple=TRUE),
            fluidRow(
              column(4,
                actionButton(ns('delete_selected_rr_terms'), "Delete selected terms")
              ),
              column(4,
                actionButton(ns('delete_all_rr_terms'), "Delete all terms")
              ),
              column(4,
                actionButton(ns('delete_entire_rr'), "Delete entire result from heatmap")
              ),
            )
          )
        ),
        br(),
        box(title="Enrichment Result Heatmap", status="primary", width=12,
          solidHeader = TRUE,
          plotlyOutput(ns('rr_hmap')),
        )
      ),
      tabPanel("Cluster Info",
        br(),
        box(title = "Detailed Cluster Info", status = "primary", width = 12,
          solidHeader = TRUE,
          p("View and export individual term data for cluster results"),
          fluidRow(
            column(4,
              selectInput(ns("cluslist_select"), "Select cluster result", choices=NULL, multiple=FALSE),
            ),
            column(4,
              selectInput(ns('cluslist_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
            )
          ),
          DT::dataTableOutput(ns('cluslist_table')),
          br(),
          downloadButton(ns("download_cluslist"), "Download"),
        )
      )
    )
  )
}


visualizeTabServer <- function(id, u_clusnames, u_clusdfs, u_cluslists) {
  
  moduleServer(id, function(input, output, session) {
    
    # create reactive objs to make accessible in other modules
    u_clusnames_reactive <- reactive(u_clusnames$labels)
    u_clusdfs_reactive <- reactive(u_clusdfs)
    u_cluslists_reactive <- reactive(u_cluslists)
    
    term_vec_reactive <- reactiveVal(NULL)
    
    # update select inputs based on # cluster results
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'clusdf_select', choices=u_clusnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'indiv_clus_select', choices=term_vec_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'cluslist_select', choices=u_clusnames_reactive())
    })
    
    # Update term_vec_reactive when cluster result selection changes
    observeEvent(input$clusdf_select, {
      req(input$clusdf_select)
      
      df <- u_clusdfs[[input$clusdf_select]]
      term_vec_reactive(unique(df$Representative_Term))
      
      df <- u_clusdfs[[input$clusdf_select]]
      cluslist_df <- u_cluslists[[input$clusdf_select]]
      
      hmap <- comprehensive_hmap(final_data=df, cluster_list=cluslist_df, 
                                 value_type=input$big_value_type, value_by=input$value_by)
      
      output$clusdf_hmap <- renderPlotly({
        hmap
      })
      
    })
    
    # # plot cluster result
    # plot_clusdf_hmap <- reactive({
    #   req(input$clusdf_select)
    #   
    #   df <- u_clusdfs[[input$clusdf_select]]
    #   cluslist_df <- u_cluslists[[input$clusdf_select]]
    #   
    #   hmap <- comprehensive_hmap(final_data=df, cluster_list=cluslist_df, 
    #                              value_type=input$big_value_type, value_by=input$value_by)
    #   return(hmap)
    # })
    # output$clusdf_hmap <- renderPlotly({
    #   plot_clusdf_hmap()
    # })
    
    # plot indiv cluster hmap
    plot_cluslist_hmap <- reactive({
      req(input$indiv_clus_select)
      
      df <- u_clusdfs[[input$clusdf_select]]
      cluslist_df <- u_cluslists[[input$clusdf_select]]
      
      hmap <- cluster_hmap(cluster_list=cluslist_df, term_vec=input$indiv_clus_select, 
                           final_data=df, value_type=input$small_value_type)
      
      return(hmap)
    })
    output$indiv_clus_hmap <- renderPlotly({
      plot_cluslist_hmap()
    })
    
    # reactively update which cluster list table is read based on selection
    cluslist_to_table <- reactive ({
      req(input$cluslist_select)
      df <- u_cluslists[[input$cluslist_select]]
      return(df)
    })
    # output cluslist table
    output$cluslist_table = DT::renderDataTable({
      cluslist_to_table()
    })
    
    # download cluslist table
    output$download_cluslist <- downloadHandler(
      filename = function() {
        req(input$cluslist_select)
        paste0(input$cluslist_select, input$cluslist_export_type)
      },
      content = function(file) {
        ext_type <- input$cluslist_export_type
        if (ext_type == ".txt") {
          write.table(u_cluslists[[input$cluslist_select]], file, sep='\t', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_cluslists[[input$cluslist_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_cluslists[[input$cluslist_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
  })
  
}