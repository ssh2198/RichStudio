library(shiny)
library(plotly)

source('cluster_hmap_func.R')


visualizeTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tabsetPanel(
      tabPanel("Heatmap",
        wellPanel(
          h4("Cluster Heatmap"),
          p("Select cluster result to view comprehensive heatmap displaying values for each cluster"),
          selectInput(ns("clusdf_select"), "Select cluster result", choices=NULL, multiple=FALSE),
          checkboxInput(ns('big_plotly_view'), "View as plotly", value=TRUE),
          fluidRow(
            column(4,
              selectInput(ns('big_value_type'), "Select value to show", choices=c("Pvalue", "Padj"))
            ),
            column(4,
              selectInput(ns('value_by'), "Calculate value by", choices=c("mean", "median", "min", "max"))
            )
          )
        ),
        br(),
        plotlyOutput(ns('clusdf_hmap')),
        br(),
        wellPanel(
          h4("Term Heatmap"),
          p("Select individual clusters to view heatmap of values for each term in cluster"),
          selectInput(ns("indiv_clus_select"), "Select individual clusters", choices=NULL, multiple=TRUE),
          checkboxInput(ns('small_plotly_view'), "View as plotly", value=TRUE),
          selectInput(ns('small_value_type'), "Select value to show", choices=c("Pvalue", "Padj"))
        ),
        br(),
        plotlyOutput(ns('indiv_clus_hmap'))
      ),
      tabPanel("Cluster Info",
        h3("Detailed Cluster Info"),
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
        br()
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
    })
    
    # plot cluster result
    plot_clusdf_hmap <- reactive({
      req(input$clusdf_select)
      
      df <- u_clusdfs[[input$clusdf_select]]
      cluslist_df <- u_cluslists[[input$clusdf_select]]
      
      hmap <- comprehensive_hmap(final_data=df, cluster_list=cluslist_df, as_plotly=input$big_plotly_view, 
                                 value_type=input$big_value_type, value_by=input$value_by)
      return(hmap)
    })
    output$clusdf_hmap <- renderPlotly({
      plot_clusdf_hmap()
    })
    
    # plot indiv cluster hmap
    plot_cluslist_hmap <- reactive({
      req(input$indiv_clus_select)
      
      df <- u_clusdfs[[input$clusdf_select]]
      cluslist_df <- u_cluslists[[input$clusdf_select]]
      
      hmap <- cluster_hmap(cluster_list=cluslist_df, term_vec=input$indiv_clus_select, 
                           final_data=df, value_type=input$small_value_type, as_plotly=input$small_plotly_view)
      
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
          write.table(u_cluslists[[input$cluslist_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_cluslists[[input$cluslist_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_cluslists[[input$cluslist_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
  })
  
}