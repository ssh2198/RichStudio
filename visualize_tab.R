library(shiny)
library(plotly)

source('cluster_hmap_func.R')


visualizeTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tabsetPanel(
      tabPanel("Heatmap",
        br(),
        selectInput(ns("clusdf_select"), "View cluster result", choices=NULL, multiple=FALSE),
        checkboxInput(ns('big_plotly_view'), "View as plotly", value=TRUE),
        br(),
        plotlyOutput(ns('clusdf_hmap')),
        br(),
        selectInput(ns("indiv_clus_select"), "Select individual clusters", choices=NULL, multiple=TRUE),
        checkboxInput(ns('small_plotly_view'), "View as plotly", value=TRUE),
        br(),
        plotlyOutput(ns('indiv_clus_hmap'))
      ),
      tabPanel("Cluster Info",
        br(),
        selectInput(ns("cluslist_select"), "Select cluster result to view info", choices=NULL, multiple=FALSE),
        br(),
        DT::dataTableOutput(ns('cluslist_table'))
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
    
    # plot cluster result
    plot_clusdf_hmap <- reactive({
      req(input$clusdf_select)
      
      df <- u_clusdfs[[input$clusdf_select]]
      cluslist_df <- u_cluslists[[input$clusdf_select]]
      
      term_vec_reactive(df$Representative_Term) # create list of terms in heatmap
      
      hmap <- comprehensive_hmap(final_data=df, cluster_list=cluslist_df, as_plotly=input$big_plotly_view, 
                                 value_type="Padj", value_by="mean")
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
                           final_data=df, value_type="Padj", as_plotly=input$small_plotly_view)
      
      return(hmap)
    })
    output$indiv_clus_hmap <- renderPlotly({
      plot_cluslist_hmap()
    })
    
    # reactively update which cluster list table is read based on selection
    cluslist_to_table <- reactive ({
      req(input$clus_table_select)
      df <- u_cluslists[[input$cluslist_select]]
      return(df)
    })
    # output cluslist table
    output$cluslist_table = DT::renderDataTable({
      cluslist_to_table()
    })
    
  })
  
}