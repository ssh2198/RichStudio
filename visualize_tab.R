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
        br(),
        plotlyOutput(ns('clusdf_hmap')),
        br(),
        selectInput(ns("indiv_clus_select"), "Select individual clusters", choices=NULL, multiple=TRUE),
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
    
    # update select inputs based on # cluster results
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'clusdf_select', choices=u_clusnames_reactive())
      # fix choices
      updateSelectInput(session=getDefaultReactiveDomain(), 'indiv_clus_select', choices=u_clusnames_reactive())
    })
    
    # plot cluster result
    plot_clusdf_hmap <- reactive({
      req(input$clusdf_select)
      df <- u_clusdfs[[input$clusdf_select]]
      hmap <- comprehensive_hmap(df, names(u_clusdfs), as_plotly=TRUE)
      return(hmap)
    })
    output$clusdf_hmap <- renderPlotly({
      plot_clusdf_hmap()
    })
    
  })
  
}