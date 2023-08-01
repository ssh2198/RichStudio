library(shiny)

degViewUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    br(),
    selectInput(ns('selected_degs'), "Select DEGs", choices=NULL, multiple=TRUE),
    actionButton(ns('delete_degs'), "Delete selected DEGS"),
    br(),
    br(),
    selectInput(ns('deg_table_select'), "Select DEG to view", choices=NULL),
    DT::dataTableOutput(ns('deg_table'))
  )
}

degViewServer <- function(id, 
                          u_degnames=reactiveValues(labels=NULL), 
                          u_degpaths=reactive(reactiveValuesToList(u_degpaths))) {
  
  moduleServer(id, function(input, output, session) {
    # create reactive obj to make u_degnames and u_degpaths accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels)
    u_degpaths_reactive <- reactive(reactiveValuesToList(u_degpaths)) 
    
    # when delete button clicked
    observeEvent(input$delete_degs, {
      req(input$selected_degs) # Make sure DEG selected
      
      nvector <- intersect(names(u_degpaths()), u_degnames$labels)
      subset_nvector <- u_degpaths()[nvector]
      
      # remove selected files from u_degpaths and u_degnames 
      u_degpaths <- setdiff(u_degpaths(), input$selected_degs)
      u_degnames$labels <- setdiff(u_degnames$labels, input$selected_degs)
    })
    
    # update select inputs based on # file inputs
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_degs', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=u_degnames_reactive())
    })
    
    # reactively update which table is read based on selection
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