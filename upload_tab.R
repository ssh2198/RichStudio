library(shiny)

uploadTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # UPLOAD TAB CONTENTS
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          # DEG upload panel
          tabPanel("DEG",
            br(),
            textInput(ns('deg_text'), "Text Input", placeholder="Paste list of significant genes"),
            textInput(ns('textinput_name'), "Name", placeholder="Set name for pasted gene list"),
            fileInput(ns('deg_files'), 'File Input', multiple=FALSE, accept=c('.csv', '.tsv', '.xls', '.txt')),
            helpText("Accepted formats: .csv, .tsv, .xls, .txt"),
            selectInput(ns('separator_select'), "Element separator", c("Comma", "Space", "Tab", "Guess"), selected="Guess"),
            actionButton(ns('upload_deg_button'), "Upload")
          ),
          # Rich Result upload panel
          tabPanel("Rich Result",
            br(),
            textInput(ns('rr_text'), "Text Input", placeholder="Paste rich result here"),
            fileInput(ns('rr_files'), 'File Input', multiple=FALSE, accept=c('.csv', '.tsv', '.xls', '.txt')),
            helpText("Accepted formats: .csv, .tsv, .xls, .txt"),
            selectInput(ns('separator_select'), "Element separator", c(Comma=",", Space=" ", Tab="\t", "Guess"), selected="Guess"),
            actionButton(ns('upload_deg_button'), "Upload")
          )
        )
      ),
      mainPanel(
        # Uploaded File View
        h3("Uploaded Files"),
        tabsetPanel(
          # View DEGs
          tabPanel("DEG",
            br(),
            selectInput(ns('selected_degs'), "Select DEGs", choices=NULL, multiple=TRUE),
            actionButton(ns('delete_degs'), "Delete selected DEGS"),
            br(),
            br(),
            selectInput(ns('deg_table_select'), "Select DEG to view", choices=NULL),
            DT::dataTableOutput(ns('deg_table'))
          ),
          # View rich results
          tabPanel("Rich Result")
        )
      )
    )
  )
}


uploadTabServer <- function(id, u_degnames, u_degpaths) {
  
  moduleServer(id, function(input, output, session) {
    # create reactive obj to make u_degnames and u_degpaths accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels) 
    u_degpaths_reactive <- reactive(reactiveValuesToList(u_degpaths)) 
    
    # when deg upload button clicked
    observeEvent(input$upload_deg_button, {
      req(input$deg_files) # Make sure file uploaded
      
      lab <- input$deg_files$name
      dp <- input$deg_files$datapath
      names(dp) <- lab
      
      u_degpaths(append(u_degpaths(), dp)) # set u_degpaths
      u_degnames$labels <- c(u_degnames$labels, lab) # set u_degnames 
    })
    
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