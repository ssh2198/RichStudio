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


uploadTabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # keep track of DEGs inserted and not yet removed
    uploaded_degs <- reactiveValues(labels=NULL)
    uploaded_deg_datapaths <- reactiveVal(list())
    
    # Create reactive objects to make uploaded_degs and uploaded_deg_datapaths accessible in the Enrich tabPanel
    uploaded_degs_reactive <- reactive(uploaded_degs$labels)
    uploaded_deg_datapaths_reactive <- reactive(reactiveValuesToList(uploaded_deg_datapaths()))
    
    # when deg upload button clicked
    observeEvent(input$upload_deg_button, {
      req(input$deg_files) # Make sure file uploaded
      lab <- input$deg_files$name
      dp <- input$deg_files$datapath
      names(dp) <- lab
      uploaded_deg_datapaths(append(uploaded_deg_datapaths(), dp)) # set datapaths
      uploaded_degs$labels <- c(uploaded_degs$labels, lab) # set labels 
    })
    
    # when delete button clicked
    observeEvent(input$delete_degs, {
      req(input$selected_degs) # Make sure DEG selected
      nvector <- intersect(names(uploaded_deg_datapaths()), uploaded_degs$labels)
      subset_nvector <- uploaded_deg_datapaths()[nvector]
      
      uploaded_deg_datapaths <- setdiff(uploaded_deg_datapaths(), input$selected_degs)
      uploaded_degs$labels <- setdiff(uploaded_degs$labels, input$selected_degs)
    })
    
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_degs', choices=uploaded_degs_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=uploaded_degs_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_to_enrich', choices=uploaded_degs_reactive())
    })
    
    deg_to_table <- reactive ({
      req(input$deg_table_select)
      df <- read.csv(uploaded_deg_datapaths()[[input$deg_table_select]], 
                     header=TRUE,
                     sep='\t')
      return(df)
    })
    output$deg_table = DT::renderDataTable({
      deg_to_table()
    })
  })
}