library(shiny)
source('deg_rr_view.R')

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
            degViewUI('deg_view')
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
    degViewServer('deg_view', u_degnames_reactive(), u_degpaths_reactive())
    
  })
  
}