library(shiny)
# add reset button
# automatic column based on col name

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
            textAreaInput(ns('deg_text'), "Text Input", placeholder="Paste list of significant genes"),
            textInput(ns('textinput_name'), "Name", placeholder="Set name for pasted gene list"),
            fileInput(ns('deg_files'), 'File Input', multiple=TRUE, accept=c('.csv', '.tsv', '.xls', '.txt')),
            helpText("Accepted formats: .csv, .tsv, .xls, .txt"),
            selectInput(ns('deg_sep'), "Element separator", c("Comma", "Space", "Tab", "Guess"), selected="Guess"),
            actionButton(ns('upload_deg_button'), "Upload")
          ),
          # Rich Result upload panel
          tabPanel("Rich Result",
            br(),
            fileInput(ns('rr_files'), 'File Input', multiple=TRUE, accept=c('.csv', '.tsv', '.xls', '.txt')),
            helpText("Accepted formats: .csv, .tsv, .xls, .txt"),
            selectInput(ns('rr_sep'), "Element separator", c(Comma=",", Space=" ", Tab="\t", "Guess"), selected="Guess"),
            checkboxInput(ns('rr_rownames'), "First column contains rownames", value=FALSE),
            actionButton(ns('upload_rr_button'), "Upload")
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
          tabPanel("Rich Result",
            br(),
            selectInput(ns('selected_rrs'), "Select rich results", choices=NULL, multiple=TRUE),
            actionButton(ns('delete_rrs'), "Delete selected rich results"),
            br(),
            br(),
            selectInput(ns('rr_table_select'), "Select rich result to view", choices=NULL),
            DT::dataTableOutput(ns('rr_table'))
          )
        )
      )
    )
  )
}


uploadTabServer <- function(id, u_degnames, u_degdfs, u_rrnames, u_rrdfs) {
  
  moduleServer(id, function(input, output, session) {
    
    # create reactive objs to make accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels) 
    u_degdfs_reactive <- reactive(u_degdfs) 
    u_rrnames_reactive <- reactive(u_rrnames$labels) 
    u_rrdfs_reactive <- reactive(u_rrdfs)
    
    # update select inputs based on # file inputs
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_degs', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'selected_rrs', choices=u_rrnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'rr_table_select', choices=u_rrnames_reactive())
    })
    
    
    # <!----- DEG FILE MANAGEMENT -----!>
    # when deg upload button clicked
    observeEvent(input$upload_deg_button, {
      req(input$deg_files) # Make sure file uploaded
      
      for (i in seq_along(input$deg_files$name)) {
        lab <- input$deg_files$name[i]
        df <- read.delim(input$deg_files$datapath[i], header=TRUE, sep='\t') # read uploaded degfile
        
        u_degdfs[[lab]] <- df # set u_degdfs
        u_degnames$labels <- c(u_degnames$labels, lab) # set u_degnames 
      }
      
    })
    
    # when deg delete button clicked
    observeEvent(input$delete_degs, {
      req(input$selected_degs) # Make sure DEG selected
      
      # remove selected files from u_degdfs and u_degnames 
      u_degdfs <- setdiff(names(u_degdfs), input$selected_degs)
      u_degnames$labels <- setdiff(u_degnames$labels, input$selected_degs)
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
    # when rich result upload button clicked
    observeEvent(input$upload_rr_button, {
      req(input$rr_files) # Make sure file uploaded
      
      for (i in seq_along(input$rr_files$name)) {
        lab <- input$rr_files$name[i]
        
        # fix logic for csv files later
        if (input$rr_sep == ",") {
          if (input$rr_rownames == TRUE) {
            df <- read.csv(input$rr_files$datapath[i], row.names=1)
          } else {
            df <- read.csv(input$rr_files$datapath[i])
          }
        } else {
          if (input$rr_rownames == TRUE) {
            
          } else {
            df <- read.delim(input$rr_files$datapath[i], header=TRUE, sep='\t')
          }
        }
        
        u_rrdfs[[lab]] <- df # set u_rrdfs
        u_rrnames$labels <- c(u_rrnames$labels, lab) # set u_rrnames 
      }
      
    })
    
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
    
  })
  
}