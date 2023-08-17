library(shiny)

exportTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # EXPORT TAB CONTENTS
    sidebarLayout(
      sidebarPanel(
        h4("Export"),
        p("Select files to download"),
        selectInput(ns('selected_degs'), "Select DEGs", choices=NULL, multiple=TRUE),
        selectInput(ns('selected_rrs'), "Select rich results", choices=NULL, multiple=TRUE),
        selectInput(ns('selected_cluslists'), "Select cluster results", choices=NULL, multiple=TRUE),
        selectInput(ns('file_ext'), "Export as", choices=c(".txt", ".csv", "tsv"), multiple=FALSE),
        downloadButton("download", "Download selected files")
      ),
      mainPanel(
        # File Table View
        h3("File Table View"),
        br(),
        # View file
        fluidRow(
          column(4,
            selectInput(ns('deg_table'), "Select DEGs", choices=NULL, multiple=FALSE)
          ),
          column(4,
            selectInput(ns('rr_table'), "Select rich results", choices=NULL, multiple=FALSE)
          ),
          column(4,
            selectInput(ns('cluslist_table'), "Select cluster results", choices=NULL, multiple=FALSE)
          )
        ),
        br(),
        DT::dataTableOutput(ns('file_table'))
      )
    )
  )
}


exportTabServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    
    
  })
  
}