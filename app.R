#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tools)
library(gtools)
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(devtools)

install_github("hurlab/richR")
library(richR)

#SET WORKING DIRECTORY
getwd()
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
base_dir = dirname(current_path)
output = "output/"

source('cluster_hmap_func.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("Enrichment Analysis",
    # UPLOAD TAB
    tabPanel("Upload",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            # DEG upload panel
            tabPanel("DEG",
              br(),
              textInput('deg_text', "Text Input", placeholder="Paste list of significant genes"),
              fileInput('deg_files', 'File Input', multiple=FALSE, accept=c('.csv', '.tsv', '.xls', '.txt')),
              helpText("Accepted formats: .csv, .tsv, .xls, .txt"),
              selectInput('separator_select', "Element separator", c("Comma", "Space", "Tab", "Guess"), selected="Guess"),
              actionButton('upload_deg_button', "Upload")
            ),
            # Rich Result upload panel
            tabPanel("Rich Result",
              br(),
              textInput('rr_text', "Text Input", placeholder="Paste rich result here"),
              fileInput('rr_files', 'File Input', multiple=FALSE, accept=c('.csv', '.tsv', '.xls', '.txt')),
              helpText("Accepted formats: .csv, .tsv, .xls, .txt"),
              selectInput('separator_select', "Element separator", c(Comma=",", Space=" ", Tab="\t", "Guess"), selected="Guess"),
              actionButton('upload_deg_button', "Upload")
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
              selectInput('selected_degs', "Select DEGs", choices=NULL, multiple=TRUE),
              tags$div(id='placeholder'), # dynamic ui
              actionButton('delete_degs', "Delete selected DEGS"),
              br(),
              br(),
              selectInput('deg_table_select', "Select DEG to view", choices=NULL),
              DT::dataTableOutput('deg_table')
            ),
            # View rich results
            tabPanel("Rich Result")
          )
        )
      )
    ),
    tabPanel("Enrich",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Enrich",
              br(),
              selectInput('keytype_select', "Select keytype", c("ACCNUM", "ALIAS", "ENSEMBL", "ENSEMBLPROT", "ENSEMBLTRANS",
                                                                "ENTREZID", "ENZYME", "EVIDENCE", "EVIDENCEALL", "FLYBASE",
                                                                "FLYBASECG", "FLYBASEPROT", "GENENAME", "GO", "GOALL", "MAP",
                                                                "ONTOLOGY", "ONTOLOGYALL", "PATH", "PMID", "REFSEQ", "SYMBOL",
                                                                "UNIGENE", "UNIPROT"), selected="SYMBOL"),
              selectInput('annot_select', "Select ontology", c("BP", "MF", "CC", "ALL"))
            ),
            tabPanel("Cluster")
          )
        ),
        mainPanel(
          h3("Uploaded Files"),
          tabsetPanel(
            tabPanel("DEG",
              
            ),
            tabPanel("Rich Result")
          )
        )
      )
    ),
    tabPanel("Visualize",
      tabsetPanel(
        tabPanel("Heatmap"),
        tabPanel("Cluster Info")
      )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # keep track of DEGs inserted and not yet removed
  uploaded_degs <- reactiveValues(labels=NULL)
  uploaded_deg_datapaths <- reactiveVal(list())
  
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
    updateSelectInput(session=getDefaultReactiveDomain(), 'selected_degs', choices=uploaded_degs$labels)
    updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=uploaded_degs$labels)
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)



