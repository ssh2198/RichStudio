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
source('shiny_enrich.R')
source('upload_tab.R')



ui <- fluidPage(
  
  navbarPage("Enrichment Analysis",
    tabPanel("Upload",
      uploadTabUI("upload"),
    ),
    tabPanel("Enrich",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Enrich",
              br(),
              textInput('header_input', "Header", value="geneID"),
              selectInput('anntype_select', "Select annotation source", c("GO", "KEGG")),
              selectInput('keytype_select', "Select keytype", c("ACCNUM", "ALIAS", "ENSEMBL", "ENSEMBLPROT", "ENSEMBLTRANS",
                                                                "ENTREZID", "ENZYME", "EVIDENCE", "EVIDENCEALL", "FLYBASE",
                                                                "FLYBASECG", "FLYBASEPROT", "GENENAME", "GO", "GOALL", "MAP",
                                                                "ONTOLOGY", "ONTOLOGYALL", "PATH", "PMID", "REFSEQ", "SYMBOL",
                                                                "UNIGENE", "UNIPROT"), selected="SYMBOL"),
              selectInput('ont_select', "Select ontology", c("BP", "MF", "CC")),
              actionButton('enrich_deg', "Enrich")
            ),
            tabPanel("Cluster")
          )
        ),
        mainPanel(
          h3("Uploaded Files"),
          tabsetPanel(
            tabPanel("DEG",
              br(),
              selectInput('deg_to_enrich', "Select DEGs to enrich", choices=NULL, multiple=TRUE),
              p('Enriched DEGs will appear in "Rich Result" tab', style="color:grey")
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
  
  uploadTabServer("upload")
  
}

# Run the application 
shinyApp(ui = ui, server = server)



