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

# SOURCE RELATED SCRIPTS
source('cluster_hmap_func.R')
source('shiny_enrich.R')
source('upload_tab.R')
source('enrich_tab.R')


ui <- fluidPage(
  
  navbarPage("Enrichment Analysis",
    tabPanel("Upload",
      uploadTabUI("upload"),
    ),
    tabPanel("Enrich",
      enrichTabUI("enrich"),
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
  u_degnames <- reactiveValues(labels=NULL) # stores uploaded deg names
  u_degpaths <- reactiveVal(list()) # stores uploaded deg datapaths
  
  uploadTabServer("upload", u_degnames=u_degnames, u_degpaths=u_degpaths)
  enrichTabServer("enrich", u_degnames=u_degnames, u_degpaths=u_degpaths)
  
}

# Run the application 
shinyApp(ui = ui, server = server)



