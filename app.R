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
library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(heatmaply)
library(devtools)

#install_github("hurlab/richR")
library(richR)
library(shinyHeatmaply)

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
source('visualize_tab.R')
source('export_tab.R')


ui <- fluidPage(
  
  navbarPage("Enrichment Analysis",
    tabPanel("Upload",
      uploadTabUI("upload"),
    ),
    tabPanel("Enrich",
      enrichTabUI("enrich"),
    ),
    tabPanel("Visualize",
      visualizeTabUI("visualize"),
    ),
    # tabPanel("Export",
    #   exportTabUI("export"),
    # )
  )
)


server <- function(input, output) {
  
  # DYNAMIC DEG / RICH RESULT / CLUSTER LISTS
  u_degnames <- reactiveValues(labels=NULL)  # uploaded deg names
  u_degdfs <- reactiveValues()  # uploaded deg dataframes
  u_rrnames <- reactiveValues(labels=NULL)  # rich result names
  u_rrdfs <- reactiveValues()  # rich result dataframes
  u_clusnames <- reactiveValues(labels=NULL)  # cluster result names
  u_clusdfs <- reactiveValues()  # cluster result dataframes
  u_cluslists <- reactiveValues()  # cluster info lists
  
  # SERVER LOGIC
  uploadTabServer("upload", u_degnames=u_degnames, u_degdfs=u_degdfs, 
                  u_rrnames=u_rrnames, u_rrdfs=u_rrdfs)
  enrichTabServer("enrich", u_degnames=u_degnames, u_degdfs=u_degdfs, 
                  u_rrnames=u_rrnames, u_rrdfs=u_rrdfs, u_clusnames=u_clusnames, u_clusdfs=u_clusdfs, u_cluslists=u_cluslists)
  visualizeTabServer("visualize", u_clusnames=u_clusnames, u_clusdfs=u_clusdfs, u_cluslists=u_cluslists)
  # exportTabServer("export")
}

# Run the application 
shinyApp(ui = ui, server = server)



