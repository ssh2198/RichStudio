library(shiny)
library(plotly)


visualizeTabUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tabsetPanel(
      tabPanel("Heatmap",
        plotlyOutput(ns('hmap'))
      ),
      tabPanel("Cluster Info")
    )
  )
}


visualizeTabServer <- function(id, clustered_gs=NULL, cluster_list=NULL) {
  
  moduleServer(id, function(input, output, session) {
    
  })
  
}