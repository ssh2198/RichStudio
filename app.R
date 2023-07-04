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
library(devtools)

install_github("hurlab/richR")
library(richR)

gs1 <- read.delim("~/Desktop/Hur Lab/enrichment-analysis3/data/GO_HF12wk_vs_WT12wk.txt")
gs2 <- read.delim("~/Desktop/Hur Lab/enrichment-analysis3/data/GO_HF36wk_vs_HF12wk.txt")
gs3 <- read.delim("~/Desktop/Hur Lab/enrichment-analysis3/data/KEGG_HF36wk_vs_HF12wk.txt")

genesets <- list(gs1, gs2, gs3)

cluster <- function(genesets) {
  
  # suffix all non 'Term' columns with their index
  for (i in seq_along(genesets)) {
    non_term_cols <- colnames(genesets[[i]])
    non_term_cols[-which(non_term_cols == 'Term')] <- paste0(non_term_cols[-which(non_term_cols == 'Term')], i)
    colnames(genesets[[i]]) <- non_term_cols
  }
  
  # initialize merged_gs with first geneset
  merged_gs <- genesets[[1]]
  
  # merge genesets
  for (i in 2:length(genesets)) {
    merged_gs <- merge(merged_gs, genesets[[i]], by='Term', all=TRUE)
  }
  
  # combine unique 'GeneID' elements
  for (i in 2:length(genesets)) {
    geneID_1 <- paste0('GeneID', i-1)
    geneID_2 <- paste0('GeneID', i)
    
    merged_gs$GeneID <- mapply(function (x, y) paste(unique(c(x, y)), collapse=','), 
                               merged_gs[, geneID_1], merged_gs[, geneID_2])
  }
  
  # average Pvalue
  pval_cols <- c()
  for (i in seq_along(genesets)) {
    pval_cols <- c(pval_cols, paste0('Pvalue', i))
  }
  merged_gs$Pvalue <- rowMeans(merged_gs[, pval_cols], na.rm=TRUE)
  
  #return
  return()
  
}

# use richCluster from richR package to cluster
clustered_gs <- richCluster(x=merged_gs, minSize=3)
write.table(clustered_gs, file='/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis3/data/clustered_data.txt', sep='\t')

clustered_data <- richCluster(x=combined_data, minSize=3)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Gene Enrichment Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # insert file input later
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("my_table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$my_table <- renderTable({
    data[, -1]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



