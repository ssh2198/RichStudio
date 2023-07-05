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
library(gtools)
library(data.table)
library(ggplot2)

install_github("hurlab/richR")
library(richR)

gs1 <- read.delim("~/Desktop/Hur Lab/enrichment-analysis3/data/GO_HF12wk_vs_WT12wk.txt")
gs2 <- read.delim("~/Desktop/Hur Lab/enrichment-analysis3/data/GO_HF36wk_vs_HF12wk.txt")
gs3 <- read.delim("~/Desktop/Hur Lab/enrichment-analysis3/data/KEGG_HF36wk_vs_HF12wk.txt")

genesets <- list(gs1, gs2, gs3)


# this function merges a list of genesets together to prepare for clustering
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
  
  # average Padj
  padj_cols <- c()
  for (i in seq_along(genesets)) {
    padj_cols <- c(padj_cols, paste0('Padj', i))
  }
  merged_gs$Padj <- rowMeans(merged_gs[, padj_cols], na.rm=TRUE)
  
  #return
  return(merged_gs)
  
}

# use richCluster from richR package to cluster
clustered_gs <- richCluster(x=merged_gs, minSize=3)
write.table(clustered_gs, file='/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis3/data/clustered_data.txt', sep='\t')

hmap_prepare <- function(clustered_gs, num_clusters) {
  
  # create tibble with Term and AnnotationCluster
  x <- as_tibble(cbind(clustered_gs$Term, clustered_gs$AnnotationCluster))
  colnames(x) <- c('Term', 'Cluster')
  
  # order clusters numerically
  x <- x[order(as.numeric(x$Cluster)), ]
  
  clusters <- c()
  #group the Padj values by cluster using the Annotation clustered levels
  clustered_gs$AnnotationCluster <- as.factor(clustered_gs$AnnotationCluster)
  for (i in 1:length(levels(clustered_gs$AnnotationCluster))){
    clusters[[i]] <- clustered_gs[clustered_gs$AnnotationCluster == mixedsort(levels(clustered_gs$AnnotationCluster))[i],]
  }
  
  # define Padj column names
  padj_cols <- c()
  for (i in seq_along(genesets)) {
    padj_cols <- c(padj_cols, paste0('Padj', i))
  }
  
  # subset Padj columns of clustered_gs into values
  values <- data.frame()
  values <- clustered_gs[, padj_cols]
  values$p_rank <- rowMeans(values, na.rm=TRUE)
  
  # order clusters by p_rank
  setorder(values, p_rank, na.last=TRUE)
  values <- select(values, -p_rank)
  
  # combine Term and Cluster with Padj values
  final_data <- cbind(x, values)
  final_data <- final_data[c(1:num_clusters), ]
  
  return(final_data)
}

make_heatmap <- function(final_data) {
  hmap <- final_data
  hmap$Cluster <- sub("^", "Cluster", hmap$Cluster) # prefix with 'Cluster'
  
  # rename 'Padj' columns to 'GS'
  new_cols <- c()
  for (i in seq_along(genesets)) {
    new_cols <- c(new_cols, paste0('GS', i))
  }
  colnames(hmap) <- c('Term', 'Cluster', new_cols)
  
  # melt the data for heatmap construction
  melted_data <- melt(hmap)
  
  # plot the heatmap
  p <- ggplot(melted_data, aes(variable, Term)) +
    geom_tile(aes(fill=value), colour='white') +
    labs(x='Genesets', y='Clusters', title='Adjusted p-value per cluster') +
    scale_y_discrete(labels=melted_data$Cluster) +
    scale_fill_gradient(low='red', high='green') +
    theme(axis.text=element_text(size=12))
  return(p)
}



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



