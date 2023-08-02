library(tidyverse)
library(gtools)
library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)

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



# sample genesets to try
gs1 <- read.delim("data/try-this/GO_HF12wk_vs_WT12wk.txt")
gs2 <- read.delim("data/try-this/KEGG_HF36wk_vs_WT12wk.txt")
gs3 <- read.delim("data/try-this/GO_WT36wk_vs_WT12wk.txt")

genesets <- list(gs1, gs2, gs3)

# to test functionality of the functions, run

#genesets %>%
#  merge_genesets() %>%
#  cluster(cutoff=0.5, overlap=0.5, minSize=2) %>%
#  hmap_prepare() %>%
#  make_heatmap(genesets=genesets)


# this function merges a list of genesets together to prepare for clustering
merge_genesets <- function(genesets) {
  
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
  geneid_cols <- c()
  for (i in seq_along(genesets)) {
    geneid_cols <- c(geneid_cols, paste0('GeneID', i))
  }
  merged_gs$GeneID <- apply(merged_gs[, geneid_cols], 1, function (x) paste(unique(c(x)), collapse=','))
  # catch NA
  merged_gs$GeneID <- sapply(merged_gs$GeneID, function(x) gsub('NA,', '', x))
  # catch NA at the end
  merged_gs$GeneID <- sapply(merged_gs$GeneID, function(x) gsub(',NA$', '', x, perl=TRUE))
  
  # combine unique 'Annot' elements
  annot_cols <- c()
  for (i in seq_along(genesets)) {
    annot_cols <- c(annot_cols, paste0('Annot', i))
  }
  merged_gs$Annot <- apply(merged_gs[, annot_cols], 1, function(x) paste(unique(c(x)), collapse=',', recycle0=TRUE))
  # catch NA
  merged_gs$Annot <- sapply(merged_gs$Annot, function(x) gsub('NA,', '', x))
  # catch NA at the end
  merged_gs$Annot <- sapply(merged_gs$Annot, function(x) gsub(',NA$', '', x, perl=TRUE))
  merged_gs <- dplyr::select(merged_gs, -annot_cols)
  
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

# clusters the merged geneset
cluster <- function(merged_gs, cutoff, overlap, minSize) {
  clustered_gs <- richCluster(x=merged_gs, gene=TRUE, cutoff=cutoff, overlap=overlap, minSize=minSize) # from richR
  write.table(clustered_gs, file='/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/clustered_data.txt', sep='\t')
  return(clustered_gs)
}

# return list of terms & corresponding pvalues for each cluster
cluster_list <- function(clustered_gs, merged_gs, genesets) {
  
  # order clusters numerically
  x <- clustered_gs[order(as.numeric(clustered_gs$AnnotationCluster)), ]
  term_indices <- sapply(x$Cluster, function(x) unlist(strsplit(x, ',')))
  
  cluster_list <- data.frame()
  for (i in seq_along(term_indices)) {
    annots <- term_indices[[i]] # get Annot string from term_indices
    matching_rows <- merged_gs[merged_gs$Annot %in% annots, ]
    matching_rows$Cluster <- i
    cluster_list <- rbind(cluster_list, matching_rows)
  }
  
  # define unnecessary columns
  cols_to_filter <- c('Term', 'Annot')
  for (i in seq_along(genesets)) {
    cols_to_filter <- c(cols_to_filter, paste0('Pvalue', i))
    cols_to_filter <- c(cols_to_filter, paste0('Padj', i))
    cols_to_filter <- c(cols_to_filter, paste0('GeneID', i))
  }
  # filter cluster_list
  cluster_list <- cluster_list[, c('Cluster', cols_to_filter)]
  return(cluster_list)
}

# prepares the rich cluster result for heatmap
hmap_prepare <- function(clustered_gs) {
  
  # create tibble with Term and AnnotationCluster
  x <- as_tibble(cbind(clustered_gs$Term, clustered_gs$AnnotationCluster))
  colnames(x) <- c('Term', 'Cluster')
  
  # order clusters numerically
  x <- x[order(as.numeric(x$Cluster)), ]
  
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
  values <- dplyr::select(values, -p_rank)
  
  # combine Term and Cluster with Padj values
  final_data <- cbind(x, values)
  
  return(final_data)
}

# returns the heatmap
make_heatmap <- function(final_data, genesets, as_plotly=FALSE) {
  hmap <- final_data
  hmap$Cluster <- sub("^", "Cluster", hmap$Cluster) # prefix with 'Cluster'
  
  # rename 'Padj' columns to 'GS'
  new_cols <- c()
  for (i in seq_along(genesets)) {
    new_cols <- c(new_cols, paste0('GS', i))
  }
  colnames(hmap) <- c('Term', 'Cluster', new_cols)
  
  # melt the data for heatmap construction
  melted_data <- reshape2::melt(hmap)
  
  # plot the heatmap
  if (as_plotly == FALSE) {
    p <- ggplot(melted_data, aes(variable, Term)) +
      geom_tile(aes(fill=value), colour='white') +
      labs(x='Genesets', y='Clusters', title='Adjusted p-value per cluster') +
      scale_y_discrete(labels=melted_data$Cluster) +
      scale_fill_gradient(low='red', high='green') +
      theme(axis.text=element_text(size=12))
    return(p)
  }
  else if (as_plotly == TRUE) {
    plotly_p <- plot_ly (
      data = melted_data,
      x = ~variable,
      y = ~Term,
      z = ~value,
      type = "heatmap",
      colors = c('red', 'green'),
      text = ~value,
      hoverinfo = "text"
    )
    return(plotly_p)
  }
}