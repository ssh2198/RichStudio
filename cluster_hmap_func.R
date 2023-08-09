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



# SAMPLE GENESETS
#gs1 <- read.delim("data/try-this/GO_HF12wk_vs_WT12wk.txt")
#gs2 <- read.delim("data/try-this/KEGG_HF36wk_vs_WT12wk.txt")
#gs3 <- read.delim("data/try-this/GO_WT36wk_vs_WT12wk.txt")

#genesets <- list(gs1, gs2, gs3)
#gs_names <- c("GO_HF12wk_vs_WT12wk.txt", "KEGG_HF36wk_vs_WT12wk.txt", "GO_WT36wk_vs_WT12wk.txt")

# to test functionality of the functions, run:
#genesets %>%
#  merge_genesets() %>%
#  cluster(cutoff=0.5, overlap=0.5, minSize=2) %>%
#  hmap_prepare() %>%
#  comprehensive_hmap(genesets=genesets, gs_names=gs_names)



# SAMPLE GENESETS (.CSV)
#go1 <- read.csv("data/rich-results/aug5-csv-richr/DRG _padj_GO_Aug-3way.csv", row.names=1)
#go2 <- read.csv("data/rich-results/aug5-csv-richr/SCN _padj_GO_Aug-3way.csv", row.names=1)
#go3 <- read.csv("data/rich-results/aug5-csv-richr/Shared _padj_GO_Aug-3way.csv", row.names=1)

#kegg1 <- read.csv("data/rich-results/aug5-csv-richr/DRG _padj_KEGG_Aug-3way.csv", row.names=1)
#kegg2 <- read.csv("data/rich-results/aug5-csv-richr/SCN _padj_KEGG_Aug-3way.csv", row.names=1)
#kegg3 <- read.csv("data/rich-results/aug5-csv-richr/Shared _padj_KEGG_Aug-3way.csv", row.names=1)

#genesets <- list(go3, kegg1, kegg2)
#gs_names <- c("Shared _padj_GO_Aug-3way.csv", "DRG _padj_KEGG_Aug-3way.cs", "SCN _padj_KEGG_Aug-3way.csv")


# this function merges a list of genesets together to prepare for clustering
merge_genesets <- function(genesets) {
  
  # suffix all non 'Term' columns with their index
  for (i in seq_along(genesets)) {
    rownames(genesets[[i]]) <- NULL
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
  # cluster from richR package
  clustered_gs <- richCluster(x=merged_gs, gene=TRUE, cutoff=.5, overlap=.5, minSize=2) # from richR
  
  # for debugging
  write.table(clustered_gs, file='/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/clustered_data.txt', sep='\t')
  
  # order clusters by ascending cluster #
  clustered_gs <- clustered_gs[order(as.numeric(clustered_gs$AnnotationCluster)), ]
  return(clustered_gs)
}


# return list of terms & corresponding pvalues for each cluster
cluster_list <- function(clustered_gs, merged_gs, genesets) {
  
  # get list of Annots in cluster, index=cluster#
  term_indices <- sapply(clustered_gs$Cluster, function(x) unlist(strsplit(x, ',')))
  
  # for each cluster #, find matching row in merged_gs corresponding to Annot
  cluster_list <- data.frame()
  for (i in seq_along(term_indices)) {
    annots <- term_indices[[i]] # get Annot string from term_indices
    matching_rows <- merged_gs[merged_gs$Annot %in% annots, ] # find matching rows
    matching_rows$Cluster <- i # get cluster #
    cluster_list <- rbind(cluster_list, matching_rows) # append matching rows
  }
  
  # define unnecessary columns (change later if you want)
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

cluster_list_new <- function(clustered_gs, merged_gs, gs_names) {
  
  # get list of Annots in cluster, index=cluster#
  term_indices <- sapply(clustered_gs$Cluster, function(x) unlist(strsplit(x, ',')))
  
  # for each cluster #, find matching row in merged_gs corresponding to Annot
  cluster_list <- data.frame()
  for (i in seq_along(term_indices)) {
    annots <- term_indices[[i]] # get Annot string from term_indices
    matching_rows <- merged_gs[merged_gs$Annot %in% annots, ] # find matching rows
    matching_rows$Cluster <- i # get cluster #
    cluster_list <- rbind(cluster_list, matching_rows) # append matching rows
  }
  
  # define wanted columns
  og_pval_cols <- grep("^Pvalue\\d+$", colnames(cluster_list), value=TRUE)
  pval_cols <- gsub("\\d+$", "", og_pval_cols) # get rid of number
  pval_cols <- paste(pval_cols, gs_names, sep="_") # concat gs name to end
  
  og_padj_cols <- grep("^Padj\\d+$", colnames(cluster_list), value=TRUE)
  padj_cols <- gsub("\\d+$", "", og_padj_cols)
  padj_cols <- paste(padj_cols, gs_names, sep="_")
  
  og_geneid_cols <- grep("^GeneID\\d+$", colnames(cluster_list), value=TRUE)
  geneid_cols <- gsub("\\d+$", "", og_geneid_cols)
  geneid_cols <- paste(geneid_cols, gs_names, sep="_")
  
  og_cols_to_subset <- c(og_pval_cols, og_padj_cols, og_geneid_cols)
  cols_to_subset <- c(pval_cols, padj_cols, geneid_cols)
  
  # create cluster_list
  cluster_list <- cluster_list[, c("Cluster", "Term", "Annot", og_cols_to_subset)]
  colnames(cluster_list) <- c("Cluster", "Term", "Annot", cols_to_subset)
  
  # order columns of cluster_list by geneset
  # todo later
}


# prepares the rich cluster result for heatmap
hmap_prepare <- function(clustered_gs, gs_names) {
  
  # create tibble with Term and AnnotationCluster
  x <- as_tibble(cbind(clustered_gs$Term, clustered_gs$AnnotationCluster))
  colnames(x) <- c('Term', 'Cluster')
  
  # order clusters numerically
  x <- x[order(as.numeric(x$Cluster)), ]
  
  # subset Padj columns of clustered_gs into new df values
  padj_cols <- grep("^Padj\\d+$", colnames(clustered_gs), value=TRUE)
  values <- data.frame()
  values <- clustered_gs[, padj_cols]
  colnames(values) <- gs_names  # change Padj colnames to file name
  
  # order clusters by p_rank (mean Padj)
  values$p_rank <- rowMeans(values, na.rm=TRUE)
  setorder(values, p_rank, na.last=TRUE)
  values <- dplyr::select(values, -p_rank)
  
  # combine Term and Cluster with Padj values
  final_data <- data.frame()
  final_data <- cbind(x, values)
  
  return(final_data)
}


# CHANGE COLNAME LATER
# returns the heatmap
comprehensive_hmap <- function(final_data, as_plotly=TRUE) {
  
  # melt the data for heatmap construction
  melted_data <- reshape2::melt(hmap)
  
  # plot the heatmap
  if (as_plotly == FALSE) {
    p <- ggplot(melted_data, aes(variable, Term)) +
      geom_tile(aes(fill=value), colour='white') +
      labs(x='Genesets', y='Clusters', title='Adjusted p-value per cluster') +
      scale_y_discrete(labels=melted_data$Term) +
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

# testing
# term_vec <- c("blood vessel morphogenesis", "myelination", "cell migration", "regulation of protein phosphorylation")
cluster_hmap <- function(cluster_list, term_vec, final_data) {
  
  # find cluster associated with term
  term_name_df <- final_data[final_data$Term %in% term_vec, ]
  term_nums <- term_name_df$Cluster # list of cluster #s
  
  cluster_hmap <- data.frame()
  for (i in seq_along(term_nums)) {
    tmp <- cluster_list[cluster_list$Cluster == term_nums[i], ]
    cluster_hmap <- rbind(cluster_hmap, tmp)
  }
  
  # subset Padj columns
  padj_cols <- grep("^Padj", colnames(cluster_hmap), value=TRUE)
  cluster_hmap <- cluster_hmap[, c("Term", padj_cols)]
  # remove "Padj_" from colnames
  colnames(cluster_hmap) <- gsub("^Padj_", "", colnames(cluster_hmap))
  
  # melt the data for heatmap construction
  melted_chmap_data <- reshape2::melt(cluster_hmap)
  
  # plot the heatmap
  if (as_plotly == FALSE) {
    p <- ggplot(melted_chmap_data, aes(variable, Term)) +
      geom_tile(aes(fill=value), colour='white') +
      labs(x='Genesets', y='Clusters', title='Adjusted p-value per cluster') +
      scale_y_discrete(labels=melted_data$Term) +
      scale_fill_gradient(low='red', high='green') +
      theme(axis.text=element_text(size=12))
    return(p)
  }
  else if (as_plotly == TRUE) {
    plotly_p <- plot_ly (
      data = melted_chmap_data,
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