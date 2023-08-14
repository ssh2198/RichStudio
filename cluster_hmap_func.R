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
gs1 <- read.delim("data/try-this/GO_HF12wk_vs_WT12wk.txt")
gs2 <- read.delim("data/try-this/KEGG_HF36wk_vs_WT12wk.txt")
gs3 <- read.delim("data/try-this/GO_WT36wk_vs_WT12wk.txt")

genesets <- list(gs1, gs2, gs3)
names(genesets) <- c("GO_HF12wk_vs_WT12wk.txt", "KEGG_HF36wk_vs_WT12wk.txt", "GO_WT36wk_vs_WT12wk.txt")
gs_names <- c("GO_HF12wk_vs_WT12wk.txt", "KEGG_HF36wk_vs_WT12wk.txt", "GO_WT36wk_vs_WT12wk.txt")

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
    rownames(genesets[[i]]) <- NULL # prevents rownames from messing up
    non_term_cols <- colnames(genesets[[i]])
    non_term_cols[-which(non_term_cols == 'Term')] <- paste(non_term_cols[-which(non_term_cols == 'Term')], names(genesets[i]), sep="_")
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
    geneid_cols <- c(geneid_cols, paste('GeneID', names(genesets[i]), sep="_"))
  }
  merged_gs$GeneID <- apply(merged_gs[, geneid_cols], 1, function (x) paste(unique(c(x)), collapse=','))
  # catch NA
  merged_gs$GeneID <- sapply(merged_gs$GeneID, function(x) gsub('NA,', '', x))
  # catch NA at the end
  merged_gs$GeneID <- sapply(merged_gs$GeneID, function(x) gsub(',NA$', '', x, perl=TRUE))
  
  # combine unique 'Annot' elements
  annot_cols <- c()
  for (i in seq_along(genesets)) {
    annot_cols <- c(annot_cols, paste('Annot', names(genesets[i]), sep="_"))
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
    pval_cols <- c(pval_cols, paste('Pvalue', names(genesets[i]), sep="_"))
  }
  merged_gs$Pvalue <- rowMeans(merged_gs[, pval_cols], na.rm=TRUE)
  
  # average Padj
  padj_cols <- c()
  for (i in seq_along(genesets)) {
    padj_cols <- c(padj_cols, paste('Padj', names(genesets[i]), sep="_"))
  }
  merged_gs$Padj <- rowMeans(merged_gs[, padj_cols], na.rm=TRUE)
  
  #return
  return(merged_gs)
  
}

# clusters the merged geneset
cluster <- function(merged_gs, cutoff, overlap, minSize) {
  # cluster from richR package
  # gene=TRUE returns list of terms within cluster under 'Cluster' column
  clustered_gs <- richCluster(x=merged_gs, gene=TRUE, cutoff=cutoff, overlap=overlap, minSize=minSize) # from richR
  
  # for debugging
  write.table(clustered_gs, file='/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/clustered_data.txt', sep='\t')
  
  # order clusters by ascending cluster #
  clustered_gs <- clustered_gs[order(as.numeric(clustered_gs$AnnotationCluster)), ]
  return(clustered_gs)
}


# return list of terms & corresponding pvalues, padjs, geneIDs for each cluster
get_cluster_list <- function(clustered_gs, merged_gs, gs_names) {
  
  # get list of Annots in cluster, index=cluster#
  term_indices <- sapply(clustered_gs$Cluster, function(x) unlist(strsplit(x, ',')))
  
  # for each cluster #, find matching row in merged_gs corresponding to Annot
  cluster_list <- data.frame()
  for (i in seq_along(term_indices)) {
    annots <- term_indices[[i]] # get Annot string from term_indices
    matching_rows <- merged_gs[merged_gs$Annot %in% annots, ] # find matching rows
    matching_rows$Cluster <- i # set cluster #
    cluster_list <- rbind(cluster_list, matching_rows) # append matching rows
  }
  
  # Create a vector of column names to subset
  selected_cols <- c("Cluster", "Term", "Annot")
  
  # Append the Pvalue, Padj, and GeneID columns for each file name
  selected_cols <- c(selected_cols,
                     paste0("Pvalue_", gs_names),
                     paste0("Padj_", gs_names),
                     paste0("GeneID_", gs_names))
  
  # Subset the dataframe based on the selected columns
  cluster_list <- cluster_list[, selected_cols]
  return(cluster_list)
}


# prepares the rich cluster result for heatmap
hmap_prepare <- function(clustered_gs, gs_names) {
  
  # create tibble with Term and AnnotationCluster
  x <- as_tibble(cbind(clustered_gs$AnnotationCluster, clustered_gs$Term))
  colnames(x) <- c('Cluster', 'Representative_Term')
  
  # order clusters numerically
  x <- x[order(as.numeric(x$Cluster)), ]
  
  # subset Padj columns of clustered_gs into new df values
  value_cols <- c(paste0("Pvalue_", gs_names), paste0("Padj_", gs_names))
  values <- clustered_gs[, value_cols]
  
  # combine Term and Cluster with Padj values
  final_data <- data.frame()
  final_data <- cbind(x, values)
  
  return(final_data)
}


change_finaldata_valueby <- function(final_data, cluster_list, value_by="mean") {
  
  new_final_data <- final_data
  value_cols <- c(grep(paste0("^", "Pvalue"), colnames(cluster_list), value=TRUE), 
                  grep(paste0("^", "Padj"), colnames(cluster_list), value=TRUE))
  
  for (i in seq_along(final_data$Cluster)) {
    x <- filter(cluster_list, Cluster == i) # subsetted df containing all terms in cluster
    
    # find mean/med/min/max of pvalue and padjs
    if (value_by == "mean") {
      x_values <- colMeans(x[, c(value_cols)], na.rm = TRUE)
    } else if (value_by == "median") {
      x_values <- sapply(x[, c(value_cols)], median, na.rm = TRUE)
    } else if (value_by == "min") {
      x_values <- sapply(x[, c(value_cols)], function(col) suppressWarnings(min(col, na.rm = TRUE)))
    } else if (value_by == "max") {
      x_values <- sapply(x[, c(value_cols)], function(col) suppressWarnings(max(col, na.rm = TRUE)))
    }
    x_values[!is.finite(x_values)] <- NA
    
    final_x <- new_final_data[new_final_data$Cluster==i, ] # specific row in final_data
    final_x[, c(value_cols)] <- x_values 
    
    new_final_data[new_final_data$Cluster==i, ] <- final_x
  }
  
  return(new_final_data)
  
}


# returns the heatmap
# value_type = type of value to display in heatmap
# value_by = how value is calculated
comprehensive_hmap <- function(final_data, cluster_list, as_plotly=TRUE, value_type="Padj", value_by="mean") {
  
  # change value based on value_by
  new_final_data <- change_finaldata_valueby(final_data=final_data, cluster_list=cluster_list, value_by=value_by)
  
  # grab either Pvalue or Padj
  value_cols <- grep(paste0("^", value_type), colnames(new_final_data), value=TRUE)
  
  # subset new_final_data by value_type
  subsetted_final_data <- new_final_data[, c("Cluster", "Representative_Term", value_cols)]
  
  # remove "Pvalue" or "Padj" from colnames
  colnames(subsetted_final_data) <- gsub(paste0("^", value_type, "_"), "", colnames(subsetted_final_data))
  
  # melt the data for heatmap construction
  melted_data <- reshape2::melt(subsetted_final_data)
  
  # plot the heatmap
  if (as_plotly == FALSE) {
    p <- ggplot(melted_data, aes(variable, Representative_Term)) +
      geom_tile(aes(fill=value), colour='white') +
      labs(x='Genesets', y='Representative Term for Cluster', title=paste0(value_type, " by Cluster"), fill=value_type) +
      scale_y_discrete(labels=melted_data$Representative_Term) +
      scale_fill_gradient(low='red', high='green') +
      theme(axis.text=element_text(size=12))
    return(p)
  }
  else if (as_plotly == TRUE) {
    plotly_p <- plot_ly (
      data = melted_data,
      x = ~variable,
      y = ~Representative_Term,
      z = ~value,
      type = "heatmap",
      colors = c('red', 'green'),
      text = ~value,
      colorbar = list(title = value_type),
      hoverinfo = "text"
    ) %>%
      layout (
        title = paste0(value_type, " by Cluster"),
        xaxis = list(title = 'Geneset'), 
        yaxis = list(title = 'Representative Term for Cluster')
      )
    return(plotly_p)
  }
}

# testing
# term_vec <- c("transport", "monocarboxylic acid biosynthetic process", "isoprenoid metabolic process", "acetyl-CoA metabolic process")
cluster_hmap <- function(cluster_list, term_vec, final_data, value_type="Padj", as_plotly=TRUE) {
  
  # find cluster associated with term
  term_name_df <- final_data[final_data$Representative_Term %in% term_vec, ]
  term_nums <- term_name_df$Cluster # list of cluster #s
  
  cluster_hmap <- data.frame()
  for (i in seq_along(term_nums)) {
    tmp <- cluster_list[cluster_list$Cluster == term_nums[i], ]
    cluster_hmap <- rbind(cluster_hmap, tmp)
  }
  
  # subset value_type columns
  value_cols <- grep(paste0("^", value_type), colnames(cluster_hmap), value=TRUE)
  cluster_hmap <- cluster_hmap[, c("Term", value_cols)]
  # remove "Padj_" from colnames
  colnames(cluster_hmap) <- gsub(paste0("^", value_type, "_"), "", colnames(cluster_hmap))
  
  # melt the data for heatmap construction
  melted_chmap_data <- reshape2::melt(cluster_hmap)
  
  # plot the heatmap
  if (as_plotly == FALSE) {
    p <- ggplot(melted_chmap_data, aes(variable, Term)) +
      geom_tile(aes(fill=value), colour='white') +
      labs(x='Genesets', y='Term', title=paste0(value_type, " by Term"), fill=value_type) +
      scale_y_discrete(labels=melted_chmap_data$Term) +
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
      colorbar = list(title = value_type),
      hoverinfo = "text"
    ) %>%
      layout (
        title = paste0(value_type, " by Term"),
        xaxis = list(title = 'Geneset'), 
        yaxis = list(title = 'Term')
      )
    return(plotly_p)
  }
  
}