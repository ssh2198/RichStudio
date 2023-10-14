# Functions to create integrative heatmaps from multiple enrichment results


# Helper function to append geneset to combined dataframe
append_df <- function(custom_data, gs) {
  # Get the names of columns in each data frame
  colnames_cd <- colnames(custom_data)
  colnames_gs <- colnames(gs)
  
  # Identify common and unique columns
  common_cols <- intersect(colnames_cd, colnames_gs)
  cd_only_cols <- setdiff(colnames_cd, colnames_gs)
  
  # Add missing columns to gs
  gs[ , cd_only_cols] <- NA
  
  custom_data[colnames_cd]
  gs[colnames_cd]
  
  # Combine the data frames by rows
  combined_df <- rbind(custom_data, gs)
  return(combined_df)
}


# Add geneset to combined dataframe
# gs_name <- "GO_HF12wk_vs_WT12wk.txt"
# gs <- gs1
# term_vec <- c("acyl-CoA biosynthetic process", "single-organism cellular process", "positive regulation of positive chemotaxis")
# 
# gs_name <- "KEGG_HF36wk_vs_WT12wk.txt"
# gs <- gs2
# term_vec <- c("Steroid biosynthesis", "Terpenoid backbone biosynthesis")
# term_vec <- c("Metabolic pathways", "Lysosome")
add_gs <- function(custom_data=NULL, gs, gs_name, term_vec) {
  
  # Subset gs by rows with gs$Term in term_vec
  gs <- gs[which(gs$Term %in% term_vec), ]
  
  # Append filename to all colnames except "Annot" and "Term"
  exclude_cols <- c("Annot", "Term")
  colnames(gs) <- ifelse(colnames(gs) %in% exclude_cols, colnames(gs), 
                         paste(colnames(gs), gs_name, sep="_"))
  
  # If custom_data empty, set it to gs
  if (nrow(custom_data) == 0) {
    custom_data <- gs
  } else {
    # Match anything + _ + (gs_name)
    gsname_cols <-  c(grep(paste0(".*_", gs_name), colnames(custom_data), value=TRUE))
    
    # If gs not previously added
    if (length(gsname_cols) == 0) {
      custom_data <- base::merge(custom_data, gs, by=c('Annot', 'Term'), all=TRUE)
    } 
    # Else, gs previously added
    else { 
      custom_data <- append_df(custom_data, gs)
    }
  }
  
  # Merge GeneID cols
  geneid_cols <- c(grep(paste0("^", "GeneID", "_"), colnames(custom_data), value=TRUE))
  tmp <- custom_data[, geneid_cols]
  if (is.null(dim(tmp))) {
    names(tmp) <- c("GeneID")
    custom_data$GeneID <- tmp
  } else {
    custom_data$GeneID <- apply(custom_data[, geneid_cols], 1, function (x) paste(unique(c(x)), collapse=','))
    # Catch NA
    custom_data$GeneID <- sapply(custom_data$GeneID, function(x) gsub('NA,', '', x))
    # Catch NA at the end
    custom_data$GeneID <- sapply(custom_data$GeneID, function(x) gsub(',NA$', '', x, perl=TRUE))
  }
  return(custom_data)
}


# Remove selected terms/geneset from combined dataframe
# term_vec <- c("Steroid biosynthesis", "Lysosome")
# delete_all <- c("Steroid biosynthesis", "Lysosome", "Toxoplasmosis", "Prion diseases", "Terpenoid backbone biosynthesis", "Metabolic pathways")
remove_gs <- function(custom_data, gs_name, term_vec, delete_all=NULL) {
  # delete entire geneset if passed term_vec arg "delete_all" w/ all terms
  if (!is.null(delete_all)) {
    # match anything + _ + (gs_name)
    gs_cols <- c(grep(paste0(".*_", gs_name), colnames(custom_data), value=TRUE))
    # filter out gs columns and term rows
    custom_data2 <- custom_data[, -which(colnames(custom_data) %in% gs_cols)]
    custom_data2 <- custom_data2[-which(custom_data2$Term %in% delete_all), ]
    if (nrow(custom_data2) == 0) {
      custom_data2 <- data.frame()
    }
  } 
  # else only delete selected terms
  else {
    # subset custom_data by terms to delete
    custom_data2 <- custom_data[-which(custom_data$Term %in% term_vec), ]
  }
  return(custom_data2)
}


# Create heatmap from combined dataframe
# value_type <- "Padj"
custom_hmap <- function(custom_data, value_type) {
  # Grab either Pvalue or Padj based on value_type
  value_cols <- c(grep(paste0("^", value_type, "_"), colnames(custom_data), value=TRUE))
  # Subset new_final_data by value_type
  subsetted_final_data <- custom_data[, c("Annot", "Term", "GeneID", value_cols)]
  
  # Remove "Pvalue" or "Padj" from colnames
  colnames(subsetted_final_data) <- gsub(paste0("^", value_type, "_"), "", colnames(subsetted_final_data))
  
  melted_custom_data <- reshape2::melt(subsetted_final_data, id.vars = c("Term", "Annot", "GeneID"))
  
  melted_custom_data$value <- as.numeric(melted_custom_data$value)
  melted_custom_data$value <- -log10(melted_custom_data$value)
  melted_custom_data$value <- round(melted_custom_data$value, 4)
  
  my_nticks <- length(unique(melted_custom_data$Term))
  
  # Plot the heatmap
  p <- plot_ly (
    data = melted_custom_data,
    x = ~variable,
    y = ~Term,
    z = ~value,
    type = "heatmap",
    colors = c('green', 'red'),
    text = ~paste0(Term, "<br>", "-log10(", value_type, "): ", value, "<br>"),  # Customize hover text
    colorbar = list(title = paste0("-log10(", value_type, ")")),
    hoverinfo = "text"
  ) %>%
    layout (
      title = paste0("-log10(", value_type, ") by Term"),
      xaxis = list(title = 'Geneset'), 
      yaxis = list(title = 'Term', categoryorder = "trace", nticks = my_nticks)
    )
  return(p)
}
