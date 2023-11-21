# Functions to create uploaded file dataframe

df <- data.frame() # for testing
new_df <- read.delim("data/degs/HF_12wk_DRG_vs_WT_12wk_DRG_DE_12_19.txt")
name <- "HF_12wk_DRG_vs_WT_12wk_DRG_DE_12_19.txt"
df <- add_file_degdf(df, name, new_df)

add_file_degdf <- function(df, name, new_df) {
  possible_geneID <- c("GeneID", "gene_id", "gene", "gene list") # Possible "GeneID" colnames
  geneIDmatches <- grep(paste(possible_geneID, collapse="|"), colnames(new_df), ignore.case=TRUE)
  
  if (length(names(new_df)) > 1) {
    expr_data <- "True"
  } else {
    expr_data <- "False"
  }
  
  # If one GeneID column matched
  if (length(geneIDmatches) == 1) {
    geneIDcol <-  names(new_df[geneIDmatches])
    new_file_vec <- c(name=name, GeneID_header=geneIDcol, has_expr_data=expr_data)
    if (!is.null(df)) { # rbind if df already exists
      df <- rbind(df, new_file_vec)
      names(df) <- c("name", "GeneID_header", "has_expr_data")
    } else { # else set df to the new file vector
      df <- new_file_vec
    }
    return(df) # Return appended df on success
  } else {
    return(geneIDmatches) # Return list of possible geneID col matches if multiple/none
  }
}

rm_file_degdf <- function(df, df_name_to_rm) {
  if (!is.atomic(df)) {
    df <- df[df$name != df_name_to_rm]
  } else {
    df <- NULL
  }
  return(df)
}