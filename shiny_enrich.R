library(devtools)
install_github("hurlab/richR")
install_github("guokai8/bioAnno")

library(richR)
library(bioAnno)


# deg2 <- read.delim('/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/degs/HF_12wk_SCN_vs_WT_12wk_SCN_DE_12_19.txt')

## build Annotation package
fromKEGG(species="hsa")  # from KEGG
fromAnnHub(species="human")  # from AnnotationHub

# x: deg to analyze
# header: name of vector in question
shiny_enrich <- function(x, header, anntype, keytype, ontology) {
  x <- x[, header]
  if (anntype == "GO") {
    x <- as.character(x)
    hsa_go <- buildAnnot(species="human", keytype=keytype, anntype=anntype)
    return(richGO(x=x, godata=hsa_go, ontology="BP"))
  } 
  else if (anntype == "KEGG") {
    hsa_ko <- buildAnnot(species="hsa", keytype=keytype, anntype=anntype)
    return(richKEGG(x=x, kodata=hsa_ko))
  }
}

# deg2_test <- shiny_enrich(deg2, 'geneID', "GO", "SYMBOL", "BP")
