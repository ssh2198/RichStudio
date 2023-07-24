library(devtools)
install_github("hurlab/richR")
install_github("guokai8/bioAnno")

library(richR)
library(bioAnno)


deg1 <- read.delim('/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/degs/HF_12wk_DRG_vs_WT_12wk_DRG_DE_12_19.txt')
deg2 <- read.delim('/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/degs/HF_12wk_SCN_vs_WT_12wk_SCN_DE_12_19.txt')
deg3 <- read.delim('/Users/sarahhong/Desktop/Hur Lab/enrichment-analysis/data/degs/HF_36wk_DRG_vs_HF_12wk_DRG_DE_12_19.txt')

## build Annotation package
fromKEGG(species="hsa")  # from KEGG
fromAnnHub(species="human")  # from AnnotationHub

shiny_enrich <- function(x, species, keytype, anntype, ontology) {
  if (anntype == "GO") {
    x$geneID <- as.character(x$geneID)
    hsa_go <- buildAnnot(species="human", keytype=keytype, anntype=anntype)
    return(richGO(x=x, godata=hsa_go, ontology="BP"))
  } 
  else if (anntype == "KEGG") {
    hsa_ko <- buildAnnot(species=species, keytype=keytype, anntype=anntype)
    return(richKEGG(x=x, kodata=hsa_ko))
  }
}

deg1_test <- shiny_enrich(deg2, "human", "GO", "BP")
