# deg1 <- read.delim("data/degs/HF_12wk_DRG_vs_WT_12wk_DRG_DE_12_19.txt")
# deg1_enriched <- shiny_enrich(deg1, 'geneID', 'mouse',  "GO", "SYMBOL", "BP")
# net <- ggnetwork(deg1_enriched, deg1)

rr_network <- function(rr, deg) {
  return(ggnetwork(rr))
}

# net1 <- ggnetwork(deg1_enriched, weightcut=.2)
# net2 <- ggnetwork(deg1_enriched, weightcut=.5)