# Testing ComplexHeatmap for cluster row annotations

library(ComplexHeatmap)
library(InteractiveComplexHeatmap)

clusty_annot <- melted_chmap_data %>%
  distinct(Cluster, Term) %>%
  arrange(Cluster)

clusty <- cluster_hmap[, !(colnames(cluster_hmap) %in% c("Cluster", "Term"))]
clusty <- apply(clusty, c(1, 2), function(x) -log10(x))
clusty <- apply(clusty, 2, function(x) replace_na(x, 0))


col_fun <- colorRamp2(c(0, max(melted_chmap_data$value, na.rm=TRUE)), c("white", "purple"))
#col_fun(seq(-3, 3))

rownames(clusty) <- cluster_hmap$Term
la <- ComplexHeatmap::rowAnnotation(Cluster=clusty_annot$Cluster, name="Cluster", show_annotation_name=TRUE)
ch <- ComplexHeatmap::Heatmap(as.matrix(clusty), name="Test", col=col_fun, cluster_rows=FALSE, 
                              cluster_columns=FALSE, left_annotation=la)

ch <- draw(ch)
htShiny(ch)


# diff_la = rowAnnotation(Cluster=anno_empty(border = FALSE, 
#                                       width =  unit(60, "mm")))
# diff_ch <- ComplexHeatmap::Heatmap(as.matrix(clusty), name="Test", col=col_fun, cluster_rows=FALSE, 
#                                          cluster_columns=FALSE, left_annotation=diff_la)
# for(i in 1:length(clusty_annot$Cluster)) {
#   decorate_annotation("Cluster", slice = i, {
#     grid.rect(x = 0, width = unit(2, "mm"), gp = gpar(fill = i, col = NA), just = "left")
#     grid.text(paste(clusty_annot$Cluster[i], collapse = "\n"), x = unit(4, "mm"), just = "left")
#   })
# }



# Trying to get cluster annotation...
# g <- ggplot(melted_chmap_data, 
#             aes(x=variable, y=Term, fill=value)) +
#   geom_tile() + 
#   labs(title=paste0("-log10(", value_type, ") by Term"),
#        x="Geneset",
#        y="Term",
#        fill=paste0("-log10(", value_type, ")"))
# g2 <- ggplot(melted_chmap_data, 
#              aes(x=1, y=Term, fill=Cluster)) +
#   geom_tile() + 
#   labs(x="", y="Cluster", fill="Cluster") +
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank())
# g_final <- plot_grid(
#   g2, g,
#   ncol=2,
#   align = "hv"
#   #rel_widths = c(1, 1)
# )
# layout <- c(
#   area(t=1, l=1, b=4, r=10),
#   area(t=1, l=10, b=4, r=11)
# )
# g + g2 + plot_layout(design=layout) +
#   plot_annotation(title = paste0("-log10(", value_type, ") by Term"), 
#                   theme = theme(plot.title = element_text(hjust = 0.5))) +  
#   plot_layout(guides = "collect")
# 
#