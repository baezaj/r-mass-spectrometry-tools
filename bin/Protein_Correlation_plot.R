
# setup -------------------------------------------------------------------

library(tidyverse)
library(rio)
library(RColorBrewer) # color palette
library(viridisLite) # color palette
library(fastcluster)
library(dendextend)
library(dendsort)
library(pheatmap)



# data import -------------------------------------------------------------


# Protein quant data 
protein_quant <- import("")



# Protein correlation -----------------------------------------------------



data_cor_prot <- protein_quant %>%
  filter(devstage != "pool",
         !is.na(abundance)) %>%
  unite(id, devstage, sex, ivf, sample_id, sep = "_") %>% 
  select(id, protein_group, abundance) %>%
  spread(protein_group, abundance) %>%
  column_to_rownames("id")


# matrix transformation
cor_matrix_prot <- cor(data_cor_prot, use = "pairwise.complete.obs", method = "pearson")

# Scale Rows
dat.n <- t(cor_matrix_prot)
dat.tn <- t(dat.n)

# Distance
# Calculate distance between experiments in rows
d1 <- dist(dat.n, method = "euclidean", diag = FALSE, upper = FALSE)
# # Calculate distance between proteins in rows
# d2 <- dist(dat.tn, method = "euclidean", diag = FALSE, upper = TRUE)

any(is.na(d1)) # must be FALSE

# hclust
# Clustering distance between experiments using Ward linkage
h1 <- fastcluster::hclust(d1, method = "ward.D2", members = NULL)
# Clustering distance between proteins using Ward linkage
h2 <- fastcluster::hclust(d1, method = "ward.D2", members = NULL)


# Plotting dendrogram
plot(h2, cex = 0.1, hang = -1)
abline(h = 130, col = "red")
clusters <- cutree(h2, 8)
# dat.tn$cluster <- clusters

# merging protein clusters with protein names
prot_corr <- as.data.frame(cor_matrix_prot) %>%
  rownames_to_column("protein_group")
prot_corr$cluster <- clusters

prot_corr <- prot_corr %>%
  select(protein_group, cluster) %>%
  right_join(prot_list, .) %>%
  arrange(cluster)

export(prot_corr, file = "final_data/Protein_correlation_matrix_clusters.csv")

gc()
# Row colorings
nofclust.height <-  length(unique(as.vector(clusters)))
selcol2 <- colorRampPalette(brewer.pal(8, "Paired"))
clustcol.height <- selcol2(nofclust.height)

png(filename = "figures/Protein_correlation_MSstats_matrix.png", width = 12, height = 12, units = "in", res = 800)

# Heatmap
heatmap.2(dat.tn,
          
          # Dendrogram
          Colv = as.dendrogram(h1),
          Rowv = as.dendrogram(h2),
          dendrogram = "both",
          
          # Data scaling
          scale = "none",
          
          # Level trace
          trace = "none",
          
          # Colors
          col = colorRampPalette(brewer.pal(10, "RdBu"))(50),
          # col = colorRampPalette(viridis(100, direction = -1, option = "D"))(50),
          
          # Row & Column labeling
          labRow = FALSE, #rownames(dat.tn),
          labCol = FALSE,
          cexCol = 0.9,
          margins = c(10,12),
          
          # Color key & density info
          density.info = "histogram",
          na.color = "grey90",
          
          # Plot labels
          main = "Protein Correlations",
          
          # Row colors
          RowSideColors = clustcol.height[clusters]
)


# Legend
legend("right",
       legend = unique(as.vector(clusters)),
       col = clustcol.height,
       lty= 1,
       lwd = 5,
       cex=0.8,
       title = "Cluster"
)

dev.off()

rm(dat.n, dat.tn, d1, h1, h2, clustcol.height, nofclust.height, selcol2);gc()

