


library(tidyverse)
library(rio)
library(RColorBrewer)
library(tidytext) # using with PCA
library(irlba) # for sparsed matrix PCA





data_tidy <- import("processed_data/01_out_Peptide_abundance_tidy.csv", setclass = "tibble")



# PCA ---------------------------------------------------------------------


# PCA if using light and heavy proteins
pca_data_irlba <- data_tidy %>%
         filter(!is.na(abundance_norm)) %>% 
  select(master_protein_accessions, tissue, time, isotope, biorep, abundance_norm) %>%
  unite(id, tissue, time, isotope, biorep, sep = "_")

set.seed(1223)
# Cast Sparse data
sparse_data <- pca_data_irlba %>%
  cast_sparse(row = id, column = master_protein_accessions, value = abundance_norm)

# Irlba pca
pca_irlba <- prcomp_irlba(sparse_data, n = 11, retx = TRUE, center = TRUE, scale. = TRUE)

# dataframe for PCA data
pca_plot_irlba <- data.frame(id = sparse_data@Dimnames[[1]],
                             pca_irlba$x) %>%
  separate(id, c("tissue", "time", "isotope", "biorep"), sep = "_", remove = FALSE) %>%
  unite(condition, time, tissue, sep = "_", remove = FALSE) %>%
  mutate(biorep = as.numeric(biorep))

pca_plot_irlba$condition[pca_plot_irlba$condition == "8_liver"] <- "08_liver"
pca_plot_irlba$condition[pca_plot_irlba$condition == "8_heart"] <- "08_heart"

# PC components variation
percent_variation <- pca_irlba$sdev^2 / sum(pca_irlba$sdev^2)

# PCA plot
ggplot(pca_plot_irlba, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = condition, shape = tissue), size = 5) +
  theme_bw(base_size = 12) +
  scale_shape_manual(values = c(15, 17)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  labs(title = "Principle Components Analysis",
       x = paste0("Principal component 1 (",  round(percent_variation[1], digits = 2) * 100, "%)"),
       y = paste0("Principal component 2 (",  round(percent_variation[2], digits = 2) * 100, "%)")) +
  expand_limits(x = c(-40, 40), y = c(-40, 40)) +
  coord_fixed()
ggsave(filename = "figures/PCA_Heart_Liver_timepoints.pdf")

