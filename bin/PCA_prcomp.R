
# PCA ---------------------------------------------------------------------


# PCA if using light and heavy proteins
pca_data <- proteinquants %>%
  filter(abundance > 0)%>%
  select(sample_id, run_order, ProteinAccession, GeneName, abundance) %>%
  unite(id, ProteinAccession, GeneName, sep = "-") %>%
  unite(id2, sample_id, run_order, sep = "_") %>%
  spread(id2, abundance) %>%
  column_to_rownames("id")

# Removing missing values from the matrix
pca_data <- t(na.omit(pca_data))

# Running the PCA
pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# PC components variation
percent_variation <- pca_res$sdev^2 / sum(pca_res$sdev^2)


# df for plotting PCA
pca_plot <- data.frame(pca_res$x) %>%
  rownames_to_column("id2") %>%
  separate(id2, c("sample_id", "run_order"),
           sep = "_", remove = FALSE) %>%
  separate(sample_id, c("treatment", "biorep"), remove = FALSE, convert = TRUE)


# df from plotting variance
pca_df <- as.data.frame(summary(pca_res)$importance)
pca_df <- pca_df %>%
  mutate(condition = rownames(.)) %>%
  gather(temp, value, 1:(ncol(.)-1))
pca_df$number <- as.numeric(substring(pca_df$temp,
                                      first = 3,
                                      last = nchar(pca_df$temp)))

# PCA plot
pca_plot %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = condition), size = 5) +
  theme_bw(base_size = 12) +
  labs(title = "Principle Components Analysis",
       x = paste0("Principal component 1 (",  round(percent_variation[1], digits = 2) * 100, "%)"),
       y = paste0("Principal component 2 (",  round(percent_variation[2], digits = 2) * 100, "%)")) +
  coord_fixed()

