
# libraries ---------------------------------------------------------------

library(tidyverse)
library(rio)
library(enrichR)
library(glue)


# data import -------------------------------------------------------------

prot_cluster <- import("intermediate_data/Protein_PCA_loadings_w_clusters.csv")


# setting up enrichR ------------------------------------------------------


listEnrichrSites()


# databases
dbs_list <- listEnrichrDbs()

# database libraries to query
database_list <-  c("BioPlanet_2019",
                    "BioCarta_2016",
                    "CORUM",
                    "Elsevier_Pathway_Collection",
                    "GO_Biological_Process_2018",
                    "GO_Cellular_Component_2018",
                    "GO_Molecular_Function_2018",
                    "Jensen_COMPARTMENTS",
                    "KEGG_2019_Mouse",
                    "WikiPathways_2019_Mouse")



# Function ----------------------------------------------------------------

run_enrichr <- function(data, dbs = NULL){
  
  # Extracting the gene list
  gene_list <- data$genes  
  
  # Running EnrichR
  enrichr_df <- enrichr(gene_list, dbs)
  
  # Adding the library name
  enrichr_df <- map_df(enrichr_df, ~as.data.frame(.x), .id = "db_library")
  
}



# Running function --------------------------------------------------------


cluster_nest <- prot_cluster %>% 
  group_by(cluster) %>% 
  nest() %>% 
  mutate(go = map(data, run_enrichr, dbs = database_list)) %>% 
  select(-data) %>% 
  unnest(go)

cluster_nest <- cluster_nest %>% 
  separate(Overlap, into = c("gene_count", "gene_total"), sep = "/", convert = TRUE, remove = FALSE) %>% 
  mutate(ratio = gene_count / gene_total)





make_plots <- function(data, db_library, cluster){
  data %>% 
    ggplot() +
    geom_bar(aes(x = reorder(Term, -Adjusted.P.value), y = -log10(Adjusted.P.value)), stat = "identity") +
    coord_flip() +
    theme_light(base_size = 10) +
    labs(title = paste0("database library:", db_library),
         subtitle = paste0("cluster: ", cluster)) +
    scale_x_discrete(labels = scales::label_wrap(50))
}

clust_trim <- cluster_nest %>% 
  arrange(Adjusted.P.value) %>% 
  group_by(cluster, db_library) %>% 
  slice(1:10) %>% 
  ungroup()

clust_trim <- clust_trim %>% 
  # filter(cluster == 6) %>% 
  group_by(cluster, db_library) %>% 
  nest() %>% 
  mutate(plots = pmap(list(data, db_library, cluster), make_plots))



pdf(file = "figures/enrichR_analysis_clusters.pdf")
print(clust_trim$plots)
dev.off()