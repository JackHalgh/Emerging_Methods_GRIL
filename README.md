# Emerging_Methods_GRIL


```
#By Jack A. Greenhalgh, Sept 2025.
#Department of Biology, McGill University, 1205 Dr Penfield Ave, Montreal, Quebec, H3A 1B1, Canada
#############################################
# FULL WORKFLOW FOR SURVEY DATA ANALYSIS WITH ICC
#############################################

library(tidyverse)
library(janitor)
library(pheatmap)
library(reshape2)
library(FactoMineR)
library(factoextra)
library(irr) 

#############################################
# 1. LOAD EXAMPLE SURVEY DATA
#############################################

# Simulate multiple expert ratings
set.seed(1)
species_list <- c("Amphibians", "Arthropods", "Fishes", "Macrophytes",
                  "Molluscs ")

method_list <- c("eDNA", "Drones", "Passive Acoustic Monitoring",
                 "Underwater Cameras", "Remote Sensing", "Conventional Methods")

n_experts <- 5

species_survey <- expand.grid(expert_id = 1:n_experts, species = species_list) %>%
  mutate(body_size = runif(n(),1,5),
         dispersal_potential = runif(n(),1,5),
         sound_production = runif(n(),1,5),
         diel_behaviour = runif(n(),1,5),
         growing_season = runif(n(),1,5),
         reproduction = runif(n(),1,5),
         visual_observability = runif(n(),1,5)) %>%
  clean_names()

methods_survey <- expand.grid(expert_id = 1:n_experts, method = method_list) %>%
  mutate(body_size = runif(n(),1,5),
         dispersal_potential = runif(n(),1,5),
         sound_production = runif(n(),1,5),
         diel_behaviour = runif(n(),1,5),
         growing_season = runif(n(),1,5),
         reproduction = runif(n(),1,5),
         visual_observability = runif(n(),1,5)) %>%
  clean_names()

#############################################
# 2. CALCULATE INTRACLASS CORRELATION (ICC) PER TRAIT
#############################################

# Function to compute ICC for each trait
compute_icc <- function(df, traits, id_col = "species"){
  icc_results <- map_df(traits, function(trait){
    trait_matrix <- df %>%
      select(all_of(c(id_col, "expert_id", trait))) %>%
      pivot_wider(names_from = expert_id, values_from = trait) %>%
      select(-all_of(id_col)) %>%
      as.matrix()
    
    icc_val <- icc(trait_matrix, model="twoway", type="consistency", unit="average")$value
    tibble(trait = trait, ICC = icc_val)
  })
  return(icc_results)
}

species_traits <- colnames(species_survey)[3:9]
methods_traits <- colnames(methods_survey)[3:9]

icc_species <- compute_icc(species_survey, species_traits, id_col="species")
icc_methods <- compute_icc(methods_survey, methods_traits, id_col="method")

print("ICC for Species Traits:")
print(icc_species)

print("ICC for Method Traits:")
print(icc_methods)

#############################################
# 3. AVERAGE TRAIT SCORES PER SPECIES/METHOD
#############################################

species_avg <- species_survey %>%
  group_by(species) %>%
  summarise(across(body_size:visual_observability, list(mean=mean, sd=sd))) %>%
  ungroup()

methods_avg <- methods_survey %>%
  group_by(method) %>%
  summarise(across(body_size:visual_observability, list(mean=mean, sd=sd))) %>%
  ungroup()

species_mat <- species_avg %>%
  select(ends_with("_mean")) %>%
  as.matrix()
rownames(species_mat) <- species_avg$species

methods_mat <- methods_avg %>%
  select(ends_with("_mean")) %>%
  as.matrix()
rownames(methods_mat) <- methods_avg$method

#############################################
# 4. STANDARDIZE TRAITS
#############################################

species_std <- scale(species_mat)
methods_std <- scale(methods_mat)

#############################################
# 5. HEATMAPS
#############################################

pheatmap(species_std,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         fontsize_row = 9,
         fontsize_col = 10,
         angle_col = 45,
         main = "Species Traits Heatmap")

pheatmap(methods_std,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         fontsize_row = 12,
         fontsize_col = 12,
         angle_col = 45,
         main = "Methods Traits Heatmap")

#############################################
# 6. SPECIES × METHOD SUITABILITY MATRIX
#############################################

suitability <- species_std %*% t(methods_std)

# Normalize 0-5
suitability_norm <- apply(suitability, 2, function(x) (x - min(x)) / (max(x) - min(x)) * 5)

pheatmap(suitability_norm,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         fontsize_row = 9,
         fontsize_col = 10,
         main = "Species × Method Suitability")

#############################################
# 7. HIERARCHICAL CLUSTERING OF SPECIES
#############################################

species_cluster <- hclust(dist(species_std))
plot(species_cluster,
     main = "Species Clustering by Detectability Traits",
     xlab = "", sub = "")

species_clusters <- cutree(species_cluster, k = 3)
species_clusters_df <- data.frame(species = rownames(species_std),
                                  cluster = species_clusters)

#############################################
# 8. PCA OF SPECIES TRAITS
#############################################

species_pca <- PCA(species_std, graph = FALSE)
fviz_pca_biplot(species_pca,
                label = "var",
                repel = TRUE,
                title = "PCA of Species Detectability Traits")

#############################################
# 9. EXPORT RESULTS
#############################################

write.csv(suitability_norm, "species_method_suitability.csv")
write.csv(species_avg, "species_avg_traits.csv")
write.csv(methods_avg, "methods_avg_traits.csv")
write.csv(species_clusters_df, "species_clusters.csv")
write.csv(icc_species, "icc_species_traits.csv")
write.csv(icc_methods, "icc_method_traits.csv")

#############################################
# END OF WORKFLOW
#############################################

```
