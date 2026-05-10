# Load necessary libraries
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Load the data
data <- read.csv("Votre avis sur la formation (réponses) - Réponses au formulaire 1.csv", header = TRUE, sep = ",")

# We will use Q7 to Q30 as active quantitative variables.
# Q1 is Horodateur (Supplementary Quanti)
# Q2, Q3, Q4, Q5, Q6, Q31, Q32, Q33, Q34, Q35 are Supplementary Quali

# Perform PCA
# Q1 (Horodateur) and Q6 (Plateformes) are moved to quali.sup as they are non-numeric strings.
res.pca <- PCA(data, 
               quanti.sup = NULL, 
               quali.sup = c(1, 2, 3, 4, 5, 6, 31, 32, 33, 34, 35), 
               graph = FALSE)

# Generate Scree Plot (Eboulis des valeurs propres)
# Matching PDF style: fviz_eig
p1 <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) +
      theme_minimal() +
      labs(title = "Éboulis des valeurs propres (Scree Plot)",
           x = "Dimensions", y = "% de variance expliquée")
ggsave("scree_plot.png", plot = p1, width = 8, height = 6)

# Generate Variables Circle (Cercle des corrélations)
# Matching PDF style EXACTLY: col.var="cos2", scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.6)
p2 <- fviz_pca_var(res.pca, col.var = "cos2") +
      scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.6) +
      theme_minimal() +
      labs(title = "Cercle des corrélations des variables",
           subtitle = "Coloré par la qualité de représentation (cos2)")
ggsave("var_circle.png", plot = p2, width = 10, height = 8)

# Generate Individuals Map (Carte des individus)
# Matching PDF style EXACTLY: geom="text", col.ind="cos2", scale_color_gradient2
p3 <- fviz_pca_ind(res.pca, geom = c("point", "text"), col.ind = "cos2") +
      scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.6) +
      theme_minimal() +
      labs(title = "Carte des individus (Répondants)",
           subtitle = "Coloré par la qualité de représentation (cos2)")
ggsave("ind_map.png", plot = p3, width = 10, height = 8)

# Generate Individuals Map colored by Sexe (Habillage)
p4 <- fviz_pca_ind(res.pca, 
                   geom = c("point", "text"),
                   habillage = 3, # Q3 is Sexe
                   addEllipses = TRUE, 
                   ellipse.level = 0.95) +
      theme_minimal() +
      labs(title = "Carte des individus par Sexe",
           subtitle = "Groupes définis par le genre")
ggsave("ind_sexe.png", plot = p4, width = 10, height = 8)

# Extra: Map colored by Niveau d'étude (Q4)
p5 <- fviz_pca_ind(res.pca, 
                   geom = c("point", "text"),
                   habillage = 4, # Q4 is Niveau d'étude
                   addEllipses = TRUE, 
                   ellipse.level = 0.95) +
      theme_minimal() +
      labs(title = "Carte des individus par Niveau d'étude",
           subtitle = "Groupes définis par le niveau d'étude")
ggsave("ind_etude.png", plot = p5, width = 10, height = 8)

# Output summary of Eigenvalues to a text file for interpretation
sink("pca_summary.txt")
print(res.pca$eig)
sink()

# Save coordinates and contributions for the report
write.csv(res.pca$var$coord, "var_coord.csv")
write.csv(res.pca$var$contrib, "var_contrib.csv")
# --- 4. Multiple Correspondence Analysis (MCA / ACM) ---
# We use qualitative variables: Age, Sexe, Niveau d'étude, Fréquence, and the Binary questions (Q31-Q35)
res.mca <- MCA(data[, c(2:5, 31:35)], graph = FALSE)

# MCA Summary
sink("mca_summary.txt")
print(res.mca$eig)
sink()

# MCA Variables Plot
p_mca_var <- fviz_mca_var(res.mca, repel = TRUE, col.var = "contrib") +
             theme_minimal() +
             labs(title = "ACM - Nuage des modalités", subtitle = "Coloré par la contribution")
ggsave("mca_var.png", plot = p_mca_var, width = 10, height = 8)

# MCA Individuals Plot
p_mca_ind <- fviz_mca_ind(res.mca, label = "none", habillage = 2) + # Habillage by Sexe
             theme_minimal() +
             labs(title = "ACM - Nuage des individus", subtitle = "Groupés par Sexe")
ggsave("mca_ind.png", plot = p_mca_ind, width = 10, height = 8)


# --- 5. Hierarchical Clustering on Principal Components (HCPC) ---
# We perform HCPC on the PCA results to find clusters of individuals
res.hcpc <- HCPC(res.pca, nb.clust = -1, graph = FALSE) # -1 lets the algorithm choose the number of clusters

# HCPC Dendrogram
p_hcpc_tree <- fviz_dend(res.hcpc, 
                         cex = 0.7,                     # Label size
                         palette = "jco",               # Color palette
                         rect = TRUE, rect_fill = TRUE, # Add rectangle around clusters
                         rect_border = "jco",           # Rectangle color
                         labels_track_height = 0.8) +
               labs(title = "Classification Hiérarchique (HCPC) - Dendrogramme")
ggsave("hcpc_dendro.png", plot = p_hcpc_tree, width = 10, height = 8)

# HCPC Cluster Map (3D-like or 2D with colors)
p_hcpc_map <- fviz_cluster(res.hcpc,
                           repel = TRUE,            # Avoid label overlapping
                           show.clust.cent = TRUE, # Show cluster centers
                           palette = "jco",         # Color palette
                           ggtheme = theme_minimal(),
                           main = "Carte des clusters (Individus)")
ggsave("hcpc_clusters.png", plot = p_hcpc_map, width = 10, height = 8)

# Save cluster assignments
data$cluster <- res.hcpc$data.clust$clust
write.csv(data, "data_with_clusters.csv", row.names = FALSE)


# --- 6. Behavioral Analysis (Focus on Binary Questions) ---
# We can look at the v.test of the binary variables in the MCA or clusters
sink("behavioral_analysis.txt")
cat("Description of clusters by variables:\n")
print(res.hcpc$desc.var$test.chi2)
cat("\nSpecific modalities that characterize the clusters:\n")
print(res.hcpc$desc.var$category)
sink()
