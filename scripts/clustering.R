# Clustering 
# http://www.sthda.com/english/rpkgs/factoextra/
# http://www.sthda.com/english/articles/26-clustering-basics/85-data-preparation-and-essential-r-packages-for-cluster-analysis/#at_pco=smlwn-1.0&at_si=5a6b49869dd1a0bd&at_ab=per-2&at_pos=0&at_tot=1
# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization

# What you will learn
# PCA, correspondence analysis, hclut, kmeans, optimal clusters


library(ggplot2)
library(cluster)
library(factoextra)
library(FactoMineR)

# Load
data("USArrests")  # Load the data set
df <- USArrests    # Use df as shorter name
str(df)
glimpse(df)

ggplot(df, aes(x = UrbanPop, y = Assault)) +
  geom_point(size = 2, color = "blue") +
  geom_smooth() +
  geom_text(label = row.names(df))

ggplot(df, aes(x = UrbanPop, y = Rape)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_label(label = row.names(df))

# remove NA values
df <- na.omit(df)

# scale 
df <- scale(df)

# 2. Compute k-means
set.seed(123)
km.res <- kmeans(scale(USArrests), 4, nstart = 25)

# 3. Visualize
library("factoextra")
fviz_cluster(km.res, data = df,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

# http://www.sthda.com/english/articles/25-cluster-analysis-in-r-practical-guide/111-types-of-clustering-methods-overview-and-quick-start-r-code/
# k means clustering
# http://www.sthda.com/english/articles/27-partitioning-clustering-essentials/87-k-means-clustering-essentials/
  
# pca
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(USArrests, k = 4, stand = TRUE)

# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))


# Determine the optimal number of clusters
my_data <- scale(USArrests)
fviz_nbclust(my_data, kmeans, method = "gap_stat")




####################################################################
### Exercise using factoxr
library("factoextra")
data("decathlon2")
df <- decathlon2[1:23, 1:10]
library("FactoMineR")
res.pca <- PCA(df,  graph = FALSE)

# Extract eigenvalues/variances
get_eig(res.pca)

# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabel = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(res.pca)
var
head(var$coord)
head(var$contrib)

# Graph of variables: default plot
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Extract the results for individuals
ind <- get_pca_ind(res.pca)
ind
head(ind$coord)
# Graph of individuals
# 1. Use repel = TRUE to avoid overplotting
# 2. Control automatically the color of individuals using the cos2
# cos2 = the quality of the individuals on the factor map
# Use points only
# 3. Use gradient color
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)

# Color individuals by groups:
  # Compute PCA on the iris data set
  # The variable Species (index = 5) is removed
  # before PCA analysis
  iris.pca <- PCA(iris[,-5], graph = FALSE)

# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)










