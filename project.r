# Install and load the kohonen package
# install.packages("kohonen")  # Uncomment this if the package is not installed
library(kohonen)

# Load the dataset
data(iris)

# Remove the target variable for unsupervised learning
iris_data <- iris[, -5]

# Normalize the data
iris_scaled <- scale(iris_data)

# Define the grid for SOM
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")

# Train the SOM model
iris_som <- som(X = as.matrix(iris_scaled), grid = som_grid, rlen = 100)

# Plot the U-Matrix (Distance Map)
plot(iris_som, type = "dist.neighbours", main = "Distance Map")

# Visualize Node Clustering
# Plot node clustering (Codebook Vectors)
plot(iris_som, type = "codes", main = "Codebook Vectors")

# Feature Weight Visualization
# Visualize individual variables
par(mfrow = c(2, 2))  # Layout for multiple plots
plot(iris_som, type = "property", property = getCodes(iris_som)[, 1], main = "Feature 1: Sepal.Length")
plot(iris_som, type = "property", property = getCodes(iris_som)[, 2], main = "Feature 2: Sepal.Width")
plot(iris_som, type = "property", property = getCodes(iris_som)[, 3], main = "Feature 3: Petal.Length")
plot(iris_som, type = "property", property = getCodes(iris_som)[, 4], main = "Feature 4: Petal.Width")
par(mfrow = c(1, 1))  # Reset layout

# Clustering the SOM with hierarchical clustering
# Perform hierarchical clustering on SOM nodes
som_cluster <- cutree(hclust(dist(getCodes(iris_som))), k = 3)  # Define 3 clusters

# Plot clusters on the SOM map
plot(iris_som, type = "mapping", bgcol = rainbow(3)[som_cluster], main = "Cluster Mapping")
add.cluster.boundaries(iris_som, som_cluster)
