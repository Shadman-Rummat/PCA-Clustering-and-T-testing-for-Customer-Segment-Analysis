## Step 1: Explore the data ----
smart <- read.csv("E:/UoB/Semester 2/MABS/Assessments/Assignment 1/Clustering.csv") ##Note: set YOUR path
str(smart)
summary(smart)

## Step 2: Training a model on the data ----
interests <- smart[1:14]
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests_z)

k.max <- 10
#data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(interests_z, k, nstart=50, iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


set.seed(2345)
smart_clusters <- kmeans(interests_z, 5)

## Step 3: Check the results ----
smart_clusters

# look at the size of the clusters
smart_clusters$size

# look at the cluster centers
smart_clusters$centers

# look at other parameters
smart_clusters$cluster

# Check the results ----
# look at the size of the clusters
smart_clusters
smart_clusters$tot.withinss
smart_clusters$betweenss
smart_clusters$size
smart_clusters$centers

## Step 4: Visualize clusters ----
library (cluster)
clusplot(interests_z, smart_clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "K-means Cluster Plot")

# Add cluster labels to the original data
smart$Cluster <- smart_clusters$cluster

# Save the dataset with cluster labels
write.csv(smart, "clustered_customers.csv", row.names = FALSE)

