# Step 1: Load a Demo Dataset
offers <- read.csv("E:\\UoB\\Semester 2\\MABS\\Assessments\\Assignment 1\\PCA Offers.csv")
head(offers)
summary(offers)
str(offers)

# Step 2: Standardize the Data
offers.sc <- offers
offers.sc[1:6] <- data.frame(scale(offers[1:6])) 
summary(offers.sc)

library(corrplot)
corrplot(cor(offers.sc[1:6]), method = 'ellipse', order="hclust") 
# This parameter orders the correlation matrix using hierarchical clustering. 
# Hierarchical clustering groups similar variables together, making the plot easier to interpret by placing highly correlated variables next to each other.

# Step 3: Perform PCA
offers.pc <- prcomp(offers.sc[1:6], scale=TRUE) 
summary(offers.pc)

# Step 4: Visualize PCA Results
# Scree Plot
screeplot(offers.pc, type = "lines", main = "Scree Plot")
plot(offers.pc, type="l")

# Biplot
biplot(offers.pc)

# Step 5: Cumulative Variance Explained
cumsum(summary(offers.pc)$importance[2,]) # The cumsum function computes the cumulative sum of the elements in the vector.
summary(offers.pc)$importance[3,]

# Step 6: Create a New Dataset with Selected Components (First 3 Components)
pca_scores <- offers.pc$x[, 1:3]
head(pca_scores)
# Step 7: Principal Component Loadings
offers.pc$rotation


# Apply Varimax rotation using the 'principal' function from the 'psych' package
pca_rotated <- principal(offers.sc[, 1:6], nfactors = 3, rotate = "varimax")

# View the rotated factor matrix
pca_rotated$loadings
