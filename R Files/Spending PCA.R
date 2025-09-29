# Step 1: Load a Demo Dataset
spend <- read.csv("E:\\UoB\\Semester 2\\MABS\\Assessments\\Assignment 1\\PCA Spending.csv")
head(spend)
summary(spend)
str(spend)

# Step 2: Standardize the Data
spend.sc <- spend
spend.sc[1:6] <- data.frame(scale(spend[1:6])) 
summary(spend.sc)

library(corrplot)
corrplot(cor(spend.sc[1:6]), method = 'ellipse', order="hclust") 
# This parameter orders the correlation matrix using hierarchical clustering. 
# Hierarchical clustering groups similar variables together, making the plot easier to interpret by placing highly correlated variables next to each other.

# Step 3: Perform PCA
spend.pc <- prcomp(spend.sc[1:6], scale=TRUE) 
summary(spend.pc)

# Step 4: Visualize PCA Results
# Scree Plot
screeplot(spend.pc, type = "lines", main = "Scree Plot")
plot(spend.pc, type="l")

# Biplot
biplot(spend.pc)

# Step 5: Cumulative Variance Explained
cumsum(summary(spend.pc)$importance[2,]) # The cumsum function computes the cumulative sum of the elements in the vector.
summary(spend.pc)$importance[3,]

# Step 6: Create a New Dataset with Selected Components (First 3 Components)
pca_scores <- spend.pc$x[, 1:3]
head(pca_scores)

# Step 7: Principal Component Loadings
spend.pc$rotation


### Performing rotation
# Install the necessary packages
install.packages("psych")  # For Varimax rotation
library(psych)

# Apply Varimax rotation using the 'principal' function from the 'psych' package
pca_rotated <- principal(spend.sc[, 1:6], nfactors = 3, rotate = "varimax")

# View the rotated factor matrix
pca_rotated$loadings
