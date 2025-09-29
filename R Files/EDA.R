smart.df <-read.csv("E:\\UoB\\Semester 2\\MABS\\Assessments\\Assignment 1\\EDA File.csv")
str(smart.df)
spending_columns <- smart.df[, c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")]

summary(spending_columns)


par(mfrow = c(2, 1))  # Set up a 2-row, 1-column layout for plots

for (col in c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")) 
{
  # Histogram with density curve
  hist(smart.df[[col]], breaks = 30, prob = TRUE, main = paste("Histogram with Density Curve for", col), xlab = col, col = "lightblue")
  lines(density(smart.df[[col]], na.rm = TRUE), col = "red", lwd = 2)
}

# List of spending columns
spending_columns <- c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")

# Loop through each spending column
for (col in spending_columns) 
  {
  # Create a horizontal boxplot
  boxplot(smart.df[[col]],
          main = paste("Boxplot for", col),
          xlab = "Amount Spent",
          ylab = col,
          col = "lightgreen",
          horizontal = TRUE)  # Horizontal boxplot
  
  # List of spending columns
  spending_columns <- c("Spend_Wine", "Spend_OrganicFood", "Spend_Meat", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods")
  
  # Loop through each spending column
  for (col in spending_columns) {
    # Create a horizontal boxplot
    boxplot(smart.df[[col]],
            main = paste("Boxplot for", col),
            xlab = "Amount Spent",
            ylab = col,
            col = "lightgreen",
            horizontal = TRUE)  # Horizontal boxplot
  }
  
summary(spending_columns)

install.packages("moments")
library(moments)
summary(spending_columns)
smart.df <-read.csv("E:\\UoB\\Semester 2\\MABS\\Assessments\\Assignment 1\\EDA File.csv")
str(smart.df)
summary(smart.df$Age)

hist(smart.df$Age,
     main = "Histogram for Age of Customers", 
     xlab = "Age",
     ylab = "Number of Customers", 
     breaks = 15,
     col = "lightblue",
     freq = FALSE)
lines(density(smart.df$Age, na.rm = TRUE),
      col = "darkred",
      lwd = 2)

boxplot(smart.df$Age,
        xlab = "Age",
        main = "Boxplot for Age of Customers",
        horizontal = TRUE,  # Horizontal boxplot
        col = "lightblue")

options(scipen = 999)
summary(smart.df$Annual_Income)

hist(smart.df$Annual_Income,
     main = "Histogram for Annual Income of Customers", 
     xlab = "Annual Income",
     ylab = "Relative Frequency", 
     breaks = 25,  # Number of bins
     col = "lightblue",
     freq = FALSE)  # freq=FALSE for relative frequency (density)

# Add a density curve
lines(density(smart.df$Annual_Income, na.rm = TRUE),
      col = "darkred",
      lwd = 2)
boxplot(smart.df$Annual_Income,
        xlab = "Annual Income",
        main = "Boxplot for Annual Income of Customers",
        horizontal = TRUE,  # Horizontal boxplot
        col = "lightblue")
