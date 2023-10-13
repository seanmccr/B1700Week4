# B1700 Week 4 | Data Pre-Processing | 05/10/23

# FOR ALLAN; LINE 79, WHAT DOES THRESHOLD MEAN 
#           LINE 110-115, SAME AS ABOVE

# ----- Installing and Loading Packages -----
# Install packages needed
install.packages("zoo")
# Load packages needed
library(tidyverse)
library(dplyr)
library(tibble)
library(zoo)

# ----- Initial Phase and Creating our DF -----
# Code for creating our dataframe
data <- data_frame(
  id = 1:10,
  age = c(22, 32, NA, 41, 19, NA, 31, 25, 43, 38),
  gender = c("M", "F", "F", NA, "M", NA, "F", "F", "M", "F")
)

# ----- Identifying Missing Data -----
# Code to identify 'NA' within our dataframe using a logical vector i.e TRUE or FALSE
missingdata <- is.na(data)
missingdata

# Code to show where each case is complete or not
complete.cases(data)

# This code provides the exact rows that are returning 'NA' in them
data[!complete.cases(data),]

# This code provides an exact number of missing values in our data set
totalmissingvalues <- sum(missingdata)
totalmissingvalues

# This code provides us with the missing values per variable
colmissval <- colSums(missingdata)
colmissval

# This code provides us with the mean of the missing values (number of missing values / total values per variable)
colpropval <- colMeans(missingdata)
colpropval

# ----- Handling Missing Data -----                    
# This code omits any row of data returning a 'NA' value
# Simplest way of removing 'NA' values, however can chop off a lot of our data
dataNAom <- na.omit(data)
dataNAom

# ----- Outlier Detection -----
# Code and dataset with outliers in it for next exercise
url <- "https://www.dropbox.com/scl/fi/jb9b9uhx728e4h6g46r1n/t10_data_b1700_02.csv?rlkey=3sjwjwd6y59uj5lq588eufvpm&dl=1"
dataframe2 <- read.csv(url)
rm(url)

# ----- Visual Methods -----
# Code to create a box plot using a variable from our new dataframe
boxplot(dataframe2$Pl, main = "Box Plot - Outlier Detection")

# Scatter plot of 'League Position vs Losses' recorded
plot(dataframe2$Pos, dataframe2$L, main = " Position vs No. of Losses", 
     xlab = "League Pos", ylab = "No of Losses", pch = 20, frame = FALSE)
# Remember, labels must be "" as it is characters being inputted. Also remember 'pch'
# as this provides the actual points for your data on the table.
# Remember the $ sign is used to access list variables within your dataset

# Using a histogram to identify outliers in our data
hist(dataframe2$D, col = "green")

# Another method to identify an outlier in the data
summary(dataframe2)

# ----- Statistical Methods -----
# Creation of new dataset
data <- c(50,51,52,55,56,57,80,81,82) # Example data set

# ----- Z-Scores -----
# Code to identify outliers in our data
z_scores <- scale(data)
threshold = 2 # 2 is normal limit that is used. Anything outside of 2 would be considered or identified as an outlier. Number used depends on nature of data used.
outliers_z <- data[abs(z_scores) > threshold]
outliers_z # there are no outliers in the data

data <- c(50, 51, 52, 55, 56, 57, 80, 81, 182) # Now, we introduce an outlier

z_scores <- scale(data)
threshold = 2 ###
outliers_z <- data[abs(z_scores) > threshold]
print(outliers_z)  # the outlier has been identified successfully

# ----- Interquartile Range (IQR) -----
# Calculate IQR
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)
IQR <- Q3 - Q1

# OUTLIERS ARE DEFINED AS BEING BELOW 
# Q1-(1.5*IQR) OR ABOVE Q3-(1.5*IQR)
# Define threshold (e.g., 1.5 times IQR)
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers; data below the defined lower bound (equation above) or above
# the upper bound (equation above))
outliers <- data[data < lower_bound | data > upper_bound]

# Print outliers
print(outliers)  # the outlier of 182 has been identified

# ----- Outlier Treatment -----
# Removing outliers
data <- c(50, 51, 52, 55, 56, 57, 80, 81, 182)
data_clean <- data[data !=182] # this code cleans data by removing the number from the dataset using a logical function

# Removing outliers method 2
data <- c(50, 51, 52, 55, 56, 57, 80, 81, 182)
data_clean <- data[-9]

# Removing outliers method 3 w/z-scores
data <- c(50, 51, 52, 55, 56, 57, 80, 81, 182) # Example data with the 182 outlier
z_scores <- scale(data) # calculate z-scores
threshold = 2  # set threshold 
data_cleaned <- data[abs(z_scores) <= threshold] # create a 'clean' set of data
print(data_cleaned) # the outlier has been removed


