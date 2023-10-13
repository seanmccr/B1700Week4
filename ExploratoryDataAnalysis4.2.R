# B1700 Week 4 | Exploratory Data Analysis | 11.10.23

# ----- Data Preparation and Pre-processing -----
# Loading new dataset, reading and removing url
rm(list = ls()) # clear environment
url <- "https://www.dropbox.com/scl/fi/n9l6lfr0q2o69mphkov4m/t10_data_b1700_01.csv?rlkey=9bdr3wmm344316wte04b897hl&dl=1"
NewDataset <- read.csv(url)
rm(url)

# Install packages we may need
install.packages("rstatix")
install.packages("MASS")
install.packages("vcd")

# ----- Data Inspection -----
# Shows top 6 rows of data in the dataset
head(NewDataset)

# Shows bottom 6 rows of data in the datset
tail(NewDataset)

# Shows the number of collumns and rows in the dataset
dim(NewDataset)

# ----- Structure of Dataset -----
# Shows an overview of each variable + how R defines each variable in terms of type
str(NewDataset)

# Shows summary of our dataset
summary(NewDataset)

# ----- Converting Data Types -----
# Converts the 'Pos' variable into a factor, and then into a numeric type.
NewDataset$NewDS2 <- as.factor(NewDataset$Pos)
NewDataset$NewDS2 <- as.numeric(NewDataset$Pos)

# ----- Descriptive Statistics -----
##### Measures of Central Tendency #####
# Code to show the average of the points
mean(NewDataset$Pts)

# Code to show the mos common points tally
median(NewDataset$Pts)

# Code to show the points tally listed as a horizontal table 
table(NewDataset$Pts)

# READ UP ON MEASURES OF DISPERSION
##### Measures of Dispersion #####
# Upper and lower range of our point tally
range(NewDataset$Pts)

# Variability of our data set # 
var(NewDataset$Pts)

# Standard deviation of our dataset
sd(NewDataset$Pts)

##### Measures of Shape #####
# Install and Load the 'moments' library
install.packages("moments")
library("moments")

# Calculate skewness of variable 'Pts'
skewness(NewDataset$Pts)

# Functions to visualise the 'skewness' of the 'Pts' variable
qqnorm(NewDataset$Pts)
qqline(NewDataset$Pts)

# Kurtosis shows the concentration of data points in the tails relative to a normal distribution
kurtosis(NewDataset$Pts)

# ----- Data Visualisation -----
##### Univariate Analysis #####
# Code to make histogram using 'Pts'; shows the number of times that a 'Pts' total falls within a numerical frame
hist(NewDataset$Pts, col = "blue", main = "Histogram")

# Code to make boxplot showing the max, minimum, mean and expected range that 'Pts' fall within
boxplot(NewDataset$Pts, col = "red", main = "Box Plot", xlab = "Points")

# Creating a density plot of the 'Pts' variable
library(ggplot2)
ggplot(NewDataset, aes(x = Pts)) +
  geom_density(fill = "green", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot for PTS", x = "Pts", y = "Density")

##### Bivariate Analysis #####
# Shows scatter plot of teams 'Draws/D' against the position they are in
plot(NewDataset$Pos, NewDataset$D, main = "Scatter Plot")

# Scatter plot of teams 'Draws/D' against 'Pos', with linear regression line, with the expected range of which the 'Draw/D' variables will fall within
ggplot(NewDataset, aes(x = Pos, y = D)) +
  geom_point() +
  labs(title = "Scatter Plot", x = "League Position", y = "Drawn Games (n)") +
  geom_smooth(method = 'lm') +
  theme_test()

# Code to find the relationship (correlation) between two variables in the dataset
cor(NewDataset$Pos, NewDataset$GD)

# Loading library needed and then function to create a table of correlations for our variables
library(rstatix)
result <- NewDataset %>% cor_test(Pos, GD)
print(result)

# Code to load library and create a heat map of our variables
library(ggplot2)
ggplot(NewDataset, aes(Pos, W)) + geom_tile(aes(fill = GD)) +
scale_fill_gradient(low = "white", high = "green")

##### Multivariate Analysis #####
# Code to remove non-numeric variables from 'NewDataSet' and the remainder creates 'NewDataSet2)
NewDataset2 <- NewDataset[sapply(NewDataset, is.numeric)]
# Code to create a multivariate analysis of the numeric variables
pairs(NewDataset2)

# Parallel co-ordinate plot coding
library(MASS)
parcoord(NewDataset2, col = 1:nrow(NewDataset2))

# ----- Techniques for Categorical Data -----
##### Frequency Table #####
#Frequency table code; this shows us how often a value is returned in our dataset within
# one variable.
table(NewDataset$D)

##### Bar Plots #####
# Code for a bar plot of the number of draws each team had, with how many times each team had that
# number of draws
barplot(table(NewDataset$D), main = "Bar Plot of No. of Draws",
        col = "purple", xlab = "Frequency", ylab = "Draws (n) for each team")

##### Pie Chart #####
# Code for creating a pie chart
pie(table(NewDataset$D), main = "Pie Chart", col = rainbow(length(table(NewDataset$D))))

##### Mosaic Plot #####
# Code for creating a mosaic plot
library(vcd)
mosaic(~ GD + D, data = NewDataset)

##### Stacked Bar Plots #####
# Creating a new data frame
library(ggplot2)
df <- data.frame(
  
  finish = c("Top", "Middle", "Bottom", "Top", "Middle", "Bottom",
             "Top", "Middle", "Bottom"),
  league = c("League1", "League1", "League1", "League2",
             "League2", "League2", "League3", "League3", "League3"),
  points = c(12, 10, 8, 15, 10, 7, 18, 12, 4)
)

# Code for stacked bar plot
ggplot(df, aes(fill=finish, y=points, x=league)) +
  geom_bar(position="stack", stat="identity") +
  labs(title="Stacked Bar Plot", x="league", y="points")


# ----- PRACTICAL ACTIVITY -----
# Load in new dataset
url <- "https://www.dropbox.com/scl/fi/iqnrgpxs6brdseigkonb1/dummy01.csv?rlkey=6x84p8xdieb9m0rnbrnjivtnv&dl=1"
Dataframe3 <- read.csv(url)
rm(url)

# Summary, head, tail and string commands with the new dataframe to 
# give us an overview of our dataset
summary(Dataframe3)
head(Dataframe3)
tail(Dataframe3)
str(Dataframe3)

# Turn 'gender' to a factor variable type
Dataframe3$gender <- as.factor(Dataframe3$gender)

# Determining mean, range, variation and standard deviation of the 'heart' variable
mean(Dataframe3$heart)
range(Dataframe3$heart)
var(Dataframe3$heart)
sd(Dataframe3$heart)

# Histogram with x & y axis' labelled
hist(Dataframe3$rest, col = "orange", main = "Histogram", xlab = "Rest", ylab = "Frequency")

# Boxplot of the 'Recovery' variable
boxplot(Dataframe3$rest, col = "green", main = "Boxplot of Recovery", xlab = "Recovery")

# Code to create a boxplot for the 'heart' variable, comparing observations by the 5 positions. For each position, seperate them
# into sex
ggplot(Dataframe3, aes(x = position, y = goals, fill = gender)) +
  geom_boxplot() +
  labs(title = "Group Comparison", x = "Group", y = "Value") +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  theme_minimal()

# Code to correlate between 'goals' and 'heart'
library(rstatix)
CorResult <- Dataframe3 %>% cor_test(goals, heart)
print(CorResult)

# Scatterplot code for the relationship between rest and recovery
ggplot(Dataframe3, aes(x = recovery, y = rest)) +
  geom_point() +
  labs(title = "Relationship between Recovery and Rest", x = "Recovery", y = "Rest") +
  geom_smooth(method = 'lm') +
  theme_test()

library(ggplot2)  
ggplot(Dataframe3, aes(goals, heart)) + geom_tile(aes(fill = rest)) +
  scale_fill_gradient(low = "white", high = "darkgreen")

# In statistics, heterogeneity = presence of variability or differences in componentes
# or elemtns of a study/dataset. The higher the heterogeneity, the higher the variability in the 
# dataset.

# A gaussian function describes a bell-shaped curve, also known as the normal distribution. It 
# gets this shape/characterization due to it's mean and standard
# deviation


