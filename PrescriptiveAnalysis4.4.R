# B1700 | Prescriptive Analysis | 13.10.23

# ----- Prescriptive Analysis Example -----
# An example of prescriptive analysis in R
# Create a clean environment
rm(list = ls())

# Install the lpSolve package if not installed
if(!require(lpSolve)) install.packages('lpSolve')
# Coding example of a prescriptive analysis model
library(lpSolve)
objective.in <- c(3, 2)
const.mat <- matrix(c(2, 1, 1, 1), nrow=2)

const.rhs <- c(4, 2)
const.dir <- c("<=", "<=")
optimum <- lp("max", objective.in, const.mat, const.dir, const.rhs) 
optimum$solution

# ----- Linear Programming Model Example -----
# Example of a linear programming model, where f.obj = player ratings, f.con = players salaries
# Load the lpSolve package
library(lpSolve)

# Set the coefficients of the objective function
f.obj <- c(85, 92, 88, 80, 83) 

# Set the coefficients of the constraints
f.con <- matrix(c(15, 20, 18, 10, 12), nrow=1)

# Set the type of constraints
f.dir <- c("<=")

# Set the right-hand side coefficients
f.rhs <- c(50)

# Run the linear programming model
optimum_lineup <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)

# Print the optimal solution
print(optimum_lineup$solution)

# ----- Monteo Carlo Simulation Example -----

# Install and load necessary packages
library(ggplot2)
# Monte Carlo simulation function
simulate_free_throw <- function(n) {
  shots_made <- rbinom(n, 1, 0.7)
  mean(shots_made)
}

# Run the simulation 10,000 times
n_simulations <- 10000
results <- replicate(n_simulations, simulate_free_throw(1))

# Visualize the results
df <- data.frame(success = results)
ggplot(df, aes(x = success)) + geom_histogram(binwidth = 0.1) + labs(title = "Free Throw Simulation Results", x = "Outcome", y = "Count")

# ----- Decision Trees -----

# Install and load necessary package
if(!require(rpart)) install.packages('rpart')
# Code for the decision tree
library(rpart)

# Generate some mock data
set.seed(123) # To ensure reproducibility
n <- 200 # Number of players

PPG <- rnorm(n, mean=15, sd=5)
APG <- rnorm(n, mean=5, sd=2)

Role <- ifelse(PPG >= 18 & APG >= 7, "Starter", "Bench")

players_data <- data.frame(PPG, APG, Role)

# Creating the Decision Tree
tree_model <- rpart(Role ~ PPG + APG, data=players_data, method="class")

# Printing the decision tree summary
print(tree_model)

# Visualize the tree
plot(tree_model, margin = 0.1)
text(tree_model, use.n = TRUE)








