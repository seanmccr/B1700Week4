# B1700 | Predictive Analytics | 13.10.23

if(!require(caret)) install.packages('caret')
install.packages("forecast")
# ----- Data Preparation in R for Predictve Analysis -----
##### Scaling #####
# Loading in dataset, code to create a scaled variable using a current variable,
# Code to create a boxplot of our original variable and then thre scaled version
data("mtcars")
mtcars$mpg_scaled <- scale(mtcars$mpg)
boxplot(mtcars$mpg, mtcars$mpg_scaled, main = "Boxplot",
        names = c("original", "scaled"), col = c("green", "blue"))

##### Hot Encoding #####
# We will apply one-hot encoding using model.matrix
EncodedCyl <- model.matrix(~as.factor(mtcars$cyl)-1)
# Now we convert the matrix to a data frame and add it to the original data
mtcars <- cbind(mtcars, as.data.frame(EncodedCyl))
head(mtcars)

##### Splitting Data into Training and Test Sets #####
library(caret)
data(mtcars)
# Set seed for reproducibility
set.seed(123)
# Creating the partition along a 70% training/30% testing line
trainIndex <- createDataPartition(mtcars$mpg, p = .7, list = FALSE)
# Creating the training set
trainSet <- mtcars[trainIndex, ]
# Creating the testing set 
testSet <- mtcars[-trainIndex, ]

# ----- Building Predictive Models in R -----
##### Linear Regression: Predicting Continuous Outcomes -----
# Loading in dataset
url <- "https://www.dropbox.com/scl/fi/72x6r85ncsqn4hf1oszlm/t15_data_b1700_02.csv?rlkey=w9bakm4od1vi5vem1opyu69fp&dl=1"
df <- read.csv(url)
rm(url)
# Creating a linear model fit using the variables we have inputted
lm_fit <- lm(total_points ~ training_hours + previous_points, data = df)
summary(lm_fit)

##### - Logistic Regression: Predicting Binary Outcomes -----
# Loading in dataset
url <- "https://www.dropbox.com/scl/fi/g58fadnr4etjqgfyn1thl/t15_data_b1700_01.csv?rlkey=nb8hz1p4613tmucclafl4gyhv&dl=1"
df <- read.csv(url)
rm(url)
# Code to create our logistic regression model using the variables we have inputted
log_fit <- glm(outcome ~ team_strength + opponent_strength, family=binomial(link='logit'), data=df)
summary(log_fit)

##### Time Series Forecasting: Predicting Future Trends #####
# Loading in dataset
url <- "https://www.dropbox.com/scl/fi/8o1iwbrmdnxk2nsswzlj3/t15_data_b1700_03.csv?rlkey=ckf4o5vylblvyzd82lxvxc29n&dl=1"
df <- read.csv(url)
rm(url)
# Code to create a time series object
df$attendance_ts <- ts(df$attendance, start = c(2019, 1), frequency = 12)
# Code to plot the time series data
plot(df$attendance_ts, main = "Monthly Attendance at Home Games", 
     xlab = "Date", ylab = "Attendance")

###### Time Series Decomposition ------
# Decomposing the time series
decomposed_attendance <- decompose(df$attendance_ts)
# Code to plot the decomposed time series
plot(decomposed_attendance)

##### Time Series Forecasting #####
# Fitting ARIMA Model using 'auto.arima'
library("forecast")
fit_model <- auto.arima(df$attendance_ts)
# Producing a 12 month forecast
future_forecast <- forecast(fit_model, h = 12)
# Code to plot the forecast
plot(future_forecast)


# REMEMBER ABOUT OVERFITTING AND UNDERFITTING! 
# REMEMBER ETHICAL CONSIDERATIONS ABOUT HISTORICAL BIASES
# WITHIN THE DATASETS I.E ETHNICITY, BACKGROUND
