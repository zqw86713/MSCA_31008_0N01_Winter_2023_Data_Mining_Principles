install.packages("caret")
install.packages("tidyselect")
install.packages("ggplot2")

library(tidyselect)
library(caret)
library(ggplot2)

setwd("C:/Users/QZHAN101/Downloads/play/UCHICAGO/MSCA_31008_Data_Mining_Principles")

# 1.	Use csv or caret data.
data(GermanCredit)

df <- GermanCredit

str(df)


# 2.	Build a linear model to predict Amount using the entire sample
# except Class variable.

# remove Class column from dataframe.
df <- subset(df, select = -Class)

full_df_ml <- lm(Amount ~ ., data = df)

summary(full_df_ml)

full_df_ml$coefficients


# 3.	Repeat the following steps 1000 times (for loop):

# Create a function to split sample into train and test using 632:328 ratio
split_sample_to_train_and_test <- function(df, portion = 0.632) {
    # set.seed(3456)
    train_indices <- createDataPartition(df$Amount,
                                         p = portion,
                                         list = FALSE,
                                         times = 1)
    # create training set
    df_train <- df[train_indices, ]
    
    # create testing set
    df_test <- df[-train_indices, ]
    
    # view number of rows in each set
    # nrow(df_train)
    # nrow(df_test)
    
    # Return multiple values as list
    return(list(df_train, df_test))
}

# 2. Create linear model using train data with Amount as the dependent
#  variable and the same independent variables you selected before.
#  Predict results with holdout data
create_linear_model  <- function(df) {
    df_ml <- lm(Amount ~ ., data = df)
    
    return(df_ml)
}

predict_result <- function(model, df) {
    y.cap = predict(model, newdata = df)
    return(y.cap)
}


# set iteration times
iter_number <- 1000

# create three lists to Save Coefficients, R squared in training,
# and R squared of holdout predictions.
list_coefficients <- vector("list", iter_number)
r_squared_training <- vector("list", iter_number)
r_squared_holdout <- vector("list", iter_number)

# start for-loop
for (x in 1:iter_number) {
    df_split <- split_sample_to_train_and_test(df)
    
    # check training data set
    # head(df_split[[1]][])
    train_set <- df_split[[1]][]
    
    # check testing data set
    # head(df_split[[2]][])
    test_set <- df_split[[2]][]
    
    # Create linear model
    my.ml <- create_linear_model(train_set)
    
    # Predict results with holdout data
    # predict_result(my.ml, test_set)
    
    # Save Coefficients,
    list_coefficients[[x]] <- summary(my.ml)$coefficients
    
    # save R squared in training,
    r_squared_training[[x]] <- summary(my.ml)$r.squared
    
    # save R squared of holdout predictions.
    r_squared_holdout[[x]] <-
        cor(test_set$Amount, predict(my.ml, newdata = test_set)) ^ 2
    
}



# 4. Pick 3 variables and plot the distribution of its coefficients.
#  Each column in matrix we created above is the different coefficients
#  for a specific variable. Show the name of the coefficient in the plot.
#  Histogram.
variable1 <- "Duration"
variable2 <- "Age"
variable3 <- "NumberExistingCredits"

# pre-allocate for slightly more efficiency
n = length(list_coefficients)
temp_data_list = vector("list", length = n)


for (i in 1:n) {
    dat1 <- i  # keep track of which iteration produced it
    dat2  <- data.frame(list_coefficients[[i]][variable1, "Estimate"])
    dat3 <- data.frame(list_coefficients[[i]][variable2, "Estimate"])
    dat4  <- data.frame(list_coefficients[[i]][variable3, "Estimate"])
    
    temp_data_list[[i]] <-
        cbind(dat1, dat2, dat3, dat4) # add it to list
}

coefficient_data = do.call(rbind, temp_data_list)

# remove temporary variable.
rm(temp_data_list)

# The first column is the iteration number, I rename the 2, 3, 4 columns.
names(coefficient_data)[1] <- 'Number'
names(coefficient_data)[2] <- variable1
names(coefficient_data)[3] <- variable2
names(coefficient_data)[4] <- variable3

hist(
    coefficient_data$Duration,
    main = "The Coefficient of Duration",
    xlab = "Duration",
    col = "yellow",
    border = "blue"
)

hist(
    coefficient_data$Age,
    main = "The Coefficient of Age",
    xlab = "Age",
    col = "green",
    border = "blue"
)

hist(
    coefficient_data$NumberExistingCredits,
    main = "The Coefficient of Number Existing Credits",
    xlab = "number of Existing Credits",
    col = "red",
    border = "blue"
)


# 5.Plot distribution of R squared in train.

# convert list object to dataframe.
r2_training <- do.call(rbind.data.frame, r_squared_training)

# rename the column
names(r2_training)[1] <- 'r2'

# plot the one dimension
p <-
    ggplot(r2_training, aes(
        x = 1:nrow(r2_training),
        y = r2,
        colour = r2
    )) +  # Apply nrow function
    geom_point()

p + labs(
    x = "number of iteration",
    y = "R-squared of training set",
    title = "The distribution of R squared in train",
    colour = "R-squared"
)


# 6.Calculate percentage decrease of R square from train to holdout.
# (Train.R.Squared – Holdout.R.Squared)/Train.R.Squared.
# Plot the distribution of this as well. Interpret the results of the
# above plots? How would we hope/expect them to look? Does this
# indicate a good result? What do these plots say about what we
#  usually expect the R squared to be and how much R squared we
#   usually expect to lose from Train to holdout?

# convert list object to dataframe.
r2_holdout <- do.call(rbind.data.frame, r_squared_holdout)

# Calculate percentage decrease of R square from train to holdout.
percent_decrease <- (r2_training - r2_holdout) / r2_training

# plot the one dimension
p <-
    ggplot(percent_decrease, aes(
        x = 1:nrow(percent_decrease),
        y = r2,
        colour = r2
    )) +  # Apply nrow function
    geom_point()

p + labs(
    x = "Number of iteration",
    y = "Decreased Percentage",
    title = "The decreased Percentage of R-squared from training to holdout",
    colour = "% decrease"
)


# 7.Calculate the mean of each coefficient.
mean(coefficient_data$Duration)
mean(coefficient_data$Age)
mean(coefficient_data$NumberExistingCredits)


# 8.Calculate the standard deviation of each coefficient.
sd(coefficient_data$Duration)
sd(coefficient_data$Age)
sd(coefficient_data$NumberExistingCredits)

# 9.Compare the means of the 1000 coefficients to the coefficients
# from the model created in step 2 created using the entire sample.
# Show the percentage difference.

(mean(coefficient_data$Duration) - full_df_ml$coefficients['Duration'])/full_df_ml$coefficients['Duration']

(mean(coefficient_data$Age) - full_df_ml$coefficients['Age'])/full_df_ml$coefficients['Age']

(mean(coefficient_data$NumberExistingCredits) - full_df_ml$coefficients['NumberExistingCredits'])/full_df_ml$coefficients['NumberExistingCredits']



# 10.   Using method of choice, calculate CI for each coefficient from the repeated
#  sample model. Calculate the width of the CI as upper.bound - lower.bound*sqrt(.632)
ci_my_ml <- confint(my.ml)
my_Model_Lower_bound = ci_my_ml[, 1]
my_Model_Higher_bound = ci_my_ml[, 2]
my_Model_width = my_Model_Higher_bound - my_Model_Lower_bound


# 11. Calculate CI for full model using confint function. The first column 
# of the output will be the lower bound for the confidence intervals and
#  the second column of the output will be the upper bounds. Calculate width
#   as Upper.bound - lower.bound.
ci_df_ml <- confint(full_df_ml)
full_Model_Lower_bound = ci_df_ml[, 1]
full_Model_Higher_bound = ci_df_ml[, 2]
full_Model_width = full_Model_Higher_bound - full_Model_Lower_bound


# 12. Calculate how many of the repeated sample CI’s are tighter or broader than 
# the full model CI’s. If the width is smaller, the CI is tighter. 
# If the width is bigger, the CI is broader.

df_my_Model_width <- as.data.frame(my_Model_width)
df_full_Model_width <- as.data.frame(full_Model_width)

df1 <- df_my_Model_width > df_full_Model_width
sum(df1, na.rm = TRUE)


# 13. Interpret results. How did the means compare? How about the confidence
#  intervals, how many were tighter or broader? 
#  What does this say about each method? 
#  What if we tried doing 10,000 samples?
