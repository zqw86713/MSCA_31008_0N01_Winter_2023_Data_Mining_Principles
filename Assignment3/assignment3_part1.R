# install the package
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("caret")
install.packages("poLCA")
install.packages("dplyr")
install.packages("Amelia")
install.packages("stringr")
install.packages("GGally")

# load the package
library(ggplot2)
library(caret)
library(poLCA)
library(dplyr)
library(Amelia)
library(ggcorrplot)
library(stringr)
library(GGally)


# Step 0.	Use csv data.
GermanCredit <- read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data",
  stringsAsFactors = TRUE
)

colnames(GermanCredit) <- c(
    "CheckingAccountStatus", 
    "Duration", 
    "CreditHistory",
    "Purpose",
    "Amount", 
    "SavingsAccountBonds", 
    "EmploymentDuration", 
    "InstallmentRatePercentage",
    "Personal", 
    "OtherDebtorsGuarantors", 
    "ResidenceDuration", 
    "Property", 
    "Age", 
    "OtherInstallmentPlans", 
    "Housing", 
    "NumberExistingCredits", 
    "Job",
    "NumberPeopleMaintenance",
    "Telephone", 
    "ForeignWorker", 
    "Class"
)

# df <- GermanCredit

str(GermanCredit)

summary(GermanCredit)


# Step 1
# Perform latent class analysis of only the categorical
#  variables for market segmentation using 
#  (function poLCA in package poLCA). Remember: the local optima 
#  problem is big for all the clustering and latent class methods. 
#  The data for analysis should only include the variables 
#  that you think have business relevance for market segmentation.
LCA_data <- GermanCredit[, c(1,3, 6, 7, 9, 12, 17)]


# convert to factor
LCA_data$CheckingAccountStatus <- as.factor(LCA_data$CheckingAccountStatus)
LCA_data$CreditHistory <- as.factor(LCA_data$CreditHistory)
LCA_data$SavingsAccountBonds <- as.factor(LCA_data$SavingsAccountBonds)
LCA_data$EmploymentDuration <- as.factor(LCA_data$EmploymentDuration)
LCA_data$Personal <- as.factor(LCA_data$Personal)
LCA_data$Property <- as.factor(LCA_data$Property)
LCA_data$Job <- as.factor(LCA_data$Job)


# convert to numeric
LCA_data$CheckingAccountStatus <- as.numeric(LCA_data$CheckingAccountStatus)
LCA_data$CreditHistory <- as.numeric(LCA_data$CreditHistory)
LCA_data$SavingsAccountBonds <- as.numeric(LCA_data$SavingsAccountBonds)
LCA_data$EmploymentDuration <- as.numeric(LCA_data$EmploymentDuration)
LCA_data$Personal <- as.numeric(LCA_data$Personal)
LCA_data$Property <- as.numeric(LCA_data$Property)
LCA_data$Job <- as.numeric(LCA_data$Job)


set.seed(2023)

# Create a function to split sample into train and test using ratio
split_sample_to_train_and_test <- function(df, portion=0.7) {
  
  # For bootstrap samples, simple random sampling is used. 
  train_indices <- createDataPartition(df$CheckingAccountStatus,
                                       p = portion,
                                       list = FALSE,
                                       times = 1)
  # create training set
  df_train <- df[train_indices, ]
  
  # create testing set
  df_test <- df[-train_indices, ]
  
  # Return multiple values as list
  return(list(df_train, df_test))
}


# split
df_split <-  split_sample_to_train_and_test(LCA_data)

# check training data set
# head(df_split[[1]][])
train_set <- df_split[[1]][]

# check testing data set
# head(df_split[[2]][])
test_set <- df_split[[2]][]


# define function and select only the categorical variables
f<-cbind(
  CheckingAccountStatus,
  CreditHistory,
  SavingsAccountBonds,
  EmploymentDuration,
  Personal,
  Property,
  Job)~1


# created vector with 5 characters
# bic  = bayesian information criterion
# abic = adjusted bayesian information criterion
# aic  = akaike information criterion
lca_columns= c("model",
               "log_likelihood",
               "df",
               "bic",
               "abic",
               "aic",
               "likelihood_ratio",
               "r2_entropy"
               )

# pass this vector length to ncol parameter and nrow with 0
lca_results = data.frame(matrix(nrow = 0, ncol = length(lca_columns)))

# assign column names
colnames(lca_results) = lca_columns
lca_results$model<-as.integer(lca_results$model)


# calculate entropy R2 for poLCA model
# https://gist.github.com/daob/c2b6d83815ddd57cde3cebfdc2c267b3
machine_tolerance <- sqrt(.Machine$double.eps)
entropy.R2 <- function(fit) {
  entropy <- function(p) {
    p <- p[p > machine_tolerance] # since Lim_{p->0} p log(p) = 0
    sum(-p * log(p))
  }
  error_prior <- entropy(fit$P) # Class proportions
  error_post <- mean(apply(fit$posterior, 1, entropy))
  R2_entropy <- (error_prior - error_post) / error_prior
  R2_entropy
}


# run a sequence of models with two to ten groups
# with nrep=10 it runs every model 10 times and keeps the model with the lowest BIC
# we used the maxiter argument, which indicates the number of random starting points to be used in the estimation.
# by setting it to 100, the function will run 100 times with different starting points, and it will choose the best 
# solution based on the log-likelihood.
# set.seed(1)
lca_row_number <- 1
for(i in 2:6){
  lca_result <- poLCA(formula = f, 
                      data = train_set, 
                      nclass=i, maxiter=100, 
                      na.rm=FALSE,  
                      nrep=10, 
                      verbose=TRUE, 
                      calc.se=TRUE,
                      graphs = FALSE)
  
  #-ve df are not acceptable model so we exit the loop
  if(lca_result$resid.df < 0){
    break
  }
  
  lca_results[lca_row_number,1] <- c(sprintf("Model %s", lca_row_number))
  lca_results[lca_row_number,2] <- lca_result$llik
  lca_results[lca_row_number,3] <- lca_result$resid.df
  lca_results[lca_row_number,4] <- lca_result$bic
  lca_results[lca_row_number,5] <- (-2*lca_result$llik) + 
    ((log((lca_result$N + 2)/24)) * lca_result$npar)
  lca_results[lca_row_number,6] <- lca_result$aic
  lca_results[lca_row_number,7] <- lca_result$Gsq
  
  # calculate the Entropy (a pseudo-r-squared) for each solution
  lca_results[lca_row_number,8] <- entropy.R2(lca_result)
  
  lca_row_number = lca_row_number + 1
}

# convert model to numeric
model <- as.factor(lca_results$model)
lca_results$model <- as.numeric(model)

# plot the AIC 
lca_results %>%
  ggplot(aes(x=model,y=aic, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: AIC on model")+
  xlab("Model")+
  ylab("AIC")


# plot ABIC
lca_results %>%
  ggplot(aes(x=model,y=abic, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: Adjust BIC on model")+
  xlab("Model")+
  ylab("ABIC")


# plot BIC
lca_results %>%
  ggplot(aes(x=model,y=bic, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot: bic on model")+
  xlab("Model")+
  ylab("BIC")

# AIC showing 2 
# BIC showing 2
# ABIC showing 2
# The cluster is 2.

# STEP 3
# Perform Test validation of LCA using your chosen K. 
# For Test, use the centers class-conditional probabilities 
# - probs - from training set as input to probs.start for test 
# (generated from the training set LCA solution, as the starting 
# point for the test. Use similarity of relative class sizes and 
# test class conditional probabilities as measures of stability.
# 

# Rerun your LCA model with the training set and your chosen K. 
# Save the conditional probability
lca_result_k2 <- poLCA(formula = f, 
                    data = train_set, 
                    nclass=2, maxiter=100, 
                    na.rm=FALSE,  
                    nrep=10, 
                    verbose=TRUE, 
                    calc.se=TRUE,
                    graphs = FALSE)

cond_prob <- lca_result_k2$probs

# Build a new LCA model with the test set, your chosen K, 
# and the probability you just saved as the initial probability. 
# For example, the test model should be "poLCA(f, German.Test.Data, ..., 
# prob.start = LCA.train.object$probs....)
lca_result_test <- poLCA(
  formula=f, 
  data=test_set,
  nclass = 2, 
  maxiter = 1000,
  graphs = FALSE, 
  tol = 1e-10, 
  na.rm = FALSE,
  probs.start = lca_result_k2$probs, 
  nrep = 10, 
  verbose = TRUE, 
  calc.se = TRUE
)


# Print class sizes and conditional probabilities of the train model 
# and the test model. How similar are they? Would you consider the
#  model stable?
lca_result_test
