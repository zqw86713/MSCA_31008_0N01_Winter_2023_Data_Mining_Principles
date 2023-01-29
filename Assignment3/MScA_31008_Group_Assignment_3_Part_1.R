# ---
# title: "MSCA 31008 Statistical Analysis - Assignment 2 Part 1"
# authors: "Prinu Mathew, Qingwei Zhang, Jake Brewer"
# start_date: "01/19/2023"
# last_revision_date: "02/05/2023"
# ---

# install the package
install.packages("ggplot2")
install.packages('ggcorrplot')
install.packages("caret")
install.packages("poLCA")
install.packages("dplyr")
install.packages('Amelia')
install.packages("stringr")
install.packages('GGally')

# load the package
library(ggplot2)
library(caret)
library(poLCA)
library(dplyr)
library(Amelia)
library(ggcorrplot)
library(stringr)
library(GGally)

#*******************************Step 1: Import and prepare the data for analysis*******************************#

# load the data
data(GermanCredit)

# check the first few rows of the data
head(GermanCredit)

#define functions. 
# convert element to integer
func.df.ToInt <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- as.integer(df[[colname]])
  }
  
  return(df)
}

# convert element to numeric
func.df.ToNum <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- str_replace_all(df[[colname]], "[^0-9.]", "")
    df[[colname]] <- suppressWarnings(
      as.numeric(gsub(",", "", format(df[[colname]], scientific = F)))
    )
  }
  
  return(df)
}

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

# group all "Good" to "1"
# group all "Bad" to "0"
GermanCredit_Final <- GermanCredit %>%
  mutate(Class.Category = case_when(
    str_detect(Class, "Bad")  ~ "1",
    str_detect(Class, "Good")  ~ "2"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('Class.Category'))

# group all "CreditHistory.NoCredit.AllPaid" to "1"
# group all "CreditHistory.ThisBank.AllPaid" to "2"
# group all "CreditHistory.PaidDuly" to "3"
# group all "CreditHistory.Delay" to "4"
# group all "CreditHistory.Critical" to "5"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(CreditHistory = case_when(
    str_detect(CreditHistory.NoCredit.AllPaid, "1")  ~ "1",
    str_detect(CreditHistory.ThisBank.AllPaid, "1")  ~ "2",
    str_detect(CreditHistory.PaidDuly, "1")  ~ "3",
    str_detect(CreditHistory.Delay, "1")  ~ "4",
    str_detect(CreditHistory.Critical, "1")  ~ "5"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('CreditHistory'))

# group all "Purpose.NewCar" to "1"
# group all "Purpose.UsedCar" to "2"
# group all "Purpose.Furniture.Equipment" to "3"
# group all "Purpose.Radio.Television" to "4"
# group all "Purpose.DomesticAppliance" to "5"
# group all "Purpose.Repairs" to "6"
# group all "Purpose.Education" to "7"
# group all "Purpose.Retraining" to "8"
# group all "Purpose.Business" to "9"
# group all "Purpose.Other" to "10"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(Purpose.Category = case_when(
    str_detect(Purpose.NewCar, "1")  ~ "1",
    str_detect(Purpose.UsedCar, "1")  ~ "2",
    str_detect(Purpose.Furniture.Equipment, "1")  ~ "3",
    str_detect(Purpose.Radio.Television, "1")  ~ "4",
    str_detect(Purpose.DomesticAppliance, "1")  ~ "5",
    str_detect(Purpose.Repairs, "1")  ~ "6",
    str_detect(Purpose.Education, "1")  ~ "7",
    str_detect(Purpose.Retraining, "1")  ~ "8",
    str_detect(Purpose.Business, "1")  ~ "9",
    str_detect(Purpose.Other, "1")  ~ "10"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('Purpose.Category'))

# group all "CheckingAccountStatus.none" to "1"
# group all "CheckingAccountStatus.lt.0" to "2"
# group all "CheckingAccountStatus.0.to.200" to "3"
# group all "CheckingAccountStatus.gt.200" to "4"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(CheckingAccountStatus = case_when(
    str_detect(CheckingAccountStatus.none, "1")  ~ "1",
    str_detect(CheckingAccountStatus.lt.0, "1")  ~ "2",
    str_detect(CheckingAccountStatus.0.to.200, "1")  ~ "3",
    str_detect(CheckingAccountStatus.gt.200, "1")  ~ "4"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('CheckingAccountStatus'))

# group all "EmploymentDuration.Unemployed" to "1"
# group all "EmploymentDuration.lt.1" to "2"
# group all "EmploymentDuration.1.to.4" to "3"
# group all "EmploymentDuration.4.to.7" to "4"
# group all "EmploymentDuration.gt.7" to "5"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(EmploymentDuration = case_when(
    str_detect(EmploymentDuration.Unemployed, "1")  ~ "1",
    str_detect(EmploymentDuration.lt.1, "1")  ~ "2",
    str_detect(EmploymentDuration.1.to.4, "1")  ~ "3",
    str_detect(EmploymentDuration.4.to.7, "1")  ~ "4",
    str_detect(EmploymentDuration.gt.7, "1")  ~ "5"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('EmploymentDuration'))

# group all "SavingsAccountBonds.Unknown" to "1"
# group all "SavingsAccountBonds.lt.100" to "2"
# group all "SavingsAccountBonds.100.to.500" to "3"
# group all "SavingsAccountBonds.500.to.1000" to "4"
# group all "SavingsAccountBonds.gt.1000" to "5"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(SavingsAccountBonds = case_when(
    str_detect(SavingsAccountBonds.Unknown, "1")  ~ "1",
    str_detect(SavingsAccountBonds.lt.100, "1")  ~ "2",
    str_detect(SavingsAccountBonds.100.to.500, "1")  ~ "3",
    str_detect(SavingsAccountBonds.500.to.1000, "1")  ~ "4",
    str_detect(SavingsAccountBonds.gt.1000, "1")  ~ "5"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('SavingsAccountBonds'))

# group all "Personal.Male.Divorced.Seperated" to "1"
# group all "Personal.Female.NotSingle" to "2"
# group all "Personal.Male.Single" to "3"
# group all "Personal.Male.Married.Widowed" to "4"
# group all "Personal.Female.Single" to "5"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(Personal.Category = case_when(
    str_detect(Personal.Male.Divorced.Seperated, "1")  ~ "1",
    str_detect(Personal.Female.NotSingle, "1")  ~ "2",
    str_detect(Personal.Male.Single, "1")  ~ "3",
    str_detect(Personal.Male.Married.Widowed, "1")  ~ "4",
    str_detect(Personal.Female.Single, "1")  ~ "5"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('Personal.Category'))

# group all "Property.RealEstate" to "1"
# group all "Property.Insurance" to "2"
# group all "Property.CarOther" to "3"
# group all "Property.Unknown" to "4"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(Property.Category = case_when(
    str_detect(Property.RealEstate, "1")  ~ "1",
    str_detect(Property.Insurance, "1")  ~ "2",
    str_detect(Property.CarOther, "1")  ~ "3",
    str_detect(Property.Unknown, "1")  ~ "4"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('Property.Category'))

# group all "Job.UnemployedUnskilled" to "1"
# group all "Job.UnskilledResident" to "2"
# group all "Job.SkilledEmployee" to "3"
# group all "Job.Management.SelfEmp.HighlyQualified" to "4"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(JobSkillLevel = case_when(
    str_detect(Job.UnemployedUnskilled, "1")  ~ "1",
    str_detect(Job.UnskilledResident, "1")  ~ "2",
    str_detect(Job.SkilledEmployee, "1")  ~ "3",
    str_detect(Job.Management.SelfEmp.HighlyQualified, "1")  ~ "4"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('JobSkillLevel'))

# group all "OtherDebtorsGuarantors.None" to "1"
# group all "OtherDebtorsGuarantors.CoApplicant" to "2"
# group all "OtherDebtorsGuarantors.Guarantor" to "3"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(OtherDebtorsGuarantors = case_when(
    str_detect(OtherDebtorsGuarantors.None, "1")  ~ "1",
    str_detect(OtherDebtorsGuarantors.CoApplicant, "1")  ~ "2",
    str_detect(OtherDebtorsGuarantors.Guarantor, "1")  ~ "3"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('OtherDebtorsGuarantors'))

# group all "OtherInstallmentPlans.Bank, "1"" to "1"
# group all "OtherInstallmentPlans.Stores" to "2"
# group all "OtherInstallmentPlans.None" to "3"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(OtherInstallmentPlans = case_when(
    str_detect(OtherInstallmentPlans.Bank, "1")  ~ "1",
    str_detect(OtherInstallmentPlans.Stores, "1")  ~ "2",
    str_detect(OtherInstallmentPlans.None, "1")  ~ "3"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('OtherInstallmentPlans'))

# group all "Housing.Rent, "1", "1"" to "1"
# group all "Housing.Own" to "2"
# group all "Housing.ForFree" to "3"
GermanCredit_Final <- GermanCredit_Final %>%
  mutate(Housing.Category = case_when(
    str_detect(Housing.Rent, "1")  ~ "1",
    str_detect(Housing.Own, "1")  ~ "2",
    str_detect(Housing.ForFree, "1")  ~ "3"
  ))
GermanCredit_Final <- func.df.ToNum(GermanCredit_Final,list('Housing.Category'))

# drop columns
drop_columns <- c("ResidenceDuration","Class","CheckingAccountStatus.none","Telephone", 
                  "Purpose.Vacation","ForeignWorker","EmploymentDuration.Unemployed",
                  "EmploymentDuration.lt.1","EmploymentDuration.1.to.4","EmploymentDuration.4.to.7",
                  "EmploymentDuration.gt.7","SavingsAccountBonds.Unknown","SavingsAccountBonds.lt.100",
                  "SavingsAccountBonds.100.to.500","SavingsAccountBonds.500.to.1000",
                  "SavingsAccountBonds.gt.1000","CheckingAccountStatus.none","CheckingAccountStatus.lt.0",
                  "CheckingAccountStatus.0.to.200","CheckingAccountStatus.gt.200",
                  "CreditHistory.NoCredit.AllPaid","CreditHistory.ThisBank.AllPaid","CreditHistory.PaidDuly",
                  "CreditHistory.Delay","CreditHistory.Critical","Purpose.NewCar","Purpose.UsedCar",
                  "Purpose.Furniture.Equipment","Purpose.Radio.Television","Purpose.DomesticAppliance",
                  "Purpose.Repairs","Purpose.Education","Purpose.Retraining","Purpose.Business",
                  "Purpose.Other","Personal.Male.Divorced.Seperated","Personal.Female.NotSingle",
                  "Personal.Male.Single","Personal.Male.Married.Widowed","Personal.Female.Single",
                  "Property.RealEstate","Property.Insurance","Property.CarOther","Property.Unknown",
                  "Job.UnemployedUnskilled","Job.UnskilledResident","Job.SkilledEmployee","Job.Management.SelfEmp.HighlyQualified",
                  "OtherDebtorsGuarantors.None","OtherDebtorsGuarantors.CoApplicant","OtherDebtorsGuarantors.Guarantor",
                  "OtherInstallmentPlans.Bank","OtherInstallmentPlans.Stores","OtherInstallmentPlans.None",
                  "Housing.Rent","Housing.Own","Housing.ForFree")
GermanCredit_Final <- GermanCredit_Final[,!(names(GermanCredit_Final) %in% drop_columns)]


# check for any NA’s in the dataframe
missmap(GermanCredit_Final,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)
colSums(is.na(GermanCredit_Final))


# correlations
# A positive correlation indicates the extent to which those variables increase or decrease in parallel; 
# a negative correlation indicates the extent to which one variable increases as the other decreases.
germanCredit_corr <- round(cor(GermanCredit_Final), 1)
ggcorrplot(germanCredit_corr, type = "lower", lab = TRUE)


# define function and select only the categorical variables
f<-cbind(Age,Duration,InstallmentRatePercentage,NumberExistingCredits,NumberPeopleMaintenance,Class.Category,
         CreditHistory,Purpose.Category,CheckingAccountStatus,EmploymentDuration,SavingsAccountBonds,
         Personal.Category,Property.Category,JobSkillLevel,OtherDebtorsGuarantors,OtherInstallmentPlans,
         Housing.Category)~1


#*******************************Step 2: Perform latent class analysis of only the categorical variables for market segmentation *******************************#

# define the model
set.seed(20230121)

# created vector with 5 characters
# bic  = bayesian information criterion
# abic = adjusted bayesian information criterion
# aic  = akaike information criterion
lca_columns= c("model","log_likelihood","df","bic","abic","aic","likelihood_ratio","r2_entropy")

# pass this vector length to ncol parameter and nrow with 0
lca_results = data.frame(matrix(nrow = 0, ncol = length(lca_columns)))

# assign column names
colnames(lca_results) = lca_columns
lca_results$model<-as.integer(lca_results$model)


# split the data to train and test.
for(i in 1:1000){
  sample <- sample(c(TRUE,FALSE), 
                   nrow(GermanCredit_Final), 
                   replace=TRUE, 
                   prob=c(0.7,0.3))
  train <- GermanCredit_Final[sample, ]
  test <- GermanCredit_Final[!sample, ]
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
                      data = train, 
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
  lca_results[lca_row_number,5] <- (-2*lca_result$llik) + ((log((lca_result$N + 2)/24)) * lca_result$npar)
  lca_results[lca_row_number,6] <- lca_result$aic
  lca_results[lca_row_number,7] <- lca_result$Gsq
  
  # calculate the Entropy (a pseudo-r-squared) for each solution
  lca_results[lca_row_number,8] <- entropy.R2(lca_result)
  
  lca_row_number = lca_row_number + 1
}


# temp_lca_result <- poLCA(formula = f, data = GermanCredit_Final, nclass=2, maxiter=100, na.rm=FALSE,  
#                     nrep=10, verbose=TRUE, calc.se=TRUE, graphs = FALSE)

model <- as.factor(lca_results$model)
lca_results$model <- as.numeric(model)

# We believe the K=2 is the elbow.
# 
plot(x=lca_results$model,
     y=lca_results$aic, 
     type = "b", 
     xlab="Principal Component", 
     ylab = "AIC")

plot(x=lca_results$model, 
     y=lca_results$abic, 
     type = "b", 
     xlab="Principal Component", 
     ylab = "ABIC"
     )

# The cluster is 2.


#*******************************Step 3: Perform Test validation of LCA *******************************#

lca_result_k2 <- poLCA(formula = f, 
                    data = GermanCredit_Final, 
                    nclass=2, maxiter=100, 
                    na.rm=FALSE,  
                    nrep=10, 
                    verbose=TRUE, 
                    calc.se=TRUE,
                    graphs = FALSE)

# conditional probability
condition_prob <- lca_result_k2$probs
