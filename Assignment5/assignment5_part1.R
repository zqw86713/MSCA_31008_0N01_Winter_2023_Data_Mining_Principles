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


# 2. Use the training and Test samples for the GermanCredit data set 
# that you used in Assignment 1. Call them, say, Train and Test.

set.seed(3456)

# Create a function to split sample into train and test using 632:328 ratio
# # Use the same seed from the previous assignment for train-test split

split_sample_to_train_and_test <- function(df, portion = 0.632) {
    
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


df_split <- split_sample_to_train_and_test(df)

# check training data set
# head(df_split[[1]][])
train_set <- df_split[[1]][]

# check testing data set
# head(df_split[[2]][])
test_set <- df_split[[2]][]



# Load Anil’s `clustreg` and `clustreg.predict` functions
clustreg=function(dat,k,tries,sed,niter)
{
    
    set.seed(sed)
    dat=as.data.frame(dat)
    rsq=rep(NA,niter)
    res=list()
    rsq.best=0
    for(l in 1:tries) 
    {
        
        c = sample(1:k,nrow(dat),replace=TRUE)
        yhat=rep(NA,nrow(dat))
        for(i in 1:niter) 
        {		
            resid=pred=matrix(0,nrow(dat),k)
            for(j in 1:k)
            {	
                pred[,j]=predict(glm(dat[c==j,],family="gaussian"),newdata=dat)		
                resid[,j] = (pred[,j]-dat[,1])^2
            }
            
            c = apply(resid,1,which.min)
            for(m in 1:nrow(dat)) {yhat[m]=pred[m,c[m]]}
            rsq[i] = cor(dat[,1],yhat)^2	
        }
        
        if(rsq[niter] > rsq.best) 
        {	
            rsq.best=rsq[niter]
            l.best=l
            c.best=c
            yhat.best=yhat
        }
    }
    
    res=list("Complete")
    for(i in k:1) {res=list(summary(lm(dat[c.best==i,])),res)}
    
    return(list(data=dat,nclust=k,tries=tries,seed=sed,rsq.best=rsq.best,number.loops=niter, Best.try=l.best,cluster=c.best,results=res))
}


clustreg.predict=function(results,newdat){
    
    yhat=rep(NA,nrow(newdat))
    resid=pred=matrix(0,nrow(newdat),length(table(results$cluster)))
    
    for(j in 1:length(table(results$cluster))){			
        pred[,j]=predict(glm(results$data[results$cluster==j,],family="gaussian"),newdata=newdat)		
        resid[,j] = (pred[,j]-newdat[,1])^2
    }
    
    c = apply(resid,1,which.min)
    for(m in 1:nrow(newdat)) {yhat[m]=pred[m,c[m]]}
    rsq = cor(newdat[,1],yhat)^2	
    
    return(list(results=results,newdata=newdat,cluster=c,yhat=yhat,rsq=rsq))
    
}


# Standardize your variables
standaridized_variables = as.data.frame(scale(train_set[,-10]))


# Use the Train data set to build a cluster-wise regression model. 
# Choose "Amount" as the dependent variable. Build 1, 2, and 3 cluster
#  solutions. 4 clusters may be too many for this data set.

train_stand <- standaridized_variables[c(2,1,3,4,5,6,7)]


# Perform cluster-wise regression for 1, 2, and 3 clusters. 
# Arguments for clustreg formula are: dat, k, tries, seed, niter
clustreg.train.1=clustreg(train_stand,1,1,3456,1)
clustreg.train.2=clustreg(train_stand,2,2,3456,10)
clustreg.train.3=clustreg(train_stand,3,2,3456,10)


# Plot R^2 as a function of the number of clusters
plot(c(1,2,3),c(clustreg.train.1$rsq.best,
                clustreg.train.2$rsq.best,
                clustreg.train.3$rsq.best),
     ylim=c(0,1),
     type="l",
     col=4,
     main="VAF Plot for Train Data: Cluster-wise Regression",
     ylab="Variance Accounted For",
     xlab="Number of Clusters")
