#Regularization

#What is Regularization ?
  # Regularization is a method that prevents Over-Fitting 

#How does it help us (Goals) ?
  # It improves the generalizability of a learned model by
      # avoiding huge weights (betas) 
      # thereby prevents a small change in beta to cause a huge change in dependent
      # not have too much weight for betas
      # This is called as paramter shrinkage
      # It can also aid in feature selection

#Type of Regularization for linear regression
    # (1) Lasso (also called L1 regularization), 
    # (2) Ridge (also called L2 regularization)
    # (3) Elastic Regression
          # Elastic regression is a combination of Lasso & Ridge Regression

#The mathematics behind regularization
    # Let us see normal linear regression first
      # In normal linear regression, we are trying to find a function
      # that tries to minimize the sum of squared error (between actual y and predicted y)
      # Mathematically we can represent the objective as 
          # Min (y - y')^2, where y = actual and y' = predicted
          # in otherwords, Min (y - (bo + bi*xi)^2)
          # We refer the above eqn as cost (or loss) function in machine learning language

    # In Regularization, we modify the above cost function as follows
          # Min [ (y - (bo + bi*xi)^2) + lambda*(summation(|bi|^p)) ]
          # lambda and p are constants
          # so what will happen to the betas now ? - Think about it
          # Will this equation help us to achieve the goals described above ? - Think about it
          
    # Lasso (L1 Regression)
          # Min [ (y - (bo + bi*xi)^2) + lambda*(summation(|bi|) ]
          # L1 produces results with sparse betas and smaller coefficients
          # In otherwords, many betas are set to zeros
          # Useful when you are interested in keeping very attributes in the model
          # So what
            # Model is efficient to store
            # Model is efficient to compute
          # Note L1 can acheive both paramdter shrinkage and feature selection

    # Ridge (L2 Regression)
          # Min [ (y - (bo + bi*xi)^2) + lambda*(summation(bi^2) ]
          # Ridge acheives parameter shrinkage only
          # Ridge may produce a better fit than lasso


#Basics

install.packages("MASS")   
install.packages("glmnet") # To fit ridge/lasso/elastic net models

library(MASS)
library(glmnet)

#We shall use the telecom data set where the monthly spend is predicted as a
#function of usage

#Data used - telecom dataset
#Predictors - all variables except amount
#Predicted  - amount variable

# Assign telecom to data
# Lasso, Ridge and Elastic Net require data to be in matrix form
# split data into training and test folds
# Lasso, Ridge and Elastic Net cannot handle factor variables.
# So factor variables were removed. 

data <-telecom
set.seed(pi)
data$random <- runif(nrow(data),min=0,max=nrow(data))
datatrain <- subset(data,random <= 700)
datatest  <- subset(data,random >= 700)
View(datatrain)
View(datatest)

# Split data into dependent and independent variables
# move dependent variable into datay
# as.numeric is needed to ensure that numeric nature is preserved

dataytrain <- as.numeric(datatrain[,9])
dataytest  <- as.numeric(datatest[,9])

# move independent variable into datax and it must be matrix form
dataxtrain <- data.matrix(datatrain[,1:8])
dataxtest  <- data.matrix(datatest[,1:8])

# Use data.matrix instead of as.matrix

# Fit the models
# We shall use the function cv.glmnet for this purpose
# cv.glmnet arguments
# x : matrix of independent variables
# y : matrix of dependent variables
# lambda : glm chooses automatically
# nfolds : Number of folds in cross validation - Default is 10 - should be enough
# type.measure : the measure that is use to compare cv results - default is - deviance
# alpha : 0 (ridge), 1 (lambda), 0.5 (mix)

# The results of the models are stored in a model object
# alpha = 1 for lasso; alpha = 0 for ridge and 0 < alpha < 1 for elasticnet


lasso.cv <- cv.glmnet(dataxtrain, dataytrain, type.measure="mse", alpha=1.0,family="gaussian")
ridge.cv <- cv.glmnet(dataxtrain, dataytrain, type.measure="mse", alpha=0.0,family="gaussian")
elnet.cv <- cv.glmnet(dataxtrain, dataytrain, type.measure="mse", alpha=0.5,family="gaussian")

# Make Predictions on the test set
ylasso<- predict(lasso.cv, s=lasso.cv$lambda.1se, newx=dataxtest)
yridge<- predict(ridge.cv, s=ridge.cv$lambda.1se, newx=dataxtest)
yelnet<- predict(elnet.cv, s=elnet.cv$lambda.1se, newx=dataxtest)

#Aggregate y actual and predicted y's from lasso, ridge and elnet into yresult
yresult        <- as.data.frame(cbind(dataytest,ylasso,yridge,yelnet))
names(yresult) <- c("yactual","ylasso","yridge","yelnet")
View(yresult)

#Now calculate root mean-squared-error (rmse) in each case
yresult$lasso_se <- ((yresult$yactual-yresult$ylasso)^2)^0.5
yresult$ridge_se <- ((yresult$yactual-yresult$yridge)^2)^0.5
yresult$elnet_se <- ((yresult$yactual-yresult$yelnet)^2)^0.5

#To compare performance of lasso, ridge and elnet - compare their mean rmse
rmse_lasso <- mean(yresult$lasso_se)
rmse_ridge <- mean(yresult$ridge_se)
rmse_elnet <- mean(yresult$elnet_se)

#Print the values and check which is the lowest
rmse_lasso
rmse_ridge
rmse_elnet

#As the RMSE value of elnet regression is the least, it has the best fit
#among three models

View(yresult)

#Calculation of R-Square
#To find R-Square we need to find
  # TSS
  # PSS
  # RSS
#R-Square = 1 - RSS / TSS

# Total sum of squares (TSS)
# Difference between the actual and the mean
yresult$ymean <- mean(yresult$yactual)
TSS = sum((yresult$yactual-yresult$ymean)^2) 

#Predicted (Explained) Sum of squares (PSS)
#Based on the difference between predicted and mean
PSS_lasso = sum((yresult$ylasso-yresult$ymean)^2)
PSS_ridge = sum((yresult$yridge-yresult$ymean)^2)
PSS_elnet = sum((yresult$yelnet-yresult$ymean)^2)

#Residual Sum of squares (RSS)
#Based on the difference between actual and predicted
RSS_lasso = sum((yresult$yactual-yresult$ylasso)^2)
RSS_ridge = sum((yresult$yactual-yresult$yridge)^2)
RSS_elnet = sum((yresult$yactual-yresult$yelnet)^2)

#Calcualation of R-Squared
lasso_Rsquare <- 1 - RSS_lasso/TSS
ridge_Rsquare <- 1 - RSS_ridge/TSS
elnet_Rsquare <- 1 - RSS_elnet/TSS

#Note elnet model's R-Square is the best among the three.
#R-Square will corrborate with RMSE, if you have noticed.
#Lower the RMSE, higher the R-Square

#Extracting the coefficients
coef(lasso.cv)
coef(ridge.cv)
coef(elnet.cv)


