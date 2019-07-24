#Chapter 9 Regression#

#Regresssion#

#Regression is the preferred model for modeling a metric (ratio scale) 
#variable as a function of other metric and categorical variables
#It takes the form Y = a1 + b1x1 + b2x2 + .... + error
#Where Y is the Dependent variables
#x1, x2, x3 are the Independent variables
#b1, b2, b3 are the coefficients attacahed to x1,x2,x3...and so on
#The coefficients represent the marginal impact of a DV of IV
#For example in Y = 2 + 3X1 + 4X2, 3 means that Y increases by 3 units for every
#an unit increase in X1 at constant X2 and Y increased by 4 unites for 
#an unit increase in X2 at constant X1

#--------------------------------------------------------------------------------#

#About Data Used#
#Source : Statistics for Business Decision Making and Analysis
#Author : Robert E. Stine and Dean Foster, Pearson (2011)

#Business Context#
#A Chain of pharmacies is looking to expand into a new community. It has data on the 
#annual profits (in dollars) of pharmacies that are located in 110 cities around the
#Unites States of America. For each city, the chain also has the following variables
#that managers believe are associated with profits:

#Income (median annual salary) and Disposable Income (median income net of taxes)
#Birth Rate (per 1000 people in the local population)
#Social Security Recipients (Per 100000 people in the local population)
#Cardiovascular deaths (Per 100000 people in the local population) and
#Percentage of local population aged 65 or more.

#The Business Question#
#Managers would like to understand
#(A) Whether and How these variables are related to profits
#(B) Provide a means to choose new communities for expansion
#(C) Analyze the performance of the stores

#--------------------------------------------------------------------------------#

#Solution#
#Identifying the analytical solution for the business problem#
#The solution is to build a multiple regression problem to model the business problem#
#By means of building the model, the aforementioned questions are answered#

#--------------------------------------------------------------------------------#

#We shall follow these steps in building the model#
#Understand business context - Done
#Understand machine learning context - Done - MRM techiques is to be used.

#Let us recollect the key principles for regression
#Analytics Principles in Regression - LINE Principles #
#Linearity (of relation of IV and DV)
#Independence of errors
#Normality of errors
#Equality of Variance of errors 

#Let us now build the model following these 10 Steps#

#Step 1 - The relationship between y and x is linear (Principle 1)
#Step 2 - There are no lurking (undefined predictors) variables 
#Step 3 - Check correlation among predictors
#Step 4 - Build the regrssion model
#Step 5 - Check callibration plot and independency of errors
#Step 6 - Check for normality of errors
#Step 7 - Check for equality of variance
#Step 8 - Interpret the overall model 
#step 9 - Interpret the model coefficients
#Step 10 - Check for Multicollinearity
#Step 11 - Re-specify the model if need be

#--------------------------------------------------------------------------------#


#Step 1 - The relationship between y and x is linear (Principle 1)
#Can be verified through scatterplots

#Scatter Plots
#Let's load the package "car"
#Visual cue for possible correlation
install.packages("car")
library(car)

scatterplotMatrix(~Profit+Income+Disposable_Income+Birthrate+SocSecurity
                   +Deathrate+Above65,data=retail,main="ScatterPlot Matrix")

##Inference##
##The relationship between Profit and Other Variables look Linear
#So let us move to the next step

#--------------------------------------------------------------------------------#

#Step 2 - There are no lurking (undefined predictors) variables 

#A survey of the data presented does not show any obvious lurking variables
#So let us move to the next step

#--------------------------------------------------------------------------------#

#Step 3 - Look for Correlation
retail_numeric <- retail[,c(2:8)]
cor(retail[,c(2:8)])

#There are few correlations >0.7. This may cause multicollinearity later.
#Watch for them laters

#So let us move to the next step

#--------------------------------------------------------------------------------#

#Step 4 - Build the regression model

#remove incomplete cases from data and re-assign to new data frame

retailcomplete <- retail[complete.cases(retail),] 

#IV (Predictors) : Income, Disposable_Income, Birthrate, SocSecurity, Deathrate, Above65
#DV (Predicted)  : Profit

regmodel <- lm(Profit ~ Income + Disposable_Income + Birthrate + SocSecurity
                       +Deathrate + Above65, data=retailcomplete)

#notes
#regmodel is the R object where the constructed model is stored
#The IV's are separated by "+"
#The ~ denote the relationship tested : DV's as a function of IV's
#lm means linear model family
#data denotes the dataset

#Model is built and stored in the object named regmodel

#Predict y-hat, so that we can calculate the residuals
#Let's us call y-hat as pred_profit
retailcomplete$predprofit <- predict(regmodel,data=retailcomplete)

#Calculate Residual (Error) field
retailcomplete$error <- retailcomplete$Profit - retailcomplete$predprofit

#Let's move to the next step

#--------------------------------------------------------------------------------#

#Step 5 - Check callibration plot and independency of errors

#Step 5A - Callibration plot

#Callibration plot is a scatterplot between y-hat and y
#In this case it is a scatterplot between predprofit and Profit

plot(retailcomplete$Profit,retailcomplete$predprofit)

#Inference from scatterplot
#We expect allignment along diagonal and find it is good.


#Step 5B - Check independence of residuals
#Plot residuals against y-hat in this case predprofit

plot(retailcomplete$error,retailcomplete$predprofit)

#Inference from scatterplot
#No pattern is found in scatterplot. Residuals look independent.


#--------------------------------------------------------------------------------#

#Step 6 - Check for normality of errors

hist(retailcomplete$error)
plot(density(retailcomplete$error))
qqnorm(retailcomplete$error)

#Inference
#Residual plot looks close to normal based on histogram, density plot and qqplot

shapiro.test(retailcomplete$error)
#Normality tests further validates that residuals are normal

#--------------------------------------------------------------------------------#


#Step 7 - Check for equality of variance

#The goal is to check if "equality of variance" of residuals hold across the range of profit
#Let us store the errors in three vectors and check its variance

errorvector1 <- subset(retailcomplete[,c("error")],retailcomplete$Profit <= quantile(retailcomplete$Profit,0.33), data=retailcomplete)$error
errorvector2 <- subset(retailcomplete[,c("error")],retailcomplete$Profit > quantile(retailcomplete$Profit,0.33) & retailcomplete$Profit <= quantile(retailcomplete$Profit,0.67), data=retailcomplete)$error
errorvector3 <- subset(retailcomplete[,c("error")],retailcomplete$Profit > quantile(retailcomplete$Profit,0.67), data=retailcomplete)$error

#Now check for variance of the residuals in the two vectors named errorvector1 and errorvector2

var(errorvector1)
var(errorvector2)
var(errorvector3)

#Inference
#Variance of residuals across the profit might be similar

#Breusch Pagan Test
install.packages("olsrr")
library(olsrr)
ols_test_breusch_pagan(regmodel)
#   Ho: the error variance is constant            
#   Ha: the error variance is not constant

#   In this case the p-value is  0.231818
#   so do not reject Null-Hypothesis
#   Variance is constant

#score test
ols_test_score(regmodel)
#   Ho: the error variance is constant            
#   Ha: the error variance is not constant
#   In this case the p-value is  0.2543923
#   so do not reject Null-Hypothesis
#   Variance is constant


#--------------------------------------------------------------------------------#

#Step 8 - Interpret the overall model 

#Use the function summary to interpret the model

summary(regmodel)
#p-value of F-statistic < 0.05
#Hence model is significant
#Adj-Rsquare = 0.7415 which is very good 

#--------------------------------------------------------------------------------#

#Step 9 - Look for significant Coefficients 

#Use the function summary to interpret the model

summary(regmodel)
#Identify those variables where p-value is < 0.05
#Variable Disposable_Income, Birthrate and Above65 are significant

#Remove the insignificant variables one-by-one and re-run the model

#Removing SocSecurity
regmodel1 <- lm(Profit ~ Income + Disposable_Income + Birthrate
               +Deathrate + Above65, data=retailcomplete)
summary(regmodel1)

#Removing Deathrate
regmodel1 <- lm(Profit ~ Income + Disposable_Income + Birthrate
                + Above65, data=retailcomplete)
summary(regmodel1)

#Removing Income
regmodel1 <- lm(Profit ~ Disposable_Income + Birthrate
                + Above65, data=retailcomplete)

summary(regmodel1)
#Adj-Rsquare = 0.745 which is very good

#--------------------------------------------------------------------------------#

#Step 10 - Check for Multicollinearity
#Use variance inflation factor to identity variable contributing to multicollinearity
library(car)
vif(regmodel1)
#Note after removing insignificant variables there is no multicollinearity

#--------------------------------------------------------------------------------#

#Step 11 - Respecify the model
#In this case we discard regmodel and keep regmodel1
#In regmodel1, there are three significant variables
#(1) Disposable Income
#(2) Birthrate
#(3) Above65

#--------------------------------------------------------------------------------#

#Answering the business questions

options(scipen=999)

#(A) Whether and How these variables are related to profits
#(B) Provide a means to choose new communities for expansion
#(C) Predict sales at current locations to identify underperforming sites.

#(A) Whether and How these variables are related to profits
# Profits = 10044 + 3.23*Disposable_Income + 1874.04*Birthrate + 6619.20*Above65
# In areas with comparable Birthrates and Above65 population, Profits increase by 
# $3283 for every $1000 increase in disposable income

# In areas with comparable Birthrates and Disposable Income, Profits increase by 
# $6619 for each % increase of Above65

# In areas with comparable Above and Disposable Income, Profits increase by 
# $1874 for every unit risein Birthrate

#(B) Provide a means to choose new communities for expansion
# Communities with above average birth-rates and above65 could be targetted

#(C) Predict sales at current locations to identify underperforming sites.
retailcomplete$expectedprofit <- predict(regmodel1,data=retailcomplete)
retailcomplete$Performance <- ifelse(retailcomplete$Profit < retailcomplete$expectedprofit,"Poor","Good")
View(retailcomplete[,c('Location','Performance')])
#--------------------------------------------------------------------------------#

