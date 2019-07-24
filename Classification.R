#Classfication#

#It is a supervised machine-learning approach. classification is the problem of 
#identifying to which of a set of categories (sub-populations) a new observation belongs,
#on the basis of a training set of data containing observations (or instances) 
#whose category membership is known

#Classification Techniques#
#Some Techniques#
#1. Logistic Regression#
#2. Decision Trees#
#3. Naive Bayes#

#Data#

#For the purpose of demonstration We shall use the real-world public data set
#https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
#The data set provides demographic and socio-economic indicators and
#Indicates if a person has subscribed to term-deposit or not

#Citation#
#[Moro et al., 2014] S. Moro, P. Cortez and P. Rita. 
#A Data-Driven Approach to Predict the Success of Bank Telemarketing. 
#Decision Support Systems, Elsevier, 62:22-31, June 2014

#Variables in data and description#
#Input variables:
#bank client data:
  #age (numeric)
  #job : type of job (categorical)
  #marital : marital status (categorical)
  #education (categorical)
  #default: has credit in default? 
  #housing: has housing loan?
  #loan: has personal loan? 
  #contact: contact communication type 
  #month: last contact month of year 
  #day_of_week: last contact day of the week 
  #duration: last contact duration, in seconds 
  #campaign: number of contacts performed during this campaign and for this client 
  #pdays: number of days that passed by after the client was last contacted 
  #previous: number of contacts performed before this campaign and for this client (numeric)
  #poutcome: outcome of the previous marketing campaign (categorical)

# social and economic context attributes
  #emp.var.rate: employment variation rate - quarterly indicator (numeric)
  #cons.price.idx: consumer price index - monthly indicator (numeric) 
  #cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
  #euribor3m: euribor 3 month rate - daily indicator (numeric)
  #nr.employed: number of employees - quarterly indicator (numeric)

#Output variable (desired target):
  #Has the client subscribed a term deposit? (binary: 'yes','no')

#######################################################################################

#Classification Steps
# Step 1 : Prepare data {factor, missing values}
# Step 2 : Select validation strategy and divide into test and train data
# Step 3 : Execute classification
# Step 3 : Model Evaluation


#Turning off Warnings

options(warn=-1)
options(digits=2)

#Basics
install.packages("fmsb")
install.packages("party")
install.packages("klaR")
install.packages("rpart")   
install.packages("rpart.plot") 
install.packages("rattle")
install.packages("ggplot2")

library(fmsb)   #Nagelkerke R2
library(ggplot2)#Visualization
library(party)  #Decision Trees
library(klaR)   #Naive Bayes
library(rpart)  #CART
library(rpart.plot) #CART Visualization
library(rattle) #CART Visualization
library(pROC)   #roc
library(ModelMetrics) #auc


#######################################################################################
# Step 1 : Data Preparation

#Imported Data : bank_additional
#rm(bank)
bank <- bank_additional

#Explore the data
str(bank)

#Study the data and identify categorical variables that need to be converted into factors
#We find that following needs to be converted into factors

bank$job        <- as.factor(bank$job)
bank$marital    <- as.factor(bank$marital)
bank$education  <- as.factor(bank$education)
bank$default    <- as.factor(bank$default)
bank$housing    <- as.factor(bank$housing)
bank$loan       <- as.factor(bank$loan)
bank$contact    <- as.factor(bank$contact)
bank$month      <- as.factor(bank$month)
bank$day_of_week<- as.factor(bank$day_of_week)
bank$poutcome   <- as.factor(bank$poutcome)
bank$y          <- as.factor(bank$y)
bank$y          <- as.factor(as.numeric(bank$y)-1)

#Identify factor levels with only few counts and group them as " to "unknown"
#Why do we need to do this
#When a category has many options and some option have few counts
#It is possible that some options donot occure in test data and may cause a problem
#In model application

bank$default[bank$default == "yes"] <- "unknown"
bank$education[bank$education == "illeterate"] <- "unknown"
#To do simple check of freq counts, use the table function

#Check if any of the attributes has NULL Values
#If yes replace by mean or median
table(is.na(bank))
#Luckily we don't have any

#######################################################################################

# Step 2 : Select validation strategy and divide into test and train data
# One of the critical tasks in machine learning is the ability to validate results
# We use k-fold validation. In this example We shall use 4-fold validation

# In a k-fold validation method. The dataset is divided into k-folds of equal size
# Data is trained on k-1 folds and tested on the kth fold
# Let's split the data into four-folds

# step 2a : attach a random number column

set.seed(10)
bank$random <- runif(nrow(bank),min=0,max=nrow(bank))

#Folds are to be generated randomly

bank$fold   <- ifelse(bank$random <= quantile(bank$random,0.25),1,
                 ifelse((bank$random > quantile(bank$random,0.25) & bank$random < quantile(bank$random,0.5)),2,
                   ifelse((bank$random > quantile(bank$random,0.5) & bank$random < quantile(bank$random,0.75)),3,4)))
# step 2b : create testing folds

testfold1  <- bank[bank$fold==1, ]                  
testfold2  <- bank[bank$fold==2, ] 
testfold3  <- bank[bank$fold==3, ]                  
testfold4  <- bank[bank$fold==4, ]

# step 2c : Create training folds

trainfold1  <- bank[bank$fold !=1, ]                  
trainfold2  <- bank[bank$fold !=2, ] 
trainfold3  <- bank[bank$fold !=3, ]                  
trainfold4  <- bank[bank$fold !=4, ]

#Store the relationship to be built as a varible for convenience

predicted <- "y";
predictors<- c("age","job","marital","education","default","housing",
               "loan","contact","month","day_of_week","duration","campaign",
               "pdays","previous","poutcome","emp.var.rate","cons.price.idx",
               "cons.conf.idx","euribor3m","nr.employed")
fmla <- paste(predicted, "~" , paste(predictors, collapse=' + '),sep= ' ')

#Check the relationship stored
fmla

#######################################################################################

#Step 3: Execute Classification Technique - Technique 1 - Logistic Regression#

logmodela<-glm(fmla,data=trainfold1,family=binomial(link="logit"))
logmodelb<-glm(fmla,data=trainfold2,family=binomial(link="logit"))
logmodelc<-glm(fmla,data=trainfold3,family=binomial(link="logit"))
logmodeld<-glm(fmla,data=trainfold4,family=binomial(link="logit"))

#model summary
summary(logmodela)
summary(logmodelb)
summary(logmodelc)
summary(logmodeld)

#Pseudo R-Square function - NagelKerkeR2 - fmsb
NagelkerkeR2(logmodela)
NagelkerkeR2(logmodelb)
NagelkerkeR2(logmodelc)
NagelkerkeR2(logmodeld)

#Step 4 Model Evaluation - Logistic Regression

#Generate Confusion Matrix to Evaluate the model#

#step 4a: Predict Probabilities for each model#
testfold1$pred <- predict(logmodela, newdata=testfold1, type="response")
testfold2$pred <- predict(logmodelb, newdata=testfold2, type="response")
testfold3$pred <- predict(logmodelc, newdata=testfold3, type="response")
testfold4$pred <- predict(logmodeld, newdata=testfold4, type="response")

#Assigning a class label#
#Study the predicted probability distribution#
#Plot the predicted probabilities against original class and identify as threshold#
testfold        <- rbind(testfold1,testfold2,testfold3,testfold4)
m <- ggplot(data=testfold, aes(x=pred)) 
m <- m + geom_density(aes(fill=testfold$y), size=.5, alpha=.2) 
m + scale_fill_manual(values = c("red","blue"))

#AUC_Logistic
auc(testfold$y,testfold$pred)
plot(roc(testfold$y,testfold$pred))

threshold = 0.5 # default
threshold = 0.20 # from graph
#Use threshold to Assign Classes in each of the testfold#

testfold1$class <- ifelse(testfold1$pred > threshold,1,0)
testfold2$class <- ifelse(testfold2$pred > threshold,1,0)
testfold3$class <- ifelse(testfold3$pred > threshold,1,0)
testfold4$class <- ifelse(testfold4$pred > threshold,1,0)

table(testfold1$class)
table(testfold2$class)
table(testfold3$class)
table(testfold4$class)

#Generate confusion matrix
measuresfold1 <- table(pred=testfold1$class, actual=testfold1$y)
measuresfold2 <- table(pred=testfold2$class, actual=testfold2$y)
measuresfold3 <- table(pred=testfold3$class, actual=testfold3$y)
measuresfold4 <- table(pred=testfold4$class, actual=testfold4$y)

#Confusion matrix measures
#Test Fold 1
accuracyfold1    <- (measuresfold1[1,1] + measuresfold1[2,2])/sum(measuresfold1) # TP+FP / TP+TN+FP+FN 
recallfold1      <-  measuresfold1[2,2]/(measuresfold1[1,2]+measuresfold1[2,2])  # TP/TP+FN also known as sensitivity 
precisionfold1   <-  measuresfold1[2,2]/(measuresfold1[2,1]+measuresfold1[2,2])  # TP/TP+FP
specificityfold1 <-  measuresfold1[1,1]/(measuresfold1[1,1]+measuresfold1[2,1])  # TN/TN+FP

#Test Fold 2
accuracyfold2    <- (measuresfold2[1,1] + measuresfold2[2,2])/sum(measuresfold2) # TP+FP / TP+TN+FP+FN 
recallfold2      <-  measuresfold2[2,2]/(measuresfold2[1,2]+measuresfold2[2,2])  # TP/TP+FN also known as sensitivity 
precisionfold2   <-  measuresfold2[2,2]/(measuresfold2[2,1]+measuresfold2[2,2])  # TP/TP+FP
specificityfold2 <-  measuresfold2[1,1]/(measuresfold2[1,1]+measuresfold2[2,1])  # TN/TN+FP

#Test Fold 3
accuracyfold3    <- (measuresfold3[1,1] + measuresfold3[2,2])/sum(measuresfold3) # TP+FP / TP+TN+FP+FN 
recallfold3      <-  measuresfold3[2,2]/(measuresfold3[1,2]+measuresfold3[2,2])  # TP/TP+FN also known as sensitivity 
precisionfold3   <-  measuresfold3[2,2]/(measuresfold3[2,1]+measuresfold3[2,2])  # TP/TP+FP
specificityfold3 <-  measuresfold3[1,1]/(measuresfold3[1,1]+measuresfold3[2,1])  # TN/TN+FP

#Test Fold 4
accuracyfold4    <- (measuresfold4[1,1] + measuresfold4[2,2])/sum(measuresfold4) # TP+FP / TP+TN+FP+FN 
recallfold4      <-  measuresfold4[2,2]/(measuresfold4[1,2]+measuresfold4[2,2])  # TP/TP+FN also known as sensitivity 
precisionfold4   <-  measuresfold4[2,2]/(measuresfold4[2,1]+measuresfold4[2,2])  # TP/TP+FP
specificityfold4 <-  measuresfold4[1,1]/(measuresfold4[1,1]+measuresfold4[2,1])  # TN/TN+FP

accuracy        <- c(accuracyfold1,accuracyfold2,accuracyfold3,accuracyfold4)
recall          <- c(recallfold1, recallfold2, recallfold3, recallfold4)
precision       <- c(precisionfold1,precisionfold2,precisionfold3,precisionfold4)
specificity     <- c(specificityfold1,specificityfold2,specificityfold3,specificityfold4)
technique       <- c("logistic","logistic","logistic","logistic") 
fold_index      <- c("fold1","fold2","fold3","fold4")

rm(table)
table <- data.frame(technique,fold_index,accuracy,recall,precision,specificity)
View(table)

#Measures across four folds should be similar
#You can control the threshold to impact the accuracy, recall, precision and sensitivity measures
#The ideal measure depends on the business requirement

#######################################################################################

#Step 3: Execute Classification Technique - - Technique 2 - Decision Trees#
#Set minsplit criteria and minbucket criteria
#minsplit : the minimum number of observations that must exist in a node in order for a split to be attempted. 
#minbucket : the minimum number of observations in any terminal <leaf> node. 

#minsplit and minbucket 
zminsplit = 60
zminbucket = zminsplit / 3
zcp = 0.04

treemodela <- rpart(fmla,method="class", data=trainfold1,control=rpart.control(minsplit=zminsplit,minbucket=zminbucket,maxdepth=10,cp=zcp))
treemodelb <- rpart(fmla,method="class", data=trainfold2,control=rpart.control(minsplit=zminsplit,minbucket=zminbucket,maxdepth=10,cp=zcp))
treemodelc <- rpart(fmla,method="class", data=trainfold3,control=rpart.control(minsplit=zminsplit,minbucket=zminbucket,maxdepth=10,cp=zcp))
treemodeld <- rpart(fmla,method="class", data=trainfold4,control=rpart.control(minsplit=zminsplit,minbucket=zminbucket,maxdepth=10,cp=zcp))

#Use parameters minsplit and minbucket to control the size of the rules

#Plotting the tree
fancyRpartPlot(treemodela, cex=1)
#Note : Rules may differ on values across trees...So at thia stage don't do too
#much interpretation. Later when get a single tree on whole data, do it.

#Step 4 Model Evaluation - Decision Trees#

testfold1$treepred <- predict(treemodela,newdata=testfold1,type="prob")[,2]
testfold2$treepred <- predict(treemodelb,newdata=testfold2,type="prob")[,2]
testfold3$treepred <- predict(treemodelc,newdata=testfold3,type="prob")[,2]
testfold4$treepred <- predict(treemodeld,newdata=testfold4,type="prob")[,2]

#Assigning a class label#
#Study the predicted probability distribution#
#Plot the predicted probabilities against original class and identify as threshold#
testfold        <- rbind(testfold1,testfold2,testfold3,testfold4)
m <- ggplot(data=testfold, aes(x=treepred)) 
m <- m + geom_density(aes(fill=testfold$y), size=.5, alpha=.2) 
m + scale_fill_manual( values = c("red","blue"))

#AUC_CART
auc(testfold$y,testfold$treepred)
plot(roc(testfold$y,testfold$treepred))

threshold = 0.5  # default
threshold = 0.075 # from graph
#Use threshold to Assign Classes in each of the testfold#

testfold1$tclass <- ifelse(testfold1$treepred > threshold,1,0)
testfold2$tclass <- ifelse(testfold2$treepred > threshold,1,0)
testfold3$tclass <- ifelse(testfold3$treepred > threshold,1,0)
testfold4$tclass <- ifelse(testfold4$treepred > threshold,1,0)

table(testfold1$tclass)
table(testfold2$tclass)
table(testfold3$tclass)
table(testfold4$tclass)

#Generate confusion matrix
tmeasuresfold1 <- table(pred=testfold1$tclass, actual=testfold1$y)
tmeasuresfold2 <- table(pred=testfold2$tclass, actual=testfold2$y)
tmeasuresfold3 <- table(pred=testfold3$tclass, actual=testfold3$y)
tmeasuresfold4 <- table(pred=testfold4$tclass, actual=testfold4$y)

#Decision Tree - Confusion matrix measures
#Test Fold 1
taccuracyfold1    <- (tmeasuresfold1[1,1] + tmeasuresfold1[2,2])/sum(tmeasuresfold1) # TP+FP / TP+TN+FP+FN 
trecallfold1      <-  tmeasuresfold1[2,2]/(tmeasuresfold1[1,2]+tmeasuresfold1[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold1   <-  tmeasuresfold1[2,2]/(tmeasuresfold1[2,1]+tmeasuresfold1[2,2])  # TP/TP+FP
tspecificityfold1 <-  tmeasuresfold1[1,1]/(tmeasuresfold1[1,1]+tmeasuresfold1[2,1])  # TN/TN+FP

#Test Fold 2
taccuracyfold2    <- (tmeasuresfold2[1,1] + tmeasuresfold2[2,2])/sum(tmeasuresfold2) # TP+FP / TP+TN+FP+FN 
trecallfold2      <-  tmeasuresfold2[2,2]/(tmeasuresfold2[1,2]+tmeasuresfold2[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold2   <-  tmeasuresfold2[2,2]/(tmeasuresfold2[2,1]+tmeasuresfold2[2,2])  # TP/TP+FP
tspecificityfold2 <-  tmeasuresfold2[1,1]/(tmeasuresfold2[1,1]+tmeasuresfold2[2,1])  # TN/TN+FP

#Test Fold 3
taccuracyfold3    <- (tmeasuresfold3[1,1] + tmeasuresfold3[2,2])/sum(tmeasuresfold3) # TP+FP / TP+TN+FP+FN 
trecallfold3      <-  tmeasuresfold3[2,2]/(tmeasuresfold3[1,2]+tmeasuresfold3[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold3   <-  tmeasuresfold3[2,2]/(tmeasuresfold3[2,1]+tmeasuresfold3[2,2])  # TP/TP+FP
tspecificityfold3 <-  tmeasuresfold3[1,1]/(tmeasuresfold3[1,1]+tmeasuresfold3[2,1])  # TN/TN+FP

#Test Fold 4
taccuracyfold4    <- (tmeasuresfold4[1,1] + tmeasuresfold4[2,2])/sum(tmeasuresfold4) # TP+FP / TP+TN+FP+FN 
trecallfold4      <-  tmeasuresfold4[2,2]/(tmeasuresfold4[1,2]+tmeasuresfold4[2,2])  # TP/TP+FN also known as sensitivity 
tprecisionfold4   <-  tmeasuresfold4[2,2]/(tmeasuresfold4[2,1]+tmeasuresfold4[2,2])  # TP/TP+FP
tspecificityfold4 <-  tmeasuresfold4[1,1]/(tmeasuresfold4[1,1]+tmeasuresfold4[2,1])  # TN/TN+FP


accuracy        <- c(taccuracyfold1,taccuracyfold2,taccuracyfold3,taccuracyfold4)
recall          <- c(trecallfold1, trecallfold2, trecallfold3, trecallfold4)
precision       <- c(tprecisionfold1,tprecisionfold2,tprecisionfold3,tprecisionfold4)
specificity     <- c(tspecificityfold1,tspecificityfold2,tspecificityfold3,tspecificityfold4)
technique       <- c("decisiontree","decisiontree","decisiontree","decisiontree") 
fold_index      <- c("fold1","fold2","fold3","fold4")


treetable <- data.frame(technique,fold_index,accuracy,recall,precision,specificity)
View(treetable)
table <- rbind(table,treetable)
View(table)

###########################################################################################################################

#Step 3: Execute Model Building - Technique 3 - Naive Bayes #

#Build Model
bayesmodela <- NaiveBayes(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold1)
bayesmodelb <- NaiveBayes(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold2)
bayesmodelc <- NaiveBayes(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold3)
bayesmodeld <- NaiveBayes(y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data=trainfold4)

#Predict Probabilities
bayesmodelpredfold1 <- predict(bayesmodela,newdata=testfold1,type="raw")
bayesmodelpredfold2 <- predict(bayesmodelb,newdata=testfold2,type="raw")
bayesmodelpredfold3 <- predict(bayesmodelc,newdata=testfold3,type="raw")
bayesmodelpredfold4 <- predict(bayesmodeld,newdata=testfold4,type="raw")
bayesmodelpredfold1 <- as.data.frame(bayesmodelpredfold1)
bayesmodelpredfold2 <- as.data.frame(bayesmodelpredfold2)
bayesmodelpredfold3 <- as.data.frame(bayesmodelpredfold3)
bayesmodelpredfold4 <- as.data.frame(bayesmodelpredfold4)

testfold1$bayespred <- bayesmodelpredfold1[,3] 
testfold2$bayespred <- bayesmodelpredfold2[,3] 
testfold3$bayespred <- bayesmodelpredfold3[,3] 
testfold4$bayespred <- bayesmodelpredfold4[,3] 

#Assigning a class label#
#Plot the predicted probabilities against original class and identify as threshold#
testfold        <- rbind(testfold1,testfold2,testfold3,testfold4)
m <- ggplot(data=testfold, aes(x=bayespred)) 
m <- m + geom_density(aes(fill=testfold$y), size=.5, alpha=.2) 
m + scale_fill_manual( values = c("red","blue"))

#AUC_BAYES
auc(testfold$y,testfold$bayespred)
plot(roc(testfold$y,testfold$bayespred))


threshold = 0.5  # default
threshold = 0.2 # from graph
#Use threshold to Assign Classes in each of the testfold#

testfold1$bayesclass <- ifelse(testfold1$bayespred > threshold,1,0)
testfold2$bayesclass <- ifelse(testfold2$bayespred > threshold,1,0)
testfold3$bayesclass <- ifelse(testfold3$bayespred > threshold,1,0)
testfold4$bayesclass <- ifelse(testfold4$bayespred > threshold,1,0)

#generate confusion matrix
bmeasuresfold1 <- table(pred=testfold1$bayesclass, actual=testfold1$y)
bmeasuresfold2 <- table(pred=testfold2$bayesclass, actual=testfold2$y)
bmeasuresfold3 <- table(pred=testfold3$bayesclass, actual=testfold3$y)
bmeasuresfold4 <- table(pred=testfold4$bayesclass, actual=testfold4$y)

#Confusion matrix measures
#Test Fold 1
baccuracyfold1    <- (bmeasuresfold1[1,1] +bmeasuresfold1[2,2])/sum(bmeasuresfold1) # TP+FP / TP+TN+FP+FN 
brecallfold1      <-  bmeasuresfold1[2,2]/(bmeasuresfold1[1,2]+bmeasuresfold1[2,2])  # TP/TP+FN also known as sensitivity 
bprecisionfold1   <-  bmeasuresfold1[2,2]/(bmeasuresfold1[2,1]+bmeasuresfold1[2,2])  # TP/TP+FP
bspecificityfold1 <-  bmeasuresfold1[1,1]/(bmeasuresfold1[1,1]+bmeasuresfold1[2,1])  # TN/TN+FP

#Test Fold 2
baccuracyfold2    <- (bmeasuresfold2[1,1] +bmeasuresfold2[2,2])/sum(bmeasuresfold2) # TP+FP / TP+TN+FP+FN 
brecallfold2      <-  bmeasuresfold2[2,2]/(bmeasuresfold2[1,2]+bmeasuresfold2[2,2])  # TP/TP+FN also known as sensitivity 
bprecisionfold2   <-  bmeasuresfold2[2,2]/(bmeasuresfold2[2,1]+bmeasuresfold2[2,2])  # TP/TP+FP
bspecificityfold2 <-  bmeasuresfold2[1,1]/(bmeasuresfold2[1,1]+bmeasuresfold2[2,1])  # TN/TN+FP

#Test Fold 3
baccuracyfold3    <- (bmeasuresfold3[1,1] +bmeasuresfold3[2,2])/sum(bmeasuresfold3) # TP+FP / TP+TN+FP+FN 
brecallfold3      <-  bmeasuresfold3[2,2]/(bmeasuresfold3[1,2]+bmeasuresfold3[2,2])  # TP/TP+FN also known as sensitivity 
bprecisionfold3   <-  bmeasuresfold3[2,2]/(bmeasuresfold3[2,1]+bmeasuresfold3[2,2])  # TP/TP+FP
bspecificityfold3 <-  bmeasuresfold3[1,1]/(bmeasuresfold3[1,1]+bmeasuresfold3[2,1])  # TN/TN+FP

#Test Fold 4
baccuracyfold4    <- (bmeasuresfold4[1,1] +bmeasuresfold4[2,2])/sum(bmeasuresfold4) # TP+FP / TP+TN+FP+FN 
brecallfold4      <-  bmeasuresfold4[2,2]/(bmeasuresfold4[1,2]+bmeasuresfold4[2,2])  # TP/TP+FN also known as sensitivity 
bprecisionfold4   <-  bmeasuresfold4[2,2]/(bmeasuresfold4[2,1]+bmeasuresfold4[2,2])  # TP/TP+FP
bspecificityfold4 <-  bmeasuresfold4[1,1]/(bmeasuresfold4[1,1]+bmeasuresfold4[2,1])  # TN/TN+FP

accuracy        <- c(baccuracyfold1,baccuracyfold2,baccuracyfold3,baccuracyfold4)
recall          <- c(brecallfold1, brecallfold2, brecallfold3, brecallfold4)
precision       <- c(bprecisionfold1,bprecisionfold2,bprecisionfold3,bprecisionfold4)
specificity     <- c(bspecificityfold1,bspecificityfold2,bspecificityfold3,bspecificityfold4)
technique       <- c("NaiveBayes","NaiveBayes","NaiveBayes","NaiveBayes") 
fold_index      <- c("fold1","fold2","fold3","fold4")

bayestable <- data.frame(technique,fold_index,accuracy,recall,precision,specificity)
View(bayestable)
table <- rbind(table,bayestable)
View(table)

###########################################################################################################################

