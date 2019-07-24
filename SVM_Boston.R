library(MASS)
View(Boston)
install.packages("caTools")
library(caTools)
install.packages("Metrics")
library(Metrics)
install.packages("dplyr")
library(dplyr)
data = Boston
data1<-data[,-14]
View(data)
data[1]
summary(data)
library(e1071)
mymodel1 <- svm(medv~., data=data,
               kernel = "radial")
regressor = svm(formula = medv ~ ., data = data, type = "eps-regression",kernel ="radial")
tr<-data.frame(re)
plot(data$medv)
predictions<-predict(regressor,data = data1)
comp<-data.frame(data$medv,predictions)
comp$ID <- seq.int(nrow(comp))
comp
ggplot(data=comp, aes(x=ID, y=predictions, group=1)) +
  geom_line(color="red")+
  geom_point(data=comp, aes(x=ID, y=data.medv, group=1)) 
errval <- comp$data.medv - comp$predictions
svr_RMSE <- rmse(comp$data.medv,comp$predictions)   

svm()

#tuning:::

set.seed(123)
tmodel <- tune(svm, medv~., data=data,
               ranges = list(epsilon = seq(0,1,0.02), cost = 2^(2:6)))

mymodel2 <- tmodel$best.model
summary(mymodel2)
predictions1<-predict(mymodel2,data=data1)
compare1<-data.frame(data$medv,predictions1)
compare1$ID <- seq.int(nrow(compare1))
View(compare1)

ggplot(data=compare1, aes(x=ID, y=predictions1, group=1)) +
  geom_line(color="red")+
  geom_point(data=compare1, aes(x=ID, y=data.medv, group=1)) 
svr_RMSE1 <- rmse(compare1$data.medv,compare1$predictions1)   

