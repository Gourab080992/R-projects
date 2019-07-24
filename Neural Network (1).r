###########################################################################
#############Chapter 3 - Deep Learning with neuralnet###################### 
###########################################################################
#Reference BooK : Neural Networks with R


install.packages("neuralnet")
install.packages("ISLR") #Package of datasets

#DV - Private: A factor with levels No and Yes indicating private or public university
#Apps: Number of applications received
#Accept: Number of applications accepted
#Enroll: Number of new students enrolled
#Top10perc: Percentage of new students from top 10 percent of H.S. class
#Top25perc: Percentage of new students from top 25 percent of H.S. class
#F.Undergrad: Number of full time undergraduates
#P.Undergrad: Number of part time undergraduates
#Outstate: Out-of-state tuition
#Room.Board: Room and board costs
#Books: Estimated book costs
#Personal: Estimated personal spending
#PhD: Percentage of faculty with Ph.D.s
#Terminal: Percentage of faculty with terminal degree
#S.F.Ratio: Student-faculty ratio
#perc.alumni: Percentage of alumni who donate
#Expend: Instructional expenditure per student
#Grad.Rate: Graduation rate

library("neuralnet")
library(ISLR)

data = College
View(data)

#Scaling the data
max_data <- apply(data[,2:18], 2, max) 
min_data <- apply(data[,2:18], 2, min)
data_scaled <- scale(data[,2:18],center = min_data, scale = max_data - min_data) 

Private = as.numeric(College$Private)-1
data_scaled = cbind(Private,data_scaled)

index = sample(1:nrow(data),round(0.70*nrow(data)))
train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])

n = names(train_data)
f <- as.formula(paste("Private ~", paste(n[!n %in% "Private"], collapse = " + ")))
deep_net = neuralnet(f,data=train_data,hidden=c(5,3),linear.output=F)
plot(deep_net)
#default activation function is logistic

predicted_data <- compute(deep_net,test_data[,2:18])
print(head(predicted_data$net.result))
predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)

table(test_data$Private,predicted_data$net.result)

###########################################################################

