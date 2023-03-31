
install.packages("rpart.plot")
library(rpart.plot)
library(party)
library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
# Q1 
data = read.csv(file = "~/Documents/MSBA_Lectures/BAN_671/Assignment/Assignment3/admit.csv")
head(data)
str(data)

# Q2
data$admit <- as.factor(data$admit)
typeof(data$admit)
class(data$admit)

data$rank <- as.factor(data$rank)
typeof(data$rank)
class(data$rank)
str(data)


#Q3
# train test divide
set.seed(3333)
train <- sample(rownames(data), dim(data) [1]*0.8)
train_df <- data[train, ]
test <- setdiff(rownames (data), train)
test_df <- data[test, ]


#Q4
model <- ctree(admit ~ ., train_df) 
plot(model)

#Q6
pre = predict(model,test_df)
confMat <- table(test_df$admit,pre)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy


