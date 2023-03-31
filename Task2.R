

#Q1
library("ggplot2")
data=as.data.frame(iris)
ggplot(data=data)+
  geom_point(mapping=aes(x=Petal.Length,y=Petal.Width,color=Species))


#Q2

library('e1071')
svmfitted<-svm(Species~., data=data, kernel='linear', cost=10, scale=FALSE)
summary(svmfitted)


#Q3 
plot(svmfitted, data=data,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)


#Q4
# prediction with svm
svmfitted$fitted
predict <- predict(svmfitted,data)
pred_tab <- table(Predicted=predict, Actual = data$Species)
pred_tab
sum(diag(pred_tab)/sum(pred_tab))


svmfitted_low<-svm(Species ~ ., data=data, kernel='radial', cost=10, gamma=0.1)

plot(svmfitted_low, data=data,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

svmfitted_high<-svm(Species ~ ., data=data, kernel='radial', cost=10, gamma=0.5)

plot(svmfitted_high, data=data,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)


svmfitted_low_cost<-svm(Species ~ ., data=data, kernel='radial', cost=1, gamma=0.1)

plot(svmfitted_low_cost, data=data,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

pred <- predict(svmfit4,mydata)
tab <- table(Predicted=pred, Actual = mydata$Species)
tab

svmfitted_high_cost<-svm(Species ~ ., data=data, kernel='radial', cost=100000, gamma=0.1)

plot(svmfitted_high_cost, data=data,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length=4) 
)

pred <- predict(svmfit5,mydata)
tab <- table(Predicted=pred, Actual = mydata$Species)
tab
#Accuracy
sum(diag(tab)/sum(tab))