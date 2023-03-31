

#Q1
data = read.csv(file = "~/Documents/MSBA_Lectures/BAN_671/Assignment/Assignment3/AirlineClustering.csv")
head(data)
row.names(data) <- data[,1]
data <- data[,-1]

data_df <- scale(data[,c(1:10)])
data_df <- cbind(data_df ,data$PhoneSale)
colnames(data_df)[11] <- "PhoneSale"
head(data_df)

#Q2
# select the number of clusters
set.seed(123)
library(factoextra)
fviz_nbclust(data_df,kmeans,method="wss") 

#Q3
# k mean clustering with 6 clusters
model <- kmeans(x = data_df, centers = 6, nstart = 25)
model


#Q4
model$cluster
model$centers
hist (model$cluster, xlab = "Clusters", labels=TRUE)



#Q5
model$cluster <- as.factor(model$cluster)
data$C1 <- ifelse(model$cluster == 1, 1, 0)
data$C2 <- ifelse(model$cluster == 2, 1, 0)
data$C3 <- ifelse(model$cluster == 3, 1, 0)
data$C4 <- ifelse(model$cluster == 4, 1, 0)
data$C5 <- ifelse(model$cluster == 5, 1, 0)
data$C6 <- ifelse(model$cluster == 6, 1, 0)
data
data$PhoneSale <- as.factor(data$PhoneSale)
logit.reg <- glm(data$PhoneSale ~.-C6, data = data, family = "binomial")
options(scipen=999)
summary(logit.reg)




