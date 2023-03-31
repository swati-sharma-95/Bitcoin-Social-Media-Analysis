

data = read.csv(file = "~/Documents/MSBA_Lectures/BAN_671/Assignment/Assignment3/admit.csv")
head(data)
str(data)

#clustering based on gre and gpa
mydata <- data[c(2,3)] # 
# standardize data
scaled <- scale(mydata) # standardized data=(x-Mean)/StandardDeviation	
pairwisedistance <- dist(scaled) 

# hierarchical clustering
hc1 <- hclust(pairwisedistance, method = "complete") 
plot(hc1) 

cluster.c1 <- cutree(hc1,2) 
cluster.c1 
cluster.c2 <- cutree(hc1,3) 
cluster.c2 


