

#Q1
data = read.csv(file = "~/Documents/MSBA_Lectures/BAN_671/Assignment/Assignment3/market basket.csv",header=TRUE)
head(data)

#Q2
summary(data)

#Q3
library(arules) 
rules = apriori(data)
inspect(rules)

#Q4-5
rules <- apriori(data, parameter = list(minlen=2, supp=0.05, conf=0.7), appearance=list(lhs=c("K=Bag"),default="rhs"))
inspect(rules)   

#Q6
rules <- apriori(data, parameter = list(minlen=2, supp=0.05, conf=0.7), appearance=list(lhs=c("K=Bag"),
rhs=c("T=Bag","F=Bag","PS=Bag","SKG=Bag","SG=Bag","BP=Bag","SS=Bag","SW=Bag","SNB=Bag","SB=Bag","RS=Bag"),default="none"))
inspect(rules)  
rules.sorted<-sort(rules, by="lift")
inspect(rules.sorted)     
