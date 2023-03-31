

library("readxl")
mydata = read_excel("~/Documents/MSBA_Lectures/BAN_671/Assignment/Assignment3/hourlydata_bitstamp_after2016_updated.xlsx")


# Converting variables to vectors
volumerate <- as.vector(mydata$volumerate)
range <- as.vector(mydata$range)
return <- as.vector(mydata$return)
avg_sentilst_lexicon_pos <- as.vector(mydata$avg_sentilst_lexicon_pos)
avg_sentilst_lexicon_neg <- as.vector(mydata$avg_sentilst_lexicon_neg)
stdev_sentilst_lexicon_pos <- as.vector(mydata$stdev_sentilst_lexicon_pos)
stdev_sentilst_lexicon_neg <- as.vector(mydata$stdev_sentilst_lexicon_neg)
similarity <- as.vector(mydata$similarity)
postcount <- as.vector(mydata$postcount)


# Generate time series for each variable (frequency - hour)
volumerate.ts = ts(as.numeric(volumerate),start=c(2016,1),end=c(2020,1), freq=24*365)
range.ts = ts(as.numeric(range),start=c(2016,1),end=c(2020,1), freq=24*365)
return.ts = ts(as.numeric(return),start=c(2016,1),end=c(2020,1), freq=24*365)
avg_sentilst_lexicon_pos.ts = ts(as.numeric(avg_sentilst_lexicon_pos),start=c(2016,1),end=c(2020,1), freq=24*365)
avg_sentilst_lexicon_neg.ts = ts(as.numeric(avg_sentilst_lexicon_neg),start=c(2016,1),end=c(2020,1), freq=24*365)
stdev_sentilst_lexicon_pos.ts = ts(as.numeric(stdev_sentilst_lexicon_pos),start=c(2016,1),end=c(2020,1), freq=24*365)
stdev_sentilst_lexicon_neg.ts = ts(as.numeric(stdev_sentilst_lexicon_neg),start=c(2016,1),end=c(2020,1), freq=24*365)
similarity.ts = ts(as.numeric(similarity),start=c(2016,1),end=c(2020,1), freq=24*365)
postcount.ts = ts(as.numeric(postcount),start=c(2016,1),end=c(2020,1), freq=24*365)
data.ts = ts.intersect(return.ts,avg_sentilst_lexicon_pos.ts,avg_sentilst_lexicon_neg.ts,stdev_sentilst_lexicon_pos.ts,stdev_sentilst_lexicon_neg.ts,similarity.ts,postcount.ts,volumerate.ts,range.ts)

# Q1,5. fitting var model 
library(vars)
VARselect(data.ts, lag.max = 10, season=12, type=c("both"))$select
model.var=VAR(data.ts, p=3,type="both",season=12) # based on BIC/SIC
options(scipen=999) # avoid scientific notation
summary(model.var) 
class(model.var)

#2. Eigenvalues 
roots(model.var, modulus = TRUE)
# granger causality test
library(aod)	
covmatrix=vcov(model.var)

#3. testing if avg. negative sentiment and positive sentiment predict Bitcoin returns
covmatrix1=vcov(model.var)[c(3,4,12,13,21,22),c(3,4,12,13,21,22)] #index of variables regarding lagged neg & positive sentiment
wald.test(b=coefficients(model.var)$return.ts[c(2,3,11,12,20,21),1], Sigma = covmatrix1, Terms=c(1,2,3,4,5,6)) 

#4. testing if Bitcoin returns predict social media pos/neg sentiment
covmatrix2=vcov(model.var)[c(42,51,60),c(42,51,60)] #index of variables regarding lagged bitcoin returns
wald.test(b=coefficients(model.var)$avg_sentilst_lexicon_pos.ts[c(1,10,19),1], Sigma = covmatrix2, Terms=c(1,2,3)) 

covmatrix3=vcov(model.var)[c(82,91,100),c(82,91,100)] #index of variables regarding lagged bitcoin returns
wald.test(b=coefficients(model.var)$avg_sentilst_lexicon_neg.ts[c(1,10,19),1], Sigma = covmatrix3, Terms=c(1,2,3)) 

#6. testing if bitcoin returns (neg.) spark more social media posting
covmatrix4=vcov(model.var)[c(242,251,260),c(242,251,260)] #index of variables regarding lagged bitcoin returns
wald.test(b=coefficients(model.var)$postcount.ts[c(1,10,19),1], Sigma = covmatrix4, Terms=c(1,2,3)) 





