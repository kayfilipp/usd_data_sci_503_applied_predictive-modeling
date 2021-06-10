#Module 1 exercises 

setwd("C:/Users/13234/Documents/usd_data_sci/503_applied_predictve_modeling/mod1")

#exercise 1: 
#a.	Using visualizations, explore the predictor variables to 
#understand their distributions as well as the relationships between predictors.
#b.	Do there appear to be any outliers in the data? Are any predictors skewed?
#c.	Are there any relevant transformations of one or more predictors that
#might improve the classification model?

#we have 9 predictors, a refreactive index and element percentage for 8 elements.

library(mlbench)
data(Glass)
df = as.data.frame(Glass)

#a. visualize. 
require(corrplot)
#extract predictors into correlogram 
predictors = subset(df,select=-c(Type))
predictors.cor = cor(predictors)
corrplot(predictors.cor,order="hclust")

#we can also pick a threshold such as 0.4 to decide if we have a colinearity issue.
#potentially problematic areas: RI->AL, RI->SI, RI->CA 
#								MG->AL, MG->CA, MG->BA
#								AL->BA 

thresh_cors = ifelse(abs(predictors.cor)>=0.4,1,0)
print(thresh_cors) 

#we also observe a distribution of each variable 
#looking at skewness helps too. 

#findings:
library(Hmisc) 
hist.data.frame(predictors)

require(e1071)
svals <- apply(predictors,2,skewness)
print(svals)

#for normality, we can also use the shapiro wilkes test 
#p>0.05 means normality.

for (i in names(predictors)){
	pval <- shapiro.test(predictors[,i])[2]
	result<-ifelse(pval >=0.05,"Normal","Not Normal")
	print(paste("shapiro test for:",i,"=",result,pval))
}

#all evidence concludes that the predictors are not normally distributed. 
#MG and SI are skewed left, NA and SI are the only predictors with a skewness < abs(1),
#all other predictors are right skewed 

#finding outliers in the data , are any predictors skewed 
#define an outlier as two sd's away from the mean 
#using a boxplot, we find that NA, SI, AL, K, CA, and BA have a considerable number of outliers. 

boxplot(predictors)

#c. are there any relevant transformations to improve the model? 
#we can determine this with the box-cox transformation package in R. 
