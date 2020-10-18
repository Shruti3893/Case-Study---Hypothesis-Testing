#Hypothesis Testing Exercise

#Sales of products in four different regions is tabulated for males and females. 
#Find if male-female buyer rations are similar across regions.

#Data Set - Buyer Ratio.mtw

install.packages("readr")
library(readr)
setwd("C://Users//Lenovo//Desktop//ExcelR//Assignments//Hypothesis Testing")
getwd()
BuyerRatio<-read.csv("BuyerRatio.csv")
View(BuyerRatio)
str(BuyerRatio)
summary(BuyerRatio)
var(BuyerRatio[-1])
sd(BuyerRatio$East)
sd(BuyerRatio$West)
sd(BuyerRatio$North)
sd(BuyerRatio$South)
range(BuyerRatio[-1])
install.packages("moments")
library(moments)
skewness(BuyerRatio[-1])
kurtosis(BuyerRatio[-1])
hist(BuyerRatio$East)
hist(BuyerRatio$West)
hist(BuyerRatio$North)
hist(BuyerRatio$South)
plot(BuyerRatio$East,BuyerRatio$West)
plot(BuyerRatio$North,BuyerRatio$South)
barplot(BuyerRatio$East)
barplot(BuyerRatio$West)
barplot(BuyerRatio$North)
barplot(BuyerRatio$South)
boxplot(BuyerRatio$East,BuyerRatio$West,BuyerRatio$North,BuyerRatio$South)
sum(is.na(BuyerRatio))
attach(BuyerRatio)
colnames(BuyerRatio)<-c("Oberved.values","East","West","North","South")
colnames(BuyerRatio)

#############Variance test###############

var.test(East,West)#variance test
# p-value = 0.3462 > 0.05 so p high null fly => Equal variances

var.test(East,North)
# p-value = 0.3877 > 0.05 so p high null fly => Equal variances

var.test(East,South)
# p-value = 0.6559 > 0.05 so p high null fly => Equal variances

var.test(West,North)
# p-value = 0.9239 > 0.05 so p high null fly => Equal variances

var.test(West,South)
# p-value = 0.5826 > 0.05 so p high null fly => Equal variances

var.test(North,South)
# p-value = 0.6452 > 0.05 so p high null fly => Equal variances

#########Chi Squared Test#################

Stacked_Data <- stack(BuyerRatio)
View(Stacked_Data)
attach(Stacked_Data)
table(ind,values)
chisq.test(table(ind,values))
# p-value = 0.297 > 0.05  => Accept null hypothesis
# => All Regions have equal proportions 