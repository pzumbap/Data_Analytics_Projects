#-----------------------------------------------------------------------------------------------------------------
#                                Pre-processing 1
#-----------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(rio)
library(car)
library(moments)
library(dplyr)
set.seed(54252888)
master_dataset=rio::import("6304 Module 8 Assignment Data.xlsx")
colnames(master_dataset)=tolower(make.names(colnames(master_dataset)))
str(master_dataset)
names(master_dataset)
#-----------------------------------------------------------------------------------------------------------------
#                                Pre-processing 2
#-----------------------------------------------------------------------------------------------------------------
primary_dataset=subset(master_dataset,region=="vermont"|region=="appleton"|region=="green bay"
                       |region=="indianapolis"|region=="worcester")
unique(primary_dataset$region)
primary_dataset=subset(primary_dataset,cylinders=="4"|cylinders=="6"|cylinders=="8")
unique(primary_dataset$cylinders)
stratifiedSample = primary_dataset %>%
  group_by(region) %>%
  sample_n(size=50)
stratifiedSample$region=as.factor(stratifiedSample$region)
stratifiedSample$cylinders=as.factor(stratifiedSample$cylinders)
str(stratifiedSample)
attach(stratifiedSample)
#-----------------------------------------------------------------------------------------------------------------
#                                Analysis 1
#-----------------------------------------------------------------------------------------------------------------
# Equality of variances test. Using Levene Test
leveneTest(asking.price~region,data=stratifiedSample) 
#The p-value of 0.2917<5 tells us to reject the NULL and accept that the variances are different.
boxplot(asking.price~region,pch=19,col="red",
        main="Asking price data grouped by Region")
list_variance1=aggregate(asking.price~region,stratifiedSample,var)
list_variance1[order(-list_variance1$asking.price),]#Aggregates the data by region calculating the variance from the stratified dataset
#-----------------------------------------------------------------------------------------------------------------
#                                Analysis 2
#-----------------------------------------------------------------------------------------------------------------
analysis2.out=aov(asking.price~region,data=stratifiedSample)
summary(analysis2.out)
analysis2.out$coefficients
#We can see we'are missing 'appleton' and that means it's the "best case" and it's equal to the intercept
list_means2=aggregate(asking.price~region,stratifiedSample,mean)
list_means2[order(-list_means2$asking.price),]
tukey1=TukeyHSD(analysis2.out)
tukey1
par(mar=c(5.1,10,4.1,2.1))
plot(tukey1,las=1)
par(mar=c(5.1,4.1,4.1,2.1)) 
#-----------------------------------------------------------------------------------------------------------------
#                                Analysis 3
#-----------------------------------------------------------------------------------------------------------------
leveneTest(odometer~region,data=stratifiedSample) 
boxplot(odometer~region,pch=19,col="red",
        main="Odometer data grouped by Region")
list_variance2=aggregate(odometer~region,stratifiedSample,var)
list_variance2[order(-list_variance2$odometer),]
analysis3.out=aov(odometer~region,data=stratifiedSample)
summary(analysis3.out)
analysis3.out$coefficients
list_means3=aggregate(odometer~region,stratifiedSample,mean)
list_means3[order(-list_means3$odometer),]
tukey3=TukeyHSD(analysis3.out)
tukey3
par(mar=c(5.1,10,4.1,2.1))
plot(tukey3,las=1)
par(mar=c(5.1,4.1,4.1,2.1)) 
#-----------------------------------------------------------------------------------------------------------------
#                                Analysis 4
#-----------------------------------------------------------------------------------------------------------------
leveneTest(asking.price~cylinders,data=stratifiedSample) 
boxplot(asking.price~cylinders,pch=19,col="red",
        main="Asking price data grouped by number of Cylinders")
list_variance3=aggregate(asking.price~cylinders,stratifiedSample,var)
list_variance3[order(-list_variance3$asking.price),]
analysis4.out=aov(asking.price~cylinders,data=stratifiedSample)
summary(analysis4.out)
analysis4.out$coefficients
list_means4=aggregate(asking.price~cylinders,stratifiedSample,mean)
list_means4[order(-list_means4$asking.price),]
tukey4=TukeyHSD(analysis4.out)
tukey4
par(mar=c(5.1,3,4.1,2.1))
plot(tukey4,las=1)
par(mar=c(5.1,4.1,4.1,2.1)) 
