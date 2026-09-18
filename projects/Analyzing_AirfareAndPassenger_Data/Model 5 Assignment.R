#Pablo Zumba
#Processing 1: 
rm(list=ls())
set.seed(54252888)
library(rio)
flights = import("6304 Module 5 Assignment Data.xlsx")
colnames(flights)=tolower(make.names(colnames(flights)))
names(flights)
#Processing 2:
subFlights = subset(flights,origin=="LAS"|origin=="LAX"|origin=="BWI"|origin=="LGA"|origin=="MCI"
                    |origin=="MCO"|origin=="ATL"|origin=="BNA")
subFlights = subFlights[sample(1:nrow(subFlights),50),]
subFlights$origin = as.factor(subFlights$origin)
subFlights$destination = as.factor(subFlights$destination)
subFlights$market.leading.airline = as.factor(subFlights$market.leading.airline)
subFlights$low.price.airline = as.factor(subFlights$low.price.airline)
attach(subFlights)
#--------------------------------------------------------------------------------------------------------
#                             ANALYSIS
#--------------------------------------------------------------------------------------------------------
#Analysis 1
str(subFlights)
#Analysis 2
table(origin)
#Analysis 3
plot(subFlights[,c(3,4,5,7,9)],pch=19,main="Continuous Variables only")
judge_cor = round(cor(subFlights[,c(3,4,5,7,9)]),3)
library(corrplot)
corrplot(judge_cor,method="number")
#Analysis 4
model_1.out = lm(price~origin+average.fare+distance+avg.weekly.passengers+route.market.share,data = subFlights)
summary(model_1.out)
#Analysis 5 was a written explanion of the beta values on origin vs price.
#Analysis 6. L.I.N.E assumptions
par(mfrow=c(2,2))
# Linearity
plot(price,model_1.out$fitted.values,
     pch=19,main="Actuals v. Fitteds, Price")
abline(0,1,col="red",lwd=3)
# Normality
qqnorm(model_1.out$residuals,pch=19,
       main="Normality Plot, Flight Price")
qqline(model_1.out$residuals,lwd=3,col="red")
hist(model_1.out$residuals,col="red",
     main="Residuals, Flight Price",
     probability=TRUE)
curve(dnorm(x,mean(model_1.out$residuals),
            sd(model_1.out$residuals)),
      from=min(model_1.out$residuals),
      to=max(model_1.out$residuals),
      lwd=3,col="Black",add=TRUE)
# Equality of Variances
plot(model_1.out$fitted.values,rstandard(model_1.out),
     pch=19,main="Equality of Variances, Flight Price")
abline(0,0,lwd=3,col="red")
par(mfrow=c(1,1))

#Analysis 7
subFlights[which.max(abs(model_1.out$residuals)),c(1,2,9)]
#Analysis 8
subFlights[which.min(abs(model_1.out$residuals)),c(1,2,9)]
