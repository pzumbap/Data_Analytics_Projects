#Pablo Zumba
#Processing 1: 
rm(list=ls())
set.seed(54252888)
us_Passengers = rio::import("6304 Module 7 Assignment Data.xlsx")
colnames(us_Passengers)=tolower(make.names(colnames(us_Passengers)))
names(us_Passengers)
str(us_Passengers)

#Analysis 1: Show a line plot of the data using the index as ‘x’ and passengers as "y" variable.
us_Passengers$index=seq(1:nrow(us_Passengers))
names(us_Passengers)
attach(us_Passengers)
plot(index,passengers,pch=19,
     main="Passengers us-domestic -- Raw Data")
plot(index,passengers,type="l",pch=19,
     main="Passengers us-domestic -- Raw Data")
plot(index,passengers,type="o",pch=19,
     main="Passengers us-domestic -- Raw Data")

#Analysis 2: Using all the rows parameterize a base time series simple regression model using "index" 
#as the independent variable and passengers as dependent variable.  Show the summary of your regression output.
model_1.out=lm(passengers~index,data=us_Passengers)
summary(model_1.out)

#Analysis 3:
plot(index,passengers,type="o",pch=19,
     main="Model 1 | Simple Regression plot")
points(model_1.out$fitted.values, type="l", lwd=3, col="red")
cor(us_Passengers$passengers, model_1.out$fitted.values)
#plot(us_Passengers$index,rstandard(model_1.out),pch=19,type="o", main="Base Model, Standardized Residuals")
#abline(0,0,col="red",lwd=3)

#Analysis 4: The Durbin Watson Test
durbin_test=car::dwt(model_1.out)
durbin_test

#Analysis 5: Making Seasonal Indices
seasonal_indices=data.frame(month=1:12,average=0,index2=0)
for(i in 1:12) {
  count=0
  for(j in 1:nrow(us_Passengers)) {
    if(i==us_Passengers$month[j]) {
      seasonal_indices$average[i]=seasonal_indices$average[i]+us_Passengers$passengers[j]
      count=count+1
    }
  }
  seasonal_indices$average[i]=seasonal_indices$average[i]/count
  seasonal_indices$index2[i]=seasonal_indices$average[i]/mean(us_Passengers$passengers)
}
#An index of exactly 1.00 means the total # of Passengers are exactly at the 12 month average.
#If it's below 1, means below the average and vice-versa.

#De-seasonalizing the original data
#We need to take out the seasonal fluctuation and have the rest which is the (secular and noise variation)
for(i in 1:12){
  for(j in 1:nrow(us_Passengers)){
    if(i==us_Passengers$month[j]){
      us_Passengers$deseason.index[j]=us_Passengers$passengers[j]/seasonal_indices$index2[i]
    }
  }
}

#Analysis 6
attach(us_Passengers)
model_2_des=lm(deseason.index~index,data=us_Passengers)
summary(model_2_des)
plot(index,deseason.index,type="o",pch=19,
     main="Model 2 | Using Deseasonalized data ")
points(model_2_des$fitted.values, type="l", lwd=3, col="red")
cor(deseason.index, model_2_des$fitted.values)
#plot(index,rstandard(model_2_des),pch=19,type="o", main="Base Model, Standardized Residuals")
#abline(0,0,col="red",lwd=3)

#Analysis 7

us_Passengers$deseasonModel1=model_1.out$fitted.values
for(i in 1:12){
  for(j in 1:nrow(us_Passengers)){
    if(i==us_Passengers$month[j]){
      us_Passengers$reseasonModel1[j]=us_Passengers$deseasonModel1[j]*seasonal_indices$index2[i]
    }
  }
}
us_Passengers$deseasonModel2=model_2_des$fitted.values
for(i in 1:12){
  for(j in 1:nrow(us_Passengers)){
    if(i==us_Passengers$month[j]){
      us_Passengers$reseasonModel2[j]=us_Passengers$deseasonModel2[j]*seasonal_indices$index2[i]
    }
  }
}
attach(us_Passengers)
#par(mfrow=c(2,1))
plot(index,passengers,type="l",pch=19, main="Original and Reseasonalized | Model 1")
points(index,reseasonModel1, type="l",pch=19,col="red", lwd=3)
#plot(index,passengers,type="l",pch=19, main="Original and Reseasonalized | Model 2")
points(index,reseasonModel2, type="l",pch=19,col="blue", lwd=1)
#par(mfrow=c(1,1))

#Analysis 8
us_Passengers$deseason.forecast=model_2_des$fitted.values
for(i in 1:12){
  for(j in 1:nrow(us_Passengers)){
    if(i==us_Passengers$month[j]){
      us_Passengers$reseason.forecast[j]=us_Passengers$deseason.forecast[j]*seasonal_indices$index2[i]
    }
  }
}
attach(us_Passengers)
plot(index,passengers,type="l",pch=19, lwd=2, col="blue", lty="twodash", main="Original passenger's data (Blue-dash) and Reseasonalized-forecast data (Red)")
points(index,reseason.forecast, type="l",pch=19,col="red", lwd=3)

points(model_1.out$fitted.values, type="l", lwd=1, col="blue", lty="twodash")
points(model_2_des$fitted.values, type="l", lwd=2, col="red")

cor(us_Passengers$passengers, model_1.out$fitted.values)
cor(deseason.index, model_2_des$fitted.values)

us_Passengers[which.min(rstandard(model_1.out)),c(1,3)]
