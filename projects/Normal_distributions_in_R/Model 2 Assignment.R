#Pablo Zumba
rm(list=ls())
library(rio)
marksBrothers = as.data.frame(import("6304 Module 2 Assignment Data.xlsx"))
attach(marksBrothers)

#Part 1. Determining Normality
isNormalDist <- function(data_sample, colorGraph, titleGraph=NULL){
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  hist(data_sample, col=colorGraph, main=titleGraph, probability = TRUE)
  lines(density(data_sample),lwd=3,col="Black")
  boxplot(data_sample,col=colorGraph, main="BoxPlot Distribution")
  #Make a code to draw the mean
  qqnorm(data_sample, pch=19, main="QQplot compared to QQline")
  qqline(data_sample, lwd=3, col='red')
  print("Skewness=")
  print(moments::skewness(data_sample))
  print("Kurtosis=")
  print(moments::kurtosis(data_sample))
}

isNormalDist(Harpo,"Green", "Harpo Distribution")
isNormalDist(Zeppo,"Blue", "Zeppo Distribution")
isNormalDist(Groucho,"Red", "Groucho Distribution")
isNormalDist(Chico,"Purple", "Chico Distribution")

#Part 2. Groucho variable and sampling distributions.
set.seed(54252888)
sample_means = data.frame()
for(i in 1:1000){
  sample_means[i,1] = mean(sample(Groucho,25))
} 
attach(sample_means)
isNormalDist(V1, "Brown", "Distribution of Sample Means n=1000")
