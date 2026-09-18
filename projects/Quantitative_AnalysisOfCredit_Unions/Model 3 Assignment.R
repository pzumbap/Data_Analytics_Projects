#Pablo Zumba
#Processing 1: 
rm(list=ls())
library(rio)
masterDataset = import("6304 Module 3 Assignment Data.xlsx")
attach(masterDataset)
str(masterDataset)
#Processing 2: intermediate datasets for CA, FL, NY-NJ:
ca_CU = subset(masterDataset, state == "CA")
fl_CU = subset(masterDataset, state == "FL")
nynj_CU = subset(masterDataset, state == "NY" | state == "NJ")
#Processing 3: Taking 20 random samples from each intermediate dataframe
set.seed(54252888)
ca_20samples = ca_CU[sample(1:nrow(ca_CU),20),]
fl_20samples = fl_CU[sample(1:nrow(fl_CU),20),]
nyny_20samples = nynj_CU[sample(1:nrow(nynj_CU),20),]

#Analysis 1 
results_fl = t.test(fl_20samples$members, conf.level = 0.9)
results_fl$conf.int
width_fl = results_fl$conf.int[2]-results_fl$conf.int[1]
width_fl
hist(fl_20samples$members,col="brown",main="Histogram of FL members")
abline(v=results_fl$conf.int[1],lwd=3,col="blue")
abline(v=results_fl$conf.int[2],lwd=3,col="blue")
abline(v=mean(fl_20samples$members),lwd=3,col="green")
#Analysis 2
results_fl2 = t.test(fl_20samples$members, conf.level = 0.99)
results_fl2$conf.int
width_fl2 = results_fl2$conf.int[2]-results_fl2$conf.int[1]
width_fl2
hist(fl_20samples$members,col="brown",main="Histogram of FL members at conf.int=0.99")
abline(v=results_fl2$conf.int[1],lwd=3,col="blue")
abline(v=results_fl2$conf.int[2],lwd=3,col="blue")
abline(v=mean(fl_20samples$members),lwd=3,col="green")

#Analysis 3 Hypothesis Tests
hist(ca_20samples$total.assets,col="red",main="20 samples of total assets on CA")
abline(v=170,lwd=3,col="blue")
abline(v=mean(ca_20samples$total.assets),lwd=3,col="green")
summary(ca_20samples$total.assets)
mean(ca_20samples$total.assets)
sd(ca_20samples$total.assets)
moments::skewness(ca_20samples$total.assets)
moments::kurtosis((ca_20samples$total.assets))
t.test(ca_20samples$total.assets,mu=170,alternative = "greater") #The mean is greater than 170. We failed to reject the Null since p-val > 5%
#Analysis 4 Independent Sampling or Paired comparisons
pairedComparison=t.test(ca_20samples$total.assets,nyny_20samples$total.assets,mu=0, alternative = c("two.sided")) 
pairedComparison
boxplot(ca_20samples$total.assets,nyny_20samples$total.assets,notch=FALSE,col="red",
        main="Total assets Boxplot",
        names=c("CA","NY|NJ"))
abline(h=mean(ca_20samples$total.assets),col="blue",lwd=3)
abline(h=mean(nyny_20samples$total.assets),col="grey",lwd=3)
#Analysis 5 | The largest FL Credit Union
#Assuming the "largest" means the one that has more members
attach(fl_CU)
fl_CU[which.max(members), c("name", "city", "members", "total.assets")]

fl_CU[which.max(fl_CU$total.assets), c(2,3,5,6)]
