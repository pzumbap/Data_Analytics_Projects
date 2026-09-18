rm(list=ls())
library(rio)

#Import SnackChain.Xlsx file.
stores_df       <- import("SnackChain.xlsx", sheet="stores")  #79 stores
products_df     <- import("SnackChain.xlsx", sheet="products") #58 products
transactions_df <- import("SnackChain.xlsx", sheet="transactions") #524950 transactions
#Joining Tables.
temp <- merge(transactions_df, products_df, by.x=c("UPC"), by.y=c("UPC"))  #join by UPCs between transactions and products
df   <- merge(temp, stores_df, by.x=c("STORE_NUM"), by.y=c("STORE_ID"))  #join by adding the stores.
#Removing temporary Files.
rm(transactions_df)
rm(stores_df)
rm(temp)
rm(products_df)
#________________________________________________________________________________________________________________
                                            #Data Preprocessing
#________________________________________________________________________________________________________________
#Remove oral hygiene products
df <- df[df$CATEGORY != "ORAL HYGIENE PRODUCTS", ]  #this could be for another project

#Check for missing values
colSums(is.na(df))
#' 282548 missing PARKING values; we will not use this data since it is higher (store) level,
#' 10 NA values for price and 173 for BASE_PRICE
df <- subset(df, select = -c(PARKING))
df <- df[complete.cases(df), ] #Removing missing values

#Renaming columns to make them easier to understand
colnames(df)[colnames(df) == "STORE_NUM"] = "store_id"
colnames(df)[colnames(df) == "UPC"] = "product_id"
colnames(df)[colnames(df) == "WEEK_END_DATE"] = "week_ending_date"
colnames(df)[colnames(df) == "UNITS"] = "units_sold"
colnames(df)[colnames(df) == "VISITS"] = "unique_purchases"
colnames(df)[colnames(df) == "HHS"] = "purchasing_households"
colnames(df)[colnames(df) == "SPEND"] = "total_spend"
colnames(df)[colnames(df) == "PRICE"] = "price_product_charged"
colnames(df)[colnames(df) == "BASE_PRICE"] = "base_price_product"
colnames(df)[colnames(df) == "FEATURE"] = "circular"
colnames(df)[colnames(df) == "DISPLAY"] = "promotional_display"
colnames(df)[colnames(df) == "TPR_ONLY"] = "temp_price_reduction"
colnames(df)[colnames(df) == "DESCRIPTION"] = "product"
colnames(df)[colnames(df) == "MANUFACTURER"] = "manufacturer"
colnames(df)[colnames(df) == "CATEGORY"] = "category"
colnames(df)[colnames(df) == "SUB_CATEGORY"] = "subcategory"
colnames(df)[colnames(df) == "PRODUCT_SIZE"] = "size"
colnames(df)[colnames(df) == "STORE_NAME"] = "store_name"
colnames(df)[colnames(df) == "CITY"] = "city"
colnames(df)[colnames(df) == "STATE"] = "state"
colnames(df)[colnames(df) == "MSA"] = "metropilitan_area"
colnames(df)[colnames(df) == "SEGMENT"] = "type_of_store"
colnames(df)[colnames(df) == "SIZE"] = "store_sqft"
colnames(df)[colnames(df) == "AVG_WEEKLY_BASKETS"] = "avg_weekly_baskets_sold"

#Converting Categorical variables into factor variables.
df$store_id = factor(df$STORE_ID)
df$product_id = factor(df$product_id)
df$category = factor(df$category)
df$subcategory = factor(df$subcategory)
df$category = relevel(df$category, "BAG SNACKS")
df$city = factor(df$city)
df$state = factor(df$state)
df$type_of_store = factor(df$type_of_store)
df$type_of_store = relevel(df$type_of_store, "VALUE")
df$product = factor(df$product)


#Extracting year, month, and week_ending_date from date.
df$year = format(df$week_ending_date, "%Y")
df$year = factor(df$year)
df$month = format(df$week_ending_date, "%b")
df$month = factor(df$month)
df$month = relevel(df$month, "Jan")
df$week_num = difftime(df$week_ending_date, df$week_ending_date[1], units="weeks")
df$week_num = df$week_num + 1
attach(df)
str(df)

df = df[df$price_product_charged != 0, ] #eliminating 0's on price product to avoid errors when performing log()
df = df[df$total_spend != 0, ] #eliminating 0's on total spend

df$log_total_spend <- log(df$total_spend)
df$log_units_sold <- log(df$units_sold)
df$log_purchasing_households <- log(df$purchasing_households)
df$log_price_product_charged <- log(df$price_product_charged)

df <- subset(df, df$log_total_spend != -Inf)
#________________________________________________________________________________________________________________
#                                 1.
#________________________________________________________________________________________________________________
#Data Visualization
hist(df$total_spend)
hist(log(df$total_spend))
hist(df$units_sold)
hist(log(df$units_sold))
hist(df$purchasing_households)
hist(log(df$purchasing_households))

#Correlations
library(corrplot)
correlation = cor(df[c("units_sold", "promotional_display", "purchasing_households", "total_spend", "temp_price_reduction", 
                       "circular", "price_product_charged", "base_price_product", "metropilitan_area", 
                       "store_sqft", "avg_weekly_baskets_sold")], use="pairwise.complete.obs")
corrplot(correlation)

#Plots
boxplot(df$total_spend ~ df$promotional_display)
boxplot(df$total_spend ~ df$circular)
boxplot(df$total_spend ~ df$city)
boxplot(df$total_spend ~ df$temp_price_reduction)
boxplot(df$total_spend ~ df$type_of_store)

library(car)
library(lmtest)
library(plm)


# Random Effect Model for spend
spendrandom <- plm(log(total_spend) ~ circular
                   + promotional_display
                   + temp_price_reduction
                   + price_product_charged
                   + week_num
                   + month,
                   data=df, index=c("store_id"),model= "random")

# Random Effect Model for spend
unitsrandom <- plm(log(units_sold) ~ circular
                   + promotional_display
                   + temp_price_reduction
                   + price_product_charged
                   + week_num
                   + month,
                   data=df, index=c("store_id"),model= "random")

# Random Effect Model for spend
hhsrandom <- plm(log(purchasing_households) ~ circular
                 + promotional_display
                 + temp_price_reduction
                 + price_product_charged
                 + week_num
                 + month,
                 data=df, index=c("store_id"),model= "random")

library(stargazer)
stargazer(spendrandom, unitsrandom, hhsrandom, type = "html", out="~/Downloads/SnackChain.html")
#________________________________________________________________________________________________________________
#                                               2. Models w/out category and type of store
#________________________________________________________________________________________________________________
#Best models from question 1 to be used as the base for question 2

#Total Spend  base model
lm10 = lm(log_total_spend ~  promotional_display + circular + temp_price_reduction + store_id + week_num + month , data=df)
summary(lm10 )
par(mfrow=c(2,2))
plot(lm10 )


#Random Effect Week Number
lm11 = lme4::lmer(log_total_spend  ~ promotional_display + circular + temp_price_reduction + week_num + month + store_id + (1 | week_num), data=df, REML = FALSE)
summary(lm11)
par(mfrow=c(2,2))
plot(lm11)

qqnorm(resid(lm10))
qqline(resid(lm10))

#Random Effect Week Number and Store Number
lm12 = lme4::lmer(log_total_spend ~ promotional_display + circular + temp_price_reduction + price_product_charged  + week_num + month  + (1 | store_id) , data=df, REML = FALSE)
summary(lm12)
par(mfrow=c(2,2))
plot(lm12)

qqnorm(resid(lm11))
qqline(resid(lm11))


library('MuMIn')

r.squaredGLMM(lm11)
r.squaredGLMM(lm12)


library(stargazer)
stargazer(lm10, lm11, lm12, type="html", out="totalspend.html")
AIC(lm10, lm11, lm12)



#Units Sold base model
lm13 = lm(log_units_sold ~ promotional_display + circular + temp_price_reduction + store_id + week_num + month + category + type_of_store  , data=df)
summary(lm13)
lm13$coefficients
par(mfrow=c(2,2))
plot(lm13)


#Random Effect Week Number
lm14 = lme4::lmer(log_units_sold ~ promotional_display + circular + temp_price_reduction + store_id + week_num + month + category + type_of_store + (1 | week_num), data=df, REML = FALSE)
summary(lm14)
par(mfrow=c(2,2))
plot(lm14)

qqnorm(resid(lm14))
qqline(resid(lm14))

#Random Effect Week Number and Store Number
lm15 = lme4::lmer(log_units_sold ~ promotional_display + circular + temp_price_reduction + price_product_charged  + week_num + month + category + type_of_store  + (1 | store_id) , data=df, REML = FALSE)
summary(lm15)
par(mfrow=c(2,2))
plot(lm15)

qqnorm(resid(lm15))
qqline(resid(lm15))


library('MuMIn')

r.squaredGLMM(lm14)
r.squaredGLMM(lm15)


library(stargazer)
stargazer(lm13, lm14, lm15, type="html", out="unitssold.html")
AIC(lm13, lm14, lm15)




#Purchasing household  base model
lm16 = lm(log_purchasing_households ~ promotional_display + circular + temp_price_reduction + store_id + week_num + month  , data=df)
summary(lm16)
lm16$coefficients
par(mfrow=c(2,2))
plot(lm16)



#Random Effect Week Number
lm17 = lme4::lmer(log_purchasing_households ~ promotional_display + circular + temp_price_reduction + store_id + month + (1 | week_num), data=df, REML = FALSE)
summary(lm17)
par(mfrow=c(2,2))
plot(lm17)

qqnorm(resid(lm17))
qqline(resid(lm17))

#Random Effect Week Number and Store Number
lm18 <- lme4::lmer(log_purchasing_households  ~ promotional_display + circular + temp_price_reduction + price_product_charged  + week_num + month + (1 | store_id) , data=df, REML = FALSE)
summary(lm18)
par(mfrow=c(2,2))
plot(lm18)

qqnorm(resid(lm18))
qqline(resid(lm18))

install.packages('MuMIn')
library('MuMIn')

r.squaredGLMM(lm17)
r.squaredGLMM(lm18)

library(stargazer)
stargazer(lm16, lm17, lm18, type="html", out="PurchasingHouseholds.html")
AIC(lm16, lm17, lm18)


df_CC <- df[df$category == "COLD CEREAL", ]
df_FP <- df[df$category == "FROZEN PIZZA", ]
df_MS  <- df[df$type_of_store == "MAINSTREAM", ]
df_UP  <- df[df$type_of_store == "UPSCALE", ]


#________________________________________________________________________________________________________________
#                                               2. Category Only
#________________________________________________________________________________________________________________
#Random Effect Week Number and Store Number
lm19 <- lme4::lmer(log_total_spend ~ promotional_display 
                   + circular 
                   + temp_price_reduction 
                   + price_product_charged  
                   + week_num 
                   + month 
                   + category  
                   + (1 | store_id) , data=df , REML = FALSE)
summary(lm19)
par(mfrow=c(2,2))
plot(lm19)



#Random Effect Week Number and Store Number
lm20 = lme4::lmer(log_units_sold ~ promotional_display 
                  + circular 
                  + temp_price_reduction 
                  + price_product_charged  
                  + week_num 
                  + month 
                  + category  
                  + (1 | store_id) , data=df , REML = FALSE)
summary(lm20)
par(mfrow=c(2,2))
plot(lm20)


#Random Effect Week Number and Store Number
lm21 <- lme4::lmer(log_purchasing_households  ~ promotional_display 
                   + circular 
                   + temp_price_reduction 
                   + price_product_charged  
                   + week_num 
                   + month 
                   + category  
                   + (1 | store_id) , data=df , REML = FALSE)
summary(lm21)
par(mfrow=c(2,2))
plot(lm21)

library('MuMIn')
r.squaredGLMM(lm19)
r.squaredGLMM(lm20)
r.squaredGLMM(lm21)


library(stargazer)
stargazer(lm19, lm20, lm21, type="html", out="category.html")

#________________________________________________________________________________________________________________
#                                               2. Type of Store Only
#________________________________________________________________________________________________________________
#Random Effect Week Number and Store Number
lm22 = lme4::lmer(log_total_spend ~ promotional_display 
                  + circular 
                  + temp_price_reduction 
                  + price_product_charged  
                  + week_num 
                  + month 
                  + type_of_store  + (1 | store_id) , data=df , REML = FALSE)
summary(lm22)
par(mfrow=c(2,2))
plot(lm22)

#Random Effect Week Number and Store Number
lm23 = lme4::lmer(log_units_sold ~ promotional_display 
                  + circular 
                  + temp_price_reduction 
                  + price_product_charged 
                  + week_num 
                  + month 
                  + type_of_store  
                  + (1 | store_id) , data=df , REML = FALSE)
summary(lm23)
par(mfrow=c(2,2))
plot(lm23)

#Random Effect Week Number and Store Number
lm24 <- lme4::lmer(log_purchasing_households  ~ promotional_display 
                   + circular 
                   + temp_price_reduction 
                   + price_product_charged  
                   + week_num 
                   + month 
                   + type_of_store 
                   + (1 | store_id) , data=df , REML = FALSE)
summary(lm24)
par(mfrow=c(2,2))
plot(lm24)|

  library('MuMIn')
r.squaredGLMM(lm22)
r.squaredGLMM(lm23)
r.squaredGLMM(lm24)

library(stargazer)
stargazer(lm22, lm23, lm24, type="html", out="type_of_store.html")
#________________________________________________________________________________________________________________
#                                         3. Price Elasticity
#________________________________________________________________________________________________________________
#Data exploration
table(product)
table(product_id)
table(category)
summary(df)
hist(df$price_product_charged, breaks = 50)
hist(df$total_spend, breaks = 50)
df = df[df$units_sold != 1800, ]
hist(log(df$units_sold), breaks = 30)
#summary(df)

#Creating a dataframe to store log(sales), log(price), category, and product name separately to avoid row disparity.
sales = as.vector(df[,'total_spend'])
units = as.vector(df[, 'units_sold'])
price = as.vector(df[,'price_product_charged'])
category = as.vector(df[,'category'])
product_name = as.vector(df[,'product'])
households = as.vector(df[,'purchasing_households'])
elasticity_df = data.frame(log(sales), log(price), log(units), log(households),category, product_name)
colnames(elasticity_df) = c("log_sales", "log_price", "log_units", "log_purchasing_households","category", "product_name")
hist(elasticity_df$log_sales, breaks = 50)
hist(elasticity_df$log_price, breaks = 50)
hist(elasticity_df$log_units, breaks = 50)
rm(sales, units, price, category, product_name)
#summary(elasticity_df)

#Creating a function that calculates the elasticity given a dependent variable and a product name (product name can be iterated from a list).
#The function returns the product name, elasticity and its category. PE = Price Elasticity
elasticity_by_product = data.frame(product_name=c("Perfectly Inelastic Test product name"), elasticity_num=c(0),  category=c("Test_name"))
calc_elasticity = function(x){
  only_one_product_at_a_time = subset(elasticity_df, product_name==x)
  model_for_elasticity = lm(log_sales ~ log_price, data = only_one_product_at_a_time)
  PE = as.numeric(model_for_elasticity$coefficients["log_price"] * mean(only_one_product_at_a_time$log_price)/mean(only_one_product_at_a_time$log_sales))
  category_name = (only_one_product_at_a_time$category[1])
  aux_vector = c(x, PE, category_name)
  return(aux_vector)
}

#Iterating and adding each product with its respective PE to elasticity_by_product dataframe.
list_of_products = unique(elasticity_df$product_name) # Making a list of the 41 unique products.
for (i in list_of_products){
  elasticity_by_product = rbind(elasticity_by_product, calc_elasticity(i))
}


#Data transformation on elasticity_by_product dataset.
elasticity_by_product$elasticity_num = as.numeric(elasticity_by_product$elasticity_num) #Converting to Number
elasticity_by_product <- elasticity_by_product[-1,] #Deleting the test product with elasticity = 0
library(dplyr)
elasticity_by_product <- elasticity_by_product %>% 
  mutate(direction = if_else(elasticity_num > 0, "[+]", "[-]"))#Preserving the symbol before to perform abs()
library(tidyverse)
elasticity_by_product$elasticity_num = abs(elasticity_by_product$elasticity_num) #Making absolute values
elasticity_by_product$elasticity_num = round(elasticity_by_product$elasticity_num, digits = 2)
elasticity_by_product = elasticity_by_product[order(-elasticity_by_product$elasticity_num),]#Sorting data frame

#Filtering the Top 5, most and least elastic products:
top_five_elastic = elasticity_by_product[1:5,]
row.names(top_five_elastic) <- NULL
tope_five_inelastic = tail(elasticity_by_product, n = 5)
row.names(tope_five_inelastic) <- NULL
top_five_elastic
tope_five_inelastic

                                #Analysis of elasticity using units_sold as a dependent variable.
elasticity_by_product_units = data.frame(product_name=c("Perfectly Inelastic Test product name"), elasticity_num=c(0),  category=c("Test_name"))
calc_elasticity_units = function(x){
  only_one_product_at_a_time = subset(elasticity_df, product_name==x)
  model_for_elasticity = lm(log_units ~ log_price, data = only_one_product_at_a_time)
  PE = as.numeric(model_for_elasticity$coefficients["log_price"] * mean(only_one_product_at_a_time$log_price)/mean(only_one_product_at_a_time$log_units))
  category_name = (only_one_product_at_a_time$category[1])
  aux_vector = c(x, PE, category_name)
  return(aux_vector)
}
for (i in list_of_products){
  elasticity_by_product_units = rbind(elasticity_by_product_units, calc_elasticity_units(i))
}
elasticity_by_product_units$elasticity_num = as.numeric(elasticity_by_product_units$elasticity_num) #Converting to Number
elasticity_by_product_units <- elasticity_by_product_units[-1,] #Deleting the test product with elasticity = 0
elasticity_by_product_units <- elasticity_by_product_units %>% 
  mutate(direction = if_else(elasticity_num > 0, "[+]", "[-]"))#Preserving the symbol before to perform abs()
elasticity_by_product_units$elasticity_num = abs(elasticity_by_product_units$elasticity_num) #Making absolute values
elasticity_by_product_units$elasticity_num = round(elasticity_by_product_units$elasticity_num, digits = 2)
elasticity_by_product_units = elasticity_by_product_units[order(-elasticity_by_product_units$elasticity_num),]#Sorting data frame

#Top 5, most and least elastic products for units sold:
top_five_elastic_units = elasticity_by_product_units[1:5,]
row.names(top_five_elastic_units) <- NULL
tope_five_inelastic_units = tail(elasticity_by_product_units, n = 5)
row.names(tope_five_inelastic_units) <- NULL
top_five_elastic_units
tope_five_inelastic_units


#Determining the LINE assumptions of regression for one product.
assumptions_LINE = function(product_name) {
  only_one_product = subset(df, product==product_name)
  m1 = lm(log_total_spend ~ log_price_product_charged + log_purchasing_households, data=only_one_product)
  #m1 = lm(total_spend ~ price_product_charged + units_sold + purchasing_households, data=only_one_product)
  par(mfrow=c(2,2))
  # Linearity
  plot(only_one_product$total_spend,m1$fitted.values, pch=19,main="Actuals v. Fitteds, total_spend")
  abline(0,1,col="red",lwd=3)
  # Normality
  qqnorm(m1$residuals,pch=19,
         main="Normality Plot, total_spend")
  qqline(m1$residuals,lwd=3,col="red")
  hist(m1$residuals,col="red",
       main="Residuals, total_spend",
       probability=TRUE)
  curve(dnorm(x,mean(m1$residuals),
              sd(m1$residuals)),
        from=min(m1$residuals),
        to=max(m1$residuals),
        lwd=3,col="Black",add=TRUE)
  # Equality of Variances
  plot(m1$fitted.values,rstandard(m1),
       pch=19,main="Equality of Variances, total_spend")
  abline(0,0,lwd=3,col="red")
  par(mfrow=c(1,1))
  summary(m1)
}

#assumptions_LINE("FRSC PEPPERONI PIZZA")
