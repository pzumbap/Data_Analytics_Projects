# Cleaning Process

# 1) Libraries needed for data cleaning
library(tidyverse)
library(ggplot2)
library(tibble)

#********************************************************************************************************

# 2) Importing CSV file downloaded from "National Longitudinal Surveys" and converting it into a tibble
divorce_df <- read_csv("~/MEGAsync/Google DS Certificate/8) Portfolio and Case Study/Divorce_Project_2.0/2) Data Cleaning/Divorce_project_version_two.csv")
#divorce_df <- as_tibble(raw_data_divorce)

#********************************************************************************************************

# 3) Exploring dataframe
#head(divorce_df)
#str(divorce_df)
#colnames(divorce_df)

#********************************************************************************************************

#4) Cleaning Values depending of each variable.
divorce_df <- as_tibble(divorce_df)
vallabels = function(data) {
  data$R0532200 <- factor(data$R0532200, 
    levels=c(0.0,1.0), 
    labels=c("No",
      "Yes"))
  data$R0536300 <- factor(data$R0536300, 
    levels=c(0.0,1.0,2.0), 
    labels=c("No Information",
    "Male",
    "Female"))
  data$R0552200 <- factor(data$R0552200, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,30.0,31.0,32.0,34.0,35.0), 
    labels=c("Roman Catholic",
      "Baptist",
      "Methodist",
      "Lutheran",
      "Presbyterian",
      "Episcopal/Anglican",
      "United Church of Christ",
      "Disciples of Christ",
      "Reformed Church",
      "Nazarene, Wesleyan, Free Methodist",
      "Pentecostal",
      "Nondemonimational Christian",
      "Other Protestant",
      "Jewish-Orthodox",
      "Jewish-Conservative",
      "Jewish-Reform",
      "Other Jewish",
      "Mormon",
      "Eastern Orthodox",
      "Unitarian",
      "Muslim",
      "Hindu/Buddhist",
      "Native American",
      "Other",
      "Agnostic",
      "Atheist",
      "Personal philosophy",
      "Bah'ai",
      "Greek, Roman, Norse, Mythology",
      "Satanic",
      "Wicca/Witchcraft/Magic/Pagan",
      "Scientology",
      "Sikh"))
  data$R0690800 <- factor(data$R0690800, 
    levels=c(0.0,1.0,2.0), 
    labels=c("Not true",
      "Sometimes true",
      "Often true"))
  data$R0691200 <- factor(data$R0691200, 
    levels=c(0.0,1.0,2.0), 
    labels=c("Not true",
      "Sometimes true",
      "Often true"))
  data$R1482600 <- factor(data$R1482600, 
    levels=c(1.0,2.0,3.0,4.0), 
    labels=c("Black",
      "Hispanic",
      "Mixed Race(Non-Hispanic)",
      "Non-Black/Non-Hispanic"))
  data$S1249300 <- factor(data$S1249300, 
    levels=c(0.0,1.0), 
    labels=c("NO",
      "YES"))
  data$T1067500 <- factor(data$T1067500, 
    levels=c(0.0,1.0), 
    labels=c("NO",
      "YES"))
  data$T3162503 <- factor(data$T3162503, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0), 
    labels=c("1-Disagree strongly",
      "2-Disagree moderately",
      "3-Disagree a little",
      "4-Neither agree nor disagree",
      "5-Agree a little",
      "6-Agree moderately",
      "7-Agree strongly"))
  data$T9110400 <- factor(data$T9110400, 
    levels=c(0.0,1.0), 
    labels=c("NO",
      "YES"))
  data$U4370902 <- factor(data$U4370902, 
    levels=c(0.0,1.0,2.0,3.0), 
    labels=c("Rarely/None of the time/1 Day",
      "Some/A little of the time/1-2 Days",
      "Occasionally/Moderate amount of the time/3-4 Days",
      "Most/All of the time/5-7 Days"))
  data$U4370904 <- factor(data$U4370904, 
    levels=c(0.0,1.0,2.0,3.0), 
    labels=c("Rarely/None of the time/1 Day",
      "Some/A little of the time/1-2 Days",
      "Occasionally/Moderate amount of the time/3-4 Days",
      "Most/All of the time/5-7 Days"))
  data$U4370905 <- factor(data$U4370905, 
    levels=c(0.0,1.0,2.0,3.0), 
    labels=c("Rarely/None of the time/1 Day",
      "Some/A little of the time/1-2 Days",
      "Occasionally/Moderate amount of the time/3-4 Days",
      "Most/All of the time/5-7 Days"))
  data$Z9073400 <- factor(data$Z9073400,
    levels=c(1.0,2.0,3.0), 
    labels=c("Divorced",
      "Widowed",
      "Unknown-includes legal annulments"))
  data$U3572700 <- factor(data$U3572700, 
    levels=c(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0), 
    labels=c("None",
      "GED",
      "High school diploma (Regular 12 year program)",
      "Associate/Junior college (AA)",
      "Bachelor's degree (BA, BS)",
      "Master's degree (MA, MS)",
      "PhD",
      "Professional degree (DDS, JD, MD)"))
  data$Z9149100 <- factor(data$Z9149100, 
    levels=c(0.0,1.0,2.0,3.0,4.0), 
    labels=c("Never-married",
      "Married",
      "Separated",
      "Divorced",
      "Widowed"))
  return(data)
}
divorce_df <- vallabels(divorce_df) #Calls the previous function to stablish new values on each variable.

#********************************************************************************************************

# 5) Renaming Column names for a more intuitive names using the "Variable Search" tool provided by National Longitudinal Surveys Link: https://www.nlsinfo.org/investigator/pages/search?s=NLSY97
# Values that are -1,-2,-3,-4 and -5 means refusal, dont know, N/A, Skip and NonInterview, respectively, should be treated as NA when analyze data.
divorce_df <- divorce_df %>% 
  rename(
    id_code = R0000100, #Identification Code
    live_two_parents_1997 = R0532200, #Does Youth live with both biological parents? 1=Yes, 0=No
    gender = R0536300, #Gender of Youth 1=Male, 2=Female
    birthdate = R0536402, #Birthdate of youth
    religion = R0552200,
    female_depression_1997 = R0690800, #Youth is unhappy, sad or depressed? 0=NotTrue, 1=SometimesTrue, 2=OftenTrue
    male_depression_1997 = R0691200, #Youth is unhappy, sad or depressed? 0=NotTrue, 1=SometimesTrue, 2=OftenTrue
    ethnicity = R1482600, #Combined Race 1=Black, 2=Hispanic, 3=Mixed Race(Non-Hispanic), 4=Non-Black/Non-Hispanic
    parents_divorced_97_02 = S1249300, #parents divorced in the last 5 years? 1997-2002, 1=Yes, 0=No
    parents_divorced_02_07 = T1067500, #parents divorced in the last 5 years? 2002-2007, 1=Yes, 0=No
    personality_scale_2008 = T3162503, #Does youth feel Anxious or easily upset? 1=Disagree strongly, 2=Disagree moderately, 3=Disagree a little, 4=Neither agree nor disagree, 5=Agree a little, 6=Agree moderately, 7=Agree strongly
    parents_divorced_08_13 = T9110400, #parents divorced in the last 6 years? 2008-2013, 1=Yes, 0=No
    depression_medication_2019 = U4370902, #individual took pain medication in last month due felt depressed? 0=Rarely/None of the time/1 Day, 1=Some/A little of the time/1-2 Days, 2=Occasionally/Moderate amount of the time/3-4 Days, 3=Most/All of the time/ 5-7 Days
    sleep_restless_medication_2019 = U4370904, #Took medication due sleep was restless, 0=Rarely/None of the time/1 Day, 1=Some/A little of the time/1-2 Days, 2=Occasionally/Moderate amount of the time/3-4 Days, 3=Most/All of the time/ 5-7 Days
    sad_medication_2019 = U4370905, #individual took pain medication in last month due felt sad? 0=Rarely/None of the time/1 Day, 1=Some/A little of the time/1-2 Days, 2=Occasionally/Moderate amount of the time/3-4 Days, 3=Most/All of the time/ 5-7 Days
    reason_1st_marriage_ended = Z9073400, #Reason that first marriage ended excluding. There are people with valid skip that could be still marriage or never marriage, 1=Divorced, 2=Widowed, 3=Unknown - includes legal annulments.
    marital_status = Z9149100, # ollapsed marital status as of the most recent survey date. 0=Never-married, 1=Married, 2=Separated, 3=Divorced, 4=Widowed
    highest_degree = U3572700 #Highest degree received at the end of the survey 2019
  )

#********************************************************************************************************

# 6) Handle missing values as NA
divorce_df[divorce_df == -1] = NA  # Refused 
divorce_df[divorce_df == -2] = NA  # Dont know 
divorce_df[divorce_df == -3] = NA  # Invalid missing 
divorce_df[divorce_df == -4] = NA  # Valid missing 
divorce_df[divorce_df == -5] = NA  # Non-interview 

#********************************************************************************************************
#Exploring the cleaned dataset more in deep
# head(divorce_df)
# str(divorce_df)
# colnames(divorce_df)
# summary(divorce_df)
# 
# ggplot(data = divorce_df) +
#   geom_bar(aes(live_two_parents_1997))
# 
# divorce_df %>% 
#   count(depression_medication_2019, marital_status) %>% 
#   ggplot(aes(depression_medication_2019, marital_status)) + 
#   geom_tile(aes(fill = n))

#********************************************************************************************************

#7) Saving cleaned dataframe as .csv file
#write.csv(divorce_df, '~/MEGAsync/Google DS Certificate/8) Portfolio and Case Study/Divorce_Project_2.0/2) Data Cleaning/divorce_df_CLEANED.csv')