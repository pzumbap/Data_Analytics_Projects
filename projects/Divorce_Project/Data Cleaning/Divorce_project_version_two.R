
# Set working directory
#setwd()

new_data <- read.table('Divorce_project_version_two.dat', sep=' ')
names(new_data) <- c('R0000100',
  'R0532200',
  'R0536300',
  'R0536402',
  'R0552200',
  'R0690800',
  'R0691200',
  'R1482600',
  'S1249300',
  'T1067500',
  'T3162503',
  'T9110400',
  'U4370902',
  'U4370904',
  'U4370905',
  'Z9073400',
  'Z9149100')


# Handle missing values

  new_data[new_data == -1] = NA  # Refused 
  new_data[new_data == -2] = NA  # Dont know 
  new_data[new_data == -3] = NA  # Invalid missing 
  new_data[new_data == -4] = NA  # Valid missing 
  new_data[new_data == -5] = NA  # Non-interview 


# If there are values not categorized they will be represented as NA

vallabels = function(data) {
  data$R0000100[1.0 <= data$R0000100 & data$R0000100 <= 999.0] <- 1.0
  data$R0000100[1000.0 <= data$R0000100 & data$R0000100 <= 1999.0] <- 1000.0
  data$R0000100[2000.0 <= data$R0000100 & data$R0000100 <= 2999.0] <- 2000.0
  data$R0000100[3000.0 <= data$R0000100 & data$R0000100 <= 3999.0] <- 3000.0
  data$R0000100[4000.0 <= data$R0000100 & data$R0000100 <= 4999.0] <- 4000.0
  data$R0000100[5000.0 <= data$R0000100 & data$R0000100 <= 5999.0] <- 5000.0
  data$R0000100[6000.0 <= data$R0000100 & data$R0000100 <= 6999.0] <- 6000.0
  data$R0000100[7000.0 <= data$R0000100 & data$R0000100 <= 7999.0] <- 7000.0
  data$R0000100[8000.0 <= data$R0000100 & data$R0000100 <= 8999.0] <- 8000.0
  data$R0000100[9000.0 <= data$R0000100 & data$R0000100 <= 9999.0] <- 9000.0
  data$R0000100 <- factor(data$R0000100, 
    levels=c(0.0,1.0,1000.0,2000.0,3000.0,4000.0,5000.0,6000.0,7000.0,8000.0,9000.0), 
    labels=c("0",
      "1 TO 999",
      "1000 TO 1999",
      "2000 TO 2999",
      "3000 TO 3999",
      "4000 TO 4999",
      "5000 TO 5999",
      "6000 TO 6999",
      "7000 TO 7999",
      "8000 TO 8999",
      "9000 TO 9999"))
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
    labels=c("1.  Roman Catholic",
      "2.  Baptist",
      "3.  Methodist",
      "4.  Lutheran",
      "5.  Presbyterian",
      "6.  Episcopal/Anglican",
      "7.  United Church of Christ (or Congregationalist or Evangelical Reformed)",
      "8.  Disciples of Christ (or the Christian Church)",
      "9.  Reform (or Reformed Church in America or Christian Reformed Church)",
      "10.  Holiness (Nazarene, Wesleyan, Free Methodist)",
      "11.  Pentecostal (Assembly of God, Pentecostal Holiness)",
      "12.  Nondemonimational Christian (Bible Church)",
      "13.  Other Protestant",
      "14.  Jewish  -  Orthodox",
      "15.  Jewish  -  Conservative",
      "16.  Jewish  -  Reform",
      "17.  Jewish  -  Other Jewish",
      "18.  Mormon (all types of Latter Day Saints)",
      "19.  Eastern Orthodox",
      "20.  Unitarian",
      "21.  Muslim (or Moslem or Islam)",
      "22.  Hindu/Buddhist",
      "23.  Native American Tribal Religion",
      "33.  Other (SPECIFY)",
      "24.  None, no religion - Agnostic (doesn't know if there is a God)",
      "25.  None, no religion - Atheist (confident there is no God)",
      "26.  None, no religion - Personal philosophy",
      "27.  Bah'ai",
      "28.  Greek, Roman, Norse, Etc. Mythology",
      "29.  Satanic",
      "30.  Wicca/Witchcraft/Magic/Pagan",
      "31.  Scientology",
      "32.  Other Eastern (Sikh)"))
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
      "Mixed Race (Non-Hispanic)",
      "Non-Black / Non-Hispanic"))
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
    labels=c("1 -- Disagree strongly",
      "2 -- Disagree moderately",
      "3 -- Disagree a little",
      "4 -- Neither agree nor disagree",
      "5 -- Agree a little",
      "6 -- Agree moderately",
      "7 -- Agree strongly"))
  data$T9110400 <- factor(data$T9110400, 
    levels=c(0.0,1.0), 
    labels=c("NO",
      "YES"))
  data$U4370902 <- factor(data$U4370902, 
    levels=c(0.0,1.0,2.0,3.0), 
    labels=c("Rarely/None of the time/1 Day",
      "Some/A little of the time/1-2 Days",
      "Occasionally/Moderate amount of the time/3-4 Days",
      "Most/All of the time/ 5-7 Days"))
  data$U4370904 <- factor(data$U4370904, 
    levels=c(0.0,1.0,2.0,3.0), 
    labels=c("Rarely/None of the time/1 Day",
      "Some/A little of the time/1-2 Days",
      "Occasionally/Moderate amount of the time/3-4 Days",
      "Most/All of the time/ 5-7 Days"))
  data$U4370905 <- factor(data$U4370905, 
    levels=c(0.0,1.0,2.0,3.0), 
    labels=c("Rarely/None of the time/1 Day",
      "Some/A little of the time/1-2 Days",
      "Occasionally/Moderate amount of the time/3-4 Days",
      "Most/All of the time/ 5-7 Days"))
  data$Z9073400 <- factor(data$Z9073400, 
    levels=c(1.0,2.0,3.0), 
    labels=c("Divorced",
      "Widowed",
      "Unknown - includes legal annulments"))
  data$Z9149100 <- factor(data$Z9149100, 
    levels=c(0.0,1.0,2.0,3.0,4.0), 
    labels=c("Never-married",
      "Married",
      "Separated",
      "Divorced",
      "Widowed"))
  return(data)
}

varlabels <- c("PUBID - YTH ID CODE 1997",
  "YOUTH BOTHBIO (ROS ITEM) L1 1997",
  "KEY!SEX (SYMBOL) 1997",
  "KEY!BDATE M/Y (SYMBOL) 1997",
  "WHAT RELIG PR RAISED IN? 1997",
  "R UNHAPPY, SAD, DEPRESSED (FEM) 1997",
  "R UNHAPPY, SAD, DEPRESSED (MALE) 1997",
  "KEY!RACE_ETHNICITY (SYMBOL) 1997",
  "R'S PARENTS DIVORCED IN LAST 5 YRS 2002",
  "R'S PARENTS DIVORCED IN LAST 5 YRS 2007",
  "PSNALTY SCALE: ANXIOUS, EASILY UPSET 2008",
  "R'S PARENTS DIVORCED IN LAST 6 YRS 2013",
  "CESD - DEPRESSION 2019",
  "CESD -  RESTLESS SLEEP 2019",
  "CESD - FEELING SAD 2019",
  "CVC_FIRST_MARRIAGE_END",
  "CVC_MARSTAT_COLLAPSED"
)


# Use qnames rather than rnums

qnames = function(data) {
  names(data) <- c("PUBID_1997",
    "YOUTH_BOTHBIO.01_1997",
    "KEY_SEX_1997",
    "KEY_BDATE_Y_1997",
    "P2-013_1997",
    "PC12-024_1997",
    "PC12-028_1997",
    "KEY_RACE_ETHNICITY_1997",
    "YHEA-3000_2002",
    "YHEA-3000_2007",
    "YTEL-TIPIA~000004_2008",
    "YHEA-3000_2013",
    "YHEA-CESD-1A~000003_2019",
    "YHEA-CESD-1A~000005_2019",
    "YHEA-CESD-1A~000006_2019",
    "CVC_FIRST_MARRY_END_XRND",
    "CVC_MARSTAT_COLLAPSED_XRND")
  return(data)
}


#********************************************************************************************************

# Remove the '#' before the following line to create a data file called "categories" with value labels. 
categories <- vallabels(new_data)

# Remove the '#' before the following lines to rename variables using Qnames instead of Reference Numbers
#new_data <- qnames(new_data)
#categories <- qnames(categories)

# Produce summaries for the raw (uncategorized) data file
summary(new_data)

# Remove the '#' before the following lines to produce summaries for the "categories" data file.
categories <- vallabels(new_data)
summary(categories)

#************************************************************************************************************

