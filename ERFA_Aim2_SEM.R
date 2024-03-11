###########################################################################################################################

### Reading the data

setwd("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA")

data <- read.csv("ERFA.Aim2.Data.csv", header=T)
dim(data)
# [1] 6089  283

d <- data


#########################

### Recreating the CONSORT

d1 <- subset(d, !is.na(Year1_INC_Sz_until2020))
dim(d1)
# [1] 5108 283

table(d1$FIMMOT_indep.1)
sum(table(d1$FIMMOT_indep.1))
sum(table(d1$FIMCOG_indep.1))
sum(table(d1$depression_fann.1))
sum(table(d1$GAD7TOT.1.binary))
sum(table(d1$TransMode.1.binary))
sum(table(d1$PARTSummary.2))
sum(table(d1$SWLSTOT.new.2))

d2 <- subset(d1, !is.na(FIMMOT_indep.1) & !is.na(FIMCOG_indep.1) &
               !is.na(depression_fann.1) & !is.na(GAD7TOT.1.binary) &
               !is.na(TransMode.1.binary) & !is.na(PARTSummary.2) &
               !is.na(SWLSTOT.new.2))
dim(d2)
# [1] 1214  283

# 1003 comes from running "erfa.sem.v6.FINAL.REDONE" in R
1214-1003
# [1] 211

d3 <- subset(d2, !is.na(DRS_PIEmpF_bin.1) & !is.na(DRS_PIEmpF_bin.2) & !is.na(PROBLEMUse.new.1) & !is.na(PROBLEMUse.new.2) &
               !is.na(AGE) & !is.na(Sex.new) &
               !is.na(DOD_Injury_severity) & !is.na(EduYears) &
               !is.na(ResF.1.binary) & !is.na(ResF.2.binary) & !is.na(Year1_INC_Sz_until2020))

dim(d3)
# [1] 1003  283

library(dplyr)

### Sensitivity analysis between d3 and d4

anti_join(d2, d3, by = "ID")

length(intersect(d2$Mod1id, d3$Mod1id))

length(setdiff(d2$Mod1id, d3$Mod1id))

d4 <- anti_join(d2, d3, by = "Mod1id")
dim(d4)
# [1] 211 283

library(table1)

table1(~ factor(Year1_INC_Sz_until2020) +
         factor(FIMMOT_indep.1) + 
         factor(FIMCOG_indep.1) +
         factor(depression_fann.1) + 
         factor(GAD7TOT.1.binary) +
         factor(TransMode.1.binary) + 
         PARTSummary.2 +
         SWLSTOT.new.2 +
         factor(DRS_PIEmpF_bin.1) + 
         factor(DRS_PIEmpF_bin.2) + 
         factor(PROBLEMUse.new.1) + 
         factor(PROBLEMUse.new.2) +
         AGE +
         factor(Sex.new) +
         factor(DOD_Injury_severity) + 
         EduYears +
         factor(ResF.1.binary) + 
         factor(ResF.2.binary), data=d3)

table1(~ factor(Year1_INC_Sz_until2020) +
         factor(FIMMOT_indep.1) + 
         factor(FIMCOG_indep.1) +
         factor(depression_fann.1) + 
         factor(GAD7TOT.1.binary) +
         factor(TransMode.1.binary) + 
         PARTSummary.2 +
         SWLSTOT.new.2 +
         factor(DRS_PIEmpF_bin.1) + 
         factor(DRS_PIEmpF_bin.2) + 
         factor(PROBLEMUse.new.1) + 
         factor(PROBLEMUse.new.2) +
         AGE +
         factor(Sex.new) +
         factor(DOD_Injury_severity) + 
         EduYears +
         factor(ResF.1.binary) + 
         factor(ResF.2.binary), data=d4, render.missing=NULL, render.categorical="FREQ (PCTnoNA%)")

# Creating a matrix for the observed frequencies
observed <- matrix(c(884, 119, 189, 22), ncol = 2, byrow = TRUE)

# Performing the chi-squared test
chi_square_result <- chisq.test(observed, correct = FALSE)

# Printing the result
print(chi_square_result)

# Creating a matrix representing the contingency table
contingency_table <- matrix(c(183, 49, 820, 162), nrow = 2, byrow = TRUE)

contingency_table <- matrix(c(183, 49, 820, 162), nrow = 2, byrow = TRUE)


# Performing chi-squared test
chi_squared_result <- chisq.test(contingency_table)

# Create the observed data matrix
observed_data <- matrix(c(272, 55, 731, 156), nrow = 2, byrow = TRUE)

# Perform the chi-squared test
chisq_result <- chisq.test(observed_data)

# Print the result
print(chisq_result)

# Create the contingency table
data <- matrix(c(878, 185, 125, 26), nrow = 2, byrow = TRUE)

# Perform the chi-squared test
result <- chisq.test(data)

# Print the result
print(result)

# Creating a matrix with the observed frequencies
observed <- matrix(c(853, 180, 150, 31), ncol = 2, byrow = TRUE)

# Performing the chi-squared test
chisq_result <- chisq.test(observed)

# Printing the result
print(chisq_result)

# Create a matrix representing the contingency table
data_matrix <- matrix(c(507, 118, 496, 93), nrow = 2, byrow = TRUE)

# Perform the chi-squared test
chi_squared_result <- chisq.test(data_matrix)

# Print the result
print(chi_squared_result)

# Create a matrix with the data
observed_data <- matrix(c(125, 22, 878, 156), nrow = 2, byrow = TRUE)

# Perform a chi-squared test
chi_squared_result <- chisq.test(observed_data)

# Print the result
print(chi_squared_result)

# Creating a matrix with the provided data
observed_data <- matrix(c(128, 17, 875, 123), nrow = 2, byrow = TRUE)

# Performing the chi-squared test
chi_squared_result <- chisq.test(observed_data)

# Printing the result
print(chi_squared_result)

# Create a matrix representing the 2x2 table
observed_data <- matrix(c(771, 232, 164, 38), nrow = 2, byrow = TRUE)

# Perform the chi-squared test
chi_square_result <- chisq.test(observed_data, correct = FALSE)

# Print the result
print(chi_square_result)

# Creating a matrix for the observed frequencies
observed <- matrix(c(709, 294, 147, 51), nrow = 2, byrow = TRUE)

# Performing the chi-squared test
chi_squared_result <- chisq.test(observed)

# Printing the results
print(chi_squared_result)

# Create a matrix representing the provided table
table_data <- matrix(c(246, 63, 757, 147), ncol = 2, byrow = TRUE)

# Perform a chi-squared test
chi_squared_result <- chisq.test(table_data, correct = FALSE)

# Print the result
print(chi_squared_result)

# Create a matrix for the 2x2 contingency table
contingency_table <- matrix(c(236, 24, 767, 95), nrow = 2, byrow = TRUE)

# Perform the chi-squared test
result_chi_sq <- chisq.test(contingency_table, correct = FALSE)

# Print the result
print(result_chi_sq)

# Creating a matrix with the provided data
data_matrix <- matrix(c(21, 8, 982, 203), nrow = 2, byrow = TRUE)

# Performing the chi-squared test
chi_squared_result <- chisq.test(data_matrix)

# Printing the result
print(chi_squared_result)

# Create a matrix for the given table
my_table <- matrix(c(15, 988, 6, 201), ncol = 2, byrow = TRUE)

# Perform the chi-squared test
result <- chisq.test(my_table)

# Print the result
print(result)


### Hand written function for t-test
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

# Given information
m1 <- 1.86
s1 <- 0.674
n1 <- 1003

m2 <- 1.75
s2 <- 0.606
n2 <- 211

# Using the t.test2 function
result <- t.test2(m1, m2, s1, s2, n1, n2)

# Extracting the p-value
p_value <- result[["p-value"]]

# Print the p-value
print(p_value)


# Given information
m1 <- 4.01
s1 <- 1.58
n1 <- 1003
  
m2 <- 3.74
s2 <- 1.66
n2 <- 211
  
  # Using the t.test2 function
  result <- t.test2(m1, m2, s1, s2, n1, n2)

# Extracting the p-value
p_value <- result[["p-value"]]

# Print the p-value
print(p_value)


# Given information
m1 <- 42.8
s1 <- 18.8
n1 <- 1003

m2 <- 44.1
s2 <- 20.0
n2 <- 211
  
  # Using the t.test2 function
  result <- t.test2(m1, m2, s1, s2, n1, n2)

# Extracting the p-value
p_value <- result[["p-value"]]

# Print the p-value
print(p_value)


# Given information
m1 <- 13.3
s1 <- 2.89
n1 <- 1003
  
m2 <- 13.6
s2 <- 2.96
n2 <- 211
  
  # Using the t.test2 function
  result <- t.test2(m1, m2, s1, s2, n1, n2)

# Extracting the p-value
p_value <- result[["p-value"]]

# Print the p-value
print(p_value)

wilcox.test(d3$PARTSummary.2, d4$PARTSummary.2)
wilcox.test(d3$SWLSTOT.2, d4$SWLSTOT.2)


#########################

## Variables Dr. Wagner suggested adding

# What about year 1 rehospitalization as an intermediate variable?
#   
# Marital status, race/ethnicity and previous incarceration and living with status could be relevant covariates.
# 
# Premorbidity variables to consider that we have already prepared for this cohort include: premorbid psych history, drugs/alcohol, other medical premorbidities that we pulled out for the LASSO like headache and neurodegenerative disease, stroke etc.

#########################


#########################

## Reading ERFA LASSO data to bring in other variables for year-1 variables 

mod1id_sz_icd_V4 <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 1 LASSO/Code/NEW CODES FOR NEW MODELS/Data management/mod1id_sz_icd_V4.csv", header = T)
dim(mod1id_sz_icd_V4)
# [1] 6089  366

mod1id_sz_icd_V4$Mod1id <- mod1id_sz_icd_V4$Mod1Id

lasso.year1.vars <- mod1id_sz_icd_V4[,c(
  "Mod1id",
  "Race",
  "Mar",
  "Incarcerate"
)]

#########################


## Merge ERFA SEM data with LASSO cohort to bring in year-1 variables 

d1 <- merge(data, mod1id_sz_icd_V4, by="Mod1id")


#########################

## Reading ERFA LASSO data to bring in other variables

### Reading the new data sent by CB on 2/18/2021 (the data were updated on 1/18/2021)

TBIMS_Form2_01_18_2021 <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/TBIMS data from Box/02_18_2021/TBIMSForm2_20210118_SAS/TBIMS_Form2_01_18_2021.csv", header = T)
dim(TBIMS_Form2_01_18_2021)
# [1] 69404   404

form2.select <- TBIMS_Form2_01_18_2021[,c("Mod2id", "Mod1id",
                                          "FollowUpPeriod",
                                          "PastYearSeiz",
                                          "PROBLEMUseF",
                                          "REHOSP"
)]
dim(form2.select)
# [1] 69404   6

form2.select.V2 <- subset(form2.select, FollowUpPeriod==1 | FollowUpPeriod==2)
dim(form2.select.V2)
# [1] 34784     6


## Cross tables with frequencies for NA:
my_table <- function(x){
  setNames(table(x,useNA = "always"),c(sort(unique(x[!is.na(x)])),'NA'))
}

# PastYearSeiz -> PastYearSeiz.new (link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=FUSEIZ)
my_table(form2.select.V2$PastYearSeiz)

# Use
# 66==6
# 88=8

form2.select.V2$PastYearSeiz.new <- form2.select.V2$PastYearSeiz
form2.select.V2$PastYearSeiz.new[form2.select.V2$PastYearSeiz==1 | form2.select.V2$PastYearSeiz==2 | form2.select.V2$PastYearSeiz==3 | form2.select.V2$PastYearSeiz==4 | form2.select.V2$PastYearSeiz==5] <- 1
form2.select.V2$PastYearSeiz.new[form2.select.V2$PastYearSeiz==8 | form2.select.V2$PastYearSeiz==88] <- 0
form2.select.V2$PastYearSeiz.new[form2.select.V2$PastYearSeiz==6 | form2.select.V2$PastYearSeiz==9 | form2.select.V2$PastYearSeiz==66] <- NA
my_table(form2.select.V2$PastYearSeiz.new)
### We will not use this variable, we will use that Dom sent: 
### "C:\Nabil Awan\Nabil_University of Pittsburgh\GSR\ERFA grant\Aim 2 SEM\TBIMS data\ERFA_Aim2_SeizureVar.csv"
### SeizureVarData <- read.csv("ERFA_Aim2_SeizureVar.csv", header = T) above
my_table(form2.select.V2$PastYearSeiz.new[form2.select.V2$FollowUpPeriod==1])
my_table(form2.select.V2$PastYearSeiz.new[form2.select.V2$FollowUpPeriod==2])

# PROBLEMUseF: 1 - No; 2 - Yes; 7 - Refused; 9 - Unknown;
table(form2.select.V2$PROBLEMUseF)
# > table(form2.select.V2$PROBLEMUseF)
# 
# 1     2   4.5     7     9    99 
# 20357  5299     1   443  1872     1 
form2.select.V2$PROBLEMUseF[form2.select.V2$PROBLEMUseF>2] <- NA
table(form2.select.V2$PROBLEMUseF)
form2.select.V2$PROBLEMUseF <- form2.select.V2$PROBLEMUseF-1
table(form2.select.V2$PROBLEMUseF)

# REHOSP: 1 - No; 2 - Yes; 666 - Variable Did Not Exist; 888 - Not Applicable; 999 - Unknown;
table(form2.select.V2$REHOSP)
# > table(form2.select.V2$REHOSP)
# 
# 1 1.09127 1.17857       2      99 
# 20112       1       1    6858     945
form2.select.V2$REHOSP[form2.select.V2$REHOSP>2] <- NA
form2.select.V2$REHOSP[form2.select.V2$REHOSP>1 & form2.select.V2$REHOSP<2] <- NA
table(form2.select.V2$REHOSP)
form2.select.V2$REHOSP <- form2.select.V2$REHOSP-1
table(form2.select.V2$REHOSP)



## Reshaping data
form2.wide <- reshape(form2.select.V2, idvar = "Mod1id", timevar = "FollowUpPeriod", direction = "wide")
dim(form2.wide)
# [1] 18185     11

names(form2.wide)

# edit(form2.wide)


### Merging the datasets Full_Baseline_Cohort_V3 and form2.wide 

form2.wide$Mod1Id <- form2.wide$Mod1id

# names(form2.select.V2)

#########################

## Merging the 

check <- merge(Full_Baseline_Cohort_V3, form2.wide, by="Mod1Id", all.x = T)
dim(check)
# [1] 5371   61


d <- data

###########################################################################################################################

hist(d$SWLSTOT.1)
hist(d$SWLSTOT.2)

hist(d$SWLSTOT.new.1)
hist(d$SWLSTOT.new.2)


###########################################################################################################################

###########################################################################################################################

## Date: 4/22/2021 (run before NNS 2021 abstract deadline)

## Year-1 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

### Model-1

library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)


### Adjusting for all covariances

### Model-3.1

library(lavaan)

erfa.sem.year1.v3.1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary
    FIMCOG_indep.1 ~~ TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary
    depression_fann.1 ~~ TransMode.1.binary + GAD7TOT.1.binary
    GAD7TOT.1.binary ~~ TransMode.1.binary
'

# fit the model 
sem.mod.year1.v3.1 <- sem(erfa.sem.year1.v3.1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v3.1, standardized=T, fit.measure=T)



### Adjusting for selected covariances

### Model-3.2

library(lavaan)

erfa.sem.year1.v3.2 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v3.2 <- sem(erfa.sem.year1.v3.2, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v3.2, standardized=T, fit.measure=T)




my_table(data$FIMMOT_indep.1)
my_table(data$FIMCOG_indep.1)
my_table(data$TransMode.1.binary)
my_table(data$depression_fann.1)
my_table(data$GAD7TOT.1.binary)
sum(is.na(data$PARTSummary.1))
my_table(data$SWLSTOT.new.1)

hist(data$SWLSTOT.new.1)
hist(data$SWLSTOT.new.2)


## With all covariates and mostly binary outcomes (and SWLS as continuous 5-35)

hist(data$SWLSTOT.1)

library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)

my_table(data$FIMMOT_indep.1)
my_table(data$FIMCOG_indep.1)
my_table(data$TransMode.1.binary)
my_table(data$depression_fann.1)
my_table(data$GAD7TOT.1.binary)
sum(is.na(data$PARTSummary.1))
my_table(data$SWLSTOT.1)


###########################################################################################################################



tapply(d$SWLSTOT.new.1, d$TransMode.1.binary, summary)


library(data.table)
setDT(d)
d[, as.list(summary(SWLSTOT.new.1)), by = TransMode.1.binary]

# Libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

# With transparency
p2 <- ggplot(data=d, aes(x=SWLSTOT.new.1, group=as.factor(TransMode.1.binary), fill=as.factor(TransMode.1.binary))) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()
#p2

###########################################################################################################################

## Date: 4/22/2021 (run before NNS 2021 abstract deadline)

## Year-2 ERFA SEM 

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

### Model-2

library(lavaan)

erfa.sem.year2.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
sem.mod.year2.v1 <- sem(erfa.sem.year2.v1, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(sem.mod.year2.v1, standardized=T, fit.measure=T)



### Model-4.1

library(lavaan)

erfa.sem.year2.v4.1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.2 ~~ FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary
    FIMCOG_indep.2 ~~ TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary
    depression_fann.2 ~~ TransMode.2.binary + GAD7TOT.2.binary
    GAD7TOT.2.binary ~~ TransMode.2.binary

'

# fit the model 
sem.mod.year2.v4.1 <- sem(erfa.sem.year2.v4.1, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(sem.mod.year2.v4.1, standardized=T, fit.measure=T)


### Model-4.2

library(lavaan)

erfa.sem.year2.v4.2 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.2 ~~ FIMCOG_indep.2
    depression_fann.2 ~~ GAD7TOT.2.binary

'

# fit the model 
sem.mod.year2.v4.2 <- sem(erfa.sem.year2.v4.2, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(sem.mod.year2.v4.2, standardized=T, fit.measure=T)




### Model-4.3 (continuous SWLS), still shows warnings

library(lavaan)

erfa.sem.year2.v4.3 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.2 ~~ FIMCOG_indep.2
    depression_fann.2 ~~ GAD7TOT.2.binary

'

# fit the model 
sem.mod.year2.v4.3 <- sem(erfa.sem.year2.v4.3, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary"))
summary(sem.mod.year2.v4.3, standardized=T, fit.measure=T)



my_table(data$FIMMOT_indep.2)
my_table(data$FIMCOG_indep.2)
my_table(data$TransMode.2.binary)
my_table(data$depression_fann.2)
my_table(data$GAD7TOT.2.binary)
sum(is.na(data$PARTSummary.2))
my_table(data$SWLSTOT.new.2)



## With all covariates and mostly binary outcomes (and SWLS as continuous 5-35)

hist(data$SWLSTOT.2)

library(lavaan)

erfa.sem.year2.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
sem.mod.year2.v1 <- sem(erfa.sem.year2.v1, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary"))
summary(sem.mod.year2.v1, standardized=T, fit.measure=T)

my_table(data$FIMMOT_indep.2)
my_table(data$FIMCOG_indep.2)
my_table(data$TransMode.2.binary)
my_table(data$depression_fann.2)
my_table(data$GAD7TOT.2.binary)
sum(is.na(data$PARTSummary.2))
my_table(data$SWLSTOT.2)

###########################################################################################################################

###########################################################################################################################

## Date: 4/22/2021 (run before NNS 2021 abstract deadline)

#### Year-1 and year-2 ERFA CLSEM 

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.clsem.v1 <- '
# synchronous covariances
# None

## Regression paths

## Year-1

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary


## Year-2

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ FIMMOT_indep.1 + Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ FIMCOG_indep.1 + Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ TransMode.1.binary + Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ depression_fann.1 + Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ GAD7TOT.1.binary + Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ PARTSummary.1 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ SWLSTOT.new.1 + PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
clsem.mod.v1 <- sem(erfa.clsem.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1","FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(clsem.mod.v1, standardized=T, fit.measure=T)

my_table(data$FIMMOT_indep.2)
my_table(data$FIMCOG_indep.2)
my_table(data$TransMode.2.binary)
my_table(data$depression_fann.2)
my_table(data$GAD7TOT.2.binary)
sum(is.na(data$PARTSummary.2))
my_table(data$SWLSTOT.new.2)

###########################################################################################################################

###########################################################################################################################

###

###########################################################################################################################

###########################################################################################################################

###

###########################################################################################################################

###########################################################################################################################

## Date: 7/14/2021 (run before NNS 2021 abstract deadline)

## Year-1 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year1.v3 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
sem.mod.year1.v3 <- sem(erfa.sem.year1.v3, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v3, standardized=T, fit.measure=T)
###

my_table(data$FIMMOT_indep.1)
my_table(data$FIMCOG_indep.1)
my_table(data$TransMode.1.binary)
my_table(data$depression_fann.1)
my_table(data$GAD7TOT.1.binary)
sum(is.na(data$PARTSummary.1))
my_table(data$SWLSTOT.new.1)
hist(data$SWLSTOT.new.1)

###########################################################################################################################

###########################################################################################################################

###

## Date: 7/14/2021 (run before NNS 2021 abstract deadline)

## Year-2 ERFA SEM 

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year2.v3 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
sem.mod.year2.v3 <- sem(erfa.sem.year2.v3, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(sem.mod.year2.v3, standardized=T, fit.measure=T)

###

###########################################################################################################################

###########################################################################################################################

### Full model with FIM going to driving status and all of those going to depression and anxiety
## Date: 7/14/2021 (run before NNS 2021 abstract deadline)

## Year-1 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year1.v4 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ Year1_INC_Sz_until2020 + PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
sem.mod.year1.v4 <- sem(erfa.sem.year1.v4, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v4, standardized=T, fit.measure=T)


###########################################################################################################################

###########################################################################################################################

###
## Year-2 ERFA SEM 

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year2.v4 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
sem.mod.year2.v4 <- sem(erfa.sem.year2.v4, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(sem.mod.year2.v4, standardized=T, fit.measure=T)

###########################################################################################################################




###########################################################################################################################

###########################################################################################################################

### Modified model 
## Date: 7/14/2021 (run before NNS 2021 abstract deadline)

### Recoding FIM data
data$FIMMOT_dep.1 <- ifelse(data$FIMMOT_indep.1==1, 0, 1) 
data$FIMCOG_dep.1 <- ifelse(data$FIMCOG_indep.1==1, 0, 1)
data$FIMMOT_dep.2 <- ifelse(data$FIMMOT_indep.2==1, 0, 1) 
data$FIMCOG_dep.2 <- ifelse(data$FIMCOG_indep.2==1, 0, 1)

table(data$FIMMOT_dep.1)
table(data$FIMMOT_indep.1)

data$TransMode.1.binary.NO <- ifelse(data$TransMode.1.binary==1, 0, 1)
data$TransMode.2.binary.NO <- ifelse(data$TransMode.2.binary==1, 0, 1)

table(data$TransMode.1.binary.NO)
table(data$TransMode.1.binary)

# Checking interactions: data$Year1_INC_Sz_until2020 data$FIMCOG_dep.1 data$TransMode.1.binary

summary(glm(TransMode.1.binary.NO~FIMCOG_dep.1*Year1_INC_Sz_until2020, family = "binomial", data=data))
summary(glm(TransMode.1.binary.NO~factor(FIMCOG_dep.1)*factor(Year1_INC_Sz_until2020), family = "binomial", data=data))
summary(glm(TransMode.1.binary~factor(FIMCOG_dep.1)*factor(Year1_INC_Sz_until2020), family = "binomial", data=data))
# A trending interaction between driving and FIM-cog

# Dummies

# data$FIMCOG_dep.1_1.Year1_INC_Sz_until2020_0 <- ifelse(data$FIMCOG_dep.1==1 & data$Year1_INC_Sz_until2020==0, 1, 0)
# data$FIMCOG_dep.1_0.Year1_INC_Sz_until2020_1 <- ifelse(data$FIMCOG_dep.1==0 & data$Year1_INC_Sz_until2020==1, 1, 0)
# data$FIMCOG_dep.1_1.Year1_INC_Sz_until2020_1 <- ifelse(data$FIMCOG_dep.1==1 & data$Year1_INC_Sz_until2020==1, 1, 0)

# Graph and look at the frequency of driving status

data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020 <- ifelse(data$FIMCOG_dep.1==0 & data$Year1_INC_Sz_until2020==0, 1,
                                                       ifelse(data$FIMCOG_dep.1==1 & data$Year1_INC_Sz_until2020==0, 2,
                                                              ifelse(data$FIMCOG_dep.1==0 & data$Year1_INC_Sz_until2020==1, 3, 4)))
table(data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020)

data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020 <- factor(data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020,
                                                       levels = c(1, 2, 3, 4),
                                                       labels = c("No Seiz & Indep Cog", "No Seiz & Dep Cog", "Seiz & Indep Cog", "Seiz & Dep Cog"))
table(data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020)

# data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020[data$FIMCOG_dep.1==0 & data$Year1_INC_Sz_until2020==0] <- 1
# data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020[data$FIMCOG_dep.1==1 & data$Year1_INC_Sz_until2020==0] <- 2
# data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020[data$FIMCOG_dep.1==0 & data$Year1_INC_Sz_until2020==1] <- 3
# data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020[data$FIMCOG_dep.1==1 & data$Year1_INC_Sz_until2020==1] <- 4
# 
# table(data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020)
# 
# sum(data$FIMCOG_dep.1==1 & data$Year1_INC_Sz_until2020==0, na.rm = T)

counts <- table(data$TransMode.1.binary, data$FIMCOG_dep.1_and_Year1_INC_Sz_until2020)
counts
barplot(counts, main="Distribution of people who are driving",
        xlab="")

summary(glm(TransMode.1.binary~FIMCOG_dep.1_and_Year1_INC_Sz_until2020, family = "binomial", data=data))
summary(glm(TransMode.1.binary~factor(FIMCOG_dep.1)*factor(Year1_INC_Sz_until2020), family = "binomial", data=data))
summary(glm(TransMode.1.binary~FIMCOG_dep.1*Year1_INC_Sz_until2020, family = "binomial", data=data))

-2.02132 + -0.87408 + 0.40173
# [1] -2.49367

# So, without creating dummies the last term is only for the interaction






# Checking interactions: data$Year1_INC_Sz_until2020 data$FIMCOG_dep.1 data$TransMode.1.binary

summary(glm(depression_fann.1~TransMode.1.binary.NO*FIMCOG_dep.1*Year1_INC_Sz_until2020, family = "binomial", data=data))
# driving and FIM-cog interaction present

summary(glm(depression_fann.1~TransMode.1.binary.NO*FIMCOG_dep.1, family = "binomial", data=data))


summary(glm(GAD7TOT.1.binary~TransMode.1.binary.NO*FIMCOG_dep.1*Year1_INC_Sz_until2020, family = "binomial", data=data))

summary(glm(SWLSTOT.new.1~PARTSummary.1*FIMCOG_dep.1*Year1_INC_Sz_until2020, family = "gaussian", data=data))
# driving and FIM-cog interaction present


## Year-1 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year1.v4 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_dep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_dep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_dep.1 + FIMCOG_dep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_dep.1 + FIMCOG_dep.1 + TransMode.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_dep.1 + FIMCOG_dep.1 + TransMode.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ Year1_INC_Sz_until2020 + FIMMOT_dep.1 + FIMCOG_dep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ Year1_INC_Sz_until2020 + PARTSummary.1 + FIMMOT_dep.1 + FIMCOG_dep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
sem.mod.year1.v4 <- sem(erfa.sem.year1.v4, data=data, ordered=c("FIMMOT_dep.1","FIMCOG_dep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v4, standardized=T, fit.measure=T)

## 3-way interaction among Sz, FIM-cog and driving for anxiety and depression models.
## 2-way interaction among Sz and FIM-cog for anxiety and depression models.


###########################################################################################################################

### Suggested model with temporality and variables from the original model without adjusting for covariances

### Model-5

library(lavaan)

erfa.sem.year1.v5 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
sem.mod.year1.v5 <- sem(erfa.sem.year1.v5, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.year1.v5, standardized=T, fit.measure=T)


###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances

### Model-6

library(lavaan)

erfa.sem.year1.v6 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v6 <- sem(erfa.sem.year1.v6, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.year1.v6, standardized=T, fit.measure=T)

###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances (ordinal SWLS, removed FIM at discharge)

### Model-6 (FINAL)

library(lavaan)

erfa.sem.v6.FINAL <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.v6.FINAL <- sem(erfa.sem.v6.FINAL, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.v6.FINAL, standardized=T, fit.measure=T)


###########################################################################################################################



###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances (ordinal SWLS, removed FIM at discharge) and FIM and MH variables covariances (both way)

### Model-6 (FINAL.test)

library(lavaan)

erfa.sem.v6.FINAL.test <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary
    FIMMOT_indep.1 ~~ depression_fann.1
    FIMCOG_indep.1 ~~ depression_fann.1
    FIMMOT_indep.1 ~~ GAD7TOT.1.binary
    FIMCOG_indep.1 ~~ GAD7TOT.1.binary
'

# fit the model 
sem.mod.v6.FINAL.test <- sem(erfa.sem.v6.FINAL.test, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.v6.FINAL.test, standardized=T, fit.measure=T)


###########################################################################################################################



###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances (ordinal SWLS, removed FIM at discharge) and FIM and MH variables covariances (both way)

### Model-6 (FINAL.REDONE)
# NOTE: Doesn't run on R Studio, but runs on R on my laptop.

library(lavaan)

erfa.sem.v6.FINAL.REDONE <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary
    # FIMMOT_indep.1 ~~ depression_fann.1
    FIMCOG_indep.1 ~~ depression_fann.1
    # FIMMOT_indep.1 ~~ GAD7TOT.1.binary
    FIMCOG_indep.1 ~~ GAD7TOT.1.binary
'

# fit the model 
sem.mod.v6.FINAL.REDONE <- sem(erfa.sem.v6.FINAL.REDONE, data=d, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.v6.FINAL.REDONE, standardized=T, fit.measure=T)

###########################################################################################################################



###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances (ordinal SWLS, removed FIM at discharge) and FIM and MH variables covariances (both way)

### Model-6 (FINAL.test)

library(lavaan)

erfa.sem.v6.FINAL.test <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary
#    FIMMOT_indep.1 ~~ depression_fann.1
    FIMCOG_indep.1 ~~ depression_fann.1
#    FIMMOT_indep.1 ~~ GAD7TOT.1.binary
    FIMCOG_indep.1 ~~ GAD7TOT.1.binary
'

# fit the model 
sem.mod.v6.FINAL.test <- sem(erfa.sem.v6.FINAL.test, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.v6.FINAL.test, standardized=T, fit.measure=T)


###########################################################################################################################







### Suggested model with temporality and variables from the original model adjusting for covariances

### Model-6.1

library(lavaan)

erfa.sem.year1.v6.1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v6.1 <- sem(erfa.sem.year1.v6.1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v6.1, standardized=T, fit.measure=T)


###########################################################################################################################


###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances, FIM at discharge removed

### Model-6.2

library(lavaan)

erfa.sem.year1.v6.2 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr2 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v6.2 <- sem(erfa.sem.year1.v6.2, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v6.2, standardized=T, fit.measure=T)



###########################################################################################################################

# Test model

library(lavaan)

erfa.sem.year.test <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr2 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year.test <- sem(erfa.sem.year.test, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
# sem.mod.year.test <- sem(erfa.sem.year.test, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","depression_fann.1","GAD7TOT.1.binary","TransMode.1.binary"))
summary(sem.mod.year.test, standardized=T, fit.measure=T)

# Libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

# SWLS (year-2) vs. driving status (year-1) with transparency

class(d$TransMode.1.binary)
d$TransMode.1.binary <- as.factor(d$TransMode.1.binary)
# data <- data %>%
#   mutate(TransMode.1.binary = as.factor(TransMode.1.binary))
ggplot(d, aes(x = SWLSTOT.2, colour = TransMode.1.binary)) +
  geom_density(alpha=0.5)
ggplot(d, aes(x = SWLSTOT.2, fill = TransMode.1.binary)) +
  geom_density(alpha=0.5)

ggplot(d, aes(x = SWLSTOT.2, fill = TransMode.1.binary)) +
  geom_histogram(position = "identity", alpha=0.5, bins=20)


ggplot(d, aes(x = SWLSTOT.new.2, fill = TransMode.1.binary)) +
  geom_histogram(position = "identity", alpha=0.5, bins=20)

###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances, FIM at discharge removed, driving status had high correlation with SWLS so removed

### Model-6.2.2

library(lavaan)

erfa.sem.year1.v6.2.2 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr2 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v6.2.2 <- sem(erfa.sem.year1.v6.2.2, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v6.2.2, standardized=T, fit.measure=T)


###########################################################################################################################

###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances, FIM at discharge removed, driving status had high correlation with SWLS so removed, also without FIMMOT_indep.1 and FIMCOG_indep.1 for the SLWS path

### Model-6.2.3

library(lavaan)

erfa.sem.year1.v6.2.3 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr2 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + depression_fann.1 + GAD7TOT.1.binary + TransMode.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v6.2.3 <- sem(erfa.sem.year1.v6.2.3, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v6.2.3, standardized=T, fit.measure=T)


###########################################################################################################################


###########################################################################################################################

### Suggested model with temporality and variables from the original model adjusting for covariances, FIM at discharge removed, FIMMOT removed

### Model-6.3

library(lavaan)

erfa.sem.year1.v6.3 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr2 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ Year1_INC_Sz_until2020 + PARTSummary.2 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    # FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v6.3 <- sem(erfa.sem.year1.v6.3, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v6.3, standardized=T, fit.measure=T)


###########################################################################################################################

### Suggested model with temporality and FIM coded independent only for 7 adjusting for covariances

### Model-8

library(lavaan)

erfa.sem.year1.v8 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.7.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.7.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.7.1 ~~ FIMCOG_indep.7.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v8 <- sem(erfa.sem.year1.v8, data=data, ordered=c("FIMMOT_indep.7.1","FIMCOG_indep.7.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.year1.v8, standardized=T, fit.measure=T)

###########################################################################################################################

### Suggested model with temporality and FIM coded independent only for 7 and continuous SWLS adjusting for covariances

### Model-12

library(lavaan)

erfa.sem.year1.v12 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.7.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.7.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ PARTSummary.2 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.7.1 ~~ FIMCOG_indep.7.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v12 <- sem(erfa.sem.year1.v12, data=data, ordered=c("FIMMOT_indep.7.1","FIMCOG_indep.7.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v12, standardized=T, fit.measure=T)

###########################################################################################################################


###########################################################################################################################

### Suggested model with temporality and FIM coded independent only for 7 and continuous SWLS adjusting for covariances

### Model-8 (TEST)

library(lavaan)

erfa.sem.v8.test <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.7.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.7.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.7.1 + FIMCOG_indep.7.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.7.1 ~~ FIMCOG_indep.7.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.v8.test <- sem(erfa.sem.v8.test, data=data, ordered=c("FIMMOT_indep.7.1","FIMCOG_indep.7.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2"))
summary(sem.mod.v8.test, standardized=T, fit.measure=T)

###########################################################################################################################


###########################################################################################################################

### All year-1 and year-2 variables and covariances considered together (year-2 seizure, ordinal SWLS, FIM(indep=6,7))

### Model-13

library(lavaan)

erfa.sem.year1.v13 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Seiz.2 ~ Seiz.1
Year2_INC_Sz_until2020 ~ Year1_INC_Sz_until2020 + AGE + Sex.new + DOD_Injury_severity

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v13 <- sem(erfa.sem.year1.v13, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.2","Year2_INC_Sz_until2020"))
summary(sem.mod.year1.v13, standardized=T, fit.measure=T)

###########################################################################################################################

### All year-1 and year-2 variables and covariances considered together (year-2 seizure, continuous SWLS, FIM(indep=6,7))

### Model-14

library(lavaan)

erfa.sem.year1.v14 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Seiz.2 ~ Seiz.1
Year2_INC_Sz_until2020 ~ Year1_INC_Sz_until2020 + AGE + Sex.new + DOD_Injury_severity

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v14 <- sem(erfa.sem.year1.v14, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","Year2_INC_Sz_until2020"))
summary(sem.mod.year1.v14, standardized=T, fit.measure=T)


###########################################################################################################################

###########################################################################################################################

### All year-1 and year-2 variables and covariances considered together (year-2 seizure, continuous SWLS, FIM(indep=6,7)), FIM at discharge removed

### Model-15

library(lavaan)

erfa.sem.year1.v15 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Seiz.2 ~ Seiz.1
Year2_INC_Sz_until2020 ~ Year1_INC_Sz_until2020 + AGE + Sex.new + DOD_Injury_severity

# Participation_yr2 ~ PTS_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ Year1_INC_Sz_until2020 + Year2_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# SWLS_yr2 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.2 ~ Year1_INC_Sz_until2020 + Year2_INC_Sz_until2020 + PARTSummary.2 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# residual correlations
    FIMMOT_indep.1 ~~ FIMCOG_indep.1
    depression_fann.1 ~~ GAD7TOT.1.binary

'

# fit the model 
sem.mod.year1.v15 <- sem(erfa.sem.year1.v15, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
summary(sem.mod.year1.v15, standardized=T, fit.measure=T)

summary(sem.mod.year1.v6.2, standardized=T, fit.measure=T)

###########################################################################################################################


###########################################################################################################################


###########################################################################################################################


###########################################################################################################################


###########################################################################################################################

table(d$SWLSTOT.1)
hist(d$SWLSTOT.1)

table(d$FIMCOG_indep.1)
hist(d$FIMCOG_indep.1)

table(d$FIMCOG_indep.7.1)
hist(d$FIMCOG_indep.7.1)

table(d$FIMCOG_indep.2)
hist(d$FIMCOG_indep.2)

table(d$FIMCOG_indep.7.2)
hist(d$FIMCOG_indep.7.2)

###########################################################################################################################
