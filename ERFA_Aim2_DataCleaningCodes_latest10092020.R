###################################################################################################
### Reading some user-written functions

## Cross tables with frequencies for NA:
my_table <- function(x){
  setNames(table(x,useNA = "always"),c(sort(unique(x[!is.na(x)])),'NA'))
}

## Checking row percentages:
# Load function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# 2-Way Cross Tabulation
library(gmodels)
#CrossTable(dp3$emp1, dp3$SA1)
###################################################################################################



###################################################################################################
### Set the working directory

# setwd("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/TBIMS data")



### Reading the form-1 TBIMS data

form1 <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/TBIMS data from Box/02_18_2021/TBIMSForm1_20210118_SAS/TBIMS_Form1_01_18_2021.csv", header = T)
#head(form1)
dim(form1)
# [1] 18438   355
# Previous data had dim: 18320,   356
# Previous data had dim: 17316,   296


# # Checking if form1 has all UID
# # edit(form1)
# check_for_missing_UID <- subset(form1, UID=="") 
# dim(check_for_missing_UID)
# # [1] 139 296

# Checking if form1 has all SystemSubjectID
# edit(form1)
# check_for_missing_SystemSubjectID <- subset(form1, is.na(SystemSubjectID)) 
# dim(check_for_missing_SystemSubjectID)
# # [1] 3 356

# # Checking if form1 has either of UID or SystemSubjectID
# # edit(form1)
# check_for_missing_UID_or_SystemSubjectID <- subset(form1, !(UID=="" & is.na(SubjectId))) 
# dim(check_for_missing_UID_or_SystemSubjectID)
# # [1] 17316   296



# So, form-1 does not have 3 SystemSubjectIDs and there is no UID in this data like before

###################################################################################################



###################################################################################################
### Reading the form-2 TBIMS data

form2 <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/TBIMS data from Box/02_18_2021/TBIMSForm2_20210118_SAS/TBIMS_Form2_01_18_2021.csv", header = T)
#head(form2)
dim(form2)
# [1] 69404   404
# Dim of previous data was: [1] 68245   405
# Dim of previous data was: [1] 61830   394

# Checking if form1 has all UID
# edit(form2)
# check_for_missing_UID <- subset(form2, UID=="") 
# dim(check_for_missing_UID)
# [1] 23 394
# check_for_missing_UID_yr1 <- subset(form2, UID=="" & FollowUpPeriod==1) 
# dim(check_for_missing_UID_yr1)
# [1]  14 394

# Checking if form1 has all SystemSubjectID
# edit(form2)
# check_for_missing_SystemSubjectID <- subset(form2, is.na(SystemSubjectId))  # SystemSubjectId spelling!
# dim(check_for_missing_SystemSubjectID)
# # [1] 1904   405
# edit(check_for_missing_SystemSubjectID[,c("UID", "SystemSubjectId", "FollowUpPeriod")])
## Further checking how many of these 1904 missing SystemSubjectID are corresponding to year-1
# check_for_missing_SystemSubjectID_yr1 <- subset(form2, is.na(SystemSubjectId) & FollowUpPeriod==1)  # SystemSubjectId spelling!
# dim(check_for_missing_SystemSubjectID_yr1)
# # [1] 262 405

# Checking if form1 has either of UID or SystemSubjectID
# edit(form2)
# check_for_missing_UID_or_SystemSubjectID <- subset(form2, !(UID=="" & is.na(SubjectId))) 
# dim(check_for_missing_UID_or_SystemSubjectID)
# [1] 61830   394
# missin_both <- subset(form2, UID=="" & is.na(SubjectId))
# dim(missin_both)

# So, form-2 doesn't have 23 UIDs, it doesn't have 1549 SystemSubjectIds (262 of them correspond to year-1), but it has either UID or SystemSubjectId for each case.

# form2.nomiss.UID <- subset(form2, !(UID==""))
# dim(form2.nomiss.UID)
# [1] 61807   394

# form2.nomiss.SystemSubjectID <- subset(form2, !is.na(SystemSubjectId))
# dim(form2.nomiss.SystemSubjectID)
# # [1] 66341   405

# form2.miss.UID.but.nomiss.SystemSubjectID <- subset(form2, !is.na(SystemSubjectId))
# dim(form2.miss.UID.but.nomiss.SystemSubjectID)
# [1] 60281   394



### Excluding people who didn't have SystemSubjectID

# dim(form2)
# # [1] 68245   405
# form2.V2 <- subset(form2, !is.na(SystemSubjectId))
# dim(form2.V2)
# # [1] 66341   405

###################################################################################################



# ###################################################################################################
# ### Reading seizure data created by Dom using "ERFA_Aim2_SzCleaning_4_6_20.Rmd"
# 
# SeizureVarData <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/TBIMS data/ERFA_Aim2_SeizureVar.csv", header = T)
# #head(SeizureVarData)
# dim(SeizureVarData)
# # [1] 16774     4
# ### Need to remove the last two digits from SystemSubjectID
# SeizureVarData$SystemSubjectID <- substr(SeizureVarData$SystemSubjectID, 1, nchar(SeizureVarData$SystemSubjectID)-2)
# head(SeizureVarData)
# SeizureVarData.nomiss <- subset(SeizureVarData, !is.na(SeizureVarData$Seizure_YN))
# dim(SeizureVarData.nomiss)
# # [1] 4623     4
# ### So, size of our study cohort is n=4623
# 
# # Checking if SeizureVarData.nomiss has all UID
# # edit(SeizureVarData.nomiss)
# check_for_missing_UID <- subset(SeizureVarData.nomiss, SeizureVarData.nomiss$UID=="") 
# dim(check_for_missing_UID)
# # [1] 0 4
# # No such case.
# 


### Reading the corrected seizure data created by the R code in "C:\Nabil Awan\Nabil_University of Pittsburgh\GSR\ERFA grant\Aim 1 LASSO\Code\NEW CODES FOR NEW MODELS\Data management\Exploring_data.R"

seizure_data_corrected_02_22_2021 <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 1 LASSO/Code/NEW CODES FOR NEW MODELS/Data management/seizure_data_corrected_02_22_2021.csv", header = T)
dim(seizure_data_corrected_02_22_2021)
# [1] 18186    10

table(seizure_data_corrected_02_22_2021$Year1_INC_Sz_until2020)
# > table(seizure_data_corrected_02_22_2021$Year1_INC_Sz_until2020)
# 
# 0    1 
# 5044  689
table(seizure_data_corrected_02_22_2021$Year2_INC_Sz_until2020)
# > table(seizure_data_corrected_02_22_2021$Year2_INC_Sz_until2020)
# 
# 0    1 
# 4470  694

# ###################################################################################################



###################################################################################################
### Selecting form-1 variables

# form1.select <- form1[,c("UID",
#                          "SystemSubjectID",
#                          "AGE",
#                          "Sex",
#                          "EduYears",
#                          "ResDis",
# #                         "PROBLEMUse",
#                          "GCSTot",
#                          "PTADays",
#                          "TFCDays",
#                          "FIMMOTD",
#                          "FIMCOGD")]

form1.select <- form1[,c("Mod1id",
                         "Injury", # Just for checking or restricting cohort by injury date
                         "INJYEAR", # Just for checking or restricting cohort by injury date
                         "AGE",
                         "Sex",
                         "EduYears",
                         "ResDis",
                         #                         "PROBLEMUse",
                         "GCSTot",
                         "PTADays",
                         "TFCDays",
                         "FIMMOTD",
                         "FIMCOGD")]

dim(form1.select)
# [1] 18438    12
###################################################################################################



###################################################################################################
### Selecting form-2 variables
# form2.select <- form2.V2[,c("UID",
#                          "SystemSubjectId",
#                          "FollowUpPeriod",
#                          "DRS_PIEmpF",
#                          "DRSF",
#                          "EMPLOYMENTF",
#                          "PROBLEMUseF",
# #                         "SeizInYear",
#                          grep("PHQ", names(form2), value=TRUE),
#                          grep("GAD", names(form2), value=TRUE),
#                          "SWLSTOT",
#                          "PARTOutAbout",
#                          "PARTProductivity",
#                          "PARTSocial",
#                          "TransMode",
# #                         "FIMMOTF",
# #                         "FIMCOGF"
#                          grep("FIM", names(form2), value=TRUE),
#                          "PastYearSeiz"
#                          )]

form2.select <- form2[,c("Mod1id",
                            "FollowUpPeriod",
                            "ResF",
                            "DRS_PIEmpF",
                            "DRSF",
                            "EMPLOYMENTF",
                            "PROBLEMUseF",
                            #                         "SeizInYear",
                            grep("PHQ", names(form2), value=TRUE),
                            grep("GAD", names(form2), value=TRUE),
                            "SWLSTOT",
                            "PARTOutAbout",
                            "PARTProductivity",
                            "PARTSocial",
                            "TransMode",
                            #                         "FIMMOTF",
                            #                         "FIMCOGF"
                            grep("FIM", names(form2), value=TRUE),
                            "PastYearSeiz"
)]


dim(form2.select)
# [1] 69404    59

# ### NOTE: the spelling of "SystemSubjectId" needs to be changed to "SystemSubjectID" to merge the form2.select data with form1.select data.
# names(form2.select)[names(form2.select) == "SystemSubjectId"] <- "SystemSubjectID"
# ### Deleting the last two digits from "SystemSubjectID" for form-2:
# form2.select$SystemSubjectID <- substr(form2.select$SystemSubjectID, 1, nchar(form2.select$SystemSubjectID)-2)

###################################################################################################



# Question about transmode: We have a "Not Applicable: No motorized transportation" (is this the same as "does not leave the home")
# There is another "Not Applicable: Variable not due this year" and an "Unknown"



###################################################################################################
### Recoding and cleaning form1.select

# AGE
my_table(form1.select$AGE)
# Note: the database is supposed to contain only adults
form1.select$AGE[form1.select$AGE==999 | form1.select$AGE==-6869 | form1.select$AGE==-6869 | form1.select$AGE==0 | form1.select$AGE==2] <- NA
my_table(form1.select$AGE)

# Sex (1=Female, 2=Male) -> sex.new (1=male, 0=female)
my_table(form1.select$Sex)
form1.select$Sex[form1.select$Sex==9 | form1.select$Sex==888] <- NA
my_table(form1.select$Sex)
form1.select$Sex.new <- form1.select$Sex-1
my_table(form1.select$Sex.new)

# EduYears -> EduYears.new 
my_table(form1.select$EduYears)
form1.select$EduYears.new[form1.select$EduYears<=11] <- 1
form1.select$EduYears.new[form1.select$EduYears==12] <- 2
form1.select$EduYears.new[form1.select$EduYears>=13 & form1.select$EduYears<=20] <- 3
form1.select$EduYears.new[form1.select$EduYears==66 | form1.select$EduYears==77 | form1.select$EduYears==99 | form1.select$EduYears==888] <- NA
my_table(form1.select$EduYears.new)
# Creating dummies by taking higher educated (EduYears.new==3) as the base category
form1.select$EduYears.dummy.1 <- ifelse(form1.select$EduYears.new==1, 1, 0)
my_table(form1.select$EduYears.dummy.1)
form1.select$EduYears.dummy.2 <- ifelse(form1.select$EduYears.new==2, 1, 0)
my_table(form1.select$EduYears.dummy.2)

# Unplanned rehab transfer
my_table(form1.select$ResDis)
# ResDis -> ResDis.new (link for codes: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=1&ID=RES)
my_table(form1.select$ResDis)
form1.select$ResDis.new[form1.select$ResDis==1 | form1.select$ResDis==5] <- 1 # Private Home
form1.select$ResDis.new[form1.select$ResDis==2 | form1.select$ResDis==7 | form1.select$ResDis==8 | form1.select$ResDis==9] <- 2 # Healthcare setting
form1.select$ResDis.new[form1.select$ResDis==3 | form1.select$ResDis==4 | form1.select$ResDis==6 | form1.select$ResDis==77] <- 3 # Other
form1.select$ResDis.new[form1.select$ResDis==88 | form1.select$ResDis==99 | form1.select$ResDis==77 | form1.select$ResDis==15627 | form1.select$ResDis==75228] <- NA 
my_table(form1.select$ResDis.new)
# Creating dummies by taking other residence (ResDis.new==3) as the base category
form1.select$ResDis.dummy.1 <- ifelse(form1.select$ResDis.new==1, 1, 0)
my_table(form1.select$ResDis.dummy.1)
form1.select$ResDis.dummy.2 <- ifelse(form1.select$ResDis.new==2, 1, 0)
my_table(form1.select$ResDis.dummy.2)

# GCSTot
my_table(form1.select$GCSTot)
form1.select$GCSTot[form1.select$GCSTot==77 | form1.select$GCSTot==88 | form1.select$GCSTot==99 | form1.select$GCSTot==20 | form1.select$GCSTot==888] <- NA
my_table(form1.select$GCSTot)

# PTADays (https://www.tbindsc.org/SyllabusDetail.aspx?MOD=1&ID=8802)
my_table(form1.select$PTADays)
form1.select$PTADays[form1.select$PTADays==999 | form1.select$PTADays==888] <- NA
my_table(form1.select$PTADays)
# send form1.select$PTADays==888 to the highest category; according to the code for DoD injury severity, we can keep it as 888, won't affect our analysis

# TFCDays (https://www.tbindsc.org/SyllabusDetail.aspx?MOD=1&ID=8802)
my_table(form1.select$TFCDays)
form1.select$TFCDays[form1.select$TFCDays==999 | form1.select$TFCDays==777] <- NA
my_table(form1.select$TFCDays)
# send form1.select$TFCDays==777 to the highest category; according to the code for DoD injury severity, we can keep it as 777, won't affect our analysis

# DOD_Injury_severity (newly created variable to indicate injury severity based on Anne Ritter's papers)
form1.select$DOD_Injury_severity <- ifelse(is.na(form1.select$GCSTot)==T & is.na(form1.select$TFCDays)==T & is.na(form1.select$PTADays)==T, NA, 
                                           ifelse( (form1.select$GCSTot>=3 & form1.select$GCSTot<=8) | form1.select$TFCDays>1 | form1.select$PTADays>=8 , 1, 0))
my_table(form1.select$DOD_Injury_severity)

# FIMMOTD
# Try: 13-25, 26-38, 39-51, ... (like this 7 scales)
my_table(form1.select$FIMMOTD)
# Note: The highest value can be 91
form1.select$FIMMOTD[form1.select$FIMMOTD==999 | form1.select$FIMMOTD==97] <- NA
my_table(form1.select$FIMMOTD)
hist(form1.select$FIMMOTD)  # left skewed
# Creating the Rasch FIM as suggested by Dr. Wagner and Shannon during R21 work (Anne Ritter's papers)
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==13] <- 0
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==14] <- 10
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==15] <- 16
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==16] <- 20
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==17] <- 22
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==18] <- 24
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==19] <- 26
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==20] <- 27
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==21] <- 28
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==22] <- 29
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==23] <- 30
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==24] <- 31
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==25] <- 32
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==26] <- 33
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==27] <- 34
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==28 | form1.select$FIMMOTD==29] <- 35
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==30] <- 36
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==31 | form1.select$FIMMOTD==32] <- 37
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==33 | form1.select$FIMMOTD==34] <- 38
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==35] <- 39
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==36 | form1.select$FIMMOTD==37] <- 40
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==38 | form1.select$FIMMOTD==39] <- 41
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==40 | form1.select$FIMMOTD==41] <- 42
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==42 | form1.select$FIMMOTD==43] <- 43
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==44 | form1.select$FIMMOTD==45] <- 44
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==46 | form1.select$FIMMOTD==47] <- 45
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==48 | form1.select$FIMMOTD==49] <- 46
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==50 | form1.select$FIMMOTD==51] <- 47
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==52 | form1.select$FIMMOTD==53] <- 48
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==54] <- 49
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==55 | form1.select$FIMMOTD==56] <- 50
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==57 | form1.select$FIMMOTD==58] <- 51
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==59 | form1.select$FIMMOTD==60] <- 52
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==61 | form1.select$FIMMOTD==62] <- 53
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==63 | form1.select$FIMMOTD==64] <- 54
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==65] <- 55
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==66 | form1.select$FIMMOTD==67] <- 56
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==68] <- 57
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==69 | form1.select$FIMMOTD==70] <- 58
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==71] <- 59
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==72 | form1.select$FIMMOTD==73] <- 60
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==74] <- 61
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==75] <- 62
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==76 | form1.select$FIMMOTD==77] <- 63
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==78 | form1.select$FIMMOTD==79] <- 64
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==80] <- 66
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==81] <- 67
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==82] <- 68
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==83] <- 70
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==84] <- 71
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==85] <- 73
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==86] <- 74
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==87] <- 77
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==88] <- 79
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==89] <- 83
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==90] <- 90
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==91] <- 100
form1.select$FIMMOTD.Rasch[form1.select$FIMMOTD==999] <- NA
hist(form1.select$FIMMOTD.Rasch) # roughly normal now

# FIMCOGD
# Try: 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35
my_table(form1.select$FIMCOGD)
form1.select$FIMCOGD[form1.select$FIMCOGD==999 | form1.select$FIMCOGD==77] <- NA
my_table(form1.select$FIMCOGD)
hist(form1.select$FIMCOGD) # left skewed
# Creating the Rasch FIM as suggested by Dr. Wagner and Shannon during R21 work (Anne Ritter's papers)
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==5] <- 0
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==6] <- 10
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==7] <- 18
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==8] <- 24
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==9] <- 28
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==10] <- 31
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==11] <- 34
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==12] <- 36
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==13] <- 38
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==14] <- 41
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==15] <- 42
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==16] <- 44
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==17] <- 46
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==18] <- 48
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==19] <- 49
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==20] <- 51
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==21] <- 52
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==22] <- 54
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==23] <- 56
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==24] <- 57
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==25] <- 59
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==26] <- 61
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==27] <- 63
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==28] <- 65
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==29] <- 67
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==30] <- 70
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==31] <- 73
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==32] <- 77
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==33] <- 82
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==34] <- 90
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==35] <- 100
form1.select$FIMCOGD.Rasch[form1.select$FIMCOGD==999 | form1.select$FIMCOGD.Rasch==77] <- NA
hist(form1.select$FIMCOGD.Rasch) # roughly normal now

###################################################################################################



###################################################################################################
### Recoding, cleaning, and reshaping form2.select

# Restricting data to include only first two years
dim(form2.select)
# [1] 66341    59
form2.select <- subset(form2.select, FollowUpPeriod==1 | FollowUpPeriod==2)
dim(form2.select)
# [1] 33739    59

# anyDuplicated(form2.select[-1])

# ResF -> ResF.new (link for codes: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=RES)
my_table(form2.select$ResF)
form2.select$ResF.new[form2.select$ResF==1 | form2.select$ResF==5] <- 1 # Private Home
form2.select$ResF.new[form2.select$ResF==2 | form2.select$ResF==7 | form2.select$ResF==8 | form2.select$ResF==9] <- 2 # Healthcare setting
form2.select$ResF.new[form2.select$ResF==3 | form2.select$ResF==4 | form2.select$ResF==6 | form2.select$ResF==77] <- 3 # Other
form2.select$ResF.new[form2.select$ResF==88 | form2.select$ResF==99 | form2.select$ResF==77 | form2.select$ResF==15627 | form2.select$ResF==75228] <- NA 
# my_table(form2.select$ResF.new)
my_table(form2.select$ResF.new[form2.select$FollowUpPeriod==1])
my_table(form2.select$ResF.new[form2.select$FollowUpPeriod==2])
# Creating dummies by taking other residence (ResF.new==3) as the base category
form2.select$ResF.dummy.1 <- ifelse(form2.select$ResF.new==1, 1, 0)
my_table(form2.select$ResF.dummy.1)
form2.select$ResF.dummy.2 <- ifelse(form2.select$ResF.new==2, 1, 0)
# my_table(form2.select$ResF.dummy.2)
my_table(form2.select$ResF.dummy.1[form2.select$FollowUpPeriod==1])
my_table(form2.select$ResF.dummy.1[form2.select$FollowUpPeriod==2])
my_table(form2.select$ResF.dummy.2[form2.select$FollowUpPeriod==1])
my_table(form2.select$ResF.dummy.2[form2.select$FollowUpPeriod==2])


## EMPLOYMENTF (link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=9903 and https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=EMP)
my_table(form2.select$EMPLOYMENTF[form2.select$FollowUpPeriod==1])
my_table(form2.select$EMPLOYMENTF[form2.select$FollowUpPeriod==2])
form2.select$EMPLOYMENTF.new[form2.select$EMPLOYMENTF==88 | form2.select$EMPLOYMENTF==99 | form2.select$EMPLOYMENTF==77] <- NA
form2.select$EMPLOYMENTF.new[form2.select$EMPLOYMENTF==5 | form2.select$EMPLOYMENTF==8 | form2.select$EMPLOYMENTF==2 | form2.select$EMPLOYMENTF==3 | form2.select$EMPLOYMENTF==4] <- 1
form2.select$EMPLOYMENTF.new[form2.select$EMPLOYMENTF==7 | form2.select$EMPLOYMENTF==9 | form2.select$EMPLOYMENTF==10 | form2.select$EMPLOYMENTF==11 | form2.select$EMPLOYMENTF==77] <- 0
my_table(form2.select$EMPLOYMENTF.new[form2.select$FollowUpPeriod==1])
my_table(form2.select$EMPLOYMENTF.new[form2.select$FollowUpPeriod==2])

### Use DRS_PIEmpF (DRS employability) instead of EMPLOYMENTF
# Link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=9912
# Link: http://tbims.org/combi/drs/drssyl.html
table(form2.select$DRS_PIEmpF[form2.select$FollowUpPeriod==1])
my_table(form2.select$DRS_PIEmpF[form2.select$FollowUpPeriod==1])
levels(form2.select$DRS_PIEmpF[form2.select$FollowUpPeriod==1])
table(form2.select$DRS_PIEmpF[form2.select$FollowUpPeriod==2])
my_table(form2.select$DRS_PIEmpF[form2.select$FollowUpPeriod==2])
levels(form2.select$DRS_PIEmpF[form2.select$FollowUpPeriod==2])

form2.select$DRS_PIEmpF_bin <- NULL
form2.select$DRS_PIEmpF_bin[form2.select$DRS_PIEmpF=="0" | form2.select$DRS_PIEmpF=="1"] <- 1
form2.select$DRS_PIEmpF_bin[form2.select$DRS_PIEmpF=="2" | form2.select$DRS_PIEmpF=="3"] <- 0
my_table(form2.select$DRS_PIEmpF_bin[form2.select$FollowUpPeriod==1])
my_table(form2.select$DRS_PIEmpF_bin[form2.select$FollowUpPeriod==2])

### DRSF (to know the missingness)
# Note: Should range from 0-29
my_table(form2.select$DRSF[form2.select$FollowUpPeriod==1])
my_table(form2.select$DRSF[form2.select$FollowUpPeriod==2])
# Should I recode 30 as missing or as 29? Note: the DRS range is 0-29
form2.select$DRSF[form2.select$DRSF==99 | form2.select$DRSF==30] <- NA
my_table(form2.select$DRSF[form2.select$FollowUpPeriod==1])
my_table(form2.select$DRSF[form2.select$FollowUpPeriod==2])


# PROBLEMUse (1=No, 2=Yes) (link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=9908) -> PROBLEMUse.new (0=No, 1=Yes)
my_table(form2.select$PROBLEMUse)
form2.select$PROBLEMUse[form2.select$PROBLEMUse==7 | form2.select$PROBLEMUse==9 | form2.select$PROBLEMUse==99] <- NA
my_table(form2.select$PROBLEMUse)
form2.select$PROBLEMUse.new <- form2.select$PROBLEMUse-1
my_table(form2.select$PROBLEMUse.new)
my_table(form2.select$PROBLEMUse.new[form2.select$FollowUpPeriod==1])
my_table(form2.select$PROBLEMUse.new[form2.select$FollowUpPeriod==2])

# PastYearSeiz -> PastYearSeiz.new (link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=FUSEIZ)
my_table(form2.select$PastYearSeiz)
form2.select$PastYearSeiz.new <- form2.select$PastYearSeiz
form2.select$PastYearSeiz.new[form2.select$PastYearSeiz==6 | form2.select$PastYearSeiz==9 | form2.select$PastYearSeiz==88] <- NA
form2.select$PastYearSeiz.new[form2.select$PastYearSeiz==8] <- 0
my_table(form2.select$PastYearSeiz.new)
### We will not use this variable, we will use that Dom sent: 
### "C:\Nabil Awan\Nabil_University of Pittsburgh\GSR\ERFA grant\Aim 2 SEM\TBIMS data\ERFA_Aim2_SeizureVar.csv"
### SeizureVarData <- read.csv("ERFA_Aim2_SeizureVar.csv", header = T) above
my_table(form2.select$PastYearSeiz.new[form2.select$FollowUpPeriod==1])


# PHQPleasure (link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=PHQ)
my_table(form2.select$PHQPleasure)
form2.select$PHQPleasure[form2.select$PHQPleasure==6 | form2.select$PHQPleasure==9 | form2.select$PHQPleasure==10] <- NA
my_table(form2.select$PHQPleasure)
form2.select$PHQPleasure.binary[form2.select$PHQPleasure==1 | form2.select$PHQPleasure==2 | form2.select$PHQPleasure==3] <- 1
form2.select$PHQPleasure.binary[form2.select$PHQPleasure==0] <- 0
my_table(form2.select$PHQPleasure.binary)

# PHQDown
my_table(form2.select$PHQDown)
form2.select$PHQDown[form2.select$PHQDown==6 | form2.select$PHQDown==9 | form2.select$PHQDown==10] <- NA
my_table(form2.select$PHQDown)
form2.select$PHQDown.binary[form2.select$PHQDown==2 | form2.select$PHQDown==3] <- 1
form2.select$PHQDown.binary[form2.select$PHQDown==0 | form2.select$PHQDown==1] <- 0
my_table(form2.select$PHQDown.binary)

# PHQSleep
my_table(form2.select$PHQSleep)
form2.select$PHQSleep[form2.select$PHQSleep==6 | form2.select$PHQSleep==9 | form2.select$PHQSleep==10] <- NA
my_table(form2.select$PHQSleep)
form2.select$PHQSleep.binary[form2.select$PHQSleep==2 | form2.select$PHQSleep==3] <- 1
form2.select$PHQSleep.binary[form2.select$PHQSleep==0 | form2.select$PHQSleep==1] <- 0
my_table(form2.select$PHQSleep.binary)

# PHQTired
my_table(form2.select$PHQTired)
form2.select$PHQTired[form2.select$PHQTired==6 | form2.select$PHQTired==9 | form2.select$PHQTired==10] <- NA
my_table(form2.select$PHQTired)
form2.select$PHQTired.binary[form2.select$PHQTired==2 | form2.select$PHQTired==3] <- 1
form2.select$PHQTired.binary[form2.select$PHQTired==0 | form2.select$PHQTired==1] <- 0
my_table(form2.select$PHQTired.binary)

# PHQEAt
my_table(form2.select$PHQEAt)
form2.select$PHQEAt[form2.select$PHQEAt==6 | form2.select$PHQEAt==9 | form2.select$PHQEAt==10] <- NA
my_table(form2.select$PHQEAt)
form2.select$PHQEAt.binary[form2.select$PHQEAt==2 | form2.select$PHQEAt==3] <- 1
form2.select$PHQEAt.binary[form2.select$PHQEAt==0 | form2.select$PHQEAt==1] <- 0
my_table(form2.select$PHQEAt.binary)

# PHQBad
my_table(form2.select$PHQBad)
form2.select$PHQBad[form2.select$PHQBad==6 | form2.select$PHQBad==9 | form2.select$PHQBad==10] <- NA
my_table(form2.select$PHQBad)
form2.select$PHQBad.binary[form2.select$PHQBad==2 | form2.select$PHQBad==3] <- 1
form2.select$PHQBad.binary[form2.select$PHQBad==0 | form2.select$PHQBad==1] <- 0
my_table(form2.select$PHQBad.binary)

# PHQConcentrate
my_table(form2.select$PHQConcentrate)
form2.select$PHQConcentrate[form2.select$PHQConcentrate==6 | form2.select$PHQConcentrate==9 | form2.select$PHQConcentrate==10] <- NA
my_table(form2.select$PHQConcentrate)
form2.select$PHQConcentrate.binary[form2.select$PHQConcentrate==2 | form2.select$PHQConcentrate==3] <- 1
form2.select$PHQConcentrate.binary[form2.select$PHQConcentrate==0 | form2.select$PHQConcentrate==1] <- 0
my_table(form2.select$PHQConcentrate.binary)

# PHQSlow
my_table(form2.select$PHQSlow)
form2.select$PHQSlow[form2.select$PHQSlow==6 | form2.select$PHQSlow==9 | form2.select$PHQSlow==10] <- NA
my_table(form2.select$PHQSlow)
form2.select$PHQSlow.binary[form2.select$PHQSlow==2 | form2.select$PHQSlow==3] <- 1
form2.select$PHQSlow.binary[form2.select$PHQSlow==0 | form2.select$PHQSlow==1] <- 0
my_table(form2.select$PHQSlow.binary)

# PHQDead
my_table(form2.select$PHQDead)
form2.select$PHQDead[form2.select$PHQDead==6 | form2.select$PHQDead==9 | form2.select$PHQDead==10] <- NA
my_table(form2.select$PHQDead)
# Only exception in Fann's criteria is PHQDead (it can be 'several days' to be considered symptomatic)
form2.select$PHQDead.binary[form2.select$PHQDead==1 | form2.select$PHQDead==2 | form2.select$PHQDead==3] <- 1
form2.select$PHQDead.binary[form2.select$PHQDead==0] <- 0
my_table(form2.select$PHQDead.binary)

# PHQDifficult
# my_table(form2.select$PHQDifficult)
# form2.select$PHQDifficult[form2.select$PHQDifficult==6 | form2.select$PHQDifficult==9 | form2.select$PHQDifficult==10] <- NA
# my_table(form2.select$PHQDifficult)

# PHQ9TOT
my_table(form2.select$PHQ9TOT)
form2.select$PHQ9TOT[form2.select$PHQ9TOT==88 | form2.select$PHQ9TOT==99] <- NA
my_table(form2.select$PHQ9TOT)
# Checking the distribution of PHQ9TOT
hist(form2.select$PHQ9TOT)
# Right skewed

### depression_fann (created by Fann's criteria): 
# Criteria: Patients were considered to have a PTD based on at least 5 symptom endorsed "more than half the days" (suicidal ideation could be "several days"), with at least one being a "cardinal symptom," i.e., either 1) anhedonia or 2) depressed mood

# Count number of symptoms per person
form2.select$PHQcount <- rowSums(cbind(form2.select$PHQPleasure.binary, form2.select$PHQDown.binary, form2.select$PHQSleep.binary, form2.select$PHQTired.binary, form2.select$PHQEAt.binary, form2.select$PHQBad.binary, form2.select$PHQConcentrate.binary, form2.select$PHQSlow.binary, form2.select$PHQDead.binary), na.rm=T)
form2.select$PHQcount[is.na(form2.select$PHQPleasure.binary) & is.na(form2.select$PHQDown.binary) & is.na(form2.select$PHQSleep.binary) & is.na(form2.select$PHQTired.binary) & is.na(form2.select$PHQEAt.binary) & is.na(form2.select$PHQBad.binary) & is.na(form2.select$PHQConcentrate.binary) & is.na(form2.select$PHQSlow.binary) & is.na(form2.select$PHQDead.binary)] <- NA
my_table(form2.select$PHQcount[form2.select$FollowUpPeriod==1])
my_table(form2.select$PHQcount[form2.select$FollowUpPeriod==2])

form2.select$depression_fann <- ifelse((form2.select$PHQcount>=5 & (form2.select$PHQPleasure.binary==1 | form2.select$PHQDown.binary==1)), 1, 0)
#form2.select$depression_fann <- ifelse((form2.select$PHQcount>=5 & form2.select$PHQPleasure.binary==1) | (form2.select$PHQcount>=5 & form2.select$PHQDown.binary==1) | (form2.select$PHQcount>=5 & form2.select$PHQPleasure.binary==1 & form2.select$PHQDown.binary==1), 1, 0)
my_table(form2.select$depression_fann[form2.select$FollowUpPeriod==1])
my_table(form2.select$depression_fann[form2.select$FollowUpPeriod==2])


# GADNervous (link: https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=GAD)
my_table(form2.select$GADNervous)
form2.select$GADNervous[form2.select$GADNervous==6 | form2.select$GADNervous==9 | form2.select$GADNervous==10] <- NA
my_table(form2.select$GADNervous)

# GADCntrlWry
my_table(form2.select$GADCntrlWry)
form2.select$GADCntrlWry[form2.select$GADCntrlWry==6 | form2.select$GADCntrlWry==9 | form2.select$GADCntrlWry==10] <- NA
my_table(form2.select$GADCntrlWry)

# GADWorry
my_table(form2.select$GADWorry)
form2.select$GADWorry[form2.select$GADWorry==6 | form2.select$GADWorry==9 | form2.select$GADWorry==10 | form2.select$GADWorry==8] <- NA
my_table(form2.select$GADWorry)

# GADRelax
my_table(form2.select$GADRelax)
form2.select$GADRelax[form2.select$GADRelax==6 | form2.select$GADRelax==9 | form2.select$GADRelax==10] <- NA
my_table(form2.select$GADRelax)

# GADRestless
my_table(form2.select$GADRestless)
form2.select$GADRestless[form2.select$GADRestless==6 | form2.select$GADRestless==9 | form2.select$GADRestless==10] <- NA
my_table(form2.select$GADRestless)

# GADAnnoy
my_table(form2.select$GADAnnoy)
form2.select$GADAnnoy[form2.select$GADAnnoy==6 | form2.select$GADAnnoy==9 | form2.select$GADAnnoy==10] <- NA
my_table(form2.select$GADAnnoy)

# GADAfraid
my_table(form2.select$GADAfraid)
form2.select$GADAfraid[form2.select$GADAfraid==6 | form2.select$GADAfraid==9 | form2.select$GADAfraid==10 | form2.select$GADAfraid==8] <- NA
my_table(form2.select$GADAfraid)

# # GADDifficult
# my_table(form2.select$GADDifficult)
# form2.select$GADDifficult[form2.select$GADDifficult==6 | form2.select$GADDifficult==9 | form2.select$GADDifficult==10] <- NA
# my_table(form2.select$GADDifficult)

# GAD7TOT
my_table(form2.select$GAD7TOT)
form2.select$GAD7TOT[form2.select$GAD7TOT==88 | form2.select$GAD7TOT==99] <- NA
my_table(form2.select$GAD7TOT)
# Cheking the distribution of GAD7TOT
hist(form2.select$GAD7TOT)
# Right skewed
## Categorizing GAD according to the following links given by Dr. Wagner
# https://med.dartmouth-hitchcock.org/documents/GAD-7-anxiety-screen.pdf
# https://adaa.org/sites/default/files/GAD-7_Anxiety-updated_0.pdf
# https://www.thecalculator.co/health/Generalized-Anxiety-Disorder-GAD-7-Calculator-802.html#here
# 0-4: minimal anxiety
# 5-9: mild anxiety
# 10-14: moderate anxiety
# 15-21: severe anxiety 
# Or you can use 10 as the cut point to binarize
form2.select$GAD7TOT.new[form2.select$GAD7TOT>=0 & form2.select$GAD7TOT<=4] <- 1
form2.select$GAD7TOT.new[form2.select$GAD7TOT>=5 & form2.select$GAD7TOT<=9] <- 2
form2.select$GAD7TOT.new[form2.select$GAD7TOT>=10 & form2.select$GAD7TOT<=14] <- 3
form2.select$GAD7TOT.new[form2.select$GAD7TOT>=15 & form2.select$GAD7TOT<=21] <- 4
my_table(form2.select$GAD7TOT[form2.select$FollowUpPeriod==1])
my_table(form2.select$GAD7TOT.new[form2.select$FollowUpPeriod==1])
my_table(form2.select$GAD7TOT[form2.select$FollowUpPeriod==2])
my_table(form2.select$GAD7TOT.new[form2.select$FollowUpPeriod==2])

# SWLSTOT
my_table(form2.select$SWLSTOT)
form2.select$SWLSTOT[form2.select$SWLSTOT==66 | form2.select$SWLSTOT==88 | form2.select$SWLSTOT==99] <- NA
my_table(form2.select$SWLSTOT)
# Distribution of SWLS
hist(form2.select$SWLSTOT[form2.select$FollowUpPeriod==1])
hist(form2.select$SWLSTOT[form2.select$FollowUpPeriod==2])
## Making SWLS ordinal following the above links provided by Dr. Wagner:
# https://www.midss.org/sites/default/files/understanding_swls_scores.pdf
# http://labs.psychology.illinois.edu/~ediener/Documents/Understanding%20SWLS%20Scores.pdf
# https://positivepsychology.com/life-satisfaction-scales/
form2.select$SWLSTOT.new[form2.select$SWLSTOT>=5 & form2.select$SWLSTOT<=9] <- 1 # Extremely Dissatisfied
form2.select$SWLSTOT.new[form2.select$SWLSTOT>=10 & form2.select$SWLSTOT<=14] <- 2 # Dissatisfied
form2.select$SWLSTOT.new[form2.select$SWLSTOT>=15 & form2.select$SWLSTOT<=19] <- 3 # Slightly below average in life satisfaction
form2.select$SWLSTOT.new[form2.select$SWLSTOT>=20 & form2.select$SWLSTOT<=24] <- 4 # Average score
form2.select$SWLSTOT.new[form2.select$SWLSTOT>=25 & form2.select$SWLSTOT<=29] <- 5 # High score
form2.select$SWLSTOT.new[form2.select$SWLSTOT>=30 & form2.select$SWLSTOT<=35] <- 6 # Very high score; highly satisfied
my_table(form2.select$SWLSTOT[form2.select$FollowUpPeriod==1])
my_table(form2.select$SWLSTOT.new[form2.select$FollowUpPeriod==1])
my_table(form2.select$SWLSTOT[form2.select$FollowUpPeriod==2])
my_table(form2.select$SWLSTOT.new[form2.select$FollowUpPeriod==2])

# PARTOutAbout
my_table(form2.select$PARTOutAbout)

# PARTProductivity
my_table(form2.select$PARTProductivity)

# PARTSocial
my_table(form2.select$PARTSocial)

# PARTSummary (average of 3 PART variables above)
form2.select$PARTSummary <- (form2.select$PARTOutAbout + form2.select$PARTProductivity + form2.select$PARTSocial)/3
summary(form2.select$PARTSummary)
# Distribution of PARTSummary
hist(form2.select$PARTSummary[form2.select$FollowUpPeriod==1])
hist(form2.select$PARTSummary[form2.select$FollowUpPeriod==2])

# TransMode -> TransMode.new (1=1 - Driving Independently, 2=2,4 - Specialized/Personal transportation, 3=3 - Takes public transit, 4=8 - No motorized transportation)
# Try: 1, 2=public transport, 3=specialized/personal/no motorized vehicle 
my_table(form2.select$TransMode)
form2.select$TransMode.new[form2.select$TransMode==1] <- 1
form2.select$TransMode.new[form2.select$TransMode==2 | form2.select$TransMode==4] <- 2
form2.select$TransMode.new[form2.select$TransMode==3] <- 3
form2.select$TransMode.new[form2.select$TransMode==8] <- 4
form2.select$TransMode.new[form2.select$TransMode==7 | form2.select$TransMode==9 | form2.select$TransMode==88] <- NA
my_table(form2.select$TransMode.new)
# Creating binary driving status (TransMode.binary=1: drives independently; TransMode.binary=1: does not drive independently)
form2.select$TransMode.binary[form2.select$TransMode.new==1] <- 1
form2.select$TransMode.binary[form2.select$TransMode.new==2 | form2.select$TransMode.new==3 | form2.select$TransMode.new==4] <- 0
my_table(form2.select$TransMode.binary)

# FIMMOTF
my_table(form2.select$FIMMOTF)
form2.select$FIMMOTF[form2.select$FIMMOTF<13 | form2.select$FIMMOTF>91] <- NA
my_table(form2.select$FIMMOTF)
# Distribution of FIMMOTF
hist(form2.select$FIMMOTF[form2.select$FollowUpPeriod==1])
hist(form2.select$FIMMOTF[form2.select$FollowUpPeriod==2])
# Both left skewed
# Creating the Rasch FIM as suggested by Dr. Wagner and Shannon during R21 work (Anne Ritter's papers)
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==13] <- 0
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==14] <- 10
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==15] <- 16
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==16] <- 20
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==17] <- 22
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==18] <- 24
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==19] <- 26
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==20] <- 27
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==21] <- 28
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==22] <- 29
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==23] <- 30
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==24] <- 31
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==25] <- 32
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==26] <- 33
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==27] <- 34
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==28 | form2.select$FIMMOTF==29] <- 35
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==30] <- 36
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==31 | form2.select$FIMMOTF==32] <- 37
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==33 | form2.select$FIMMOTF==34] <- 38
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==35] <- 39
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==36 | form2.select$FIMMOTF==37] <- 40
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==38 | form2.select$FIMMOTF==39] <- 41
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==40 | form2.select$FIMMOTF==41] <- 42
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==42 | form2.select$FIMMOTF==43] <- 43
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==44 | form2.select$FIMMOTF==45] <- 44
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==46 | form2.select$FIMMOTF==47] <- 45
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==48 | form2.select$FIMMOTF==49] <- 46
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==50 | form2.select$FIMMOTF==51] <- 47
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==52 | form2.select$FIMMOTF==53] <- 48
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==54] <- 49
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==55 | form2.select$FIMMOTF==56] <- 50
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==57 | form2.select$FIMMOTF==58] <- 51
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==59 | form2.select$FIMMOTF==60] <- 52
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==61 | form2.select$FIMMOTF==62] <- 53
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==63 | form2.select$FIMMOTF==64] <- 54
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==65] <- 55
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==66 | form2.select$FIMMOTF==67] <- 56
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==68] <- 57
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==69 | form2.select$FIMMOTF==70] <- 58
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==71] <- 59
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==72 | form2.select$FIMMOTF==73] <- 60
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==74] <- 61
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==75] <- 62
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==76 | form2.select$FIMMOTF==77] <- 63
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==78 | form2.select$FIMMOTF==79] <- 64
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==80] <- 66
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==81] <- 67
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==82] <- 68
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==83] <- 70
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==84] <- 71
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==85] <- 73
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==86] <- 74
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==87] <- 77
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==88] <- 79
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==89] <- 83
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==90] <- 90
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==91] <- 100
form2.select$FIMMOTF.Rasch[form2.select$FIMMOTF==999] <- NA
my_table(form2.select$FIMMOTF[form2.select$FollowUpPeriod==1])
my_table(form2.select$FIMMOTF.Rasch[form2.select$FollowUpPeriod==1])
my_table(form2.select$FIMMOTF[form2.select$FollowUpPeriod==2])
my_table(form2.select$FIMMOTF.Rasch[form2.select$FollowUpPeriod==2])
hist(form2.select$FIMMOTF.Rasch[form2.select$FollowUpPeriod==1]) # Still left skewed but the residuals will hopefully be less skewed
hist(form2.select$FIMMOTF.Rasch[form2.select$FollowUpPeriod==2]) # Still left skewed but the residuals will hopefully be less skewed

# FIMCOGF
my_table(form2.select$FIMCOGF)
form2.select$FIMCOGF[form2.select$FIMCOGF<5 | form2.select$FIMCOGF>35] <- NA
my_table(form2.select$FIMCOGF)
# Distribution of FIMCOGF
hist(form2.select$FIMCOGF[form2.select$FollowUpPeriod==1]) # left skewed
hist(form2.select$FIMCOGF[form2.select$FollowUpPeriod==2]) # left skewed
# Creating the Rasch FIM as suggested by Dr. Wagner and Shannon during R21 work (Anne Ritter's papers)
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==5] <- 0
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==6] <- 10
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==7] <- 18
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==8] <- 24
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==9] <- 28
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==10] <- 31
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==11] <- 34
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==12] <- 36
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==13] <- 38
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==14] <- 41
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==15] <- 42
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==16] <- 44
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==17] <- 46
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==18] <- 48
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==19] <- 49
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==20] <- 51
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==21] <- 52
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==22] <- 54
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==23] <- 56
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==24] <- 57
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==25] <- 59
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==26] <- 61
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==27] <- 63
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==28] <- 65
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==29] <- 67
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==30] <- 70
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==31] <- 73
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==32] <- 77
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==33] <- 82
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==34] <- 90
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==35] <- 100
form2.select$FIMCOGF.Rasch[form2.select$FIMCOGF==999] <- NA
my_table(form2.select$FIMCOGF[form2.select$FollowUpPeriod==1])
my_table(form2.select$FIMCOGF.Rasch[form2.select$FollowUpPeriod==1])
my_table(form2.select$FIMCOGF[form2.select$FollowUpPeriod==2])
my_table(form2.select$FIMCOGF.Rasch[form2.select$FollowUpPeriod==2])
hist(form2.select$FIMCOGF.Rasch[form2.select$FollowUpPeriod==1]) # Still left skewed but the residuals will hopefully be less skewed
hist(form2.select$FIMCOGF.Rasch[form2.select$FollowUpPeriod==2]) # # Still left skewed but the residuals will hopefully be less skewed

# Trying log transformation on Rasch
hist(log(form2.select$FIMCOGF.Rasch[form2.select$FollowUpPeriod==1])) # roughly normal now (at the right end)
hist(log(form2.select$FIMCOGF.Rasch[form2.select$FollowUpPeriod==2])) # roughly normal now (at the right end)

# However, I may still prefer to use Rasch adjusted FIM and not its further log

## FIM categories: https://www.healthline.com/health/fim-scores#categories

### Bringing in all FIM variables and 
### taking more of an adjudication approach and 
### look at individual items and 
### categorize people with ALL items at 6 or 7 as independent

### This article used quintiles: http://europepmc.org/article/PMC/4516906


### MOT components/items

my_table(form2.select$FIMFeedF)
form2.select$FIMFeedF[form2.select$FIMFeedF==8 | form2.select$FIMFeedF==9] <- NA
my_table(form2.select$FIMFeedF)
form2.select$FIMFeedF.binary <- ifelse(form2.select$FIMFeedF==6 | form2.select$FIMFeedF==7, 1, 0)
my_table(form2.select$FIMFeedF.binary)
form2.select$FIMFeedF.binary.7 <- ifelse(form2.select$FIMFeedF==7, 1, 0)
my_table(form2.select$FIMFeedF.binary.7)

my_table(form2.select$FIMGroomF)
form2.select$FIMGroomF[form2.select$FIMGroomF==8 | form2.select$FIMGroomF==9] <- NA
my_table(form2.select$FIMGroomF)
form2.select$FIMGroomF.binary <- ifelse(form2.select$FIMGroomF==6 | form2.select$FIMGroomF==7, 1, 0)
my_table(form2.select$FIMGroomF.binary)
form2.select$FIMGroomF.binary.7 <- ifelse(form2.select$FIMGroomF==7, 1, 0)
my_table(form2.select$FIMGroomF.binary.7)

my_table(form2.select$FIMBathF)
form2.select$FIMBathF[form2.select$FIMBathF==8 | form2.select$FIMBathF==9] <- NA
my_table(form2.select$FIMBathF)
form2.select$FIMBathF.binary <- ifelse(form2.select$FIMBathF==6 | form2.select$FIMBathF==7, 1, 0)
my_table(form2.select$FIMBathF.binary)
form2.select$FIMBathF.binary.7 <- ifelse(form2.select$FIMBathF==7, 1, 0)
my_table(form2.select$FIMBathF.binary.7)

my_table(form2.select$FIMDrupF)
form2.select$FIMDrupF[form2.select$FIMDrupF==8 | form2.select$FIMDrupF==9] <- NA
my_table(form2.select$FIMDrupF)
form2.select$FIMDrupF.binary <- ifelse(form2.select$FIMDrupF==6 | form2.select$FIMDrupF==7, 1, 0)
my_table(form2.select$FIMDrupF.binary)
form2.select$FIMDrupF.binary.7 <- ifelse(form2.select$FIMDrupF==7, 1, 0)
my_table(form2.select$FIMDrupF.binary.7)

my_table(form2.select$FIMDrsdwnF)
form2.select$FIMDrsdwnF[form2.select$FIMDrsdwnF==8 | form2.select$FIMDrsdwnF==9] <- NA
my_table(form2.select$FIMDrsdwnF)
form2.select$FIMDrsdwnF.binary <- ifelse(form2.select$FIMDrsdwnF==6 | form2.select$FIMDrsdwnF==7, 1, 0)
my_table(form2.select$FIMDrsdwnF.binary)
form2.select$FIMDrsdwnF.binary.7 <- ifelse(form2.select$FIMDrsdwnF==7, 1, 0)
my_table(form2.select$FIMDrsdwnF.binary.7)

my_table(form2.select$FIMToiletF)
form2.select$FIMToiletF[form2.select$FIMToiletF==8 | form2.select$FIMToiletF==9] <- NA
my_table(form2.select$FIMToiletF)
form2.select$FIMToiletF.binary <- ifelse(form2.select$FIMToiletF==6 | form2.select$FIMToiletF==7, 1, 0)
my_table(form2.select$FIMToiletF.binary)
form2.select$FIMToiletF.binary.7 <- ifelse(form2.select$FIMToiletF==7, 1, 0)
my_table(form2.select$FIMToiletF.binary.7)

my_table(form2.select$FIMBladMgtF)
form2.select$FIMBladMgtF[form2.select$FIMBladMgtF==8 | form2.select$FIMBladMgtF==9 | form2.select$FIMBladMgtF==66] <- NA
my_table(form2.select$FIMBladMgtF)
form2.select$FIMBladMgtF.binary <- ifelse(form2.select$FIMBladMgtF==6 | form2.select$FIMBladMgtF==7, 1, 0)
my_table(form2.select$FIMBladMgtF.binary)
form2.select$FIMBladMgtF.binary.7 <- ifelse(form2.select$FIMBladMgtF==7, 1, 0)
my_table(form2.select$FIMBladMgtF.binary.7)

#form2.select$FIMBladAsstF
#form2.select$FIMBladAccF


my_table(form2.select$FIMBwlMgtF)
form2.select$FIMBwlMgtF[form2.select$FIMBwlMgtF==8 | form2.select$FIMBwlMgtF==9 | form2.select$FIMBwlMgtF==66] <- NA
my_table(form2.select$FIMBwlMgtF)
form2.select$FIMBwlMgtF.binary <- ifelse(form2.select$FIMBwlMgtF==6 | form2.select$FIMBwlMgtF==7, 1, 0)
my_table(form2.select$FIMBwlMgtF.binary)
form2.select$FIMBwlMgtF.binary.7 <- ifelse(form2.select$FIMBwlMgtF==7, 1, 0)
my_table(form2.select$FIMBwlMgtF.binary.7)

#form2.select$FIMBwlAsstF
#form2.select$FIMBwlAccF


my_table(form2.select$FIMBedTransF)
form2.select$FIMBedTransF[form2.select$FIMBedTransF==8 | form2.select$FIMBedTransF==9] <- NA
my_table(form2.select$FIMBedTransF)
form2.select$FIMBedTransF.binary <- ifelse(form2.select$FIMBedTransF==6 | form2.select$FIMBedTransF==7, 1, 0)
my_table(form2.select$FIMBedTransF.binary)
form2.select$FIMBedTransF.binary.7 <- ifelse(form2.select$FIMBedTransF==7, 1, 0)
my_table(form2.select$FIMBedTransF.binary.7)

my_table(form2.select$FIMToilTransF)
form2.select$FIMToilTransF[form2.select$FIMToilTransF==8 | form2.select$FIMToilTransF==9] <- NA
my_table(form2.select$FIMToilTransF)
form2.select$FIMToilTransF.binary <- ifelse(form2.select$FIMToilTransF==6 | form2.select$FIMToilTransF==7, 1, 0)
my_table(form2.select$FIMToilTransF.binary)
form2.select$FIMToilTransF.binary.7 <- ifelse(form2.select$FIMToilTransF==7, 1, 0)
my_table(form2.select$FIMToilTransF.binary.7)

my_table(form2.select$FIMTubTransF)
form2.select$FIMTubTransF[form2.select$FIMTubTransF==8 | form2.select$FIMTubTransF==9] <- NA
my_table(form2.select$FIMTubTransF)
form2.select$FIMTubTransF.binary <- ifelse(form2.select$FIMTubTransF==6 | form2.select$FIMTubTransF==7, 1, 0)
my_table(form2.select$FIMTubTransF.binary)
form2.select$FIMTubTransF.binary.7 <- ifelse(form2.select$FIMTubTransF==7, 1, 0)
my_table(form2.select$FIMTubTransF.binary.7)

#form2.select$FIMLocoModeF

my_table(form2.select$FIMLocoF)
form2.select$FIMLocoF[form2.select$FIMLocoF==8 | form2.select$FIMLocoF==9] <- NA
my_table(form2.select$FIMLocoF)
form2.select$FIMLocoF.binary <- ifelse(form2.select$FIMLocoF==6 | form2.select$FIMLocoF==7, 1, 0)
my_table(form2.select$FIMLocoF.binary)
form2.select$FIMLocoF.binary.7 <- ifelse(form2.select$FIMLocoF==7, 1, 0)
my_table(form2.select$FIMLocoF.binary.7)

my_table(form2.select$FIMStairsF)
form2.select$FIMStairsF[form2.select$FIMStairsF==8 | form2.select$FIMStairsF==9] <- NA
my_table(form2.select$FIMStairsF)
form2.select$FIMStairsF.binary <- ifelse(form2.select$FIMStairsF==6 | form2.select$FIMStairsF==7, 1, 0)
my_table(form2.select$FIMStairsF.binary)
form2.select$FIMStairsF.binary.7 <- ifelse(form2.select$FIMStairsF==7, 1, 0)
my_table(form2.select$FIMStairsF.binary.7)


## Creating FIMMOT_indep (modified independence to independence across the board (6 or 7 per item))
form2.select$FIMMOT_indep <- ifelse(form2.select$FIMFeedF.binary==1 & form2.select$FIMGroomF.binary==1 & form2.select$FIMBathF.binary==1 & form2.select$FIMDrupF.binary==1 & form2.select$FIMDrsdwnF.binary==1 & form2.select$FIMToiletF.binary==1 & form2.select$FIMBladMgtF.binary==1 & form2.select$FIMBwlMgtF.binary==1 & form2.select$FIMBedTransF.binary==1 & form2.select$FIMToilTransF.binary==1 & form2.select$FIMTubTransF.binary==1 & form2.select$FIMLocoF.binary==1 & form2.select$FIMStairsF.binary==1, 1, 0)
my_table(form2.select$FIMMOT_indep)

form2.select$FIMMOT_indep.7 <- ifelse(form2.select$FIMFeedF.binary.7==1 & form2.select$FIMGroomF.binary.7==1 & form2.select$FIMBathF.binary.7==1 & form2.select$FIMDrupF.binary.7==1 & form2.select$FIMDrsdwnF.binary.7==1 & form2.select$FIMToiletF.binary.7==1 & form2.select$FIMBladMgtF.binary.7==1 & form2.select$FIMBwlMgtF.binary.7==1 & form2.select$FIMBedTransF.binary.7==1 & form2.select$FIMToilTransF.binary.7==1 & form2.select$FIMTubTransF.binary.7==1 & form2.select$FIMLocoF.binary.7==1 & form2.select$FIMStairsF.binary.7==1, 1, 0)
my_table(form2.select$FIMMOT_indep.7)


### COG components/items

my_table(form2.select$FIMCompF)
form2.select$FIMCompF[form2.select$FIMCompF==8 | form2.select$FIMCompF==9] <- NA
my_table(form2.select$FIMCompF)
form2.select$FIMCompF.binary <- ifelse(form2.select$FIMCompF==6 | form2.select$FIMCompF==7, 1, 0)
my_table(form2.select$FIMCompF.binary)
form2.select$FIMCompF.binary.7 <- ifelse(form2.select$FIMCompF==7, 1, 0)
my_table(form2.select$FIMCompF.binary.7)

my_table(form2.select$FIMExpressF)
form2.select$FIMExpressF[form2.select$FIMExpressF==8 | form2.select$FIMExpressF==9] <- NA
my_table(form2.select$FIMExpressF)
form2.select$FIMExpressF.binary <- ifelse(form2.select$FIMExpressF==6 | form2.select$FIMExpressF==7, 1, 0)
my_table(form2.select$FIMExpressF.binary)
form2.select$FIMExpressF.binary.7 <- ifelse(form2.select$FIMExpressF==7, 1, 0)
my_table(form2.select$FIMExpressF.binary.7)

my_table(form2.select$FIMSocialF)
form2.select$FIMSocialF[form2.select$FIMSocialF==8 | form2.select$FIMSocialF==9] <- NA
my_table(form2.select$FIMSocialF)
form2.select$FIMSocialF.binary <- ifelse(form2.select$FIMSocialF==6 | form2.select$FIMSocialF==7, 1, 0)
my_table(form2.select$FIMSocialF.binary)
form2.select$FIMSocialF.binary.7 <- ifelse(form2.select$FIMSocialF==7, 1, 0)
my_table(form2.select$FIMSocialF.binary.7)

my_table(form2.select$FIMProbSlvF)
form2.select$FIMProbSlvF[form2.select$FIMProbSlvF==8 | form2.select$FIMProbSlvF==9 | form2.select$FIMProbSlvF==888] <- NA
my_table(form2.select$FIMProbSlvF)
form2.select$FIMProbSlvF.binary <- ifelse(form2.select$FIMProbSlvF==6 | form2.select$FIMProbSlvF==7, 1, 0)
my_table(form2.select$FIMProbSlvF.binary)
form2.select$FIMProbSlvF.binary.7 <- ifelse(form2.select$FIMProbSlvF==7, 1, 0)
my_table(form2.select$FIMProbSlvF.binary.7)

my_table(form2.select$FIMMemF)
form2.select$FIMMemF[form2.select$FIMMemF==8 | form2.select$FIMMemF==9] <- NA
my_table(form2.select$FIMMemF)
form2.select$FIMMemF.binary <- ifelse(form2.select$FIMMemF==6 | form2.select$FIMMemF==7, 1, 0)
my_table(form2.select$FIMMemF.binary)
form2.select$FIMMemF.binary.7 <- ifelse(form2.select$FIMMemF==7, 1, 0)
my_table(form2.select$FIMMemF.binary.7)


## Creating FIMCOG_indep (modified independence to independence across the board (6 or 7 per item))
form2.select$FIMCOG_indep <- ifelse(form2.select$FIMCompF.binary==1 & form2.select$FIMExpressF.binary==1 & form2.select$FIMSocialF.binary==1 & form2.select$FIMProbSlvF.binary==1 & form2.select$FIMMemF.binary==1, 1, 0)
my_table(form2.select$FIMCOG_indep)
form2.select$FIMCOG_indep.7 <- ifelse(form2.select$FIMCompF.binary.7==1 & form2.select$FIMExpressF.binary.7==1 & form2.select$FIMSocialF.binary.7==1 & form2.select$FIMProbSlvF.binary.7==1 & form2.select$FIMMemF.binary.7==1, 1, 0)
my_table(form2.select$FIMCOG_indep.7)


### Reshaping form2 data
dim(form2.select)
# [1] 34784   125
# Removing UID (SystemSubjectID is enough for unique identifier)
# form2.select.long <- form2.select[-1]
# dim(form2.select.long)
# [1] 31418   101
form2.select.long <- form2.select
dim(form2.select.long)
# [1] 34784   125

form2.select.wide <- reshape(form2.select.long, idvar = "Mod1id", timevar = "FollowUpPeriod", direction = "wide")
dim(form2.select.wide)
# [1] 18185   247

### IMPORTANT INFO BELOW THAT HELPED ME SOLVE THE WARNING (COMMENTED OUT FOR FUTURE USE):

# anyDuplicated(form2.select.long)
# # [1] 0
# 
# ### Finding out which UUID causes the warning: "multiple rows match for FollowUpPeriod=2"
# tab.form2.ID <- table(form2.select.long$SystemSubjectID)
# # sum(as.numeric(tab.form2.ID)>2)
# # which((as.numeric(tab.form2.ID)>2))
# row.names(tab.form2.ID)[which((as.numeric(tab.form2.ID)>2))]
# # [1] "2101712"
# 
# form2.select.long[form2.select.long$SystemSubjectID=="2101712",]
# ## Notice the row number!
# which(form2.select.long$SystemSubjectID=="2101712")
# # [1] 30144 30145 30153
# form2.select.long[30152:30154,]
# # I checked and it looks like the entry after and before 210172201 and 210172205 is 210171202, which should be 210172202. I just changed this manually in the raw form-1 file and hence commented this section out.

#########################################################################################################################







#########################################################################################################################
### Merging data

# names(SeizureVarData.nomiss)
# dim(SeizureVarData.nomiss[-1])
# # [1] 4623     3

names(seizure_data_corrected_02_22_2021)
dim(seizure_data_corrected_02_22_2021[-1])
# [1] 18186     9

SeizureData.Yr1orYr2.nomiss <- subset(seizure_data_corrected_02_22_2021, !(is.na(Year1_INC_Sz_until2020) & is.na(Year2_INC_Sz_until2020)))
dim(SeizureData.Yr1orYr2.nomiss)
# [1] 6716   10
# Analytic sample size, n=6716, these are people who had seizure in either year-1 or year-2.

table(SeizureData.Yr1orYr2.nomiss$Year1_INC_Sz_until2020)
# > table(SeizureData.Yr1orYr2.nomiss$Year1_INC_Sz_until2020)
# 
# 0    1 
# 5044  689
table(SeizureData.Yr1orYr2.nomiss$Year2_INC_Sz_until2020)
# > table(SeizureData.Yr1orYr2.nomiss$Year2_INC_Sz_until2020)
# 
# 0    1 
# 4470  694




names(form1.select)
dim(form1.select[-1])
# [1] 18438    21

merge.1 <- merge(SeizureData.Yr1orYr2.nomiss, form1.select, by="Mod1id", all.x = T)
dim(merge.1)
# [1] 6716   31
names(form2.select.wide)
dim(form2.select.wide)
# [1] 18185   247
merge.2 <- merge(merge.1, form2.select.wide, by="Mod1id", all.x = T)
dim(merge.2)
# [1] 6716  277
# Previous data dim: 4623,  226

ERFA.Aim2.Data <- merge.2
write.csv(ERFA.Aim2.Data, file="C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA/ERFA.Aim2.Data.csv", row.names = F)

#########################################################################################################################







# #########################################################################################################################
# #########################################################################################################################
# #########################################################################################################################
# 
# ### Reshaping data into wide format (CONSIDERING MISSING SystemSubjectID and UID)
# 
# dim(form2.select)
# # [1] 33739   100
# # Removing UID (SystemSubjectID is enough for unique identifier)
# # form2.select.long <- form2.select[-1]
# # dim(form2.select.long)
# # # [1] 31986   101
# form2.select.wide <- reshape(form2.select.long, idvar = "SystemSubjectID", timevar = "FollowUpPeriod", direction = "wide")
# # Warning message:
# #   In reshapeWide(data, idvar = idvar, timevar = timevar, varying = varying,  :
# #                    multiple rows match for FollowUpPeriod=2: first taken
# 
# # So, let's find our who have more than 2 follow-up time points
# sum(table(form2.select$SystemSubjectID)>3)
# 
# # Find by anyDuplicated(form2.select.long)
# ?anyDuplicated
# anyDuplicated(form2.select.long)
# 
# sum(form2.select.long$FollowUpPeriod==1)
# # [1] 16788
# sum(form2.select.long$FollowUpPeriod==2)
# # [1] 15198
# dim(form2.select.wide)
# # [1] 16677   199
# 
# ID_data_yr1 <- form2.select.long[form2.select.long$FollowUpPeriod==1,]
# dim(ID_data_yr1)
# # [1] 16788   101
# length(unique(ID_data_yr1$SystemSubjectID))
# # 16527
# ID_data_yr2 <- form2.select.long[form2.select.long$FollowUpPeriod==2,]
# dim(ID_data_yr2)
# # [1] 15198   101
# length(unique(ID_data_yr2$SystemSubjectID))
# # 14892
# 
# length(intersect(ID_data_yr1$SystemSubjectID, ID_data_yr2$SystemSubjectID))
# # [1] 14742
# ## 16527-14742
# # [1] 1785
# ## 14892-14742
# # [1] 150
# ## 14742 + 1785 + 150
# # 16677
# 
# ### Let's look at the duplicated data now (before we reshape data into wide format we need to remove the duplicates first)
# 
# ## Finding NA in SystemSubjectID or UID (form-1)
# sum(is.na(form1.select$SystemSubjectID))
# # 0
# sum(is.na(form1.select$UID))
# # 0
# 
# ## Finding NA in SystemSubjectID or UID (form-2)
# sum(is.na(form2.select$SystemSubjectID))
# # 568
# sum(is.na(form2.select$UID))
# # 0
# 
# #####################################
# ### So, use UID to merge and reshape
# #####################################
# 
# ### Trying to see if that solves the problem of duplicates
# form2.select.long <- form2.select[-2]
# dim(form2.select.long)
# # [1] 31986   101
# form2.select.wide <- reshape(form2.select.long, idvar = "UID", timevar = "FollowUpPeriod", direction = "wide")
# dim(form2.select.wide)
# # [1] 16778   199
# 
# ########################################################################################################
# ### Okay, so doesn't solve the problem but still better to use UID because not all have SystemSubjectID
# ########################################################################################################
# 
# ### Now, look at the duplicated data again:
# 
# ID_data_yr1 <- form2.select.long[form2.select.long$FollowUpPeriod==1,]
# dim(ID_data_yr1)
# # ID_data_yr1 <- subset(form2.select.long, FollowUpPeriod==1)
# # dim(ID_data_yr1)
# # [1] 16788   101
# length(unique(ID_data_yr1$UID))
# # 16775
# # So, number of duplicates in year-1: 16788-16775=13
# ## Duplicated UIDs in year-1:
# length(ID_data_yr1$UID[duplicated(ID_data_yr1$UID)])
# # [1] 13
# # Data frame with a list of ids and the number of times they occurred.
# n_occur <- data.frame(table(ID_data_yr1$UID))
# n_occur
# # Tells you which ids occurred more than once.
# n_occur[n_occur$Freq > 1,]
# # Returns the records with more than one occurrence
# ID_data_yr1[ID_data_yr1$UID %in% n_occur$Var1[n_occur$Freq > 1],]
# 
# # Number of unique UID only in year 2
# dim(n_occur[n_occur$Freq == 0,])[1]
# # [1] 3
# 
# ########################################################################################################
# ### Okay, so there are " "(empty strings) in UID!!! 14 of them!
# ########################################################################################################
# ### Let's see if they have SystemSubjectID (which is only available in form2.select)
# empty.str.UID.yr1 <- subset(form2.select, form2.select$UID=="" & form2.select$FollowUpPeriod==1)
# dim(empty.str.UID.yr1)
# # [1]  14 102
# empty.str.UID.yr1[,c("UID", "SystemSubjectID")]
# # 1 of them doesn't have SystemSubjectID (all others have that)
# 
# ### But again...
# empty.str.SystemSubjectID.yr1 <- subset(form2.select, is.na(form2.select$SystemSubjectID) & form2.select$FollowUpPeriod==1)
# dim(empty.str.SystemSubjectID.yr1)
# # 262 people don't have SystemSubjectID. 1 of them doesn't have any of UID and SystemSubjectID 
# empty.str.SystemSubjectID.yr1[,c("UID", "SystemSubjectID")]
# 
# 
# ID_data_yr2 <- form2.select.long[form2.select.long$FollowUpPeriod==2,]
# dim(ID_data_yr2)
# # ID_data_yr2 <- subset(form2.select.long, FollowUpPeriod==2)
# # dim(ID_data_yr2)
# # [1] 15198   101
# length(unique(ID_data_yr2$UID))
# # 15194
# # So, number of duplicates in year-2: 15198-15194=4
# ## Duplicated UIDs in year-2:
# length(ID_data_yr2$UID[duplicated(ID_data_yr2$UID)])
# # [1] 4
# # Data frame with a list of ids and the number of times they occurred.
# n_occur <- data.frame(table(ID_data_yr2$UID))
# n_occur
# 
# # Number of unique UID only in year 1
# dim(n_occur[n_occur$Freq == 0,])[1]
# # [1] 1584
# 
# length(intersect(ID_data_yr1$UID, ID_data_yr2$UID))
# # [1] 15191
# ## Common UIDs in year-1 and year-2: 15191
# 
# ## Unique UIDs only in year-1: 16775-15191
# # [1] 1584
# ## Unique UIDs only in year-2: 15194-15191
# # [1] 3
# ## So, the unique cohort after reshape should be: 15191 + 1584 + 3
# # 16778 (which is dim(form2.select.wide)[1] in above, but it takes the first duplicate)
# 
# ########################################################################################################
# ### 0 occurrences?? 1584 of them! Okay, these are just the levels of UID that R could not forget! 
# ### Doesn't matter for us. 
# ########################################################################################################
# # Tells you which ids occurred more than once.
# n_occur[n_occur$Freq > 1,]
# ########################################################################################################
# ### Okay, so there are " "(empty strings) in UID!!! 5 of them!
# ########################################################################################################
# # Returns the records with more than one occurrence
# ID_data_yr2[ID_data_yr2$UID %in% n_occur$Var1[n_occur$Freq > 1],]
# ### Let's see if they have SystemSubjectID (which is only available in form2.select)
# #form2.select[]
# 
# # edit(form2.select.long)
# 
# 
# 
# 
# ########################################################################################################
# ### FIXING THE EMPTY UID OR SystemSubjectID PROBLEM:
# 
# ### So, empty string in UID for year-1: 14
# ### So, empty string in UID for year-1: 4
# 
# class(form2.select$UID)
# # [1] "factor"
# levels(form2.select$UID)
# 
# class(form2.select$SystemSubjectID)
# # [1] "character"
# levels(form2.select$SystemSubjectID)
# 
# # # Converting the class of UID to "character"
# # class(form2.select$UID) <- "character"
# # class(form2.select$UID)
# # # [1] "character"
# # levels(form2.select$UID)
# 
# data_EMPTY_UID_OR_SystemSubjectID <- subset(form2.select, UID=="" | is.na(SystemSubjectID))
# dim(data_EMPTY_UID_OR_SystemSubjectID)
# # [1] 585 102
# view_UID_SysID_FollowPeriod <- data_EMPTY_UID_OR_SystemSubjectID[,c("UID","SystemSubjectID","FollowUpPeriod")]
# edit(view_UID_SysID_FollowPeriod)
# 
# # dim(subset(form2.select, UID=="" & !is.na(SystemSubjectID)))[1]
# # Missing SystemSubjectID only: data_miss_SystemSubjectID <- subset(form2.select, is.na(SystemSubjectID))
# # edit(data_miss_SystemSubjectID)
# # data_has_SystemSubjectID_but_no_UID <- subset(form2.select, UID=="")
# # edit(data_has_SystemSubjectID_but_no_UID)
# 
# ### SO, WE CAN REPLACE THE UID BY SystemSubjectID WHEREVER UID IS MISSING.
# ### THEN WE WILL DISCARD THOSE WHO DON'T HAVE BOTH UID AND SystemSubjectID.
# 
# # dim(form2.select[form2.select$UID=="" & is.na(form2.select$SystemSubjectID),])
# # form2.select[form2.select$UID=="" & is.na(form2.select$SystemSubjectID),][,c("UID", "SystemSubjectID")]
# # Only 2 people don't have both UID and SystemSubjectID
# 
# # Removing these two people
# form2.select.long.V2 <- subset(form2.select, !(UID == "" & is.na(SystemSubjectID)))
# dim(form2.select.long.V2)
# # [1] 31984   102
# dim(form2.select)
# # [1] 31986   102
# 
# # Replacing UID by SystemSubjectID for cases who don't have UID but have SystemSubjectID
# form2.select.long.V2$UID_V2 <- ifelse(form2.select.long.V2$UID=="", form2.select.long.V2$SystemSubjectID, form2.select.long.V2$UID) 
# #form2.select.long.V2$UID[form2.select.long.V2$UID==""] <- form2.select.long.V2$SystemSubjectID[form2.select.long.V2$UID==""]  
# # Checking if anyone still has missing UID_V2
# # data_has_SystemSubjectID_but_no_UID <- subset(form2.select.long.V2, UID_V2=="")
# # edit(data_has_SystemSubjectID_but_no_UID)
# data_EMPTY_UID_OR_SystemSubjectID <- subset(form2.select.long.V2, UID=="" | is.na(SystemSubjectID))
# dim(data_EMPTY_UID_OR_SystemSubjectID)
# # [1] 583 103
# view_UID_SysID_FollowPeriod <- data_EMPTY_UID_OR_SystemSubjectID[,c("UID","SystemSubjectID","FollowUpPeriod","UID_V2")]
# edit(view_UID_SysID_FollowPeriod)
# 
# 
# ###################################################################################################
# 
# 
# 
# #########################################################################################################################
# #########################################################################################################################
# #########################################################################################################################
# 
# 
# 
# 
# 










#########################################################################################################################
#########################################################################################################################
#########################################################################################################################

### Reading the data

setwd("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA")

data <- read.csv("ERFA.Aim2.Data.csv", header=T)
dim(data)
# [1] 6716  237

### Studying the missingness of different variables to be used in the analysis

my_table(data$Year1_INC_Sz_until2020)
my_table(data$Year2_INC_Sz_until2020)
my_table(data$AGE)
my_table(data$Sex.new)
my_table(data$EduYears.dummy.1)
my_table(data$EduYears.dummy.2)
my_table(data$ResDis.dummy.1)
my_table(data$ResDis.dummy.2)
my_table(data$ResF.1)
my_table(data$ResF.new.1)
my_table(data$ResF.dummy.1.1)
my_table(data$ResF.dummy.1.2)
my_table(data$ResF.2)
my_table(data$ResF.new.2)
my_table(data$ResF.dummy.2.1)
my_table(data$ResF.dummy.2.2)
my_table(data$DOD_Injury_severity)
my_table(data$FIMMOTD.Rasch)
my_table(data$FIMCOGD.Rasch)
# my_table(data$DRS_PIEmpF.1)
# my_table(data$DRS_PIEmpF.2)
my_table(data$DRS_PIEmpF_bin.1)
my_table(data$DRS_PIEmpF_bin.2)
my_table(data$EMPLOYMENTF.new.1)
my_table(data$EMPLOYMENTF.new.2)
my_table(data$PROBLEMUse.new.1)
my_table(data$PROBLEMUse.new.2)
#my_table(data$DRSF.1)
#my_table(data$DRSF.2)
my_table(data$depression_fann.1)
my_table(data$depression_fann.2)
my_table(data$GAD7TOT.new.1)
my_table(data$GAD7TOT.new.2)
my_table(data$SWLSTOT.new.1)
my_table(data$SWLSTOT.new.2)
#my_table(data$PARTSummary.1)
#my_table(data$PARTSummary.2)
sum(is.na(data$PARTSummary.1))
sum(is.na(data$PARTSummary.2))
#my_table(data$data$data$TransMode.1)
sum(is.na(data$TransMode.new.1))
sum(is.na(data$TransMode.new.2))
my_table(data$FIMMOT_indep.1)
my_table(data$FIMMOT_indep.2)
my_table(data$FIMCOG_indep.1)
my_table(data$FIMCOG_indep.2)

### Year-1 models

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  

# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMMOT_yr1) 

# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMCOG_yr1)

# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (DrivingStatus_yr1) 

# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Depression_yr1)

# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Anxiety_yr1)

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  



### Year-2 models

# FIMMOT_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  

# FIMCOG_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  

# DrivingStatus_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  

# Depression_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  

# Anxiety_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  

# (Mediation testing) Participation_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence + Participation_yr1

# (Mediation testing) Participation_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMMOT_yr2) + Participation_yr1 

# (Mediation testing) Participation_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMCOG_yr2) + Participation_yr1

# (Mediation testing) Participation_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  + (DrivingStatus_yr2) + Participation_yr1 

# (Mediation testing) Participation_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  + (Depression_yr2) + Participation_yr1

# (Mediation testing) Participation_yr2 ~ PTS_yr1 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  + (Anxiety_yr2) + Participation_yr1

# SWLS_yr2 ~ PTS_yr1 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence + SWLS_yr1  

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################


### SEM for year-1 (with binary FIM according to Shannon's suggestion)

# Dr. Wagner's suggestions
# # ResF
# 1 vs other
# 
# my_table(data$ResF.1)
# my_table(data$ResF.2)
# 
# # GAD
# mild+moderate vs other
# 
# # Transportation
# my_table(data$TransMode.new.1)
# my_table(data$TransMode.new.2)
# 
# 1) indep vs. not
# 2) 8=worst, 4, 2, 3, 1=best


# Residence binary

# my_table(data$ResF.new.1)
# my_table(data$ResF.new.2)

# https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=RES
data$ResF.1.binary <- ifelse(data$ResF.1==1, 1, 0)
data$ResF.2.binary <- ifelse(data$ResF.2==1, 1, 0)

my_table(data$ResF.1.binary)
my_table(data$ResF.2.binary)


# Transport binary

my_table(data$TransMode.1)
my_table(data$TransMode.2)

# https://www.tbindsc.org/SyllabusDetail.aspx?MOD=2&ID=TRANS
data$TransMode.1.binary <- ifelse(data$TransMode.1==1, 1, 0)
data$TransMode.2.binary <- ifelse(data$TransMode.2==1, 1, 0)

my_table(data$TransMode.1.binary)
my_table(data$TransMode.2.binary)



# GAD binary

my_table(data$GAD7TOT.new.1)
my_table(data$GAD7TOT.new.2)
# 0-4: minimal anxiety
# 5-9: mild anxiety
# 10-14: moderate anxiety
# 15-21: severe anxiety

data$GAD7TOT.1.binary <- ifelse(data$GAD7TOT.new.1==3 | data$GAD7TOT.new.1==4, 1, 0)
data$GAD7TOT.2.binary <- ifelse(data$GAD7TOT.new.2==3 | data$GAD7TOT.new.2==4, 1, 0)

my_table(data$GAD7TOT.1.binary)
my_table(data$GAD7TOT.2.binary)


# Also, use EduYears as continuous. Although it looks like categorical but we can use it as years of education 

# https://www.tbindsc.org/SyllabusDetail.aspx?MOD=1&ID=EDU
my_table(data$EduYears)
data$EduYears[data$EduYears>20] <- NA  
my_table(data$EduYears)


data2 <- data

write.csv(data2, file="C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA/ERFA.Aim2.Data.csv", row.names = F)


data2 <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA/ERFA.Aim2.Data.csv", header = T)
dim(data2)
# [1] 6716  243

table(data2$INJYEAR)

data3 <- subset(data2, INJYEAR<2019)
dim(data3)
# [1] 6089  243

table(data3$Year1_INC_Sz_until2020)

table(data3$Year2_INC_Sz_until2020)

write.csv(data3, file="C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA/ERFA.Aim2.Data.csv", row.names = F)






# my_table(data$TransMode.new.1)
# data$TransMode.new.1.dumeq2 <- ifelse(data$TransMode.new.1==2,1,0)
# data$TransMode.new.1.dumeq3 <- ifelse(data$TransMode.new.1==3,1,0)
# data$TransMode.new.1.dumeq4 <- ifelse(data$TransMode.new.1==4,1,0)
# my_table(data$TransMode.new.1.dumeq2)
# my_table(data$TransMode.new.1.dumeq3)
# my_table(data$TransMode.new.1.dumeq4)
# 
# my_table(data$TransMode.new.1)
# data$TransMode.indep.1 <- ifelse(data$TransMode.new.1==1,1,0)
# my_table(data$TransMode.indep.1)
# 
# 
# my_table(data$GAD7TOT.new.1)
# data$GAD7TOT.new.1.dumeq2 <- ifelse(data$GAD7TOT.new.1==2,1,0)
# data$GAD7TOT.new.1.dumeq3 <- ifelse(data$GAD7TOT.new.1==3,1,0)
# data$GAD7TOT.new.1.dumeq4 <- ifelse(data$GAD7TOT.new.1==4,1,0)
# my_table(data$GAD7TOT.new.1.dumeq2)
# my_table(data$GAD7TOT.new.1.dumeq3)
# my_table(data$GAD7TOT.new.1.dumeq4)


library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Seizure_YN + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.indep.1 ~ Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.new.1 ~ Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.new.1.dumeq2 + TransMode.new.1.dumeq3 + TransMode.new.1.dumeq4 + depression_fann.1 + GAD7TOT.new.1.dumeq2 + GAD7TOT.new.1.dumeq3 + GAD7TOT.new.1.dumeq4 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.indep.1 + depression_fann.1 + GAD7TOT.new.1.dumeq2 + GAD7TOT.new.1.dumeq3 + GAD7TOT.new.1.dumeq4 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary


# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMMOT_yr1) 
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMCOG_yr1)
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (DrivingStatus_yr1) 
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Depression_yr1)
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Anxiety_yr1)

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.new.1.dumeq2 + TransMode.new.1.dumeq3 + TransMode.new.1.dumeq4 + depression_fann.1 + GAD7TOT.new.1.dumeq2 + GAD7TOT.new.1.dumeq3 + GAD7TOT.new.1.dumeq4 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.indep.1 + depression_fann.1 + GAD7TOT.new.1.dumeq2 + GAD7TOT.new.1.dumeq3 + GAD7TOT.new.1.dumeq4 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

'



library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Seizure_YN + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.indep.1 ~ Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1 ~ Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# PARTSummary.1 ~ FIMMOTF.Rasch.1 + FIMCOG_indep.1 + TransMode.new.1.dumeq2 + TransMode.new.1.dumeq3 + TransMode.new.1.dumeq4 + depression_fann.1 + GAD7TOT.new.1 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.indep.1 + depression_fann.1 + GAD7TOT.1 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.1.binary


# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMMOT_yr1) 
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMCOG_yr1)
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (DrivingStatus_yr1) 
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Depression_yr1)
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Anxiety_yr1)

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.new.1.dumeq2 + TransMode.new.1.dumeq3 + TransMode.new.1.dumeq4 + depression_fann.1 + GAD7TOT.new.1.dumeq2 + GAD7TOT.new.1.dumeq3 + GAD7TOT.new.1.dumeq4 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.indep.1 + depression_fann.1 + GAD7TOT.1 + Seizure_YN + FIMMOTD.Rasch + FIMCOGD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

'



# fit the model 
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.indep.1","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)





data$TransMode.new.1.dumeq4.as.outcome <- data$TransMode.new.1.dumeq4

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOTF.Rasch.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOGF.Rasch.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.new.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1
#TransMode.new.1.dumeq4.as.outcome ~ Seizure_YN

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1 ~ Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + depression_fann.1 + GAD7TOT.1 + Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMMOT_yr1) 
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (FIMCOG_yr1)
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (DrivingStatus_yr1) 
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Depression_yr1)
# (Mediation testing) Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  + (Anxiety_yr1)

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.1 ~ PARTSummary.1 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + depression_fann.1 + GAD7TOT.1 + Seizure_YN + FIMMOTD.Rasch + DRS_PIEmpF.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears.dummy.1 + EduYears.dummy.2 + ResF.dummy.1.1 + ResF.dummy.2.1

'






# fit the model 
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)


















###########################################################################################################################

### 4/11/2021

### Reading the data

setwd("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA")

data <- read.csv("ERFA.Aim2.Data.csv", header=T)
dim(data)
# [1] 6089  243



## Year-1 ERFA SEM



## With all covariates and mostly continuous outcomes (FIM Rasch, GAD raw)

library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOTF.Rasch.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOGF.Rasch.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1 ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1 + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1 + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'



# fit the model 
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.1.binary","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)








## With all covariates and mostly binary outcomes (FIM Rasch, GAD binary)


library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOTF.Rasch.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOGF.Rasch.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'



# fit the model 
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)






## With all covariates and mostly binary outcomes (FIM Rasch, GAD binary, TransMode ordinal)
## Note: FIMMOT.Rasch and FIMCOG.Rasch were left skewed!

library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOTF.Rasch.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOGF.Rasch.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
TransMode.new.1 ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + TransMode.new.1 + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOTF.Rasch.1 + FIMCOGF.Rasch.1 + TransMode.new.1 + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary"))
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1","GAD7TOT.1.binary"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)








## With all covariates and mostly binary outcomes (TransMode ordinal)

table(data$TransMode.new.1)
my_table(data$TransMode.new.1)
data$TransMode.new.1.dumeq2 <- ifelse(data$TransMode.new.1==2,1,0)
data$TransMode.new.1.dumeq3 <- ifelse(data$TransMode.new.1==3,1,0)
data$TransMode.new.1.dumeq4 <- ifelse(data$TransMode.new.1==4,1,0)
my_table(data$TransMode.new.1.dumeq2)
my_table(data$TransMode.new.1.dumeq3)
my_table(data$TransMode.new.1.dumeq4)


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
TransMode.new.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.new.1.dumeq2 + TransMode.new.1.dumeq3 + TransMode.new.1.dumeq4 + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.new.1.dumeq2 + TransMode.new.1.dumeq3 + TransMode.new.1.dumeq4 + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.1.binary"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)










## Year-1 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

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

my_table(data$FIMMOT_indep.1)
my_table(data$FIMCOG_indep.1)
my_table(data$TransMode.1.binary)
my_table(data$depression_fann.1)
my_table(data$GAD7TOT.1.binary)
sum(is.na(data$PARTSummary.1))
my_table(data$SWLSTOT.new.1)



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









#########################################################################################################################


### SEM for year-2

### Reading the data

setwd("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA")

data <- read.csv("ERFA.Aim2.Data.csv", header=T)
dim(data)
# [1] 6089  243


## Year-2 ERFA SEM

## With all covariates and mostly binary outcomes

library(lavaan)

erfa.sem.year2.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.1 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOG_indep.1 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

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
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1","depression_fann.1"))
sem.mod.year2.v1 <- sem(erfa.sem.year2.v1, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("TransMode.new.1.dumeq4.as.outcome","depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("depression_fann.1"))
# sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.new.1","depression_fann.1","GAD7TOT.new.1","SWLSTOT.new.1"))
# summary(sem.mod.year1.v1, fit.measure=T)
summary(sem.mod.year2.v1, standardized=T, fit.measure=T)






## Year-2 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

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









#########################################################################################################################
#########################################################################################################################
#########################################################################################################################



### Trying without driving status and SWLS (year-1)

## Year-1 ERFA SEM

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year1.v1 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# # DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# # SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

'

# fit the model 
sem.mod.year1.v1 <- sem(erfa.sem.year1.v1, data=data, ordered=c("FIMMOT_indep.1","FIMCOG_indep.1","TransMode.1.binary","depression_fann.1","GAD7TOT.1.binary","SWLSTOT.new.1"))
summary(sem.mod.year1.v1, standardized=T, fit.measure=T)


#########################################################################################################################


### Trying without driving status and SWLS (year-2)

## With all covariates and mostly binary outcomes (and SWLS as ordinal)

library(lavaan)

erfa.sem.year2.v2 <- '
# synchronous covariances
# None

## Regression paths

# FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# # DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# # SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

'

# fit the model 
sem.mod.year2.v2 <- sem(erfa.sem.year2.v2, data=data, ordered=c("FIMMOT_indep.2","FIMCOG_indep.2","TransMode.2.binary","depression_fann.2","GAD7TOT.2.binary","SWLSTOT.new.2"))
summary(sem.mod.year2.v2, standardized=T, fit.measure=T)
