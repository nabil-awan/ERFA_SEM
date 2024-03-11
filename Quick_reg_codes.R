#############################################################################################################################################

### Year-1 pre-SEM regressions (NEW CODING SCHEME FOR VARIABLES)

############################################################################

### Reading the data

d <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA/ERFA.Aim2.Data.csv", header = T)
dim(d)
# [1] 6089  283

#############################################################################################################################################

### Year-1 pre-SEM regressions (Not been updated yet)

############################################################################

# # FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMMOT_indep.1 ~ Year1_INC_Sz_until2020
tab <- glm(FIMMOT_indep.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ AGE
tab <- glm(FIMMOT_indep.1 ~ AGE, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ Sex.new
tab <- glm(FIMMOT_indep.1 ~ Sex.new, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ DOD_Injury_severity
tab <- glm(FIMMOT_indep.1 ~ DOD_Injury_severity, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ ResF.1.binary
tab <- glm(FIMMOT_indep.1 ~ ResF.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ FIMMOTD.Rasch
tab <- glm(FIMMOT_indep.1 ~ FIMMOTD.Rasch, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ DRS_PIEmpF_bin.1
tab <- glm(FIMMOT_indep.1 ~ DRS_PIEmpF_bin.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ PROBLEMUse.new.1
tab <- glm(FIMMOT_indep.1 ~ PROBLEMUse.new.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# FIMCOG_indep.1 ~ Year1_INC_Sz_until2020
tab <- glm(FIMCOG_indep.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# TransMode.1.binary ~ Year1_INC_Sz_until2020
tab <- glm(TransMode.1.binary ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# depression_fann.1 ~ Year1_INC_Sz_until2020
tab <- glm(depression_fann.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# GAD7TOT.1.binary ~ Year1_INC_Sz_until2020
tab <- glm(GAD7TOT.1.binary ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# PARTSummary.1 ~ Year1_INC_Sz_until2020
tab <- glm(PARTSummary.1 ~ Year1_INC_Sz_until2020, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ FIMMOT_indep.1
tab <- glm(PARTSummary.1 ~ FIMMOT_indep.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ FIMCOG_indep.1
tab <- glm(PARTSummary.1 ~ FIMCOG_indep.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ TransMode.1.binary
tab <- glm(PARTSummary.1 ~ TransMode.1.binary, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ depression_fann.1
tab <- glm(PARTSummary.1 ~ depression_fann.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ GAD7TOT.1.binary
tab <- glm(PARTSummary.1 ~ GAD7TOT.1.binary, family = "gaussian", data=d)
summary(tab)



############################################################################


# # SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

table(d$SWLSTOT.new.1)

library(MASS)

### Ordinal logistic regression

# SWLS_yr1 ~ Year1_INC_Sz_until2020
tab <- polr(as.factor(SWLSTOT.new.1) ~ Year1_INC_Sz_until2020, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ FIMMOT_indep.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ FIMMOT_indep.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ FIMCOG_indep.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ FIMCOG_indep.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ TransMode.1.binary
tab <- polr(as.factor(SWLSTOT.new.1) ~ TransMode.1.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ depression_fann.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ depression_fann.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ GAD7TOT.1.binary
tab <- polr(as.factor(SWLSTOT.new.1) ~ GAD7TOT.1.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ PARTSummary.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ PARTSummary.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))



#############################################################################################################################################

### Year-2 pre-SEM regressions

############################################################################

# # FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMMOT_indep.2 ~ Year2_INC_Sz_until2020
tab <- glm(FIMMOT_indep.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ AGE
tab <- glm(FIMMOT_indep.2 ~ AGE, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ Sex.new
tab <- glm(FIMMOT_indep.2 ~ Sex.new, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ DOD_Injury_severity
tab <- glm(FIMMOT_indep.2 ~ DOD_Injury_severity, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ ResF.2.binary
tab <- glm(FIMMOT_indep.2 ~ ResF.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ FIMMOTD.Rasch
tab <- glm(FIMMOT_indep.2 ~ FIMMOTD.Rasch, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ DRS_PIEmpF_bin.2
tab <- glm(FIMMOT_indep.2 ~ DRS_PIEmpF_bin.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ PROBLEMUse.new.2
tab <- glm(FIMMOT_indep.2 ~ PROBLEMUse.new.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# FIMCOG_indep.2 ~ Year2_INC_Sz_until2020
tab <- glm(FIMCOG_indep.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# TransMode.2.binary ~ Year2_INC_Sz_until2020
tab <- glm(TransMode.2.binary ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# depression_fann.2 ~ Year2_INC_Sz_until2020
tab <- glm(depression_fann.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# GAD7TOT.2.binary ~ Year2_INC_Sz_until2020
tab <- glm(GAD7TOT.2.binary ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# PARTSummary.2 ~ Year2_INC_Sz_until2020
tab <- glm(PARTSummary.2 ~ Year2_INC_Sz_until2020, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ FIMMOT_indep.2
tab <- glm(PARTSummary.2 ~ FIMMOT_indep.2, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ FIMCOG_indep.2
tab <- glm(PARTSummary.2 ~ FIMCOG_indep.2, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ TransMode.2.binary
tab <- glm(PARTSummary.2 ~ TransMode.2.binary, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ depression_fann.2
tab <- glm(PARTSummary.2 ~ depression_fann.2, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ GAD7TOT.2.binary
tab <- glm(PARTSummary.2 ~ GAD7TOT.2.binary, family = "gaussian", data=d)
summary(tab)



############################################################################


# # SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

table(d$SWLSTOT.new.2)

table(d$SWLSTOT.2)

### Ordinal logistic regression

# SWLS_yr2 ~ Year2_INC_Sz_until2020
tab <- polr(as.factor(SWLSTOT.new.2) ~ Year2_INC_Sz_until2020, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




# SWLS_yr2 ~ FIMMOT_indep.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ FIMMOT_indep.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




# SWLS_yr2 ~ FIMCOG_indep.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ FIMCOG_indep.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




# SWLS_yr2 ~ TransMode.2.binary
tab <- polr(as.factor(SWLSTOT.new.2) ~ TransMode.2.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




# SWLS_yr2 ~ depression_fann.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ depression_fann.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




# SWLS_yr2 ~ GAD7TOT.2.binary
tab <- polr(as.factor(SWLSTOT.new.2) ~ GAD7TOT.2.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




# SWLS_yr2 ~ PARTSummary.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ PARTSummary.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))




#############################################################################################################################################

# ### STATA codes
#
# import delimited "C:\Nabil Awan\Nabil_University of Pittsburgh\GSR\ERFA grant\Aim 2 SEM\TBIMS data\ERFAAim2Datacsv", casepreserve clear 
#
# *** Year-1 models
# 
# * FIMMOT_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMMOT_indep1 Seizure_YN FIMMOTDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2
# 
# 
# * FIMCOG_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMCOG_indep1 Seizure_YN FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2
# 
# 
# * DrivingStatus_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit TransModenew1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2, or  
# 
# 
# * Depression_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic depression_fann1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Anxiety_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit GAD7TOTnew1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMMOT_yr1 
# 
# reg PARTSummary1 Seizure_YN i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2 FIMMOT_indep1 
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMCOG_yr1
# 
# reg PARTSummary1 Seizure_YN i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  FIMCOG_indep1
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + DrivingStatus_yr1 
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.TransModenew1 
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Depression_yr1
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  depression_fann1
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Anxiety_yr1
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.GAD7TOTnew1
# 
# 
# * SWLS_yr1 ~ Seizure_YN + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit SWLSTOTnew1 Seizure_YN PARTSummary1 depression_fann1 GAD7TOTnew1 FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# 
# *** Year-2 models
# 
# * FIMMOT_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMMOT_indep2 Seizure_YN FIMMOTDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * FIMCOG_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMCOG_indep2 Seizure_YN FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * DrivingStatus_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit TransModenew2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Depression_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic depression_fann2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Anxiety_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit GAD7TOTnew2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2 PARTSummary1
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMMOT_yr2 + Participation_yr1 
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  FIMMOT_indep2 PARTSummary1 
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMCOG_yr2 + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  FIMCOG_indep2 PARTSummary1
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + DrivingStatus_yr2 + Participation_yr1 
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.TransModenew2 PARTSummary1 
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Depression_yr2 + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  depression_fann2 PARTSummary1
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Anxiety_yr2 + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.GAD7TOTnew2 PARTSummary1
# 
# 
# * SWLS_yr2 ~ Seizure_YN + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence + SWLS_yr1  
# 
# ologit SWLSTOTnew2 Seizure_YN PARTSummary2 depression_fann2 GAD7TOTnew2 FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2 i.SWLSTOTnew1  



























#############################################################################################################################################

### Year-1 pre-SEM regressions (OLD CODING SCHEME FOR VARIABLES)

#############################################################################################################################################

### Reading the data

d <- read.csv("C:/Nabil Awan/Nabil_University of Pittsburgh/GSR/ERFA grant/Aim 2 SEM/DATA/ERFA.Aim2.Data.csv", header = T)
dim(d)
# [1] 6089  243

############################################################################

# # FIMMOT_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary 

# FIMMOT_indep.1 ~ Year1_INC_Sz_until2020
tab <- glm(FIMMOT_indep.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ AGE
tab <- glm(FIMMOT_indep.1 ~ AGE, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ Sex.new
tab <- glm(FIMMOT_indep.1 ~ Sex.new, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ DOD_Injury_severity
tab <- glm(FIMMOT_indep.1 ~ DOD_Injury_severity, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ ResF.1.binary
tab <- glm(FIMMOT_indep.1 ~ ResF.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ FIMMOTD.Rasch
tab <- glm(FIMMOT_indep.1 ~ FIMMOTD.Rasch, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ DRS_PIEmpF_bin.1
tab <- glm(FIMMOT_indep.1 ~ DRS_PIEmpF_bin.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.1 ~ PROBLEMUse.new.1
tab <- glm(FIMMOT_indep.1 ~ PROBLEMUse.new.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # FIMCOG_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# FIMCOG_indep.1 ~ Year1_INC_Sz_until2020
tab <- glm(FIMCOG_indep.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # DrivingStatus_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# TransMode.1.binary ~ Year1_INC_Sz_until2020
tab <- glm(TransMode.1.binary ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Depression_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# depression_fann.1 ~ Year1_INC_Sz_until2020
tab <- glm(depression_fann.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # Anxiety_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# GAD7TOT.1.binary ~ Year1_INC_Sz_until2020
tab <- glm(GAD7TOT.1.binary ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Participation_yr1 ~ PTS_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# PARTSummary.1 ~ FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

# PARTSummary.1 ~ Year1_INC_Sz_until2020
tab <- glm(PARTSummary.1 ~ Year1_INC_Sz_until2020, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ FIMMOT_indep.1
tab <- glm(PARTSummary.1 ~ FIMMOT_indep.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ FIMCOG_indep.1
tab <- glm(PARTSummary.1 ~ FIMCOG_indep.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ TransMode.1.binary
tab <- glm(PARTSummary.1 ~ TransMode.1.binary, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ depression_fann.1
tab <- glm(PARTSummary.1 ~ GAD7TOT.1.binary, family = "gaussian", data=d)
summary(tab)

# PARTSummary.1 ~ GAD7TOT.1.binary

# Table 1g (ERFA SEM conceptualization.docx)
tab <- glm(PARTSummary.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary, family = "gaussian", data=d)
summary(tab)




############################################################################


# # SWLS_yr1 ~ PTS_yr1 + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.1 ~ PARTSummary.1 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + Year1_INC_Sz_until2020 + DRS_PIEmpF_bin.1 + PROBLEMUse.new.1 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.1.binary

table(d$SWLSTOT.new.1)

### Ordinal logistic regression

# SWLS_yr1 ~ Year1_INC_Sz_until2020
tab <- polr(as.factor(SWLSTOT.new.1) ~ Year1_INC_Sz_until2020, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ FIMMOT_indep.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ FIMMOT_indep.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ FIMCOG_indep.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ FIMCOG_indep.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ TransMode.1.binary
tab <- polr(as.factor(SWLSTOT.new.1) ~ TransMode.1.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ depression_fann.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ depression_fann.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ GAD7TOT.1.binary
tab <- polr(as.factor(SWLSTOT.new.1) ~ GAD7TOT.1.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr1 ~ PARTSummary.1
tab <- polr(as.factor(SWLSTOT.new.1) ~ PARTSummary.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[1,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# Table 1i
tab <- polr(as.factor(SWLSTOT.new.1) ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + PARTSummary.1, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
cbind(ctable, p)
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

############################################################################

## mid-step regressions

# Table 1j
tab <- glm(TransMode.1.binary ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.1.binary ~ FIMMOT_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.1.binary ~ FIMCOG_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.1.binary ~ depression_fann.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.1.binary ~ GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# Table 1k
tab <- glm(TransMode.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + depression_fann.1 + GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 1l
tab <- glm(depression_fann.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.1 ~ FIMMOT_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.1 ~ FIMCOG_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.1 ~ TransMode.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.1 ~ GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# Table 1m
tab <- glm(depression_fann.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 1n
tab <- glm(GAD7TOT.1.binary ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.1.binary ~ FIMMOT_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.1.binary ~ FIMCOG_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.1.binary ~ TransMode.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.1.binary ~ depression_fann.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# Table 1o
tab <- glm(GAD7TOT.1.binary ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))



# Table 1p
tab <- glm(FIMMOT_indep.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.1 ~ FIMCOG_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.1 ~ TransMode.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.1 ~ depression_fann.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.1 ~ GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 1q
tab <- glm(FIMMOT_indep.1 ~ Year1_INC_Sz_until2020 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))



# Table 1r
tab <- glm(FIMCOG_indep.1 ~ Year1_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.1 ~ FIMMOT_indep.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.1 ~ TransMode.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.1 ~ depression_fann.1, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.1 ~ GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 1s
tab <- glm(FIMCOG_indep.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))




#############################################################################################################################################

### Year-2 pre-SEM regressions

############################################################################

# # FIMMOT_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOTD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary 

# FIMMOT_indep.2 ~ Year2_INC_Sz_until2020
tab <- glm(FIMMOT_indep.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ AGE
tab <- glm(FIMMOT_indep.2 ~ AGE, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ Sex.new
tab <- glm(FIMMOT_indep.2 ~ Sex.new, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ DOD_Injury_severity
tab <- glm(FIMMOT_indep.2 ~ DOD_Injury_severity, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ ResF.2.binary
tab <- glm(FIMMOT_indep.2 ~ ResF.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ FIMMOTD.Rasch
tab <- glm(FIMMOT_indep.2 ~ FIMMOTD.Rasch, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ DRS_PIEmpF_bin.2
tab <- glm(FIMMOT_indep.2 ~ DRS_PIEmpF_bin.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# FIMMOT_indep.2 ~ PROBLEMUse.new.2
tab <- glm(FIMMOT_indep.2 ~ PROBLEMUse.new.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # FIMCOG_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOGD.Rasch + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# FIMCOG_indep.2 ~ Year2_INC_Sz_until2020
tab <- glm(FIMCOG_indep.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # DrivingStatus_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# TransMode.2.binary ~ Year2_INC_Sz_until2020
tab <- glm(TransMode.2.binary ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Depression_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# depression_fann.2 ~ Year2_INC_Sz_until2020
tab <- glm(depression_fann.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################

# # Anxiety_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

# GAD7TOT.2.binary ~ Year2_INC_Sz_until2020
tab <- glm(GAD7TOT.2.binary ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

############################################################################


# # Participation_yr2 ~ PTS_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# PARTSummary.2 ~ FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

### Table 2f

# PARTSummary.2 ~ Year2_INC_Sz_until2020
tab <- glm(PARTSummary.2 ~ Year2_INC_Sz_until2020, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ FIMMOT_indep.2
tab <- glm(PARTSummary.2 ~ FIMMOT_indep.2, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ FIMCOG_indep.2
tab <- glm(PARTSummary.2 ~ FIMCOG_indep.2, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ TransMode.2.binary
tab <- glm(PARTSummary.2 ~ TransMode.2.binary, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ depression_fann.2
tab <- glm(PARTSummary.2 ~ depression_fann.2, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ GAD7TOT.2.binary
tab <- glm(PARTSummary.2 ~ GAD7TOT.2.binary, family = "gaussian", data=d)
summary(tab)


### Table 2f_V2

# PARTSummary.2 ~ Year1_INC_Sz_until2020
tab <- glm(PARTSummary.2 ~ Year1_INC_Sz_until2020, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ FIMMOT_indep.1
tab <- glm(PARTSummary.2 ~ FIMMOT_indep.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ FIMCOG_indep.1
tab <- glm(PARTSummary.2 ~ FIMCOG_indep.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ TransMode.1.binary
tab <- glm(PARTSummary.2 ~ TransMode.1.binary, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ depression_fann.1
tab <- glm(PARTSummary.2 ~ depression_fann.1, family = "gaussian", data=d)
summary(tab)

# PARTSummary.2 ~ GAD7TOT.1.binary
tab <- glm(PARTSummary.2 ~ GAD7TOT.1.binary, family = "gaussian", data=d)
summary(tab)



# Table 2g
tab <- glm(PARTSummary.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary, family = "gaussian", data=d)
summary(tab)

# Table 2g_V2
tab <- glm(PARTSummary.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary, family = "gaussian", data=d)
summary(tab)

############################################################################

### Table 1h

tab <- lm(SWLSTOT.1 ~ Year1_INC_Sz_until2020, data = d)
summary(tab)

tab <- lm(SWLSTOT.1 ~ FIMMOT_indep.1, data = d)
summary(tab)

tab <- lm(SWLSTOT.1 ~ FIMCOG_indep.1, data = d)
summary(tab)

tab <- lm(SWLSTOT.1 ~ TransMode.1.binary, data = d)
summary(tab)

tab <- lm(SWLSTOT.1 ~ depression_fann.1, data = d)
summary(tab)

tab <- lm(SWLSTOT.1 ~ GAD7TOT.1.binary, data = d)
summary(tab)

tab <- lm(SWLSTOT.1 ~ PARTSummary.1, data = d)
summary(tab)


### Table 2h

tab <- lm(SWLSTOT.2 ~ Year2_INC_Sz_until2020, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ FIMMOT_indep.2, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ FIMCOG_indep.2, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ TransMode.2.binary, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ depression_fann.2, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ GAD7TOT.2.binary, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ PARTSummary.2, data = d)
summary(tab)


### Table 2h_V2

tab <- lm(SWLSTOT.2 ~ Year1_INC_Sz_until2020, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ FIMMOT_indep.1, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ FIMCOG_indep.1, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ TransMode.1.binary, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ depression_fann.1, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ GAD7TOT.1.binary, data = d)
summary(tab)

tab <- lm(SWLSTOT.2 ~ PARTSummary.2, data = d)
summary(tab)


# # SWLS_yr2 ~ PTS_yr2 + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + age + sex + GCS + Education + Residence  
# SWLSTOT.new.2 ~ PARTSummary.2 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + Year2_INC_Sz_until2020 + DRS_PIEmpF_bin.2 + PROBLEMUse.new.2 + AGE + Sex.new + DOD_Injury_severity + EduYears + ResF.2.binary

table(d$SWLSTOT.new.2)

### Ordinal logistic regression

# SWLS_yr2 ~ Year2_INC_Sz_until2020
tab <- polr(as.factor(SWLSTOT.new.2) ~ Year2_INC_Sz_until2020, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr2 ~ FIMMOT_indep.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ FIMMOT_indep.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr2 ~ FIMCOG_indep.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ FIMCOG_indep.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr2 ~ TransMode.2.binary
tab <- polr(as.factor(SWLSTOT.new.2) ~ TransMode.2.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr2 ~ depression_fann.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ depression_fann.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr2 ~ GAD7TOT.2.binary
tab <- polr(as.factor(SWLSTOT.new.2) ~ GAD7TOT.2.binary, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))

# SWLS_yr2 ~ PARTSummary.2
tab <- polr(as.factor(SWLSTOT.new.2) ~ PARTSummary.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))[2,]
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))



# Table 1i

tab <- lm(SWLSTOT.1 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + PARTSummary.1, data = d)
summary(tab)


# Table 2i

tab <- lm(SWLSTOT.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + PARTSummary.2, data = d)
summary(tab)


# Table 2i_V2

tab <- lm(SWLSTOT.2 ~ Year1_INC_Sz_until2020 + FIMMOT_indep.1 + FIMCOG_indep.1 + TransMode.1.binary + depression_fann.1 + GAD7TOT.1.binary + PARTSummary.2, data = d)
summary(tab)


tab <- polr(as.factor(SWLSTOT.new.2) ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary + PARTSummary.2, data = d, Hess=TRUE)
summary(tab)
# store table
(ctable <- coef(summary(tab)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
cbind(ctable, p)
# confidence intervals
(ci <- confint(tab)) # default method gives profiled CIs
## OR and CI
exp(cbind(OR = coef(tab), ci))


#############################################################################

## mid-step regressions

# Table 2j
tab <- glm(TransMode.2.binary ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.2.binary ~ FIMMOT_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.2.binary ~ FIMCOG_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.2.binary ~ depression_fann.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(TransMode.2.binary ~ GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# Table 2k
tab <- glm(TransMode.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + depression_fann.2 + GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 2l
tab <- glm(depression_fann.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.2 ~ FIMMOT_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.2 ~ FIMCOG_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.2 ~ TransMode.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(depression_fann.2 ~ GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# Table 2m
tab <- glm(depression_fann.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 2n
tab <- glm(GAD7TOT.2.binary ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.2.binary ~ FIMMOT_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.2.binary ~ FIMCOG_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.2.binary ~ TransMode.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(GAD7TOT.2.binary ~ depression_fann.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

# Table 2o
tab <- glm(GAD7TOT.2.binary ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))




# Table 2p
tab <- glm(FIMMOT_indep.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.2 ~ FIMCOG_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.2 ~ TransMode.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.2 ~ depression_fann.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMMOT_indep.2 ~ GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 2q
tab <- glm(FIMMOT_indep.2 ~ Year2_INC_Sz_until2020 + FIMCOG_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))



# Table 2r
tab <- glm(FIMCOG_indep.2 ~ Year2_INC_Sz_until2020, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.2 ~ FIMMOT_indep.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.2 ~ TransMode.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.2 ~ depression_fann.2, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))

tab <- glm(FIMCOG_indep.2 ~ GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


# Table 2s
tab <- glm(FIMCOG_indep.2 ~ Year2_INC_Sz_until2020 + FIMMOT_indep.2 + TransMode.2.binary + depression_fann.2 + GAD7TOT.2.binary, family = "binomial", data=d)
summary(tab)
require(MASS)
exp(cbind(coef(tab), confint(tab)))


#############################################################################################################################################

# ### STATA codes
#
# import delimited "C:\Nabil Awan\Nabil_University of Pittsburgh\GSR\ERFA grant\Aim 2 SEM\TBIMS data\ERFAAim2Datacsv", casepreserve clear 
#
# *** Year-1 models
# 
# * FIMMOT_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMMOT_indep1 Seizure_YN FIMMOTDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2
# 
# 
# * FIMCOG_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMCOG_indep1 Seizure_YN FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2
# 
# 
# * DrivingStatus_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit TransModenew1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2, or  
# 
# 
# * Depression_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic depression_fann1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Anxiety_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit GAD7TOTnew1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMMOT_yr1 
# 
# reg PARTSummary1 Seizure_YN i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2 FIMMOT_indep1 
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMCOG_yr1
# 
# reg PARTSummary1 Seizure_YN i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  FIMCOG_indep1
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + DrivingStatus_yr1 
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.TransModenew1 
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Depression_yr1
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  depression_fann1
# 
# 
# * (Mediation testing) Participation_yr1 ~ Seizure_YN + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Anxiety_yr1
# 
# reg PARTSummary1 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.GAD7TOTnew1
# 
# 
# * SWLS_yr1 ~ Seizure_YN + Participation_yr1 + Depression_yr1 + Anxiety_yr1 + FIM_discharge + employment_yr1 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit SWLSTOTnew1 Seizure_YN PARTSummary1 depression_fann1 GAD7TOTnew1 FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF1 PROBLEMUsenew1 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# 
# *** Year-2 models
# 
# * FIMMOT_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMMOT_indep2 Seizure_YN FIMMOTDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * FIMCOG_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic FIMCOG_indep2 Seizure_YN FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * DrivingStatus_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit TransModenew2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Depression_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# logistic depression_fann2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * Anxiety_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  
# 
# ologit GAD7TOTnew2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2 PARTSummary1
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMMOT_yr2 + Participation_yr1 
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  FIMMOT_indep2 PARTSummary1 
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + FIMCOG_yr2 + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  FIMCOG_indep2 PARTSummary1
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + DrivingStatus_yr2 + Participation_yr1 
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.TransModenew2 PARTSummary1 
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Depression_yr2 + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  depression_fann2 PARTSummary1
# 
# 
# * (Mediation testing) Participation_yr2 ~ Seizure_YN + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence  + Anxiety_yr2 + Participation_yr1
# 
# reg PARTSummary2 Seizure_YN FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2  i.GAD7TOTnew2 PARTSummary1
# 
# 
# * SWLS_yr2 ~ Seizure_YN + Participation_yr2 + Depression_yr2 + Anxiety_yr2 + FIM_discharge + employment_yr2 + SA_preinjury + AGE + Sexnew + DOD_Injury_severity + Education + Residence + SWLS_yr1  
# 
# ologit SWLSTOTnew2 Seizure_YN PARTSummary2 depression_fann2 GAD7TOTnew2 FIMMOTDRasch FIMCOGDRasch i.DRS_PIEmpF2 PROBLEMUsenew2 AGE Sexnew DOD_Injury_severity EduYearsdummy1 EduYearsdummy2 ResDisdummy1 ResDisdummy2 i.SWLSTOTnew1  

