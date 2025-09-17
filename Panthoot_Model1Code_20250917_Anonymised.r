# Analyses code for the Manuscript "Sex specific flexible use of pant-hoot vocalizations in wild Eastern chimpanzees"
# Authors: 
# Contact:

# Files needed: 
# PanthootData_Model1&2_20250813_Anonymised.csv

#### Set working directory as appropriate
#setwd("C:/YOUR/FILE/DIRECTORY")

#install packages if necesary
#install.packages(c("EloRating","dplyr", "lubridate", "plot3D", "dplyr", "lme4", "tidyverse", "performance", "readxl", "car", "effects","interactions", "ggpubr", "gdata","ggplot2", "emmeans", "jtools", "ggdist", "DHARMa","gtable"))

# load packages if necesary (many Will already have been loaded if dominance hierarchy code run first)
library(EloRating);
library(dplyr); 
library(lubridate); 
library(plot3D); 
library(dplyr); 
library(lme4); 
library(tidyverse); 
library(performance); 
library(readxl); 
library(car); 
library(effects);
library(interactions); 
library(ggpubr); 
library(gdata);
library(ggplot2); 
library(emmeans); 
library(jtools); 
library(ggdist);
library(ggdist);
library(DHARMa);
library(see);
library(gtable);

############################################

#IMPORT DATA AND PREPARE FOR MODEL 1

############################################
#Amount of data summary stats
# Import data: 
ImportedData <- read.csv("PanthootData_Model1&2_20250813_Anonymised.csv") 
ImportedDataNumberPHs <- ImportedData[, c("Community", "ChimpID", "Sex", "FAI", "Duration_Hrs", "Number_Recieved", "Number_Produced", "To_remove", "Age")]
View(ImportedDataNumberPHs)

# Check number rows:
nrow(ImportedDataNumberPHs) # Should be N = 206

# Check representation of IDs:
table(ImportedDataNumberPHs$ChimpID)

# Format variables: 
ImportedDataNumberPHs$Community <- as.factor(ImportedDataNumberPHs$Community)
ImportedDataNumberPHs$ChimpID  <- as.factor(ImportedDataNumberPHs$ChimpID) 
ImportedDataNumberPHs$Sex <- as.factor(ImportedDataNumberPHs$Sex)
ImportedDataNumberPHs$FAI <- as.numeric(ImportedDataNumberPHs$FAI)
ImportedDataNumberPHs$Duration_Hrs <- as.numeric(ImportedDataNumberPHs$Duration_Hrs)
ImportedDataNumberPHs$Number_Recieved <- as.numeric(ImportedDataNumberPHs$Number_Recieved)
ImportedDataNumberPHs$Number_Produced <- as.numeric(ImportedDataNumberPHs$Number_Produced)
ImportedDataNumberPHs$Age <- as.factor(ImportedDataNumberPHs$Age)

# Set reference levels for categorical variables:
ImportedDataNumberPHs$Sex <- relevel(ImportedDataNumberPHs$Sex, ref = "m")
ImportedDataNumberPHs$Community <- relevel(ImportedDataNumberPHs$Community, ref = "sonso")

# Create new category incorporating sex and community variables:
ImportedDataNumberPHs$SexCommunity <- paste(ImportedDataNumberPHs$Sex, ImportedDataNumberPHs$Community)
ImportedDataNumberPHs$SexCommunity <- as.factor(ImportedDataNumberPHs$SexCommunity)

# Describe structure of variables (check): 
str(ImportedDataNumberPHs$Community) # Factor w/ 2 levels "sonso","waibira"
str(ImportedDataNumberPHs$ChimpID) # Factor w/ 46 levels
str(ImportedDataNumberPHs$Sex) # Factor w/ 2 levels "m","f"
str(ImportedDataNumberPHs$SexCommunity) # factor with 3 levels "f sonso","f waibira", "m waibira"
str(ImportedDataNumberPHs$FAI) # Number
str(ImportedDataNumberPHs$Duration_Hrs) # Number
str(ImportedDataNumberPHs$Number_Recieved) # Number
str(ImportedDataNumberPHs$Number_Produced) # Number
str(ImportedDataNumberPHs$Age) # factor

# Summary of number of data points and descriptive statistics for total time of focal follows split by sex & community
dataframe=ImportedDataNumberPHs
dataframe$Group1=ImportedDataNumberPHs$SexCommunity
dataframe$dependent=ImportedDataNumberPHs$Duration_Hrs
DurationTotalBySexCommuity<- dataframe %>% group_by(Group1) %>%  summarise(NumberFollows = n(), SumDuration = sum(dependent),)
View(DurationTotalBySexCommuity) #should be  Male: N (focal follow number) = 59, total hours focal follow i.e. 'sum' = 171.35 hrs ; female sonso : N = 29, sum = 165.1 hrs; female waibira n=118, sum = 279.1833 

# Number of IDs: 
IDsbySexCommunity <- ImportedDataNumberPHs %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(SexCommunity) %>%
     summarise(NumberChimps = n())
IDsbySexCommunity # Should be: 10 sonso females; 20 waibira females; 16 waibira males


#################
# Model 1: Are there sex differences in the rate of pant-hoot production? ----
## Data preparation: ----
# Import data: 

# Remove data to exclude from analyses (one focal from TIB with duration =0 , and one focal from  ST2 who was unidentified female)
NumberPHs_femVmal<-subset(ImportedDataNumberPHs, To_remove!="y")

# Apply 40minute minimum (i.e. 2/3 of an hour) focal follow criteria:
NumberPHs_femVmal<-subset(NumberPHs_femVmal, Duration_Hrs>(2/3))

# Check number rows after focal follow removal:
nrow(NumberPHs_femVmal) # Should be N = 169

# Check representation of IDs:
table(NumberPHs_femVmal$ChimpID)

#create variable number produced per hr for sumamry stats. 
NumberPHs_femVmal$ProducedPerHr <- NumberPHs_femVmal$Number_Produced/NumberPHs_femVmal$Duration_Hrs

# Summary of number of data points and descriptive statistics for number pant-hoots produced (mean, SD, min, max, median, quartiles, ...):
dataframe=NumberPHs_femVmal
dataframe$dependent1=NumberPHs_femVmal$Number_Produced
dataframe$dependent2=NumberPHs_femVmal$ProducedPerHr
dataframe$Group1=NumberPHs_femVmal$Sex
SummaryNumProdFemVMal<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent1[!is.na(dependent1)]), 
															MeanNumberProduced = mean(dependent1, na.rm=TRUE), 
															sdNumProd = sd(dependent1, na.rm=TRUE), 
															minNumProd = min(dependent1, na.rm=TRUE),
															maxNumProd = max(dependent1, na.rm=TRUE), 
															medianNumProd = median(dependent1, na.rm=TRUE), 
															qoneNumProd = quantile(dependent1, .25, na.rm=TRUE), 
															qthreeNumProd = quantile(dependent1, .75, na.rm=TRUE),
															MeanProducedPerHr = mean(dependent2, na.rm=TRUE), 
															sdProdPerHr = sd(dependent2, na.rm=TRUE), 
															minProdPerHr = min(dependent2, na.rm=TRUE),
															maxProdPerHr = max(dependent2, na.rm=TRUE), 
															medianProdPerHr = median(dependent2, na.rm=TRUE), 
															qoneProdPerHr = quantile(dependent2, .25, na.rm=TRUE), 
															qthreeProdPerHr = quantile(dependent2, .75, na.rm=TRUE)
															)
															
SummaryNumProdFemVMal$sterrNumProd <- SummaryNumProdFemVMal$sdNumProd/(sqrt(SummaryNumProdFemVMal$NumberFocalFollows))
SummaryNumProdFemVMal$sterrProdPerHr <- SummaryNumProdFemVMal$sdProdPerHr/(sqrt(SummaryNumProdFemVMal$NumberFocalFollows))
View(SummaryNumProdFemVMal)    
#For number follows should be  Male: Number follows = 51, female: N = 118, 
#For number produced should be  Male mean number PHs produced = 5.5; female mean number PHs produced = 1.8 
#For produced per hour should be  Male mean PH per hour = 1.55; female mean PH per hr = 0.54


# Summary of mean number pant-hoots produced per hour per ID; to then take means & SD for descriptives:
dataframe=NumberPHs_femVmal
dataframe$dependent=NumberPHs_femVmal$ProducedPerHr
dataframe$Group1=NumberPHs_femVmal$ChimpID
SummaryNumProdFemVMalByChimp<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent[!is.na(dependent)]), 
															MeanProducedPerHr = mean(dependent, na.rm=TRUE), 
															Sex = first(Sex)
															)
# Split values by sex
SummaryNumProdFemVMalMByChimp <-subset(SummaryNumProdFemVMalByChimp, Sex == "m")
SummaryNumProdFemVMalFByChimp <-subset(SummaryNumProdFemVMalByChimp, Sex == "f")
FMeanByChimp <- mean(SummaryNumProdFemVMalFByChimp$MeanProducedPerHr)
MMeanByChimp <- mean(SummaryNumProdFemVMalMByChimp$MeanProducedPerHr)
MSDByChimp <-  sd(SummaryNumProdFemVMalMByChimp$MeanProducedPerHr)
FSDByChimp <-  sd(SummaryNumProdFemVMalFByChimp$MeanProducedPerHr)
FSterrByChimp <- MSDByChimp/sqrt(15)
MSterrByChimp <- FSDByChimp/sqrt(29)

#summary dataframe for duration of focal follows by sex
dataframe$dependent=NumberPHs_femVmal$Duration_Hrs
SummaryHrsFemVMal<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), Sum = sum(dependent),)
View(SummaryHrsFemVMal) #should be  Male: Number follows = 51, total hours focal follow i.e. sum = 168.6 hrs ; female: N = 118, sum = 433/1 hrs 

# Number of IDs: 
IDsbySex<- NumberPHs_femVmal %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(Sex) %>%
     summarise(count = n())
IDsbySex # Should be: 29 females; 15 males

# Check outcome variable distribution: 
plot(table(NumberPHs_femVmal$Number_Produced))

# Check representation of IDs (by num recieved and duration of follows):
plot(NumberPHs_femVmal$ChimpID, NumberPHs_femVmal$Duration_Hrs) 
plot(NumberPHs_femVmal$ChimpID, NumberPHs_femVmal$Number_Recieved) 
plot(NumberPHs_femVmal$ChimpID, NumberPHs_femVmal$FAI) 

# Check variation of num recieved, duration and FAI by Sex: 
plot(NumberPHs_femVmal$Sex,NumberPHs_femVmal$Number_Recieved) 
plot(NumberPHs_femVmal$Sex,NumberPHs_femVmal$Duration_Hrs)
plot(NumberPHs_femVmal$Sex,NumberPHs_femVmal$FAI)


################

# Check range of focal follow duration: 
range(NumberPHs_femVmal$Duration_Hrs)

# Standardise/center numeric predictors:
Number_RecievedMean <- mean(NumberPHs_femVmal$Number_Recieved)
Number_RecievedSD <- sd(NumberPHs_femVmal$Number_Recieved)
NumberPHs_femVmal$Number_RecievedCentered <-(NumberPHs_femVmal$Number_Recieved-Number_RecievedMean )/Number_RecievedSD 

FAI_Mean <- mean(NumberPHs_femVmal$FAI)
FAI_SD <- sd(NumberPHs_femVmal$FAI)
NumberPHs_femVmal$FAI_Centered <-(NumberPHs_femVmal$FAI-FAI_Mean)/FAI_SD 

############################################

#RUN MODEL 1

############################################

## Model 1: ----
# Control function for models:
contr=glmerControl(optCtrl = list(maxfun=10000))

# Poisson model:
NProd_Sex_Pois=glmer(Number_Produced ~ Sex*Number_RecievedCentered + FAI_Centered 
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID) + (1|Age),
	family=poisson, data=NumberPHs_femVmal)
summary(NProd_Sex_Pois)

# Model checks: NProd_Sex_NoMin_Pois
# Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Sex_Pois) 
check_overdispersion(NProd_Sex_Pois) # Over-dispersion detected: dispersion ratio =  1.985506.007; Pearson's Chi-Squared = 321.652; p-value = < 0.001

# Negative binomial model to address zero inflation:
NProd_Sex_NB=glmer.nb(Number_Produced ~ Sex*Number_RecievedCentered + FAI_Centered 
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID) + (1|Age), nAGQ = 0,
	data=NumberPHs_femVmal)
summary(NProd_Sex_NB)	

##############

#FINAL MODEL 1

##############
# Model after removing non-significant interactions:
NProd_Sex_NB.2=glmer.nb(Number_Produced ~ Sex + Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID) + (1|Age), nAGQ = 0,
	data=NumberPHs_femVmal)
summary(NProd_Sex_NB.2)	

##############

#MODEL 1 CHECKS

##############
# Model checks: NProd_Sex_NB.2
# Over-dispersion: 
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Sex_NB.2) #dispersion parameter close to 1, no concern
check_overdispersion(NProd_Sex_NB.2) #dispersion ratio = 0.471      p-value = 0.216 No overdispersion detected.

# Collinearity:
ranef.diagn.plot(NProd_Sex_NB.2)
check_collinearity(NProd_Sex_NB.2)
NProd_Sex_NB.2_colinearitycheck <-  check_collinearity(NProd_Sex_NB.2) #max VIF =  1.34, no concern

# General checks:
check_model_NProd_Sex_NB.2 <- check_model(NProd_Sex_NB.2) 
check_model_NProd_Sex_NB.2 
check_outliers_NProd_Sex_NB.2 <- check_outliers(NProd_Sex_NB.2)
check_outliers_NProd_Sex_NB.2  # no outliers detected

# Stability:
source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
full.stab=glmm.model.stab(model.res=NProd_Sex_NB.2, data=NumberPHs_femVmal,contr=contr)
# check for convergence issues these are indicated separately for lme4 (column lme4.warnings) and the optimizer opt.warnings): 
table(full.stab$detailed$lme4.warnings)
table(full.stab$detailed$opt.warnings)

M1x<-round(full.stab$summary[, -1], 3) 
m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_NProd_Sex_NB.2 <- m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_NProd_Sex_NB.2

### Final Model 1 output: ----
Summary_NProd_Sex_NB.2 <- summary(NProd_Sex_NB.2)

# Results of individual predictors to extract p-values and significance test):
SigTests_NProd_Sex_NB.2<-as.data.frame(drop1(NProd_Sex_NB.2, test="Chisq"))
SigTests_NProd_Sex_NB.2<-round(SigTests_NProd_Sex_NB.2, 3)
SigTests_NProd_Sex_NB.2

# Contribution of effects: 
summary(NProd_Sex_NB.2)$varcor 
# Number of levels per grouping:
summary(NProd_Sex_NB.2)$ngrps
# Extract estimate:
Estimates_NProd_Sex_NB.2 <- round(summary(NProd_Sex_NB.2)$coefficients, 3)
Estimates_NProd_Sex_NB.2

# Negative binomial null model: 
NProd_Sex_NoMin_NB_Null=glmer.nb(Number_Produced ~ FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID)+ (1|Age), nAGQ = 0,
	data=NumberPHs_femVmal)
#summary(NProd_Sex_NB_Null)	
# Compare full model with null model:
NullCompare_NProd_Sex_NB.2 <- as.data.frame(anova(NProd_Sex_NB.2, NProd_Sex_NoMin_NB_Null, test="Chisq")) 
NullCompare_NProd_Sex_NB.2   # Chisq=  42.08 ; DF= 2 ; Pr(>Chisq):7.273428e-10
NullCompare_NProd_Sex_NB.2Dataframe <- as.data.frame(NullCompare_NProd_Sex_NB.2)

# Conditional and Marginal R squared:
ModelPerformance_NProd_Sex_NB.2 <- model_performance(NProd_Sex_NB.2) 
ModelPerformance_NProd_Sex_NB.2 # R^2 cond =  0.400, R^2 marg = 0.355
ModelPerformance_NProd_Sex_NB.2Dataframe <- as.data.frame(ModelPerformance_NProd_Sex_NB.2)


# Model complexity (calculate number of observations per estimated item):
length(residuals(NProd_Sex_NB.2))/
  (length(fixef(NProd_Sex_NB.2))+
     nrow(as.data.frame(summary(NProd_Sex_NB.2)$varcor)))

Sexeffects <- effect(NProd_Sex_NB.2, term = "Sex")
print(Sexeffects)

# Confidence intervals:
CI_NProd_Sex_NB.2 <- confint(NProd_Sex_NB.2, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
CI_NProd_Sex_NB.2

#library(readr)
#CI_NProd_Sex_NB.2<- read_csv("Model1_ConfInts.csv")


plot(allEffects(NProd_Sex_NB.2))


##############

#MODEL 1 SAVE OUTPUTS

##############
#Save outputs to new location
#Set location as relavent where prefered
#setwd("C:/YOUR/DIRECTORY")

#Data collected summaries
DataCollected_NumFollowsPerChimpID <- as.data.frame (table(ImportedDataNumberPHs$ChimpID))
write.csv(DataCollected_NumFollowsPerChimpID, file = "DataCollected_NumFollowsPerChimpID.csv")

write.csv(IDsbySexCommunity, file = "DataCollected_NumberChimpsPerSexCommunity.csv")
write.csv(DurationTotalBySexCommuity, file = "DataCollected_DurationFollowsPerSexCommunity.csv")

#Data used in model 1 summaries
DataUsed_Model1_NumFollowsPerChimpID <- as.data.frame (table(NumberPHs_femVmal$ChimpID))
write.csv(DataUsed_Model1_NumFollowsPerChimpID, file = "DataUsed_Model1_NumFollowsPerChimpID.csv")

write.csv(SummaryNumProdFemVMal, file = "DataUsed_Model1_NumberProducedSummary.csv")
write.csv(SummaryHrsFemVMal, file = "DataUsed_Model1_DurationFollowsPerSex.csv")
write.csv(IDsbySex, file = "DataUsed_Model1_NumberChimpsPerSex.csv")

#Save outputs to new location
#setwd("C:/YOUR/DIRECTORY")

#Save data descriptives
write.csv(SummaryNumProdFemVMal, file = "Model1_SummaryNumProd.csv")

#Save sample sizes table 
SampleSizes_NumberPHs_femVmal <- as.data.frame (table(NumberPHs_femVmal$SexCommunity))
write.csv(SampleSizes_NumberPHs_femVmal, file = "Model1_SampleSizes.csv")

#Save Data contributeded by sex
write.csv(IDsbySexCommunity , file = "Model1_IDsbySexCommunity.csv")

#Save results of individual predictors to extract p-values and significance test
write.csv(SigTests_NProd_Sex_NB.2 , file = "Model1_SigTests.csv")
 
#Save results of null model comparison 
write.csv(NullCompare_NProd_Sex_NB.2Dataframe , file = "Model1_NullComparison.csv")
 
#Save model estimates
write.csv(Estimates_NProd_Sex_NB.2, file = "Model1_Estimates.csv")

#Save model performance
write.csv(ModelPerformance_NProd_Sex_NB.2Dataframe , file = "Model1_Performance.csv")

#Save model CIs
write.csv(CI_NProd_Sex_NB.2, file = "Model1_ConfInts.csv")

#___________________________________________________
#Save text files
# Open a connection to a text file, save text from final model summary, overdisp tests, colinearity and VIF values
sink("Model1_SummaryOutput_NProd.txt")
print("Model1 Summary")
print("summary(NProd_Sex_NB.2)")
print(summary(NProd_Sex_NB.2))
print("____________________________________________________________________________")
print("Model1 Overdispersion test output")
print("from source diagnostic_fcns.r")
print("overdisp.test(NProd_Sex_NB.2)")
print(overdisp.test(NProd_Sex_NB.2))
print("check_overdispersion(NProd_Sex_NB.2)")
print(check_overdispersion(NProd_Sex_NB.2))
print("____________________________________________________________________________")
print("Model1 Colinearity check output")
print("NProd_Sex_NB.2_colinearitycheck")
print(NProd_Sex_NB.2_colinearitycheck)
print("____________________________________________________________________________")
print("Model1 Outliers check output")
print("check_outliers_NProd_Sex_NB.2")
print(check_outliers_NProd_Sex_NB.2)
print("____________________________________________________________________________")
print("Model1 VarCor summary")
print("summary(NProd_Sex_NB.2)$varcor ")
print(summary(NProd_Sex_NB.2)$varcor)
sink()

#___________________________________________________
#Save figures

# Save colinearity plot as a PNG file
png("Model1_RanefDiagnPlot.png")
ranef.diagn.plot(NProd_Sex_NB.2)
dev.off()

# Save model checks plot as a PNG file
png("Model1_ModelCheckPlot.png")
check_model_NProd_Sex_NB.2
dev.off()

#Save stability plot as PNG file
png("Model1_StabilityPlot.png")
m.stab.plot(full.stab$summary[, -1]) 
dev.off()

# Save summary plots of effects as PNG file
png("Model1_AllEffectsPlot.png")
plot(allEffects(NProd_Sex_NB.2))
dev.off()

##############
##############

#MODEL 1 PLOTS

##############
# Plot Figure 1 (v2):
# Get values: 
Sexeffects <- effect(NProd_Sex_NB.2, term = "Sex", offset=1)
SexeffectsSummary <- summary(Sexeffects)

SexeffectsEffect<- data.frame(SexeffectsSummary$effect)
SexeffectsOffset<- data.frame(SexeffectsSummary$offset)
SexeffectsLower<- data.frame(SexeffectsSummary$lower)
SexeffectsUpper<- data.frame(SexeffectsSummary$upper)
SexeffectsEffect$SexeffectsSummary.effect <- as.numeric(SexeffectsEffect$SexeffectsSummary.effect)
SexeffectsOffset$SexeffectsSummary.offset <- as.numeric(SexeffectsOffset$SexeffectsSummary.offset)
SexeffectsLower$SexeffectsSummary.lower <- as.numeric(SexeffectsLower$SexeffectsSummary.lower)
SexeffectsUpper$SexeffectsSummary.upper <- as.numeric(SexeffectsUpper$SexeffectsSummary.upper)

# Split values for points by group:
NumberPHs_femVmalM <-subset(NumberPHs_femVmal, Sex == "m")
NumberPHs_femVmalF <-subset(NumberPHs_femVmal, Sex == "f")
NumberPHs_femVmalFSonso <-subset(NumberPHs_femVmalF , Community == "sonso")
NumberPHs_femVmalFWaibira <-subset(NumberPHs_femVmalF, Community == "waibira")

YpointsM <-as.numeric(((NumberPHs_femVmal$ProducedPerHr)*SexeffectsOffset$SexeffectsSummary.offset ))
YpointsF <-as.numeric(((NumberPHs_femVmal$ProducedPerHr)*SexeffectsOffset$SexeffectsSummary.offset ))

# Create Y-axis points values: 
YpointsM <-NumberPHs_femVmalM$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsF <-NumberPHs_femVmalF$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsFSonso <-NumberPHs_femVmalFSonso$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsFWaibira <-NumberPHs_femVmalFWaibira$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset

# Create X-axis points values:
# Create ID number (for jitters):
MLength <- nrow(NumberPHs_femVmalM) 
FLength <- nrow(NumberPHs_femVmalF)
FLengthSonso <- nrow(NumberPHs_femVmalFSonso)
FLengthWaibira <- nrow(NumberPHs_femVmalFWaibira)
NumberPHs_femVmalM$JitterIDnum <- c(1:MLength)
NumberPHs_femVmalF$JitterIDnum <- c(1:FLength)
NumberPHs_femVmalFSonso$JitterIDnum <- c(1:FLengthSonso)
NumberPHs_femVmalFWaibira$JitterIDnum <- c(1:FLengthWaibira)

# Make jitter constant for points X-axis values:
Mxaxisconstant <- 1/MLength
Fxaxisconstant <- 1/FLength
FxaxisconstantSonso <- 1/FLengthSonso
FxaxisconstantWaibira <- 1/FLengthWaibira

# X-axis values:
XpointsM <-( 3+((NumberPHs_femVmalM$JitterIDnum)*Mxaxisconstant)-Mxaxisconstant)
XpointsF <-( 1+((NumberPHs_femVmalF$JitterIDnum)*Fxaxisconstant)-Fxaxisconstant)
XpointsFSonso <-( 1+((NumberPHs_femVmalFSonso$JitterIDnum)*FxaxisconstantSonso)-FxaxisconstantSonso)
XpointsFWaibira <-( 1+((NumberPHs_femVmalFWaibira$JitterIDnum)*FxaxisconstantWaibira)-FxaxisconstantWaibira)

# Make values for horizontal bars for model estimates and CI limits:
MEst <- SexeffectsEffect$SexeffectsSummary.effect[1]
MLower1 <- SexeffectsLower$SexeffectsSummary.lower[1]
MUpper1 <- SexeffectsUpper$SexeffectsSummary.upper[1]

FEst <- SexeffectsEffect$SexeffectsSummary.effect[2]
FLower1 <- SexeffectsLower$SexeffectsSummary.lower[2]
FUpper1 <- SexeffectsUpper$SexeffectsSummary.upper[2]

#sterr values for means used to create 95% confidence interval around mean (i.e. sterr *1.96)
F95ByChimp <- FSterrByChimp * 1.96
M95ByChimp <- MSterrByChimp *1.96

FLower2 <- FMeanByChimp - F95ByChimp
FUpper2 <- FMeanByChimp + F95ByChimp
MLower2 <- MMeanByChimp - M95ByChimp
MUpper2 <- MMeanByChimp + M95ByChimp

# Axes values:
ylimit=range(c(0,8))
xlimit=range(c(0,5))
ylabel="Number pant-hoots produced per hour" 
xlabel="Sex"
axiscrossX=0
axiscrossY=0

par(mar=c(3.5, 3.5, 0.5, 0.5),mgp=c(1.25, 1, 0)) # set margins (mar) and distance of axes labels/tick lables (mgp)
plot(x=1, y=1, xlab="", ylab=ylabel, type="n", axes=F,  xlim=xlimit, ylim=ylimit)
axis(side=2, pos=axiscrossY, las=1) # y-axes sides and cross point
axis(side=1, pos=axiscrossX, tck=0, labels=FALSE) # x-axis

# Add X-axis label and sex labels:
mtext(text=xlabel, at=c(2.5), line=0.6, side=1, cex=1.2)
mtext(text=c("Female", "Male"), at=c(1.5, 3.5), line=-0.4, side=1, cex=1)

# Add data points:
points(x=XpointsM, y=YpointsM, pch=19, cex=.7, col="#6BC5A4")
points(x=XpointsM, y=YpointsM, pch=21, cex=.7, col="#3A9273")
points(x=XpointsFWaibira, y=YpointsFWaibira, pch=19, cex=.7, col="#FEAE98")
points(x=XpointsFWaibira, y=YpointsFWaibira, pch=21, cex=.7, col="#FE8969")
points(x=XpointsFSonso, y=YpointsFSonso, pch=19, cex=.7, col="#49B7FC")
points(x=XpointsFSonso, y=YpointsFSonso, pch=21, cex=.7, col="#0490E6")

# Add horizontal bars for estimates:
segments(x0=0.9, x1=1.9, y0=FEst, y1=FEst, lwd=2, lend=1)
segments(x0=2.9, x1=3.9, y0=MEst, y1=MEst, lwd=2, lend=1)

# Add max and min bars:
xcoordf=1.4
xcoordm= 3.4
arrows(xcoordf, FLower1, xcoordf, FUpper1, code= 3, length=0.05, angle=90)
arrows(xcoordm, MLower1, xcoordm, MUpper1, code= 3, length=0.05, angle=90)

# Add horizontal bars for mean:
segments(x0=1.1, x1=2.1, y0=FMeanByChimp, y1=FMeanByChimp, lwd=2, lend=1, lty='43')
segments(x0=3.1, x1=4.1, y0=MMeanByChimp, y1=MMeanByChimp, lwd=2, lend=1, lty='43')


# Add max and min bars:
xcoordf=1.6
xcoordm= 3.6

arrows(xcoordf, FLower2, xcoordf, FUpper2, code= 3, length=0.05, angle=90, lty='33')
arrows(xcoordm, MLower2, xcoordm, MUpper2, code= 3, length=0.05, angle=90, lty='33')

Model1_FinalPlot <- recordPlot()


# Save plot as PNG file
png("Model1_FinalPlot.png")
Model1_FinalPlot 
dev.off()

##############

#MODEL 1 supplmentary Plot

##############
# Get values: 
Sexeffects <- effect(NProd_Sex_NB.2, term = "Sex", offset=1)
SexeffectsSummary <- summary(Sexeffects)

SexeffectsEffect<- data.frame(SexeffectsSummary$effect)
SexeffectsOffset<- data.frame(SexeffectsSummary$offset)
SexeffectsLower<- data.frame(SexeffectsSummary$lower)
SexeffectsUpper<- data.frame(SexeffectsSummary$upper)
SexeffectsEffect$SexeffectsSummary.effect <- as.numeric(SexeffectsEffect$SexeffectsSummary.effect)
SexeffectsOffset$SexeffectsSummary.offset <- as.numeric(SexeffectsOffset$SexeffectsSummary.offset)
SexeffectsLower$SexeffectsSummary.lower <- as.numeric(SexeffectsLower$SexeffectsSummary.lower)
SexeffectsUpper$SexeffectsSummary.upper <- as.numeric(SexeffectsUpper$SexeffectsSummary.upper)

# Split values for points by group:
NumberPHs_femVmalM <-subset(NumberPHs_femVmal, Sex == "m")
NumberPHs_femVmalF <-subset(NumberPHs_femVmal, Sex == "f")
NumberPHs_femVmalFSonso <-subset(NumberPHs_femVmalF , Community == "sonso")
NumberPHs_femVmalFWaibira <-subset(NumberPHs_femVmalF, Community == "waibira")

YpointsM <-as.numeric(((NumberPHs_femVmal$ProducedPerHr)*SexeffectsOffset$SexeffectsSummary.offset ))
YpointsF <-as.numeric(((NumberPHs_femVmal$ProducedPerHr)*SexeffectsOffset$SexeffectsSummary.offset ))

# Create Y-axis points values: 
YpointsM <-NumberPHs_femVmalM$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsF <-NumberPHs_femVmalF$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsFSonso <-NumberPHs_femVmalFSonso$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsFWaibira <-NumberPHs_femVmalFWaibira$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset

# Create X-axis points values:
# Create ID number (for jitters):
MLength <- nrow(NumberPHs_femVmalM) 
FLength <- nrow(NumberPHs_femVmalF)
FLengthSonso <- nrow(NumberPHs_femVmalFSonso)
FLengthWaibira <- nrow(NumberPHs_femVmalFWaibira)
NumberPHs_femVmalM$JitterIDnum <- c(1:MLength)
NumberPHs_femVmalF$JitterIDnum <- c(1:FLength)
NumberPHs_femVmalFSonso$JitterIDnum <- c(1:FLengthSonso)
NumberPHs_femVmalFWaibira$JitterIDnum <- c(1:FLengthWaibira)

# Make jitter constant for points X-axis values:
Mxaxisconstant <- 1/MLength
Fxaxisconstant <- 1/FLength
FxaxisconstantSonso <- 1/FLengthSonso
FxaxisconstantWaibira <- 1/FLengthWaibira

# X-axis values:
XpointsM <-( 3+((NumberPHs_femVmalM$JitterIDnum)*Mxaxisconstant)-Mxaxisconstant)
XpointsF <-( 1+((NumberPHs_femVmalF$JitterIDnum)*Fxaxisconstant)-Fxaxisconstant)
XpointsFSonso <-( 1+((NumberPHs_femVmalFSonso$JitterIDnum)*FxaxisconstantSonso)-FxaxisconstantSonso)
XpointsFWaibira <-( 1+((NumberPHs_femVmalFWaibira$JitterIDnum)*FxaxisconstantWaibira)-FxaxisconstantWaibira)

# Make values for horizontal bars for model estimates and CI limits:
MEst <- SexeffectsEffect$SexeffectsSummary.effect[1]
MLower1 <- SexeffectsLower$SexeffectsSummary.lower[1]
MUpper1 <- SexeffectsUpper$SexeffectsSummary.upper[1]

FEst <- SexeffectsEffect$SexeffectsSummary.effect[2]
FLower1 <- SexeffectsLower$SexeffectsSummary.lower[2]
FUpper1 <- SexeffectsUpper$SexeffectsSummary.upper[2]

#sterr values for means used to create 95% confidence interval around mean
F95ByChimp <- FSterrByChimp * 1.96
M95ByChimp <- MSterrByChimp *1.96

FLower2 <- FMeanByChimp - F95ByChimp
FUpper2 <- FMeanByChimp + F95ByChimp
MLower2 <- MMeanByChimp - M95ByChimp
MUpper2 <- MMeanByChimp + M95ByChimp


# Axes values:
ylimit=range(c(0,8))
xlimit=range(c(0,5))
ylabel="Number pant-hoots produced per hour" 
xlabel="Sex"
axiscrossX=0
axiscrossY=0

par(mar=c(3.5, 3.5, 0.5, 0.5),mgp=c(1.25, 1, 0)) # set margins (mar) and distance of axes labels/tick lables (mgp)
plot(x=1, y=1, xlab="", ylab=ylabel, type="n", axes=F,  xlim=xlimit, ylim=ylimit)
axis(side=2, pos=axiscrossY, las=1) # y-axes sides and cross point
axis(side=1, pos=axiscrossX, tck=0, labels=FALSE) # x-axis

# Add X-axis label and sex labels:
mtext(text=xlabel, at=c(2.5), line=0.6, side=1, cex=1.2)
mtext(text=c("Female", "Male"), at=c(1.5, 3.5), line=-0.4, side=1, cex=1)

# Add data points:
points(x=XpointsM, y=YpointsM, pch=19, cex=.7, col="#6BC5A4")
points(x=XpointsM, y=YpointsM, pch=21, cex=.7, col="#3A9273")
points(x=XpointsFWaibira, y=YpointsFWaibira, pch=19, cex=.7, col="#FEAE98")
points(x=XpointsFWaibira, y=YpointsFWaibira, pch=21, cex=.7, col="#FE8969")
points(x=XpointsFSonso, y=YpointsFSonso, pch=19, cex=.7, col="#49B7FC")
points(x=XpointsFSonso, y=YpointsFSonso, pch=21, cex=.7, col="#0490E6")

# Add horizontal bars for estimates:
segments(x0=0.9, x1=1.9, y0=FEst, y1=FEst, lwd=2, lend=1)
segments(x0=2.9, x1=3.9, y0=MEst, y1=MEst, lwd=2, lend=1)

# Add max and min bars:
xcoordf=1.4
xcoordm= 3.4
arrows(xcoordf, FLower1, xcoordf, FUpper1, code= 3, length=0.05, angle=90)
arrows(xcoordm, MLower1, xcoordm, MUpper1, code= 3, length=0.05, angle=90)

# Add horizontal bars for mean:
segments(x0=1.1, x1=2.1, y0=FMeanByChimp, y1=FMeanByChimp, lwd=2, lend=1, lty='43')
segments(x0=3.1, x1=4.1, y0=MMeanByChimp, y1=MMeanByChimp, lwd=2, lend=1, lty='43')


# Add max and min bars:
xcoordf=1.6
xcoordm= 3.6

arrows(xcoordf, FLower2, xcoordf, FUpper2, code= 3, length=0.05, angle=90, lty='33')
arrows(xcoordm, MLower2, xcoordm, MUpper2, code= 3, length=0.05, angle=90, lty='33')

Model1_FinalSupplPlot <- recordPlot()


# Save plot as PNG file
png("Model1_FinalSupplPlot.png")
Model1_FinalSupplPlot 
dev.off()
