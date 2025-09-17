# Analyses code for the Manuscript "Sex specific flexible use of pant-hoot vocalisations in wild Eastern chimpanzees"
# Authors: 
# Contact: 

# Files needed: 
# PanthootData_Model3&4_20250812_Anonymised.csv

#### Set working directory as appropriate
#setwd("C:/YOUR/DIRECTORY")

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
library(gdata)
library(ggplot2); 
library(emmeans); 
library(jtools); 
library(ggdist);

############################################

#IMPORT DATA AND PREPARE FOR MODEL 2

############################################
#Amount of data summary stats
# Import data: 
# if running after running model 1 code, no need to take thses steps
#ImportedData <- read.csv("PanthootData_Model3&4_20250812_Anonymised.csv")
#ImportedDataNumberPHs <- ImportedData[, c("Community", "ChimpID", "Sex", "FAI", "Duration_Hrs", "Number_Recieved", "Number_Produced", "To_remove")]

# subset data to exclude male individuals:
ImportedDataNumberPHs_Fems<-subset(ImportedDataNumberPHs, Sex=="f")

# Check number rows:
nrow(ImportedDataNumberPHs_Fems) # Should be N = 147

# Check representation of IDs:
table(ImportedDataNumberPHs_Fems$ChimpID)

# Format variables: 
ImportedDataNumberPHs_Fems$Community <- as.factor(ImportedDataNumberPHs_Fems$Community)
ImportedDataNumberPHs_Fems$ChimpID  <- as.factor(ImportedDataNumberPHs_Fems$ChimpID) 
ImportedDataNumberPHs_Fems$Sex <- as.factor(ImportedDataNumberPHs_Fems$Sex)
ImportedDataNumberPHs_Fems$FAI <- as.numeric(ImportedDataNumberPHs_Fems$FAI)
ImportedDataNumberPHs_Fems$Duration_Hrs <- as.numeric(ImportedDataNumberPHs_Fems$Duration_Hrs)
ImportedDataNumberPHs_Fems$Number_Recieved <- as.numeric(ImportedDataNumberPHs_Fems$Number_Recieved)
ImportedDataNumberPHs_Fems$Number_Produced <- as.numeric(ImportedDataNumberPHs_Fems$Number_Produced)
ImportedDataNumberPHs_Fems$Age <- as.factor(ImportedDataNumberPHs_Fems$Age)

# Set reference levels for categorical variables:
ImportedDataNumberPHs_Fems$Community <- relevel(ImportedDataNumberPHs_Fems$Community, ref = "sonso")

# Describe structure of variables (check): 
str(ImportedDataNumberPHs_Fems$Community) # Factor w/ 2 levels "sonso","waibira"
str(ImportedDataNumberPHs_Fems$ChimpID) # Factor w/ 30 levels
str(ImportedDataNumberPHs_Fems$FAI) # Number
str(ImportedDataNumberPHs_Fems$Duration_Hrs) # Number
str(ImportedDataNumberPHs_Fems$Number_Recieved) # Number
str(ImportedDataNumberPHs_Fems$Number_Produced) # Number
str(ImportedDataNumberPHs_Fems$Age) # Number

# Summary of number of data points and descriptive statistics for total time of focal follows split by community
dataframe=ImportedDataNumberPHs_Fems
dataframe$Group1=ImportedDataNumberPHs_Fems$Community
dataframe$dependent=ImportedDataNumberPHs_Fems$Duration_Hrs
CollectedDurationTotalByCommuity<- dataframe %>% group_by(Group1) %>%  summarise(NumberFollows = n(), SumDuration = sum(dependent),)
View(CollectedDurationTotalByCommuity) #should be  female sonso : N = 29, sum = 165.1 hrs; female waibira n=118, sum = 279.2 

# Number of IDs: 
CollectedIDsbyCommunity <- ImportedDataNumberPHs_Fems %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(Community) %>%
     summarise(NumberChimps = n())
CollectedIDsbyCommunity # Should be: 10 sonso females; 20 waibira females

# Model 2: Are there community differences in female pant-hoot production frequencies? ----

# Remove data to exclude from analyses:
NumPHs_fem<-subset(ImportedDataNumberPHs_Fems, To_remove!="y")
# Create subset of data with 40minute minimum data:
NumPHs_fem<-subset(NumPHs_fem, Duration_Hrs>(2/3))

# Check number rows:
nrow(NumPHs_fem) # Should be: N = 118

# Check representation of IDs:
DataUsed_NumPHs_fem <- table(NumPHs_fem$ChimpID)
DataUsed_NumPHs_fem

#create variable number produced per hr for sumamry stats. 
NumPHs_fem$ProducedPerHr <- NumPHs_fem$Number_Produced/NumPHs_fem$Duration_Hrs

# Summary of number of data points and descriptive statistics of pant-hoots produced by community (mean, SD, min, max, median, quartiles, ...):
dataframe <- NumPHs_fem
dataframe$dependent1 <- NumPHs_fem$Number_Produced
dataframe$dependent2 <- NumPHs_fem$ProducedPerHr
dataframe$Group1 <- NumPHs_fem$Community
SummaryNumProdDatafrmFems <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent1[!is.na(dependent1)]), 
															Hours = sum(Duration_Hrs),
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


SummaryNumProdDatafrmFems$sterrNumProd <- SummaryNumProdDatafrmFems$sdNumProd/(sqrt(SummaryNumProdDatafrmFems$NumberFocalFollows))
SummaryNumProdDatafrmFems$sterrProdPerHr  <- SummaryNumProdDatafrmFems$sdProdPerHr/(sqrt(SummaryNumProdDatafrmFems$NumberFocalFollows))
View(SummaryNumProdDatafrmFems) 
#For number follows should be  Sonso: Number follows = 29, Waibira: N =89 
#For number produced should be  Sonso mean number PHs produced = 2.69; Waibira mean number PHs produced = 1.52
#For produced per hour should be  Sonso mean PH per hour = 0.58 ; Waibira mean PH per hr = 0.52


# Summary of number of data points and descriptive statistics of pant-hoots produced per hour for each individual (mean, SD):
dataframe <- NumPHs_fem
dataframe$dependent <- NumPHs_fem$ProducedPerHr
dataframe$Group1 <- NumPHs_fem$ChimpID
SummaryNumProdPerHourPerID <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent[!is.na(dependent)]), 
															MeanNumberProducedPerHr = mean(dependent, na.rm=TRUE), 
															sd = sd(dependent, na.rm=TRUE), 
															Community = first(Community),
															)
View(SummaryNumProdPerHourPerID) 

# Summary of mean amount pant-hoots per hour by community:
dataframe <- SummaryNumProdPerHourPerID
dataframe$dependent <- SummaryNumProdPerHourPerID$MeanNumberProducedPerHr
dataframe$Group1 <- SummaryNumProdPerHourPerID$Community
SummaryNumProdDatafrmFemIndividuals <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberChimpIDs = length(dependent[!is.na(dependent)]), 
															MeanNumberProducedPerHour = mean(dependent, na.rm=TRUE), 
															sd = sd(dependent, na.rm=TRUE), 
															Community = first(Community),
															)
View(SummaryNumProdDatafrmFemIndividuals) 

# Focal duration:
dataframe <- NumPHs_fem
dataframe$dependent <- NumPHs_fem$Duration_Hrs
dataframe$Group1 <- NumPHs_fem$ChimpID
SummaryHrsDatafrmFems <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(),Sum = sum(dependent),)
View(SummaryHrsDatafrmFems)

# Number of IDs: 
UsedFemIDsbyCommunity <- NumPHs_fem %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(Community) %>%
     summarise(count = n())
UsedFemIDsbyCommunity #should be 10 sonso females, 19 waibira females


# Check outcome variable distribution:
plot(table(NumPHs_fem$Number_Produced))

# Check representation of IDs (by num recieved and duration of follows):
plot(NumPHs_fem$ChimpID, NumPHs_fem$Number_Recieved) 
plot(NumPHs_fem$ChimpID, NumPHs_fem$Duration_Hrs) 
plot(NumPHs_fem$ChimpID, NumPHs_fem$FAI)

# Check variation of num recieved, duration and FAI by community: 
plot(NumPHs_fem$Community,NumPHs_fem$Number_Recieved) 
plot(NumPHs_fem$Community,NumPHs_fem$Duration_Hrs) 
plot(NumPHs_fem$Community,NumPHs_fem$FAI)

# Check range of focal follow duration: 
range(NumPHs_fem$Duration_Hrs)

# Standardise/center numeric predictors:
Number_RecievedMean <- mean(NumPHs_fem$Number_Recieved)
Number_RecievedSD <- sd(NumPHs_fem$Number_Recieved)
NumPHs_fem$Number_RecievedCentered <-(NumPHs_fem$Number_Recieved-Number_RecievedMean )/Number_RecievedSD 

FAI_Mean <- mean(NumPHs_fem$FAI)
FAI_SD <- sd(NumPHs_fem$FAI)
NumPHs_fem$FAI_Centered <-(NumPHs_fem$FAI-FAI_Mean)/FAI_SD 

############################################

#RUN MODEL 2

############################################
contr=glmerControl(optCtrl = list(maxfun=10000))

# Poisson model:
NProd_Community_Pois=glmer(Number_Produced ~ Community*Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID)+ (1|Age),
	family=poisson, data=NumPHs_fem)
	
# check Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Community_Pois) #  chisq 254.3596; df  110;   2.907674e-13    ; dispersion.parameter  2.291528
check_overdispersion(NProd_Community_Pois) # Over-dispersion detected: dispersion ratio =  2.292; Pearson's Chi-Squared = 254.360; p-value = < 0.001

# Negative binomial model to address zero inflation:
NProd_Community_NB=glmer.nb(Number_Produced ~ Community*Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID) + (1|Age), nAGQ = 0,
	data=NumPHs_fem)
summary(NProd_Community_NB)	

##############

#FINAL MODEL 2

##############
# Negative binomial model after removing non-significant interacitons:
NProd_Community_NB.2=glmer.nb(Number_Produced ~ Community + Number_RecievedCentered + FAI_Centered
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID)+ (1|Age), nAGQ = 0,
	data=NumPHs_fem)
summary(NProd_Community_NB.2)	

##############

#MODEL 2 CHECKS

##############
# Model checks: NProd_Community_NB.2
# Over-dispersion: 
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Community_NB.2) #dispersion parameter close to 1, no concern
check_overdispersion(NProd_Community_NB.2) #dispersion ratio = 0.580     p-value = 0.824 No overdispersion detected.

# Collinearity:
ranef.diagn.plot(NProd_Community_NB.2)
check_collinearity(NProd_Community_NB.2)
NProd_Community_NB.2_colinearitycheck <-  check_collinearity(NProd_Community_NB.2) #max VIF = 1.27, no concern

# General checks:
check_model_NProd_Community_NB.2 <- check_model(NProd_Community_NB.2) 
check_model_NProd_Community_NB.2 
check_outliers_NProd_Community_NB.2 <- check_outliers(NProd_Community_NB.2)
check_outliers_NProd_Community_NB.2  # no outliers detected

# Stability:
source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
full.stab=glmm.model.stab(model.res=NProd_Community_NB.2, data=NumPHs_fem,contr=contr)
# check for convergence issues these are indicated separately for lme4 (column lme4.warnings) and the optimizer opt.warnings): 
table(full.stab$detailed$lme4.warnings) 
table(full.stab$detailed$opt.warnings)

M1x<-round(full.stab$summary[, -1], 3) 
m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_NProd_Community_NB.2 <- m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_NProd_Community_NB.2

# Negative binomial null model: 
NProd_Community_NB_Null=glmer.nb(Number_Produced ~ FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID)+ (1|Age), nAGQ = 0,
	data=NumPHs_fem)
summary(NProd_Community_NB_Null)	

### Final Model 1 output: ----
Summary_NProd_Community_NB.2 <- summary(NProd_Community_NB.2)

# Results of individual predictors to extract p-values and significance test):
SigTests_NProd_Community_NB.2<-as.data.frame(drop1(NProd_Community_NB.2, test="Chisq"))
SigTests_NProd_Community_NB.2<-round(SigTests_NProd_Community_NB.2, 3)
SigTests_NProd_Community_NB.2

# Contribution of effects: 
summary(NProd_Community_NB.2)$varcor 
# Number of levels per grouping:
summary(NProd_Community_NB.2)$ngrps
# Extract estimate:
Estimates_NProd_Community_NB.2 <- round(summary(NProd_Community_NB.2)$coefficients, 3)
Estimates_NProd_Community_NB.2

# Compare full model with null model:
NullCompare_NProd_Community_NB.2 <- as.data.frame(anova(NProd_Community_NB.2, NProd_Community_NB_Null, test="Chisq")) 
NullCompare_NProd_Community_NB.2   # Chisq=  21.66609 ; DF= 2 ; Pr(>Chisq):1.973645e-05
NullCompare_NProd_Community_NB.2Dataframe <- as.data.frame(NullCompare_NProd_Community_NB.2)

# Conditional and Marginal R squared:
ModelPerformance_NProd_Community_NB.2 <- model_performance(NProd_Community_NB.2) 
ModelPerformance_NProd_Community_NB.2 # sometiems doesnt work for R values
ModelPerformance_NProd_Community_NB.2Dataframe <- as.data.frame(ModelPerformance_NProd_Community_NB.2)

#alternative method for R values if above not working
#install.packages("MuMIn")
#library(MuMIn)
r.squaredGLMM(NProd_Community_NB.2) #  Both R^2m and R^2c: delta     0.2242909 0.2720712
ModelRvalues_NProd_Community_NB.2Dataframe <- as.data.frame(r.squaredGLMM(NProd_Community_NB.2))
#write.csv(ModelRvalues_NProd_Community_NB.2Dataframe, file = "Model1_Rvalues_NProd_Sex_NB2Dataframe.csv")


# Model complexity (calculate number of observations per estimated item):
length(residuals(NProd_Community_NB.2))/
  (length(fixef(NProd_Community_NB.2))+
     nrow(as.data.frame(summary(NProd_Community_NB.2)$varcor)))

Sexeffects <- effect(NProd_Community_NB.2, term = "Community")
print(Sexeffects)

# Confidence intervals:
CI_NProd_Community_NB.2 <- confint(NProd_Community_NB.2, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
CI_NProd_Community_NB.2 

write.csv(CI_NProd_Community_NB.2, file = "Model2_CI_NProd_Community_NB.2.csv")

##############

#MODEL 2 PLOTS

##############
## Plotting: ----
# Summary plots of effects: 
plot(allEffects(NProd_Community_NB.2))
Model2_AllEffectsPlot <- plot(allEffects(NProd_Community_NB.2))
Model2_AllEffectsPlot 


##############

#MODEL 2 SAVE OUTPUTS

##############
#Save outputs to new location
#Set location as relavent where prefered
#setwd("C:/YOUR/DIRECTORY")

#Data collected summaries
CollectedDurationTotalByCommuityDataframe <- as.data.frame(CollectedDurationTotalByCommuity)
write.csv(CollectedDurationTotalByCommuityDataframe, file = "DataCollected_DurationByCommuity.csv")

CollectedIDsbyCommunityDataframe <- as.data.frame(CollectedIDsbyCommunity)
write.csv(CollectedIDsbyCommunityDataframe, file = "DataCollected_NumIDsByCommunity.csv")

#Data used summaries
write.csv(SummaryNumProdDatafrmFems, file = "DataUsed_Model2_SummaryProdDatafrmFems.csv")
write.csv(SummaryNumProdPerHourPerID, file = "DataUsed_Model2_SummaryNumProdPerHourPerID.csv")
write.csv(SummaryNumProdDatafrmFemIndividuals, file = "DataUsed_Model2_SummaryNumProdDatafrmFemIndividuals.csv")

#model output values
write.csv(SigTests_NProd_Community_NB.2, file = "Model2_SigTests.csv")
write.csv(Estimates_NProd_Community_NB.2, file = "Model2_Estimates.csv")
write.csv(NullCompare_NProd_Community_NB.2Dataframe, file = "Model2_NullCompare.csv")
write.csv(ModelPerformance_NProd_Community_NB.2Dataframe, file = "Model2_ModelPerformance.csv")
write.csv(ModelRvalues_NProd_Community_NB.2Dataframe, file = "Model2_RValues.csv")
CI_NProd_Community_NB.2Dataframe <- as.data.frame(CI_NProd_Community_NB.2)
write.csv(CI_NProd_Community_NB.2Dataframe , file = "Model2_ConfInts.csv")


#Data collected summaries
DataCollected_NumFollowsPerChimpID <- as.data.frame (table(ImportedDataNumberPHs$ChimpID))
write.csv(DataCollected_NumFollowsPerChimpID, file = "DataCollected_NumFollowsPerChimpID.csv")

write.csv(IDsbySexCommunity, file = "DataCollected_NumberChimpsPerSexCommunity.csv")
write.csv(DurationTotalBySexCommuity, file = "DataCollected_DurationFollowsPerSexCommunity.csv")

#Data used in model 2 summaries
DataUsed_Model2_NumFollowsPerChimpID <- as.data.frame (table(NumberPHs_fem$ChimpID))
write.csv(DataUsed_Model2_NumFollowsPerChimpID, file = "DataUsed_Model2_NumFollowsPerChimpID.csv")

write.csv(SummaryNumProdFem, file = "DataUsed_Model2_NumberProducedSummary.csv")
write.csv(SummaryHrsFem, file = "DataUsed_Model2_DurationFollowsPerSex.csv")
write.csv(IDsbyCommunity, file = "DataUsed_Model2_NumberChimpsPerCommunity.csv")

#___________________________________________________
#Save text files
# Open a connection to a text file, save text from final model summary, overdisp tests, colinearity and VIF values
sink("Model2_SummaryOutput_NProd.txt")
print("Model2 Summary")
print("summary(NProd_Community_NB.2)")
print(summary(NProd_Community_NB.2))
print("____________________________________________________________________________")
print("Model2 Overdispersion test output")
print("from source diagnostic_fcns.r")
print("overdisp.test(NProd_Community_NB.2)")
print(overdisp.test(NProd_Community_NB.2))
print("check_overdispersion(NProd_Community_NB.2)")
print(check_overdispersion(NProd_Community_NB.2))
print("____________________________________________________________________________")
print("Model2 Colinearity check output")
print("NProd_Community_NB.2_colinearitycheck")
print(NProd_Community_NB.2_colinearitycheck)
print("____________________________________________________________________________")
print("Model2 Outliers check output")
print("check_outliers_NProd_Community_NB.2")
print(check_outliers_NProd_Community_NB.2)
print("____________________________________________________________________________")
print("Model2 VarCor summary")
print("summary(NProd_Community_NB.2)$varcor ")
print(summary(NProd_Community_NB.2)$varcor)
sink()

#___________________________________________________
#Save figures

# Save colinearity plot as a PNG file
png("Model2_RanefDiagnPlot.png")
ranef.diagn.plot(NProd_Community_NB.2)
dev.off()

# Save model checks plot as a PNG file
png("Model2_ModelCheckPlot.png")
check_model_NProd_Community_NB.2
dev.off()

#Save stability plot as PNG file
png("Model2_StabilityPlot.png")
m.stab.plot(full.stab$summary[, -1]) 
dev.off()

# Save summary plots of effects as PNG file
png("Model2_AllEffectsPlot.png")
plot(allEffects(NProd_Community_NB.2))
dev.off()





