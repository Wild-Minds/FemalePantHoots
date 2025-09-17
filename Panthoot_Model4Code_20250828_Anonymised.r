# Analyses code for the Manuscript "Sex specific flexible use of pant-hoot vocalisations in wild Eastern chimpanzees"
# Authors: 
# Contact: 

# Files needed: 
# PanthootData_Model3&4_20250812_Anonymised

#load packages used
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

#IMPORT DATA AND PREPARE FOR MODEL 4

############################################
#### Set working directory as appropriate
#setwd("C:/YOUR/DIRECTORY")

#Model 4 Amount of data summary stats and data preparation
# Import data: 
ImportedData<- read.csv("PanthootData_Model3&4_20250812_Anonymised.csv")
colnames(ImportedData) #examine column names to decide which to retain.                   
ImportedDataPHResponsesFems <- ImportedData[, c("EventID2", "Date", "Hr","Min", "Community", "ChimpID", "FocalFollowNum","OpportunitiesToResp", "FocalPhResp", "Sex", "Age",
												"Core75", "Activity", "FoodPart","Drum", "InPartyPh", "AlphaPresent", "IndepIndivs","FocalPhResp",
												"Parity","MaleDepOffspring", "Oes4")]

# Check number rows:
nrow(ImportedDataPHResponsesFems) # Should be N = 2642

# Replace 'NotFeed' with 'notfeedripefruit'; and notripefruit with notfeedripefruit
ImportedDataPHResponsesFems$FoodPart<- ifelse(ImportedDataPHResponsesFems$FoodPart == 'NotFeed', 'NotFeedRipeFruit', ImportedDataPHResponsesFems$FoodPart)
ImportedDataPHResponsesFems$FoodPart<- ifelse(ImportedDataPHResponsesFems$FoodPart == 'NotRipeFruit', 'NotFeedRipeFruit', ImportedDataPHResponsesFems$FoodPart)

# Format variables: 
ImportedDataPHResponsesFems$ChimpID <-as.factor(ImportedDataPHResponsesFems$ChimpID)
ImportedDataPHResponsesFems$Community <-as.factor(ImportedDataPHResponsesFems$Community)
ImportedDataPHResponsesFems$FocalFollowNum <-as.factor(ImportedDataPHResponsesFems$FocalFollowNum)
ImportedDataPHResponsesFems$AlphaPresent <-as.factor(ImportedDataPHResponsesFems$AlphaPresent)
ImportedDataPHResponsesFems$InPartyPh <-as.factor(ImportedDataPHResponsesFems$InPartyPh)
ImportedDataPHResponsesFems$FoodPart <-as.factor(ImportedDataPHResponsesFems$FoodPart)
ImportedDataPHResponsesFems$Sex <-as.factor(ImportedDataPHResponsesFems$Sex)
ImportedDataPHResponsesFems$Drum <-as.factor(ImportedDataPHResponsesFems$Drum)
ImportedDataPHResponsesFems$Activity <-as.factor(ImportedDataPHResponsesFems$Activity)
ImportedDataPHResponsesFems$Parity <-as.factor(ImportedDataPHResponsesFems$Parity)
ImportedDataPHResponsesFems$MaleDepOffspring <-as.factor(ImportedDataPHResponsesFems$MaleDepOffspring)
ImportedDataPHResponsesFems$Oes4 <-as.factor(ImportedDataPHResponsesFems$Oes4)
ImportedDataPHResponsesFems$Core75 <-as.factor(ImportedDataPHResponsesFems$Core75)

ImportedDataPHResponsesFems$IndepIndivs <- as.numeric(ImportedDataPHResponsesFems$IndepIndivs)
ImportedDataPHResponsesFems$Age <-as.numeric(ImportedDataPHResponsesFems$Age)
ImportedDataPHResponsesFems$Hr <-as.numeric(ImportedDataPHResponsesFems$Hr)

#Remove rows with missing data (this will automatically subset for females as no male individuals have data in the female-only columns, e.g. Oestrus)
PHootsRecievedForModel4  <-  droplevels(na.omit(ImportedDataPHResponsesFems))
nrow(PHootsRecievedForModel4) # should be 1066

#subset for only when party size has less than 15 independent individuals (same issue as for model 3 in that low representation of large party size)
PHootsRecievedForModel4 <-subset(PHootsRecievedForModel4, IndepIndivs<15)
nrow(PHootsRecievedForModel4) # should be 1045

#Check that the data and outcome variables are balanced wrt the random and control variables:
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$ChimpID) #Most IDs well represented and most with no response have plenty datapoints
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$FocalFollowNum) #some not represented well, keep an eye on
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Hr) #okay

# Check that the responses are distributed across predictor variables:
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$AlphaPresent)   #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$InPartyPh)  #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Activity)  #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$FoodPart)  #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Parity)  #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$MaleDepOffspring)  #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Oes4)   #okay
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Core75) #okay, though only 20 reponses when not in core
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$IndepIndivs) 
plot(table(PHootsRecievedForModel4$IndepIndivs)) #okay (a bit of a tail, but still responses all the way up)

table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Community) #okay
DataForModel4_PHootsRecievedByCommunity <- table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Community) 
DataForModel4_PHootsRecievedByCommunity <- data.frame(DataForModel4_PHootsRecievedByCommunity)

# Number of 1s and 0s in the outcome overall:
table(PHootsRecievedForModel4$FocalPhResp) # 0 =  906, 1 = 139

# Check representation of IDs now data rows removed:
dataframe <- PHootsRecievedForModel4
dataframe$Group1 <- PHootsRecievedForModel4$ChimpID
DataForModel4_NumDatapointsPerChimpID <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberDataPoints = length(ChimpID),
															Community = first(Community),
															Sex = first(Sex)
															)		
View(DataForModel4_NumDatapointsPerChimpID) 

#check representation of Number data points by community 
dataframe <- PHootsRecievedForModel4
dataframe$Group1 <- PHootsRecievedForModel4$Community
DataForModel4_NumDatapointsPerCommunity <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberDataPoints = length(Community))		
View(DataForModel4_NumDatapointsPerCommunity) # should be f sonso=448; f waibira=597

# Number of IDs by community: 
DataForModel4_NumIDsPerCommunity <- PHootsRecievedForModel4 %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(Community) %>%
     summarise(NumberChimps = n())
View(DataForModel4_NumIDsPerCommunity) # Should be: 9 sonso females; 18 waibira females;
#merge prev two dataframes (for saving for descriptives)
DataForModel4_NumIDsPerCommunity$NumberDataPoints <- DataForModel4_NumDatapointsPerCommunity$NumberDataPoints

# Set reference levels for categorical variables:
PHootsRecievedForModel4$FoodPart <- relevel(PHootsRecievedForModel4$FoodPart, ref = "NotFeedRipeFruit")
PHootsRecievedForModel4$Activity <- relevel(PHootsRecievedForModel4$Activity, ref = "NotFeed")
PHootsRecievedForModel4$Community <- relevel(PHootsRecievedForModel4$Community, ref = "sonso")
PHootsRecievedForModel4$Core75 <- relevel(PHootsRecievedForModel4$Core75, ref = "No")

# Standardize numeric predictors:
IndepIndivsMean <- mean(PHootsRecievedForModel4$IndepIndivs)
IndepIndivsSD <- sd(PHootsRecievedForModel4$IndepIndivs)
PHootsRecievedForModel4$IndepIndivsCentered <- (PHootsRecievedForModel4$IndepIndivs-IndepIndivsMean)/IndepIndivsSD 

AgeMean <- mean(PHootsRecievedForModel4$Age)
AgeSD <- sd(PHootsRecievedForModel4$Age)
PHootsRecievedForModel4$AgeCentered <- (PHootsRecievedForModel4$Age-AgeMean)/AgeSD 

HrMean <- mean(PHootsRecievedForModel4$Hr)
HrSD <- sd(PHootsRecievedForModel4$Hr)
PHootsRecievedForModel4$HrCentered <- (PHootsRecievedForModel4$Hr-HrMean)/HrSD 

########################################

#RUN MODEL 4

############################################
# GLMM 4: Which individual factors predict pant-hoot responses in female chimpanzees from two communities? ----
contr=glmerControl(optCtrl = list(maxfun=100000), calc.derivs=F, optimizer = "nloptwrap")

# Full model with all interactions: 
Model4_ResponsePHsByCommunity.1=glmer(FocalPhResp ~ Community*AgeCentered  + Community*AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Community*Core75 + Community*(Activity/FoodPart)
				+ Community*Oes4 + Community*(Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.1)

#rerun without least significant interaction: Community*(Activity/FoodPart) 
Model4_ResponsePHsByCommunity.2=glmer(FocalPhResp ~ Community*AgeCentered  + Community*AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Community*Core75 + (Activity/FoodPart)
				+ Community*Oes4 + Community*(Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.2)

#rerun without least significant interaction: Community*(Parity/MaleDepOffspring) 
Model4_ResponsePHsByCommunity.3=glmer(FocalPhResp ~ Community*AgeCentered  + Community*AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Community*Core75 + (Activity/FoodPart)
				+ Community*Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.3)

#rerun without least significant interaction: Community*AlphaPresent
Model4_ResponsePHsByCommunity.4=glmer(FocalPhResp ~ Community*AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Community*Core75 + (Activity/FoodPart)
				+ Community*Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.4)

#rerun without least significant interaction: Community*AgeCentered
Model4_ResponsePHsByCommunity.5=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Community*Core75 + (Activity/FoodPart)
				+ Community*Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.5)

#rerun without least significant interaction: Community*Oes4
Model4_ResponsePHsByCommunity.6=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Community*Core75 + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.6)

#rerun without least significant interaction: Community*Core75
Model4_ResponsePHsByCommunity.7=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh 
				+ Community*Drum +  Core75 + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.7)

#rerun without least significant interaction: Community*InPartyPh
Model4_ResponsePHsByCommunity.8=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + InPartyPh + Community*Drum +  Core75 + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.8)

#rerun without least significant interaction: Community*IndepIndivsCentered
Model4_ResponsePHsByCommunity.9=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ IndepIndivsCentered  + InPartyPh + Community*Drum +  Core75 + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.9)

##############

#FINAL MODEL 4

##############
#rerun without least significant interaction: Community*Drum
Model4_ResponsePHsByCommunity.10=glmer(FocalPhResp ~ Community + AgeCentered  + AlphaPresent+ IndepIndivsCentered  + InPartyPh + Drum +  Core75 + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.10)

#Final full model = Model4_ResponsePHsByCommunity.10 as no interactions were significant

# Null Model with only control variables and random effects:
Model4_ResponsePHsByCommunityNull =glmer(FocalPhResp ~ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
				data=PHootsRecievedForModel4, family=binomial, control=contr)


##############

#MODEL 4 CHECKS

##############
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
ranef.diagn.plot(Model4_ResponsePHsByCommunity.10) #ok
check_collinearity(Model4_ResponsePHsByCommunity.10) # max VIF = 3.79 OK. 
Model4_ResponsePHsByCommunity.10_collinearitycheck <-  data.frame(check_collinearity(Model4_ResponsePHsByCommunity.10))

check_model(Model4_ResponsePHsByCommunity.10)  
check_model_Model4_ResponsePHsByCommunity.10 <- check_model(Model4_ResponsePHsByCommunity.10) 
check_model_Model4_ResponsePHsByCommunity.10
check_outliers_Model4_ResponsePHsByCommunity.10 <- check_outliers(Model4_ResponsePHsByCommunity.10)
check_outliers_Model4_ResponsePHsByCommunity.10# 4 outliers detected: cases 188, 684, 752, 932. Based on the following method and threshold: cook (0.8) For variable: (Whole model).

source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
m.stab=glmm.model.stab(model.res=Model4_ResponsePHsByCommunity.10, data=PHootsRecievedForModel4,contr=contr)
# Check for convergence issues: 
table(m.stab$detailed$lme4.warnings) 
table(m.stab$detailed$opt.warnings)
# Stability: 
M4x<-round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 
StabilityPlot_Model4_ResponsePHsByCommunity.10 <- m.stab.plot(m.stab$summary[, -1]) 

###Model perameters	
# Full model vs. null model comparison:
Model4_ResponsePHsByCommunity_NullCompare <- as.data.frame(anova(Model4_ResponsePHsByCommunityNull, Model4_ResponsePHsByCommunity.10, test="Chisq")) 
Model4_ResponsePHsByCommunity_NullCompare

#Extract model estimates
Model4_ResponsePHsByCommunity.10_Estimates <- round(summary(Model4_ResponsePHsByCommunity.10)$coefficients, 5)
Model4_ResponsePHsByCommunity.10_Estimates <- data.frame(Model4_ResponsePHsByCommunity.10_Estimates)
Model4_ResponsePHsByCommunity.10_Estimates

# Results of individual predictors (p-values and significance test):
Model4_ResponsePHsByCommunity.10_SigTests <-as.data.frame(drop1(Model4_ResponsePHsByCommunity.10, test="Chisq"))
Model4_ResponsePHsByCommunity.10_SigTests <-round(Model4_ResponsePHsByCommunity.10_SigTests, 5)
Model4_ResponsePHsByCommunity.10_SigTests

# Contribution of random effects: 
summary(Model4_ResponsePHsByCommunity.10)$varcor
Model4_ResponsePHsByCommunity.10_RandomEffectContribution <- data.frame(summary(Model4_ResponsePHsByCommunity.10)$varcor)
Model4_ResponsePHsByCommunity.10_RandomEffectContribution

# Number of levels per grouping:
summary(Model4_ResponsePHsByCommunity.10)$ngrps #27 chimps
Model4_ResponsePHsByCommunity.10_RandomEffectLevels <- data.frame(summary(Model4_ResponsePHsByCommunity.10)$ngrps)
Model4_ResponsePHsByCommunity.10_RandomEffectLevels

# Conditional and Marginal R squared:
model_performance(Model4_ResponsePHsByCommunity.10)
Model4_ResponsePHsByCommunity_ModelPerformance <- data.frame(model_performance(Model4_ResponsePHsByCommunity.10))

# Model complexity (How many observations per estimated item):
length(residuals(Model4_ResponsePHsByCommunity.10))/
  (length(fixef(Model4_ResponsePHsByCommunity.10))+
     nrow(as.data.frame(summary(Model4_ResponsePHsByCommunity.10)$varcor)))

Summary_Model4_ResponsePHsByCommunity.10 <- summary(Model4_ResponsePHsByCommunity.10)

# Confidence intervals:
Model4_ResponsePHsByCommunity.10_Confints <- confint(Model4_ResponsePHsByCommunity.10, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
Model4_ResponsePHsByCommunity.10_Confints
write.csv(Model4_ResponsePHsByCommunity.10_Confints, file = "Model4_ConfInts.csv")

#Run version of model without outliers
#create dataframe which excludes 4 outliers: cases 188, 684, 752, 932
exclude_outliers <- c(188, 684, 752, 932)
PHootsRecievedForModel4NoOutliers <- PHootsRecievedForModel4[-exclude_outliers, ]
PHootsRecievedForModel4OnlyOutliers <- PHootsRecievedForModel4[exclude_outliers, ]

#check number rows after ID removal
nrow(PHootsRecievedForModel4NoOutliers) # Should be N = 1041

#run version without outliers to see impact
Model4_ResponsePHsByCommunity_NoOutliers =glmer(FocalPhResp ~ Community + AgeCentered  + AlphaPresent+ IndepIndivsCentered  + InPartyPh + Drum + Core75 + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID),
              data=PHootsRecievedForModel4NoOutliers, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity_NoOutliers)

#Null without outliers
Model4_ResponsePHsByCommunity_NoOutliersNull =glmer(FocalPhResp ~ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
				data=PHootsRecievedForModel4NoOutliers, family=binomial, control=contr)

# Compare full no outlier model with null model:
Model4_ResponsePHsByCommunity_NoOutliers_NullCompare <- as.data.frame(anova(Model4_ResponsePHsByCommunity_NoOutliers, Model4_ResponsePHsByCommunity_NoOutliersNull, test="Chisq")) 
Model4_ResponsePHsByCommunity_NoOutliers_NullCompare  
Model4_ResponsePHsByCommunity_NoOutliers_NullCompare <- as.data.frame(Model4_ResponsePHsByCommunity_NoOutliers_NullCompare1)

# Results of individual predictors (to extract p-values and significance test):
Model4_ResponsePHsByCommunity_NoOutliers_SigTests <-as.data.frame(drop1(Model4_ResponsePHsByCommunity_NoOutliers, test="Chisq"))
Model4_ResponsePHsByCommunity_NoOutliers_SigTests <-round(Model4_ResponsePHsByCommunity_NoOutliers_SigTests, 5)
Model4_ResponsePHsByCommunity_NoOutliers_SigTests 

# Extract Estimates:
Model4_ResponsePHsByCommunity_NoOutliers_Estimates <- round(summary(Model4_ResponsePHsByCommunity_NoOutliers)$coefficients, 5)
Model4_ResponsePHsByCommunity_NoOutliers_Estimates
Model4_ResponsePHsByCommunity_NoOutliers_Estimates <- data.frame(Model4_ResponsePHsByCommunity_NoOutliers_Estimates)

#Confidence intervals
Model4_ResponsePHsByCommunity_NoOutliers_ConfInts  <- confint(Model4_ResponsePHsByCommunity_NoOutliers, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
Model4_ResponsePHsByCommunity_NoOutliers_ConfInts  

##############

#MODEL 4 SAVE OUTPUTS

##############
#Save outputs to new location 
#set working directory as appropriate
#setwd("C:/YOUR/DIRECTORY")
write.csv(DataForModel4_NumIDsPerCommunity, file = "DataForModel4_NumIDsPerCommunitySex.csv")
write.csv(DataForModel4_NumDatapointsPerChimpID, file = "DataForModel4_NumDatapointsPerChimpID.csv")
write.csv(DataForModel4_PHootsRecievedByCommunity, file = "DataForModel4_PHootsRecievedByCommunity.csv")
write.csv(Model4_ResponsePHsByCommunity.10_collinearitycheck, file = "Model4_ResponsePHsByCommunity.10_collinearitycheck.csv")
write.csv(Model4_ResponsePHsByCommunity.10_Estimates, file = "Model4_ResponsePHsByCommunity.10_Estimates.csv")
write.csv(Model4_ResponsePHsByCommunity_NullCompare1, file = "Model4_ResponsePHsByCommunity_NullCompare1.csv")
write.csv(Model4_ResponsePHsByCommunity_NullCompare2, file = "Model4_ResponsePHsByCommunity_NullCompare2.csv")
write.csv(Model4_ResponsePHsByCommunity.10_SigTests , file = "Model4_ResponsePHsByCommunity.10_SigTests.csv")
write.csv(Model4_ResponsePHsByCommunity.10_RandomEffectContribution, file = "Model4_ResponsePHsByCommunity.10_RandomEffectContribution.csv")
write.csv(Model4_ResponsePHsByCommunity.10_RandomEffectLevels, file = "Model4_ResponsePHsByCommunity.10_RandomEffectLevels.csv")
write.csv(Model4_ResponsePHsByCommunity_ModelPerformance, file = "Model4_ResponsePHsByCommunity_ModelPerformance.csv")

write.csv(Model4_ResponsePHsByCommunity_NoOutliers_NullCompare1, file = "Model4_ResponsePHsByCommunity_NoOutliers_NullCompare1.csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_NullCompare2, file = "Model4_ResponsePHsByCommunity_NoOutliers_NullCompare2.csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_SigTests , file = "Model4_ResponsePHsByCommunity_NoOutliers_SigTests .csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_Estimates, file = "Model4_ResponsePHsByCommunity_NoOutliers_Estimates.csv")

write.csv(PHootsRecievedForModel4, file = "Model4_DataUsed.csv")


write.csv(Model4_ResponsePHsByCommunity.10_Confints, file = "Model4_ConfInts.csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_ConfInts, file = "Model4_NoOutliers_ConfInts.csv")


