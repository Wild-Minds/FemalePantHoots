# Analyses code for the Manuscript "Sex specific flexible use of pant-hoot vocalisations in wild Eastern chimpanzees"
# Authors: 
# Contact: 

# Files needed: PanthootData_Model3&4_20250812_Anonymised.csv

######install and load packages used#####
#install.packages("EloRating");
#install.packages("dplyr"); 
#install.packages("lubridate"); 
#install.packages("plot3D"); 
#install.packages("dplyr"); 
#install.packages("lme4"); 
#install.packages("tidyverse"); 
#install.packages("performance"); 
#install.packages("readxl"); 
#install.packages("car"); 
#install.packages("effects");
#install.packages("interactions"); 
#install.packages("ggpubr"); 
#install.packages("gdata")
#install.packages("ggplot2"); 
#install.packages("emmeans"); 
#install.packages("jtools"); 
#install.packages("ggdist");
#install.packages("DHARMa");
#install.packages("see")

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
library(DHARMa);
library(see)

############################################

#IMPORT DATA AND PREPARE FOR MODEL 3

############################################
#### Set working directory as appropriate
#setwd("C:/YOUR/DIRECTORY")

#Amount of data summary stats
# Import data: 
ImportedData<- read.csv("PanthootData_Model3&4_20250812_Anonymised.csv")
colnames(ImportedData) #examine column names to decide which to retain. For model 3 we dont need "FocalPhResp","Parity","MaleDepOffspring", or "Oes4".                           
ImportedDataPHResponses <- ImportedData[, c("EventID2", "Date", "Hr","Min", "Community", "ChimpID", "FocalFollowNum","OpportunitiesToResp", "FocalPhResp", 
											"Sex", "Age","Core75", "Activity", "FoodPart","Drum", "InPartyPh", "AlphaPresent", "IndepIndivs")]

# Check number rows:
nrow(ImportedDataPHResponses) # Should be N = 2642

# Format variables: 
ImportedDataPHResponses$ChimpID <-as.factor(ImportedDataPHResponses$ChimpID)
ImportedDataPHResponses$Community <-as.factor(ImportedDataPHResponses$Community)
ImportedDataPHResponses$FocalFollowNum <-as.factor(ImportedDataPHResponses$FocalFollowNum)
ImportedDataPHResponses$AlphaPresent <-as.factor(ImportedDataPHResponses$AlphaPresent)
ImportedDataPHResponses$InPartyPh <-as.factor(ImportedDataPHResponses$InPartyPh)
ImportedDataPHResponses$FoodPart <-as.factor(ImportedDataPHResponses$FoodPart)
ImportedDataPHResponses$Sex <-as.factor(ImportedDataPHResponses$Sex)
ImportedDataPHResponses$Drum <-as.factor(ImportedDataPHResponses$Drum)
ImportedDataPHResponses$Activity <-as.factor(ImportedDataPHResponses$Activity)
ImportedDataPHResponses$Core75 <-as.factor(ImportedDataPHResponses$Core75)

ImportedDataPHResponses$IndepIndivs <- as.numeric(ImportedDataPHResponses$IndepIndivs)
ImportedDataPHResponses$Age <-as.numeric(ImportedDataPHResponses$Age)
ImportedDataPHResponses$Hr <-as.numeric(ImportedDataPHResponses$Hr)
ImportedDataPHResponses$Date <-as.numeric(ImportedDataPHResponses$Date)

# Create new category incorporating sex and community variables:
ImportedDataPHResponses$SexCommunity <- paste(ImportedDataPHResponses$Sex, ImportedDataPHResponses$Community)
ImportedDataPHResponses$SexCommunity <- as.factor(ImportedDataPHResponses$SexCommunity)

# Check representation of IDs before missing data rows removed:
dataframe <- ImportedDataPHResponses
dataframe$Group1 <- ImportedDataPHResponses$ChimpID
DataCollected_NumDatapointsPerChimpID <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberDataPoints = length(ChimpID),
															Community = first(Community),
															Sex = first(Sex),
															SexCommunity = first(SexCommunity),
															)		
View(DataCollected_NumDatapointsPerChimpID) 

#check representation of Number data points by group & Sex
dataframe <- ImportedDataPHResponses
dataframe$Group1 <- ImportedDataPHResponses$SexCommunity
DataCollected_NumDatapointsPerCommunitySex <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberDataPoints = length(SexCommunity)
															)		
View(DataCollected_NumDatapointsPerCommunitySex) # should be f sonso=541; f waibira=931; m waibira=1170

# Number of IDs by sex & community: 
DataCollected_NumberChimpsPerSexCommunity <- ImportedDataPHResponses %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(SexCommunity) %>%
     summarise(NumberChimps = n())
View(DataCollected_NumberChimpsPerSexCommunity) # Should be: 10 sonso females; 18 waibira females; 16 waibira males

#merge prev two dataframes (for summary stats)
DataCollected_NumberChimpsPerSexCommunity$NumberDataPoints <- DataCollected_NumDatapointsPerCommunitySex$NumberDataPoints

#Remove rows with missing data (e.g. Ben who is alpha male, who will not have data for is alpha present, others with missing activity etc)
PHootsRecieved <- ImportedDataPHResponses[!is.na(ImportedDataPHResponses$Activity), ]
PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$Hr), ]
PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$FoodPart), ]
PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$Drum), ]
PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$InPartyPh), ]
PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$AlphaPresent), ]
PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$IndepIndivs), ]
nrow(PHootsRecieved) # should be 2115

PHootsRecieved <- PHootsRecieved[!is.na(PHootsRecieved$Core75), ]
nrow(PHootsRecieved) # should be 1971 (we lost 144 by including range)

# Check that the data and outcome variables are balanced wrt the random and control variables:
table(PHootsRecieved$FocalPhResp, PHootsRecieved$ChimpID) #Most IDs well represented and most with no response have plenty datapoints
table(PHootsRecieved$FocalPhResp, PHootsRecieved$FocalFollowNum) #most are well represented, overall sample larger than ChimpID, more with lower N but okay
table(PHootsRecieved$FocalPhResp, PHootsRecieved$Hr) #slight skew to earlier in day but okay

# Check that the data and outcome variables are balanced wrt the predictor variables:
table(PHootsRecieved$FocalPhResp, PHootsRecieved$Sex) #okay
table(PHootsRecieved$FocalPhResp, PHootsRecieved$AlphaPresent) #okay
table(PHootsRecieved$FocalPhResp, PHootsRecieved$InPartyPh) #okay
table(PHootsRecieved$FocalPhResp, PHootsRecieved$Activity) #okay
table(PHootsRecieved$FocalPhResp, PHootsRecieved$FoodPart) # lower representation of ripe fruit, but okay
table(PHootsRecieved$FocalPhResp, PHootsRecieved$Core75) # higher representation of yes, but ok
table(PHootsRecieved$FocalPhResp, PHootsRecieved$IndepIndivs) # tail for large group sizes, low sample size of pant-hoot responses from 15+ likely due to low sample, consider excluding cases
plot(table(PHootsRecieved$IndepIndivs))

#subset for only when party size has less than 15 independent individuals
PHootsRecievedForModel3 <-subset(PHootsRecieved, IndepIndivs<15)
nrow(PHootsRecievedForModel3) # should be 1936 (used to be 2079 before including range)

#Recheck data distributions:
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$ChimpID) 
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$FocalFollowNum) 
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Hr) 
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$AlphaPresent) 
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$InPartyPh) 
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Activity) 
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$FoodPart)
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Core75)
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$IndepIndivs) 
plot(table(PHootsRecievedForModel3$IndepIndivs)) #tail no longer a problem

table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Sex) # okay
DataForModel3_PHootsRecievedBySex <- table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Sex)
DataForModel3_PHootsRecievedBySex <- data.frame(DataForModel3_PHootsRecievedBySex)

# Number of 1s and 0s in the outcome overall:
table(PHootsRecievedForModel3$FocalPhResp) # 0 = 1639, 1 = 297  (was 0= 1766, 1 = 313 )

#Make dataframe for export of representation of IDs now rows removed:
dataframe <- PHootsRecievedForModel3
dataframe$Group1 <- PHootsRecievedForModel3$ChimpID
DataForModel3_NumDatapointsPerChimpID <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberDataPoints = length(ChimpID),
															Community = first(Community),
															Sex = first(Sex),
															SexCommunity = first(SexCommunity),
															)		
View(DataForModel3_NumDatapointsPerChimpID) 

#representation of Number data points by community & Sex
dataframe <- PHootsRecievedForModel3
dataframe$Group1 <- PHootsRecievedForModel3$SexCommunity
DataForModel3_NumDatapointsPerCommunitySex <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberDataPoints = length(SexCommunity))		
View(DataForModel3_NumDatapointsPerCommunitySex) # should be f sonso=463; f waibira=597; m waibira=876 (was f sonso=484; f waibira=648; m waibira=947)

# Number of IDs by sex & community: 
DataForModel3_NumIDsPerCommunitySex <- PHootsRecievedForModel3 %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(SexCommunity) %>%
     summarise(NumberChimps = n())
View(DataForModel3_NumIDsPerCommunitySex) # Should be: 10 sonso females; 18 waibira females; 14 waibira males (was 15 before including locaiton)
#merge prev two dataframes (to save for summary stats)
DataForModel3_NumIDsPerCommunitySex$NumberDataPoints <- DataForModel3_NumDatapointsPerCommunitySex$NumberDataPoints

#format food FoodPart variable for models
# Replace 'NotFeed' with 'notfeedripefruit'; and 'notripefruit' with 'notfeedripefruit'
PHootsRecievedForModel3$FoodPart <- gsub("NotFeed", "NotFeedRipeFruit", PHootsRecievedForModel3$FoodPart)
PHootsRecievedForModel3$FoodPart <- gsub("NotRipeFruit", "NotFeedRipeFruit", PHootsRecievedForModel3$FoodPart)

# Set reference levels for categorical variables:
PHootsRecievedForModel3$Sex <- relevel(PHootsRecievedForModel3$Sex, ref = "m")
PHootsRecievedForModel3$Activity <- relevel(PHootsRecievedForModel3$Activity, ref = "NotFeed")
PHootsRecievedForModel3$FoodPart <- as.factor(PHootsRecievedForModel3$FoodPart)
PHootsRecievedForModel3$FoodPart <- relevel(PHootsRecievedForModel3$FoodPart, ref = "NotFeedRipeFruit")
PHootsRecievedForModel3$Core75 <- relevel(PHootsRecievedForModel3$Core75, ref = "No")

# Standardize numeric predictors:
IndepIndivsMean <- mean(PHootsRecievedForModel3$IndepIndivs)
IndepIndivsSD <- sd(PHootsRecievedForModel3$IndepIndivs)
PHootsRecievedForModel3$IndepIndivsCentered <- (PHootsRecievedForModel3$IndepIndivs-IndepIndivsMean)/IndepIndivsSD 

AgeMean <- mean(PHootsRecievedForModel3$Age)
AgeSD <- sd(PHootsRecievedForModel3$Age)
PHootsRecievedForModel3$AgeCentered <- (PHootsRecievedForModel3$Age-AgeMean)/AgeSD 

HrMean <- mean(PHootsRecievedForModel3$Hr)
HrSD <- sd(PHootsRecievedForModel3$Hr)
PHootsRecievedForModel3$HrCentered <- (PHootsRecievedForModel3$Hr-HrMean)/HrSD 

# Group by 'Sex' and quantify the unique values of 'ChimpID':
DataForModel3_NumberChimpsBySex <- PHootsRecievedForModel3 %>%
															group_by(Sex) %>%
															summarise(unique_chimp_count = n_distinct(ChimpID))
print(DataForModel3_NumberChimpsBySex) # females: N = 28, Males: N = 14 (was 15 before including location))
DataForModel3_NumberChimpsBySex <- data.frame(DataForModel3_NumberChimpsBySex)

############################################

#RUN MODEL 3

############################################
# Model 3: Which factors predict pant-hoot responses in male and female chimpanzees? ------
contr=glmerControl(optCtrl = list(maxfun=100000), calc.derivs=F, optimizer = "nloptwrap")

#model with all interactions
Model3_ResponsePHsBySex.1 =glmer(FocalPhResp ~ Sex*AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + Sex*Core75 + Sex*(Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.1)

#rerun without least significant interaction: Sex*(Activity/FoodPart)
Model3_ResponsePHsBySex.2 =glmer(FocalPhResp ~ Sex*AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + Sex*Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.2)

#rerun without least significant interaction: Sex*Age 
Model3_ResponsePHsBySex.3 =glmer(FocalPhResp ~ AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + Sex*Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.3)

#rerun without least significant interaction: Sex*Core75 
Model3_ResponsePHsBySex.4 =glmer(FocalPhResp ~ AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.4)

#rerun without least significant interaction: Sex*Drum
Model3_ResponsePHsBySex.5 =glmer(FocalPhResp ~ AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Drum + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.5)

#############

#FINAL MODEL 3

##############
#rerun without least significant interaction: Sex*alphapresent
Model3_ResponsePHsBySex.6 =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Drum + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.6)

#Final full model = Model3_ResponsePHsBySex.6 as remaining interactions are significant

##############

#MODEL3 CHECKS

##############
#model without interactions (for collinearity calcs)
Model3_ResponsePHsBySex_NoInteracts =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Core75 + Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)

#make null model with only control variables and random effects:
Model3_ResponsePHsBySexNull =glmer(FocalPhResp ~ HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)


#########################################
### Model checks: ----
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
ranef.diagn.plot(Model3_ResponsePHsBySex.6) #fine
check_collinearity(Model3_ResponsePHsBySex_NoInteracts)#max VIF = 2.64 ok
Model3_ResponsePHsBySex.6_collinearitycheck <-  data.frame(check_collinearity(Model3_ResponsePHsBySex_NoInteracts))

# General checks:
check_model(Model3_ResponsePHsBySex.6) 
check_model_Model3_ResponsePHsBySex.6 <- check_model(Model3_ResponsePHsBySex.6) 
check_model_Model3_ResponsePHsBySex.6 
check_outliers_Model3_ResponsePHsBySex.6 <- check_outliers(Model3_ResponsePHsBySex.6)
check_outliers_Model3_ResponsePHsBySex.6  #  8 outliers detected: cases 615, 862, 1352, 1353, 1490, 1643, 1823, 1906. Based on the following method and threshold: cook (0.8). For variable: (Whole model).


# Stability:
source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
full.stab=glmm.model.stab(model.res=Model3_ResponsePHsBySex.6, data=PHootsRecievedForModel3,contr=contr)

# check for convergence issues these are indicated separately for lme4 (column lme4.warnings) and the optimizer opt.warnings): 
table(full.stab$detailed$lme4.warnings) #none
table(full.stab$detailed$opt.warnings) #none

M3x<-round(full.stab$summary[, -1], 3) 
 m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_Model3_ResponsePHsBySex.6 <- m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_Model3_ResponsePHsBySex.6

###Model perameters	
# Full model vs. null model comparison:
Model3_ResponsePHsBySex_NullCompare <- as.data.frame(anova(Model3_ResponsePHsBySex.6, Model3_ResponsePHsBySexNull, test="Chisq")) 
Model3_ResponsePHsBySex_NullCompare # p value:3.110721e-44 , Chi2 = 235.434  df = 11  

#Extract model estimates
Model3_ResponsePHsBySex.6_Estimates <- round(summary(Model3_ResponsePHsBySex.6)$coefficients, 5)
Model3_ResponsePHsBySex.6_Estimates <- data.frame(Model3_ResponsePHsBySex.6_Estimates)
Model3_ResponsePHsBySex.6_Estimates

# Results of individual predictors (to extract p-values and significance test):
Model3_ResponsePHsBySex.6_SigTests <-as.data.frame(drop1(Model3_ResponsePHsBySex.6, test="Chisq"))
Model3_ResponsePHsBySex.6_SigTests <-round(Model3_ResponsePHsBySex.6_SigTests, 5)
Model3_ResponsePHsBySex.6_SigTests 

# Contribution of random effects: 
summary(Model3_ResponsePHsBySex.6)$varcor
Model3_ResponsePHsBySex.6_RandomEffectContribution <- data.frame(summary(Model3_ResponsePHsBySex.6)$varcor)
Model3_ResponsePHsBySex.6_RandomEffectContribution

# Number of levels per grouping:
summary(Model3_ResponsePHsBySex.6)$ngrps #146 focal follows, 42 chimps
Model3_ResponsePHsBySex.6_RandomEffectLevels <- data.frame(summary(Model3_ResponsePHsBySex.6)$ngrps)
Model3_ResponsePHsBySex.6_RandomEffectLevels

# Conditional and Marginal R squared:
model_performance(Model3_ResponsePHsBySex.6)
Model3_ResponsePHsBySex_ModelPerformance <- data.frame(model_performance(Model3_ResponsePHsBySex.6))

# Model complexity (How many observations per estimated item):
length(residuals(Model3_ResponsePHsBySex.6))/
  (length(fixef(Model3_ResponsePHsBySex.6))+
     nrow(as.data.frame(summary(Model3_ResponsePHsBySex.6)$varcor))) 
	 
Summary_Model3_ResponsePHsBySex <- summary(Model3_ResponsePHsBySex.6)

# Confidence intervals: 
Model3_ResponsePHsBySex_ConfidenceIntervals  <- confint(Model3_ResponsePHsBySex.6, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
Model3_ResponsePHsBySex_ConfidenceIntervals

##############

#MODEL 3 POST HOC MODELS

##############
### Post-hoc tests to interpret significant interactions: ----
# Create new datasets one for female and one for male:
PHootsRecievedForModel3F <- subset(PHootsRecievedForModel3, Sex == "f")
PHootsRecievedForModel3M <- subset(PHootsRecievedForModel3, Sex == "m")

# Model but with female and male data separately:
Model3_ResponsePHsBySex_FemalePostHoc =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum  + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3F , family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_FemalePostHoc)

Model3_ResponsePHsBySex_MalePostHoc =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum  + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3M , family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_MalePostHoc)

#estimates for posthoc models
Model3_ResponsePHsBySex_FemalePostHoc_Estimates <- round(summary(Model3_ResponsePHsBySex_FemalePostHoc)$coefficients, 5)
Model3_ResponsePHsBySex_FemalePostHoc_Estimates
Model3_ResponsePHsBySex_FemalePostHoc_Estimates <- data.frame(Model3_ResponsePHsBySex_FemalePostHoc_Estimates)

Model3_ResponsePHsBySex_MalePostHoc_Estimates <- round(summary(Model3_ResponsePHsBySex_MalePostHoc)$coefficients, 5)
Model3_ResponsePHsBySex_MalePostHoc_Estimates
Model3_ResponsePHsBySex_MalePostHoc_Estimates <- data.frame(Model3_ResponsePHsBySex_MalePostHoc_Estimates)

# Results of individual predictors (to extract p-values and significance test for post-hoc models):
Model3_ResponsePHsBySex_FemalePostHoc_SigTests <-as.data.frame(drop1(Model3_ResponsePHsBySex_FemalePostHoc, test="Chisq"))
Model3_ResponsePHsBySex_FemalePostHoc_SigTests <-round(Model3_ResponsePHsBySex_FemalePostHoc_SigTests, 5)
Model3_ResponsePHsBySex_FemalePostHoc_SigTests 

Model3_ResponsePHsBySex_MalePostHoc_SigTests  <-as.data.frame(drop1(Model3_ResponsePHsBySex_MalePostHoc, test="Chisq"))
Model3_ResponsePHsBySex_MalePostHoc_SigTests  <-round(Model3_ResponsePHsBySex_MalePostHoc_SigTests, 5)
Model3_ResponsePHsBySex_MalePostHoc_SigTests 

#Confidence intervals for posthoc models
#Model3_ResponsePHsBySex_FemalePostHoc_ConfidenceIntervals  <- confint(Model3_ResponsePHsBySex_FemalePostHoc, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
Model3_ResponsePHsBySex_FemalePostHoc_ConfidenceIntervals

#Model3_ResponsePHsBySex_MalePostHoc_ConfidenceIntervals  <- confint(Model3_ResponsePHsBySex_MalePostHoc, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
Model3_ResponsePHsBySex_MalePostHoc_ConfidenceIntervals

##############

#MODEL 3 WITHOUT OUTLIERS

##############
#Run version of model without outliers
#create datafram which excludes 8 outliers: 615, 862, 1352, 1353, 1490, 1643, 1823, 1906
exclude_outliers <- c(615, 862, 1352, 1353, 1490, 1643, 1823, 1906)
PHootsRecievedForModel3NoOutliers <- PHootsRecievedForModel3[-exclude_outliers, ]
PHootsRecievedForModel3OnlyOutliers <- PHootsRecievedForModel3[exclude_outliers, ]

#check number rows after ID removal
nrow(PHootsRecievedForModel3NoOutliers) # Should be N = 1928

#run version without outliers to see impact
Model3_ResponsePHsBySex_NoOutliers =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Drum + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3NoOutliers, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_NoOutliers)
#all interpretation the same except for Activity/FoodPart is nonsignificant

#Null without outliers
Model3_ResponsePHsBySex_NoOutliersNull =glmer(FocalPhResp ~ HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3NoOutliers, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_NoOutliersNull)

# Compare full no outlier model with null model:
Model3_ResponsePHsBySex_NoOutliers_NullCompare <- as.data.frame(anova(Model3_ResponsePHsBySex_NoOutliers, Model3_ResponsePHsBySex_NoOutliersNull, test="Chisq")) 
Model3_ResponsePHsBySex_NoOutliers_NullCompare  
Model3_ResponsePHsBySex_NoOutliers_NullCompare <- as.data.frame(Model3_ResponsePHsBySex_NoOutliers_NullCompare)

# Results of individual predictors (to extract p-values and significance test):
Model3_ResponsePHsBySex_NoOutliers_SigTests <-as.data.frame(drop1(Model3_ResponsePHsBySex_NoOutliers, test="Chisq"))
Model3_ResponsePHsBySex_NoOutliers_SigTests <-round(Model3_ResponsePHsBySex_NoOutliers_SigTests, 5)
Model3_ResponsePHsBySex_NoOutliers_SigTests 

# Extract Estimates:
Model3_ResponsePHsBySex_NoOutliers_Estimates <- round(summary(Model3_ResponsePHsBySex_NoOutliers)$coefficients, 5)
Model3_ResponsePHsBySex_NoOutliers_Estimates
Model3_ResponsePHsBySex_NoOutliers_Estimates <- data.frame(Model3_ResponsePHsBySex_NoOutliers_Estimates)

#Confidence intervals
#Model3_ResponsePHsBySex_NoOutliers_ConfidenceIntervals  <- confint(Model3_ResponsePHsBySex_NoOutliers, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
#Model3_ResponsePHsBySex_NoOutliers_ConfidenceIntervals

### Post-hoc tests on models without outliers
# Create new datasets one for female and one for male when outliers are excluded:
PHootsRecievedForModel3NoOutliersF <- subset(PHootsRecievedForModel3NoOutliers, Sex == "f")
PHootsRecievedForModel3NoOutliersM <- subset(PHootsRecievedForModel3NoOutliers, Sex == "m")

# Model but with female and male data separately when outliers are excluded:
Model3_ResponsePHsBySex_FemalePostHocNoOutliers =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3NoOutliersF , family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_FemalePostHocNoOutliers)

Model3_ResponsePHsBySex_MalePostHocNoOutliers =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + Core75 + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3NoOutliersM , family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_MalePostHocNoOutliers)

#estimates for posthoc models
Estimates_Model3_ResponsePHsBySex_FemalePostHocNoOutliers <- round(summary(Model3_ResponsePHsBySex_FemalePostHocNoOutliers)$coefficients, 3)
Estimates_Model3_ResponsePHsBySex_FemalePostHocNoOutliers
Estimates_Model3_ResponsePHsBySex_FemalePostHocNoOutliers <- data.frame(Estimates_Model3_ResponsePHsBySex_FemalePostHocNoOutliers)

Estimates_Model3_ResponsePHsBySex_MalePostHocNoOutliers <- round(summary(Model3_ResponsePHsBySex_MalePostHocNoOutliers)$coefficients, 3)
Estimates_Model3_ResponsePHsBySex_MalePostHocNoOutliers
Estimates_Model3_ResponsePHsBySex_MalePostHocNoOutliers <- data.frame(Estimates_Model3_ResponsePHsBySex_MalePostHocNoOutliers)

# Results of individual predictors (to extract p-values and significance test for post-hoc models):
SigTests_Model3_ResponsePHsBySex_FemalePostHocNoOutliers <-as.data.frame(drop1(Model3_ResponsePHsBySex_FemalePostHocNoOutliers, test="Chisq"))
SigTests_Model3_ResponsePHsBySex_FemalePostHocNoOutliers <-round(SigTests_Model3_ResponsePHsBySex_FemalePostHocNoOutliers, 3)
SigTests_Model3_ResponsePHsBySex_FemalePostHocNoOutliers

SigTests_Model3_ResponsePHsBySex_MalePostHocNoOutliers <-as.data.frame(drop1(Model3_ResponsePHsBySex_MalePostHocNoOutliers, test="Chisq"))
SigTests_Model3_ResponsePHsBySex_MalePostHocNoOutliers <-round(SigTests_Model3_ResponsePHsBySex_MalePostHocNoOutliers, 3)
SigTests_Model3_ResponsePHsBySex_MalePostHocNoOutliers

##############

#MODEL 3 SAVE OUTPUTS

##############
#Save outputs to new location 
#set working directory as appropriate
#setwd("C:/YOUR/DIRECTORY")

write.csv(DataCollected_NumDatapointsPerChimpID, file = "DataCollected_PhResps_NumDatapointsPerChimpID.csv")
write.csv(DataCollected_NumberChimpsPerSexCommunity, file = "DataCollected_PhResps_NumberChimpsPerSexCommunity.csv")

#Save model checks and outputs
#setwd("C:/YOUR/DIRECTORY")
write.csv(DataForModel3_NumIDsPerCommunitySex, file = "DataForModel3_NumIDsPerCommunitySex.csv")
write.csv(DataForModel3_NumDatapointsPerChimpID, file = "DataForModel3_NumDatapointsPerChimpID.csv")
write.csv(DataForModel3_PHootsRecievedBySex, file = "DataForModel3_PHootsRecievedBySex.csv")
write.csv(DataForModel3_NumberChimpsBySex, file = "DataForModel3_NumberChimpsBySex.csv")
write.csv(Model3_ResponsePHsBySex.6_collinearitycheck, file = "Model3_ResponsePHsBySex.6_collinearitycheck.csv")
write.csv(Model3_ResponsePHsBySex.6_Estimates, file = "Model3_ResponsePHsBySex.6_Estimates.csv")
write.csv(Model3_ResponsePHsBySex_NullCompare, file = "Model3_ResponsePHsBySex_NullCompare.csv")
write.csv(Model3_ResponsePHsBySex.6_SigTests , file = "Model3_ResponsePHsBySex.6_SigTests.csv")
write.csv(Model3_ResponsePHsBySex.6_RandomEffectContribution, file = "Model3_ResponsePHsBySex.6_RandomEffectContribution.csv")
write.csv(Model3_ResponsePHsBySex.6_RandomEffectLevels, file = "Model3_ResponsePHsBySex.6_RandomEffectLevels.csv")
write.csv(Model3_ResponsePHsBySex_ModelPerformance, file = "Model3_ResponsePHsBySex_ModelPerformance.csv")

write.csv(Model3_ResponsePHsBySex_MalePostHoc_SigTests , file = "Model3_ResponsePHsBySex_MalePostHoc_SigTests.csv")
write.csv(Model3_ResponsePHsBySex_FemalePostHoc_SigTests, file = "Model3_ResponsePHsBySex_FemalePostHoc_SigTests.csv")
write.csv(Model3_ResponsePHsBySex_MalePostHoc_Estimates, file = "Model3_ResponsePHsBySex_MalePostHoc_Estimates.csv")
write.csv(Model3_ResponsePHsBySex_FemalePostHoc_Estimates, file = "Model3_ResponsePHsBySex_FemalePostHoc_Estimates.csv")
write.csv(Model3_ResponsePHsBySex_NoOutliers_NullCompare, file = "Model3_ResponsePHsBySex_NoOutliers_NullCompare.csv")
write.csv(Model3_ResponsePHsBySex_NoOutliers_SigTests , file = "Model3_ResponsePHsBySex_NoOutliers_SigTests .csv")
write.csv(Model3_ResponsePHsBySex_NoOutliers_Estimates, file = "Model3_ResponsePHsBySex_NoOutliers_Estimates.csv")

write.csv(Model3_ResponsePHsBySex_ConfidenceIntervals, file = "Model3_ConfInts.csv")
write.csv(Model3_ResponsePHsBySex_FemalePostHoc_ConfidenceIntervals, file = "Model3_FemalePostHoc_ConfInts.csv")
write.csv(Model3_ResponsePHsBySex_MalePostHoc_ConfidenceIntervals, file = "Model3_MalePostHoc_ConfInts.csv")
write.csv(Model3_ResponsePHsBySex_NoOutliers_ConfidenceIntervals, file = "Model3_NoOutliers_ConfInts.csv")

write.csv(PHootsRecievedForModel3, file = "Model3_DataUsed.csv")


#Save text files
# Open a connection to a text file, save text from final model summary, overdisp tests, colinearity and VIF values
sink("Model3_SummaryOutput_NProd.txt")
print("Model3 Summary")
print("summary(Model3_ResponsePHsBySex.6)")
print(summary(Model3_ResponsePHsBySex.6))
print("____________________________________________________________________________")
print("Model3 Contribution of random effects")
print("summary(Model3_ResponsePHsBySex.6)$varcor")
print(summary(Model3_ResponsePHsBySex.6)$varcor)
print("____________________________________________________________________________")
print("Model3 Outliers check output")
print("check_outliers(Model3_ResponsePHsBySex.6)")
print(check_outliers(Model3_ResponsePHsBySex.6))
print("____________________________________________________________________________")
print("Model3 Female Posthoc")
print("summary(Model3_ResponsePHsBySex_FemalePostHoc)")
print(summary(Model3_ResponsePHsBySex_FemalePostHoc))
print("____________________________________________________________________________")
print("Model3 Male Posthoc")
print("summary(Model3_ResponsePHsBySex_FemalePostHoc)")
print(summary(Model3_ResponsePHsBySex_FemalePostHoc))
print("____________________________________________________________________________")
sink()

#Save figures
# Save colinearity plot as a PNG file
png("Model3_RanefDiagnPlot.png")
ranef.diagn.plot(Model3_ResponsePHsBySex.6)
dev.off()

# Save model checks plot as a PNG file
png("Model3_ModelCheckPlot.png")
check_model_Model3_ResponsePHsBySex.6 
dev.off()

#Save stability plot as PNG file
png("Model3_StabilityPlot.png")
m.stab.plot(full.stab$summary[, -1]) 
dev.off()

##############

#MODEL 3 PLOT GRAPH

##############
# Parameter values used for plotting Results:
My_label_2 = theme(plot.title = element_text(size = 22, hjust = 0.5),
					axis.title.x = element_text(size = 16),
					axis.text.x = element_text(size = 15),
					axis.text.y = element_text(size = 15),
                   axis.title.y = element_text(size = 16, vjust=2),
                   legend.key.size = unit(0.2, 'cm'), legend.key.height = unit(0.5, 'cm'), 
                   legend.key.width = unit(1.3, 'cm'), legend.title = element_text(size=14), 
                   legend.text = element_text(size=12)) 


# Interaction between Sex and IndepIndivs:
Model3_ResponsePHsBySex.6_Plot1BasePlot <- interact_plot(Model3_ResponsePHsBySex.6,
									pred = "IndepIndivsCentered", modx = "Sex", 
									data = PHootsRecievedForModel3, 
									plot.points = F, 
									interval = T,
									int.type = "confidence", 
									geom = "line", vary.lty = T,
									x.label = "Context", y.label = "Probability of pant-hoot response", 
									colors = "Set2", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
		scale_y_continuous(breaks=seq(0, 1,0.2), limits = c(-0.025, 1.04),expand = c(0, 0)) +
		scale_x_continuous(limits = c(-1.51, 3.0), breaks=seq(-1.51, 2.77,0.61),  labels =c(0, 2, 4, 6, 8, 10, 12, 14), expand = c(0, 0), name="Number individuals in party") +
		theme_classic() +
		theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
		theme(legend.position = "none")+
		My_label_2
Model3_ResponsePHsBySex.6_Plot1BasePlot 

# prepare points data: aim to get variable that is 'proportion responded to', per ID at group size
df_pointsprep <- PHootsRecievedForModel3 %>%
  select(IndepIndivsCentered, Sex, FocalPhResp, FocalFollowNum) %>%
  mutate(y = FocalPhResp,
         modx_group = interaction(Sex))
view(df_pointsprep)

#create new variable to do grouping by so that we can make summary numbers for each Focal Follow at each party size	 
df_pointsprep$FocalFollowNum_IndepIndiv <- paste(df_pointsprep$FocalFollowNum, df_pointsprep$IndepIndivsCentered, sep = ",")

df_points <- df_pointsprep %>%
							 group_by(FocalFollowNum_IndepIndiv) %>%
							  summarise(
									Nfull = n(), 
									Sex = unique(Sex),
									FocalPhRespSum = sum(FocalPhResp),
									IndepIndivsCentered = unique(IndepIndivsCentered),
									modx_group = unique(modx_group)
									)	

df_points$ProportionRespond <- df_points$FocalPhRespSum/df_points$Nfull
view(df_points)
max(df_points$ProportionRespond)

Model3_ResponsePHsBySex.6_Plot1final_plot <- Model3_ResponsePHsBySex.6_Plot1BasePlot +
  geom_point(data = df_points, 
             aes(x = IndepIndivsCentered, y = ProportionRespond, color = Sex, group = modx_group),
             position = position_jitter(width = 0.085, height = 0.025),  # jitter both axes
             size = 1.5, alpha = 0.25)

Model3_ResponsePHsBySex.6_Plot1final_plot



#Interaction between in group pant-hoots and sex
Model3_ResponsePHsBySex.6_Plot2BasePlot  <- cat_plot(Model3_ResponsePHsBySex.6,
									pred = "InPartyPh", modx = "Sex",
									data = PHootsRecievedForModel3,
									plot.points = F, interval = T,
									int.type = "confidence",
									geom = "line",vary.lty = T,
									x.label = "Pant-hoot recieved from within party", y.label = "Probability of pant-hoot response", 
									colors = "Set2", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
		scale_y_continuous(breaks=seq(0, 1,0.1), limits = c(-0.025, 1.04),expand = c(0, 0)) +
		scale_x_discrete(breaks = c(0, 1), labels =c("No","Yes"))+
		theme_classic() +
		theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
		theme(legend.position = "none")+
		My_label_2
Model3_ResponsePHsBySex.6_Plot2BasePlot 


# prepare points data: aim to get variable that is 'proportion responded to', per focal follow at recieved vs didnt recive ingroup PH
df_pointsprep2 <- PHootsRecievedForModel3 %>%
					select(InPartyPh, Sex, FocalPhResp, FocalFollowNum) %>%
					mutate(y = FocalPhResp,
					modx_group = interaction(Sex))
view(df_pointsprep2)

#create new variable to do grouping by so that we can make summary numbers  proportion responded to whne did and didnt recieve ingorup ph for each focal follow 
df_pointsprep2$FocalFollowNum_InPartyPh <- paste(df_pointsprep2$FocalFollowNum, df_pointsprep2$InPartyPh, sep = ",")

df_points2 <- df_pointsprep2 %>%
							 group_by(FocalFollowNum_InPartyPh) %>%
							  summarise(
									Nfull = n(), 
									Sex = unique(Sex),
									FocalPhRespSum = sum(FocalPhResp),
									InPartyPh = unique(InPartyPh),
									modx_group = unique(modx_group)
									)	

df_points2$ProportionRespond <- df_points2$FocalPhRespSum/df_points2$Nfull
view(df_points2)
max(df_points2$ProportionRespond)


Model3_ResponsePHsBySex.6_Plot2final_plot <- Model3_ResponsePHsBySex.6_Plot2BasePlot +
  geom_point(data = df_points2, 
             aes(x = InPartyPh, y = ProportionRespond, color = Sex, group = modx_group),
             position = position_jitter(width = 0.35, height = 0.02),  # jitter both axes
             size = 1.5, alpha = 0.25)

Model3_ResponsePHsBySex.6_Plot2final_plot


# Combine two plots:
Model3_FinalPlot <- ggarrange(Model3_ResponsePHsBySex.6_Plot1final_plot, Model3_ResponsePHsBySex.6_Plot2final_plot, ncol = 2, nrow = 1, labels = c("A", "B"), 
          font.label = list(size = 20, color = "black"), label.y = c(1.01, 1.01), label.x = c(-0.01, -0.01)) 
Model3_FinalPlot


ggsave("Model3_FinalPlotsBoth.png", plot = Model3_FinalPlot, scale = (300/96), width = 837, height= 574, units = c("px"), dpi = 300, limitsize=TRUE, bg="white")
