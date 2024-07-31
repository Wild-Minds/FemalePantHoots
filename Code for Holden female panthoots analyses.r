# Analyses code for the Manuscript "The use of pant-hoot vocalisations in female chimpanzees of Budongo Forest"
# Authors: Eve Holden, Charlotte Grund, Robert Eguma, Liran Samuni, Klaus Zuberbühler, Adrian Soldati*, Catherine Hobaiter*
# Contact: eh54@st-andrews.ac.uk 

# Code for analyses: 
#Elo-rating of males to determine the alpha male
#Model 1 comparing male and female pant-hooting rates
#Model 2 comparing Waibira and Sonso pant-hooting rates
#Model 3 predictors for male and female pant-hoot responses
#Model 4 predictors for Waibria and Sonso female pant-hoot responses (incl female-specific variables)

# Files needed: 
# Waibira_PantGrunts_20220123.csv
# Wabira_Sex.csv
# Waibira_IDsPresent.csv
# Sonso_PantGrunts_Male only.csv
# Holden_PanthootData_Model 1&2.xlsx
# Holden_PanthootData_Model 3&4.xlsx

#### Set working directory as appropriate

# R libraries needed
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
#MASS needed for Model 2, but if loaded before Elo ratings it masks a necesary function and interrupts code
#library(MASS) 

############################

#ELO RATINGS FOR WAIBIRA ALPHA MALE

############################
# Files needed: 
# Waibira_PantGrunts_20220123.csv
# Wabira_Sex.csv
# Waibira_IDsPresent.csv

# Elo-rating of males to determine the alpha male Waibira ----

# import necessary files
WaibiraPantGrunts<-read.csv("Waibira_PantGrunts_20220123.csv", header = T, stringsAsFactors = T, sep = ",")
Waibira_Sex<-read.csv("Wabira_Sex.csv", header = T, stringsAsFactors = T, sep = ",")
WaibiraIDsPresent<-read.csv("Waibira_IDsPresent.csv", header = T, stringsAsFactors = T, sep = ",")

# attach sexes to elo.data and remove females
elo.data <- WaibiraPantGrunts %>%
  left_join(Waibira_Sex, by = c("Winner" = "ID")) %>%
  left_join(Waibira_Sex, by = c("Loser" = "ID")) %>%
  filter(Sex.x == "M" & Sex.y == "M") %>%
  select(-Sex.x, -Sex.y) %>% arrange(Date)
head(elo.data)

# define date column as date and remove cases that happen before last observation in presence dataset
elo.data$Date<-as.Date(elo.data$Date, format = "%d/%m/%Y")
elo.data<-subset(elo.data, Date<"2022-03-23")

# prepare data 
WaibiraIDsPresent <- data.frame(WaibiraIDsPresent)
Date <- as.Date(WaibiraIDsPresent$Date, 
                format = "%d/%m/%Y") # create date for presence file
WaibiraIDsPresent <- WaibiraIDsPresent[, c(intersect(colnames(WaibiraIDsPresent), 
                           c(as.character(elo.data$Loser), 
                             as.character(elo.data$Winner))))] # remove all individuals from presence that never have any pant grunts
WaibiraIDsPresent <- cbind(Date, WaibiraIDsPresent) # add Date column to presence data again
WaibiraIDsPresent <- subset(WaibiraIDsPresent, 
               Date >= min(as.Date(elo.data$Date, format = "%Y-%m-%d")) & 
                 Date <= max(as.Date(elo.data$Date, format = "%Y-%m-%d"))) # define minimum and maximum dates in presence data to match pant grunt data

WaibiraIDsPresent<-WaibiraIDsPresent[!duplicated(WaibiraIDsPresent$Date),] # remove duplicates generated through cleaning process

# remove cases where winners and losers were the same  (data input errors)
elo.data$Winner<-as.character(elo.data$Winner) # first need to format this column to character
elo.data$Loser<-as.character(elo.data$Loser)

elo.data<-elo.data[elo.data$Winner!=elo.data$Loser,] # remove rows where winner and loser were same ID
elo.data<-elo.data[order(elo.data$Date),] # order dataset by date (not necessary just easier to look at)

# run elo-rating using EloRating package
eloWaibira<-elo.seq(winner=elo.data$Winner, loser=elo.data$Loser, Date=elo.data$Date, draw = NULL, presence = WaibiraIDsPresent, startvalue = 1000, k = 50, normprob = TRUE,
              init = "average", intensity = NULL, iterate = 0, runcheck = T, progressbar = T)

# Cite the packages:
citation("EloRating")
help(package = "EloRating") # version

# plot the results
eloplot(eloWaibira, from = "2016-01-01", to = "2017-10-31", ids = c("BEN", "URS", "ATA", "TRS", "AKL", "KEV", "CHN", "ALF", "MAP", "TAL"))

?eloplot
# check for any issues with the data or script
seqcheck(winner=elo.data$Winner, loser=elo.data$Loser, Date=elo.data$Date, draw = NULL, presence = WaibiraIDsPresent)

# extract the standardised ratings for all individuals for earliest month data used in models 3 & 4 (first date for data used in models 3&4):
elo_listWaibira_Start<-extract_elo(eloobject = eloWaibira, extractdate = "2016-01-13", standardize = T, IDs = NULL, daterange = 1)
elo_listWaibira_Start
# extract the standardised ratings for all individuals for middle of the data collection (median date for data used in models 3&4):
elo_listWaibira_Mid<-extract_elo(eloobject = eloWaibira, extractdate = "2017-03-17", standardize = T, IDs = NULL, daterange = 1)
elo_listWaibira_Mid
# extract the standardised ratings for all individuals for end of the data collection (last date for data used in models 3&4):
elo_listWaibira_End<-extract_elo(eloobject = eloWaibira, extractdate = "2017-10-08", standardize = T, IDs = NULL, daterange = 1)
elo_listWaibira_End

write.csv(elo_listWaibira_Start, file = "Elo rating_Waibira start.csv")
write.csv(elo_listWaibira_Mid, file = "Elo rating_Waibira middle.csv")
write.csv(elo_listWaibira_End, file = "Elo rating_Waibira end.csv")

############################

#ELO RATINGS FOR SONSO ALPHA MALE

############################
## Elo-rating of males to determine the alpha male Sonso ----
# Files needed: 
# Sonso_PantGrunts_Male only.csv

# import necessary files
SonsoPantGrunts<-read.csv("Sonso_PantGrunts_Male only.csv", header = T, stringsAsFactors = T, sep = ",")
elo.dataSonso <- SonsoPantGrunts
elo.dataSonso$Date<-as.Date(SonsoPantGrunts$Date, format = "%d/%m/%Y")

# remove cases where winners and losers were the same individual
elo.dataSonso$Winner<-as.character(elo.dataSonso$Winner) # first need to format this column to character
elo.dataSonso$Loser<-as.character(elo.dataSonso$Loser)
elo.dataSonso<-elo.dataSonso[elo.dataSonso$Winner!=elo.dataSonso$Loser,] # remove rows where winner and loser were same ID
elo.dataSonso<-elo.dataSonso[order(elo.dataSonso$Date),] # order dataset by date (not necessary just easier to look at)

# run elo-rating using EloRating package
eloSonso<-elo.seq(winner=elo.dataSonso$Winner, loser=elo.dataSonso$Loser, Date=elo.dataSonso$Date, draw = NULL, startvalue = 1000, k = 50, normprob = TRUE,
              init = "average", intensity = NULL, iterate = 0, runcheck = T, progressbar = T)

# plot the results
eloplot(eloSonso, from = "2016-01-01", to = "2017-07-01", ids = c("HW", "MS", "FK", "NK", "SQ", "KT", "SM", "ZL", "ZF", "PS"))

# check for any issues with the data or script
seqcheck(winner=elo.dataSonso$Winner, loser=elo.dataSonso$Loser, Date=elo.dataSonso$Date, draw = NULL)

# extract the standardised ratings for all individuals for start of the data collection (first date for data used in models 3&4:
elo_listSonso_Start<-extract_elo(eloobject = eloSonso, extractdate = "2016-02-16", standardize = T, IDs = NULL, daterange = 1)
elo_listSonso_Start
#extract the standardised ratings for all individuals for mid of the data collection (median date for data used in models 3&4):
elo_listSonso_Mid<-extract_elo(eloobject = eloSonso, extractdate = "2016-03-17", standardize = T, IDs = NULL, daterange = 1)
elo_listSonso_Mid
#extract the standardised ratings for all individuals for end of the data collection (last date for data used in models 3&4):
elo_listSonso_End<-extract_elo(eloobject = eloSonso, extractdate = "2016-06-04", standardize = T, IDs = NULL, daterange = 1)
elo_listSonso_End

write.csv(elo_listSonso_Start, file = "Elo rating_Sonso start.csv")
write.csv(elo_listSonso_Mid, file = "Elo rating_Sonso middle.csv")
write.csv(elo_listSonso_End, file = "Elo rating_Sonso end.csv")

############################################

#IMPORT DATA AND PREPARE FOR MODEL 1

############################################
# Files needed: 
# Holden_PanthootData_Model 1&2.xlsx
#### Set working directory as appropriate

#Amount of data summary stats
# Import data: 
ImportedData <- read_excel("Holden_PanthootData_Model 1&2.xlsx", sheet="Sheet1")
ImportedDataNumberPHs <- ImportedData[, c("Community", "ChimpID", "Sex", "FAI", "Duration_Hrs", "Number_Recieved", "Number_Produced", "To_remove")]

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

# Summary of number of data points and descriptive statistics for total time of focal follows split by sex & community
dataframe=ImportedDataNumberPHs
dataframe$Group1=ImportedDataNumberPHs$SexCommunity
dataframe$dependent=ImportedDataNumberPHs$Duration_Hrs
DurationTotalBySexCommuity<- dataframe %>% group_by(Group1) %>%  summarise(NumberFollows = n(), SumDuration = sum(dependent),)
View(DurationTotalBySexCommuity) #should be  Male: N (focal follow number) = 59, total hours focal follow i.e. 'sum' = 171.3 hrs ; female sonso : N = 29, sum = 165.1 hrs; female waibira n=118, sum = 279.2 

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

# Summary of number of data points and descriptive statistics for number pant-hoots produced (mean, SD, min, max, median, quartiles, ...):
dataframe=NumberPHs_femVmal
dataframe$dependent=NumberPHs_femVmal$Number_Produced
dataframe$Group1=NumberPHs_femVmal$Sex
SummaryNumProdFemVMal<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent[!is.na(dependent)]), 
															MeanNumberProduced = mean(dependent, na.rm=TRUE), 
															sd = sd(dependent, na.rm=TRUE), 
															min = min(dependent, na.rm=TRUE),
															max = max(dependent, na.rm=TRUE), 
															median = median(dependent, na.rm=TRUE), 
															qone = quantile(dependent, .25, na.rm=TRUE), 
															qthree = quantile(dependent, .75, na.rm=TRUE))
															
SummaryNumProdFemVMal$sterr <- SummaryNumProdFemVMal$sd/(sqrt(SummaryNumProdFemVMal$NumberFocalFollows))
View(SummaryNumProdFemVMal)    #should be  Male: Number follows = 51, mean number PHs produced = 5.5; female: N = 118, mean number PHs produced = 1.8 

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
NProd_Sex_NoMin_Pois=glmer(Number_Produced ~ Sex*Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID),
	family=poisson, data=NumberPHs_femVmal)

# Model checks: NProd_Sex_NoMin_Pois
# Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Sex_NoMin_Pois) 
check_overdispersion(NProd_Sex_NoMin_Pois) # Over-dispersion detected: dispersion ratio = 1.979; Pearson's Chi-Squared = 322.642; p-value = < 0.001

# Negative binomial model to address zero inflation:
NProd_Sex_NoMin_NB=glmer.nb(Number_Produced ~ Sex*Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumberPHs_femVmal)
#summary(NProd_Sex_NoMin_NB)	

##############

#FINAL MODEL 1

##############
# Model after removing non-significant interactions:
NProd_Sex_NB.2=glmer.nb(Number_Produced ~ Sex + Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumberPHs_femVmal)
summary(NProd_Sex_NB.2)	

##############

#MODEL 1 CHECKS

##############
# Model checks: NProd_Sex_NB.2
# Over-dispersion: 
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Sex_NB.2) #dispersion parameter close to 1, no concern
check_overdispersion(NProd_Sex_NB.2)# No over-dispersion detected: dispersion ratio = 1.015; Pearson's Chi-Squared = 165.437; p-value = 0.432

# Collinearity:
ranef.diagn.plot(NProd_Sex_NB.2)
check_collinearity(NProd_Sex_NB.2)
NProd_Sex_NB.2_colinearitycheck <-  check_collinearity(NProd_Sex_NB.2) #max VIF = 1.32, no concern

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
StabilityPlot_NProd_Sex_NB.2 <- m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_NProd_Sex_NB.2

# Negative binomial null model: 
NProd_Sex_NoMin_NB_Null=glmer.nb(Number_Produced ~ FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumberPHs_femVmal)
#summary(NProd_Sex_NB_Null)	

# Compare full model with null model:
NullCompare_NProd_Sex_NB.2 <- as.data.frame(anova(NProd_Sex_NB.2, NProd_Sex_NoMin_NB_Null, test="Chisq")) 
NullCompare_NProd_Sex_NB.2   # Chisq=  41.1 ; DF= 2 ; Pr(>Chisq):1.167e-09 
NullCompare_NProd_Sex_NB.2Dataframe <- as.data.frame(NullCompare_NProd_Sex_NB.2)
	
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

# Conditional and Marginal R squared:
ModelPerformance_NProd_Sex_NB.2 <- model_performance(NProd_Sex_NB.2) 
ModelPerformance_NProd_Sex_NB.2 # R^2 cond = 0.369, R^2 marg = 0.317 
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

##############

#MODEL 1 PLOTS

##############
## Plotting: ----
# Summary plots of effects: 
plot(allEffects(NProd_Sex_NB.2))
Model1_AllEffectsPlot <- plot(allEffects(NProd_Sex_NB.2))
Model1_AllEffectsPlot 

# Plot Figure 1:
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

NumberPHs_femVmal$ProducedPerHr <- NumberPHs_femVmal$Number_Produced/NumberPHs_femVmal$Duration_Hrs

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

# Summary of number pant-hoots for IDs; to create means & SD for descriptives:
dataframe=NumberPHs_femVmal
dataframe$dependent=NumberPHs_femVmal$ProducedPerHr
dataframe$Group1=NumberPHs_femVmal$ChimpID
SummaryNumProdFemVMal<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent[!is.na(dependent)]), 
															MeanProducedPerHr = mean(dependent, na.rm=TRUE), 
															Sex = first(Sex)
															)
# Split values for points by group:
SummaryNumProdFemVMalM <-subset(SummaryNumProdFemVMal, Sex == "m")
SummaryNumProdFemVMalF <-subset(SummaryNumProdFemVMal, Sex == "f")
FMean <- mean(SummaryNumProdFemVMalF$MeanProducedPerHr)
MMean <- mean(SummaryNumProdFemVMalM$MeanProducedPerHr)
MSD <-  sd(SummaryNumProdFemVMalM$MeanProducedPerHr)
FSD <-  sd(SummaryNumProdFemVMalF$MeanProducedPerHr)

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
MLower <- SexeffectsLower$SexeffectsSummary.lower[1]
MUpper <- SexeffectsUpper$SexeffectsSummary.upper[1]

FEst <- SexeffectsEffect$SexeffectsSummary.effect[2]
FLower <- SexeffectsLower$SexeffectsSummary.lower[2]
FUpper <- SexeffectsUpper$SexeffectsSummary.upper[2]

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
segments(x0=1, x1=2, y0=FEst, y1=FEst, lwd=3, lend=1)
segments(x0=3, x1=4, y0=MEst, y1=MEst, lwd=3, lend=1)

# Add max and min bars:
xcoordf=1.5
xcoordm= 3.5
arrows(xcoordf, FLower, xcoordf, FUpper, code= 3, length=0.05, angle=90)
arrows(xcoordm, MLower, xcoordm, MUpper, code= 3, length=0.05, angle=90)

# Add horizontal bars for mean:
segments(x0=1, x1=2, y0=FMean, y1=FMean, lwd=3, lend=1, lty=2)
segments(x0=3, x1=4, y0=MMean, y1=MMean, lwd=3, lend=1, lty=2)

Model1_FinalPlot <- recordPlot()

##############

#MODEL 1 SAVE OUTPUTS

##############
#Save outputs to new location
#Set working directory as preferred 
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

#Save data descriptives
write.csv(SummaryNumProdFemVMal, file = "Model1_SummaryNumProd.csv")
write.csv(SummaryHrsFemVMal, file = "Model1_SummaryHrs.csv")
 
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

# Save summary plots of effects as PNG file
png("Model1_FinalPlot.png")
Model1_FinalPlot 
dev.off()


############################################

#IMPORT DATA AND PREPARE FOR MODEL 2

############################################

# Files needed: 
# Holden_PanthootData_Model 1&2.xlsx
#### Set working directory as appropriate
#call MASS
library(MASS)

#Amount of data summary stats
# Import data: 
ImportedData <- read_excel("Holden_PanthootData_Model 1&2.xlsx", sheet="Sheet1")
ImportedDataNumberPHs <- ImportedData[, c("Community", "ChimpID", "Sex", "FAI", "Duration_Hrs", "Number_Recieved", "Number_Produced", "To_remove")]

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

# Set reference levels for categorical variables:
ImportedDataNumberPHs_Fems$Community <- relevel(ImportedDataNumberPHs_Fems$Community, ref = "sonso")

# Describe structure of variables (check): 
str(ImportedDataNumberPHs_Fems$Community) # Factor w/ 2 levels "sonso","waibira"
str(ImportedDataNumberPHs_Fems$ChimpID) # Factor w/ 30 levels
str(ImportedDataNumberPHs_Fems$FAI) # Number
str(ImportedDataNumberPHs_Fems$Duration_Hrs) # Number
str(ImportedDataNumberPHs_Fems$Number_Recieved) # Number
str(ImportedDataNumberPHs_Fems$Number_Produced) # Number

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

# Summary of number of data points and descriptive statistics of pant-hoots produced by community (mean, SD, min, max, median, quartiles, ...):
dataframe <- NumPHs_fem
dataframe$dependent <- NumPHs_fem$Number_Produced
dataframe$Group1 <- NumPHs_fem$Community
SummaryNumProdDatafrmFems <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent[!is.na(dependent)]), 
															MeanNumberProduced = mean(dependent, na.rm=TRUE), 
															sd = sd(dependent, na.rm=TRUE), 
															min = min(dependent, na.rm=TRUE),
															max = max(dependent, na.rm=TRUE), 
															median = median(dependent, na.rm=TRUE), 
															qone = quantile(dependent, .25, na.rm=TRUE), 
															qthree = quantile(dependent, .75, na.rm=TRUE))
										SummaryNumProdDatafrmFems$sterr <- SummaryNumProdDatafrmFems$sd/(sqrt(SummaryNumProdDatafrmFems$NumberFocalFollows ))
View(SummaryNumProdDatafrmFems) # number of focal follows: Sonso = 29; Waibira = 89

NumPHs_fem$ProducedPerHour <- NumPHs_fem$Number_Produced/NumPHs_fem$Duration_Hrs

# Summary of number of data points and descriptive statistics of pant-hoots produced per hour for each individual (mean, SD):
dataframe <- NumPHs_fem
dataframe$dependent <- NumPHs_fem$ProducedPerHour
dataframe$Group1 <- NumPHs_fem$ChimpID
SummaryNumProdPerHourPerID <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), 
															NumberFocalFollows = length(dependent[!is.na(dependent)]), 
															MeanNumberProducedPerHour = mean(dependent, na.rm=TRUE), 
															sd = sd(dependent, na.rm=TRUE), 
															Community = first(Community),
															)
View(SummaryNumProdPerHourPerID) 

# Summary of mean amount pant-hoots per hour by community:
dataframe <- SummaryNumProdPerHourPerID
dataframe$dependent <- SummaryNumProdPerHourPerID$MeanNumberProducedPerHour
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
	+ (1|ChimpID),
	family=poisson, data=NumPHs_fem)

# check Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Community_Pois) #  chisq 255.4106; df  112;  P 3.28404e-13 ; dispersion.parameter 2.280452
check_overdispersion(NProd_Community_Pois) # Over-dispersion detected: dispersion ratio = 2.280; Pearson's Chi-Squared = 255.411; p-value = < 0.001

# Negative binomial model to address zero inflation:
NProd_Community_NB=glmer.nb(Number_Produced ~ Community*Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_fem)
#summary(NProd_Community_NB)	

# Negative binomial model after removing non-significant interacitons:
NProd_Community_NB.2=glmer.nb(Number_Produced ~ Community + Number_RecievedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_fem)
summary(NProd_Community_NB.2)	

#variance explained by ID is negligable. so run as glm instead of glmm
NProd_Community_GLM.1 <- glm.nb(Number_Produced ~ Community*Number_RecievedCentered + FAI_Centered + offset(log(Duration_Hrs)),
                              data = NumPHs_fem)
summary(NProd_Community_GLM.1)

##############

#FINAL MODEL 2

##############
# interaction non significant so remove
NProd_Community_GLM.2 <- glm.nb(Number_Produced ~ Community + Number_RecievedCentered + FAI_Centered + offset(log(Duration_Hrs)),
                              data = NumPHs_fem)
summary(NProd_Community_GLM.2)

##############

#MODEL 2 CHECKS

##############
#check for overdispersion	
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Community_GLM.2) #  chisq  115.5181; df  113 ;  P 0.4166178  ; dispersion.parameter     1.022284
check_overdispersion(NProd_Community_GLM.2)# no overdispersion detected

#continue model checks for negative binomial model 
check_collinearity(NProd_Community_GLM.2) # LOW  OK (max VIF= 1.28)

check_model_NProd_Community_GLM.2 <- check_model(NProd_Community_GLM.2) 
check_model_NProd_Community_GLM.2 
check_outliers(NProd_Community_GLM.2) # OK: No outliers detected. Based on the following method and threshold: cook (0.844).

#Negative binomial null model for Community dif:
NProd_Community_NB_Null <- glm.nb(Number_Produced ~ FAI_Centered + offset(log(Duration_Hrs)),
                              data = NumPHs_fem)
#summary(NProd_Community_NB_Null)

#compare full Community to null model
NullCompare_NProd_Community_GLM.2 <- as.data.frame(anova(NProd_Community_GLM.2, NProd_Community_NB_Null, test="Chisq")) 
NullCompare_NProd_Community_GLM.2#Chisq= 21.31576; DF= 2 ; Pr(>Chisq): 2.351477e-05


#results individual predictors (to extract p-values and significance test) and write sig test results to csv:
SigTests_NProd_Community_GLM.2<-as.data.frame(drop1(NProd_Community_GLM.2, test="Chisq"))
SigTests_NProd_Community_GLM.2<-round(SigTests_NProd_Community_GLM.2, 3)
SigTests_NProd_Community_GLM.2

# extract Estimates:
Estimates_NProd_Community_GLM.2 <- round(summary(NProd_Community_GLM.2)$coefficients, 3)
Estimates_NProd_Community_GLM.2

# Conditional and marginal R squared:
ModelPerformance_NProd_Community_GLM.2 <- model_performance(NProd_Community_GLM.2) 
ModelPerformance_NProd_Community_GLM.2 # Nagelkerke's R2 = 0.295 

# How many observations per estimated item (check complexity of model):
length(residuals(NProd_Community_GLM.2)/4) # 118

#Plots: 
plot(allEffects(NProd_Community_GLM.2)) #Plot exported as PNG named "NProd_Community_GLM.2_AllEffects plot"

# Confidence intervals:
CI_NProd_Community_GLM.2 <- confint(NProd_Community_GLM.2, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
CI_NProd_Community_GLM.2

##############

#MODEL 2 SAVE OUTPUTS

##############
#Save outputs to new location 
#set working directory as appropriate
ImportedDataNumberPHs_FemsChimpID <- as.data.frame(table(ImportedDataNumberPHs_Fems$ChimpID))
write.csv(ImportedDataNumberPHs_FemsChimpID, file = "DataCollected_NumFollowsPerChimpID_Females.csv")

write.csv(CollectedDurationTotalByCommuity, file = "DataCollected_DurationFollowsByCommuity_Females.csv")

write.csv(CollectedIDsbyCommunity, file = "DataCollected_NumberChimpsPerCommunity_Females.csv")

DataUsed_NumPHs_fem <- as.data.frame(DataUsed_NumPHs_fem)
write.csv(DataUsed_NumPHs_fem, file = "DataUsed_Model2_NumberFollowsPerChimpID.csv")

write.csv(SummaryNumProdDatafrmFems, file = "DataUsed_Model2_NumberProducedSummary.csv")

write.csv(SummaryHrsDatafrmFems, file = "DataUsed_Model2_DurationFollowsPerCommunity.csv")

write.csv(UsedFemIDsbyCommunity , file = "DataUsed_Model2_NumberChimpsPerCommunity.csv")

#Save outputs to new location
write.csv(NullCompare_NProd_Community_GLM.2, file = "Model2_NullComparison.csv")

write.csv(SigTests_NProd_Community_GLM.2, file = "Model2_SigTests.csv")

write.csv(Estimates_NProd_Community_GLM.2, file = "Model2_Estimates.csv")

write.csv(ModelPerformance_NProd_Community_GLM.2, file = "Model2_Performance.csv")

write.csv(CI_NProd_Community_GLM.2, file = "Model2_ConfidenceIntervals.csv")

#__________________________________________________
#Save text files
# Open a connection to a text file, save text from final model summary, overdisp tests, colinearity and VIF values
sink("Model2_SummaryOutput_NProd.txt")
print("Model2 Summary")
print("summary(NProd_Community_GLM.2)")
print(summary(NProd_Community_GLM.2))
print("____________________________________________________________________________")
print("Model2 Overdispersion test output")
print("from source diagnostic_fcns.r")
print("overdisp.test(NProd_Community_GLM.2)")
print(overdisp.test(NProd_Community_GLM.2))
print("check_overdispersion(NProd_Community_GLM.2)")
print(check_overdispersion(NProd_Community_GLM.2))
print("____________________________________________________________________________")
print("Model2 Colinearity check output")
print("check_collinearity(NProd_Community_GLM.2)")
print(check_collinearity(NProd_Community_GLM.2))
print("____________________________________________________________________________")
print("Model2 Outliers check output")
print("check_outliers(NProd_Community_GLM.2)")
print(check_outliers(NProd_Community_GLM.2))
print("____________________________________________________________________________")
sink()

#___________________________________________________
#Save figures
# Save summary plots of effects as PNG file
png("Model2_AllEffectsPlot.png")
plot(allEffects(NProd_Community_GLM.2))
dev.off()


############################################

#IMPORT DATA AND PREPARE FOR MODEL 3

############################################
# Files needed: 
# Holden_PanthootData_Model 3&4.xlsx
#Set working directory as appropriate

#Amount of data summary stats
# Import data: 
ImportedData<- read_excel("Holden_PanthootData_Model 3&4.xlsx", sheet = "Sheet1")
ImportedDataPHResponses <- ImportedData[, c("Community", "ChimpID", "FocalFollowNum", "Sex", "Age", "Activity", "FoodPart", "Hr", "FocalPhResp", "OpportunitiesToResp", "Drum", "InPartyPh", "AlphaPresent", "IndepIndivs")]

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

ImportedDataPHResponses$IndepIndivs <- as.numeric(ImportedDataPHResponses$IndepIndivs)
ImportedDataPHResponses$Age <-as.numeric(ImportedDataPHResponses$Age)
ImportedDataPHResponses$Hr <-as.numeric(ImportedDataPHResponses$Hr)

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

#merge prev two dataframes
DataCollected_NumberChimpsPerSexCommunity$NumberDataPoints <- DataCollected_NumDatapointsPerCommunitySex$NumberDataPoints

#Remove rows with missing data (e.g. Ben who is alpha male, who will not have data for is alpha present, others with missing activity etc)
PHootsRecieved  <-  droplevels(na.omit(ImportedDataPHResponses))
nrow(PHootsRecieved) # should be 2115

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
table(PHootsRecieved$FocalPhResp, PHootsRecieved$IndepIndivs) # tail for large group sizes, low sample size of pant-hoot responses from 15+ likely due to low sample, consider excluding cases
plot(table(PHootsRecieved$IndepIndivs))

#subset for only when party size has less than 15 independent individuals
PHootsRecievedForModel3 <-subset(PHootsRecieved, IndepIndivs<15)
nrow(PHootsRecievedForModel3) # should be 2079

#Recheck data distributions:
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$ChimpID) #Most IDs well represented and most with no response have plenty datapoints
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$FocalFollowNum) #most are well represented, overall sample larger than ChimpID, more with lower N but okay
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Hr) #slight skew to earlier in day but okay
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$AlphaPresent) #okay
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$InPartyPh) #okay
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Activity) #okay
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$FoodPart) # lower representation of ripe fruit, but okay
table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$IndepIndivs) 
plot(table(PHootsRecievedForModel3$IndepIndivs)) #tail no longer a problem

table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Sex) # okay
DataForModel3_PHootsRecievedBySex <- table(PHootsRecievedForModel3$FocalPhResp, PHootsRecievedForModel3$Sex)
DataForModel3_PHootsRecievedBySex <- data.frame(DataForModel3_PHootsRecievedBySex)

# Number of 1s and 0s in the outcome overall:
table(PHootsRecievedForModel3$FocalPhResp) # 0 = 1766, 1 = 313 

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
View(DataForModel3_NumDatapointsPerCommunitySex) # should be f sonso=484; f waibira=648; m waibira=947

# Number of IDs by sex & community: 
DataForModel3_NumIDsPerCommunitySex <- PHootsRecievedForModel3 %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(SexCommunity) %>%
     summarise(NumberChimps = n())
View(DataForModel3_NumIDsPerCommunitySex) # Should be: 10 sonso females; 18 waibira females; 15 waibira males
#merge prev two dataframes
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
print(DataForModel3_NumberChimpsBySex) # females: N = 28, Males: N = 15
DataForModel3_NumberChimpsBySex <- data.frame(DataForModel3_NumberChimpsBySex)

############################################

#RUN MODEL 3

############################################
# Model 3: Which factors predict pant-hoot responses in male and female chimpanzees? ------
contr=glmerControl(optCtrl = list(maxfun=100000), calc.derivs=F, optimizer = "nloptwrap")

#model with all interactions
Model3_ResponsePHsBySex.1 =glmer(FocalPhResp ~ Sex*AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + Sex*(Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.1)

#rerun without least significant interaction: Sex*(Activity/FoodPart) 
Model3_ResponsePHsBySex.2 =glmer(FocalPhResp ~ Sex*AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.2)

#rerun without least significant interaction: Sex*Age 
Model3_ResponsePHsBySex.3 =glmer(FocalPhResp ~ AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Sex*Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.3)

#rerun without least significant interaction: Sex*Drum
Model3_ResponsePHsBySex.4 =glmer(FocalPhResp ~ AgeCentered  + Sex*AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.4)

##############

#FINAL MODEL 3

##############
#rerun without least significant interaction: Sex*alphapresent
Model3_ResponsePHsBySex.5 =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3, family=binomial, control=contr)
summary(Model3_ResponsePHsBySex.5)

#Final full model = Model3_ResponsePHsBySex.5 as remaining interactions are significant

##############

#MODEL3 CHECKS

##############
#model without interactions (for collinearity calcs)
Model3_ResponsePHsBySex_NoInteracts =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
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
ranef.diagn.plot(Model3_ResponsePHsBySex.5) #fine
check_collinearity(Model3_ResponsePHsBySex_NoInteracts)#max VIF = 2.62 ok
Model3_ResponsePHsBySex.5_collinearitycheck <-  data.frame(check_collinearity(Model3_ResponsePHsBySex_NoInteracts))

# General checks:
check_model(Model3_ResponsePHsBySex.5) 
check_model_Model3_ResponsePHsBySex.5 <- check_model(Model3_ResponsePHsBySex.5) 
check_model_Model3_ResponsePHsBySex.5 
check_outliers_Model3_ResponsePHsBySex.5 <- check_outliers(Model3_ResponsePHsBySex.5)
check_outliers_Model3_ResponsePHsBySex.5  # 10 outliers detected: cases 665, 933, 1444, 1445, 1541, 1613, 1770, 1811, 1961, 2049. Based on the following method and tHreshold: cook (0.839).

# Stability:
source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
full.stab=glmm.model.stab(model.res=Model3_ResponsePHsBySex.5, data=PHootsRecievedForModel3,contr=contr)

# check for convergence issues these are indicated separately for lme4 (column lme4.warnings) and the optimizer opt.warnings): 
table(full.stab$detailed$lme4.warnings) 
table(full.stab$detailed$opt.warnings) 

M3x<-round(full.stab$summary[, -1], 3) 
StabilityPlot_Model3_ResponsePHsBySex.5 <- m.stab.plot(full.stab$summary[, -1]) 
StabilityPlot_Model3_ResponsePHsBySex.5

###Model perameters	
# Full model vs. null model comparison:
Model3_ResponsePHsBySex_NullCompare <- as.data.frame(anova(Model3_ResponsePHsBySex.5, Model3_ResponsePHsBySexNull, test="Chisq")) 
Model3_ResponsePHsBySex_NullCompare # p value:2.513581e-45 , Chi2 = 237.3109  df = 10  

#Extract model estimates
Model3_ResponsePHsBySex.5_Estimates <- round(summary(Model3_ResponsePHsBySex.5)$coefficients, 5)
Model3_ResponsePHsBySex.5_Estimates <- data.frame(Model3_ResponsePHsBySex.5_Estimates)
Model3_ResponsePHsBySex.5_Estimates

# Results of individual predictors (to extract p-values and significance test):
Model3_ResponsePHsBySex.5_SigTests <-as.data.frame(drop1(Model3_ResponsePHsBySex.5, test="Chisq"))
Model3_ResponsePHsBySex.5_SigTests <-round(Model3_ResponsePHsBySex.5_SigTests, 5)
Model3_ResponsePHsBySex.5_SigTests 

# Contribution of random effects: 
summary(Model3_ResponsePHsBySex.5)$varcor
Model3_ResponsePHsBySex.5_RandomEffectContribution <- data.frame(summary(Model3_ResponsePHsBySex.5)$varcor)
Model3_ResponsePHsBySex.5_RandomEffectContribution

# Number of levels per grouping:
summary(Model3_ResponsePHsBySex.5)$ngrps #159 focal follows, 43 chimps
Model3_ResponsePHsBySex.5_RandomEffectLevels <- data.frame(summary(Model3_ResponsePHsBySex.5)$ngrps)
Model3_ResponsePHsBySex.5_RandomEffectLevels

# Conditional and Marginal R squared:
model_performance(Model3_ResponsePHsBySex.5)
Model3_ResponsePHsBySex_ModelPerformance <- data.frame(model_performance(Model3_ResponsePHsBySex.5))

# Model complexity (How many observations per estimated item):
length(residuals(Model3_ResponsePHsBySex.5))/
  (length(fixef(Model3_ResponsePHsBySex.5))+
     nrow(as.data.frame(summary(Model3_ResponsePHsBySex.5)$varcor))) 
	 
Summary_Model3_ResponsePHsBySex <- summary(Model3_ResponsePHsBySex.5)

# Confidence intervals: 
Model3_ResponsePHsBySex_ConfidenceIntervals  <- confint(Model3_ResponsePHsBySex.5, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
Model3_ResponsePHsBySex_ConfidenceIntervals

##############

#MODEL 3 POST HOC MODELS

##############
### Post-hoc tests to interpret significant interactions: ----
# Create new datasets one for female and one for male:
PHootsRecievedForModel3F <- subset(PHootsRecievedForModel3, Sex == "f")
PHootsRecievedForModel3M <- subset(PHootsRecievedForModel3, Sex == "m")

# Model but with female and male data separately:
Model3_ResponsePHsBySex_FemalePostHoc =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3F , family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_FemalePostHoc)

Model3_ResponsePHsBySex_MalePostHoc =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
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
#create datafram which excludes 10 outliers: cases 665, 933, 1444, 1445, 1541, 1613, 1770, 1811, 1961, 2049
exclude_outliers <- c(665, 933, 1444, 1445, 1541, 1613, 1770, 1811, 1961, 2049)
PHootsRecievedForModel3NoOutliers <- PHootsRecievedForModel3[-exclude_outliers, ]
PHootsRecievedForModel3OnlyOutliers <- PHootsRecievedForModel3[exclude_outliers, ]

#check number rows after ID removal
nrow(PHootsRecievedForModel3NoOutliers) # Should be N = 2069

#run version without outliers to see impact
Model3_ResponsePHsBySex_NoOutliers =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + Sex*IndepIndivsCentered  + Sex*InPartyPh + Drum + (Activity/FoodPart)
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
Model3_ResponsePHsBySex_FemalePostHocNoOutliers =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
				 + HrCentered + I(HrCentered^2) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel3NoOutliersF , family=binomial, control=contr)
summary(Model3_ResponsePHsBySex_FemalePostHocNoOutliers)

Model3_ResponsePHsBySex_MalePostHocNoOutliers =glmer(FocalPhResp ~ AgeCentered  + AlphaPresent + IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
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

write.csv(DataCollected_NumDatapointsPerChimpID, file = "DataCollected_PhResps_NumDatapointsPerChimpID.csv")
write.csv(DataCollected_NumberChimpsPerSexCommunity, file = "DataCollected_PhResps_NumberChimpsPerSexCommunity.csv")

#Save model checks and outputs
write.csv(DataForModel3_NumIDsPerCommunitySex, file = "DataForModel3_NumIDsPerCommunitySex.csv")
write.csv(DataForModel3_NumDatapointsPerChimpID, file = "DataForModel3_NumDatapointsPerChimpID.csv")
write.csv(DataForModel3_PHootsRecievedBySex, file = "DataForModel3_PHootsRecievedBySex.csv")
write.csv(DataForModel3_NumberChimpsBySex, file = "DataForModel3_NumberChimpsBySex.csv")
write.csv(Model3_ResponsePHsBySex.5_collinearitycheck, file = "Model3_ResponsePHsBySex.5_collinearitycheck.csv")
write.csv(Model3_ResponsePHsBySex.5_Estimates, file = "Model3_ResponsePHsBySex.5_Estimates.csv")
write.csv(Model3_ResponsePHsBySex_NullCompare, file = "Model3_ResponsePHsBySex_NullCompare.csv")
write.csv(Model3_ResponsePHsBySex.5_SigTests , file = "Model3_ResponsePHsBySex.5_SigTests.csv")
write.csv(Model3_ResponsePHsBySex.5_RandomEffectContribution, file = "Model3_ResponsePHsBySex.5_RandomEffectContribution.csv")
write.csv(Model3_ResponsePHsBySex.5_RandomEffectLevels, file = "Model3_ResponsePHsBySex.5_RandomEffectLevels.csv")
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


#Save text files
# Open a connection to a text file, save text from final model summary, overdisp tests, colinearity and VIF values
sink("Model3_SummaryOutput_NProd.txt")
print("Model3 Summary")
print("summary(Model3_ResponsePHsBySex.5)")
print(summary(Model3_ResponsePHsBySex.5))
print("____________________________________________________________________________")
print("Model3 Contribution of random effects")
print("summary(Model3_ResponsePHsBySex.5)$varcor")
print(summary(Model3_ResponsePHsBySex.5)$varcor)
print("____________________________________________________________________________")
print("Model3 Outliers check output")
print("check_outliers(Model3_ResponsePHsBySex.5)")
print(check_outliers(Model3_ResponsePHsBySex.5))
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
ranef.diagn.plot(Model3_ResponsePHsBySex.5)
dev.off()

# Save model checks plot as a PNG file
png("Model3_ModelCheckPlot.png")
check_model_Model3_ResponsePHsBySex.5 
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
Model3_ResponsePHsBySex.5_Plot1 <- interact_plot(Model3_ResponsePHsBySex.5,
									pred = "IndepIndivsCentered", modx = "Sex", 
									data = PHootsRecievedForModel3, 
									plot.points = F, 
									interval = T,
									int.type = "confidence", 
									geom = "line", vary.lty = T,
									x.label = "Context", y.label = "Probability of pant-hoot response", 
									colors = "Set2", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
		scale_y_continuous(breaks=seq(0, 0.25,0.05), limits = c(0, 0.26),expand = c(0, 0)) +
		scale_x_continuous(limits = c(-1.51, 3.0), breaks=seq(-1.51, 2.77,0.61),  labels =c(0, 2, 4, 6, 8, 10, 12, 14), expand = c(0, 0), name="Number individuals in party") +
		theme_classic() +
		theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
		theme(legend.position = "none")+
		My_label_2
Model3_ResponsePHsBySex.5_Plot1

#Interaction between in group pant-hoots and sex
Model3_ResponsePHsBySex.5_Plot2 <- cat_plot(Model3_ResponsePHsBySex.5,
									pred = "InPartyPh", modx = "Sex",
									data = PHootsRecievedForModel3,
									plot.points = F, interval = T,
									int.type = "confidence",
									geom = "line",vary.lty = T,
									x.label = "Pant-hoot recieved from within party", y.label = "Probability of pant-hoot response", 
									colors = "Set2", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
		scale_y_continuous(breaks=seq(0, 1,0.1), limits = c(0, 0.63),expand = c(0, 0)) +
		scale_x_discrete(breaks = c(0, 1), labels =c("No","Yes"))+
		theme_classic() +
		theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
		theme(legend.position = "none")+
		My_label_2
Model3_ResponsePHsBySex.5_Plot2

# Combine two plots:
Model3_FinalPlot <- ggarrange(Model3_ResponsePHsBySex.5_Plot1, Model3_ResponsePHsBySex.5_Plot2, ncol = 2, nrow = 1, labels = c("A", "B"), 
          font.label = list(size = 20, color = "black"), label.y = c(1.01, 1.01), label.x = c(-0.01, -0.01)) 
Model3_FinalPlot

ggsave("Model3_FinalPlotsBoth.png", plot = Model3_FinalPlot, scale = (300/96), width = 837, height= 574, units = c("px"), dpi = 300, limitsize=TRUE, bg="white")



############################################

#IMPORT DATA AND PREPARE FOR MODEL 4

############################################
# Files needed: 
# Holden_PanthootData_Model3&4.xlsx
# Set working directory as appropriate

#Model 4 Amount of data summary stats and data preparation
# Import data: 
ImportedData<- read_excel("Holden_PanthootData_Model 3&4.xlsx", sheet = "Sheet1")
ImportedDataPHResponsesFems <- ImportedData[, c("Community", "ChimpID", "FocalFollowNum", "Sex", "Age", "Activity", "FoodPart", "Hr", "FocalPhResp", "Parity", "MaleDepOffspring", "Oes4", "OpportunitiesToResp", "Drum", "InPartyPh", "AlphaPresent", "IndepIndivs")]

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

ImportedDataPHResponsesFems$IndepIndivs <- as.numeric(ImportedDataPHResponsesFems$IndepIndivs)
ImportedDataPHResponsesFems$Age <-as.numeric(ImportedDataPHResponsesFems$Age)
ImportedDataPHResponsesFems$Hr <-as.numeric(ImportedDataPHResponsesFems$Hr)

#Remove rows with missing data (this will automatically subset for females as no male individuals have data in the female-only columns, e.g. Oestrus)
PHootsRecievedForModel4  <-  droplevels(na.omit(ImportedDataPHResponsesFems))
nrow(PHootsRecievedForModel4) # should be 1134

#subset for only when party size has less than 15 independent individuals (same issue as for model 3 in that low representation of large party size)
PHootsRecievedForModel4 <-subset(PHootsRecievedForModel4, IndepIndivs<15)
nrow(PHootsRecievedForModel4) # should be 1112

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
table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$IndepIndivs) 
plot(table(PHootsRecievedForModel4$IndepIndivs)) #okay (a bit of a tail, but still responses all the way up)

table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Community) #okay
DataForModel4_PHootsRecievedByCommunity <- table(PHootsRecievedForModel4$FocalPhResp, PHootsRecievedForModel4$Community) 
DataForModel4_PHootsRecievedByCommunity <- data.frame(DataForModel4_PHootsRecievedByCommunity)

# Number of 1s and 0s in the outcome overall:
table(PHootsRecievedForModel4$FocalPhResp) # 0 =  968, 1 = 144

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
View(DataForModel4_NumDatapointsPerCommunity) # should be f sonso=464; f waibira=648

# Number of IDs by community: 
DataForModel4_NumIDsPerCommunity <- PHootsRecievedForModel4 %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(Community) %>%
     summarise(NumberChimps = n())
View(DataForModel4_NumIDsPerCommunity) # Should be: 9 sonso females; 18 waibira females;
#merge prev two dataframes
DataForModel4_NumIDsPerCommunity$NumberDataPoints <- DataForModel4_NumDatapointsPerCommunity$NumberDataPoints

# Set reference levels for categorical variables:
PHootsRecievedForModel4$FoodPart <- relevel(PHootsRecievedForModel4$FoodPart, ref = "NotFeedRipeFruit")
PHootsRecievedForModel4$Activity <- relevel(PHootsRecievedForModel4$Activity, ref = "NotFeed")
PHootsRecievedForModel4$Community <- relevel(PHootsRecievedForModel4$Community, ref = "sonso")

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
Model4_ResponsePHsByCommunity.1=glmer(FocalPhResp ~ Community*AgeCentered  + Community*AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Community*Drum + Community*(Activity/FoodPart)
				+ Community*Oes4 + Community*(Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.1)

#rerun without least significant interaction: Community*(Activity/FoodPart) 
Model4_ResponsePHsByCommunity.2=glmer(FocalPhResp ~ Community*AgeCentered  + Community*AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Community*Drum + (Activity/FoodPart)
				+ Community*Oes4 + Community*(Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.2)

#rerun without least significant interaction: Community*Oes4 
Model4_ResponsePHsByCommunity.3=glmer(FocalPhResp ~ Community*AgeCentered  + Community*AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Community*Drum + (Activity/FoodPart)
				+ Oes4 + Community*(Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.3)

#rerun without least significant interaction: Community*AlphaPresent
Model4_ResponsePHsByCommunity.4=glmer(FocalPhResp ~ Community*AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Community*Drum + (Activity/FoodPart)
				+ Oes4 + Community*(Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.4)

#rerun without least significant interaction: Community*(Parity/MaleDepOffspring)
Model4_ResponsePHsByCommunity.5=glmer(FocalPhResp ~ Community*AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Community*Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.5)

#rerun without least significant interaction: Community*AgeCentered
Model4_ResponsePHsByCommunity.6=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Community*Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.6)

#rerun without least significant interaction: Community*Drum
Model4_ResponsePHsByCommunity.7=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ Community*IndepIndivsCentered  + Community*InPartyPh + Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.7)

#rerun without least significant interaction: Community*IndepIndivsCentered
Model4_ResponsePHsByCommunity.8=glmer(FocalPhResp ~ AgeCentered  + AlphaPresent+ IndepIndivsCentered  + Community*InPartyPh + Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.8)

#rerun without least significant interaction: Community*InPartyPh
Model4_ResponsePHsByCommunity.9=glmer(FocalPhResp ~ Community + AgeCentered  + AlphaPresent+ IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID)+(1|FocalFollowNum),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.9)

# Check random effects:
summary(Model4_ResponsePHsByCommunity.9)$varcor # remove focal follow num as random effect due to est =0

##############

#FINAL MODEL 4

##############

Model4_ResponsePHsByCommunity.10=glmer(FocalPhResp ~ Community + AgeCentered  + AlphaPresent+ IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID),
              data=PHootsRecievedForModel4, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity.10)

#Final full model = Model4_ResponsePHsByCommunity.10 as no interactions were significant

# Null Model with only control variables and random effects:
Model4_ResponsePHsByCommunityNull =glmer(FocalPhResp ~ HrCentered + I(HrCentered^2) 
                + (1|ChimpID),
				data=PHootsRecievedForModel4, family=binomial, control=contr)

##############

#MODEL 4 CHECKS

##############
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
ranef.diagn.plot(Model4_ResponsePHsByCommunity.10) #ok
check_collinearity(Model4_ResponsePHsByCommunity.10) # LOW (max 3.54) OK. 
Model4_ResponsePHsByCommunity.10_collinearitycheck <-  data.frame(check_collinearity(Model4_ResponsePHsByCommunity.10))

check_model(Model4_ResponsePHsByCommunity.10)  
check_model_Model4_ResponsePHsByCommunity.10 <- check_model(Model4_ResponsePHsByCommunity.10) 
check_model_Model4_ResponsePHsByCommunity.10
check_outliers_Model4_ResponsePHsByCommunity.10 <- check_outliers(Model4_ResponsePHsByCommunity.10)
check_outliers_Model4_ResponsePHsByCommunity.10# 2 outliers detected: cases 578, 844. Based on the following method and threshold: cook (0.84).

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
#create dataframe which excludes 2 outliers: cases 578, 844
exclude_outliers <- c(578, 844)
PHootsRecievedForModel4NoOutliers <- PHootsRecievedForModel4[-exclude_outliers, ]
PHootsRecievedForModel4OnlyOutliers <- PHootsRecievedForModel4[exclude_outliers, ]

#check number rows after ID removal
nrow(PHootsRecievedForModel4NoOutliers) # Should be N = 1110

#run version without outliers to see impact
Model4_ResponsePHsByCommunity_NoOutliers =glmer(FocalPhResp ~ Community + AgeCentered  + AlphaPresent+ IndepIndivsCentered  + InPartyPh + Drum + (Activity/FoodPart)
				+ Oes4 + (Parity/MaleDepOffspring) 
				+ HrCentered + I(HrCentered^2) 
                + (1|ChimpID),
              data=PHootsRecievedForModel4NoOutliers, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity_NoOutliers)

#Null without outliers
Model4_ResponsePHsByCommunity_NoOutliersNull =glmer(FocalPhResp ~ HrCentered + I(HrCentered^2) +
                (1|ChimpID),
              data=PHootsRecievedForModel4NoOutliers, family=binomial, control=contr)
summary(Model4_ResponsePHsByCommunity_NoOutliersNull)

# Compare full no outlier model with null model:
Model4_ResponsePHsByCommunity_NoOutliers_NullCompare <- as.data.frame(anova(Model4_ResponsePHsByCommunity_NoOutliers, Model4_ResponsePHsByCommunity_NoOutliersNull, test="Chisq")) 
Model4_ResponsePHsByCommunity_NoOutliers_NullCompare  
Model4_ResponsePHsByCommunity_NoOutliers_NullCompare <- as.data.frame(Model4_ResponsePHsByCommunity_NoOutliers_NullCompare)

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
write.csv(DataForModel4_NumIDsPerCommunity, file = "DataForModel4_NumIDsPerCommunitySex.csv")
write.csv(DataForModel4_NumDatapointsPerChimpID, file = "DataForModel4_NumDatapointsPerChimpID.csv")
write.csv(DataForModel4_PHootsRecievedByCommunity, file = "DataForModel4_PHootsRecievedByCommunity.csv")
write.csv(Model4_ResponsePHsByCommunity.10_collinearitycheck, file = "Model4_ResponsePHsByCommunity.10_collinearitycheck.csv")
write.csv(Model4_ResponsePHsByCommunity.10_Estimates, file = "Model4_ResponsePHsByCommunity.10_Estimates.csv")
write.csv(Model4_ResponsePHsByCommunity_NullCompare, file = "Model4_ResponsePHsByCommunity_NullCompare.csv")
write.csv(Model4_ResponsePHsByCommunity.10_SigTests , file = "Model4_ResponsePHsByCommunity.10_SigTests.csv")
write.csv(Model4_ResponsePHsByCommunity.10_RandomEffectContribution, file = "Model4_ResponsePHsByCommunity.10_RandomEffectContribution.csv")
write.csv(Model4_ResponsePHsByCommunity.10_RandomEffectLevels, file = "Model4_ResponsePHsByCommunity.10_RandomEffectLevels.csv")
write.csv(Model4_ResponsePHsByCommunity_ModelPerformance, file = "Model4_ResponsePHsByCommunity_ModelPerformance.csv")

write.csv(Model4_ResponsePHsByCommunity_NoOutliers_NullCompare, file = "Model4_ResponsePHsByCommunity_NoOutliers_NullCompare.csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_SigTests , file = "Model4_ResponsePHsByCommunity_NoOutliers_SigTests .csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_Estimates, file = "Model4_ResponsePHsByCommunity_NoOutliers_Estimates.csv")

write.csv(Model4_ResponsePHsByCommunity.10_Confints, file = "Model4_ConfInts.csv")
write.csv(Model4_ResponsePHsByCommunity_NoOutliers_ConfInts, file = "Model4_NoOutliers_ConfInts.csv")

