# Analyses code for the Manuscript "The use of pant-hoot vocalisations in female chimpanzees of Budongo Forest" ----
# Authors: Eve Holden, Charlotte Grund, Robert Eguma, Liran Samuni, Klaus Zuberbühler, Adrian Soldati*, Catherine Hobaiter*
# Contact: eholden540@gmail.com 

# Files needed: 
# Data for Model 1 and 2 - female PH.xlsx
# Data for Model 3 and 4 - female PH.xlsx

# Packages used: 
library(EloRating); library(dplyr); library(lubridate); library(plot3D); library(dplyr); library(MASS); library(lme4); library(tidyverse); library(performance); library(readxl); 
library(car); library(effects); library(interactions); library(ggpubr); library(gdata); library(ggplot2); library(emmeans); library(jtools); library(ggdist)

# Parameter values used for plotting Results:
My_label_2 = theme(plot.title = element_text(size = 32, hjust = 0.5),axis.title.x = element_text(size = 22),axis.text.x = element_text(size = 22),axis.text.y = element_text(size = 22),axis.title.y = element_text(size = 22),
                   legend.key.size = unit(0.2, 'cm'), legend.key.height = unit(0.5, 'cm'), legend.key.width = unit(1.3, 'cm'), legend.title = element_text(size=14), legend.text = element_text(size=12)) 

# GLMM 1: Are there sex differences in the rate of pant-hoot production? ----
## Data preparation: ----
# Import data: 
ImportedData <- read_excel("Data for Model 1 and 2 - female PH.xlsx", sheet="Sheet 1")
ImportedDataNumPHs <- ImportedData[, c("Community", "ChimpID", "Sex", "FAI", "Duration_Hrs", "Num_Received", "Num_Produced", "To_remove")]

# Remove data to exclude from analyses:
NumPHs_femVmal.2<-subset(ImportedDataNumPHs, To_remove!="y")

# Apply 40minute minimum focal follow criteria:
NumPHs_femVmal.2<-subset(NumPHs_femVmal.2, Duration_Hrs>(2/3))

# Check number rows after focal follow removal:
nrow(NumPHs_femVmal.2) # Should be N = 169

# Format variables to include in model: 
NumPHs_femVmal.2$Community <- as.factor(NumPHs_femVmal.2$Community)
NumPHs_femVmal.2$ChimpID  <- as.factor(NumPHs_femVmal.2$ChimpID) 
NumPHs_femVmal.2$Sex <- as.factor(NumPHs_femVmal.2$Sex)
NumPHs_femVmal.2$FAI <- as.numeric(NumPHs_femVmal.2$FAI)
NumPHs_femVmal.2$Duration_Hrs <- as.numeric(NumPHs_femVmal.2$Duration_Hrs)
NumPHs_femVmal.2$Num_Received <- as.numeric(NumPHs_femVmal.2$Num_Received)
NumPHs_femVmal.2$Num_Produced <- as.numeric(NumPHs_femVmal.2$Num_Produced)

# Set reference levels for variables:
NumPHs_femVmal.2$Sex <- relevel(NumPHs_femVmal.2$Sex, ref = "m")
NumPHs_femVmal.2$Community <- relevel(NumPHs_femVmal.2$Community, ref = "sonso")

# Create new category incorporating sex and community variables:
NumPHs_femVmal.2$SexCommunity <- paste(NumPHs_femVmal.2$Sex, NumPHs_femVmal.2$Community)
NumPHs_femVmal.2$SexCommunity <- as.factor(NumPHs_femVmal.2$SexCommunity)

# Summary of number of data points and descriptive statistics (mean, SD, min, max, median, quartiles, ...):
dataframe=NumPHs_femVmal.2
dataframe$dependent=NumPHs_femVmal.2$Num_Produced
dataframe$Group1=NumPHs_femVmal.2$Sex
SummaryNumProdFemVMal<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), Nused = length(dependent[!is.na(dependent)]), mean = mean(dependent, na.rm=TRUE), sd = sd(dependent, na.rm=TRUE), min = min(dependent, na.rm=TRUE),max = max(dependent, na.rm=TRUE), median = median(dependent, na.rm=TRUE), qone = quantile(dependent, .25, na.rm=TRUE), qthree = quantile(dependent, .75, na.rm=TRUE))
SummaryNumProdFemVMal$sterr <- SummaryNumProdFemVMal$sd/(sqrt(SummaryNumProdFemVMal$Nused))
View(SummaryNumProdFemVMal)
dataframe$dependent=NumPHs_femVmal.2$Duration_Hrs
SummaryHrsFemVMal<- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), Sum = sum(dependent),)
View(SummaryHrsFemVMal)

# Describe structure of variables: 
str(NumPHs_femVmal.2$Community) # Factor w/ 2 levels "sonso","waibira"
str(NumPHs_femVmal.2$ChimpID) # Factor w/ 44 levels
str(NumPHs_femVmal.2$Sex) # Factor w/ 2 levels "m","f"
str(NumPHs_femVmal.2$FAI) # Number
str(NumPHs_femVmal.2$Duration_Hrs) # Number
str(NumPHs_femVmal.2$Num_Received) # Number
str(NumPHs_femVmal.2$Num_Produced) # Number

# Sample sizes: 
table(NumPHs_femVmal.2$SexCommunity) #num focal follows: f sonso=29; f waibira=89; m waibira=51

# Number of IDs: 
IDsbySexCommunity <- NumPHs_femVmal.2 %>%
     distinct(ChimpID, .keep_all = TRUE) %>%
     group_by(SexCommunity) %>%
     summarise(count = n())
IDsbySexCommunity # Should be: 10 sonso females; 19 waibira females; 15 waibira males

# Check outcome variable distribution: 
plot(table(NumPHs_femVmal.2$Num_Produced))

# Check representation of IDs (by num recieved and duration of follows):
table(NumPHs_femVmal.2$ChimpID)
plot(NumPHs_femVmal.2$ChimpID, NumPHs_femVmal.2$Num_Received)
plot(NumPHs_femVmal.2$ChimpID, NumPHs_femVmal.2$Duration_Hrs) 
plot(NumPHs_femVmal.2$ChimpID, NumPHs_femVmal.2$FAI) 

# Check variation of num recieved, duration and FAI by Sex and community: 
plot(NumPHs_femVmal.2$Sex,NumPHs_femVmal.2$Num_Received) 
plot(NumPHs_femVmal.2$Sex,NumPHs_femVmal.2$Duration_Hrs)
plot(NumPHs_femVmal.2$Sex,NumPHs_femVmal.2$FAI)

# Check range of focal follow duration: 
range(NumPHs_femVmal.2$Duration_Hrs)

# Standardise/center numeric predictors:
Num_ReceivedMean <- mean(NumPHs_femVmal.2$Num_Received)
Num_ReceivedSD <- sd(NumPHs_femVmal.2$Num_Received)
NumPHs_femVmal.2$Num_ReceivedCentered <-(NumPHs_femVmal.2$Num_Received-Num_ReceivedMean )/Num_ReceivedSD 

FAI_Mean <- mean(NumPHs_femVmal.2$FAI)
FAI_SD <- sd(NumPHs_femVmal.2$FAI)
NumPHs_femVmal.2$FAI_Centered <-(NumPHs_femVmal.2$FAI-FAI_Mean)/FAI_SD 

## Model: ----
# Control function fo models:
contr=glmerControl(optCtrl = list(maxfun=10000000))

# Poisson model:
NProd_Sex_Pois.2=glmer(Num_Produced ~ Sex*Num_ReceivedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID),
	family=poisson, data=NumPHs_femVmal.2)
summary(NProd_Sex_Pois.2)

### Model checks: ----
# Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Sex_Pois.2) 
check_overdispersion(NProd_Sex_Pois.2) # Over-dispersion detected: dispersion ratio = 1.979; Pearson's Chi-Squared = 322.642; p-value = < 0.001

# Negative binomial model to address zero inflation:
NProd_Sex_NB.2=glmer.nb(Num_Produced ~ Sex*Num_ReceivedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_femVmal.2)
summary(NProd_Sex_NB.2)	

# Model after removing non-significant interactions:
NProd_Sex_NB.3=glmer.nb(Num_Produced ~ Sex + Num_ReceivedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_femVmal.2)
summary(NProd_Sex_NB.3)	

# Over-dispersion: 
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Sex_NB.3)
check_overdispersion(NProd_Sex_NB.3)# No over-dispersion detected: dispersion ratio = 1.015; Pearson's Chi-Squared = 165.437; p-value = 0.432

# Collinearity:
ranef.diagn.plot(NProd_Sex_NB.3)
check_collinearity(NProd_Sex_NB.3)

# General checks:
check_model_NProd_Sex_NB.3 <- check_model(NProd_Sex_NB.3) 
check_model_NProd_Sex_NB.3 
check_outliers(NProd_Sex_NB.3)

# Stability:
source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
full.stabS=glmm.model.stab(model.res=NProd_Sex_NB.3, data=NumPHs_femVmal.2,contr=contr)
# check for convergence issues these are indicated separately for lme4 (column lme4.warnings) and the optimizer opt.warnings): 
table(full.stabS$detailed$lme4.warnings) 
table(full.stabS$detailed$opt.warnings) 

M1x<-round(full.stabS$summary[, -1], 3) 
StabilityPlot_NProd_Sex_NB.3 <- m.stab.plot(full.stabS$summary[, -1]) 
StabilityPlot_NProd_Sex_NB.3

# Negative binomial null model: 
NProd_Sex_NB_Null=glmer.nb(Num_Produced ~ FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_femVmal.2)
summary(NProd_Sex_NB_Null)	

# Compare full model with null model:
NullCompare_NProd_Sex_NB.3 <- as.data.frame(anova(NProd_Sex_NB.3, NProd_Sex_NB_Null, test="Chisq")) # Chisq= 41.13655; DF= 2 ; Pr(>Chisq): 1.167649e-09

### Output: ----
Summary_NProd_Sex_NB.3 <- summary(NProd_Sex_NB.3)

# Results of individual predictors to extract p-values and significance test):
SigTests_NProd_Sex_NB.3<-as.data.frame(drop1(NProd_Sex_NB.3, test="Chisq"))
SigTests_NProd_Sex_NB.3<-round(SigTests_NProd_Sex_NB.3, 3)
SigTests_NProd_Sex_NB.3

# Contribution of effects: 
summary(NProd_Sex_NB.3)$varcor 
# Number of levels per grouping:
summary(NProd_Sex_NB.3)$ngrps
# Extract estimate:
Estimates_NProd_Sex_NB.3 <- round(summary(NProd_Sex_NB.3)$coefficients, 3)
Estimates_NProd_Sex_NB.3

# Conditional and Marginal R squared:
ModelPerformance_NProd_Sex_NB.3 <- model_performance(NProd_Sex_NB.3) 
ModelPerformance_NProd_Sex_NB.3 # R^2 cond = 0.369, R^2 marg = 0.317 

# Model complexity (calculate number of observations per estimated item):
length(residuals(NProd_Sex_NB.3))/
  (length(fixef(NProd_Sex_NB.3))+
     nrow(as.data.frame(summary(NProd_Sex_NB.3)$varcor)))

Sexeffects <- effect(NProd_Sex_NB.3, term = "Sex")
print(Sexeffects)

# Confidence intervals:
CI_NProd_Sex_NB.3 <- confint(NProd_Sex_NB.3, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
CI_NProd_Sex_NB.3

## Plotting: ----
# Summary plots of effects: 
plot(allEffects(NProd_Sex_NB.3))

# Plot Figure 1:
# Get values: 
Sexeffects <- effect(NProd_Sex_NB.3, term = "Sex")
SexeffectsSummary <- summary(Sexeffects)

SexeffectsEffect<- data.frame(SexeffectsSummary$effect)
SexeffectsOffset<- data.frame(SexeffectsSummary$offset)
SexeffectsLower<- data.frame(SexeffectsSummary$lower)
SexeffectsUpper<- data.frame(SexeffectsSummary$upper)
SexeffectsEffect$SexeffectsSummary.effect <- as.numeric(SexeffectsEffect$SexeffectsSummary.effect)
SexeffectsOffset$SexeffectsSummary.offset <- as.numeric(SexeffectsOffset$SexeffectsSummary.offset)
SexeffectsLower$SexeffectsSummary.lower <- as.numeric(SexeffectsLower$SexeffectsSummary.lower)
SexeffectsUpper$SexeffectsSummary.upper <- as.numeric(SexeffectsUpper$SexeffectsSummary.upper)

NumPHs_femVmal.2$ProducedPerHr <- NumPHs_femVmal.2$Num_Produced/NumPHs_femVmal.2$Duration_Hrs

# Split values for points by group 2:
NumPHs_femVmal.2M <-subset(NumPHs_femVmal.2, Sex == "m")
NumPHs_femVmal.2F <-subset(NumPHs_femVmal.2, Sex == "f")

YpointsM <-as.numeric(((NumPHs_femVmal.2$ProducedPerHr)*SexeffectsOffset$SexeffectsSummary.offset ))
YpointsF <-as.numeric(((NumPHs_femVmal.2$ProducedPerHr)*SexeffectsOffset$SexeffectsSummary.offset ))

# Create Y-axis points values: 
YpointsM <-NumPHs_femVmal.2M$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset
YpointsF <-NumPHs_femVmal.2F$ProducedPerHr*SexeffectsOffset$SexeffectsSummary.offset

# Create X-axis points values:
# Create ID number (for jitters):
MLength <- nrow(NumPHs_femVmal.2M) 
FLength <- nrow(NumPHs_femVmal.2F)
NumPHs_femVmal.2M$JitterIDnum <- c(1:MLength)
NumPHs_femVmal.2F$JitterIDnum <- c(1:FLength)
# Make jitter constant for points X-axis values:
Mxaxisconstant <- 1/MLength
Fxaxisconstant <- 1/FLength
# X-axis values:
XpointsM <-( 3+((NumPHs_femVmal.2M$JitterIDnum)*Mxaxisconstant)-Mxaxisconstant)
XpointsF <-( 1+((NumPHs_femVmal.2F$JitterIDnum)*Fxaxisconstant)-Fxaxisconstant)

# Make values for horizontal bars for estimates and CI limits:
MEst <- SexeffectsEffect$SexeffectsSummary.effect[1]
MLower <- SexeffectsLower$SexeffectsSummary.lower[1]
MUpper <- SexeffectsUpper$SexeffectsSummary.upper[1]

FEst <- SexeffectsEffect$SexeffectsSummary.effect[2]
FLower <- SexeffectsLower$SexeffectsSummary.lower[2]
FUpper <- SexeffectsUpper$SexeffectsSummary.upper[2]

# Axes values:
ylimit=range(c(0,8))
xlimit=range(c(0,5))
ylabel="Number pant-hoots produced" 
xlabel="Sex"
axiscrossX=0
axiscrossY=0

par(mar=c(3.5, 3.5, 0.5, 0.5),mgp=c(2, 1, 0)) # set margins (mar) and distance of axes labels/tick lables (mgp)
plot(x=1, y=1, xlab="", ylab=ylabel, type="n", axes=F,  xlim=xlimit, ylim=ylimit)
axis(side=2, pos=axiscrossY, las=1) # y-axes sides and cross point
axis(side=1, pos=axiscrossX, tck=0, labels=FALSE) # x-axis

# Add X-axis label and sex labels:
mtext(text=xlabel, at=c(2.5), line=0.4, side=1, cex=1.2)
mtext(text=c("Female", "Male"), at=c(1.5, 3.5), line=-0.6, side=1, cex=1)

# Add data points:
points(x=XpointsM, y=YpointsM, pch=19, cex=.7, col="gray61")
points(x=XpointsM, y=YpointsM, pch=21, cex=.7, col="gray21")
points(x=XpointsF, y=YpointsF, pch=19, cex=.7, col="gray61")
points(x=XpointsF, y=YpointsF, pch=21, cex=.7, col="gray21")

# Add horizontal bars for estimates:
segments(x0=1, x1=2, y0=FEst, y1=FEst, lwd=3, lend=1)
segments(x0=3, x1=4, y0=MEst, y1=MEst, lwd=3, lend=1)

# Add max and min bars:
xcoordf=1.5
xcoordm= 3.5
arrows(xcoordf, FLower, xcoordf, FUpper, code= 3, length=0.05, angle=90)
arrows(xcoordm, MLower, xcoordm, MUpper, code= 3, length=0.05, angle=90)


# GLM 2: Are there community differences in female pant-hoot production frequencies? ----
## Data preparation:----
# Check number rows:
nrow(ImportedDataNumPHs) # Should be: N = 206

# subset data to exclude male individuals:
NumPHs_fem.2<-subset(ImportedDataNumPHs, Sex=="f")

# Remove data to exclude from analyses:
NumPHs_fem.2<-subset(NumPHs_fem.2, To_remove!="y")
# Create subset of data with 40minute minimum data:
NumPHs_fem.2<-subset(NumPHs_fem.2, Duration_Hrs>(2/3))

# Check number rows:
nrow(NumPHs_fem.2) # Should be: N = 118

# Format variables:
NumPHs_fem.2$Community <- as.factor(NumPHs_fem.2$Community)
NumPHs_fem.2$ChimpID  <- as.factor(NumPHs_fem.2$ChimpID) 
NumPHs_fem.2$Sex <- as.factor(NumPHs_fem.2$Sex)
NumPHs_fem.2$FAI <- as.numeric(NumPHs_fem.2$FAI)
NumPHs_fem.2$Duration_Hrs <- as.numeric(NumPHs_fem.2$Duration_Hrs)
NumPHs_fem.2$Num_Received <- as.numeric(NumPHs_fem.2$Num_Received)
NumPHs_fem.2$Num_Produced <- as.numeric(NumPHs_fem.2$Num_Produced)

# Set reference levels for binary variables: 
NumPHs_fem.2$Community <- relevel(NumPHs_fem.2$Community, ref = "sonso")

#Describe structure of variables (after 40min cutoff applied)
str(NumPHs_fem.2$Community) # Factor w/ 2 levels "sonso","waibira"
str(NumPHs_fem.2$ChimpID) #Factor w/ 29 levels
str(NumPHs_fem.2$Sex) # Factor w/ 1 level "f"
str(NumPHs_fem.2$FAI) #number
str(NumPHs_fem.2$Duration_Hrs) #number
str(NumPHs_fem.2$Num_Received) #number
str(NumPHs_fem.2$Num_Produced) #number

# Look at sample sizes:
table(NumPHs_fem.2$Community) # number of focal follows: Sonso = 29; Waibira = 89

# Summary of number of data points, and descriptive statistics of outcome variable (mean, SD, min, max, median, quartiles, ...):
dataframe <- NumPHs_fem.2
dataframe$dependent <- NumPHs_fem.2$Num_Produced
dataframe$Group1 <- NumPHs_fem.2$Community

SummaryDatafrmFems <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(), Nused = length(dependent[!is.na(dependent)]), mean = mean(dependent, na.rm=TRUE), sd = sd(dependent, na.rm=TRUE), min = min(dependent, na.rm=TRUE), max = max(dependent, na.rm=TRUE), median = median(dependent, na.rm=TRUE), qone = quantile(dependent, .25, na.rm=TRUE), qthree = quantile(dependent, .75, na.rm=TRUE))
SummaryDatafrmFems$sterr <- SummaryDatafrmFems$sd/(sqrt(SummaryDatafrmFems$Nused))
View(SummaryDatafrmFems) # number of focal follows: Sonso = 29; Waibira = 89

# Focal duration:
dataframe$dependent=NumPHs_fem.2$Duration_Hrs
SummaryHrsFem <- dataframe %>% group_by(Group1) %>%  summarise(Nfull = n(),Sum = sum(dependent),)
View(SummaryHrsFem)

# Number of IDs: 
IDsbyCommunity <- data.frame(table(NumPHs_fem.2$ChimpID, NumPHs_fem.2$Community))
IDsbyCommunity$ChimpID <- IDsbyCommunity$Var1 
IDsbyCommunity$Community <- IDsbyCommunity$Var2
IDsbyCommunity<- IDsbyCommunity[, c("Community", "ChimpID","Freq")]
IDsbyCommunity<- subset(IDsbyCommunity, Freq!=0)
View(IDsbyCommunity)
table(IDsbyCommunity$Community) # 10 Sonso females; 19 Waibira females

# Check outcome variable distribution:
plot(table(NumPHs_fem.2$Num_Produced))

# Check representation of IDs (by num recieved and duration of follows):
table(NumPHs_fem.2$ChimpID)

plot(NumPHs_fem.2$ChimpID, NumPHs_fem.2$Num_Received) 
plot(NumPHs_fem.2$ChimpID, NumPHs_fem.2$Duration_Hrs) 
plot(NumPHs_fem.2$ChimpID, NumPHs_fem.2$FAI)

# Check variation of num recieved, duration and FAI by community: 
plot(NumPHs_fem.2$Community,NumPHs_fem.2$Num_Received) 
plot(NumPHs_fem.2$Community,NumPHs_fem.2$Duration_Hrs) 
plot(NumPHs_fem.2$Community,NumPHs_fem.2$FAI)

# Check range of focal follow duration: 
range(NumPHs_fem.2$Duration_Hrs)

# Standardise/center numeric predictors:
Num_ReceivedMean <- mean(NumPHs_fem.2$Num_Received)
Num_ReceivedSD <- sd(NumPHs_fem.2$Num_Received)
NumPHs_fem.2$Num_ReceivedCentered <-(NumPHs_fem.2$Num_Received-Num_ReceivedMean )/Num_ReceivedSD 

FAI_Mean <- mean(NumPHs_fem.2$FAI)
FAI_SD <- sd(NumPHs_fem.2$FAI)
NumPHs_fem.2$FAI_Centered <-(NumPHs_fem.2$FAI-FAI_Mean)/FAI_SD 

## Model: ----
contr=glmerControl(optCtrl = list(maxfun=10000000))

# Poisson model:
NProd_Community_Pois.2=glmer(Num_Produced ~ Community*Num_ReceivedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID),
	family=poisson, data=NumPHs_fem.2)
summary(NProd_Community_Pois.2)	

### Model checks: ----
# Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Community_Pois.2) #  chisq 255.4106; df  112;  P 3.28404e-13 ; dispersion.parameter 2.280452
check_overdispersion(NProd_Community_Pois.2) # Over-dispersion detected: dispersion ratio = 2.280; Pearson's Chi-Squared = 255.411; p-value = < 0.001

# Negative binomial model to address zero inflation:
NProd_Community_NB.2=glmer.nb(Num_Produced ~ Community*Num_ReceivedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_fem.2)
summary(NProd_Community_NB.2)	

# Negative binomial model after removing non-significant interacitons:
NProd_Community_NB.3=glmer.nb(Num_Produced ~ Community + Num_ReceivedCentered + FAI_Centered  
	+ offset(log(Duration_Hrs))
	+ (1|ChimpID), nAGQ = 0,
	data=NumPHs_fem.2)
summary(NProd_Community_NB.3)	
summary(NProd_Community_NB.3)$varcor

# Variance explained by ID is negligible. Therefore, run model as GLM instead of GLMM:
NProd_Community_GLM.1 <- glm.nb(Num_Produced ~ Community*Num_ReceivedCentered + FAI_Centered + offset(log(Duration_Hrs)),
                              data = NumPHs_fem.2)
summary(NProd_Community_GLM.1)

# Remove non-significant interaction: 
NProd_Community_GLM.2 <- glm.nb(Num_Produced ~ Community + Num_ReceivedCentered + FAI_Centered + offset(log(Duration_Hrs)),
                              data = NumPHs_fem.2)
summary(NProd_Community_GLM.2)

# Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
overdisp.test(NProd_Community_GLM.2) #  chisq  115.5181; df  113 ;  P 0.4166178  ; dispersion.parameter     1.022284
check_overdispersion(NProd_Community_GLM.2)# No over-dispersion detected.

# Collinearity:
check_collinearity(NProd_Community_GLM.2)

# General model checks:
check_model_NProd_Community_GLM.2 <- check_model(NProd_Community_GLM.2) 
check_model_NProd_Community_GLM.2 
check_outliers(NProd_Community_GLM.2)

# Negative binomial null model:
NProd_Community_NB_Null <- glm.nb(Num_Produced ~ FAI_Centered + offset(log(Duration_Hrs)),
                              data = NumPHs_fem.2)
summary(NProd_Community_NB_Null)

# Compare full model vs. null model:
NullCompare_NProd_Community_GLM.2 <- as.data.frame(anova(NProd_Community_GLM.2, NProd_Community_NB_Null, test="Chisq")) 
NullCompare_NProd_Community_GLM.2#Chisq= 21.31576; DF= 2 ; Pr(>Chisq): 2.351477e-05

### Model's output: ----
Summary_NProd_Community_GLM.2 <- summary(NProd_Community_GLM.2)

# Results of individual predictors (to extract p-values and significance test):
SigTests_NProd_Community_GLM.2<-as.data.frame(drop1(NProd_Community_GLM.2, test="Chisq"))
SigTests_NProd_Community_GLM.2<-round(SigTests_NProd_Community_GLM.2, 3)
SigTests_NProd_Community_GLM.2

# Extract Estimate:
Estimates_NProd_Community_GLM.2 <- round(summary(NProd_Community_GLM.2)$coefficients, 3)
Estimates_NProd_Community_GLM.2

# Conditional and Marginal R squared:
ModelPerformance_NProd_Community_GLM.2 <- model_performance(NProd_Community_GLM.2) 
ModelPerformance_NProd_Community_GLM.2

# Model complexity (how many observations per estimated item):
length(residuals(NProd_Community_GLM.2)/4) 

# Confidence intervals:
CI_NProd_Community_GLM.2 <- confint(NProd_Community_GLM.2, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
CI_NProd_Community_GLM.2

## Plotting: ----
# Summary plots of effects:
plot(allEffects(NProd_Community_GLM.2)) 


# GLMM 3: Which factors predict pant-hoot responses in male and female chimpanzees? ------
## Data preparation: ----
ImportedData<- read_excel("Data for Model 3 and 4 - female PH.xlsx", sheet = "Sheet1")
ImportedDataPHResponses <- ImportedData[, c("Community", "ChimpID", "FocalFollowNum", "Sex", "Age", "Activity", "part", "hr", "phResp", "OpportunitiesToResp", "drum", "inGrpPh", "alphaPresent", "indepIndiv")]

nrow(ImportedDataPHResponses)
AllPHootsRec  <-  droplevels(na.omit(ImportedDataPHResponses))
nrow(AllPHootsRec) 

# Replace 'NotFeed' with 'notfeedripefruit'; and notripefruit with notfeedripefruit
AllPHootsRec$part<- ifelse(AllPHootsRec$part == 'NotFeed', 'NotFeedRipeFruit', AllPHootsRec$part)
AllPHootsRec$part<- ifelse(AllPHootsRec$part == 'NotRipeFruit', 'NotFeedRipeFruit', AllPHootsRec$part)

AllPHootsRec$ChimpID <-as.factor(AllPHootsRec$ChimpID)
AllPHootsRec$FocalFollowNum <-as.factor(AllPHootsRec$FocalFollowNum)
AllPHootsRec$alphaPresent <-as.factor(AllPHootsRec$alphaPresent)
AllPHootsRec$inGrpPh <-as.factor(AllPHootsRec$inGrpPh)
AllPHootsRec$part <-as.factor(AllPHootsRec$part)
AllPHootsRec$Sex <-as.factor(AllPHootsRec$Sex)
AllPHootsRec$drum <-as.factor(AllPHootsRec$drum)
AllPHootsRec$Activity <-as.factor(AllPHootsRec$Activity)

AllPHootsRec$indepIndiv <- as.numeric(AllPHootsRec$indepIndiv)
AllPHootsRec$Age <-as.numeric(AllPHootsRec$Age)
AllPHootsRec$hr <-as.numeric(AllPHootsRec$hr)

# Set reference levels for categorical variables:
AllPHootsRec$Sex <- relevel(AllPHootsRec$Sex, ref = "m")
AllPHootsRec$part <- relevel(AllPHootsRec$part, ref = "NotFeedRipeFruit")
AllPHootsRec$Activity <- relevel(AllPHootsRec$Activity, ref = "NotFeed")

# Standardize numeric predictors (except Hr):
indepIndivMean <- mean(AllPHootsRec$indepIndiv)
indepIndivSD <- sd(AllPHootsRec$indepIndiv)
AllPHootsRec$indepIndivCentered <- (AllPHootsRec$indepIndiv-indepIndivMean)/indepIndivSD 

AgeMean <- mean(AllPHootsRec$Age)
AgeSD <- sd(AllPHootsRec$Age)
AllPHootsRec$AgeCentered <- (AllPHootsRec$Age-AgeMean)/AgeSD 

# Convert Hr to radians: 
AllPHootsRec$hr.rad =2*pi*as.numeric(AllPHootsRec$hr)/24

str(AllPHootsRec)
nrow(AllPHootsRec)

# Summary of number of data points: 
phResp_Sex <- table(AllPHootsRec$phResp, AllPHootsRec$Sex)
phResp_Sex_with_margins <- addmargins(phResp_Sex)
phResp_Sex_with_margins

# Group by 'Sex' and quantify the unique values of 'ChimpID':
count_data <- ImportedData %>%
  group_by(Sex) %>%
  summarise(unique_chimp_count = n_distinct(ChimpID))
print(count_data) # females: N = 28, Males: N = 16

# Check that the data are balanced wrt the random variables:
table(AllPHootsRec$ChimpID) 
table(AllPHootsRec$FocalFollowNum)
hist(AllPHootsRec$hr)

# Check distribution across individuals and variables:
table(AllPHootsRec$phResp, AllPHootsRec$ChimpID) 
table(AllPHootsRec$phResp, AllPHootsRec$hr) 
table(AllPHootsRec$phResp, AllPHootsRec$alphaPresent) 
table(AllPHootsRec$phResp, AllPHootsRec$inGrpPh) 
table(AllPHootsRec$phResp, AllPHootsRec$Activity) 
table(AllPHootsRec$phResp, AllPHootsRec$part) 
table(AllPHootsRec$phResp, AllPHootsRec$indepIndiv)
plot(table(AllPHootsRec$indepIndiv))

# Number of 1s and 0s in the outcome:
table(AllPHootsRec$phResp) # 0 = 1980, 1 = 344 

## Model:----
contr=glmerControl(optCtrl = list(maxfun=10000000), calc.derivs=F, optimizer = "nloptwrap")

# Model with all interactions: 
AllInteractsSex.1=glmer(phResp ~ Sex*AgeCentered  + Sex*alphaPresent + Sex*indepIndivCentered  + Sex*inGrpPh + Sex*drum + Sex*(Activity/part)
				+ sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=AllPHootsRec, family=binomial, control=contr)
summary(AllInteractsSex.1)

# Check random effects:
summary(AllInteractsSex.1)$varcor #to exclude the random intercepts estimated to be 0 -> none

# Model after removing non-significant interactions:
fullSex=glmer(phResp ~ AgeCentered  + Sex*alphaPresent + Sex*indepIndivCentered  + inGrpPh + drum +(Activity/part)
				+ sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=AllPHootsRec, family=binomial, control=contr)
summary(fullSex)

# Model without interactions (to calculate collinearity):
fullSex_no_interacts=glmer(phResp ~ AgeCentered  + Sex+ alphaPresent + indepIndivCentered  + inGrpPh + drum +(Activity/part)
				+ sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=AllPHootsRec, family=binomial, control=contr)
summary(fullSex_no_interacts)

### Model checks: ----
# # Over-dispersion:
source("diagnostic_fcns.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
ranef.diagn.plot(fullSex) 
check_collinearity(fullSex_no_interacts) 

check_model(fullSex) 
check_outliers(fullSex)

# Stability:
source("glmm_stability.r") # Function courtesy of Roger Mundry. Please contact him to obtain the function necessary to run this code. 
full.stabS=glmm.model.stab(model.res=fullSex, data=AllPHootsRec,contr=contr)

# Check for convergence issues:
table(full.stabS$detailed$lme4.warnings) 
table(full.stabS$detailed$opt.warnings)
# Stability: 
M3x<-round(full.stabS$summary[, -1], 3) 
m.stab.plot(full.stabS$summary[, -1])

# Null model:
null=glmer(phResp ~ (1|ChimpID)+(1|FocalFollowNum), data=AllPHootsRec, family=binomial, control=contr)

# Full model vs. null model comparison:
as.data.frame(anova(null, fullSex, test="Chisq")) # p value: 1.065673e-46, Chi2 = 250.5204, df = 12
fullSex_model_Coefficients <- round(summary(fullSex)$coefficients, 3)
fullSex_model_Coefficients 

### Output: ----
summary(fullSex)
# Results of individual predictors (to extract p-values and significance test):
tests=as.data.frame(drop1(fullSex, test="Chisq"))
round(tests, 3)
# Contribution of effects: 
summary(fullSex)$varcor
# Number of levels per grouping:
summary(fullSex)$ngrps
# Extract Estimate:
fullSex.reml <- fullSex
xxx <- round(summary(fullSex.reml)$coefficients, 3)
# Conditional and Marginal R squared:
model_performance(fullSex)
# Model complexity (How many observations per estimated item):
length(residuals(fullSex))/
  (length(fixef(fullSex))+
     nrow(as.data.frame(summary(fullSex)$varcor))) 
Summary_fullSex <- summary(fullSex)

# Confidence intervals:
CIFullModel <- confint(fullSex, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
CIFullModel

### Post-hoc tests to interpret significant interactions: ----
# Create new datasets one for female and one for male:
AllPHootsF <- subset(AllPHootsRec, Sex == "f")
AllPHootsM <- subset(AllPHootsRec, Sex == "m")

# Model but with female and male data separately:
fullsex_posthoc_f=glmer(phResp ~ AgeCentered  + alphaPresent + indepIndivCentered  + inGrpPh + drum +(Activity/part)
              + sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=AllPHootsF, family=binomial, control=contr)
summary(fullsex_posthoc_f)

fullsex_posthoc_m=glmer(phResp ~ AgeCentered  + alphaPresent + indepIndivCentered  + inGrpPh + drum +(Activity/part)
                        + sin(hr.rad) + cos(hr.rad) +
                          (1|ChimpID)+(1|FocalFollowNum),
                        data=AllPHootsM, family=binomial, control=contr)
summary(fullsex_posthoc_m)

## Plotting Figure 2: ----
# Plot interaction Sex*alpha_male:
plotS1 <- cat_plot(fullSex, pred = "alphaPresent", modx = "Sex", data = AllPHootsRec, plot.points = F, interval = T, int.type = "confidence",geom = "line",vary.lty = T,
                        x.label = "Alpha male present in the party", y.label = "Probability of pant-hoot response", colors = "Set2", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
  scale_y_continuous(breaks=seq(0, 1,0.05), limits = c(0, 0.21)) +
  scale_x_discrete(breaks = c(0, 1), labels =c("No","Yes"))+
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
  theme(legend.position = "none")+
  My_label_2
plotS1

# Plot interaction Sex*IndepIndiv:
plotS2 <- interact_plot(fullSex, pred = "indepIndivCentered", modx = "Sex", data = AllPHootsRec, plot.points = F, interval = T, int.type = "confidence",geom = "line",vary.lty = T,
                   x.label = "number of independent individuals", y.label = "", colors = "Set2", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.1, jitter = 0.1) + 
  scale_y_continuous(breaks=seq(0, 1,0.10), limits = c(0, 0.21)) +
  scale_x_continuous(name="Number of individuals in the party") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
  My_label_2
plotS2 

# Combine two plots:
ggarrange(plotS1, plotS2p, ncol = 2, nrow = 1, labels = c("A", "B"), 
          font.label = list(size = 16, color = "red1"), label.y = c(0.97, 0.97), label.x = c(0.2, 0.1)) # Export as PDF (A4)

# Plots with data npoints for Supplementary materials: 
plotS1sm <- cat_plot(fullSex, pred = "alphaPresent", modx = "Sex", data = AllPHootsRec, interval = T, int.type = "confidence", geom = "point", vary.lty = T, plot.points = T,
                     x.label = "Alpha male present in the party", y.label = "Probability of pant-hoot response", colors = "Set2", line.thickness = 0.3, point.size = 0.62, point.alpha = 0.72, jitter = 0.2) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position = "none") +
  My_label_2
plotS1sm

plotS2sm <- interact_plot(fullSex, pred = "indepIndiv", modx = "Sex", data = AllPHootsRec, plot.points = T, interval = T, int.type = "confidence", geom = "line",vary.lty = T,
                        x.label = "number of independent individuals", y.label = "", colors = "Set2", line.thickness = 0.3, point.size = 0.62, point.alpha = 0.72, jitter = 0.2) + 
  scale_y_continuous(breaks=seq(0, 1,0.10), labels = NULL) +
  scale_x_continuous(name="Number of individuals in the party") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
  My_label_2
plotS2sm

# Combine two plots:
ggarrange(plotS1sm, plotS2sm, ncol = 2, nrow = 1, labels = c("A", "B"), 
          font.label = list(size = 16, color = "red1"), label.y = c(0.97, 0.97), label.x = c(0.175, 0.1)) # Export as PDF (A4)


# GLMM 4: Which individual factors predict pant-hoot responses in female chimpanzees from two communities? ----
## Data preparation: ----
ImportedData<- read_excel("Data for Model 3 and 4 - female PH.xlsx", sheet = "Sheet1")
ImportedDataPHResponsesFems <- ImportedData[, c("Community", "ChimpID", "FocalFollowNum", "Sex", "Age", "Activity", "part", "hr", "phResp", "Parity", "MaleDep", "oes4", "OpportunitiesToResp", "drum", "inGrpPh", "alphaPresent", "indepIndiv")]

nrow(ImportedDataPHResponsesFems)
FemalePHootsRec  <-  droplevels(na.omit(ImportedDataPHResponsesFems))
nrow(FemalePHootsRec)
FemalePHootsRec <- subset(FemalePHootsRec, FemalePHootsRec$Sex == "f")
nrow(FemalePHootsRec)

# Replace 'NotFeed' with 'notfeedripefruit'; and notripefruit with notfeedripefruit
FemalePHootsRec$part<- ifelse(FemalePHootsRec$part == 'NotFeed', 'NotFeedRipeFruit', FemalePHootsRec$part)
FemalePHootsRec$part<- ifelse(FemalePHootsRec$part == 'NotRipeFruit', 'NotFeedRipeFruit', FemalePHootsRec$part)

FemalePHootsRec$ChimpID <-as.factor(FemalePHootsRec$ChimpID)
FemalePHootsRec$Community <-as.factor(FemalePHootsRec$Community)
FemalePHootsRec$FocalFollowNum <-as.factor(FemalePHootsRec$FocalFollowNum)
FemalePHootsRec$alphaPresent <-as.factor(FemalePHootsRec$alphaPresent)
FemalePHootsRec$inGrpPh <-as.factor(FemalePHootsRec$inGrpPh)
FemalePHootsRec$part <-as.factor(FemalePHootsRec$part)
FemalePHootsRec$Sex <-as.factor(FemalePHootsRec$Sex)
FemalePHootsRec$drum <-as.factor(FemalePHootsRec$drum)
FemalePHootsRec$Activity <-as.factor(FemalePHootsRec$Activity)
FemalePHootsRec$Parity <-as.factor(FemalePHootsRec$Parity)
FemalePHootsRec$MaleDep <-as.factor(FemalePHootsRec$MaleDep)
FemalePHootsRec$oes4 <-as.factor(FemalePHootsRec$oes4)

FemalePHootsRec$indepIndiv <- as.numeric(FemalePHootsRec$indepIndiv)
FemalePHootsRec$Age <-as.numeric(FemalePHootsRec$Age)
FemalePHootsRec$hr <-as.numeric(FemalePHootsRec$hr)

# Set reference levels for categorical variables:
FemalePHootsRec$part <- relevel(FemalePHootsRec$part, ref = "NotFeedRipeFruit")
FemalePHootsRec$Activity <- relevel(FemalePHootsRec$Activity, ref = "NotFeed")
FemalePHootsRec$Community <- relevel(FemalePHootsRec$Community, ref = "sonso")

# Standardise numeric predictors (except Hr):
indepIndivMean <- mean(FemalePHootsRec$indepIndiv)
indepIndivSD <- sd(FemalePHootsRec$indepIndiv)
FemalePHootsRec$indepIndivCentered <- (FemalePHootsRec$indepIndiv-indepIndivMean)/indepIndivSD 

AgeMean <- mean(FemalePHootsRec$Age)
AgeSD <- sd(FemalePHootsRec$Age)
FemalePHootsRec$AgeCentered <- (FemalePHootsRec$Age-AgeMean)/AgeSD 

# Convert Hr to radians:
FemalePHootsRec$hr.rad =2*pi*as.numeric(FemalePHootsRec$hr)/24

str(FemalePHootsRec) 
nrow(FemalePHootsRec) 

# Summary of number of data points:
phResp_Community <- table(FemalePHootsRec$phResp, FemalePHootsRec$Community)
phResp_Community_with_margins <- addmargins(phResp_Community)
phResp_Community_with_margins # Sonso response = 52; Sonso no response = 421; Waibira response = 122; Waibira no response = 745

# Check that the data are balanced wrt the random & control variables:
table(FemalePHootsRec$ChimpID) 
table(FemalePHootsRec$FocalFollowNum) 
hist(FemalePHootsRec$hr) 

# Check that the responses are distributed across individuals and variables:
table(FemalePHootsRec$phResp, FemalePHootsRec$ChimpID) 
table(FemalePHootsRec$phResp, FemalePHootsRec$hr) 
table(FemalePHootsRec$phResp, FemalePHootsRec$alphaPresent) 
table(FemalePHootsRec$phResp, FemalePHootsRec$inGrpPh) 
table(FemalePHootsRec$phResp, FemalePHootsRec$Activity) 
table(FemalePHootsRec$phResp, FemalePHootsRec$part) 
table(FemalePHootsRec$phResp, FemalePHootsRec$Parity) 
table(FemalePHootsRec$phResp, FemalePHootsRec$MaleDep) 
table(FemalePHootsRec$phResp, FemalePHootsRec$oes4)  
table(FemalePHootsRec$phResp, FemalePHootsRec$indepIndiv)
plot(table(FemalePHootsRec$indepIndiv)) 

# Number of 1s and 0s in the outcome:
table(FemalePHootsRec$phResp) # 0 = 1166 1= 174 

## Model: ----
contr=glmerControl(optCtrl = list(maxfun=10000000), calc.derivs=F, optimizer = "nloptwrap")

# Full model with all interactions: 
AllInteractsFem.1=glmer(phResp ~ Community*AgeCentered  + Community*alphaPresent+ Community*indepIndivCentered  + Community*inGrpPh + Community*drum + Community*(Activity/part)
				+ Community*Parity + Community*oes4 + Community*(Parity/MaleDep) 
				+ sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID)+(1|FocalFollowNum),
              data=FemalePHootsRec, family=binomial, control=contr)
summary(AllInteractsFem.1)

# Check random effects:
summary(AllInteractsFem.1)$varcor #

# Full model after removing all non-significant interactions: 
full_fem=glmer(phResp ~ AgeCentered + alphaPresent + Community*indepIndivCentered + Community*inGrpPh + drum + Activity + (Activity/part)
				+ Parity + oes4 + (Parity/MaleDep) 
				+ sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID),
				data=FemalePHootsRec, family=binomial, control=contr)
summary(full_fem)

# Model without any interactions (calculate collinearity):
fem_no_interacts=glmer(phResp ~ AgeCentered  + alphaPresent+ Community + indepIndivCentered  + inGrpPh + drum + Activity + (Activity/part)
				+ Parity + oes4 +(Parity/MaleDep) 
				+ sin(hr.rad) + cos(hr.rad) +
                (1|ChimpID),
				data=FemalePHootsRec, family=binomial, control=contr)

### Model checks: ----
# Over-dispersion:
source("diagnostic_fcns.r")
ranef.diagn.plot(full_fem) #ok
check_collinearity(fem_no_interacts) # LOW (max 3.38) OK. 
check_model(full_fem) #EH notes: Std deviance residuals not normal. 4 binned residuals outside error bounds. vif, indluentials, and normality of random effects fine. 
check_outliers(full_fem) # 5 outliers detected: cases 202, 287, 434, 437, 1192. Based on the following method and threshold: cook (1).

# Stability (courtesy of R. Mundry):
source("glmm_stability.r") 
m.stab=glmm.model.stab(model.res=full_fem, data=FemalePHootsRec,contr=contr)
# Check for convergence issues: 
table(m.stab$detailed$lme4.warnings) 
table(m.stab$detailed$opt.warnings)
# Stability: 
M4x<-round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

# Null model:
null_f=glmer(phResp ~ (1|ChimpID), data=FemalePHootsRec, family=binomial, control=contr)

# Full model vs. null model comparison:
as.data.frame(anova(null_f, full_fem, test="Chisq")) # p value:1.41408e-30 , Chi2 = 181.1187  df = 15 

### Output: ----
summary(full_fem)
# Results of individual predictors (p-values and significance test):
testsFS=as.data.frame(drop1(full_fem, test="Chisq"))
round(testsFS, 3)
# Contribution of effects: 
summary(full_fem)$varcor
# Number of levels per grouping:
summary(full_fem)$ngrps
# Estimates and SE:
full_fem.reml <- full_fem
xxx2 <- round(summary(full_fem.reml)$coefficients, 3)
# Conditional and Marginal R squared:
model_performance(full_fem)  
# Model complexity (How many observations per estimated item):
length(residuals(full_fem))/
  (length(fixef(full_fem))+
     nrow(as.data.frame(summary(full_fem)$varcor)))

Summary_full_fem <- summary(full_fem)

# Confidence intervals:
full_femCIs <- confint(full_fem, method = "boot", boot.type = "basic", seed = 123, nsim = 1000, .progress = "txt")
full_femCIs

### Post-hoc tests to interpret significant interactions: ----
# Create new datasets one for Waibira and one for Sonso:
AllPHootsW <- subset(FemalePHootsRec, Community == "waibira")
AllPHootsS <- subset(FemalePHootsRec, Community == "sonso")

# Model but with Sonso and Waibira data separately:
FemalePHoots_posthoc_W=glmer(phResp ~ AgeCentered + alphaPresent + indepIndivCentered + inGrpPh + drum + Activity + (Activity/part)
                             + Parity + oes4 + (Parity/MaleDep) 
                             + sin(hr.rad) + cos(hr.rad) +
                               (1|ChimpID),
                             data=AllPHootsW, family=binomial, control=contr)
summary(FemalePHoots_posthoc_W)

FemalePHoots_posthoc_S=glmer(phResp ~ AgeCentered + alphaPresent + indepIndivCentered + inGrpPh + drum + Activity + (Activity/part)
                             + Parity + oes4 + (Parity/MaleDep) 
                             + sin(hr.rad) + cos(hr.rad) +
                               (1|ChimpID),
                             data=AllPHootsS, family=binomial, control=contr)
summary(FemalePHoots_posthoc_S)

## Plotting Figure 3: ----
# Interaction between Community and indepIndiv:
plotS3P <- interact_plot(full_fem, pred = "indepIndiv", modx = "Community", data = FemalePHootsRec, plot.points = F, interval = T, int.type = "confidence", geom = "line",vary.lty = T,
                        x.label = "Context", y.label = "Probability of pant-hoot response", colors = "CUD Bright", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
  scale_y_continuous(breaks=seq(0, 1,0.10), limits = c(0, 0.81)) +
  scale_x_continuous(name="Number of individuals in the party") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(legend.position = "none")+
  My_label_2
plotS3P

# Interaction between Community and inGrpPh:
plotS4 <- cat_plot(full_fem, pred = "inGrpPh", modx = "Community", data = FemalePHootsRec, plot.points = F, interval = T, int.type = "confidence", geom = "line",vary.lty = T,
                        x.label = "Pant-hoot from focal's party", y.label = "", colors = "CUD Bright", line.thickness = 1.4, point.size = 0.3, point.alpha = 0.2, jitter = 0.1) + 
  scale_y_continuous(breaks=seq(0, 1,0.10), limits = c(0, 0.81), labels = NULL) +
  scale_x_discrete(name="Pant-hoot from focal's party", breaks = c(0, 1), labels =c("No","Yes"))+
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
  My_label_2
plotS4

# Combine two plots:
ggarrange(plotS3P, plotS4, ncol = 2, nrow = 1, labels = c("A", "B"), 
          font.label = list(size = 16, color = "red1"), label.y = c(0.97, 0.97), label.x = c(0.175, 0.1)) # Export as PDF (A4)

# Plots with data points for the supplementary materials:
# Interaction between Community and indepIndiv:
FemalePHootsRec$phResp=as.numeric(as.character(FemalePHootsRec$phResp))
plotS3Psm <- interact_plot(full_fem, pred = "indepIndiv", modx = "Community", data = FemalePHootsRec, plot.points = T, interval = T, int.type = "confidence", geom = "line",vary.lty = T,
                              x.label = "number of independent individuals", y.label = "Probability of pant-hoot response", colors = "CUD Bright", line.thickness = 0.3, point.size = 0.62, point.alpha = 0.62, jitter = 0.2) + 
  scale_y_continuous(breaks=seq(0, 1,0.10)) +
  scale_x_continuous(name="Number of individuals in the party") +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))+ 
  My_label_2 +
  theme(legend.position = "none")
plotS3Psm

# Interaction between Community and inGrpPh:
plotS4sm <- cat_plot(full_fem, pred = "inGrpPh", modx = "Community", data = FemalePHootsRec, plot.points = T, interval = T, int.type = "confidence", geom = "point",vary.lty = T,
                     x.label = "Pant-hoot from focal's party", y.label = "", colors = "CUD Bright", line.thickness = 0.3, point.size = 0.62, point.alpha = 0.62, jitter = 0.2)+  
  scale_y_continuous(breaks=seq(0, 1,0.10), labels = NULL) + 
  scale_x_discrete(name="Pant-hoot from focal's party",breaks = c(0, 1), labels = c("No", "Yes"))  + 
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
  My_label_2
plotS4sm
  
# Combine two plots:
ggarrange(plotS3Psm, plotS4sm, ncol = 2, nrow = 1, labels = c("A", "B"), 
          font.label = list(size = 16, color = "red1"), label.y = c(0.97, 0.97), label.x = c(0.175, 0.1)) # Export as PDF (A4)


