# Analyses code for the Manuscript "Sex specific flexible use of pant-hoot vocalisations in wild Eastern chimpanzees"
# Authors: 
# Contact:  

# Code for analyses: 
#Elo-rating of males to determine the alpha male

# Files needed: 
# Waibira_PantGrunts_20220123.csv
# Wabira_Sex.csv
# Waibira_IDsPresent.csv
# Sonso_PantGrunts_Male only.csv

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

