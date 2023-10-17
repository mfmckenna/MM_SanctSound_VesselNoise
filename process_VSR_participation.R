
rm(list=ls()) 
library(lubridate)
library(ggplot2)
library(dplyr)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# SET UP PARAMS ####
DC = Sys.Date()
topDir =  "F:\\SanctSound\\"
inDirW = (  "F:\\SanctSound\\analysis\\ERDAP_wind" ) # WIND data
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction" ) # HMD
outDir = "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction"

inFiles = list.files( inDir, pattern = "VSRdata", full.names = T)
inFiles = inFiles[grepl("2023-10-15", inFiles)] #remove 1 day files
load(inFiles)
head(VSRdata)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
#change metric for ROWS WITHOUT SPEED
VSRdata$AISsogs  = gsub("-999;", "", VSRdata$AISsogs) # remove -999 from list
VSRdata$AISsogs  = gsub("-999", NA, VSRdata$AISsogs)  # make all no ships as NA
VSRdata$AISLsogs = gsub("-999;", "", VSRdata$AISLsogs)
VSRdata$AISLsogs = gsub("-999", NA, VSRdata$AISLsogs)
VSRdata$AISOsogs = gsub("-999;", "", VSRdata$AISOsogs)
VSRdata$AISOsogs = gsub("-999", NA, VSRdata$AISOsogs)
VSRdata$Yr = year(VSRdata$dateTime)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
#some rows have multiple ships present.... so need to get average for the minute
VSRdata$AISsog_avg = NULL
VSRdata$AISLsogs_avg = NULL
VSRdata$AISOsogs_avg = NULL

calculate_average <- function(input_string) {
  if (is.na(input_string)) {
    return(NA)
  }
  values <- strsplit(input_string, ";")[[1]]
  numeric_values <- as.numeric(values)
  if (any(is.na(numeric_values))) {
    return(NA)
  }
  return(mean(numeric_values))
}
count_values_below_10 <- function(input_string) {
  if (is.na(input_string)) {
    return(NA)
  }
  values <- strsplit(input_string, ";")[[1]]
  numeric_values <- as.numeric(values)
  if (any(is.na(numeric_values))) {
    return(NA)
  }
  count <- sum(numeric_values <= 10)
  return(count)
}

# average SOG for all ships in that minute
VSRdata$AISsog_avg       <- sapply(VSRdata$AISsogs, calculate_average)
min(VSRdata$AISsog_avg, na.rm =T)
max(VSRdata$AISsog_avg, na.rm =T)

VSRdata$AISLsogs_avg     <- sapply(VSRdata$AISLsogs, calculate_average)
min(VSRdata$AISLsogs_avg, na.rm =T)
max(VSRdata$AISLsogs_avg, na.rm =T)

VSRdata$AISOsogs_avg     <- sapply(VSRdata$AISOsogs, calculate_average)
min(VSRdata$AISOsogs_avg, na.rm =T)
max(VSRdata$AISOsogs_avg, na.rm =T)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# create factor to categorize minutes
VSRdata$Condition = "1. No AIS" # no ships
unique(VSRdata$Condition)
#x1 <- c(1, 4, 3, NA, 7), which(x1 <=7 )

# large > 10
idx = which(is.na(VSRdata$AISOsogs_avg) & VSRdata$AISLsogs_avg > 10)
VSRdata$Condition [idx] = "5. Large >10"
unique(VSRdata$Condition)
# large > 10
idx = which(is.na(VSRdata$AISOsogs_avg) & VSRdata$AISLsogs_avg <= 10)
VSRdata$Condition [idx] = "4. Large <10"
unique(VSRdata$Condition)

#  large + other
idx = which(VSRdata$AISOsogs_avg>0 & VSRdata$AISLsogs_avg > 0)
VSRdata$Condition [idx] = "6. Multiple AIS"

# other only > 10
idx = which( VSRdata$AISOsogs_avg >10 & is.na(VSRdata$AISLsogs_avg) )
VSRdata$Condition [idx] = "3. Other >10"
unique(VSRdata$Condition)

idx = which( VSRdata$AISOsogs_avg <=10 & is.na(VSRdata$AISLsogs_avg) )
VSRdata$Condition [idx] = "2. Other <10"
unique(VSRdata$Condition)

# copy to google sheets to summarize
s = VSRdata%>%
  group_by(Condition, Yr,mth)%>% 
  summarise(count = n(),Mean=mean(TOL_125_PSD), Median=median(TOL_125_PSD), Std=sd(TOL_125_PSD))
s = VSRdata%>%
  group_by(Condition, Yr)%>% 
  summarise(count = n(),Mean=mean(TOL_125_PSD), Median=median(TOL_125_PSD), Std=sd(TOL_125_PSD))

#all data across conditions
ggplot(VSRdata, aes(TOL_125_PSD, color = as.factor( Condition ) ) ) +
  stat_ecdf(geom="step", linewidth = 2) +
  facet_grid(~Yr) +
  theme_bw() 

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# distribution of sound levels during slowdowns
tmp = VSRdata[VSRdata$Slow == "Slowdown",]
t = tmp%>%
  group_by(Condition, Yr)%>% 
  summarise(count = n(),Mean=mean(TOL_125_PSD), Median=median(TOL_125_PSD), Std=sd(TOL_125_PSD))

tmp = tmp[tmp$Condition != "3. Other >10",]
tmp = tmp[tmp$Condition != "2. Other <10",]

ggplot(tmp, aes(TOL_125_PSD, color = as.factor( Condition ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Yr) +
  theme_bw() +
  scale_color_discrete(name="")
 

p = ggplot(tmp, aes(x = as.factor( Condition ), y = TOL_125_PSD, fill = Condition)) +
  geom_violin() + 
  #geom_jitter(shape=16, alpha = .1, position=position_jitter(0.2))+
  stat_summary(fun = "median",
               geom = "point",
               color = "black") +
  ylab("") +
  facet_grid(~Yr) +
  theme_minimal()+
  coord_flip()
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
p + theme(axis.title.y = element_blank(),
          text = element_text(size=20) )

 #p + stat_summary(fun.data=mean_sdl, mult=1, 
 #                geom="pointrange", color="red")

### START HERE ####



library(dplyr)
tmp%>%
  group_by(Condition, Yr)%>% 
  summarise(count = n(),Mean=mean(TOL_125_PSD), Median=median(TOL_125_PSD), Std=sd(TOL_125_PSD))

 # just plot large compared to 
tmp = tmp[!tmp$Condition == "6. Multiple AIS",]
tmp = tmp[!tmp$Condition == "3. Other AIS, greater 10",]
tmp = tmp[!tmp$Condition == "2. Other AIS, less 10",]
unique(VSRdata$Condition)

ggplot(tmp, aes(TOL_125_PSD, color = as.factor( Condition ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Yr) +
  theme_bw() 

t = VSRdata%>%
  group_by(Condition, Yr, Slow)%>% 
  summarise(count = n(),Mean=mean(TOL_125_PSD), Median=median(TOL_125_PSD), Std=sd(TOL_125_PSD))


#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# CALCULATE participation level for each minute ####
# participation metrics
VSRdata$AISsog_avg = NULL
calculate_average <- function(input_string) {
  if (is.na(input_string)) {
    return(NA)
  }
  values <- strsplit(input_string, ";")[[1]]
  numeric_values <- as.numeric(values)
  if (any(is.na(numeric_values))) {
    return(NA)
  }
  return(mean(numeric_values))
}
count_values_below_10 <- function(input_string) {
  if (is.na(input_string)) {
    return(NA)
  }
  values <- strsplit(input_string, ";")[[1]]
  numeric_values <- as.numeric(values)
  if (any(is.na(numeric_values))) {
    return(NA)
  }
  count <- sum(numeric_values <= 10)
  return(count)
}

# average SOG for all ships in that minute
VSRdata$AISsog_avg     <- sapply(VSRdata$AISsogs, calculate_average)
# number of ships below 10 kts
VSRdata$AISsog_below10 <- sapply(VSRdata$AISsogs, count_values_below_10)
# percent of ships below 10 kts
VSRdata$AISsog_below10 = as.numeric( as.character(VSRdata$AISsog_below10 ))

# check values ####
#VSRdata$AISsogs[210]
#VSRdata$AISsog_avg[210]
#VSRdata$AISsog_below10 [210]
#VSRdata$AISsog_below10[210]/VSRdata$AIS[201] # zero ships going below 
#unique((VSRdata$AISsog_below10))
#x = 3
#VSRdata[which(VSRdata$AISsog_below10 ==x )[1],]

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# "weighted" participation ####
# total vessels * % below 10
#want the fewest ships + most < 10 to have HIGHEST participation value
#want the most ships + least < 10 to have the LOWEST participation value

# calculate % below 10 knots 
VSRdata$PerBelow = VSRdata$AISsog_below10/ VSRdata$AIS
x = unique((VSRdata$PerBelow))
VSRdata[which(VSRdata$PerBelow == x[7] )[1],]

# calculate normalized AIS total vessels (flipped- because we want highest to be lowest, and not zero)
VSRdata$AISn = 1 - ((VSRdata$AIS-1)/(max(VSRdata$AIS)-1)) +.01
VSRdata$AISn[VSRdata$AIS == 0] = NA # the are rows that do not have data
#length( which(is.na( VSRdata$AISn)) )  # no AIS ship minutes
#length( which(!is.na( VSRdata$AISn)) ) # AIS ship minutes

# calculate weighted-participation
VSRdata$WParticipation = VSRdata$AISn * VSRdata$PerBelow
unique(VSRdata$WParticipation)

idx = is.na(VSRdata$WParticipation)
VSRdata$WParticipation[idx] = -999
unique(VSRdata$WParticipation)

#just minutes with AIS vessels present (vessel detections not confirmed- use in predictive model)
VSRdataV = VSRdata[VSRdata$AIS >0, ]

#plot TOL by participation level
VSRdataV$WParticipationR =  ( round( VSRdataV$WParticipation * 100 ) )
VSRdata$WParticipationR =  ( round( VSRdata$WParticipation * 100 ) )
VSRdata$Yr = year(VSRdata$dateTime)
VSRdataV$Yr = year(VSRdataV$dateTime)

ggplot(VSRdata, aes(TOL_125_PSD, color = as.factor( WParticipationR ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Yr) +
  theme_bw() 

# EXPECT HIGHER PARTICIPATION- LOWER TOL
# plot TOL ecdf by % below 10 knots
ggplot(VSRdataV, aes(TOL_125_PSD, color = as.factor( PerBelow*100 ) ) ) +
  stat_ecdf(geom="step", size = 2)+
  facet_grid(~Yr) +
  theme_bw() 

# GROUP BY PERCENTAGES: 
VSRdataV$WParticipationG = NA
idx = which( VSRdataV$WParticipationR == 0)
VSRdataV$WParticipationG [idx] = 0 # no participation
idx = which( VSRdataV$WParticipationR >= 100)
VSRdataV$WParticipationG [idx] = 100  # all participation
unique(VSRdataV$WParticipationG)

idx = which( VSRdataV$WParticipationR > 0  & VSRdataV$WParticipationR < 25)
VSRdataV$WParticipationG [idx] = 25 # low participation

idx = which( VSRdataV$WParticipationR >=25 & VSRdataV$WParticipationR < 50)
VSRdataV$WParticipationG [idx] = 50 # medium participation

idx = which( VSRdataV$WParticipationR >= 50 & VSRdataV$WParticipationR < 75)
VSRdataV[idx[1],]
VSRdataV$WParticipationG [idx] = 75 # medium participation 

idx = which( VSRdataV$WParticipationR >= 75 & VSRdataV$WParticipationR < 100)
VSRdataV$WParticipationG [idx] = 75 # high participation 

unique(VSRdataV$WParticipationG)

#PLOTS BY PARTICIPATION LEVEL
# all data-- hard to see patterns
ggplot(VSRdataV, aes(TOL_125_PSD, color = as.factor( WParticipationG ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Yr) +
  theme_bw() 

#all months
head(VSRdata)
VSRdataVs = VSRdataV[ VSRdataV$WParticipationG == 0 | VSRdataV$WParticipationG == 100,]
# by slowdown
ggplot(VSRdataVs, aes(TOL_125_PSD, color = as.factor( WParticipationG ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Slow)+
  theme_bw() 
#by year
ggplot(VSRdataVs, aes(TOL_125_PSD, color = as.factor( WParticipationG ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Yr)+
  labs(title = "", x = "", y = "", color = "weighted Participation") +
  theme_bw() 

#just slowdown periods
unique(VSRdataVs$Slow)
VSRdataVSlow = VSRdataVs[ VSRdataVs$Slow == "Slowdown", ]
ggplot(VSRdataVSlow, aes(TOL_125_PSD, color = as.factor( WParticipationG ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_grid(~Yr)+
  labs(title = "", x = "", y = "", color = "weighted Participation") +
  theme_bw() 

# WHEN AIS ships what is the "particpation" in terms of percent of time?
# more 100-- pie charts??
ggplot(VSRdataV, aes(x = as.factor( WParticipationG ), y = TOL_125_PSD)) +
  geom_violin() + 
  stat_summary(fun = "mean",
               geom = "point",
               color = "red") 


library(ggridges) # https://r-charts.com/distribution/ggridges/
ggplot(VSRdataV, aes(x = TOL_125_PSD , y = as.factor( WParticipationG ),
                     fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_gradient(low = "white", high = "#87CEFF",
                      name = "Tail prob.") +
  ylab("weighted participation") + xlab("125 Hz TOL PSD") +theme_minimal()


# GROUP BY PERCENTAGES-- Version 2
# keep none AIS times 
VSRdata0rg = 
  VSRdata = 
  VSRdata$WParticipationG2 = NA

idx = which( VSRdata$WParticipationR > 0  & VSRdata$WParticipationR < 25)
VSRdata$WParticipationG2 [idx] = 0 # low participation
unique(VSRdata$WParticipationG2)

idx = which( VSRdata$WParticipationR >=25 & VSRdata$WParticipationR <= 75)
VSRdata$WParticipationG2 [idx] = 50 # medium participation

idx = which( VSRdata$WParticipationR > 75  & VSRdata$WParticipationR >= 100)
VSRdata$WParticipationG2 [idx] = 100  # high participation

unique(VSRdata$WParticipationG2)
nrpw(VSRdata)

ggplot(VSRdataV, aes(TOL_125_PSD, color = as.factor( WParticipationG2 ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  theme_bw() 




idx = which( VSRdataV$WParticipationR >= 50 & VSRdataV$WParticipationR < 75)
VSRdataV[idx[1],]
VSRdataV$WParticipationG [idx] = 75 # medium participation 

idx = which( VSRdataV$WParticipationR >= 75 & VSRdataV$WParticipationR < 100)
VSRdataV$WParticipationG [idx] = 75 # high participation 
unique(VSRdataV$WParticipationG)

#PLOTS BY PARTICIPATION LEVEL
ggplot(VSRdataV, aes(TOL_125_PSD, color = as.factor( WParticipationG ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  theme_bw()




# is my weighted participation off???

# get summary of values in each category
VSRdataV$mth = month(VSRdataV$dateTime)
print(aggregate(VSRdataV$TOL_125_PSD, list(VSRdataV$WParticipationG), FUN=mean))
VSRdataV %>% 
  group_by(WParticipationG) %>%
  tally()


# just look at slow down months
VSRdataVslow = VSRdataV[VSRdataV$mth == 3 | VSRdataV$mth == 4,]
ggplot(VSRdataVslow, aes(x = TOL_125_PSD , y = as.factor( WParticipationG ),
                         fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_gradient(low = "white", high = "#87CEFF",
                      name = "Tail prob.") +
  ylab("weighted participation") + xlab("125 Hz TOL PSD") +theme_minimal()
ggplot(VSRdataVslow, aes(TOL_125_PSD, color = as.factor( WParticipationG ) ) ) +
  stat_ecdf(geom="step", size = 2) +
  theme_bw() 

print(aggregate(VSRdataVslow$TOL_125_PSD, list(VSRdataVslow$WParticipationG), FUN=mean))
VSRdataVslow %>% 
  group_by(WParticipationG) %>%
  tally()

# START HERE ####
library(suncalc)
library(mgcv)
library(zoo)
library(MuMIn)
library(visreg)
library(corrplot)
options(na.action = "na.omit")

VSRdataV$TOL_125_PSDshift = shift(VSRdataV$TOL_125_PSD,fill = NA)
plot(VSRdataV$TOL_125_PSDshift, VSRdataV$TOL_125_PSD)
corACI = cor(VSRdataV$TOL_125_PSDshift,VSRdataV$TOL_125_PSD, method = "pearson",use="complete.obs")

#check variables for NAs
unique(VSRdataV$AISl)
unique(VSRdataV$AISsog_avg)
unique(VSRdataV$AIS )
unique(VSRdataV$mth)
unique(VSRdataV$TOL_125_PSD)

VSRdataV$PerBelow = ( round(VSRdataV$PerBelow*100)  )
VSRdataV$TOL_125_PSDr = (round(VSRdataV$TOL_125_PSD*10) )/10
VSRdataV$AISsog_avgr = (round(VSRdataV$AISsog_avg*10) )/10

VSRdataV$AISc   = as.factor(VSRdataV$AIS)
VSRdataV$AISlc  = as.factor(VSRdataV$AISl)
VSRdataV$mthc   = as.factor(VSRdataV$mth)

unique(VSRdataV$AISsog_avgr) #numeric
unique(VSRdataV$AISlc) # factor
unique(VSRdataV$AISc ) #factor
unique(VSRdataV$mthc) #factor
unique(VSRdataV$TOL_125_PSDr) 

corrplot(corr = cor(VSRdataV),type = "lower" )
corrplot(corr = cor(VSRdataV[,c(29,2, 9,10,11,12,15,21,22,23,25)]),type = "lower" )

# PREDICT predict TOL based on 
# equation to predict TOL based on participation (percent below) + number large ships + average speed + enviro

#smooth term selection using select=TRUE, which penalizes wiggliness and removes terms with poor fit from the model
#We also fit all models with gamma=1.4, which further restricts wiggliness
#smooth terms: https://www.rdocumentation.org/packages/mgcv/versions/1.8-33/topics/smooth.terms
as.data.frame(colnames(VSRdataV))
unique( VSRdataV$Dets )

unique(VSRdataVd$Bio)

# speed + number large vessels + % below 10
global.Gamm125 = gam(TOL_125_PSDr ~ 
                       s(AISsog_avg) + (PerBelow) + (AISl) + (Bio) + s(mth),
                     correlation = corCAR1(value=corACI, form=~dateTime),
                     data = VSRdataV, method="REML", select=TRUE, gamma=1.4, na.rm = TRUE)
#model evaluation
summary(global.Gamm125) 
visreg(global.Gamm125)

global.Gamm125 = gam(TOL_125_PSDr ~ 
                       s(AISsog_avg) + s(PerBelow,AISl) + (Bio) + s(mth),
                     correlation = corCAR1(value=corACI, form=~dateTime),
                     data = VSRdataV, method="REML", select=TRUE, gamma=1.4, na.rm = TRUE)
#model evaluation
summary(global.Gamm125) 
visreg(global.Gamm125)


# just with vessel acoustic detections present
VSRdataVd = VSRdataV[VSRdataV$Dets == 1,]
global.Gamm125 = gam(TOL_125_PSDr ~ 
                       s(AISsog_avg) + s(PerBelow,AISl) + (Bio) + s(mth),
                     correlation = corCAR1(value=corACI, form=~dateTime),
                     data = VSRdataVd, method="REML", select=TRUE, gamma=1.4, na.rm = TRUE)



# REMOVED PLOTTING ####