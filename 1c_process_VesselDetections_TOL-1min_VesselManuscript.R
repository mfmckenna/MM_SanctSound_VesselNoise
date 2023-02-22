# process sanctuary sound vessel detection data and AIS results

#NOTES ####
# this version creates a monthly graphic with noise added vs percent time with vessel noise
# !!!using TOLs at 1 minute resolution
# modified 1b-- wanted sound levels only during vessel detections for the "noise added"
# FUTURE- update with loop through sites of interest- it is a bit cluncky with how set up with two sites of interest

rm(list=ls())

# MAIN functions-- by site processing
# Reads in both vessel detection an AIS
#outputs 
# a per site .csv file: E:\RESEARCH\SanctSound\data2\combineFiles1_SPLShips
# summary table for average of all data per site (copied here):
# https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=888829761

# LOAD Libraries ####
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)
library(plotrix)

# SETUP parameters ####
ra = 7 #days for running average
range01 = function(x){(x-min(x))/(max(x)-min(x))} #normalize between 0-1
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
yearflag = 2020
frqs = c("DateF", "TOL_31.5", "TOL_40", "TOL_50", "TOL_63", "TOL_80", "TOL_100", "TOL_125", "TOL_160", "TOL_200", "TOL_250", "TOL_315", "TOL_400", "TOL_500", "TOL_630", "TOL_800", 
         "TOL_1000", "TOL_1250", "TOL_1600", "TOL_2000", "TOL_2500", "TOL_3150", "TOL_4000", "TOL_5000", "TOL_6300", "TOL_8000", "TOL_10000", "TOL_12500", "TOL_16000", "TOL_20000")
fQI = c( "TOL_31.5", "TOL_40", "TOL_50", "TOL_63", "TOL_80", "TOL_100", "TOL_125", "TOL_160", "TOL_200", "TOL_250", "TOL_315", "TOL_400", "TOL_500", "TOL_630", "TOL_800", 
         "TOL_1000", "TOL_1250", "TOL_1600", "TOL_2000", "TOL_2500", "TOL_3150", "TOL_4000", "TOL_5000", "TOL_6300", "TOL_8000", "TOL_10000", "TOL_12500", "TOL_16000", "TOL_20000")
FQsave = "TOL_125"

# range of dates for output graphics
sDatePlot = '2018-11-01'
eDatePlot = '2020-12-31'
# some analysis and output flags 
flagCSV  = TRUE  #true if you want to output hourly and daily csv files per site
flagPLT  = FALSE  #true if you want to output summary plots of data
flagYear = TRUE   #true writes out a csv of yearly data to be copied into google docs

# SB03 ####
## READ IN vessel detection ####
site1 = "SB03"
output  = NULL
tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site1, "\\")
outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site1, "\\")
DC = Sys.Date()
nFilesVD  = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVDF)
sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)

VD=NULL
for (ii in 1:(nFilesVD)) {
  tmp = read.csv(inFilesVDF[ii])
 # names(tmp)
  tmp$Sant = sant
  tmp$Dep  = depl[ii]
  colnames(tmp) = c("ISOStartTime","ISOEndTime","Label","Sant","Depl" )
  
  VD = rbind(VD,tmp)
}
VD$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOStartTime)), tz = "GMT" )
VD$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD$ISOEndTime)), tz = "GMT" )
VD$Mth = month(VD$Start )
VD$Yr  = year(VD$Start )
VD$Dur = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )

VD$Dep  = as.numeric(as.character(VD$Dep))
VD$DepC = c(0,diff(VD$Dep))
indx = which(VD$DepC == 1) #find transitions in deployments
VD[VD$Dur > 3600*12,] #vessel detections greater than 12 hour!!!
VD$DurH = VD$Dur/3600 

VD$Dep  = as.numeric(as.character(VD$Dep))
VD$DepC = c(0, diff(VD$Dep))
indx = which(VD$DepC == 1) #find transitions in deployments
# VD[VD$Dur > 3600*12,] #vessel detections greater than 12 hour!!!
VD$DurH = VD$Dur/3600 

## READ IN TOL ####
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE) )
inFilesPSDF = list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE)
inFilesPSD  = basename(inFilesPSDF)
inFilesPSD
#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}
TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 

## OUTPUTS ####
TOLmin1 = TOLmin
TOLmin1 = TOLmin1[,frqs ] # trim to frequencies of interest
TOLmin1$Site = site1
VD1 = VD

# GR01 ####
## READ IN vessel detection ####
site2 = "GR01"
output  = NULL
tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site2, "\\")
outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site2, "\\")
DC = Sys.Date()
nFilesVD  = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVDF)
sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)

VD2 = NULL
for (ii in 1:(nFilesVD)) {
  tmp = read.csv(inFilesVDF[ii])
  # names(tmp)
  tmp$Sant = sant
  tmp$Dep  = depl[ii]
  colnames(tmp) = c("ISOStartTime","ISOEndTime","Label","Sant","Depl" )
  
  VD2 = rbind(VD2,tmp)
}
VD2$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD2$ISOStartTime)), tz = "GMT" )
VD2$End   =  as.POSIXct( gsub(".000Z", "", gsub("T", " ", VD2$ISOEndTime)),   tz = "GMT" )
VD2$Mth = month(VD2$Start )
VD2$Yr  = year(VD2$Start )
VD2$Dur = as.numeric(as.character( difftime(VD2$End, VD2$Start, units = "secs" )) )

VD2$Dep  = as.numeric(as.character(VD2$Dep))
VD2$DepC = c(0,diff(VD2$Dep))
indx = which(VD2$DepC == 1) #find transitions in deployments
# VD2[VD2$Dur > 3600*12,] #vessel detections greater than 12 hour!!!
VD2$DurH = VD2$Dur/3600 

## READ IN TOLs ####
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE) )
inFilesPSDF = list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE)
inFilesPSD  = basename(inFilesPSDF)
inFilesPSD
#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}
TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
TOLmin2 = TOLmin
TOLmin2 = TOLmin2[,frqs ] # trim to frequencies of interest
TOLmin2$Site = site2
#clean up!
rm(VD,TOLmin,tmp,tmpPSD)

# VESSEL DETECTION DURATIONS- summary ####
VD = rbind(VD1,VD2)
colnames(VDc)
VDmean = aggregate(VD$Dur,    by=list(VD$Mth, VD$Yr, VD$Sant), mean, na.rm=T) 
VDsd   = aggregate(VD$Dur,    by=list(VD$Mth, VD$Yr, VD$Sant), sd, na.rm=T) 
VDagg = cbind(VDmean, VDsd$x)
## summary plot ####
ggplot(VD, aes(x=as.factor(Mth), y=(DurH), fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  ggtitle("Durations of Vessel Detections")+
  theme_minimal()+
  facet_wrap(~Sant)

# NON-VESSEL PERIODS ####
VDall = NULL
for (ii in 1:(nrow(VD) -1 ) )  {
  
  if (VD$DepC [ii+1] == 0) {
    tpL = "ambient"
    tpS = VD$End [ii] + 1
    tpE = VD$Start [ii+1] - 1
    tpD = as.numeric( difftime(tpE, tpS, units = "secs") )
    
    #recombine and build new matrix
    r1 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], as.POSIXct(VD$Start[ii]), as.POSIXct(VD$End[ii]), VD$Label[ii], (VD$Dur[ii]) ) 
    colnames(r1)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    
    r2 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], tpS, tpE, tpL, tpD) 
    colnames(r2)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    
    VDall =  rbind(VDall , rbind.data.frame(r1,r2) )
    
  } else {
    r1 =  cbind.data.frame( VD$Sant[ii], VD$Dep[ii], as.POSIXct(VD$Start[ii]), as.POSIXct(VD$End[ii]), VD$Label[ii], (VD$Dur[ii])) 
    colnames(r1)= c("Sanctuary", "Deployment", "Start", "End","Label", "DurS")
    VDall =  rbind(VDall , r1) 
    
  }
  
}
VDall$Mth = month(VDall$Start)
VDall$Yr = year(VDall$Start)
VDall$Hr = hour(VDall$Start)
VDall$long[VDall$DurS <= (3600*6)] = "short"
VDall$long[VDall$DurS >  (3600*6)]  = "long"
# IS there a difference in Durations of vessel vs non vessel periods
VDallt = VDall
VDallt = VDall[VDall$DurS > 0 ,]
## summary plot ####
ggplot(VDallt, aes( x=DurS/3600, color=(Label))  )+
  geom_boxplot()+
  ggtitle ("Is there a difference in durations of vessel vs non vessel periods?")+
  theme_minimal()+ facet_wrap(~Sanctuary)

# MATCH VDs WITH TOLs ####
TOLmin = rbind(TOLmin1,TOLmin2)
output = NULL
st = unique(TOLmin$Site)
for (ss in 1:length(st)) {
  
  tmpTOL = TOLmin[TOLmin$Site == st[ss], ]
  tmpVD  = VDallt[VDallt$Sanctuary== st[ss], ]
  
   for (ii in 1:nrow(tmpVD) ){
     
     #get all TOLs for the duration of the detection period
     tmp = tmpTOL[ tmpTOL$DateF >= tmpVD$Start [ii] & tmpTOL$DateF <= tmpVD$End [ii] ,]
     
     if ( nrow(tmp) > 0) {
       tmpMax =   apply(tmp[,fQI],2,max) 
       tmpQuant = as.data.frame( apply(tmp[,fQI],2, quantile) )
       tmpMed =   tmpQuant[3,]
       
       tmpo = cbind(tmpMax[FQsave], tmpMed[FQsave])
       colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
       tmpo = cbind(tmpVD[ii,], tmpo)
       
       output = rbind(output,tmpo)
     }else  {
       tmpo = cbind( NA,NA)
       colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
       tmpo = cbind(tmpVD[ii,], tmpo)
       output = rbind(output,tmpo)
     } 
     
   }
}

## summary plot ####
ggplot(output, aes( y=`TOL_125 max`, color=(Label))  )+
  geom_boxplot()+
  ggtitle ("Variation in sound level")+
  theme_minimal() + facet_wrap(~Sanctuary)


# MATCH with AIS data ####
#to vessel detections- matching transit times
AIStran = read.csv("F:\\RESEARCH\\SanctSound\\data2\\OC02\\smp_transit_data.csv")
AIStranOC = AIStran[ AIStran$loc_id == st,]
AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
output$AIS = 0
output$mDist = NA
for (ii in 1:nrow(output) ){
  #just getting if an AIS vessel start of transit occurred in this vessel detection period
  tmp = AIStranOC[AIStranOC$Start >= output$Start[ii] & AIStranOC$Start <= output$End[ii], ] 
  tmp = tmp[ tmp$loc_id ==output$Sanctuary[ii], ] # only matching site
  
  if (  nrow(tmp) > 0 ){
   
    output$AIS[ii]   = nrow(tmp)
    output$mDist[ii] = min(tmp$dist_nm, na.rm = T)
  }
}

output$Category[output$AIS > 0] = "A. AIS vessels" #AIS VESSEL PRESENT
output$Category[output$AIS == 0 & output$Label == "ship"] = "B. Non-AIS vessels"
output$Category[output$AIS == 0 & output$Label == "ambient"] = "C. Non-vessel"
unique(output$Category)
head(output)

# CALCULATE- difference from Ambient ####
## AIS Vessels ####
outputVD = output[ output$Category == "A. AIS vessels",]
outputAB = output[ output$Category == "C. Non-vessel",]
outputVD$SNR = NA
outputVD$SNRmax = NA
outputVD$SNRtime = NA

for (ii in 1:nrow(outputVD)) {
  
  tmpAB = outputAB[ outputAB$Sanctuary == outputVD$Sanctuary[ii] ,]
  
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD$Start[ii] - tmpAB$Start))
  
  signalMed = outputVD$`TOL_125 median`[ii]
  noiseMed  = tmpAB$`TOL_125 median`[idx]
  outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD$SNRtime[ii]= as.numeric( difftime(outputVD$Start[ii], tmpAB$Start[idx], units = "mins") )
  
  signalMed = outputVD$`TOL_125 max`[ii]
  noiseMed  = tmpAB$`TOL_125 max`[idx]
  outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

## NON-AIS Vessels ####
outputVD2 = output[ output$Category == "B. Non-AIS vessels",]
outputVD2$SNR = NA
outputVD2$SNRmax = NA
outputVD2$SNRtime = NA

for (ii in 1:nrow(outputVD2)) {
  
  tmpAB = outputAB[ outputAB$Sanctuary == outputVD2$Sanctuary[ii] ,]
  
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD2$Start[ii]-tmpAB$Start))
  signalMed = outputVD2$`TOL_125 median`[ii]
  noiseMed  = outputVD2$`TOL_125 median`[idx]
  outputVD2$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD2$SNRtime[ii]= as.numeric( difftime(outputVD2$Start[ii], tmpAB$Start[idx], units = "mins") )
  
  signalMed = outputVD2$`TOL_125 max`[ii]
  noiseMed  = outputVD2$`TOL_125 max`[idx]
  outputVD2$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVDall = rbind(outputVD2,outputVD)
hist(  outputVDall$SNRtime)
#remove more than day difference for samples
outputVDall  = outputVDall[ outputVDall$SNRtime < 1440 | outputVDall$SNRtime >-1440, ]

## summary plot ####
ggplot(outputVDall, aes(x=Category, y=SNRmax, fill = Sanctuary )  )+
  geom_boxplot() + 
  xlab("") + 
  ylab("difference in dB (125 Hz 1/3 octave band)")+
  theme_minimal()

# Vessel Noise Presence #### 
#want to plot each site- average above ambient when vessel present by percent of month with vessel noise present
head( outputVDall)

# VESSEL NOISE EXCEEDENCE ####
# METRIC: mean and standard error max noise above closest non-vessel periods (for all vessel detection periods)
VExcee_mu  = aggregate(outputVDall$SNRmax, by=list(outputVDall$Mth, outputVDall$Sanctuary), mean, na.rm=T)
VExceed_se = aggregate(outputVDall$SNRmax, by=list(outputVDall$Mth, outputVDall$Sanctuary), std.error, na.rm=T)

# VESSEL NOISE Dominance ####
# METRIC: mean and se of daily percent of time vessel noise
#total possible minutes sampled per day, per site
TOLmin$Day = as.Date(TOLmin$DateF)
daySamples = as.data.frame( TOLmin %>% 
  group_by(Day, Site) %>%
  tally() ) 

#total vessel minutes per day (can be over total time if last detection is into next day)
VDomina = aggregate(outputVDall$Mins, by=list(outputVDall$Day, outputVDall$Sanctuary), sum, na.rm=T)
colnames(VDomina) = c("Day","Site","Vessel_mins")
ggplot(VDomina, aes(Day, Vessel_mins)  )+
  geom_point() + 
  xlab("") +  ylab("")+
  facet_wrap(~Site) +
  ggtitle("Vessel NOise per day") +
  theme_minimal()
# GRRR THIS DOES NOT LOOK RIGHT ####

VDomina = merge(daySamples, VDomina, by = c("Day","Site"), all = TRUE )
VDomina$PerDay = VDomina$Vessel_mins/VDomina$n
VDomina$PerDay[is.na(VDomina$PerDay)] <- 0
VDomina$Mth = month(VDomina$Day)

## summary plot ####
ggplot(VDomina, aes(x = Mth, PerDay )  )+
  geom_point() + 
  xlab("") + 
  ylab("")+
  facet_wrap(~Site)
  ggtitle("Vessel NOise per day")
  theme_minimal()


VDominaMth = aggregate(VDomina$PerDay,   by=list(VDomina$Mth, VDomina$Site), mean, na.rm=T)
VDominaMth = aggregate(VDomina$PerDay,   by=list(VDomina$Mth, VDomina$Site), std.error, na.rm=T)

VP = cbind(VExcee_mu, VExceed_se$x, VDominaMth$x*100, VDominaMth$x)
colnames(VP) = c("Month","Site","Exceed_125mu","Exceed_125se", "Dommu","Domse")
## summary plot ####
ggplot(data=VP, aes( x = Exceed_125mu, y = Dommu, color = as.factor( Month ), shape=factor(Site))  ) +
  geom_point(size = 5)+
  geom_errorbar(aes(xmin=Exceed_125mu-Exceed_125se, xmax=Exceed_125mu+Exceed_125se), position=position_dodge(.5), width=.2, alpha = .2) +
  geom_errorbar(aes(ymin=Dommu-Domse, ymax=Dommu+Domse), position=position_dodge(.5), width=.2, alpha = .2) +
  
  theme_minimal()+
  
  labs( x = "", y = "", title = "", caption = "", subtitle = "") +

  theme( legend.position = "none",axis.text.y = element_text(size = 12, colour="black"),
         axis.text.x=element_text(size = 10,colour="black"))


#-----------------------------------------------------------------------------------------
# just remove ambient samples with AIS detections to plot all three categories
# COPY THISE PLOTS TO PPT https://docs.google.com/presentation/d/1SJd4pKsQn46464IH-L5fOiqt5-WUT-11MCg4V0IRdeU/edit#slide=id.g13e85015171_0_63
#-----------------------------------------------------------------------------------------
idx = which( output$Label == "ambient" & output$AIS > 0) #558!!
output3 = output[-idx, ]
output3 = output3[output3$long  ==  "short", ]

unique(output3$Category)

ggplot(output3, aes(x=Category, y=SNR, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of sound levels (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "median Sound Level (125 Hz TOL)")+
  theme_minimal()
median( output3$SNRmax[output3$Period =="baseline"], na.rm = T )
sd( output3$SNRmax[output3$Period =="baseline"], na.rm = T )
nrow( output3[output3$Period =="baseline", ] )

median( output3$SNRmax[output3$Period =="slowdown"], na.rm = T )
sd( output3$SNRmax[output3$Period =="slowdown"], na.rm = T )
nrow( output3[output3$Period =="slowdown", ] )


ggplot(output3, aes(x=Category, y=`TOL_125 max`, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of sound levels (125 TOL) (n=", nrow(output3),")" )) +
  labs(x="",y = "max Sound Level (125 Hz TOL)")+
  theme_minimal()

ggplot(output3, aes(x=Category, y=DurS/3600, fill = as.factor(Period) )  )+
  geom_boxplot() +
  ggtitle (paste0("Comparision of duration of events (n=", nrow(output3),")" )) +
  labs(x="",y = "Duration of event (Hours)")+
  theme_minimal()


#-----------------------------------------------------------------------------------------
# Comparisons-- AIS vessel vs non-vessel (after removing ambient samples with AIS )
#-----------------------------------------------------------------------------------------
output2 = rbind(outputShipAIS, outputNShipAIS)
nrow(output)- nrow(output2)

#combine Label and period into same variable to plot with Labelc
output2$Condition = as.factor(  paste0( output2$Period,"-", output2$Label ) )

#is the difference from ambient less?
ggplot(output2, aes(`TOL_125 median`, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="median Sound Level (125 Hz TOL)",y = "Exceedance Probability")+
  theme_minimal()
#is the difference from ambient less?
ggplot(output2, aes(`TOL_125 max`, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="max Sound Level (125 Hz TOL)",y = "Exceedance Probability")+
  theme_minimal()

#difference in SNR- greater in slowdown because lower ambient
ggplot(output2, aes(SNR, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="SNR (125 Hz TOL)",y = "Exceedance Probability")+
  ggtitle (paste0("Comparision of SNR (125 TOL) (n=", nrow(output3),")" )) +
  theme_minimal()

#difference in SNR- same because baseline had lower ambient louder ships/ slowdown had higher ambient and quieter ships
ggplot(output2, aes(SNRmax, fill = Condition, color = Condition )  )+
  stat_ecdf(geom = "step")+
  scale_color_manual(values=c("gray", "black","pink","red"))+
  labs(x="SNR (125 Hz TOL)",y = "Exceedance Probability")+
  ggtitle (paste0("Comparision of SNRmax (125 TOL) (n=", nrow(output3),")" )) +
  theme_minimal()

# Time sampled in each category-- add to above graphs
sum(output2$DurS[ output2$Condition == "baseline-ambient"])/3600
sum(output2$DurS[ output2$Condition == "baseline-ship"])/3600
sum(output2$DurS[ output2$Condition == "slowdown-ambient"])/3600
sum(output2$DurS[ output2$Condition == "slowdown-ship"])/3600

# SHIFT IN CONDIION

#difference from non-vessel periods
NA_baseline = median(output2$`TOL_125 median`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "baseline-ambient"], na.rm = T )

NAmax_baseline = median(output2$`TOL_125 max`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 max`[ output2$Condition == "baseline-ambient"], na.rm = T )

NA_slowdown = median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ambient"], na.rm = T )
 
NAmax_slowdown = median(output2$`TOL_125 max`[ output2$Condition == "slowdown-ship"], na.rm = T )-
  median(output2$`TOL_125 max`[ output2$Condition == "slowdown-ambient"], na.rm = T )

diff_periods = median(output2$`TOL_125 median`[ output2$Condition == "baseline-ship"], na.rm = T )-
  median(output2$`TOL_125 median`[ output2$Condition == "slowdown-ship"], na.rm = T )

# Average duration category
mean(output2$DurS[ output2$Condition == "baseline-ambient"])/3600

durVes_baseline     = mean(output2$DurS[ output2$Condition == "baseline-ship"])/3600
sddurVes_baseline   = (sd(output2$DurS[ output2$Condition == "baseline-ship"])/3600) / nrow(output2)
SamplesVes_baseline = (length(output2$DurS[ output2$Condition == "baseline-ship"]) ) 
TimesVes_baseline   = (sum(output2$DurS[ output2$Condition == "baseline-ship"]) ) /3600
PerTimeVessel_baseline = TimesVes_baseline/ (TimesVes_baseline + ((sum(output2$DurS[ output2$Condition == "baseline-ambient"]) ) /3600)) *100

mean(output2$DurS[ output2$Condition == "slowdown-ambient"])/3600
durVes_Slowdown     = mean(output2$DurS[ output2$Condition == "slowdown-ship"])/3600
sddurVes_Slowdown   = (sd(output2$DurS[ output2$Condition == "slowdown-ship"])/3600)  / nrow(output2)
SamplesVes_Slowdown = (length(output2$DurS[ output2$Condition == "slowdown-ship"]) ) 
TimesVes_Slowdown   = (sum(output2$DurS[ output2$Condition == "slowdown-ship"]) ) /3600
PerTimeVessel_Slowdown = TimesVes_Slowdown/ (TimesVes_Slowdown + ((sum(output2$DurS[ output2$Condition == "slowdown-ambient"]) ) /3600)) *100


#duration of ambient: 0.2 hrs vs 0.4 hrs (longer in 2021-- interesting)
#duration of VD: 0.7 vs 0.7 (no change in duration of VD)

cmplt = as.data.frame( rbind(    c("Baseline-2019", NA_baseline, NAmax_baseline, durVes_baseline, sddurVes_baseline, TimesVes_baseline ), 
                                 c("Slowdown-2021", NA_slowdown, NAmax_slowdown, durVes_Slowdown, sddurVes_Slowdown, TimesVes_Slowdown )) )
colnames(cmplt) = c("Period","NoiseAbove","NoiseAboveMax",  "Duration", "Dursd","DaySampled")

cmplt$NoiseAbove = as.numeric(as.character(cmplt$NoiseAbove ))
cmplt$NoiseAboveMax = as.numeric(as.character(cmplt$NoiseAboveMax ))
cmplt$Duration = as.numeric(as.character(cmplt$Duration ))
cmplt$Dursd = as.numeric(as.character(cmplt$Dursd ))
cmplt$DaySampled = as.numeric(as.character(cmplt$DaySampled ))

ggplot(cmplt, aes(x = NoiseAbove, y = Duration, color = Period, size = DaySampled)) +
  geom_point() + 
  #geom_text(label = floor(cmplt$DaySampled), nudge_y = 0.25, check_overlap = T, label.size = 0.35) +
  geom_pointrange(aes( ymin= Duration - Dursd, ymax= Duration+Dursd ))+
  geom_label(label = paste0("Baseline (2019) ", as.character( round(cmplt$DaySampled[1])) , " hrs") , x = 6, y = .7,color = "gray")+
  geom_label(label = paste0("Slowdown (2021) ", as.character( round(cmplt$DaySampled[2])) , " hrs")  , x = 7.5, y = .8,color = "orange")+
  ylim(c(.5,1))+
  xlim(c(0,10))+
  xlab("Noise above non-vessel (median 125 TOL) ")+
  ylab("Duration of AIS vessel detections (hours) ")+  
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal() +
  theme(legend.position="none")+ 
  ggtitle("Comparision of vessel noise conditions \n Median Noise Above & Duration")+
  theme(text = element_text(size = 16))
  
ggplot(cmplt, aes(x = NoiseAboveMax, y = Duration, color = Period, size = DaySampled)) +
  geom_point() + 
  #geom_text(label = floor(cmplt$DaySampled), nudge_y = 0.25, check_overlap = T, label.size = 0.35) +
  geom_pointrange(aes( ymin= Duration - Dursd, ymax= Duration+Dursd ))+
  geom_label(label = paste0("Baseline (2019) ", as.character( round(cmplt$DaySampled[1])) , " hrs") ,  x = 11, y = .7,color = "gray")+
  geom_label(label = paste0("Slowdown (2021) ", as.character( round(cmplt$DaySampled[2])) , " hrs")  , x = 11, y = .8,color = "orange")+
  ylim(c(.5,1))+
  xlim(c(0,15))+
  xlab("Noise above non-vessel (max 125 TOL) ")+
  ylab("Duration of AIS vessel detections (hours) ")+  
  scale_color_manual(values=c('#999999','#E69F00'))+
  theme_minimal() +
  theme(legend.position="none")+ 
  ggtitle("Comparision of vessel noise conditions \n Max Noise Above & Duration")+ 
  theme(text = element_text(size = 16))
#-----------------------------------------------------------------------------------------
# pie graphs of difference in ship traffic
#-----------------------------------------------------------------------------------------
dt = data.table(X = c(1,70), Y = c(1,1), Uships = c(33, 31), Names = c("Baseline (2019)","Slowdown (2021)"), ships = rbind(c(10, 11, 78), c(7,10,80))) 
dt = as.data.frame(dt)
colnames(dt) = c("X", "Y", "UniqueShips", "Period", "Small", "Medium", "Large" )

library(scatterpie)
ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dt,
                  cols=c("Small", "Medium", "Large") )  + coord_equal()+
  geom_text(data = dt, aes(x=X, y=Y-1,  label = Period ), size = 3)  +
  ylab("")+xlab("")+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
  
  

geom_map(map=world, aes(map_id=region), fill="gray90", color="black") +
  geom_point(data = siteLoc, aes(x=lon, y=lat), color = "red", size = 1) +
  geom_text(data = siteLoc, aes(x=lon, y=lat+.5,  label = Names ), size = 3)  +
  coord_quickmap()+
  xlim(-170,-150) + ylim(18, 30) +
  theme_minimal()
p

p2 = p +  
  
  p2
