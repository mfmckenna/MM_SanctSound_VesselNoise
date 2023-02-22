# process sanctuary sound vessel detection data and AIS results

#modified 1b-- wanted sound levels only during vessel detections for the "noise added"
# !!!using TOLs at 1 minute resolution

#tested for OC02 speed reduction
rm(list=ls())

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site processing
#-----------------------------------------------------------------------------------------
# Reads in both vessel detection 

#outputs 
# a per site .csv file: E:\RESEARCH\SanctSound\data2\combineFiles1_SPLShips
# summary table for average of all data per site (copied here):
# https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=888829761

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# LOAD Libraries
#-----------------------------------------------------------------------------------------
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)

#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 = function(x){(x-min(x))/(max(x)-min(x))} #normalize between 0-1
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
yearflag = 2020
site = "SB03"

# range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'

# some analysis and output flags 
flagCSV  = TRUE  #true if you want to output hourly and daily csv files per site
flagPLT  = FALSE  #true if you want to output summary plots of data
flagYear = TRUE   #true writes out a csv of yearly data to be copied into google docs

# OUTPUT details 
#!!!(FUTURE: create site loop here) !!!
# cat("Processing site...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
output  = NULL
tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
DC = Sys.Date()

#-----------------------------------------------------------------------------------------
# READ IN-- vessel detection
#-----------------------------------------------------------------------------------------
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
#vessel detections greater than 12 hour!!!
VD[VD$Dur > 3600*12,]
VD$DurH = VD$Dur/3600 

#-----------------------------------------------------------------------------------------
# VESSEL DETECTION DURATIONS- summary
#-----------------------------------------------------------------------------------------
VDmean = aggregate(VD$Dur,    by=list(VD$Mth,VD$Yr), mean, na.rm=T) 
VDsd =  aggregate(VD$Dur,    by=list(VD$Mth,VD$Yr), sd, na.rm=T) 
VDagg = cbind(VDmean, VDsd$x)
 ggplot(VD, aes(x=as.factor(Mth), y=(DurH), fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  theme_minimal()
colnames(VD)

ggplot(VD, aes(DurH,  color = as.factor(Mth) ) ) + stat_ecdf(geom = "point") + 
  facet_wrap(~Yr)+
  theme_minimal()

#-----------------------------------------------------------------------------------------
# ADD time stamps between detections for (non-vessel periods)
#-----------------------------------------------------------------------------------------
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
VDall$long[VDall$DurS > (3600*6)]  = "long"

#-----------------------------------------------------------------------------------------
# IS there a difference in Durations of vessel vs non vessel periods--- some initial graphics
#-----------------------------------------------------------------------------------------
# not very useful because slowdown periods are included...
# looking for monthly difference-- maybe longer ambient in louder ambient?

VDallt = VDall[VDall$DurS <= (3600*6),] #remove all detections >  6 hrs

ggplot(VDallt, aes(x=DurS/3600, color = Label )  )+
  geom_boxplot()+
  ggtitle ("Is there a difference in durations of vessel vs non vessel periods?")+
  theme_minimal()

sum1 = aggregate(VDallt$DurS,    by=list(VDallt$Mth, VDallt$Yr, VDallt$Label), mean, na.rm=T) 
sum12019 = sum1[ sum1$Group.2 == "2019",]

ggplot(sum12019, aes(x=as.factor(Group.1), y = x/3600, color =Group.3)  )+
  geom_line()+
  geom_point()+
  theme_minimal() +
  labs(x = "", y="mean monthly duration Hours")

ggplot(VDallt, aes(x=as.factor(Mth), y=(DurS)/3600, fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  labs(x="",y = "Duration-hours")+
  theme_minimal()

#-----------------------------------------------------------------------------------------
# Label vessel detections as in or out of slow down
#-----------------------------------------------------------------------------------------
VDall = VDall[VDall$Yr ==2019 , ]
slowStart = as.Date("2019-03-01")
slowEnd   = as.Date("2019-04-30")
#baseStart = as.Date("2019-07-12")
#baseEnd   = as.Date("2019-08-01")
#slowStartTrim = as.Date("2021-07-01")
#slowEndTrim   = as.Date("2021-08-01")

VDall$startDay = as.Date(VDall$Start)
min(VDall$startDay)

VDall$Period = "baseline"
VDall$Period [VDall$startDay >= slowStart & VDall$startDay <= slowEnd] = "slowdown"

# just compare vessel detection periods...
##-----------------------------------------------------------------------------------------
VDall2 = VDall[!is.na(VDall$Period), ] #remove all NA data
VDall2 = VDall2[(VDall2$Label) == "ship", ] #only ship periods
VDall2t = VDall2[VDall2$DurS <= (3600*6),] #remove all detections >  6 hrs

ggplot(VDall2t, aes(x=as.factor(Mth), y=(DurS)/3600, fill = as.factor(Period), color = as.factor(Period) )  )+
  geom_boxplot() +
  labs(x="",y = "Duration-hours")+
  theme_minimal()

# durations were longer during the slowdown?
ggplot(VDall2t, aes((DurS)/3600,  color = Period ) ) + stat_ecdf(geom = "point") + 
  theme_minimal()

# did the vessel noise periods differ in duration?
mean( VDall2t$DurS[VDall2t$Period == "baseline"] ) /60
sd( VDall2t$DurS[VDall2t$Period == "baseline"] ) /60

mean( VDall2t$DurS[VDall2t$Period == "slowdown"] )/ 60
sd( VDall2t$DurS[VDall2t$Period == "slowdown"] )/ 60
# FINDING: durations were only slightly longer during the slowdown-

#-----------------------------------------------------------------------------------------
# READ IN-- OTB
#-----------------------------------------------------------------------------------------
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE))
inFilesPSDF = ( list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE))
inFilesPSD  = basename(inFilesPSDF)
inFilesPSD

#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}

TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
  
# For each period- find corresponding sound levels and take the median-- takes a bit for all 2019
VDall3 = VDall[!is.na(VDall$Period), ]
head(VDall3)
output = NULL
for (ii in 1:nrow(VDall3)){
  
  #cat("processing period", ii, " in ",nrow(VDall3),as.character(VDall3$Start [ii]), "\n" )
  tmp = TOLmin[ TOLmin$DateF >= VDall3$Start [ii] & TOLmin$DateF <= VDall3$End [ii] ,] 
  if ( nrow(tmp) > 0) {
    tmpMax =  ( apply(tmp[,2:31],2,max) )
    tmpQuant = as.data.frame( apply(tmp[,2:31],2,quantile) )
    tmpMed = tmpQuant[3,]
    
    tmpo = cbind(tmpMax[8], tmpMed[8])
    colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
    tmpo = cbind(VDall3[ii,], tmpo)
    
    output = rbind(output,tmpo)
  }else  {
    tmpo = cbind( NA,NA)
    colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
    tmpo = cbind(VDall3[ii,], tmpo)
    output = rbind(output,tmpo)
  }
 

}

head(output)

#-----------------------------------------------------------------------------------------
# Add AIS data to vessel detections- matching transit times
#-----------------------------------------------------------------------------------------
AIStran = read.csv("E:\\RESEARCH\\SanctSound\\data2\\OC02\\smp_transit_data.csv")
AIStranOC = AIStran[ AIStran$loc_id == site,]

AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
output$AIS = 0
output$mDist = NA

for (ii in 1: nrow(output) ){
  #just getting if an AIS vessel start of transit occurred in this vessel detection period
  tmp = AIStranOC[AIStranOC$Start >= output$Start[ii] & AIStranOC$Start <= output$End[ii],] 
  if (  nrow(tmp) > 0 ){
    
    output$AIS[ii]   = nrow(tmp)
    output$mDist[ii] = min(tmp$dist_nm, na.rm = T)
  }
}

# assign labels to time periods AND PLOT 
# AIS vessels
output$Category[output$AIS > 0] = "A. AIS vessels" #AIS VESSEL PRESENT
output$Category[output$AIS == 0 & output$Label == "ship"] = "B. Non-AIS vessels"
output$Category[output$AIS == 0 & output$Label == "ambient"] = "C. Ambient"

unique(output$Category)
head(output)

# get difference from ambient- in previous sample
output$SNR = NA
output$SNRmax = NA

for (ii in 2:nrow(output)) {
  #check if previous sample is ambient...
  #output[ii,5:12]
  #output[ii-1,]
  
  #find the closest (in time) ambient sample
 
  
  # difference from to the previous sample
  output$SNR[ii]   = output$`TOL_125 median`[ii] - output$`TOL_125 median`[ii-1] 
  output$SNRmax[ii]= output$`TOL_125 max`[ii] - output$`TOL_125 max`[ii-1] 
  
}
output$SNRmax[output$Label == "ambient"] = NA  
output$SNR[output$Label    == "ambient"] = NA


# method 2 get difference from ambient- in previous sample
outputVD = output[ output$Category == "A. AIS vessels",]
outputAB = output[ output$Category == "C. Ambient",]
outputVD$SNR = NA
outputVD$SNRmax = NA
outputVD$SNRtime = NA
for (ii in 1:nrow(outputVD)) {
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD$Start[ii]-outputAB$Start))
  signalMed = outputVD$`TOL_125 median`[ii]
  noiseMed  = outputAB$`TOL_125 median`[idx]
  outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD$SNRtime[ii]= as.numeric( difftime(outputVD$Start[ii], outputAB$Start[idx], units = "mins") )
  
  signalMed = outputVD$`TOL_125 max`[ii]
  noiseMed  = outputAB$`TOL_125 max`[idx]
  outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
}
hist(  outputVD$SNRtime/60 )
# outputVD = outputVD[outputVD$SNRtime < 200, ]

outputVD2 = output[ output$Category == "B. Non-AIS vessels",]
outputVD2$SNR = NA
outputVD2$SNRmax = NA
outputVD2$SNRtime = NA

for (ii in 1:nrow(outputVD2)) {
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD2$Start[ii]-outputAB$Start))
  signalMed = outputVD2$`TOL_125 median`[ii]
  noiseMed  = outputVD2$`TOL_125 median`[idx]
  outputVD2$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD2$SNRtime[ii]= as.numeric( difftime(outputVD2$Start[ii], outputAB$Start[idx], units = "mins") )
  
  signalMed = outputVD2$`TOL_125 max`[ii]
  noiseMed  = outputVD2$`TOL_125 max`[idx]
  outputVD2$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVD = outputVD[outputVD$SNRtime < 200, ]
mB = median( outputVD$SNRmax[outputVD$Period =="baseline"], na.rm = T )
sd( outputVD$SNRmax[outputVD$Period =="baseline"], na.rm = T )
sB = nrow( outputVD[outputVD$Period =="baseline", ] )

mS = median( outputVD$SNRmax[outputVD$Period =="slowdown"], na.rm = T )
sd( outputVD$SNRmax[outputVD$Period =="slowdown"], na.rm = T )
sS = nrow( outputVD[outputVD$Period =="slowdown", ] )

outputVDall = rbind(outputVD2,outputVD)
hist(  outputVDall$SNRtime)

# CURRENT PLOT FOR LH..
ggplot(outputVDall, aes(x=Category, y=SNRmax, fill = Period )  )+
  geom_boxplot() +
  #ylim(c(-5, 50))+
  labs(title = "Difference in sound level when vessel present",
       subtitle = paste0("Baseline: ", round(mB, digits = 1), " dB (n=", sB, ")\n",
                         "Slowdown: ", round(mS, digits = 1), " dB (n=" ,sS, ")") )+ 
  xlab("")+  ylab("difference in dB (125 Hz 1/3 octave band)")+
  theme_minimal()



#-----------------------------------------------------------------------------------------
# compare the SNR of ships for the different periods...
#-----------------------------------------------------------------------------------------
head(output)
output2 = output[output$Category !="B. Non-AIS vessels", ] #remove "B. Non-AIS vessels" present 
output2 = output2[output2$Label  ==  "ship", ]
output2 = output2[output2$long  ==  "short", ]
idx = which( is.na(output2$SNR) )
output2 = output2[!is.na(output2$SNR), ]

ggplot(output2, aes(SNR,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detection SNR (n=", nrow(output2),")" )) +
  theme_minimal() 
ggplot(output2, aes(SNRmax,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detection SNRmax (n=", nrow(output2),")" )) +
  theme_minimal()

#-----------------------------------------------------------------------------------------
# Compare slowdown AIS vessel periods to baseline vessel periods
#-----------------------------------------------------------------------------------------
ggplot(output2, aes(`TOL_125 max`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detection 125 Hz TOL max  (n=", nrow(output2),")" )) +
  theme_minimal() 
ck = output2[output2$SNRmax < -10, ]
min(output2$`TOL_125 max`)
unique(as.Date( ck$Start) )
ggplot(output2, aes(`TOL_125 median`,  color = Period ) ) + 
  stat_ecdf(geom = "step") + 
  ggtitle (paste0("Comparision of vessel detection125 Hz TOL median  (n=", nrow(output2),")" )) +
  theme_minimal() 

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
