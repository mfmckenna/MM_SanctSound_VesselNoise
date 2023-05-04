# process sanctuary sound vessel detection data and AIS results

# OUTPUTS VD and TOL tables with label to read into plotting
#  INPUT: plot_VesselStory_manuscript_fig5-6,R (update this for VSR plots)

#NOTES ####
# this version creates a monthly graphic with noise added vs percent time with vessel noise
# !!!using TOLs at 1 minute resolution
# modified 1b-- wanted sound levels only during vessel detections for the "noise added"


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
library("viridis")  

# SETUP parameters ####
ra = 7 
range01 = function(x){(x-min(x))/(max(x)-min(x))}
stdize  = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
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
topDir =  "H:\\SanctSound\\data2\\"
outDir = "G:\\My Drive\\ActiveProjects\\SANCTSOUND\\combineFiles_VesselManuscript"
DC = Sys.Date()

yor = "2019"
sites = c("SB03", "GR01")
bySite = NULL

for (ss in 1:length(sites)){
  
  site1 = sites[ss] #site1 = "SB03"
  tDir   = paste0(topDir, site1, "\\")
  #outDir = paste0(topDir, site1, "\\")
  
  # VESSEL DETECTIONS ####
  ## READ IN vessel detection
  nFilesVD  = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
  inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
  inFilesVD = basename(inFilesVDF)
  sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
  depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)
  ## FORMATE vessel detection
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
  ## VESSEL DETECTION DURATIONS
  VDmean = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr, VD$Sant), mean, na.rm=T) 
  VDsd   = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr, VD$Sant), sd, na.rm=T) 
  VDagg = cbind(VDmean, VDsd$x)
  VD = VD[ VD$Yr == yor,]
  
  ## summary plot ####
  ggplot(VD, aes(x=as.factor(Mth), y=(DurH), fill = as.factor(Yr), color = as.factor(Yr) )  )+
    geom_boxplot() +
    ggtitle("Durations of Vessel Detections")+
    theme_minimal()
  
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
  ## summary plot ####
  ggplot(VDall, aes( y=DurS/3600, color=(Label))  )+
    geom_boxplot()+
    ggtitle ("Is there a difference in durations of vessel vs non vessel periods?")+
    theme_minimal()+ facet_wrap(~Sanctuary)
  
  # TOLs ####
  ## READ IN DATA
  nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE) )
  inFilesPSDF = list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE)
  inFilesPSD  = basename(inFilesPSDF)
  ## FORMATE TOL
  TOLmin = NULL
  for (ii in 1:(length(inFilesPSDF)) )  {
    tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
    TOLmin = rbind( TOLmin, tmpPSD)
  }
  TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
  TOLmin$Sant = site1 # TOL SOUND LEVELS
  rm(tmp,tmpPSD,r1,r2)
  TOLmin$yr =  year(TOLmin$DateF)
  TOLmin = TOLmin[ TOLmin$yr ==yor,] 
  
  # AIS data ####
  AIStran = read.csv(paste0(topDir, "SB03\\smp_transit_data.csv")) 
  AIStranOC = AIStran[ AIStran$loc_id == site1,]
  AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
  AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
  
  rm(AIStran,VDagg,VD,VDmean,VDsd)
  
  # MATCH VDs  ####
  ## get TOLs ####
  outputVD = NULL
  for (ii in 1:nrow(VDall) ){ # takes way too long!! not sure how to speed up?
    
    #get all TOLs for the duration of the detection period
    tmp = TOLmin[ TOLmin$DateF >= VDall$Start [ii] & TOLmin$DateF <= VDall$End [ii] ,]
    
    if ( nrow(tmp) > 0) {
      tmpMax   = apply(tmp[,fQI],2,max) 
      tmpQuant = as.data.frame( apply(tmp[,fQI],2, quantile) )
      tmpMed   = tmpQuant[3,]
      
      tmpo = cbind(tmpMax[FQsave], tmpMed[FQsave])
      colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
      tmpo = cbind(VDall[ii,], tmpo)
      outputVD = rbind(outputVD,tmpo)
      
    } else  { # not sound levels- which would be weird!
      tmpo = cbind( NA,NA)
      colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
      tmpo = cbind(VDall[ii,], tmpo)
      outputVD = rbind(outputVD, tmpo)
    } 
    
  }
  
  ## get AIS ####
  outputVD$AIS = 0
  outputVD$mDist = NA
  for (ii in 1:nrow(outputVD) ){
    #just getting if an AIS vessel start of transit occurred in this vessel detection period
    tmp = AIStranOC[AIStranOC$Start >= outputVD$Start[ii] & AIStranOC$Start <= outputVD$End[ii], ] 
    tmp = tmp[ tmp$loc_id ==outputVD$Sanctuary[ii], ] # only matching site
    
    if (  nrow(tmp) > 0 ){
      
      outputVD$AIS[ii]   = nrow(tmp)
      outputVD$mDist[ii] = min(tmp$dist_nm, na.rm = T)
    }
  }
  
  ## LABEL ####
  outputVD$Category[outputVD$AIS > 0  & outputVD$Label == "ship"] = "A. AIS vessels" # AIS VESSEL PRESENT and VD
  outputVD$Category[outputVD$AIS == 0 & outputVD$Label == "ship"] = "B. Non-AIS vessels"
  outputVD$Category[outputVD$AIS == 0 & outputVD$Label == "ambient"] = "C. Non-vessel"
  outputVD$Category[outputVD$AIS > 0  & outputVD$Label == "ambient"] =  "D. Nearby AIS"
 
  # aggregate(outputVD$`TOL_125 max`, by=list(outputVD$Category), mean, na.rm=T)
  tal = as.data.frame( outputVD %>% group_by(Category) %>% tally() )
  #aggregate(outputVD$`TOL_125 max`, by=list(outputVD$Label), mean, na.rm=T)
  
  ## PLOT TIME LINE OF VD CONDITIONS ####
  ggplot(outputVD, aes(x=Start, xend=End, y=Category, yend=Category, color=`TOL_125 max`)) +
    geom_segment()+
    theme_bw()+ 
    scale_y_discrete(limits=rev)+ 
    geom_segment(size=12) +
    xlab("")+  ylab("")+ ggtitle("Summary of Vessel Detection Periods") +
    labs(caption = (paste0("samples in each category: A=", tal$n[1]," | B=", tal$n[2]," | C=", tal$n[3]," | D=", tal$n[4] )))+
    scale_color_gradientn(colours = viridis(10))+
    theme(  axis.text.y = element_text(size = 14, colour="black"),
           axis.text.x=element_text(size = 14, colour="black"),
           plot.caption = element_text(size = 14) )
  
  rm(tal, tmp,tmpMed,tmpo, tmpQuant, VDall)
 
  # MATCH TOLs  ####
  ## VDs ####
  VD = outputVD[outputVD$Label == "ship", ]
  TOLmin$VD = 0
  for (ii in 1:nrow(outputVD) ){
    
    # find all TOLs rows that fall with the detection period
    idx =  which( TOLmin$DateF >= VD$Start[ii] &  TOLmin$DateF+60 < VD$End[ii] )
    #  Check : length(idx) -  (VD$End[ii] - VD$Start[ii])
   
    TOLmin$VD[idx] = ii  # label TOL rows with vessel detection number- should not be overlap!
    
  }
  unique(TOLmin$VD)
  
  ## AIS ####
  # MATCH AIS transit with TOL minute data ####
  TOLmin$AIS = 0
  TOLmin$AISs = 0
  TOLmin$AISm = 0
  TOLmin$AISl = 0
  TOLmin$AISu = 0
  for (ii in 1:nrow(AIStranOC) ){ 
    
    # find all TOLs rows that fall with the AIS vessel transit period
    idx =  which( TOLmin$DateF >= AIStranOC$Start[ii] &  TOLmin$DateF+60 <= AIStranOC$End[ii] )
    TOLmin$DateF [idx]
    # need to add values, if AIS transit was already added to the TOL
    TOLmin$AIS[idx] =  TOLmin$AIS[idx] + 1
    if(AIStranOC$loa[ii] < 50)                           { TOLmin$AISs[idx] = TOLmin$AISs[idx] + 1  }
    if(AIStranOC$loa[ii] >= 50 & AIStranOC$loa[ii] < 100){ TOLmin$AISm[idx] = TOLmin$AISm[idx] + 1  }
    if(AIStranOC$loa[ii] >= 100)                         { TOLmin$AISl[idx] = TOLmin$AISl[idx] + 1  }
    if(is.na(AIStranOC$loa[ii]) )                        { TOLmin$AISu[idx] = TOLminu$AIS[idx] + 1  }
  }
  ## LABEL ####
  TOLmin$mth = month(TOLmin$DateF)
  TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD > 0 ] =    "A. Vessel- AIS nearby" 
  TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD > 0 ] =    "B. Vessel- unk"
  TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD == 0] =    "D. Ambient"
  TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD == 0] =    "C. Ambient- AIS nearby"
  
  TOLmin$Label[TOLmin$VD > 0 ] =    "A. Vessel Detection" 
  TOLmin$Label[TOLmin$VD == 0 ] =   "B. Ambient"
  # aggregate(TOLmin$TOL_125, by=list(TOLmin$Label,TOLmin$mth), mean, na.rm=T)
  as.data.frame( TOLmin %>% group_by(Label, mth) %>% tally() )
  
  TOLmin$Slow = "No" 
  TOLmin$Slow[TOLmin$mth == 3 | TOLmin$mth == 4  ] =    "Slowdown" 
  # aggregate(TOLmin$TOL_125, by=list(TOLmin$Slow,TOLmin$mth), mean, na.rm=T)
  
  outputTOL = TOLmin
  rm(TOLmin)
  
  
  # SAVE RESULTS ####
  save(outputVD,  file=paste0(outDir, "\\outputVD_",site1, ".Rda") )
  save(outputTOL, file=paste0(outDir, "\\outputTOL_",site1, ".Rda") )
}

 
 
#   # EXTRA ####
#   #how many AIS without detections?
#   # nrow(outputVD[ outputVD$Category == "A. AIS vessels" & outputVD$Label == "ambient",] )/nrow(outputVD)
#     #as.data.frame( outputVD %>% group_by(Category) %>% tally() )
#   # as.data.frame( outputVD %>% group_by(Label) %>% tally() )
# 
#   #remove AIS vessels without detections
#   outputAll = output
#   output = output[ output$Category !=  "D. Nearby AIS" ,]
#   aggregate(output$`TOL_125 max`, by=list(output$Category), mean, na.rm=T)
#   aggregate(output$`TOL_125 max`, by=list(output$Label), mean, na.rm=T)
#   
#   # NOISE EXCEEDENCE ####
#   ## using AIS information ####
#   # only looks at times when vessel present- either confirmed with AIS or non-AIS from vessel detection
#     ### AIS Vessels ####
#   outputVD = output[ output$Category == "A. AIS vessels",]
#   outputAB = output[ output$Category == "C. Non-vessel",]
#   outputVD$SNR = NA
#   outputVD$SNRmax = NA
#   outputVD$SNRtime = NA
#   for (ii in 1:nrow(outputVD)) {
#     
#     tmpAB = outputAB[ outputAB$Sanctuary == outputVD$Sanctuary[ii] ,]
#     
#     #find the closest (in time) ambient sample
#     idx = which.min(abs(outputVD$Start[ii] - tmpAB$Start))
#     
#     signalMed = outputVD$`TOL_125 median`[ii]
#     noiseMed  = tmpAB$`TOL_125 median`[idx]
#     outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
#     outputVD$SNRtime[ii]= as.numeric( difftime(outputVD$Start[ii], tmpAB$Start[idx], units = "mins") )
#     
#     signalMed = outputVD$`TOL_125 max`[ii]
#     noiseMed  = tmpAB$`TOL_125 max`[idx]
#     outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
#   }
#   
#   ### NON-AIS Vessels ####
#   outputVD2 = output[ output$Category == "B. Non-AIS vessels",]
#   outputVD2$SNR = NA
#   outputVD2$SNRmax = NA
#   outputVD2$SNRtime = NA
#   for (ii in 1:nrow(outputVD2)) {
#     
#     tmpAB = outputAB[ outputAB$Sanctuary == outputVD2$Sanctuary[ii] ,]
#     
#     #find the closest (in time) ambient sample
#     idx = which.min(abs(outputVD2$Start[ii]-tmpAB$Start))
#     signalMed = outputVD2$`TOL_125 median`[ii]
#     noiseMed  = outputVD2$`TOL_125 median`[idx]
#     outputVD2$SNR[ii]   = signalMed - noiseMed # signal - noise
#     outputVD2$SNRtime[ii]= as.numeric( difftime(outputVD2$Start[ii], tmpAB$Start[idx], units = "mins") )
#     
#     signalMed = outputVD2$`TOL_125 max`[ii]
#     noiseMed  = outputVD2$`TOL_125 max`[idx]
#     outputVD2$SNRmax[ii]= signalMed - noiseMed # signal - noise
#   }
#   
#   outputVDall = rbind(outputVD2,outputVD)
#   outputVDall = outputVDall[ outputVDall$SNRtime < 1440 | outputVDall$SNRtime >-1440, ] #remove more than day difference for samples
#   ### summary plot ####
#   ggplot(outputVDall, aes(x=Category, y=SNRmax, fill = Sanctuary )  )+
#     geom_boxplot() + 
#     xlab("") + ylab("") +
#     ylim(c(-30,40))+
#     ggtitle("AIS data label: difference in dB (125 Hz 1/3 octave band)")+
#     theme_minimal()
#   
#   ## using Vessel DETECTIONS ####
#   outputVD3 = outputAll[ outputAll$Label == "ship" ,]
#   outputAB  = outputAll[ outputAll$Label == "ambient",]
#   outputVD3$SNR = NA
#   outputVD3$SNRmax = NA
#   outputVD3$SNRtime = NA
#   for (ii in 1:nrow(outputVD3)) {
#     
#     tmpAB = outputAB[ outputVD3$Sanctuary == outputVD3$Sanctuary[ii] ,]
#     
#     #find the closest (in time) ambient sample
#     idx = which.min(abs(outputVD3$Start[ii]-tmpAB$Start))
#     signalMed = outputVD3$`TOL_125 median`[ii]
#     noiseMed  = outputVD3$`TOL_125 median`[idx]
#     outputVD3$SNR[ii]   = signalMed - noiseMed # signal - noise
#     outputVD3$SNRtime[ii]= as.numeric( difftime(outputVD3$Start[ii], tmpAB$Start[idx], units = "mins") )
#     
#     signalMed = outputVD3$`TOL_125 max`[ii]
#     noiseMed  = outputVD3$`TOL_125 max`[idx]
#     outputVD3$SNRmax[ii]= signalMed - noiseMed # signal - noise
#   }
#   ### summary plot ####
#   ggplot(outputVD3, aes(x=Category, y=SNRmax, fill = Sanctuary )  )+
#     geom_boxplot() + 
#     xlab("") + ylab("") +
#     ylim(c(-30,40))+
#     ggtitle("ALL data: difference in dB (125 Hz 1/3 octave band)")+
#     theme_minimal()
#   
#   exceed_AIS = outputVDall # removes nearby AIS samples from the Exceedence metrics
#   exceed_VD = outputVD3
#   
#   rm(outputVDall, outputVD3, outputVD2, outputAB, outputVD, tmp, tmpAB, tmpMed, tmpo, tmpQuant )
#   
#   ## MONTHLY METRICS #### 
#   inputData = exceed_VD
#   inputData$Day = as.Date(inputData$Start)
#   inputData$Mins = inputData$DurS/(60)
#   hist(inputData$Mins)
#  
#   
#   # METRIC: mean and standard error max noise above closest non-vessel periods (for all vessel detection periods + AIS label)
#   VExcee_mu  = aggregate(inputData$SNRmax, by=list(inputData$Mth), mean, na.rm=T)
#   VExceed_se = aggregate(inputData$SNRmax, by=list(inputData$Mth), std.error, na.rm=T)
#   VD_n = as.data.frame( inputData %>% group_by(Mth) %>% tally() ) 
#   
#   Vexceed = merge( VExcee_mu, VExceed_se, by = "Group.1", all.y = FALSE) 
#   colnames(Vexceed ) = c("Mth","SNRmax_mean","SNRmax_se")
#   Vexceed = merge( Vexceed, VD_n, by = "Mth") 
#   Vexceed$Site = site1
#   
#   # VESSEL NOISE Dominance ####
#   # METRIC: mean and se of daily percent of time vessel noise
#   #total possible minutes sampled per day, per site
#   TOLmin$Day = as.Date(TOLmin$DateF)
#   daySamples = as.data.frame( TOLmin %>%  group_by(Day) %>%  tally() ) 
#   daySamples$yr = year(daySamples$Day)
#   daySamples = daySamples[daySamples$yr == yor,]
#   
#   #total vessel minutes per day (can be over total time if last detection is into next day)
#   VDomina = aggregate(inputData$Mins, by=list(inputData$Day), sum, na.rm=T)
#   colnames(VDomina) = c("Day","Vessel_mins")
#   VDomina = merge(daySamples, VDomina, by = c("Day"), all = TRUE )
#   VDomina$PerDay = (VDomina$Vessel_mins/VDomina$n)*100
#   VDomina$PerDay[is.na(VDomina$PerDay)] <- 0
#   VDomina$Mth = month(VDomina$Day)
#   
#   ## summary plot ####
#   ggplot(VDomina, aes(x = Mth, PerDay )  )+
#     geom_point() + 
#     xlab("") + 
#     ylab("")+ 
#     ggtitle("Vessel Noise per day")+
#     theme_minimal()
#   
#   VDominaMth_mu = aggregate(VDomina$PerDay,   by=list(VDomina$Mth), mean, na.rm=T)
#   VDominaMth_se = aggregate(VDomina$PerDay,   by=list(VDomina$Mth), std.error, na.rm=T)
#   VDomina = merge( VDominaMth_mu, VDominaMth_se, by = "Group.1", all.y = FALSE) 
#   colnames(VDomina ) = c("Mth","PerDay_mean","PerDay_se")
#   
#   VP = merge(Vexceed, VDomina, by = "Mth")
#   
#   ## summary plot ####
#   ggplot(data=VP, aes( x = SNRmax_mean, y = PerDay_mean, color = as.factor( Mth ))  ) +
#     geom_point(size = VP$n/10) +
#     geom_errorbar(aes(xmin=SNRmax_mean-SNRmax_se, xmax=SNRmax_mean+SNRmax_se), position=position_dodge(.5), width=.2, alpha = .2) +
#     geom_errorbar(aes(ymin=PerDay_mean-PerDay_se, ymax=PerDay_mean+PerDay_se), position=position_dodge(.5), width=.2, alpha = .2) +
#     geom_text(data = VP, aes(x=SNRmax_mean, y=PerDay_mean, label = Mth ), vjust = 0, nudge_y = 0.5, size = 6) +
#     theme_minimal() +
#     xlim(c(-8,10))+ ylim(c(0,80))+
#     labs( x = "", y = "", title = "", caption = "", subtitle = "") +
#     theme( legend.position = "none", axis.text.y = element_text(size = 14, colour="black"),
#            axis.text.x=element_text(size = 14, colour="black"))
#   
#   ggplot(data=VP, aes( x = as.factor(Mth) , y = PerDay_mean ) ) +
#     #geom_bar(stat= "identity" )
#     geom_point(size = range01(VP$SNRmax_mean)*10 )+
#     geom_errorbar(aes(ymin=PerDay_mean-PerDay_se, ymax=PerDay_mean+PerDay_se), position=position_dodge(.5), width=.2, alpha = .2) +
#     theme_minimal() +
#     labs( x = "", y = "", title = "", caption = "", subtitle = "") +
#     theme( legend.position = "none", axis.text.y = element_text(size = 14, colour="black"),
#                                                           axis.text.x=element_text(size = 14, colour="black"))
#   
#   
#   bySite = rbind(bySite,VP)
# 
# #normalize the number of vessel detections metrics 
# bySite$nN = range01(bySite$n)
# 
# # SAVE OUT FILE ####
# 
# ## PLOT ####
# ggplot(data=bySite, aes( x = SNRmax_mean, y = PerDay_mean, color = as.factor(Mth)) ) + #color = log10(n))
#   geom_point(size = 5) + #size = bySite$n/10
#   geom_errorbar(aes(xmin=SNRmax_mean-SNRmax_se, xmax=SNRmax_mean+SNRmax_se), position=position_dodge(.5), width=.2, alpha = .2) +
#   geom_errorbar(aes(ymin=PerDay_mean-PerDay_se, ymax=PerDay_mean+PerDay_se), position=position_dodge(.5), width=.2, alpha = .2) +
#   geom_text(data = bySite, aes(x=SNRmax_mean, y=PerDay_mean, label = Mth ), vjust = 1.5, nudge_y = 0, size = 5) +
#   theme_minimal() +
#   xlim(c(-5,5))+ ylim(c(-1,75))+
#   labs( x = "Noise Exceedence", y = "Noise Dominance", title = "Vessel Noise Presense (SB01 and GR01 with 1 min TOL)", caption = "", subtitle = "") +
#   theme( legend.position = "none", axis.text.y = element_text(size = 14, colour="black"),
#          axis.text.x=element_text(size = 14, colour="black"),
#          title=element_text(size = 14) )
# 
