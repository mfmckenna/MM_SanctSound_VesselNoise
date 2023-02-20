# Matching the 1-min TOLs with vessel detections and AIS
rm(list=ls())
# Vessel manuscript processing GR01 and SB03- to show montly varition

library(lubridate)
library(ggplot2)
library(viridis)
library(plotly)
library(dplyr)
library(scatterpie)

# SETUP parameters ####
frqs = c("DateF", "TOL_31.5", "TOL_40", "TOL_50", "TOL_63", "TOL_80", "TOL_100", "TOL_125", "TOL_160", "TOL_200", "TOL_250", "TOL_315", "TOL_400", "TOL_500", "TOL_630", "TOL_800", 
         "TOL_1000", "TOL_1250", "TOL_1600", "TOL_2000", "TOL_2500", "TOL_3150", "TOL_4000", "TOL_5000", "TOL_6300", "TOL_8000", "TOL_10000", "TOL_12500", "TOL_16000", "TOL_20000")
fQI = c( "TOL_31.5", "TOL_40", "TOL_50", "TOL_63", "TOL_80", "TOL_100", "TOL_125", "TOL_160", "TOL_200", "TOL_250", "TOL_315", "TOL_400", "TOL_500", "TOL_630", "TOL_800", 
         "TOL_1000", "TOL_1250", "TOL_1600", "TOL_2000", "TOL_2500", "TOL_3150", "TOL_4000", "TOL_5000", "TOL_6300", "TOL_8000", "TOL_10000", "TOL_12500", "TOL_16000", "TOL_20000")
FQsave = "TOL_125"

# SELECT DATA OF INTEREST ####
yor = "2019"
sites = c("SB03", "GR01")


for (ss in 1:length(sites)){
  
  site1 = sites[ss] 
  
  # DIRECTORIES ####
  tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site1, "\\")
  outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site1, "\\")
  DC = Sys.Date()
  
  # TOLs ####
  ## READ IN DATA ####
  nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE) )
  inFilesPSDF = list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE)
  inFilesPSD  = basename(inFilesPSDF)
  ## FORMATE TOL ####
  TOLmin = NULL
  for (ii in 1:(length(inFilesPSDF)) )  {
    tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
    TOLmin = rbind( TOLmin, tmpPSD)
  }
  TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
  TOLmin$Sant = site1 # TOL SOUND LEVELS
  
  ## TRUNCATE TOL ####
  TOLmin$yr =  year(TOLmin$DateF)
  TOLmin = TOLmin[ TOLmin$yr == yor,] 
  
  # VESSEL DETECTIONS ####
  ## READ IN vessel detection ####
  nFilesVD  = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
  inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
  inFilesVD = basename(inFilesVDF)
  sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
  depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)
  ## FORMATE vessel detection ####
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
  VD$yr  = year(VD$Start )
  VD$Dur = as.numeric(as.character( difftime(VD$End, VD$Start, units = "secs" )) )
  VD$Dep  = as.numeric(as.character(VD$Dep))
  VD$DepC = c(0,diff(VD$Dep))
  indx = which(VD$DepC == 1) #find transitions in deployments
  VD$DurH = VD$Dur/3600 
  VD$Dep  = as.numeric(as.character(VD$Dep))
  VD$DepC = c(0, diff(VD$Dep))
  indx = which(VD$DepC == 1) #find transitions in deployments
  VD$DurH = VD$Dur/3600 
  ## TRUNCATE VD ####
  VD = VD[ VD$yr == yor,] 
  
  # AIS data ####
  AIStran = read.csv("F:\\RESEARCH\\SanctSound\\data2\\OC02\\smp_transit_data.csv")
  AIStranOC = AIStran[ AIStran$loc_id == site1,]
  AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
  AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
  AIStranOC$yr = year(AIStranOC$Start)
  ## TRUNCATE VD ####
  AIStranOC = AIStranOC[ AIStranOC$yr == yor,] 
  rm(tmpPSD,AIStran, tmp)
  
  # MATCH VD with TOL minute data ####
  TOLmin$VD = 0
  for (ii in 1:nrow(VD) ){
    
    # find all TOLs rows that fall with the detection period
    idx =  which( TOLmin$DateF >= VD$Start[ii] &  TOLmin$DateF+60 < VD$End[ii])
    #  Check : length(idx) -  (VD$End[ii] - VD$Start[ii])
    # label those rows with vessel detection number- should not be overlap!
    TOLmin$VD[idx] = ii
    
  }
  # check: max(TOLmin$VD) - nrow(VD)
  
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
  
  TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD > 0 ] =    "A. Vessel- AIS nearby" 
  TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD > 0 ] =  "B. Vessel- unk"
  TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD == 0] =    "D. Ambient"
  TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD == 0] =    "C. Ambient- AIS nearby"
 
  TOLmin$Label[TOLmin$VD > 0 ] =    "A. Vessel Detection" 
  TOLmin$Label[TOLmin$VD == 0 ] =    "B. Ambient"

  ## summary plot ####
  TOLmin$mth = month(TOLmin$DateF)
  TOLmin2 = TOLmin[TOLmin$mth == 9,] #only plot first month
  #TOLmin2 = TOLmin[1:400,] #only plot first month
  p = ggplot(TOLmin2, aes(x=DateF, y=Category, fill=(TOL_125)) ) +
    geom_tile() +
    scale_fill_distiller(palette = "YlGnBu") +
    theme_bw() + 
    scale_y_discrete(limits=rev)+ 
    xlab("")+  ylab("")+ 
    scale_color_gradientn(colours = viridis(20))+
    theme(  axis.text.y = element_text(size = 14, colour="black"),
            axis.text.x=element_text(size = 14, colour="black"),
            plot.caption = element_text(size = 14) )
  
  ggplotly(p) 
  
  aggregate(TOLmin$TOL_125, by=list(TOLmin$Category,TOLmin$mth), median, na.rm=T)
  aggregate(TOLmin$TOL_125, by=list(TOLmin$Category), max, na.rm=T)
  aggregate(TOLmin$TOL_125, by=list(TOLmin$Label), median, na.rm=T)
  
  
  cData = as.data.frame( TOLmin %>% group_by(Category,mth) %>% tally() )
  as.data.frame( TOLmin %>% group_by(mth) %>% tally() )
  
  
  
  #pie chart of vessel dominance in soundscape
  slices = as.data.frame( TOLmin %>% group_by(mth, Category) %>% tally() )
  slices2= as.data.frame( slices %>%
    group_by(mth) %>%
    mutate(percent = n/sum(n)) )
  

  
  ggplot(data=slices2, aes(x=as.factor(mth), y=percent, group=Category,fill=Category) )+
    geom_bar(stat = "identity")

  

}