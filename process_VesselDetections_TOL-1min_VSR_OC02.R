# process SanctSound vessel detection data and AIS results

# modified 1b-- wanted sound levels only during vessel detections "noise added" 
# modified with new directory structure- site_dep
# using TOLs at 1 minute resolution

# tested for OC02 speed reduction

rm(list=ls())

# SETUP parameters ####

# LOAD Libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)
library(stringr)
library(plyr)
library(viridis)

# SETUP directories ####
site = "OC02"

output  = NULL
inDirs = list.dirs( "F:\\SanctSound\\", recursive=F)
tDir   = inDirs[grepl("OC02", inDirs)] # tol and vd data
aDir = "F:\\SanctSound\\analysis\\AIS\\AIS_transits" # AIS data
outDir = ( "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction")
DC = Sys.Date()

# SETUP parameters ####
slowdown = as.data.frame ( rbind( c("2020", ("2020-06-01"), ("2020-10-31"),"out" ), 
                  c("2021", ("2021-06-01"), ("2021-10-31"),"all" ),
                  c("2022", ("2022-06-01"), ("2022-10-31"),"all" ) ) )
colnames(slowdown) = c("Yr", "StartDate","EndDate","lanes")
slowdown$StartDate = as.Date( slowdown$StartDate)
slowdown$EndDate  = as.Date( slowdown$EndDate)
frqs = c("DateF", "TOL_31.5", "TOL_40", "TOL_50", "TOL_63", "TOL_80", "TOL_100", "TOL_125", "TOL_160", "TOL_200", "TOL_250", "TOL_315", "TOL_400", "TOL_500", "TOL_630", "TOL_800", 
         "TOL_1000", "TOL_1250", "TOL_1600", "TOL_2000", "TOL_2500", "TOL_3150", "TOL_4000", "TOL_5000", "TOL_6300", "TOL_8000", "TOL_10000", "TOL_12500", "TOL_16000", "TOL_20000")
fQI = c( "TOL_31.5", "TOL_40", "TOL_50", "TOL_63", "TOL_80", "TOL_100", "TOL_125", "TOL_160", "TOL_200", "TOL_250", "TOL_315", "TOL_400", "TOL_500", "TOL_630", "TOL_800", 
         "TOL_1000", "TOL_1250", "TOL_1600", "TOL_2000", "TOL_2500", "TOL_3150", "TOL_4000", "TOL_5000", "TOL_6300", "TOL_8000", "TOL_10000", "TOL_12500", "TOL_16000", "TOL_20000")
FQsave = "TOL_125"
# VESSEL DETECTION DATA ####
nFilesVD   = length( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVDF = ( list.files(path=tDir, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVDF)
sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
depl = sapply(strsplit(inFilesVD, "_"), "[[", 3)
VD=NULL
for (ii in 1:(nFilesVD) ) {
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
VD$Dep  = as.numeric(as.character(VD$Dep)) # unique(VD$Dep)
VD$DepC = c(0,diff(VD$Dep))
indx = which(VD$DepC == 1) # find transitions in deployments
(rm(tmp))
VD$DurH = VD$Dur/3600 
VD$Label= tolower(VD$Label)  
VD = VD[VD$Label == "ship", ]
## VD DURATIONS ####
VDmean = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr, VD$Sant), mean, na.rm=T) 
VDsd   = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr, VD$Sant), sd, na.rm=T) 
VDagg = cbind(VDmean, VDsd$x)
## NON-VESSEL PERIODS ####
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

# TOL DATA ####
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE) )
inFilesPSDF = list.files(path=tDir, pattern = "TOL_1min", full.names=TRUE, recursive = TRUE)
inFilesPSD  = basename(inFilesPSDF)
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}
TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 
TOLmin$Sant = site # TOL SOUND LEVELS
rm(tmpPSD)
TOLmin$Yr =  year(TOLmin$DateF)
TOLmin$Mth =  month(TOLmin$DateF)
## TOL SUMMARIES ####
TOLmean = aggregate(TOLmin$TOL_125, by=list(TOLmin$Mth, TOLmin$Yr), mean, na.rm=T) 

# AIS data ####
AIStran = read.csv( paste0(aDir, "\\smp_transit_data_updateFormat.csv") ) 
AIStranOC = AIStran[ AIStran$loc_id == site,]
AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
rm(AIStran)
cat("Range for AIS data: ", as.character( min( AIStranOC$Start ) ), " to ", as.character( max( AIStranOC$Start ) ) )

# TOL PROCESS---------------------------------------------------------------------------------
# TOL combine ####

## with VD label ####
TOLmin$VD = 0
for (ii in 1:nrow(VD) ){
  # find all TOLs rows that fall with the detection period
  idx =  which( TOLmin$DateF >= VD$Start[ii] &  TOLmin$DateF+60 < VD$End[ii] )
  #  Check : length(idx) -  (VD$End[ii] - VD$Start[ii])
  TOLmin$VD[idx] = ii  # label TOL rows with vessel detection number- should not be overlap!
}

## with AIS transits ####
TOLmin$AIS  = 0
TOLmin$AISs = 0
TOLmin$AISm = 0
TOLmin$AISl = 0
TOLmin$AISu = 0
TOLmin$SOG  = 0
for (ii in 1:nrow(AIStranOC) ){ 
  
  # find all TOLs rows that fall with the AIS vessel transit period
  idx =  which( TOLmin$DateF >= AIStranOC$Start[ii] &  TOLmin$DateF+60 <= AIStranOC$End[ii] )
  
  if (length(idx) > 0) {
    # cat(ii,"\n") # find transits with TOL data for trouble shooting
    
    TOLmin$AIS[idx] =  TOLmin$AIS[idx] + 1 # adds transits to minutes
    
    #adds count of different sized vessels
    if(AIStranOC$loa[ii] < 50)                           { TOLmin$AISs[idx] = TOLmin$AISs[idx] + 1  }
    if(AIStranOC$loa[ii] >= 50 & AIStranOC$loa[ii] < 100){ TOLmin$AISm[idx] = TOLmin$AISm[idx] + 1  }
    if(AIStranOC$loa[ii] >= 100)                         { TOLmin$AISl[idx] = TOLmin$AISl[idx] + 1  }
    if(is.na(AIStranOC$loa[ii]) )                        { TOLmin$AISu[idx] = TOLminu$AISu[idx] + 1  }
    
    #what is the speed of the vessel
    TOLmin$SOG[idx] = paste( TOLmin$SOG[idx], AIStranOC$avg_sog_dw[ii], sep = ",")
    
    }
}

## with speed label ####
idx =  ( which( TOLmin$AIS > 0 ) )
TOLmin$SOG[idx]    = str_replace( TOLmin$SOG[idx] , "0," , "")  #remove the zeros from the list
SOGcalc = function(x){ mean( as.numeric( strsplit( x , ",")[[1]] ) ) } # mean( as.numeric( strsplit( TOLmin$SOG [191] ,  ",")[[1]] ) )
TOLmin$SOGm   = sapply(TOLmin$SOG, SOGcalc)
idx =  ( which( TOLmin$AIS == 0 ) ) #no AIS data- so make NA
TOLmin$SOGm[idx] = NA
SOGu10  = function(x){ sum( (as.numeric( strsplit( x , ",")[[1]] ) ) < 11 ) } 
TOLmin$SOGb10 = sapply(TOLmin$SOG, SOGu10)
TOLmin$SOGb10[idx] = NA
# update results for missing AIS data, NAs for any dates after 
TOLmin$Day = as.Date ( TOLmin$DateF )
idx = TOLmin$Day > as.Date ( max( AIStranOC$End ) )
TOLmin$AIS[idx]  = NA
TOLmin$AISs[idx]  = NA
TOLmin$AISm[idx]  = NA
TOLmin$AISl[idx]  = NA
TOLmin$AISu[idx]  = NA
TOLmin$SOG[idx]  = NA
TOLmin$SOGm[idx]  = NA
TOLmin$SOGb10[idx]  = NA
TOLmin$SOGb10P[!idx] = TOLmin$SOGb10[!idx]/ TOLmin$AIS[!idx] 

# TOL PLOTS ####
# summary of minutes in each % of vessels 10 knots or below
as.data.frame( TOLmin %>% group_by(Yr, SOGb10) %>% tally() )
# count of vessels travelling <10 kts-- hard to see how many NOT within each color
# expect 0 as highest levels (all vessels above 10), NA as lowest (no vessels)
## count vessels <10 ####
ggplot(TOLmin, aes(TOL_125, color = as.factor(SOGb10) ) )  +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr)+ 
  scale_colour_discrete(name="AIS Vessels < 10 kts")+
  ylab("f(sound level)") +  xlab(paste0("Low-frequency sound levels  (125 Hz third-octave band)") ) +
  theme_minimal()

## % of vessels <10 kts ####
TOLmin$SOGb10Pr = (round_any( TOLmin$SOGb10P*100, 10) ) 
as.data.frame( TOLmin %>% group_by(Yr,SOGb10Pr) %>% tally() )
# expect 0 as highest levels (all vessels above 10), NA as lowest (no vessels), 100 lowest
ggplot(TOLmin, aes(TOL_125, color = as.factor(SOGb10Pr) ) )  +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr)+ 
  scale_colour_discrete(name="% Vessels < 10 kts")+
  ylab("f(sound level)") +  xlab(paste0("Low-frequency sound levels  (125 Hz third-octave band)") ) +
  theme_minimal()
TOLmin %>% group_by(AIS) %>%  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),  X125 = quantile(TOL_125, c(0.25, 0.5, 0.75)))

# TOL LABELS ####
## Category of vessel presence ####
TOLmin$mth = month(TOLmin$DateF)
TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD > 0 ] =    "A. Vessel- AIS nearby" 
TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD > 0 ] =    "B. Vessel- unk"
TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD == 0] =    "D. No Vessel"
TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD == 0] =    "C. No Vessel- AIS nearby"
TOLmin$Category[ is.na( TOLmin$AIS) ] =    "E. No AIS data"
## Vessel Detection Label ####
TOLmin$Label[TOLmin$VD > 0 ] =    "A. Vessel Detection" 
TOLmin$Label[TOLmin$VD == 0 ] =   "B. No Vessel Detection"
unique(TOLmin$Label )
TOLminJul = TOLmin[TOLmin$mth == 7,]
# boxplot of 125 by VD label 
ggplot(TOLminJul, aes( y=TOL_125, color=(Label))  )+
  geom_boxplot()+
  ggtitle (paste0("July: variation in sound level- ", TOLmin$Sant ) )+
  theme_minimal()+
  facet_wrap(~Yr)

## Slowdown periods ####
TOLmin$Slowdown = 0
idx = NULL
for (ss in 1: nrow(slowdown) ){
  tmp = which(TOLmin$Day >= slowdown$StartDate[ss] & TOLmin$Day <= slowdown$EndDate [ss]) 
  idx = c(idx, tmp)
}
TOLmin$Slowdown [idx] = 1 
TOLminJul = TOLmin[TOLmin$mth == 7,]
ggplot(TOLminJul, aes( y=TOL_125, color=(Slowdown))  )+
  geom_boxplot()+
  ggtitle (paste0("July: variation in sound level- ", TOLmin$Sant ) )+
  theme_minimal()+
  facet_wrap(~Yr)

# TOL SAVE OUT ####
save(TOLmin,  file = paste0(outDir, "\\outputTOL_" ,site, "_", DC, ".Rda") ) #save bc above takes forever
# load( "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction\\outputTOL_OC02_2023-06-02.Rda")

# TOL SUMMARIES ####
aggregate(TOLmin$TOL_125, by=list(TOLmin$Slowdown,TOLmin$mth, TOLmin$Yr), mean, na.rm=T)
as.data.frame( TOLmin %>% group_by(Slowdown, mth) %>% tally() )

# VESSEL DETECTIONS PROCESS---------------------------------------------------------------------------------
# VD combine #### 
## add NON-VESSEL PERIODS ####
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
## with TOLs  ####
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

## with AIS ####
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

# VD LABEL ####
outputVD$Category[outputVD$AIS > 0  & outputVD$Label == "ship"] = "A. AIS vessels" # AIS VESSEL PRESENT and VD
outputVD$Category[outputVD$AIS == 0 & outputVD$Label == "ship"] = "B. Non-AIS vessels"
outputVD$Category[outputVD$AIS == 0 & outputVD$Label == "ambient"] = "C. Non-vessel"
outputVD$Category[outputVD$AIS > 0  & outputVD$Label == "ambient"] =  "D. Nearby AIS"
# aggregate(outputVD$`TOL_125 max`, by=list(outputVD$Category), mean, na.rm=T)
tal = as.data.frame( outputVD %>% group_by(Category) %>% tally() )

# VD PLOTs  ####
# TIME LINE OF VD CONDITIONS
ggplot(outputVD, aes(x=Start, xend=End, y=Category, yend=Category, color=`TOL_125 max`)) +
  geom_segment()+
  theme_bw() + 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=12) +
  xlab("")+  ylab("")+ ggtitle("Summary of Vessel Detection Periods") +
  labs(caption = (paste0("samples in each category: A=", tal$n[1]," | B=", tal$n[2]," | C=", tal$n[3]," | D=", tal$n[4] )))+
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x=element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )

# VD SAVE out ####
save(outputVD,  file=paste0(outDir, "\\outputVD_", site , "_", DC, ".Rda") )
