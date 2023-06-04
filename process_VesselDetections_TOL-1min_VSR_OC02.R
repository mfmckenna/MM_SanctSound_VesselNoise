# process sanctuary sound vessel detection data and AIS results

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
## VD DURATIONS
VDmean = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr, VD$Sant), mean, na.rm=T) 
VDsd   = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr, VD$Sant), sd, na.rm=T) 
VDagg = cbind(VDmean, VDsd$x)

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


# TOL with VD label ####
TOLmin$VD = 0
for (ii in 1:nrow(VD) ){
  # find all TOLs rows that fall with the detection period
  idx =  which( TOLmin$DateF >= VD$Start[ii] &  TOLmin$DateF+60 < VD$End[ii] )
  #  Check : length(idx) -  (VD$End[ii] - VD$Start[ii])
  TOLmin$VD[idx] = ii  # label TOL rows with vessel detection number- should not be overlap!
}

# TOL with AIS transits ####
TOLmin$AIS  = 0
TOLmin$AISs = 0
TOLmin$AISm = 0
TOLmin$AISl = 0
TOLmin$AISu = 0
TOLmin$SOG  = 0
 

# each minute will have number of vessels by size group, speeds-- to get predicted TOL based on speed and vessel count
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


# HOW TO SPEED UP!!
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


save(TOLmin,  file = paste0(outDir, "\\outputTOL_" ,site, "_", DC, ".Rda") ) #save bc above takes forever


# summary of minutes in each % of vessels 10 knots or below
# !!! START HERE how to visualize #### 
as.data.frame( TOLmin %>% group_by(SOGb10, Yr) %>% tally() )

# count of vessels travelling <10 kts-- hard to see how many NOT within each color
# expect 0 as highest levels (all vessels above 10), NA as lowest (no vessels)
ggplot(TOLmin, aes(TOL_125, color = as.factor(SOGb10) ) )  +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr)+ 
  scale_colour_discrete(name="AIS Vessels < 10 kts")+
  ylab("f(sound level)") +  xlab(paste0("Low-frequency sound levels  (125 Hz third-octave band)") ) +
  theme_minimal()

# % of vessels travelling <10 kts-- hard to see how many total
library(plyr)
TOLmin$SOGb10Pr = (round_any( TOLmin$SOGb10P*100, 10) ) 
# expect 0 as highest levels (all vessels above 10), NA as lowest (no vessels), 100 lowest
ggplot(TOLmin, aes(TOL_125, color = as.factor(SOGb10Pr) ) )  +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr)+ 
  scale_colour_discrete(name="% Vessels < 10 kts")+
  ylab("f(sound level)") +  xlab(paste0("Low-frequency sound levels  (125 Hz third-octave band)") ) +
  theme_minimal()

TOLmin %>% group_by(AIS) %>%  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),  X125 = quantile(TOL_125, c(0.25, 0.5, 0.75)))


## LABELS ####
# Category of vessel presence ####
TOLmin$mth = month(TOLmin$DateF)
TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD > 0 ] =    "A. Vessel- AIS nearby" 
TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD > 0 ] =    "B. Vessel- unk"
TOLmin$Category[TOLmin$AIS == 0 & TOLmin$VD == 0] =    "D. No Vessel"
TOLmin$Category[TOLmin$AIS > 0  & TOLmin$VD == 0] =    "C. No Vessel- AIS nearby"
TOLmin$Category[ is.na( TOLmin$AIS) ] =    "E. No AIS data"
# unique(TOLmin$Category )

# Vessel Detection Label ####
TOLmin$Label[TOLmin$VD > 0 ] =    "A. Vessel Detection" 
TOLmin$Label[TOLmin$VD == 0 ] =   "B. No Vessel Detection"
unique(TOLmin$Label )

# Slowdown periods ####
TOLmin$Slowdown = 0
idx = NULL
for (ss in 1:nrow(slowdown) ){
  tmp = which(TOLmin$Day >= slowdown$StartDate[ss] & TOLmin$Day <= slowdown$EndDate [ss]) 
  idx = c(idx, tmp)
}
TOLmin$Slowdown [idx] = 1 


# SUMMARIES ####
aggregate(TOLmin$TOL_125, by=list(TOLmin$Slowdown,TOLmin$mth, TOLmin$Yr), mean, na.rm=T)
as.data.frame( TOLmin %>% group_by(Slowdown, mth) %>% tally() )



# SAVE OUT ####
save(TOLmin,  file = paste0(outDir, "\\outputTOL_" ,site, "_", DC, ".Rda") )

# !!!! VDs with TOL metrics #### 



# PLOTS ####
as.data.frame(colnames(TOLmin))
TOLminJul = TOLmin[TOLmin$mth == 7,]

# boxplot of 125 by label--- do 
ggplot(TOLminJul, aes( y=TOL_125, color=(Label))  )+
  geom_boxplot()+
  ggtitle (paste0("Variation in sound level- ", TOLmin$Sant ) )+
  theme_minimal()+
  facet_wrap(~Yr)

# Noise Exceedence for July--- this needs VD detections!!!

# ECDF for slowdown periods

TOLmin2 = TOL_SB03[1:400,] #only plot first month
p = ggplot(TOLmin2, aes(x=DateF, y=Label, fill=(TOL_125)) ) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_bw() + 
  ggtitle("Vessel Detection Label")+
  scale_y_discrete(limits=rev)+ 
  xlab("")+  ylab("")+ 
  scale_color_gradientn(colours = viridis(20))+
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x=element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )
ggplotly(p) 

