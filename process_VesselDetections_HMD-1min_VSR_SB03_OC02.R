# process sanctuary sound vessel detection data and AIS results

# reads in output of process_VesselDetections_HMD-1min_VSR_SB03.R
# OUTPUTS VD with label to read into plotting
# INPUT: plot_VesselStory_manuscript_fig5-6,R (update this for VSR plots)


rm(list=ls())

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
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

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# SET UP PARAMS ####
DC = Sys.Date()
topDir =  "F:\\SanctSound\\"
inDirW = (  "F:\\SanctSound\\analysis\\ERDAP_wind" ) # WIND data
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction" ) # HMD
outDir = "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction"
dirTop = "F:\\SanctSound" # ?? create loop through all manta directories ??
site = "OC02"
tDir   = paste0(topDir, site, "\\")
fQI = "TOL_125_PSD" # TOL_125_SPL

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# MANTA HMD DATA ####
## NOTE: THESE ARE OUTPUT OF 1_HMDDets_DayOut_SB03.R
inFiles = list.files( inDir, pattern = "VSRdata", full.names = T)
inFiles = inFiles[grepl(site, inFiles)] #remove 1 day files
load(inFiles)
head(VSRdata)
if (site == "OC02") {
  VSRdata$Slow = "No"
  VSRdata$Dy = as.Date( VSRdata$dateTime )
  VSRdata$Slow[ VSRdata$Dy >= as.Date("2020-07-01") & VSRdata$Dy <= as.Date("2020-10-31") ] = "Slowdown"
  VSRdata$Slow[ VSRdata$Dy >= as.Date("2021-07-01") & VSRdata$Dy <= as.Date("2021-10-31") ] = "Slowdown"
  VSRdata$Slow[ VSRdata$Dy >= as.Date("2022-07-01") & VSRdata$Dy <= as.Date("2022-10-31") ] = "Slowdown"
}

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+ 
# AIS DATA ####
aisf = list.files(path = paste0(dirTop, "\\analysis\\AIS\\AIS_transits"), pattern = "transit", full.names = T,recursive = F)
AIStranOC = NULL
for (aa in 1:length(aisf)) {
  tmp = read.csv(aisf [aa] )
  tmp = tmp[ tmp$loc_id == site,]
  tmp$Start = as.POSIXct( gsub("[+]00", "", tmp$start_time_utc), tz = "GMT" ) 
  tmp$End   = as.POSIXct( gsub("[+]00", "", tmp$end_time_utc), tz = "GMT" ) 
  AIStranOC = rbind(AIStranOC,tmp)
}
head(AIStranOC)
idx = which( duplicated(AIStranOC) )

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# VESSEL DETECTION DATA ####
inFilesVDF  = ( list.files(path=topDir, pattern ="*hips.csv", full.names=TRUE, recursive = TRUE) )
inFilesVDF = inFilesVDF[grepl(site, inFilesVDF)] 
inFilesVDF = inFilesVDF[!grepl("analysis", inFilesVDF)] 
inFilesVDF = inFilesVDF[!grepl("Ships", inFilesVDF)] 
inFilesVD = basename(inFilesVDF)
nFilesVD = length(inFilesVD)
sant = sapply(strsplit(inFilesVD[1], "_"), "[[", 2)
depl = unique( sapply(strsplit(inFilesVD, "_"), "[[", 3) )
VD=NULL
for (ii in 2:(nFilesVD)) {
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
VDmean = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr), mean, na.rm=T) 
VDsd   = aggregate(VD$DurH,    by=list(VD$Mth, VD$Yr), sd, na.rm=T) 
VDagg  = cbind(VDmean, VDsd$x)

## summary plot ####
ggplot(VD, aes(x=as.factor(Mth), y=(DurH), fill = as.factor(Yr), color = as.factor(Yr) )  )+
  geom_boxplot() +
  ggtitle("Durations of Vessel Detections")+
  theme_minimal()

## non-vessel periods ####
#fill in time between detections to get non-vessel conditions- AIS, wind, HMD
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
head(VDall)

VDall$Label = gsub("Ship", "ship", (VDall$Label))

t = aggregate(VDall$DurS, by=list(VDall$Label), sum, na.rm=T)

(t$x[1] / (t$x[2] +  t$x[1] ) )*100 # %time no ships
(t$x[2] / (t$x[2] +  t$x[1] ) )*100 # %time ships

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# MATCH VDs  ####
outputVD = NULL
for (ii in 1 : nrow(VDall) ){ # takes way too long!! not sure how to speed up?

  #HMDs for the duration of the detection period
  tmp = VSRdata[ VSRdata$dateTime >= VDall$Start [ii] & VSRdata$dateTime <= VDall$End [ii] ,]
  
  if ( nrow(tmp) > 0) {   
    #HMD summary
    tmpMax   = max(tmp[, fQI])     # apply(tmp[, fQI], 2 , max) 
    tmpQuant = quantile(tmp[,fQI]) # as.data.frame( apply(tmp[,fQI],2, quantile) )
    tmpMed   = tmpQuant[3]
    
    # AIS summary
    mAIS = sum(tmp$AIS)/nrow(tmp)   # total AIS transits-- need to update this!!
    mAISl = sum(tmp$AISl)/nrow(tmp)  # minutes with AISL
    mSOG =  mean(as.numeric( as.character(tmp$AISsogs)), rm.na = T) # AVG speed
    
    # WIND summary
    wind_speed = mean(tmp$wind_speed, rm.na = T)
    sea_water_temperature = mean(tmp$sea_water_temperature, rm.na = T)
    sea_surface_wave_significant_height = mean(tmp$sea_surface_wave_significant_height, rm.na = T)
    
    nMins = nrow(tmp)
    
    tmpo = cbind(tmpMax,tmpMed, mAIS,mAISl,mSOG, wind_speed, sea_water_temperature, sea_surface_wave_significant_height, nMins)
    colnames(tmpo) = c("TOL_125 max", "TOL_125 median","MinsAIS","MinsAISl","AvgSOG","wind_speed", "sea_water_temperature","sea_surface_wave_significant_height", "Total_mins" )
    tmpo = cbind(VDall[ii,], tmpo)
    outputVD = rbind(outputVD,tmpo)
    
  } else  { # not sound levels- which would be weird!
    tmpo = cbind( NA,NA, NA,NA,NA, NA,NA,NA, NA)
    colnames(tmpo) = c("TOL_125 max", "TOL_125 median","MinsAIS","MinsAISl","AvgSOG","wind_speed", "sea_water_temperature","sea_surface_wave_significant_height", "Total_mins" )
    tmpo = cbind(VDall[ii,], tmpo)
    outputVD = rbind(outputVD, tmpo)
  } 
}

head(outputVD)

#remove no HMD data
outputVD = outputVD[!is.na( outputVD$`TOL_125 max` ),]
outputVD = subset(outputVD, select = -c(sea_surface_wave_significant_height,sea_surface_wave_significant_height) )

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# MATCH AIS ####
outputVD$AIS = 0
outputVD$mDist = NA
for (ii in 1:nrow(outputVD) ){
  #just getting if an AIS vessel start of transit occurred in this vessel detection period
  tmp = AIStranOC[AIStranOC$Start >= outputVD$Start[ii] & AIStranOC$Start <= outputVD$End[ii], ] 
  tmp = tmp[ tmp$loc_id == outputVD$Sanctuary[ii], ] # only matching site
  
  if (  nrow(tmp) > 0 ){
    
    outputVD$AIS[ii]   = nrow(tmp)
    outputVD$mDist[ii] = min(tmp$dist_nm, na.rm = T)
  }
}
head(outputVD)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# LABEL ####

unique(outputVD$Label)
outputVD$Category[outputVD$AIS > 0  & outputVD$Label == "ship"]    = "A. AIS vessel noise" # AIS VESSEL PRESENT and VD
outputVD$Category[outputVD$AIS == 0 & outputVD$Label == "ship"]    = "B. non-AIS vessel noise"
outputVD$Category[outputVD$AIS == 0 & outputVD$Label == "ambient"] = "C. no vessel noise"
outputVD$Category[outputVD$AIS > 0  & outputVD$Label == "ambient"] = "D. distant AIS"

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# STATISTICS ####
aggregate(outputVD$`TOL_125 max`, by=list(outputVD$Category), mean, na.rm=T)
tal = as.data.frame( outputVD %>% group_by(Category) %>% tally() )
aggregate(outputVD$`TOL_125 max`, by=list(outputVD$Label), mean, na.rm=T)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# PLOT TIME LINE OF VD CONDITIONS ####
ggplot(outputVD, aes(x=Start, xend=End, y=Category, yend=Category, color=`TOL_125 max`)) +
  geom_segment()+
  theme_bw()+ 
  scale_y_discrete(limits=rev)+ 
  geom_segment(size=12) +
  xlab("")+  ylab("")+ ggtitle("Summary of Vessel Noise Occurance") +
  labs(caption = (paste0("samples in each category: A=", tal$n[1]," | B=", tal$n[2]," | C=", tal$n[3]," | D=", tal$n[4] )))+
  scale_color_gradientn(colours = viridis(10))+
  theme(  axis.text.y = element_text(size = 14, colour="black"),
          axis.text.x=element_text(size = 14, colour="black"),
          plot.caption = element_text(size = 14) )

rm(tal, tmp,tmpMed,tmpo, tmpQuant, VDall)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# SAVE R.data ####
save(outputVD,  file = paste0(outDir, "\\outputVD_" ,site, "_", DC, ".Rda") )

outputSave = outputVD

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# CALCULATE NOISE EXCEEDENCE ####
# for each VD with AIS
outputVD = outputSave
output = outputVD[ outputVD$Category !=  "D. distant AIS" ,] #remove AIS vessels without detections
outputVD = output[ output$Category == "A. AIS vessel noise" | output$Category == "B. non-AIS vessel noise",]
outputAB = output[ output$Category == "C. no vessel noise",]

outputVD$SNR = NA
outputVD$SNRmax = NA
outputVD$SNRtime = NA
outputVD = outputVD[complete.cases(outputVD[1]), ]

for (ii in 1:nrow(outputVD) ) {
  
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD$Start[ii] - outputAB$Start))
  
  signalMed = outputVD$`TOL_125 median`[ii]
  noiseMed  = outputAB$`TOL_125 median`[idx]
  outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD$SNRtime[ii] = as.numeric( difftime(outputVD$Start[ii], outputAB$Start[idx], units = "mins") )
  
  signalMed = outputVD$`TOL_125 max`[ii]
  noiseMed  = outputAB$`TOL_125 max`[idx]
  outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVD = outputVD[ outputVD$SNRtime < 1440 | outputVD$SNRtime >-1440, ] #remove more than day difference for samples

## summary plot ####
ggplot(outputVD, aes(x=Category, y=SNRmax )  )+
  geom_boxplot() +
  xlab("") + ylab("") +
  ylim(c(-30,40))+
  ggtitle("AIS data label: difference in dB (125 Hz 1/3 octave band)")+
  theme_minimal()

## summaries by category ####
inputData = outputVD
inputData$Day = as.Date(inputData$Start)
inputData$Mins = inputData$DurS/(60)
head(inputData)

if (site == "OC02") {
  inputData$Slowdown = "No"
  inputData$Dy = as.Date( inputData$Start )
  inputData$Slowdown[ inputData$Dy >= as.Date("2020-07-01") & inputData$Dy <= as.Date("2020-10-31") ] = "Slowdown"
  inputData$Slowdown[ inputData$Dy >= as.Date("2021-07-01") & inputData$Dy <= as.Date("2021-10-31") ] = "Slowdown"
  inputData$Slowdown[ inputData$Dy >= as.Date("2022-07-01") & inputData$Dy <= as.Date("2022-10-31") ] = "Slowdown"
} else if( (site == "OC02")){
  inputData$Slowdown = "NO"
  inputData$Slowdown[inputData$Mth == 2 |inputData$Mth == 3]  = "Slowdown"
}
unique(inputData$Slowdown)
head(inputData)

inputData %>%
  group_by(Yr, Mth) %>% 
  summarise(count = n(), Mean=mean(SNRmax, na.rm = T))

#used this for Table 1 in VSR paper
inputData %>%
  group_by(Slowdown, Yr, Category)%>% 
  summarise(count = n(),MedianMax=median(SNRmax) ) #, MedianSNR=median(SNR), Std=sd(SNR))

## PLOTs ####
# 6 panel: SNR max during slowdown vs not slowdown for AIS and non AIS noise across years
inplt = inputData[ inputData$Category =="A. AIS vessel noise" ,]
ggplot(inplt, aes(SNRmax, color = as.factor( Slowdown )  ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(noise exceedance)") +  xlab(paste0("Noise Exceedance (125 Hz third-octave band)") ) +
  theme_bw() +
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )
VExcee_mu  = aggregate(inplt$SNRmax, by=list(inplt$Slowdown, inplt$Yr), median, na.rm=T)
VExceed_se = aggregate(inplt$SNRmax, by=list(inplt$Slowdown, inplt$Yr), std.error, na.rm=T)
VD_n = as.data.frame( inplt %>% group_by(inplt$Slowdown, Yr) %>% tally() )

inplt = inputData[ inputData$Category =="B. non-AIS vessel noise" ,]
ggplot(inplt, aes(SNRmax, color = as.factor( Slowdown )  ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(noise exceedance)") +  xlab(paste0("Noise Exceedance (125 Hz third-octave band)") ) +
  theme_bw() +
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )

inplt = inputData
ggplot(inplt, aes(SNRmax, color = as.factor( Slowdown )  ) ) +
  stat_ecdf(geom="step", size = 2) +
  facet_wrap(~Yr) +
  scale_color_manual(values = c('gray','black')) +
  ylab("f(noise exceedance)") +  xlab(paste0("Noise Exceedance (125 Hz third-octave band)") ) +
  theme_bw() +
  theme(  text = element_text(size = 20,colour="black") , legend.position="bottom", legend.title = element_blank() )
VExcee_mu  = aggregate(inplt$SNRmax, by=list(inplt$Slowdown, inplt$Yr), median, na.rm=T)
VExceed_se = aggregate(inplt$SNRmax, by=list(inplt$Slowdown, inplt$Yr), std.error, na.rm=T)
VD_n = as.data.frame( inplt %>% group_by(inplt$Slowdown, Yr) %>% tally() )

inplt = inputData[ inputData$Category =="A. AIS vessel noise" ,]
VExcee_mu  = aggregate(inplt$SNRmax, by=list(inplt$Slowdown, inplt$Yr), median, na.rm=T)
VExceed_se = aggregate(inplt$SNRmax, by=list(inplt$Slowdown, inplt$Yr), std.error, na.rm=T)
VD_n = as.data.frame( inplt %>% group_by(inplt$Slowdown, Yr) %>% tally() )

# SAVE R.data ####
save(outputVD,  file = paste0(outDir, "\\outputVDne_" ,site, "_", DC, ".Rda") )

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# VESSEL NOISE Dominance ####
# NOT UPDATED ####
# 
# # METRIC: mean and se of daily percent of time vessel noise
# #total possible minutes sampled per day, per site
# TOLmin$Day = as.Date(TOLmin$DateF)
# daySamples = as.data.frame( TOLmin %>%  group_by(Day) %>%  tally() )
# daySamples$yr = year(daySamples$Day)
# daySamples = daySamples[daySamples$yr == yor,]
# 
# #total vessel minutes per day (can be over total time if last detection is into next day)
# VDomina = aggregate(inputData$Mins, by=list(inputData$Day), sum, na.rm=T)
# colnames(VDomina) = c("Day","Vessel_mins")
# VDomina = merge(daySamples, VDomina, by = c("Day"), all = TRUE )
# VDomina$PerDay = (VDomina$Vessel_mins/VDomina$n)*100
# VDomina$PerDay[is.na(VDomina$PerDay)] <- 0
# VDomina$Mth = month(VDomina$Day)
# 
# ## summary plot ####
# ggplot(VDomina, aes(x = Mth, PerDay )  )+
#   geom_point() +
#   xlab("") +
#   ylab("")+
#   ggtitle("Vessel Noise per day")+
#   theme_minimal()
# 
# VDominaMth_mu = aggregate(VDomina$PerDay,   by=list(VDomina$Mth), mean, na.rm=T)
# VDominaMth_se = aggregate(VDomina$PerDay,   by=list(VDomina$Mth), std.error, na.rm=T)
# VDomina = merge( VDominaMth_mu, VDominaMth_se, by = "Group.1", all.y = FALSE)
# colnames(VDomina ) = c("Mth","PerDay_mean","PerDay_se")
# 
# VP = merge(Vexceed, VDomina, by = "Mth")
# 
# ## summary plot ####
# ggplot(data=VP, aes( x = SNRmax_mean, y = PerDay_mean, color = as.factor( Mth ))  ) +
#   geom_point(size = VP$n/10) +
#   geom_errorbar(aes(xmin=SNRmax_mean-SNRmax_se, xmax=SNRmax_mean+SNRmax_se), position=position_dodge(.5), width=.2, alpha = .2) +
#   geom_errorbar(aes(ymin=PerDay_mean-PerDay_se, ymax=PerDay_mean+PerDay_se), position=position_dodge(.5), width=.2, alpha = .2) +
#   geom_text(data = VP, aes(x=SNRmax_mean, y=PerDay_mean, label = Mth ), vjust = 0, nudge_y = 0.5, size = 6) +
#   theme_minimal() +
#   xlim(c(-8,10))+ ylim(c(0,80))+
#   labs( x = "", y = "", title = "", caption = "", subtitle = "") +
#   theme( legend.position = "none", axis.text.y = element_text(size = 14, colour="black"),
#          axis.text.x=element_text(size = 14, colour="black"))
# 
# ggplot(data=VP, aes( x = as.factor(Mth) , y = PerDay_mean ) ) +
#   #geom_bar(stat= "identity" )
#   geom_point(size = range01(VP$SNRmax_mean)*10 )+
#   geom_errorbar(aes(ymin=PerDay_mean-PerDay_se, ymax=PerDay_mean+PerDay_se), position=position_dodge(.5), width=.2, alpha = .2) +
#   theme_minimal() +
#   labs( x = "", y = "", title = "", caption = "", subtitle = "") +
#   theme( legend.position = "none", axis.text.y = element_text(size = 14, colour="black"),
#          axis.text.x=element_text(size = 14, colour="black"))
# 
# 
# bySite = rbind(bySite,VP)
# 
# #normalize the number of vessel detections metrics
# bySite$nN = range01(bySite$n)
# 
# # SAVE OUT FILE ####
# 
# 
# 
