# process sanctuary sound vessel detection data and AIS results

# modified 1b-- wanted sound levels only during vessel detections "noise added" 
# using TOLs at 1 minute resolution

# tested for OC02 speed reduction

rm(list=ls())

# SETUP parameters ####
#-----------------------------------------------------------------------------------------
# LOAD Libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(scales)
library(BBmisc)
library(zoo)

site = "OC02"

output  = NULL
tDir   = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
outDir = paste0( "F:\\RESEARCH\\SanctSound\\data2\\", site, "\\")
DC = Sys.Date()

slowStart     = as.Date("2021-06-01")
slowEnd       = as.Date("2021-10-31")
slowStartTrim = as.Date("2021-07-01")
slowEndTrim   = as.Date("2021-08-01")
baseStart     = as.Date("2019-07-12")
baseEnd       = as.Date("2019-08-01")

# READ IN-- vessel detection ####
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
indx = which(VD$DepC == 1) # find transitions in deployments
(rm(tmp))

# summarize vessel info for July 2021
VD = VD[ as.Date(VD$Start) >= slowStartTrim & as.Date(VD$Start) <= slowEndTrim,]

# ADD time stamps between detections ####
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

VDallt = VDall[VDall$DurS < (3600*6),]
rm(VDallt, r1, r2, VD)

# LABEL vessel detections as in or out of slow down ####
#-----------------------------------------------------------------------------------------
VDall$startDay = as.Date(VDall$Start)

VDall$Period [VDall$startDay > slowStart & VDall$startDay < slowEnd] = "slowdown"
VDall$Period [VDall$startDay > baseStart & VDall$startDay < baseEnd] = "baseline"
VDall2 = VDall[!is.na(VDall$Period), ] #remove all NA data
rm(VDall)

# READ IN-- TOLs ####
#-----------------------------------------------------------------------------------------
nFilesPSD   = length( list.files(path=tDir, pattern = "_1min", full.names=TRUE, recursive = TRUE))
inFilesPSDF = ( list.files(path=tDir, pattern = "mean_1min", full.names=TRUE, recursive = TRUE))
inFilesPSD  = basename(inFilesPSDF)
#combine all TOLs together
TOLmin = NULL
for (ii in 1:(length(inFilesPSDF)) )  {
  tmpPSD = read.csv(inFilesPSDF[ii]) #this takes a while to load...
  TOLmin = rbind( TOLmin, tmpPSD)
}
TOLmin$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", TOLmin$yyyy.mm.ddTHH.MM.SSZ)), tz = "GMT" ) 

#  ADD TOL TO DETECION PERIODS ####
#-----------------------------------------------------------------------------------------
VDall3 = VDall2
VDall3July = VDall3[VDall3$Period == "slowdown",]
(rm(tmpPSD,VDall2,VDall3))

output = NULL
# For each period- find corresponding sound levels and take the median-- takes a bit
for (ii in 1:nrow(VDall3July)){
  tmp = TOLmin[ TOLmin$DateF >= VDall3July$Start [ii] & TOLmin$DateF <= VDall3July$End [ii] ,] 
  
  tmpMax =  ( apply(tmp[,2:31],2,max) )
  tmpQuant = as.data.frame( apply(tmp[,2:31],2,quantile) )
  tmpMed = tmpQuant[3,]
  
  tmpo = cbind(tmpMax[8], tmpMed[8])
  colnames(tmpo) = c("TOL_125 max", "TOL_125 median")
  tmpo = cbind(VDall3July[ii,], tmpo)
  
  output = rbind(output,tmpo)
}
ouputSave = output 

(rm(tmp, tmpMed,tmpo, tmpQuant))

# ADD AIS data vessel detections and ECHO type labels ####
#-----------------------------------------------------------------------------------------
AIStran   = read.csv("F:\\RESEARCH\\SanctSound\\data2\\OC02\\smp_transit_data.csv") #AIS transit data
AIStranOC = AIStran[ AIStran$loc_id == "OC02",]
AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 

AISparticipants = read.csv("G:\\My Drive\\ActiveProjects\\ECHO\\2021 Jul Swiftsure Bank slowdown results.csv") #AIS transit data July 2021 only
# but can use this to get types of vessels.... and filter by time later
#unique(AISparticipants$ECHO.Vessel.Class)
#substr(AISparticipants$Local.pacific.time.to.enter.Swiftsure.Bank.slowdown.zone[1], start=5, stop = 30)
#as.POSIXct (substr(AISparticipants$Local.pacific.time.to.enter.Swiftsure.Bank.slowdown.zone[1], start=5, stop = 15),  tz ="America/Los_Angeles", "%d/%b/%Y")

AISparticipants$EnterTime = with_tz( as.POSIXct (substr(AISparticipants$Local.pacific.time.to.enter.Swiftsure.Bank.slowdown.zone, start=5, stop = 30),  
                                                 tz ="America/Los_Angeles", "%d/%b/%Y %H:%M"), tz = "GMT" )
AISparticipants$ExitTime  = with_tz( as.POSIXct (substr(AISparticipants$Local.pacific.time.to.exit.Swiftsure.Bank.slowdown.zone, start=5, stop = 30),  
                                                tz ="America/Los_Angeles", "%d/%b/%Y %H:%M"), tz = "GMT" )

AISparticipants$DurSlow = difftime( AISparticipants$ExitTime, AISparticipants$EnterTime,units = "mins")

# summarize vessel info for July 2021
AIStranOCJul = AIStranOC[ as.Date(AIStranOC$start) >= as.Date("2021/07/01") & as.Date(AIStranOC$start) <= as.Date("2021/08/01"),]
AIStranOCJul = AIStranOCJul[ AIStranOCJul$dist_nm > 8 ,]
length( unique( AIStranOCJul$mmsi ) ) #unique vessels = 537
nrow(AIStranOCJul) #transits in 10 km buffer = 1014

length( unique( AISparticipants$Vessel.MMSI) ) #unique vessels = 347
nrow(AISparticipants) # total participant transits = 410
nrow( AISparticipants[AISparticipants$Was.the.vessel.within.1kt.of.slowdown.target. == "YES",])/nrow(AISparticipants) # = 289 yes slowed down 70%
nrow( AISparticipants[AISparticipants$Was.the.vessel.within.1kt.of.slowdown.target. == "NO",])/nrow(AISparticipants) # = 73  no slowed down 20%
nrow( AISparticipants[AISparticipants$Was.the.vessel.within.1kt.of.slowdown.target. == "Unknown Vessel Type",])/nrow(AISparticipants) # = 48  unk slowed down %10

# ADD echo types to AIS transit tables ####
#-----------------------------------------------------------------------------------------
AIStranOCJul$type_echo = NA
AIStranOCJul$type_jasco = NA
AIStranOCJul$type_MT = NA
AIStranOCJul$SlowdownOut = NA #ships leaving 
AIStranOCJul$EnterTime = NA
AIStranOCJul$ExitTime = NA

for (ss in 1:nrow(AIStranOCJul)) { 
  
  tmp =  AISparticipants[ AISparticipants$Vessel.MMSI == AIStranOCJul$mmsi[ss], ]
  
  if (nrow(tmp) > 0){
    
    AIStranOCJul$type_echo[ss]  = tmp$ECHO.Vessel.Class
    AIStranOCJul$type_jasco[ss] = tmp$JASCO.Vessel.Class
    AIStranOCJul$type_MT[ss]    = tmp$Marine.Traffic.Vessel.Class
    
    #check if start of transit in 10 km falls within exit/enter slowdown zone
    AIStranOCJul$SlowdownOut[ss] = AIStranOCJul$Start[ss] > tmp$EnterTime & AIStranOCJul$Start[ss] < tmp$ExitTime
    AIStranOCJul$EnterTime[ss] = as.character( tmp$EnterTime )
    AIStranOCJul$ExitTime[ss] = as.character( tmp$ExitTime )
    
  }
  
  
}

sum( !is.na( AIStranOCJul$type_echo ) )/ nrow(AIStranOCJul)  #% transits in ECHO program- just by vessel name, not transit time!
sum( AIStranOCJul$SlowdownOut, na.rm = T )

(rm(AIStran,AIStranOC,tmp))

# ADD type of vessels to the vessel detection periods ####
#-----------------------------------------------------------------------------------------
# output = ouputSave # use this to reset b/c above takes a bit

output$AIS = 0
output$BulkCarrier = 0
output$Container = 0
output$Tanker = 0
output$CarCarrier = 0
output$Other = 0
output$Passenger = 0

output$Cargo = 0 #in case there is no match with the Echo data
output$Tanker1 = 0 #in case there is no match with the Echo data
output$Unk = 0
output$mDist = NA #if smaller than transited at edge of the 10 km buffer

for (ii in 1: nrow(output) ){
  
  #get AIS vessel transits that occurred in this vessel detection period-- can be multiple given duration of some of detection periods
  tmp = AIStranOCJul[AIStranOCJul$Start >= output$Start[ii] & AIStranOCJul$Start <= output$End[ii],] 
  
  if (  nrow(tmp) > 0 ){
    
    output$AIS[ii]          = nrow(tmp) #total AIS vessels
    
    #ECHO labels
    output$BulkCarrier[ii]  = length( which(tmp$type_echo == "Bulk Carrier") )
    output$Container[ii]    = length( which(tmp$type_echo == "Container") )
    output$Tanker[ii]       = length( which(tmp$type_echo == "Tanker") )
    output$CarCarrier[ii]   = length( which(tmp$type_echo == "Car Carrier") )
    output$Passenger[ii]    = length( which(tmp$type_echo == "Passenger") )
    output$Other[ii]        = length( which(tmp$type_echo == "Other") )
    
    #check if need to add label for NA
    if (any( is.na(tmp$type_echo)) ) {
      idx =  which ( is.na(tmp$type_echo) )
      output$Cargo[ii]        = length( which(tmp$type[idx] == "Cargo") )
      output$Tanker1[ii]      = length( which(tmp$type[idx] == "Tanker") )
      
    }
    
    output$Unk[ii]   = nrow(tmp) -  sum(output[ii,15:22]) 
    output$mDist[ii] = min(tmp$dist_nm)
  }
}

# ADD if a vessel slowed down during detection period ####
#-----------------------------------------------------------------------------------------
output$slowdownY = NA
output$slowdownN = NA
idx = NULL 
for (ii in 1: nrow(output) ){ # ii = 683 
  
  # AISparticipants$EnterTime[1]+900 # since vessel passage near the enter
  tmp = AISparticipants[AISparticipants$EnterTime >= output$Start[ii] & AISparticipants$EnterTime+900 <= output$End[ii],] 
  if (nrow(tmp) > 0) { 
    
    output$slowdownY[ii] = length( which(tmp$Was.the.vessel.within.1kt.of.slowdown.target. == "YES") )
    output$slowdownN[ii] = length( which(tmp$Was.the.vessel.within.1kt.of.slowdown.target. == "NO") )
    idx = rbind(idx, ii)
    #cat(ii,"\n" ) 
  }
}
# which detections had matches with Echo slowdown list? output[idx, ]
(rm(idx, tmp))

# Assign labels to time periods ####
#-----------------------------------------------------------------------------------------
output$Category[output$AIS > 0]         = "A. AIS vessel (unk)" # has an AIS vessel
length( which (output$Category == "A. AIS vessel (unk)") ) 

output$Category[output$AIS > 0 & output$Tanker > 0 | output$BulkCarrier > 0 & output$Container == 0 & output$CarCarrier == 0 & output$Passenger  == 0]  = "B. 11 knt (bulk,tanker)"
length( which (output$Category == "B. 11 knt (bulk,tanker)") )

output$Category[output$AIS > 0 & output$Container  > 0 | output$CarCarrier > 0 | output$Passenger  > 0 & output$BulkCarrier == 0 & output$Tanker  == 0] = "C. 14.5 knt (cruise,container,vehicle)"
length( which (output$Category == "C. 14.5 knt (cruise,container,vehicle)") )

output$Category[output$AIS == 0 & output$Label == "ship"]    = "D. Non-AIS vessel"
length( which (output$Category == "D. Non-AIS vessel") )

output$Category[output$AIS == 0 & output$Label == "ambient"] = "E. No vessel"
output$Category[output$AIS > 0  & output$Label == "ambient"] = "E. No vessel" #no vessel det, but AIS-- checked and pretty far away so consider non-vessel
length( which (output$Category == "E. No vessel") )

length( which (output$Category == "A. AIS vessel (unk)") ) 

unique(output$Category)

# PLOTS COMPARING SLOWDOWN ###
#-----------------------------------------------------------------------------------------
as.data.frame(colnames(output))
output$slowdown = "UNK"
output$slowdown[output$slowdownY >= 1] = "YES"  # label if any vessel slowed down during the detection
output$slowdown[output$slowdownY < 1 ] = "NO"   # label if any vessel slowed down during the detection  

outputJul =output
min( as.Date(outputJul$Start ) )
max( as.Date(outputJul$Start ) )

difftime(  max( (outputJul$Start ) ), min(outputJul$Start ) )

#METRIC MAX SOUND LEVELS ####
#why are participating vessels occurring in No vessel periods?
ggplot(outputJul, aes(x=Category, y=`TOL_125 max`, fill = as.factor(slowdown))  )+
  geom_boxplot() +
  labs(title = "Max sound levels in different periods (125 Hz TOL)",
       caption = paste0("A = ", sum(outputJul$Category == "A. AIS vessel (unk)") ,"\n",
                        "B = ", sum(outputJul$Category == "B. 11 knt (bulk,tanker)"),"\n",
                        "C = ", sum(outputJul$Category == "C. 14.5 knt (cruise,container,vehicle)"),"\n",
                        "D = ", sum(outputJul$Category == "D. Non-AIS vessel"),"\n",
                        "E = ", sum(outputJul$Category == "E. No vessel"),"\n" ) ) +
  guides(fill=guide_legend(title="Participation")) +
  xlab("")+  ylab("")+   theme_minimal()+   theme(axis.text.x=element_text(angle=30,hjust=1)) 

# METRIC: TIME IN EACH CATEGORY timeline
ggplot(outputJul, aes(x=Start, xend=End, y=Category, yend=Category, color=slowdown)) +
  geom_segment()+
  theme_bw()+ #use ggplot theme with black gridlines and white background
  geom_segment(size=8) +
  xlab("")+  ylab("")+ 
  guides(fill=guide_legend(title="Participation")) #increase line width of segments in the chart
library("viridis")  
ggplot(outputJul, aes(x=Start, xend=End, y=Category, yend=Category, color=`TOL_125 max`)) +
  geom_segment()+
  theme_bw()+ #use ggplot theme with black gridlines and white background
  geom_segment(size=8) +
  xlab("")+  ylab("")+ 
  scale_color_gradientn(colours = viridis(10))+
   guides(fill=guide_legend(title="Participation")) #increase line width of segments in the chart

# METRIC: Pie chart of time comparison for categories
outputBaseline = outputJul 

#Total samples
sumBaseline = c( sum(outputBaseline$Category == "A. AIS vessel (unk)"),  
                 sum(outputBaseline$Category == "B. 11 knt (bulk,tanker)"),
                 sum(outputBaseline$Category == "C. 14.5 knt (cruise,container,vehicle)"),  
                 sum(outputBaseline$Category == "D. Non-AIS vessel"),
                 sum(outputBaseline$Category == "E. No vessel")) 
#Total time
sumTIMEBaseline = c( sum (outputBaseline$DurS[outputBaseline$Category == "A. AIS vessel (unk)"] ),  
                     sum (outputBaseline$DurS[outputBaseline$Category == "B. 11 knt (bulk,tanker)"]),
                     sum (outputBaseline$DurS[outputBaseline$Category == "C. 14.5 knt (cruise,container,vehicle)"]),  
                     sum (outputBaseline$DurS[outputBaseline$Category == "D. Non-AIS vessel"]) ,
                     sum (outputBaseline$DurS[outputBaseline$Category == "E. No vessel"]) ) 

dt = data.table(X = c(1), 
                Y = c(1), 
                Uships = c(33), 
                Names = c("Slowdown (2021)"), 
                types = rbind(sumBaseline ) )

dt = as.data.frame(dt)
colnames(dt) = c("X", "Y", "UniqueShips", "Period",  "AIS (unk)", "11knt", "14knt",  "Non-AIS", "No-Vessel" )

dtTime = data.table(X = c(1), Y = c(1), Uships = c(33), Names = c("Slowdown (2021)"), 
                    types = rbind(sumTIMEBaseline ) )
dtTime = as.data.frame(dtTime)
colnames(dtTime) =c("X", "Y", "UniqueShips", "Period",  "AIS (unk)", "11knt", "14knt",  "Non-AIS", "No-Vessel" )

library(scatterpie)
ggplot() +
  geom_scatterpie( aes(x=X, y=Y, r=UniqueShips, group=Period), data=dtTime,
                   cols=c( "AIS (unk)", "11knt", "14knt",  "Non-AIS", "No-Vessel") )  + 
  coord_equal()+
  geom_text(data = dt, aes(x=X+20, y=Y-34,label = paste0("Total days = ", floor( sum(outputBaseline$DurS)/(60*60*24) )) ), size = 5)  +
  ylab("") + xlab("") + ggtitle("Durations of Acoustic Categories") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# METRIC: Sound levels- YES vs NO periods with any vessel ####
outputTrim = outputJul[ outputJul$Category != "E. No vessel" | outputJul$Category !="D. Non-AIS vessel", ]
ggplot(outputTrim, aes(`TOL_125 max`,  color = Category ) ) + 
  stat_ecdf(geom = "step") +
  xlab("")+  ylab("")+
  labs(title = "Sound levels known AIS vessel periods (125 Hz TOL)",
       caption = paste0("NO = ", sum(outputTrim$slowdown == "NO") ,"\n",
                        "UNK = ", sum(outputTrim$slowdown == "UNK"),"\n",
                        "YES = ",  sum(outputTrim$slowdown == "YES") ) ) +
  theme_minimal()


# METRIC: Noise Above nearby non-vessel period ####
outputVD = outputJul[ outputJul$Category != "E. No vessel",] # (nrow(outputVD))
outputAB = outputJul[ outputJul$Category == "E. No vessel",] # (nrow(outputAB))

outputVD$SNR = NA
outputVD$SNRmax = NA
outputVD$SNRtime = NA
for (ii in 1:nrow(outputVD)) {
  #find the closest (in time) ambient sample
  idx = which.min(abs(outputVD$Start[ii] - outputAB$Start))
  signalMed = outputVD$`TOL_125 median`[ii]
  noiseMed  = outputAB$`TOL_125 median`[idx]
  outputVD$SNR[ii]   = signalMed - noiseMed # signal - noise
  outputVD$SNRtime[ii]= as.numeric( difftime(outputAB$Start[idx], outputVD$Start[ii], units = "mins") )
  
  signalMed = outputVD$`TOL_125 max`[ii]
  noiseMed  = outputAB$`TOL_125 max`[idx]
  outputVD$SNRmax[ii]= signalMed - noiseMed # signal - noise
}

outputVD = outputVD[outputVD$SNRtime < 24*60, ] # only samples when ambient is < 24 hours away
outputVD2 = outputVD[outputVD$Category != "D. Non-AIS vessel",] #remove non-ais vessel
ggplot(outputVD2, aes(SNRmax,  color = Category ) ) + 
  stat_ecdf(geom = "step")+
  xlab("")+  ylab("")+
  labs(title = "Noise above during vessel periods (125 Hz TOL)",
       caption = paste0("NO = ", sum(outputVD2$slowdown == "NO") ,"\n",
                        "UNK = ", sum(outputVD2$slowdown == "UNK"),"\n",
                        "YES = ",  sum(outputVD2$slowdown == "YES") ) ) +
  theme_minimal()

# how does distribution change
ggplot(outputVD2, aes(SNRmax,  color = slowdown ) ) + 
  stat_ecdf(geom = "step")+
  xlab("")+  ylab("")+
  facet_wrap(~Category) +
  theme_minimal()


# START HERE ####
# METRIC: distribution of sound levels in a specific event ####
idxC = which( output$Category == "C. 14.5 knt (cruise,container,vehicle)" )[8] # just look at first one
output$AIS[idxC]
output[idxC,]

tmp = TOLmin[ TOLmin$DateF >= output$Start [idxC] & TOLmin$DateF <= output$End [idxC] ,] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
p1 = ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("AIS vessel (1-min SPLs, n=", nrow(tmp)," minutes)" ) ) +
  theme_minimal()
# Ambient distribution
idxA = which( output$Category == "E. No vessel" )[1]
tmp = TOLmin[ TOLmin$DateF >= output$Start [idxA] & TOLmin$DateF <= output$End [idxA] ,] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
p2 = ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("No vessel detection (1-min SPLs, n=", nrow(tmp)," minutes)" ) ) +
  theme_minimal()
# Ambient distribution
idxA = which( output$Category == "D. Non-AIS vessel" )[3]
tmp = TOLmin[ TOLmin$DateF >= output$Start [idxA] & TOLmin$DateF <= output$End [idxA] ,] 
tmpM = reshape2 :: melt(tmp, id.vars = "yyyy.mm.ddTHH.MM.SSZ", measure.vars =  colnames(tmp)[2:14]) 
p3 = ggplot(tmpM, aes(variable, value))+
  geom_boxplot() +
  xlab("") + ylab("")+
  ylim(c(75,130)) +
  ggtitle(paste0("Non-AIS vessel detection (1-min SPLs, n=", nrow(tmp)," minutes)" ) ) +
  theme_minimal()

grid.arrange(p1,p3,p2)


