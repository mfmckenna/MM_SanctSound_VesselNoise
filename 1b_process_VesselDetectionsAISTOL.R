# process sanctuary sound vessel detection data and AIS results

#developed for vessel metric analysis and story map
#updated for Vessel Manuscript to include TOL data, instead of OL
rm(list=ls())


# MAIN functions-- by site processing ####
# Reads in both vessel detection and AIS data products
# Calculates daily vessel metrics per day:
# Converts vessel detection files: for each each hour, how many unique vessels detected, and percent of hour dominated by vessel noise
# some of the time will spill into the next hour, need to keep track of this
# reformat so for each hour there is a total vessel time value and total vessel count

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
library(plotly)  #https://www.r-graph-gallery.com/interactive-charts.html


# SETUP parameters ####
ra = 7 #days for running average
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
yearflag = 2020
#range of dates for output graphics
eDatePlot = '2021-12-31'
sDatePlot = '2018-11-01'

# OUTPUT details ####
tDir = "F:\\RESEARCH\\SanctSound\\"
outDir =  paste0(tDir,"data2\\combineFiles_VesselManuscript\\")
DC = Sys.Date()


#some analysis and output flags #### 
flagCSV  = FALSE   #true if you want to output hourly and daily csv files per site
flagPLT  = FALSE   #true if you want to output summary plots of data
flagYear = TRUE   #true writes out a csv of yearly data to be copied into google docs


# READ IN vessel detection data ####
# note: some deployments do not have vessel detection and therefor no files
dirVD     = (paste0(tDir,"data2\\SanctSound_VesselDetection_DataProducts") )
nFilesVD  = length( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD =     ( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVD)
x = strsplit(inFilesVD,"_")
sitesVD = unique(sapply( x, "[", 2 ))

cat(nFilesVD, " files: ", length(sitesVD), " number of sites with vessel detection data", "\n")
# as.data.frame(sitesVD) # list of sites to process
rm(x)


# READS IN AIS data (!!UPDATE!!) ####
# NOTE: update Fpattern as new data become available)
dirAIS      = paste0(tDir,"data")
Fpattern = "_2018_10_to_2020_11.csv" # "_2018_10_to_2021_04.csv"
nFilesAIS   = length( list.files(path=dirAIS,pattern = Fpattern, full.names=TRUE, recursive = TRUE))
inFilesAIS  =       ( list.files(path=dirAIS,pattern = Fpattern, full.names=TRUE, recursive = TRUE))
x = strsplit(inFilesAIS,"_")
sanctsAIS = unique(sapply( x, "[", 2 ))
#combine AIS files
multmerge = function(path){
  filenames = list.files(path=path, pattern = Fpattern, full.names=TRUE, recursive = TRUE)
  rbindlist(lapply(filenames, fread))
}
AIS <- multmerge(dirAIS)
sitesAIS = unique(AIS$LOC_ID)
cat(length(sitesAIS), " number of sites with AIS data", "\n")
rm(x)

#re-name sites
#AIS$LOC_ID[AIS$LOC_ID=="PM08"] = "PHRB"
sitesAIS = unique(AIS$LOC_ID)

# PROCESS and COMBINE FILES by site ####
# reads in SPL date when processing each site
output  = NULL
output2 = NULL #truncate to a give time period
output3 = NULL #summary of data in a specific time period, entire year 2019
output4 = NULL #monthly results for 2019 data
#need to remove columns not of interest in combined analysis
icols = c("TOL_25","TOL_31.5","TOL_40","TOL_50","TOL_63","TOL_80","TOL_100",
          "TOL_125","TOL_160","TOL_200","TOL_315","TOL_400","TOL_500",
          "TOL_630","TOL_800","TOL_1000","TOL_1250","TOL_1600","TOL_2000", 
          "deploy", "DateF", "Day")  #frequencies of interest- TOL
chFq  = "TOL_31.5"
outFq = "TOL_125"
# NOTE: multiple lines in loop with "TOL" used to find data-- so would need to update

for (ss in 1:length(sitesVD)  ) { # ss = 12
  
  cat("Processing site...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
  
  sFiles = list.files(path=dirVD, pattern = paste0(sitesVD[ss],".*hips.csv"), full.names=TRUE, recursive = TRUE)
  aData  = AIS[ AIS$LOC_ID == sitesVD[ss], ]
  aData$Day = as.Date(aData$DATE,format = "%m/%d/%Y") #AIS is daily resolution
  deply = sitesVD[ss]
  sanct = substr(sitesVD[ss],1,2)
  
  ## GET SPL files  ####
  # this give us accurate date range for detections
  dirSPL  = paste0(tDir,"data\\",sanct,"\\", deply)
  nFiles  = length( list.files(path=dirSPL, pattern = "_TOL_1h.csv", full.names=TRUE, recursive = TRUE) )
  
  ## CHECK: are there TOL SPL files ####
  # skips site if no SPL data
  if (nFiles == 0 ){
    cat("No SPL files for ", sitesVD[ss], "\n")
    #UPDATE FOR SITE WITH NO SPL DATA!!
    output = rbind(output, c(sitesVD[ss], length(sFiles), nFiles, 
                             NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA,NA,NA ) )
    next
  }else { 
    cat(nFiles, "SPL files for ",sitesVD[ss],"\n") }
  
  #CALCULATE AND EXPORT HOURLY summaries ####
  SPL = NULL
  #!! TOL fix ####
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_TOL_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_TOL_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    
    
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname   # add deployment
    if (ff > 1) { names(tmp) <- NULL }
    
    SPL = rbind(SPL,tmp)
    rm(fname,fnameF,tmp)
  }
  
  ## CHECK: SPL data headings ####
  SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
  SPL$Day   = as.Date(SPL$DateF)
  if ( is.na( SPL$DateF [1]) ){ 
    SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$yyyy_mm_ddTHH_MM_SSZ)), tz = "GMT" )
    SPL$Day   = as.Date(SPL$DateF)
  }
 
 if (length (which( colnames(SPL) == "TOL_31_5")) > 0) {  
   fx = which( colnames(SPL) == "TOL_31_5" )
   colnames(SPL)[fx] =  "TOL_31.5"  }
 
  ## CHECK: sites have different TOL bands ####
  #!! TOL fix ####
  SPL = select(SPL, c(colnames(SPL)[1], icols) )
  
  ## APPEND BB data to TOL ####
  nFiles = length( list.files(path=dirSPL,pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE))
  SPLBB = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_BB_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_BB_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname
    if (ff > 1) { names(tmp) <- NULL }
    
    SPLBB = rbind(SPLBB,tmp)
    rm(fname,fnameF,tmp)
  }
  
  colcheck = colnames(SPLBB)  #GRRR DIFFERENT HEADING FORMATS!!!
  if (colcheck[1] == "yyyy_mm_ddTHH_MM_SSZ" ){
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy_mm_ddTHH_MM_SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
  }else if (colcheck[1] == "yyyy-mm-ddTHH:MM:SSZ" ){
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPLBB$DateF)
  }else { (cat ('BB heading format not found',"\n")) }
  
  #only keep specific columns before combining
  SPLBB$BB_range = sapply( strsplit(colnames(SPLBB)[2], "_") , "[",2) 
  SPLBB = SPLBB[,c(4,2,6)]
  colnames(SPLBB) = c("DateF","BB","BB_Range")
  SPLBB =  SPLBB[!duplicated(SPLBB$DateF),]
  
  ## CHECK: duplicated rows in the SPL data ####
  duOL = nrow(SPL) - length( unique(SPL$DateF) )
  duBB = nrow(SPLBB) - length( unique(SPLBB$DateF) )
  SPL =  SPL[!duplicated(SPL$DateF),]
  
  ## MERGE BB witl TOL ####
  SPLa = merge(SPL, SPLBB, by = "DateF" ) 
  SPL = SPLa
  # unique(SPL$Day)
  
  ## CEATE accurate HOUR ranges for data ####
  beginTime =   as.POSIXct( min(SPL$DateF) )
  endTime   =   as.POSIXct( max(SPL$DateF) )
  VESSfor   =   as.data.frame( seq(from=beginTime, to=endTime, by="hour") )
  colnames(VESSfor) = "DateF"
  FullListTimes = merge(VESSfor, SPL, by="DateF", all = TRUE)
  FullListTimes$Site = sitesVD[ss]
  FullListTimes$Day = as.Date(FullListTimes$DateF)
  
  ## CREATE accurate DAY ranges for data ####
  beginDay = as.Date(beginTime)
  endDay   = as.Date(endTime)
  VESSday  = as.data.frame( seq(from=beginTime, to=endTime, by="day") ) 
  colnames(VESSday) = "Day"
  VESSday$Day = as.Date(VESSday$Day)
  
  ## PROCESS SPL data daily median ####
  uday = unique(SPL$Day)
  iBB = which( colnames(SPL)=="BB" )
  gg = c(as.numeric(grep("^TOL", colnames(SPL) ) ), iBB) 
  dSPL = NULL
  for (ii in 1:length(uday)){ # ii = 1
    dtmp = SPL[SPL$Day == uday[ii],]
    hrsample = nrow(dtmp)
    depl = dtmp$deploy[1]
    
    #SPL-- median per octave band
    tmpSPL = select(dtmp,c(gg))
    SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    dSPL = rbind(dSPL, c(depl, as.character(uday[ii]), hrsample, SPLm, dtmp$BB_Range[1]) )
    
    rm(dtmp,hrsample,depl,tmpSPL,SPLm)
  }
  
  idx = grep("^TOL", icols)
  colnames(dSPL) = c("Deployment","Day", "HourSampled", icols[idx], "BB", "BB_Range")
  dSPL = as.data.frame(dSPL)
  dSPL$Day = as.Date(dSPL$Day )
  
  ## MERGE with full day list ####
  FullListDay = merge(VESSday, dSPL, by="Day", all = TRUE)
  FullListDay$Site = sitesVD[ss]
  # ggplot(FullListDay, aes(Day, as.numeric(as.character(TOL_31.5)), color = deply))+ geom_point()
  
  rm(dname, VESSday,VESSfor,SPL,dSPL,beginDay,beginTime,deply,endDay,endTime,gg,ii,ff,sanct)
  
  ## PROCESS VESSEL DETECTION DATA TO daily metrics ####
  VESS = NULL
  for (ff in 1:length(sFiles)){
    fname  = sFiles[ff]
    dname  = sapply (strsplit( basename(fname), "_" ),"[",3 ) #add deployment
    
    tmp1a  = fread(fname,header = FALSE)
    if (nrow(tmp1a) == 1){ #some files with no detections- no rows!
      next
      
    }else {
      tmp1   = fread(fname,header = FALSE, skip=1) 
      #some weird files... with empty rows CI02_04
      tmp    = cbind(tmp1[,1:3],dname)
      VESS   = rbind(VESS,tmp) }
    
    rm(fname,dname,tmp)
  }
  ## CHECK THERE VESSEL DETECTIONS ####
  # some sites have no detections
  
  ## VESSEL DETECTION ####
  if ( VESS$V3[1] != "NoShip" ) {
    
    ### FORMAT data ####
    VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V1)), tz = "GMT" )
    VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V2)), tz = "GMT" )
    VESS$Dur_mins = VESS$DateFe  - VESS$DateFs #in minutes!
    VESS = VESS[,3:7]
    names(VESS)[1] = "label"
    names(VESS)[2] = "deply"
    VESS$DateFstart = force_tz(as.Date(VESS$DateFs), tzone="GMT")
    VESS$HrS =hour(VESS$DateFs)
    VESS$HrE =hour(VESS$DateFe)
    
    ### CHECK plot/ CLEAN up VESS ####
    #pVes = ggplot(VESS, aes(DateFs, Dur_mins))+
    #geom_point() +
    #xlab("")+
    #ylab("Duration Vessel Detections (mins)") +
    #theme_minimal()+
    #ggtitle(paste( "Check vessel detections (", sitesVD[ss], ")" ))
    VESSa = arrange(VESS, by_group = VESS$DateFs )
    VESS  = VESSa
    VESSd = dplyr::distinct(VESS)
    VESS  = VESSd
    rm(VESSa,VESSd)
    
    
    ### PROCESS each vessel detection ####
    # vessel dominant periods- loop through detections and summarize by hour
    # put in correct column in FullListTimes (HOURLY)
    # MAKE new matrix to fill in the values for each hour
    #new columns to fill in data....
    
    ### HOURLY vessel noise presence metrics ####
    FullListTimes$TotalVesselDet   = 0 #count of vessel dominated periods
    FullListTimes$TimeVesselDet    = 0 #total time vessel noise dominates
    FullListTimes$checkVD          = "noDetection"
    
    for (vv in  1 : nrow(VESS) ){
      
      tmp = VESS[vv,] #temp matrix
      tmp$DateFend = as.Date(tmp$DateFe) # needed end data in case it goes to the next day!
      
      # find column in with FullListTimes matching day/hour
      idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == FullListTimes$DateF) 
      
      if ( length(idx) > 0) { # some detections will extend to end of deployment
        # how many hours does the vessel detection dominate?
        stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
        edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
        hrsSpan =  as.numeric( difftime(edH , stH,units = "hours") )
        
        if (hrsSpan == 1 ) #spans to next hour
        {
          #add info to start hour
          FullListTimes$TotalVesselDet[idx]  = FullListTimes$TotalVesselDet[idx] + 1 # vessel count
          FullListTimes$TimeVesselDet[idx]   = FullListTimes$TimeVesselDet[idx] + difftime(edH, tmp$DateFs, units = "secs") #time in seconds difftime(edH, tmp$DateFs, units = "mins")
          FullListTimes$checkVD[idx] = tmp$deply
          
          if (is.na( FullListTimes$TotalVesselDet[idx+1] ) ) {
            #add info to next hour-- sometimes not available SPL data because last hours of period, so do a check
          }else {
            FullListTimes$TotalVesselDet[idx+1]  = FullListTimes$TotalVesselDet[idx+1] + 1 # vessel count
            FullListTimes$TimeVesselDet[idx+1]   = FullListTimes$TimeVesselDet[idx+1] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds  difftime(tmp$DateFe, edH, units = "mins")
            FullListTimes$checkVD[idx+1] = tmp$deply
          }
          
        }else if (hrsSpan == 0 ){ #detection within single hour
          
          FullListTimes$TotalVesselDet[idx]  = FullListTimes$TotalVesselDet[idx] + 1 # vessel count
          FullListTimes$TimeVesselDet[idx]   = FullListTimes$TimeVesselDet[idx]  + difftime(tmp$DateFe,tmp$DateFs,  units = "secs") #time in seconds
          FullListTimes$checkVD[idx]         = tmp$deply
          
        }else if (hrsSpan >= 2 ) { #detection spans two or more hours
          
          midHr = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS+1,":00:00"), tz = "GMT" )
          #add info to start hour
          FullListTimes$TotalVesselDet[idx]  = FullListTimes$TotalVesselDet[idx] + 1 # vessel count
          FullListTimes$TimeVesselDet[idx]   = FullListTimes$TimeVesselDet[idx] + difftime(midHr, tmp$DateFs, units = "secs")
          FullListTimes$checkVD[idx] = tmp$deply
          #add info to middle hours--- need to loop this though each hour
          for(hh in 1:((hrsSpan)-1) ) {
            FullListTimes$TotalVesselDet[idx+hh]  = FullListTimes$TotalVesselDet[idx+hh] + 1 # vessel count
            FullListTimes$TimeVesselDet[idx+hh]   = FullListTimes$TimeVesselDet[idx+hh] + 3600 #time in seconds, full hour
            FullListTimes$checkVD[idx+hh] = tmp$deply
          }
          #add info to last hour
          FullListTimes$TotalVesselDet[idx+hrsSpan]  = FullListTimes$TotalVesselDet[idx+hrsSpan] + 1 # vessel count
          FullListTimes$TimeVesselDet[idx+hrsSpan]   = FullListTimes$TimeVesselDet[idx+hrsSpan] + difftime(tmp$DateFe, edH, units = "secs") #time in seconds
          FullListTimes$checkVD[idx+hrsSpan] = tmp$deply
        }
        
        # cat("Hours for detection ", vv, ": ", hrsSpan, "hrs", "\n")
        
      }
      
    }
    
    ### HOULRY non-vessel presence metrics (quiet periods) ####
    FullListTimes$QuietDur            = 0 #average duration of quiet periods within an hour- time between detections
    FullListTimes$QuietTotal          = 0 #sum total time vessel-free
    FullListTimes$QuietDurSD          = 0 #standard deviation of quiet duration in a hour
    FullListTimes$QuietPeriods        = 0 #number of quiet periods within an hour-  time between detections
    FullListTimes$QuietTotalProp      = 0 #Total quiet time in hour (seconds)/(60*60), aka Proportion of hour vessel free
    FullListTimes$QuietTotalPeriods   = 0 #Proportion of hour vessel free/count of events (higher numbers indicate more continuous vessel-free events, 
    #lower number indicate less continuous events)
    
    # Quiet periods within each hour
    for ( hh in 1:nrow(FullListTimes) ) {
      tmpHr = FullListTimes[hh,]
      if (tmpHr$TotalVesselDet > 0 ) { #vessels are present!!
        
        #find matching detections in that hour, some are from previous hour!!!
        idxDet  = which( VESS$DateFs >= tmpHr$DateF & VESS$DateFs < tmpHr$DateF + (60*60) ) 
        if (length(idxDet) == 0 ){ #no detections but vessel present from previous hour
          FullListTimes$QuietDur[hh]     = (60*60) - tmpHr$TimeVesselDet 
          FullListTimes$QuietTotal[hh]   = (60*60) - tmpHr$TimeVesselDet
          FullListTimes$QuietDurSD[hh]   = 0 
          FullListTimes$QuietPeriods[hh] = 1 
          FullListTimes$QuietTotalProp[hh]   = ((60*60) - tmpHr$TimeVesselDet)/(60 *60)
          FullListTimes$QuietTotalPeriods[hh]   = FullListTimes$QuietTotalProp[hh] / FullListTimes$QuietPeriods[hh]
          
        } else {
          
          tmpDets = VESS[idxDet,]
          tmpDurQ = NULL
          
          #if detection exist in the hour, calculate duration between detection
          if (nrow(tmpDets) > 1 ){
            for (tt in 1:nrow(tmpDets)-1 ){
              tmpDurQ = c( tmpDurQ, difftime(tmpDets$DateFs[tt+1] , tmpDets$DateFe[tt], units = "sec") )
            }
            # calculate duration of last detection to next hour, but only if last detection is same hour, otherwise no quiet period at end of hour!!
            if (tmpDets$HrS[nrow(tmpDets)] == tmpDets$HrE[nrow(tmpDets)]) {
              tmpDurQ = c( tmpDurQ, difftime( tmpHr$DateF + (60*60), tmpDets$DateFe[nrow(tmpDets)],  units = "sec"))
            }
            # calculate duration of first detection to top of hour, but only if no detection from previous hour
            if ( tmpHr$TotalVesselDet > nrow(tmpDets) ){
              tmpDetsPre = VESS[idxDet[1]-1,] #previous detection
              tmpDurQ = c( tmpDurQ, difftime( tmpDets$DateFs[1],  tmpDetsPre$DateFe[ nrow(tmpDetsPre)], units = "secs") )
            }else {
              # calculate duration of first detection to top of hour
              tmpDurQ = c( tmpDurQ, difftime(tmpDets$DateFs[1], tmpHr$DateF,  units = "sec"))
            }
            
            #summarize values
            FullListTimes$QuietDur[hh]     = mean(tmpDurQ,na.rm = T) 
            FullListTimes$QuietTotal[hh]   = sum(tmpDurQ,na.rm = T)   
            FullListTimes$QuietDurSD[hh]   = sd(tmpDurQ,na.rm = T)  
            FullListTimes$QuietPeriods[hh] = length(tmpDurQ)  
            FullListTimes$QuietTotalProp[hh]   = sum(tmpDurQ,na.rm = T)/(60 *60)
            FullListTimes$QuietTotalPeriods[hh]   = FullListTimes$QuietTotalProp[hh] / FullListTimes$QuietPeriods[hh]
            
            #CHECK:  (60*60) - (FullListTimes$TimeVesselDet[1] + FullListTimes$QuietTotal[1])
            
          } else if (nrow(tmpDets) == 1 ) { #only one detection for the hour
            
            # calculate duration of last detection to next hour, only when ends in same hour
            if(tmpDets$HrS[nrow(tmpDets)] == tmpDets$HrE[nrow(tmpDets)]) {
              tmpDurQ = c( tmpDurQ, difftime( tmpHr$DateF + (60*60), tmpDets$DateFe[nrow(tmpDets)],  units = "secs") )
            }
            
            # check to see if detection at the beginning of the hour from previous hour
            if ( tmpHr$TotalVesselDet > nrow(tmpDets) ){
              tmpDetsPre = VESS[idxDet-1,] #previous detection
              tmpDurQ = c( tmpDurQ, difftime( tmpDets$DateFs,  tmpDetsPre$DateFe, units = "secs") )
            }else {# calculate duration from top of hour start to first detection to 
              tmpDurQ = c( tmpDurQ, difftime(tmpDets$DateFs, tmpHr$DateF,  units = "secs") )  
            }
            
            
            
            
            FullListTimes$QuietDur[hh]     = mean(tmpDurQ,na.rm = T) 
            FullListTimes$QuietTotal[hh]   = sum(tmpDurQ,na.rm = T)
            FullListTimes$QuietDurSD[hh]   = sd(tmpDurQ,na.rm = T) 
            FullListTimes$QuietPeriods[hh] = length(tmpDurQ) 
            FullListTimes$QuietTotalProp[hh]   = sum(tmpDurQ,na.rm = T)/(60 *60)
            FullListTimes$QuietTotalPeriods[hh]   = FullListTimes$QuietTotalProp[hh] / FullListTimes$QuietPeriods[hh]
          }
        }
        
      } else { #no vessels are present, best case!
        FullListTimes$QuietDur[hh]          = (60 *60) # full hour quiet in seconds
        FullListTimes$QuietTotal[hh]        = (60 *60) 
        FullListTimes$QuietDurSD[hh]        = 0       # one period of all quiet
        FullListTimes$QuietPeriods[hh]      = 1       # one period of all quiet 
        FullListTimes$QuietTotalProp[hh]    = (60 *60) / (60 *60)
        FullListTimes$QuietTotalPeriods[hh] = FullListTimes$QuietTotalProp[hh] / FullListTimes$QuietPeriods[hh] #==1
        
      }
    }
    
    #CHECK: (60*60)- (FullListTimes$TimeVesselDet[1] + FullListTimes$QuietTotal[1])
    FullListTimes$CkTime = (60*60)- (FullListTimes$TimeVesselDet + FullListTimes$QuietTotal)
    
    ### FILL IN NAs for missing data based on SPLs ####
    # not sure why some NAs within deployments... length?
    iFQ = which(colnames(FullListTimes) == chFq)
    indxNA = which (is.na (FullListTimes[, iFQ] ) )
    ix = which(colnames(FullListTimes)=="TotalVesselDet")
    FullListTimes[indxNA, ix:ncol(FullListTimes)] = NA
    
    ### FORMATE HOURLY ####
    FullListTimes$TimeVesselDetM = FullListTimes$TimeVesselDet/(60) 
    FullListTimes$JulianDay = yday(FullListTimes$DateF)
    FullListTimes$Site   = sitesVD[ss]
    FullListTimes$Day    = as.Date(FullListTimes$DateF)
    
    #remove some columns before saving... not needed
    FullListTimes2 = FullListTimes[,  which(names(FullListTimes) != c("checkVD") ) ]
    FullListTimes2 = FullListTimes2[, which(names(FullListTimes2) != c("totalTimeM") ) ]
    
    ix = which(colnames(FullListTimes2)=="deploy") 
    colnames(FullListTimes2)[ix]  = "Deployment"
    ix = which(colnames(FullListTimes2)=="DateF") 
    colnames(FullListTimes2)[ix]  = "DateTime"
    FullListTimes2$VesselTotalProp = FullListTimes2$TimeVesselDet /(60*60)
    
    
  } else { 
    
    ## NO VESSLE DETECTIONS ####
    # cat("NO VESSEL DETECTIONS SO FILL IN ALL hourly values")
    
    #FORMAT data
    VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V1)), tz = "GMT" )
    VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V2)), tz = "GMT" )
    VESS$Dur_mins = VESS$DateFe  - VESS$DateFs #in minutes!
    VESS = VESS[,3:7]
    names(VESS)[1] = "label"
    names(VESS)[2] = "deply"
    VESS$DateFstart = force_tz(as.Date(VESS$DateFs), tzone="GMT")
    VESS$HrS =hour(VESS$DateFs)
    VESS$HrE =hour(VESS$DateFe)
    
    VESSa = arrange(VESS, by_group = VESS$DateFs )
    VESS  = VESSa
    VESSd = dplyr::distinct(VESS)
    VESS  = VESSd
    rm(VESSa,VESSd)
    
    #HOURLY vessel noise presence metrics
    FullListTimes$TotalVesselDet   = 0 #count of vessel dominated periods
    FullListTimes$TimeVesselDet    = 0 #total time vessel noise dominates
    FullListTimes$checkVD          = "noDetection"
    #HOURLY quiet metrics
    FullListTimes$QuietDur            = 60 #average duration of quiet periods within an hour- time between detections
    FullListTimes$QuietTotal          = 60*60 #sum total time vessel-seconds in hour
    FullListTimes$QuietDurSD          = 0 #standard deviation of quiet duration in a hour
    FullListTimes$QuietPeriods        = 1 #number of quiet periods within an hour-  time between detections
    FullListTimes$QuietTotalProp      = 1 #Total quiet time in hour (seconds)/(60*60), aka Proportion of hour vessel free
    FullListTimes$QuietTotalPeriods   = 1 #Proportion of hour vessel free/count of events 
    #HOURLy-other metrics
    FullListTimes$TimeVesselDetM = FullListTimes$TimeVesselDet/(60) 
    FullListTimes$JulianDay = yday(FullListTimes$DateF)
    FullListTimes$Site   = sitesVD[ss]
    FullListTimes$Day    = as.Date(FullListTimes$DateF)
    
    #remove some columns before saving... not needed
    FullListTimes2 = FullListTimes[,  which(names(FullListTimes) != c("checkVD") ) ]
    FullListTimes2 = FullListTimes2[, which(names(FullListTimes2) != c("totalTimeM") ) ]
    ix = which(colnames(FullListTimes2)=="deploy") 
    colnames(FullListTimes2)[ix]  = "Deployment"
    ix = which(colnames(FullListTimes2)=="DateF") 
    colnames(FullListTimes2)[ix]  = "DateTime"
    FullListTimes2$VesselTotalProp = FullListTimes2$TimeVesselDet /(60*60)
  }
  
  #unique(as.Date(VESS$DateFs))
  
  ## EXPORT HOURLY vessel detections ####
  ## FullListTimes2 ####
  fnameOut =  paste0(outDir, sitesVD[ss], "_TOLVesselDetections_HR_",as.character(min(as.Date(FullListTimes$DateF))),"to",
                     as.character(max(as.Date(FullListTimes$DateF))), "_ver", DC, ".csv")  
  if(flagCSV == TRUE ){ write.csv(FullListTimes2,fnameOut) }
  
  #CALCULATE AND EXPORT DAILY summaries ####
  
  ## TOTAL VESSEL DOMINATE PERIODS ####
  #calculate total vessels by summing detections in each day- 
  #to avoid vessels daily detections not double counted in occur in multiple hours
  dys = as.Date( unique(FullListDay$Day) )
  FullListDay$TotalVesselDet_cnt = 0
  
  if (VESS$label[1] != "NoShip") { 
    for ( dd in 1:length(dys) ) {
      tmp = VESS[VESS$DateFstart == dys[dd],] #dd = 10
      idx = which( dys[dd]  == FullListDay$Day)
      tmp$DayE = as.Date(tmp$DateFe)
      
      if (nrow(tmp) == 0 ){ #no data on that day...
        FullListDay$TotalVesselDet_cnt[idx] = 0
      }else {
        tmpc = nrow(tmp)
        
        #does the last one go to next day?
        hrsSpan = as.numeric(tmp$DayE[tmpc] - tmp$DateFstart[tmpc] )
        
        if (hrsSpan == 1 )#spans to next day
        {
          #add info to start day
          FullListDay$TotalVesselDet_cnt[idx]   = FullListDay$TotalVesselDet_cnt[idx] + tmpc    # vessel count
          #add detection to to next day
          FullListDay$TotalVesselDet_cnt[idx+1] = FullListDay$TotalVesselDet_cnt[idx+1] + 1     # vessel count
          
        } else { #within single day
          FullListDay$TotalVesselDet_cnt[idx]   = FullListDay$TotalVesselDet_cnt[idx] + tmpc     # vessel count
        }
      }
      
    }
  }
  
  ixF = which (colnames(FullListDay ) == chFq )
  indxNAd = which( is.na(FullListDay[,ixF] ) )
  FullListDay$TotalVesselDet_cnt[indxNAd] = NA  
  
  ## TOTAL TIME METRICS ####
  #Alternative method for calculating daily values from hourly summaries-- accurate for time metrics, NOT total vessels
  DVestimeSum   = aggregate(FullListTimes$TimeVesselDet,    by=list(FullListTimes$Day), sum, na.rm=T)     #total time for the day with vessels
  DHRSsampled   = FullListTimes %>% count(Day) # NEEDED A WAY TO DIVIDE BY ACTUAL HOURS MONITORED IN THAT DAY!!
  DtimeMean     = aggregate(FullListTimes$TimeVesselDet,    by=list(FullListTimes$Day), mean, na.rm=T)    #average seconds per hour for the day
  DvessMean     = aggregate(FullListTimes$TotalVesselDet,   by=list(FullListTimes$Day), mean,na.rm=T)     #average vessels per hour
  DVessTotalProp       = DVestimeSum$x/ (DHRSsampled$n * (60*60)  )         #Proportion of day with vessel noise, divide by seconds in a day
  DQuietDursMean        = aggregate(FullListTimes$QuietDur,       by=list(FullListTimes$Day), mean,na.rm=T) #average duration of quiet (average hour then average day)
  DQuietTimeSum         = aggregate(FullListTimes$QuietTotal,     by=list(FullListTimes$Day), sum,na.rm=T)  #total quiet time on given day-- seconds
  DQuietPeriodsTotal    = aggregate(FullListTimes$QuietPeriods,   by=list(FullListTimes$Day), sum,na.rm=T)  #total quiet periods in a day-- 
  DQuietTotalPropMean   = aggregate(FullListTimes$QuietTotalProp, by=list(FullListTimes$Day), mean,na.rm=T) #mean prop hour quiet on given day 1= all quiet
  DQuietTotalProp       = DQuietTimeSum$x/ (DHRSsampled$n * (60*60)  )#Total quiet time in day (seconds)/(60*60), aka Proportion of daily vessel free
  DQuietWeightPeriods   = DQuietTotalProp/( DQuietPeriodsTotal$x/DHRSsampled$n) #  perviou version: DQuietTotalProp/DQuietPeriodsTotal$x # !! THIS DOES NOT WORK FOR NO VESSELS.... because all 
  #Proportion of hour vessel free/count of events (higher numbers indicate more continuous vessel-free events,
  VESSforDay = as.data.frame( cbind((as.character(DVestimeSum$Group.1)), DVestimeSum[,2], DtimeMean[,2], DvessMean[,2], DVessTotalProp*100,
                                    DQuietTimeSum[,2], DQuietDursMean[,2], DQuietPeriodsTotal[,2], DQuietTotalPropMean[,2]*100, DQuietTotalProp*100 ,DQuietWeightPeriods  ) )
  colnames(VESSforDay) = c("Day", "TimeVesselDet_sum","VesselDetDur_meanHourly","VesselDet_meanHourly_cnt", "PropVessel_daily",
                           "QuietDur_meanHourly",     "TimeQuiet_sum", "TotalQuietPeriods_cnt", "PercentQuiet_meanHourly", "PropQuiet_daily","WeightQuietPeriods_daily")
  VESSforDay$Day = as.Date( VESSforDay$Day )
  # head(VESSforDay)
  FullListDay2 = merge(FullListDay, VESSforDay, by = "Day")
  
  FullListDay2$PercentVessel_daily  = ( as.numeric(as.character(FullListDay2$TimeVesselDet_sum)) / (as.numeric(as.character(DHRSsampled$n)) *60 *60) ) *100
  FullListDay2$PercentQuiet_daily   = ( as.numeric(as.character(FullListDay2$TimeQuiet_sum    )) / (as.numeric(as.character(DHRSsampled$n)) *60 *60) ) *100
  # head(FullListDay2)
  FullListDay2$PercentQuiet_daily2 = 100 - as.numeric( as.character(FullListDay2$PercentVessel_daily ))
  
  ## COMBINE DAILY VD WITH AIS ####
  #so truncates AIS data in most cases
  cData = merge(FullListDay2,aData,by="Day")
  cols = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_NA_OPHRS")
  cData$LOA_ALL_OPHRS = rowSums(cData[,cols])
  cols = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_NA_UV")
  cData$LOA_ALL_UV    = rowSums(cData[,cols])
  
  #cData  = cData[, c(1:28, 32:41)] #columns to sum AIS results
  cDatat = cData #keep format for plotting, with old naming
  
  ## EXPORT DAILY FILES ####
  fnameOut =  paste0(outDir, sitesVD[ss], "_SPLVesselDetectionsAIS_Day_",as.character(min(as.Date(cData$Day))),"to",
                     as.character(max(as.Date(cData$Day))), "_ver", DC, ".csv")  
  
  if(flagCSV == TRUE ){ write.csv(cData,fnameOut)} 
  
  #as.data.frame( colnames( cData) )
  
  # PLOT of DAILY CONDITIONS ####
  #  monthly running average
  ixF = which (colnames(cDatat ) == chFq )
  cDatatNA =  cDatat[!is.na(cDatat[ixF]) , ] # this messes up AIS plotting!
  
  # !!! NEED TO UPDATE Variable Names ####
  # ## Values #### 
  # #A1) daily Vessel detections
  # p1 = ggplot(cDatatNA,aes(Day,TotalVesselDet_cnt, color = Deployment) ) +
  #   geom_point(alpha = .2)+   geom_line(aes(y=rollmean(TotalVesselDet_cnt, ra, na.pad=TRUE)),size=1) +
  #   xlab("")+     ylab("Daily vessel detections") + theme_minimal()+
  #   ylim(c(0, round(max(cDatatNA$TotalVesselDet_cnt, na.rm = TRUE)) ) ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  #   theme(legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # 
  # #B1) daily AIS vessels by type
  # dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_ALL_UV" ))
  # p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
  #   geom_point(alpha = .2)+   geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
  #   ylab("Daily unique vessels")+     xlab("") +     theme_minimal() + 
  #   ylim(c(0, round(max(cDatat$TotalVesselDet_cnt, na.rm = TRUE)) ) ) +
  #   scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  #   theme( legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # 
  # #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  # 
  # p2 = ggplot(cDatatNA,aes(Day,PerDayVess, color = Deployment) ) +
  #   geom_point(alpha = .2)+  geom_line(aes(y=rollmean(PerDayVess, ra, na.pad=TRUE)),size=1) +
  #   xlab("")+   ylab("% day dominated by vessel noise") + theme_minimal() +
  #   ylim(c(0,   round(max(cDatatNA$PerDayVess, na.rm = TRUE)) ) ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
  #   theme(legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0))) 
  # 
  # #B2) total operational hours near site
  # dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_ALL_OPHRS" ))  
  # dataAISm$Perday = ( (dataAISm$value) / 24)*100 #calcualte percent of day ships operating, can be > 100%
  # p5 = ggplot(dataAISm, aes(x=Day, y=Perday, color=factor(variable)) ) +
  #   geom_point(alpha = .2)+ geom_line(aes(y=rollmean(Perday, ra, na.pad=TRUE)),size=1) +
  #   ylab("% of day vessels operating")+     xlab("") +     theme_minimal() + 
  #   scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot)))+
  #   theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # 
  # pALL2 = grid.arrange(p1,p3,p2,p5,nrow=2,ncol=2,top = (paste0( "Daily vessel metrics (", sitesVD[ss], ")" )))
  # 
  # p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
  #   geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=.7) +
  #   ylab("Daily operational hours")+     xlab("") +     theme_minimal() + 
  #   scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
  #   theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # 
  # 
  # if(flagPLT == TRUE ){ ggsave( paste0(outDir,sitesVD[ss], "_DailyVesselMetrics_", "_ver",DC,".png") ,pALL2)}
  # 
  # 
  # ## Normalizing values #### 
  # # scale 0-1
  # #A1) Total vessels
  # cDatat$totalVesselsS = stdize(cDatat$TotalVesselDet_cnt,na.rm=T)
  # p1 = ggplot(cDatat,aes(Day,totalVesselsS,color = Deployment) ) +
  #   geom_point(alpha = .2)+  geom_line(aes(y=rollmean(totalVesselsS, ra, na.pad=TRUE)),size=1) +
  #   annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$TotalVesselDet_cnt,na.rm = T)) ),fontface="bold" ) +
  #   xlab("")+     ylab("scaled daily vessel detections") + theme_minimal()+
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
  #   theme(legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # #B1) daily AIS vessels by type
  # cDatat$LOA_S_UVS = stdize(cDatat$LOA_S_UV,na.rm=T)
  # cDatat$LOA_M_UVS = stdize(cDatat$LOA_M_UV,na.rm=T)
  # cDatat$LOA_L_UVS = stdize(cDatat$LOA_L_UV,na.rm=T)
  # cDatat$LOA_ALL_UVS = stdize(cDatat$LOA_ALL_UV,na.rm=T)
  # dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UVS","LOA_M_UVS","LOA_L_UVS","LOA_ALL_UVS" ))
  # p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
  #   geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
  #   annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", round(max(cDatat$LOA_ALL_UV,na.rm = T))),fontface="bold" ) +
  #   ylab("scaled daily unique vessels")+     xlab("") +   theme_minimal() + 
  #   scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
  #   theme( legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  # cDatat$PerDayS = stdize(cDatat$PerDayVess,na.rm=T)
  # p2 = ggplot(cDatat,aes(Day,PerDayS,color = Deployment) ) +
  #   geom_point(alpha = .2)+ geom_line(aes(y=rollmean(PerDayS, ra, na.pad=TRUE)),size=1) +
  #   annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$PerDayVess,na.rm = T)) ,"%"),fontface="bold" ) +
  #   xlab("")+   ylab("scaled % day dominated by vessel noise") + theme_minimal() +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
  #   theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # #B2) total operational hours near site AND percent of day
  # #how do I scale percent of the day???
  # cDatat$LOA_S_OPHRSS   = stdize( ((cDatat$LOA_S_OPHRS/24)*100)   ,na.rm=T)
  # cDatat$LOA_M_OPHRSS   = stdize( ((cDatat$LOA_M_OPHRS/24)*100)   ,na.rm=T)
  # cDatat$LOA_L_OPHRSS   = stdize( ((cDatat$LOA_L_OPHRS/24)*100)   ,na.rm=T)
  # cDatat$LOA_ALL_OPHRSS = stdize( ((cDatat$LOA_ALL_OPHRS/24)*100) ,na.rm=T)
  # dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRSS","LOA_M_OPHRSS","LOA_L_OPHRSS","LOA_ALL_OPHRSS" ))
  # mx = round(max(((cDatat$LOA_ALL_OPHRS/24)*100),na.rm = T))
  # #which.max(cDatat$LOA_ALL_OPHRS)
  # p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
  #   geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
  #   annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", mx ,"%"), fontface="bold" ) +
  #   ylab("scaled % of day vessels operating")+     xlab("") +     theme_minimal() + 
  #   scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
  #   scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
  #   theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  # 
  # pALLS = grid.arrange(p1,p3,p2,p4,nrow=2,ncol=2,top = (paste0( "scaled Daily vessel metrics (", sitesVD[ss], ")" )))
  # 
  # if(flagPLT == TRUE ){ ggsave( paste0(outDir,sitesVD[ss], "_DailyVesselMetricsScaled_", "_ver",DC,".png"), pALLS) }
  
  
  # MONTHLY SUMMARY of metrics by site ####
  
  #all data summary (output)
  # removed this analysis because summarized over all the data so not very useful
  #truncated time period, 2019 (output2)
  # removed this analysis because summarized over all the data so not very useful
  
  #truncated time period, year of interest and by month (output4)
  #monthly summaries in time period of interest from the daily values!!!
  
  yoi = yearflag
  cDatatKeep = cData
  cData$YR   = year(cData$Day)
  cData      = cData[cData$YR == yoi,]
  
  # cData = cDatatKeep
  
  if ( nrow(cData) != 0 ) {
    
    cData$Mth = month(cData$Day)
    
    # ALL-  TOL median values ####
    idx  = c(grep("^TOL", colnames( cData) ),grep("BB$", colnames(  cData) ) )
    cData[idx]  =  lapply(cData[idx], function(x) as.numeric(as.character(x)))
    cDataMedian  = aggregate(cData[,idx], by=list(cData$Mth), median, na.rm=T)
    colnames(cDataMedian) = paste0(colnames(cDataMedian),"_median")
    colnames(cDataMedian)[1] = "Mth"
    
    # ALL- TOL mean values ####
    cDataMean  = aggregate(cData[idx], by=list(cData$Mth), mean, na.rm=T)
    colnames(cDataMean) = paste0(colnames(cDataMean),"_mean")
    colnames(cDataMean)[1] = "Mth"
    
    cDataM1 = merge(cDataMedian, cDataMean, by = "Mth" , all =T)
    
    # ALL- TOL SD  ####
    cDataMSD = aggregate(cData[idx], by=list(cData$Mth), sd, na.rm=T)
    colnames(cDataMSD) = paste0(colnames(cDataMSD),"_SD")
    colnames(cDataMSD)[1] = "Mth"
   
    cDataM1 = merge(cDataM1, cDataMSD, by = "Mth" , all =T)
   
    # ALL- AIS counts  ####
    idx2  = (grep("^LOA_.*_UV", colnames( cData) ) )
    cDataMtotalVD =  aggregate(cData[idx2], by=list(cData$Mth), sum, na.rm=T) #total for the month
    colnames(cDataMtotalVD) = gsub("UV","UV_sum", colnames(cDataMtotalVD))
    colnames(cDataMtotalVD)[1] = "Mth"
    LOA_ALL_UV_mean = aggregate(cData[idx2], by=list(cData$Mth), mean, na.rm=T) #mean for the month
    colnames(LOA_ALL_UV_mean) = gsub("UV","UV_mean", colnames(LOA_ALL_UV_mean))
    colnames(LOA_ALL_UV_mean)[1] = "Mth"
    LOA_ALL_UV_SD = aggregate(cData[idx2], by=list(cData$Mth), sd, na.rm=T) #sd for the month
    colnames(LOA_ALL_UV_SD) = gsub("UV","UV_sd", colnames(LOA_ALL_UV_SD))
    colnames(LOA_ALL_UV_SD)[1] = "Mth"
    # ALL- AIS PPHRS  ####
    idx2  = (grep("^LOA_.*_OPHRS", colnames( cData) ) )
    cData_OPHRS =  aggregate(cData[idx2], by=list(cData$Mth), sum, na.rm=T) #total for the month
    colnames(cData_OPHRS) = gsub("OPHRS","OPHRS_sum", colnames(cData_OPHRS))
    cData_OPHRS2 =  aggregate(cData[idx2], by=list(cData$Mth), mean, na.rm=T) #total for the month
    colnames(cData_OPHRS2) = gsub("OPHRS","OPHRS_mean", colnames(cData_OPHRS2))
   
    AISmrg  = cbind(cDataMtotalVD, LOA_ALL_UV_mean[,2:ncol(LOA_ALL_UV_mean)], LOA_ALL_UV_SD[,2:ncol(LOA_ALL_UV_SD)], cData_OPHRS[,2:ncol(cData_OPHRS)], cData_OPHRS2[,2:ncol(cData_OPHRS2)] ) 
   
    cDataM1 = merge(cDataM1, AISmrg, by = "Mth" , all =T)
    
    # TOTAL DAYS  ####
    c2 = cData[!is.na(cData$TOL_25), ] 
    cDataDys = c2 %>% count(Mth)
    colnames(cDataDys) = c("Mth","Days")
    
    # OUT- MONTHLY SUMMARY - Daily DATA ####
    cDataM1 = merge(cDataM1, cDataDys, by = "Mth" , all =T)
   
    #YES: calculate median values- Yes vessels ####
    SPLvesY = filter(cData, TotalVesselDet_cnt > 0)
    idx  = c(grep("^TOL", colnames( SPLvesY) ),grep("BB$", colnames(  SPLvesY) ) )
    colnames(SPLvesY)[idx]
    if (nrow(SPLvesY) == 0 ){ #ISSUE-- no days with  vessel detections
      cDataM2Y =  as.data.frame ( matrix(NA, nrow = length(cDataM1$Mth), ncol = length(idx)+1) )
      #how to name the columns... 
      colnames(cDataM2Y)= c("Mth", colnames(SPLvesY)[idx])
      colnames(cDataM2Y) = gsub("TOL","VESS_TOL_median",  colnames(cDataM2Y))
      colnames(cDataM2Y) = gsub("BB", "VESS_BB_median",   colnames(cDataM2Y))
      
      
      
    } else {
      cDataM2Y  = aggregate(SPLvesY[,idx], by=list(SPLvesY$Mth), mean, na.rm=T)
      colnames(cDataM2Y) = gsub("TOL","VESS_TOL_median",  colnames(cDataM2Y))
      colnames(cDataM2Y) = gsub("BB", "VESS_BB_median",   colnames(cDataM2Y))
      colnames(cDataM2Y)[1] = "Mth"
    }
    
    
    ## NO: calculate median values--non-vessel SPL ####
    SPLvesN   = filter(cData, TotalVesselDet_cnt == 0)
    idx  = c(grep("^TOL", colnames( SPLvesN) ), grep("BB$", colnames(  SPLvesN) ) )
    if (nrow(SPLvesN) == 0 ){ #ISSUE-- no days with  vessel detections
      cDataM2N =  as.data.frame( matrix(NA, nrow = length(cDataM1$Mth),  ncol = length(idx)+1 ) )
      #how to name the columns... 
      colnames(cDataM2N)= c("Mth", colnames(SPLvesN)[idx])
      colnames(cDataM2N) = gsub("TOL","NO_TOL_median",  colnames(cDataM2N))
      colnames(cDataM2N) = gsub("BB", "NO_BB_median",   colnames(cDataM2N))
      
    } else {
      cDataM2N  = aggregate(SPLvesN[,idx], by=list(SPLvesN$Mth), mean, na.rm=T)
      #!!! TOL ####
      colnames(cDataM2N) = gsub("TOL","NO_TOL_median", colnames(cDataM2N))
      colnames(cDataM2N) = gsub("BB", "NO_BB_median", colnames(cDataM2N))
      colnames(cDataM2N)[1] = "Mth"
    }
    
    # FORMAT OUT ####
    cDataM  = NULL
    cDataM = merge(cDataM1, cDataM2Y, by = "Mth" , all =T)
    cDataM = merge(cDataM, cDataM2N,      by = "Mth" , all =T)
    cDataM$Site = cData$Site[1]
    
    ### COUNT OF acoustic detections
    cDataMtotalVD =  aggregate(cData$TotalVesselDet_cnt, by=list(cData$Mth), sum, na.rm=T) #total for the month
    cDataMmeanVD =  aggregate(cData$TotalVesselDet_cnt, by=list(cData$Mth), mean, na.rm=T) #total for the month
    cDataMsdVD =  aggregate(cData$TotalVesselDet_cnt, by=list(cData$Mth), sd, na.rm=T) #total for the month
    VDmrg = cbind(cDataMtotalVD, cDataMmeanVD[,2:ncol(cDataMmeanVD)], cDataMsdVD[,2:ncol(cDataMsdVD)])
    colnames(VDmrg)= c("Mth","TotalVesselDet_cnt_sum","TotalVesselDet_cnt_mean","TotalVesselDet_cnt_sd")
    cDataM = merge(cDataM, VDmrg, by = "Mth" , all =T)
    
    ## HOURLY INPUT METRICS #### 
    #works better for many comparative metrics)
    #calculate a few hourly metrics from the FullListTimes
    FullListTimes$Mth = month(FullListTimes$Day)
    FullListTimes$Yr = year(FullListTimes$Day)
    hData = FullListTimes[FullListTimes$Yr == yoi,]
    #remove NAs before running
    ixF = which (colnames(hData ) == chFq )
    hData = hData[!is.na(hData[ixF]) ,]
    
    ### Hours with vessel noise ####
    hData$TotalVesselDetP = 0
    hData$TotalVesselDetP[hData$TotalVesselDet > 0] = 1 # vessel present in the hour or not, recode 0/1
    totaHrsVess = aggregate(hData$TotalVesselDetP, by=list(hData$Mth), sum, na.rm=T)  
    totalHrs    = hData %>% count(Mth)  
    ### proportion of hours with vessels detected ####
    PrpHrsVESS = totaHrsVess$x/totalHrs$n
    PrpHRQUIET = 1-PrpHrsVESS
    #sometimes it does not match up so need to use MERGE
    hData2 = cbind(totaHrsVess$Group.1, PrpHrsVESS, PrpHRQUIET ) 
    colnames(hData2) = c("Mth","PrpHRSVESS","PrpHrsQUIET")
    cDataM2 = merge(cDataM, hData2, by = "Mth" , all =T)
    
    #### Hours SPL vessel vs non vessel  ####
    ixF = which (colnames(hData ) == outFq )
    hrsNOVess  = hData[ hData$TotalVesselDetP == 0, ]
    NVessMed   = aggregate(hrsNOVess[ixF], by=list(hrsNOVess$Mth), median, na.rm=T)
    NVessSd    = aggregate(hrsNOVess[ixF], by=list(hrsNOVess$Mth), sd, na.rm=T)
    
    hrsVess   = hData[ hData$TotalVesselDetP > 0, ]
    if ( nrow(hrsVess) == 0 ) { 
      #some sites with no hours with vessel detections!!
      YVessMed = NA
      YVessSd  = NA
      
      hData3 = cbind( NVessMed$Group.1, NVessMed[2], NVessSd[2])
      colnames(hData3) = c("Mth","NVess125_med","NVess125_sd")
      hData4 = cbind( NVessMed$Group.1, YVessMed, YVessSd)
      colnames(hData4) = c("Mth","YVess125_med", "YVess125_sd")
      
      hData3  = merge(hData3, hData4, by ="Mth" , all =T )
      cDataM3 = merge(cDataM2,hData3, by = "Mth", all =T)
      
    } else{
      ixF = which (colnames(hrsVess ) == outFq )
      YVessMed = aggregate(hrsVess[ixF], by=list(hrsVess$Mth), median, na.rm=T)
      YVessSd  = aggregate(hrsVess[ixF], by=list(hrsVess$Mth), sd, na.rm=T)
      
      hData3 = cbind( NVessMed$Group.1, NVessMed[2], NVessSd[2])
      colnames(hData3) = c("Mth","NVess125_med","NVess125_sd")
      hData4 = cbind( YVessMed$Group.1, YVessMed[2], YVessSd[2])
      colnames(hData4) = c("Mth","YVess125_med", "YVess125_sd")
      
      hData3  = merge(hData3, hData4, by ="Mth"  , all =T)
      cDataM3 = merge(cDataM2,hData3, by = "Mth", all =T)
    }
    
    #COMBINE SITE RESULTS
    output4 = as.data.frame(output4, stringsAsFactors = FALSE)
    output4 = rbind (output4, cDataM3)
    
    
    rm(cDataM1, cDataM,cDataM2Y,cDataM2N, cDataDys,cDataM2, cDataM3)
  }
  
  
  # CLEAN UP
  rm(edH,pVes,pVESStime,pVESSvess, tmp, VESS, VESSfor, VESSforDay, x, 
     edB,ff,fnameOut,HrS,hrsSpan,idx,inFiles,inFilesB, vv,stH,DtimeMean,DtimeSum,DvessSum,DvessMean, HRsampled, pALL2)
  
}

#Write out files and copy to google sheets to compare results
if(flagYear == TRUE ){
  fnameOut =  paste0(outDir, "SummaryALL_", yearflag, "_ver", DC, ".csv")  
  write.csv(output2,fnameOut) 
  
  fnameOut =  paste0(outDir, "SummaryALL_ver", DC, ".csv")  
  write.csv(output,fnameOut) 
  
  fnameOut =  paste0(outDir, "SummaryMTH_", yearflag, "_ver", DC, ".csv")  
  write.csv(output4,fnameOut) 
  
}

ggplot(output4, aes(as.factor(Mth), Site, fill=as.numeric(as.character(Days)) ) ) +
  geom_tile()+
  xlab("Month in 2019")+ 
  scale_fill_gradient2(low="white", high="blue")+
  theme( legend.title = element_blank() ) +
  ggtitle(paste0("Days sampled ", yearflag))

output4$Days
