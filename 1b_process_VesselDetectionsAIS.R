# process sanctuary sound vessel detection data and AIS results

#developed for vessel metric analysis and story map
#some results copied here: https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=1545323104

#-----------------------------------------------------------------------------------------
# MAIN functions-- by site processing
#-----------------------------------------------------------------------------------------
# Reads in both vessel detection and AIS data products
# Calculates daily vessel metrics per day:
# Converts vessel detection files: for each each hour, how many unique vessels detected, and percent of hour dominated by vessel noise
# some of the time will spill into the next hour, need to keep track of this
# reformat so for each hour there is a total vessel time value and total vessel count

#outputs 
# a per site .csv file: E:\RESEARCH\SanctSound\data2\combineFiles1_SPLShips
# summary table for average of all data per site (copied here):
# https://docs.google.com/spreadsheets/d/1dw9Vy0dXKW3Q6Ter3t19cmLf_5EJWbmL9nqJsCrNXQE/edit#gid=888829761

#-----------------------------------------------------------------------------------------
# TO ADD (future versions)
#-----------------------------------------------------------------------------------------
# Done! output table: add range and save out
# Done! duration of quiet- hour/daily events and average duration s, using vessel detection data
# DONE! check for vessel detection data-- if not available need to reflect this in data or if no detections--- looks for ""NoShip" in the 1st label column
# might be error with running average plots-- only remove NAs for Vessel detections NOT AIS (~line 710)

rm(list=ls())
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
library(plotly)  #https://www.r-graph-gallery.com/interactive-charts.html

#-----------------------------------------------------------------------------------------
# SETUP parameters
#-----------------------------------------------------------------------------------------
ra = 7 #days for running average
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
yearflag = 2020
#-----------------------------------------------------------------------------------------
# OUTPUT details
#-----------------------------------------------------------------------------------------
tDir = "F:\\RESEARCH\\SanctSound\\"
outDir =  paste0(tDir,"data2\\combineFiles1_SPLShips\\")
DC = Sys.Date()

#range of dates for output graphics
eDatePlot = '2020-12-31'
sDatePlot = '2018-11-01'

#some analysis and output flags 
flagCSV  = TRUE  #true if you want to output hourly and daily csv files per site
flagPLT  = FALSE  #true if you want to output summary plots of data
flagYear = TRUE   #true writes out a csv of yearly data to be copied into google docs

#-----------------------------------------------------------------------------------------
# READ IN-- vessel detection data
# note: some deployments do not have vessel detection and therefor no files
#-----------------------------------------------------------------------------------------
dirVD     = (paste0(tDir,"data2\\SanctSound_VesselDetection_DataProducts") )
nFilesVD  = length( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD =     ( list.files(path=dirVD, pattern = "*hips.csv", full.names=TRUE, recursive = TRUE))
inFilesVD = basename(inFilesVD)
x = strsplit(inFilesVD,"_")
sitesVD = unique(sapply( x, "[", 2 ))

cat(nFilesVD, " files: ", length(sitesVD), " number of sites with vessel detection data", "\n")
rm(x)

#-----------------------------------------------------------------------------------------
# READS IN-- AIS data (update Fpattern as new data become available)
#-----------------------------------------------------------------------------------------
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
AIS$LOC_ID[AIS$LOC_ID=="PM08"] = "PHRB"
sitesAIS = unique(AIS$LOC_ID)

#-----------------------------------------------------------------------------------------
# PROCESS and COMBINE FILES by site, reads in SPL date when processing each site
#-----------------------------------------------------------------------------------------
output  = NULL
output2 = NULL #truncate to a give time period
output3 = NULL #summary of data in a specific time period, entire year 2019
output4 = NULL #monthly results for 2019 data

for (ss in 1:length(sitesVD)  ) { 
  cat("Processing site...", sitesVD[ss], ":",ss, "of", length(sitesVD),"\n")
  
  sFiles = list.files(path=dirVD, pattern = paste0(sitesVD[ss],".*hips.csv"), full.names=TRUE, recursive = TRUE)
  aData  = AIS[ AIS$LOC_ID == sitesVD[ss], ]
  aData$Day = as.Date(aData$DATE,format = "%m/%d/%Y") #AIS is daily resolution
  deply = sitesVD[ss]
  sanct = substr(sitesVD[ss],1,2)
  
  #----------------------------------------------------------------------------------------- 
  #GET SPL files-- this give us accurate date range for detections
  #---------------------------------------------------------------------------------------
  dirSPL  = paste0(tDir,"data\\",sanct,"\\", deply)
  nFiles  = length( list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE) )
  
  #CHECK: are there OL SPL files??
  if (nFiles == 0 ){
    cat("No SPL files for ", sitesVD[ss], "\n")
    #UPDATE FOR SITE WITH NO SPL DATA!!
    output = rbind(output, c(sitesVD[ss], length(sFiles), nFiles, 
                             NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA, NA,NA,NA,NA,NA,NA ) )
    next
  }else { 
    cat(nFiles, "SPL files for ",sitesVD[ss],"\n") }
  
  #COMBINE ALL OL DATA TOGETHER, add deployment
  SPL = NULL
  for (ff in 1 : nFiles){
    fname  = list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=FALSE, recursive = TRUE)[ff]
    fnameF = list.files(path=dirSPL, pattern = "_OL_1h.csv", full.names=TRUE, recursive = TRUE)[ff]
    
    
    dname  = sapply (strsplit( fname, "_" ),"[",4 )
    tmp    = rbind(fread(fnameF))
    tmp$deploy = dname
    if (ff > 1) { names(tmp) <- NULL }
    
    SPL = rbind(SPL,tmp)
    rm(fname,fnameF,tmp)
  }
  
  #CHECK: fix format changes in SPL data headings
  SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
  SPL$Day   = as.Date(SPL$DateF)
  if ( is.na( SPL$DateF [1]) ){#try another format!!
    SPL$DateF = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPL$yyyy_mm_ddTHH_MM_SSZ)), tz = "GMT" )
    SPL$Day   = as.Date(SPL$DateF)
  }
  
  #CHECK: fix some sites have different OB-- SB01 for sure!! starts at 16 Hz
  #cat( sitesVD[ss], ":", colnames(SPL), "\n")
  if (sitesVD[ss]== "SB01") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  if (sitesVD[ss]== "SB02") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  if (sitesVD[ss]== "SB03") {
    cat("SPL data has extra column(s)... check!", "\n")
    SPL = SPL[,c(1,3:15)]
  }
  cat( sitesVD[ss], ":", colnames(SPL), "\n")

  #APPEND BB data to OL
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
  
  #GRRR DIFFERENT HEADING FORMATS!!!
  colcheck = colnames(SPLBB)
  if (colcheck[1] == "yyyy_mm_ddTHH_MM_SSZ" ){
    #SPLBB$yyyy_mm_ddTHH_MM_SSZ
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy_mm_ddTHH_MM_SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
    
  }else if (colcheck[1] == "yyyy-mm-ddTHH:MM:SSZ" ){
    #SPLBB$`yyyy-mm-ddTHH:MM:SSZ`
    SPLBB$DateF     = as.POSIXct( gsub(".000Z", "", gsub("T", " ", SPLBB$`yyyy-mm-ddTHH:MM:SSZ`)), tz = "GMT" )
    SPLBB$DateFday  = as.Date(SPL$DateF)
  }else { (cat ('BB heading format not found',"\n")) }
 
  #only keep specific columns before combining
  SPLBB = SPLBB[,c(2,4)]
  
  #CHECK: duplicated rows in the SPL data!!! keep track and then remove.
  duOL = nrow(SPL) - length( unique(SPL$DateF) )
  duBB = nrow(SPLBB) - length( unique(SPLBB$DateF) )
  SPL =  SPL[!duplicated(SPL$DateF),]
  SPLBB =  SPLBB[!duplicated(SPLBB$DateF),]
  SPLa = merge(SPL, SPLBB, by = "DateF" ) 
  SPL = SPLa
  colnames(SPL)[3]  = "OL_31.5"
  colnames(SPL)[15] = "BB_20-24000"

  # Check: nrow(SPL) - length( unique(SPL$DateF) )
  
  #CREATE accurate time ranges for data-- hourly
  #---------------------------------------------------------------------------------------
  beginTime =   as.POSIXct( min(SPL$DateF) )
  endTime   =   as.POSIXct( max(SPL$DateF) )
  VESSfor   =   as.data.frame( seq(from=beginTime, to=endTime, by="hour") )
  colnames(VESSfor) = "DateF"
  FullListTimes = merge(VESSfor, SPL, by="DateF", all = TRUE)
  FullListTimes$Site = sitesVD[ss]
  FullListTimes = FullListTimes[,c(16,13,14,1,3:12,15)] #reorder and truncate!
  FullListTimes$Day = as.Date(FullListTimes$DateF)
  
  #duplicate check: nrow(FullListTimes) - length( unique(FullListTimes$DateF) )
  
  #CREATE accurate time ranges for data-- DAY
  #---------------------------------------------------------------------------------------
  beginDay = as.Date(beginTime)
  endDay   = as.Date(endTime)
  VESSday  = as.data.frame( seq(from=beginTime, to=endTime, by="day") ) 
  colnames(VESSday) = "Day"
  VESSday$Day = as.Date(VESSday$Day)
  #process SPL data to get daily median values per octave band
  uday = unique(SPL$Day)
  dSPL = NULL 
  gg = c(as.numeric(grep("^OL", colnames(SPL) ) ), 15) #add the BB column!
  for (ii in 1:length(uday)){ # ii = 1
    dtmp = SPL[SPL$Day == uday[ii],]
    hrsample = nrow(dtmp)
    depl = dtmp$deploy[1]
    #SPL-- median per octave band
    tmpSPL = select(dtmp,c(gg))
    SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    dSPL = rbind(dSPL, c(depl, as.character(uday[ii]),  hrsample, SPLm ) )
    
    rm(dtmp,hrsample,depl,tmpSPL,SPLm)
  }
  colnames(dSPL)[1:3] = c("deply","Day","Hrs")
  dSPL = as.data.frame(dSPL)
  dSPL$Day = as.Date(dSPL$Day )
  # check duplicates: nrow(dSPL) - length( unique(dSPL$Day) )
  
  #merge with full day list
  FullListDay= merge(VESSday, dSPL, by="Day", all = TRUE)
  FullListDay$Site = sitesVD[ss]
  FullListDay = FullListDay[,c(15,2,1,4:14,3)] #reorder and truncate!
  
  #ggplot(FullListDay, aes(Day, as.numeric(as.character(OL_31.5)), color = deply))+
  #geom_point()
  
  rm(dname, VESSday,VESSfor,SPL,dSPL,beginDay,beginTime,deply,endDay,endTime,gg,ii,ff,sanct)
  
  #----------------------------------------------------------------------------------------- 
  #PROCESS VESSEL DETECTION DATA TO daily metrics-- COMBINE deployments
  #-----------------------------------------------------------------------------------------
  VESS=NULL
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
  
  #ARE THERE VESSEL DETECTIONS??
  if (VESS$V3[1] != "NoShip") {
    
    #FORMAT data
    #-----------------------------------------------------------------------------------------
    VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V1)), tz = "GMT" )
    VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V2)), tz = "GMT" )
    VESS$Dur_mins = VESS$DateFe  - VESS$DateFs #in minutes!
    VESS = VESS[,3:7]
    names(VESS)[1] = "label"
    names(VESS)[2] = "deply"
    VESS$DateFstart = force_tz(as.Date(VESS$DateFs), tzone="GMT")
    VESS$HrS =hour(VESS$DateFs)
    VESS$HrE =hour(VESS$DateFe)
    
    #CHECK plot/ CLEAN up VESS-- sort and remove duplicates
    #-----------------------------------------------------------------------------------------
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
    
    #-----------------------------------------------------------------------------------------
    #PROCESS each vessel detection, put in correct column in FullListTimes (HOURLY)
    #-----------------------------------------------------------------------------------------
    
    #MAKE new matrix to fill in the values for each hour
    #-----------------------------------------------------------------------------------------
    #make new columns to fill in data....
    
    #HOURLY vessel noise presence metrics
    FullListTimes$TotalVesselDet   = 0 #count of vessel dominated periods
    FullListTimes$TimeVesselDet    = 0 #total time vessel noise dominates
    FullListTimes$checkVD          = "noDetection"
    
    #vessel dominant periods- loop through detections and summarize by hour
    for (vv in  1:nrow(VESS) ){ # vv = 1
      
      tmp = VESS[vv,] #temp matrix
      tmp$DateFend = as.Date(tmp$DateFe) #needed end data in case it goes to the next day!
      
      #find column in with FullListTimes matching day/hour
      idx = which( as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" ) == FullListTimes$DateF) 
      
      if (length(idx) > 0){ #some detections will extend to end of deployment
        # how many hours does the vessel detection dominate?
        stH = as.POSIXct ( paste0(tmp$DateFstart, " ", tmp$HrS,":00:00"), tz = "GMT" )
        edH = as.POSIXct ( paste0(tmp$DateFend, " "  , tmp$HrE,":00:00"), tz = "GMT" )
        hrsSpan =  as.numeric( difftime(edH , stH,units = "hours") )
        
        if (hrsSpan == 1 )#spans to next hour
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
    
    #HOULRY non-vessel presence metrics (quiet periods)
    FullListTimes$QuietDur            = 0 #average duration of quiet periods within an hour- time between detections
    FullListTimes$QuietTotal          = 0 #sum total time vessel-free
    FullListTimes$QuietDurSD          = 0 #standard deviation of quiet duration in a hour
    FullListTimes$QuietPeriods        = 0 #number of quiet periods within an hour-  time between detections
    FullListTimes$QuietTotalProp      = 0 #Total quiet time in hour (seconds)/(60*60), aka Proportion of hour vessel free
    FullListTimes$QuietTotalPeriods   = 0 #Proportion of hour vessel free/count of events (higher numbers indicate more continuous vessel-free events, 
    #lower number indicate less continuous events)
    
    #(NEW) Quiet periods within each hour
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
            
          } else if(nrow(tmpDets) == 1 ) { #only one detection for the hour
            
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
    #hist(FullListTimes$CkTime)
    #FullListTimes[ FullListTimes$CkTime > 0,]
    
    # FILL IN NAs for missing data based on SPLs (not sure why some NAs within deployments... length?)
    #-----------------------------------------------------------------------------------------
    indxNA = which (is.na (FullListTimes$OL_31.5))
    FullListTimes[indxNA,16:ncol(FullListTimes)] = NA
    #check: FullListTimes[indxNA,]
    
    #FORMAT data-- hourly data (VESSfor)
    #-----------------------------------------------------------------------------------------
    FullListTimes$TimeVesselDetM = FullListTimes$TimeVesselDet/(60) 
    #errorC = FullListTimes[as.numeric(FullListTimes$TimeVesselDetM) > 60, ] # CHECk: hist(FullListTimes$TimeVesselDetM)
    
    FullListTimes$JulianDay = yday(FullListTimes$DateF)
    FullListTimes$Site   = sitesVD[ss]
    FullListTimes$Day    = as.Date(FullListTimes$DateF)
    
    #remove some columns before saving... not needed
    FullListTimes2 = FullListTimes[,  which(names(FullListTimes) != c("checkVD") ) ]
    FullListTimes2 = FullListTimes2[, which(names(FullListTimes2) != c("totalTimeM") ) ]
    
    #as.data.frame(colnames( FullListTimes2) )
    
    colnames(FullListTimes2)[2]  = "Deployment"
    colnames(FullListTimes2)[4]  = "DateTime"
    FullListTimes2$VesselTotalProp = FullListTimes2$TimeVesselDet /(60*60)
    # as.data.frame ( colnames(FullListTimes2) )
    # hist(FullListTimes2$TotalVesselDet)
    
  
  
  } else { 
    
    #cat("NO VESSEL DETECTIONS SO FILL IN ALL hourly values")
    
    #FORMAT data
    #-----------------------------------------------------------------------------------------
    #tst = as.data.frame( gsub(".000Z", "", gsub("T", " ", VESS$V1)) )
    VESS$DateFs   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V1)), tz = "GMT" )
    VESS$DateFe   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", VESS$V2)), tz = "GMT" )
    VESS$Dur_mins = VESS$DateFe  - VESS$DateFs #in minutes!
    VESS = VESS[,3:7]
    names(VESS)[1] = "label"
    names(VESS)[2] = "deply"
    VESS$DateFstart = force_tz(as.Date(VESS$DateFs), tzone="GMT")
    VESS$HrS =hour(VESS$DateFs)
    VESS$HrE =hour(VESS$DateFe)
    
    #CHECK plot/ CLEAN up VESS-- sort and remove duplicates
    #-----------------------------------------------------------------------------------------
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
    colnames(FullListTimes2)[2]  = "Deployment"
    colnames(FullListTimes2)[4]  = "DateTime"
    FullListTimes2$VesselTotalProp = FullListTimes2$TimeVesselDet /(60*60)
  }
  
  
  # EXPORT HOURLY vessel detections
  #-----------------------------------------------------------------------------------------
  fnameOut =  paste0(outDir, sitesVD[ss], "_SPLVesselDetections_Hr_",as.character(min(as.Date(FullListTimes$DateF))),"to",
                     as.character(max(as.Date(FullListTimes$DateF))), "_ver", DC, ".csv")  
  if(flagCSV == TRUE ){ write.csv(FullListTimes2,fnameOut) }
  
  #-----------------------------------------------------------------------------------------
  # CALCULATE AND EXPORT DAILY summaries
  #-----------------------------------------------------------------------------------------
  #calculate total vessels by summing detections in each day- 
  # to avoid vessels daily detections not double counted in occur in multiple hours
  dys = unique(FullListDay$Day)
  FullListDay$totalVessels = 0
  
  if (VESS$label[1] != "NoShip") { 
    for ( dd in 1:length(dys) ) {
      tmp = (VESS[VESS$DateFstart == dys[dd],]) #dd = 10
      idx = which( dys[dd]  == FullListDay$Day)
      tmp$DayE = as.Date(tmp$DateFe)
      
      if (nrow(tmp) == 0 ){ #no data on that day...
        FullListDay$totalVessels[idx] = 0
      }else {
        tmpc = nrow(tmp)
        
        #does the last one go to next day?
        hrsSpan = as.numeric(tmp$DayE[tmpc] - tmp$DateFstart[tmpc] )
        
        if (hrsSpan == 1 )#spans to next day
        {
          #add info to start day
          FullListDay$totalVessels[idx]   = FullListDay$totalVessels[idx] + tmpc    # vessel count
          #add detection to to next day
          FullListDay$totalVessels[idx+1] = FullListDay$totalVessels[idx+1] + 1     # vessel count
          
        } else { #within single day
          FullListDay$totalVessels[idx]   = FullListDay$totalVessels[idx] + tmpc     # vessel count
        }
      }
      
    }
  }
    
  indxNAd = which( is.na(FullListDay$OL_31.5) )
  FullListDay$totalVessels[indxNAd] = NA  
  
  #Alternative method for calculating daily values from hourly summaries-- accurate for time metrics, NOT total vessels
  #-----------------------------------------------------------------------------------------
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
  colnames(VESSforDay) = c("Day","sumTimeVesselDet_sec","meanHRTimeVesselDet_sec","meanHRVesselDet_cnt", "dayPropVess_percent",
                                "sumTimeQuiet_sec",     "meanHRQuietDur_sec", "sumQuietPeriods_cnt", "meanHRPropQuiet_percent", "dayPropQuiet_percent","dayWeighPeriods")
  head(VESSforDay)
  VESSforDay$Day = as.Date( VESSforDay$Day )
  
  FullListDay2 = merge(FullListDay, VESSforDay, by = "Day")

  # percent of the day with vessel noise dominating... divide seconds by total time in day sampled
  #each day divide seconds with vessel or quiet/ total time sampled
  #FullListDay2$PerDayVess  = ( as.numeric(as.character(FullListDay2$sumTimeVesselDet_sec)) / (as.numeric(as.character(FullListDay2$Hrs)) *60 *60) ) *100
  #FullListDay2$PerDayQuiet = ( as.numeric(as.character(FullListDay2$sumTimeQuiet_sec    )) / (as.numeric(as.character(FullListDay2$Hrs)) *60 *60) ) *100
  
  FullListDay2$PerDayVess  = ( as.numeric(as.character(FullListDay2$sumTimeVesselDet_sec)) / (as.numeric(as.character(DHRSsampled$n)) *60 *60) ) *100
  FullListDay2$PerDayQuiet = ( as.numeric(as.character(FullListDay2$sumTimeQuiet_sec    )) / (as.numeric(as.character(DHRSsampled$n)) *60 *60) ) *100
  
  
  head(FullListDay2)
  FullListDay2$PerDayQuiet2 = 100 - as.numeric( as.character(FullListDay2$PerDayVess ))
  
  #-----------------------------------------------------------------------------------------
  #COMBINE DAILY VD WITH AIS-- so truncates AIS data in most cases
  #-----------------------------------------------------------------------------------------
  cData = merge(FullListDay2,aData,by="Day")
  cols = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_NA_OPHRS")
  cData$LOA_ALL_OPHRS = rowSums(cData[,cols])
  cols = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_NA_UV")
  cData$LOA_ALL_UV    = rowSums(cData[,cols])
  as.data.frame(colnames(cData))
  
  cData = cData[,c(1:28, 32:41)] #columns to sum AIS results
  cDatat = cData #keep format for plotting, with old naming
  
  as.data.frame(colnames(cData))
  #rename BB column so they all match!!!
  colnames(cData)[3]  = "Deployment"
  colnames(cData)[15] = "HoursSampled"
  
  colnames(cData)[16] = "TotalVesselDet_cnt"
  colnames(cData)[17] = "TimeVesselDet_sum"
  colnames(cData)[18] = "VesselDetDur_meanHourly"
  colnames(cData)[19] = "VesselDet_meanHourly_cnt"
  colnames(cData)[20] = "PropVessel_daily"
  
  colnames(cData)[21] = "QuietDur_meanHourly"
  colnames(cData)[22] = "TimeQuiet_sum"
  colnames(cData)[23] = "TotalQuietPeriods_cnt"
  colnames(cData)[24] = "PercentQuiet_meanHourly"
  colnames(cData)[25] = "PropQuiet_daily"
  colnames(cData)[26] = "WeightQuietPeriods_daily"
  colnames(cData)[27] = "PercentVessel_daily"
  colnames(cData)[28] = "PercentQuiet_daily"
  
  as.data.frame(colnames(cData))
  
  head(cData)
  fnameOut =  paste0(outDir, sitesVD[ss], "_SPLVesselDetectionsAIS_Day_",as.character(min(as.Date(cData$Day))),"to",
                     as.character(max(as.Date(cData$Day))), "_ver", DC, ".csv")  
  
  if(flagCSV == TRUE ){ write.csv(cData,fnameOut)} 
  
  #-----------------------------------------------------------------------------------------
  #PLOT of DAILY CONDITIONS--- points with monthly running average
  #-----------------------------------------------------------------------------------------
  # remove NAs
  cDatatNA =  cDatat[!is.na(cDatat$OL_31.5),] # this messes up AIS plotting!
  #A1) daily Vessel detections
  p1 = ggplot(cDatatNA,aes(Day,totalVessels, color = deply) ) +
    geom_point(alpha = .2)+   geom_line(aes(y=rollmean(totalVessels, ra, na.pad=TRUE)),size=1) +
    xlab("")+     ylab("Daily vessel detections") + theme_minimal()+
    ylim(c(0, round(max(cDatatNA$totalVessels, na.rm = TRUE)) ) ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
 
  #B1) daily AIS vessels by type
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UV","LOA_M_UV","LOA_L_UV","LOA_ALL_UV" ))
  p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+   geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    ylab("Daily unique vessels")+     xlab("") +     theme_minimal() + 
    ylim(c(0, round(max(cDatat$totalVessels, na.rm = TRUE)) ) ) +
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position=c(.8, 0.7), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
 
  #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  colnames(cDatat)
  p2 = ggplot(cDatatNA,aes(Day,PerDayVess, color = deply) ) +
    geom_point(alpha = .2)+  geom_line(aes(y=rollmean(PerDayVess, ra, na.pad=TRUE)),size=1) +
    xlab("")+   ylab("% day dominated by vessel noise") + theme_minimal() +
    ylim(c(0,   round(max(cDatatNA$PerDayVess, na.rm = TRUE)) ) ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0))) 
  
  #B2) total operational hours near site
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRS","LOA_M_OPHRS","LOA_L_OPHRS","LOA_ALL_OPHRS" ))  
  dataAISm$Perday = ( (dataAISm$value) / 24)*100 #calcualte percent of day ships operating, can be > 100%
  p5 = ggplot(dataAISm, aes(x=Day, y=Perday, color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(Perday, ra, na.pad=TRUE)),size=1) +
    ylab("% of day vessels operating")+     xlab("") +     theme_minimal() + 
    #ylim(c(0, round(max(cDatat$PerDay, na.rm = TRUE)) ) ) +
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
 
  pALL2 = grid.arrange(p1,p3,p2,p5,nrow=2,ncol=2,top = (paste0( "Daily vessel metrics (", sitesVD[ss], ")" )))
  
  p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=.7) +
    ylab("Daily operational hours")+     xlab("") +     theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  
  if(flagPLT == TRUE ){ ggsave( paste0(outDir,sitesVD[ss], "_DailyVessleMetrics_", "_ver",DC,".png") ,pALL2)}
  
  #Normalizing values.... scale 0-1
  #-----------------------
  #A1) Total vessels
  cDatat$totalVesselsS = stdize(cDatat$totalVessels,na.rm=T)
  p1 = ggplot(cDatat,aes(Day,totalVesselsS,color = deply) ) +
    geom_point(alpha = .2)+  geom_line(aes(y=rollmean(totalVesselsS, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$totalVessels,na.rm = T)) ),fontface="bold" ) +
    xlab("")+     ylab("scaled daily vessel detections") + theme_minimal()+
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme(legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #B1) daily AIS vessels by type
  cDatat$LOA_S_UVS = stdize(cDatat$LOA_S_UV,na.rm=T)
  cDatat$LOA_M_UVS = stdize(cDatat$LOA_M_UV,na.rm=T)
  cDatat$LOA_L_UVS = stdize(cDatat$LOA_L_UV,na.rm=T)
  cDatat$LOA_ALL_UVS = stdize(cDatat$LOA_ALL_UV,na.rm=T)
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_UVS","LOA_M_UVS","LOA_L_UVS","LOA_ALL_UVS" ))
  p3 =  ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", round(max(cDatat$LOA_ALL_UV,na.rm = T))),fontface="bold" ) +
    ylab("scaled daily unique vessels")+     xlab("") +   theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position=c(.8, 0.6), axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #A2) % day dominated by Vessel noise, from total time dominated by vessel noise
  cDatat$PerDayS = stdize(cDatat$PerDayVess,na.rm=T)
  p2 = ggplot(cDatat,aes(Day,PerDayS,color = deply) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(PerDayS, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max = ", round(max(cDatat$PerDayVess,na.rm = T)) ,"%"),fontface="bold" ) +
    xlab("")+   ylab("scaled % day dominated by vessel noise") + theme_minimal() +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"),limits = as.Date(c(sDatePlot,eDatePlot))) +
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1),legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  #B2) total operational hours near site AND percent of day
  #how do I scale percent of the day???
  cDatat$LOA_S_OPHRSS   = stdize( ((cDatat$LOA_S_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_M_OPHRSS   = stdize( ((cDatat$LOA_M_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_L_OPHRSS   = stdize( ((cDatat$LOA_L_OPHRS/24)*100)   ,na.rm=T)
  cDatat$LOA_ALL_OPHRSS = stdize( ((cDatat$LOA_ALL_OPHRS/24)*100) ,na.rm=T)
  dataAISm = reshape2 :: melt(cDatat, id.vars = "Day", measure.vars = c("LOA_S_OPHRSS","LOA_M_OPHRSS","LOA_L_OPHRSS","LOA_ALL_OPHRSS" ))
  mx = round(max(((cDatat$LOA_ALL_OPHRS/24)*100),na.rm = T))
  #which.max(cDatat$LOA_ALL_OPHRS)
  
  p4 = ggplot(dataAISm,aes(x=Day,y=value,color=factor(variable)) ) +
    geom_point(alpha = .2)+ geom_line(aes(y=rollmean(value, ra, na.pad=TRUE)),size=1) +
    annotate( geom = "text", x = as.Date(eDatePlot)-100, y = 0.95, label = paste0("Max (ALL) = ", mx ,"%"), fontface="bold" ) +
    ylab("scaled % of day vessels operating")+     xlab("") +     theme_minimal() + 
    scale_colour_manual(name = "AIS vessls", values=c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00") ) +
    scale_x_date(date_breaks = "2 month", labels=date_format("%b-%y"), limits = as.Date(c(sDatePlot,eDatePlot)))+
    theme( legend.position="none", axis.text.x = element_text(angle=45, hjust = 1), legend.key = element_rect(colour = "transparent", fill = alpha("red", 0)) ) 
  
  pALLS = grid.arrange(p1,p3,p2,p4,nrow=2,ncol=2,top = (paste0( "scaled Daily vessel metrics (", sitesVD[ss], ")" )))
  
  if(flagPLT == TRUE ){ ggsave( paste0(outDir,sitesVD[ss], "_DailyVessleMetricsScaled_", "_ver",DC,".png"), pALLS) }
  
  #-----------------------------------------------------------------------------------------
  #SUMMARY of metrics by site-- all data
  #-----------------------------------------------------------------------------------------
  as.data.frame(colnames( cData ))
  head(cData)
  a0  = nrow( cData[!is.na(cData$OL_31.5),] ) #need to not count NA rows even though AIS data on all days, total days = length( unique(cDatat$Day) ), number of AIS days with data
  aDR = ( c( as.character(min( cData$Day )), as.character(max( cData$Day ))) )
  
  #AIS averages (use all the data,  remove days without SPL??)
  a1 = mean(as.numeric(as.character(cData$LOA_S_UV)),na.rm = T)
  a2 = mean(as.numeric(as.character(cData$LOA_M_UV)),na.rm = T)
  a3 = mean(as.numeric(as.character(cData$LOA_L_UV)) ,na.rm = T)
  a4 = mean(as.numeric(as.character(cData$LOA_ALL_UV)),na.rm = T)
  
  a5 = mean(as.numeric(as.character(cData$LOA_S_OPHRS)),na.rm = T)
  a6 = mean(as.numeric(as.character(cData$LOA_M_OPHRS)),na.rm = T)
  a7 = mean(as.numeric(as.character(cData$LOA_L_OPHRS)),na.rm = T)
  a8 = mean(as.numeric(as.character(cData$LOA_ALL_OPHRS)),na.rm = T)
  
  #percent of day OPHRS
  a13 = mean(as.numeric(as.character( ((cData$LOA_S_OPHRS/24)*100))), na.rm = T) 
  a14 = mean(as.numeric(as.character( ((cData$LOA_M_OPHRS/24)*100))), na.rm = T)
  a15 = mean(as.numeric(as.character( ((cData$LOA_L_OPHRS/24)*100))), na.rm = T)
  a16 = mean(as.numeric(as.character( ((cData$LOA_ALL_OPHRS/24)*100))), na.rm = T)
  
  #vessel detection averages-- new variables, not include the hourly summaries
  #a12 = mean(as.numeric(as.character(cData$VesselDet_meanHourly_cnt)),na.rm = T)#average for average hourly vessel events per day-- not ideal!
  a12 = mean(as.numeric(as.character(cData$TotalVesselDet_cnt)),na.rm = T)      #average vessel events per day
  a11 = mean(as.numeric(as.character(cData$PercentVessel_daily)),na.rm = T)     #average percent of day with vessel present
  a9  = mean(as.numeric(as.character(cData$TimeVesselDet_sum)),na.rm = T)       #average total time with vessels per day
  
  #quiet metrics
  a22  = mean(as.numeric(as.character(cData$TotalQuietPeriods_cnt)),na.rm = T)       #average quiet periods per day
  a23  = mean(as.numeric(as.character(cData$PercentQuiet_daily)),na.rm = T)          #average percent of day with quiet
  a24  = mean(as.numeric(as.character(cData$TimeQuiet_sum)),na.rm = T)               #average total time quiet
  a25  = mean(as.numeric(as.character(cData$WeightQuietPeriods_daily)),na.rm = T)    #average weighted quiet periods- Prop of hour vessel free/count of events (higher numbers indicate more continuous vessel-free events) 
  
  #median SPLs
  tmpSPL = ( cDatat[, c( as.numeric(grep("^OL", colnames(cDatat) ) ), 14) ] )  # get data
  w <- which( sapply( tmpSPL, class ) == 'character' )  #make values numeric 
  tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  
  SPLnames = colnames(tmpSPL)
  
  #median SPLs with NO or YES vessels present from hourly data
  #remove rows with no SPL data, so not included in the proporiton of hours!
  FullListTimes2narm = FullListTimes2[! is.na(FullListTimes2$OL_31.5),] 
  
  SPLvesY = FullListTimes2narm[FullListTimes2narm$TotalVesselDet > 0,]
  gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
  tmpSPL = ( SPLvesY[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLmVessY = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  a21 = nrow(SPLvesY)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
  
  SPLvesN = FullListTimes2narm[FullListTimes2narm$TotalVesselDet == 0,]
  gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
  tmpSPL = ( SPLvesN[,gg] )
  w <- which( sapply( tmpSPL, class ) == 'factor' )
  tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
  SPLmVessN = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
  a20 = nrow(SPLvesN)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
  
  SPLnamesN = sub("OL_", "NoVess_OL_",  SPLnames)
  SPLnamesY = sub("OL_", "YesVess_OL_", SPLnames)
  
  #combine summaries for all data
  tmpOUT = t(as.data.frame( c(cDatat$Site[1], as.character(aDR), length(sFiles), nFiles, a0, 
                              a1,a2,a3,a4,a5,a6,a7,a8, 
                              a13,a14,a15,a16, 
                              a9, a11, a12, 
                              a20, a21, 
                              a22, a23, a24, a25, 
                              SPLm, SPLmVessY, SPLmVessN ) ))

   colnames(tmpOUT) = c("Site","Start Day","End Day", "shipFiles", "SPLfiles", "TotalDays",
                       "mAIS_S_UV", "mAIS_M_UV", "mAIS_L_UV", "mAIS_A_UV", "mAIS_S_OPHRS","mAIS_M_OPHRS","mAIS_L_OPHRS","mAIS_A_OPHRS",
                       "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
                       "VesselDur_meanDaySec","Vessel_meanPercentDay","VesselDets_meanCnt",
                       "NoVessel_PercentofDays","YesVessel_PercentofDays",
                       "QuietPeriods_meanCnt","Quiet_meanPercentDay","QuietDur_meandDaySec","QuietPeriodWeighted",
                       SPLnames, SPLnamesY, SPLnamesN)

  output = as.data.frame(output, stringsAsFactors = FALSE)
  output = rbind (output, tmpOUT )
  
  #-----------------------------------------------------------------------------------------
  #SUMMARY of metrics by site-- truncated time period, 2019 (output2)
  #-----------------------------------------------------------------------------------------
  yoi = yearflag
  cDatatKeep = cData  # cData = cDatatKeep
  cData$YR   = year(cData$Day)
  cData      = cData[cData$YR == yoi,]
  
  #remove days without OL data... to truncate to only when all data are available
  cData = cData[!is.na(cData$OL_31.5),]
  
  #all data in time period of interest
  if ( nrow(cData) != 0 ) {
    
    a0  = nrow( cData[!is.na(cData$OL_31.5),] ) #need to not count NA rows even though AIS data on all days, total days = length( unique(cDatat$Day) ), number of AIS days with data
    aDR = ( c( as.character(min( cData$Day )), as.character(max( cData$Day ))) )
    
    #AIS averages (use all the data,  remove days without SPL??)
    a1 = mean(as.numeric(as.character(cData$LOA_S_UV)),na.rm = T)
    a2 = mean(as.numeric(as.character(cData$LOA_M_UV)),na.rm = T)
    a3 = mean(as.numeric(as.character(cData$LOA_L_UV)) ,na.rm = T)
    a4 = mean(as.numeric(as.character(cData$LOA_ALL_UV)),na.rm = T)
    
    a5 = mean(as.numeric(as.character(cData$LOA_S_OPHRS)),na.rm = T)
    a6 = mean(as.numeric(as.character(cData$LOA_M_OPHRS)),na.rm = T)
    a7 = mean(as.numeric(as.character(cData$LOA_L_OPHRS)),na.rm = T)
    a8 = mean(as.numeric(as.character(cData$LOA_ALL_OPHRS)),na.rm = T)
    
    #percent of day OPHRS
    a13 = mean(as.numeric(as.character( ((cData$LOA_S_OPHRS/24)*100))), na.rm = T) 
    a14 = mean(as.numeric(as.character( ((cData$LOA_M_OPHRS/24)*100))), na.rm = T)
    a15 = mean(as.numeric(as.character( ((cData$LOA_L_OPHRS/24)*100))), na.rm = T)
    a16 = mean(as.numeric(as.character( ((cData$LOA_ALL_OPHRS/24)*100))), na.rm = T)
    
    #vessel detection averages-- new variables, not include the hourly summaries
    #a12 = mean(as.numeric(as.character(cData$VesselDet_meanHourly_cnt)),na.rm = T)#average for average hourly vessel events per day-- not ideal!
    a12 = mean(as.numeric(as.character(cData$TotalVesselDet_cnt)),na.rm = T)     #average vessel events per day
    a11 = mean(as.numeric(as.character(cData$PercentVessel_daily)),na.rm = T)     #average percent of day with vessel present
    a9  = mean(as.numeric(as.character(cData$TimeVesselDet_sum)),na.rm = T)       #average total time with vessels per day
    
    #quiet metrics
    a22  = mean(as.numeric(as.character(cData$TotalQuietPeriods_cnt)),na.rm = T)       #average quiet periods per day
    a23  = mean(as.numeric(as.character(cData$PercentQuiet_daily)),na.rm = T)          #average percent of day with quiet
    a24  = mean(as.numeric(as.character(cData$TimeQuiet_sum)),na.rm = T)               #average total time quiet
    a25  = mean(as.numeric(as.character(cData$WeightQuietPeriods_daily)),na.rm = T)    #average weighted quiet periods- Prop of hour vessel free/count of events (higher numbers indicate more continuous vessel-free events) 
    
    #median SPLs
    gg = c( as.numeric(grep("^OL", colnames(cData) ) ), 14)
    tmpSPL = ( cData[,gg] )
    w <- which( sapply( tmpSPL, class ) == 'character' )
    tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
    SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    
    SPLnames = colnames(tmpSPL)
    
    #median SPLs with NO or YES vessels present from hourly data
    #remove rows with no SPL data, so not included in the proportion of hours!
    FullListTimes2narm = FullListTimes2[! is.na(FullListTimes2$OL_31.5),] 
    FullListTimes2narm$Yr = year(FullListTimes2narm$DateTime)
    FullListTimes2narm    = FullListTimes2narm[FullListTimes2narm$Yr == 2019,]
    
    SPLvesY = FullListTimes2narm[FullListTimes2narm$TotalVesselDet > 0,]
    gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
    tmpSPL = ( SPLvesY[,gg] )
    w <- which( sapply( tmpSPL, class ) == 'factor' )
    tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
    SPLmVessY = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    a21 = nrow(SPLvesY)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
    
    SPLvesN = FullListTimes2narm[FullListTimes2narm$TotalVesselDet == 0,]
    gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
    tmpSPL = ( SPLvesN[,gg] )
    w <- which( sapply( tmpSPL, class ) == 'factor' )
    tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
    SPLmVessN = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
    a20 = nrow(SPLvesN)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
    
    SPLnamesN = sub("OL_", "NoVess_OL_",  SPLnames)
    SPLnamesY = sub("OL_", "YesVess_OL_", SPLnames)
    
    #combine summaries for all data
    tmpOUT = t(as.data.frame( c(cData$Site[1], as.character(aDR), length(sFiles), nFiles, a0, 
                                a1,a2,a3,a4,a5,a6,a7,a8, 
                                a13,a14,a15,a16, 
                                a9, a11, a12, 
                                a20, a21, 
                                a22, a23, a24, a25, 
                                SPLm, SPLmVessY, SPLmVessN ) ))
    
    colnames(tmpOUT) = c("Site","Start Day","End Day", "shipFiles", "SPLfiles", "TotalDays",
                         "mAIS_S_UV", "mAIS_M_UV", "mAIS_L_UV", "mAIS_A_UV", "mAIS_S_OPHRS","mAIS_M_OPHRS","mAIS_L_OPHRS","mAIS_A_OPHRS",
                         "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
                         "VesselDur_meanDaySec","Vessel_meanPercentDay","VesselDets_meanCnt",
                         "NoVessel_PercentofDays","YesVessel_PercentofDays",
                         "QuietPeriods_meanCnt","Quiet_meanPercentDay","QuietDur_meandDaySec","QuietPeriodWeighted",
                         SPLnames, SPLnamesY, SPLnamesN)
    
    output2 = as.data.frame(output2,stringsAsFactors = FALSE)
    output2 = rbind (output2, tmpOUT )
  } else {
 
    cat("No ", yearflag, "  data for: ", cDatatKeep$Site[1], "\n")
  }
  
  #-----------------------------------------------------------------------------------------
  #SUMMARY of metrics by site-- truncated time period, 2019 and by month (output4)
  #-----------------------------------------------------------------------------------------
  as.data.frame(colnames(cData))
  
  #monthly summaries in time period of interest
  if ( nrow(cData) != 0 ) {
  
    cData$Mth = month(cData$Day)
    cData[4:40] =  lapply(cData[4:40], function(x) as.numeric(as.character(x)))
    
    # calculate median values-- all SPL
    cDataM1  = aggregate(cData[,4:14], by=list(cData$Mth), median, na.rm=T)  
    colnames(cDataM1) = gsub("OL","OL_median",colnames(cDataM1))
    colnames(cDataM1) = gsub("BB","BB_median",colnames(cDataM1))
    
    # calculate sd values-- 125 Hz SPL (only)
    cDataSD  = aggregate(cData$OL_125, by=list(cData$Mth), sd, na.rm=T) 
    cDataM1 = cbind(cDataM1,cDataSD[,2])
    colnames(cDataM1)[13] = "OL_sd_125"
    
    # calculate median values--vessel SPL
    SPLvesY = cData[cData$TotalVesselDet_cnt > 0,]
    #ISSUE-- no days with  vessel detections
    if (nrow(SPLvesY)== 0 ){
      cDataM2Y  = cbind(cDataM1$Group.1 , NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      colnames(cDataM2Y) = colnames(cDataM1)[1:12]
      colnames(cDataM2Y) = gsub("OL","VESS_OL",colnames(cDataM2Y))
      colnames(cDataM2Y) = gsub("BB","VESS_BB",colnames(cDataM2Y))
    } else {
      cDataM2Y  = aggregate(SPLvesY[,4:14], by=list(SPLvesY$Mth), mean, na.rm=T)
      colnames(cDataM2Y) = gsub("OL","VESS_OL_median",colnames(cDataM2Y))
      colnames(cDataM2Y) = gsub("BB","VESS_BB_median",colnames(cDataM2Y))
    }
    #ISSUE-- some months without non-vessel days data!!
    
    # calculate median values--non-vessel SPL
    SPLvesN   = cData[cData$TotalVesselDet_cnt == 0,]
    #ISSUE-- no days without vessel detections
    if (nrow(SPLvesN)== 0 ){
      cDataM2N  = cbind(cDataM2Y$Group.1, NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
      colnames(cDataM2N) = colnames(cDataM2Y)
      colnames(cDataM2N) = gsub("VESS_OL_median","NoVESS_OL_median",colnames(cDataM2N))
      colnames(cDataM2N) = gsub("VESS_BB_median","NoVESS_BB_median",colnames(cDataM2N))
    } else {
      cDataM2N  = aggregate(SPLvesN[,4:14], by=list(SPLvesN$Mth), mean, na.rm=T) 
      colnames(cDataM2N) = gsub("OL","NoVESS_OL_median",colnames(cDataM2N))
      colnames(cDataM2N) = gsub("BB","NoVESS_BB_median",colnames(cDataM2N))
    }
    
    #ISSUE-- some months without vessel days data!!
    
    # calculate mean values
    cDataM2  = aggregate(cData[,15:ncol(cData)], by=list(cData$Mth), mean, na.rm=T) 
    colnames(cDataM2) = paste0(colnames(cDataM2),"_mean")
    #SD for vessel metrics
    as.data.frame(colnames(cData))
    cDataMSD = aggregate(cData[,15:ncol(cData)], by=list(cData$Mth), sd, na.rm=T) 
    colnames(cDataMSD) = paste0(colnames(cDataMSD),"_SD")
    
    # total counts values
    cDataM3 =  aggregate(cData[,c(29,31,33,38)], by=list(cData$Mth), sum, na.rm=T) 
    colnames(cDataM3) = gsub("UV","UV_sum",colnames(cDataM3))
    cDataDys = cData %>% count(Mth)  
    colnames(cDataDys) = c("Mth","Days")
    
    #combine
    cDataM  = NULL
    cDataM$Site = cData$Site[1]
    #bind together SPL data first... because issue with different data lengths
    m1 = merge(cDataM1, cDataM2Y, by = "Group.1" , all =T) 
    m1 = merge(m1, cDataM2N,      by = "Group.1" , all =T) 
    
    cDataM   = cbind(cDataM$Site, cDataDys$Days, m1, cDataM2[,1:ncol(cDataM2)-1], cDataM3[,1:ncol(cDataM3)-1], cDataMSD[,1:ncol(cDataMSD)-1]) 
  
    colnames(cDataM)[1] = "Site"
    colnames(cDataM)[2] = "Days"
    colnames(cDataM)[3] = "Mth"
    cDataM = cDataM[ , -which(names(cDataM) %in% "Group.1") ]
    cDataM = cDataM[ , -which(names(cDataM) %in% "Group.1_mean") ]
    
    # HOURLY METRICS (works better for many comparative metrics)
    #----------------------------------------------------------
    #calculate a few hourly metrics from the FullListTimes
    FullListTimes$Mth = month(FullListTimes$Day)
    FullListTimes$Yr = year(FullListTimes$Day)
    hData = FullListTimes[FullListTimes$Yr == yoi,]
    #remove NAs before running
    hData = hData[!is.na(hData$OL_31.5) ,]
    
    # Hours with vessel noise
    hData$TotalVesselDetP = 0
    hData$TotalVesselDetP[hData$TotalVesselDet > 0] = 1 
    totaHrsVess = aggregate(hData$TotalVesselDetP, by=list(hData$Mth), sum, na.rm=T)  
    totalHrs    = hData %>% count(Mth)  
    #proportion of hours with vessels detected
    PrpHrsVESS = totaHrsVess$x/totalHrs$n
    PrpHRQUIET = 1-PrpHrsVESS
    #sometimes it does not match up so need to use MERGE
    hData2 = cbind(totaHrsVess$Group.1, PrpHrsVESS, PrpHRQUIET ) 
    colnames(hData2) = c("Mth","PrpHRSVESS","PrpHrsQUIET")
    cDataM2 = merge(cDataM,hData2, by = "Mth")
    
    # Hours SPL vessel vs non vessel
    hrsNOVess  = hData[ hData$TotalVesselDetP == 0, ]
    NVessMed   = aggregate(hrsNOVess$OL_125, by=list(hrsNOVess$Mth), median, na.rm=T)
    NVessSd    = aggregate(hrsNOVess$OL_125, by=list(hrsNOVess$Mth), sd, na.rm=T)
    
    hrsVess   = hData[ hData$TotalVesselDetP > 0, ]
    if ( nrow(hrsVess) == 0 ) { 
      #some sites with no hours with vessel detections!!
      YVessMed = NA
      YVessSd  = NA
      
      hData3 = cbind( NVessMed$Group.1, NVessMed$x, NVessSd$x)
      colnames(hData3) = c("Mth","NVess125_med","NVess125_sd")
      hData4 = cbind( NVessMed$Group.1, YVessMed, YVessSd)
      colnames(hData4) = c("Mth","YVess125_med", "YVess125_sd")
      
      hData3  = merge(hData3, hData4, by ="Mth"  )
      cDataM3 = merge(cDataM2,hData3, by = "Mth")
      
    } else{
      YVessMed = aggregate(hrsVess$OL_125, by=list(hrsVess$Mth), median, na.rm=T)
      YVessSd  = aggregate(hrsVess$OL_125, by=list(hrsVess$Mth), sd, na.rm=T)
      
      hData3 = cbind( NVessMed$Group.1, NVessMed$x, NVessSd$x)
      colnames(hData3) = c("Mth","NVess125_med","NVess125_sd")
      hData4 = cbind( YVessMed$Group.1, YVessMed$x, YVessSd$x)
      colnames(hData4) = c("Mth","YVess125_med", "YVess125_sd")
      
      hData3 = merge(hData3,hData4,by ="Mth"  )
      cDataM3 = merge(cDataM2,hData3, by = "Mth")
    }
    
    #COMBINE SITE RESULTS
    output4 = as.data.frame(output4, stringsAsFactors = FALSE)
    output4 = rbind (output4, cDataM3)
    
    rm(m1, cDataM1, cDataM,cDataM2Y,cDataM2N, cDataDys,cDataM2, cDataM3)
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

# #-----------------------------------------------------------------------------------------
# #if SBO2== calculate values for 2020 data (output3)
# #-----------------------------------------------------------------------------------------
# if (sitesVD[ss] == "SB02") {
#   
#   cDatatKeep$YR = year(cDatatKeep$Day)
#   cData    = cDatatKeep[cDatatKeep$YR == 2020,]
#   
#   a0  = nrow( cData[!is.na(cData$OL_31.5),] ) #need to not count NA rows even though AIS data on all days, total days = length( unique(cDatat$Day) ), number of AIS days with data
#   aDR = ( c( as.character(min( cData$Day )), as.character(max( cData$Day ))) )
#   
#   #AIS averages (use all the data,  remove days without SPL??)
#   a1 = mean(as.numeric(as.character(cData$LOA_S_UV)),na.rm = T)
#   a2 = mean(as.numeric(as.character(cData$LOA_M_UV)),na.rm = T)
#   a3 = mean(as.numeric(as.character(cData$LOA_L_UV)) ,na.rm = T)
#   a4 = mean(as.numeric(as.character(cData$LOA_ALL_UV)),na.rm = T)
#   
#   a5 = mean(as.numeric(as.character(cData$LOA_S_OPHRS)),na.rm = T)
#   a6 = mean(as.numeric(as.character(cData$LOA_M_OPHRS)),na.rm = T)
#   a7 = mean(as.numeric(as.character(cData$LOA_L_OPHRS)),na.rm = T)
#   a8 = mean(as.numeric(as.character(cData$LOA_ALL_OPHRS)),na.rm = T)
#   
#   #percent of day OPHRS
#   a13 = mean(as.numeric(as.character( ((cData$LOA_S_OPHRS/24)*100))), na.rm = T) 
#   a14 = mean(as.numeric(as.character( ((cData$LOA_M_OPHRS/24)*100))), na.rm = T)
#   a15 = mean(as.numeric(as.character( ((cData$LOA_L_OPHRS/24)*100))), na.rm = T)
#   a16 = mean(as.numeric(as.character( ((cData$LOA_ALL_OPHRS/24)*100))), na.rm = T)
#   
#   #vessel detection averages-- new variables, not include the hourly summaries
#   #a12 = mean(as.numeric(as.character(cData$VesselDet_meanHourly_cnt)),na.rm = T)#average for average hourly vessel events per day-- not ideal!
#   a12 = mean(as.numeric(as.character(cData$TotalVesselDet_cnt)),na.rm = T) #average vessel events per day
#   a11 = mean(as.numeric(as.character(cData$PercentVessel_daily)),na.rm = T)     #average percent of day with vessel present
#   a9  = mean(as.numeric(as.character(cData$TimeVesselDet_sum)),na.rm = T)       #average total time with vessels per day
#   
#   #quiet metrics
#   a22  = mean(as.numeric(as.character(cData$TotalQuietPeriods_cnt)),na.rm = T)       #average quiet periods per day
#   a23  = mean(as.numeric(as.character(cData$PercentQuiet_daily)),na.rm = T)          #average percent of day with quiet
#   a24  = mean(as.numeric(as.character(cData$TimeQuiet_sum)),na.rm = T)               #average total time quiet
#   a25  = mean(as.numeric(as.character(cData$WeightQuietPeriods_daily)),na.rm = T)    #average weighted quiet periods- Prop of hour vessel free/count of events (higher numbers indicate more continuous vessel-free events) 
#   
#   #median SPLs
#   gg = c( as.numeric(grep("^OL", colnames(cData) ) ), 14)
#   tmpSPL = ( cData[,gg] )
#   w <- which( sapply( tmpSPL, class ) == 'factor' )
#   tmpSPL[w] <- lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
#   SPLm = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
#   
#   SPLnames = colnames(tmpSPL)
#   
#   #median SPLs with NO or YES vessels present from hourly data
#   #remove rows with no SPL data, so not included in the proportion of hours!
#   FullListTimes2narm = FullListTimes2[! is.na(FullListTimes2$OL_31.5),] 
#   FullListTimes2narm$Yr = year(FullListTimes2narm$DateTime)
#   FullListTimes2narm    = FullListTimes2narm[FullListTimes2narm$Yr == 2020,]
#   
#   SPLvesY = FullListTimes2narm[FullListTimes2narm$TotalVesselDet > 0,]
#   gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
#   tmpSPL = ( SPLvesY[,gg] )
#   w <- which( sapply( tmpSPL, class ) == 'factor' )
#   tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
#   SPLmVessY = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
#   a21 = nrow(SPLvesY)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
#   
#   SPLvesN = FullListTimes2narm[FullListTimes2narm$TotalVesselDet == 0,]
#   gg = c( as.numeric(grep("^OL", colnames(FullListTimes2narm) ) ), 15)
#   tmpSPL = ( SPLvesN[,gg] )
#   w <- which( sapply( tmpSPL, class ) == 'factor' )
#   tmpSPL[w] = lapply( tmpSPL[w], function(x) as.numeric(as.character(x)) )
#   SPLmVessN = apply( tmpSPL, 2 , quantile , probs = 0.5 , na.rm = TRUE )
#   a20 = nrow(SPLvesN)/nrow(FullListTimes2narm) #proportion of hours with no vessel detection
#   
#   SPLnamesN = sub("OL_", "NoVess_OL_",  SPLnames)
#   SPLnamesY = sub("OL_", "YesVess_OL_", SPLnames)
#   
#   #combine summaries for all data
#   tmpOUT = t(as.data.frame( c(cData$Site[1], as.character(aDR), length(sFiles), nFiles, a0, 
#                               a1,a2,a3,a4,a5,a6,a7,a8, 
#                               a13,a14,a15,a16, 
#                               a9, a11, a12, 
#                               a20, a21, 
#                               a22, a23, a24, a25, 
#                               SPLm, SPLmVessY, SPLmVessN ) ))
#   
#   colnames(tmpOUT) = c("Site","Start Day","End Day", "shipFiles", "SPLfiles", "TotalDays",
#                        "mAIS_S_UV", "mAIS_M_UV", "mAIS_L_UV", "mAIS_A_UV", "mAIS_S_OPHRS","mAIS_M_OPHRS","mAIS_L_OPHRS","mAIS_A_OPHRS",
#                        "mAIS_S_PerOPHRS","mAIS_M_PerOPHRS","mAIS_L_PerOPHRS","mAIS_A_PerOPHRS",
#                        "VesselDur_meanDaySec","Vessel_meanPercentDay","VesselDets_meanCnt",
#                        "NoVessel_PercentofDays","YesVessel_PercentofDays",
#                        "QuietPeriods_meanCnt","Quiet_meanPercentDay","QuietDur_meandDaySec","QuietPeriodWeighted",
#                        SPLnames, SPLnamesY, SPLnamesN)
#   
#   output3 = as.data.frame(output3,stringsAsFactors = FALSE)
#   output3 = rbind (output3, tmpOUT )
# #   
# # }
# fnameOut =  paste0(outDir, "Summary2020_ver", DC, ".csv")  
# write.csv(output3,fnameOut)