# Purpose: 
# label and Integrate 1-min hybrid-milli-decade (HMD) data with event based detections
# Event detections = detection periods with start and end time

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
rm(list=ls()) 

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# DIRECTORIES ####
site = "OC02"
dirTop = "F:\\SanctSound" # ?? create loop through all manta directories ??
pltf = 0 # change to 1 if you want to plot daily 1-min spectra
dirOut = paste0("F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction\\", site)

#site = "SB03"

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# MANTA HMD DATA ####
# by deployment
ver = "manta_9.6.14"
#inSites = c( "SB03_02", "SB03_03", "SB03_04", "SB03_05", "SB03_06", "SB03_07","SB03_08", "SB03_09",
#             "SB03_10", "SB03_11", "SB03_12", "SB03_13", "SB03_14", "SB03_15", "SB03_16", "SB03_17", "SB03_18")
inSites = c( "OC02_01", "OC02_02", "OC02_04", "OC02_06")

# HMD processed for SB03_18-21 but no AIS or detections files
stFQ = 100
edFQ = 1997.6

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
idx = which( duplicated(AIStranOC) )
#AIStranOC$Start[idx]
#unique(AIStranOC$loc_id )
#max( AIStranOC$Start )
#min( AIStranOC$Start )

  #_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# GENERAL INFORMATION ####
HMDcolnames = NULL
cnt = 0
cntdul = 0
cnt00 = 0

for (s in 1:length(inSites)  ) { #  1:length(inSites) s = 1
  
  inS = inSites[s] 
  inDir = paste0(dirTop, "//", inS, "//", ver)
  inHMD =  list.files(path = inDir, pattern = "MinRes.csv", full.names = T,recursive = T)
  cnt = cnt + length(inHMD) 
  
  st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 1) #site name
  if (st == "SanctSound"){
    st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 2) #site name
    dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 3) # deployment name
  } else {
    dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 2) # deployment name
  }
  dirSite = unlist(strsplit(inDir, '/'))
  dirSite = paste0(dirSite[-length(dirSite)], collapse = '/')

  dirDets = paste0(dirSite,"\\detections\\")
  detFiles = list.files(path = dirDets, pattern = paste(st, dpl, sep="_"), full.names = T, recursive = T)
  detFiles = detFiles[!grepl("1d", detFiles)] #remove 1 day files
  detFiles = detFiles[!grepl("metadata", detFiles)] #remove metadata
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove nc files
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove xlz files
  detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(detFiles)), "_"), "[[", 4) #site name
  cat(inS, "-", length(inHMD), " HMD files,", ver, detTypes, "\n")

}

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# COMBINE DETECTIONS ####
for (s in 1:length(inSites)  ) {  #s = 1
  
  inS = inSites[s] 
  inDir = paste0(dirTop, "//", inS, "//", ver)
  inHMD =  list.files(path = inDir, pattern = "MinRes.csv", full.names = T,recursive = T)
  
  st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 1) #site name
  if (st == "SanctSound"){
    st = sapply(strsplit(basename( inHMD [1] ), "_"), "[[", 2) #site name
    dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 3) # deployment name
  } else {
    dpl = sapply(strsplit(basename( inHMD[1] ), "_"), "[[", 2) # deployment name
  }
  dirSite = unlist(strsplit(inDir, '/'))
  dirSite = paste0(dirSite[-length(dirSite)], collapse = '/')
  
  
  dirDets = paste0(dirSite,"\\detections\\")
  detFiles = list.files(path = dirDets, pattern = paste(st,dpl,sep="_"), full.names = T, recursive = T)
  detFiles = detFiles[!grepl("1d", detFiles)] #remove 1 day files
  detFiles = detFiles[!grepl("dolphins", detFiles)] #remove 1 hour files
  detFiles = detFiles[!grepl("metadata", detFiles)] #remove metadata
  detFiles = detFiles[!grepl("\\.nc", detFiles)] #remove nc files
  detTypes = sapply(strsplit(sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(detFiles)), "_"), "[[", 4) #site name
  
  cat("Processing..." , inS, "-", length(inHMD), " HMD files,", ver, ': ', detTypes)
 
  ## LOOP through detections ####
  detAll = NULL
  for (dd in 1:length(detTypes) ) {
    
    inTmp = tolower( detTypes[dd] )
    
    ## VESSEL detections ####
    if (inTmp == "ships" ){
      detTmp = detFiles[grepl(inTmp, detFiles)] 
      tmp = read.csv(detTmp)
      
      colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
      
      
      if (tmp$Label[1] != "NoShip") {
        tmp$Label = gsub("Ship", "ship", tmp$Label)
        tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
        tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
        tmp$Site = st
        tmp$Dep  = dpl
        tmp$Yr  = year(tmp$Start )
        tmp$Mth = month(tmp$Start )
        tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
        tmp$DurH = tmp$DurS/3600 
        tmp = tmp[tmp$Label == "ship", ] # head(VD)
        tmp$Type = paste0( inTmp, "_anthro") # head(VD)
        detAll = rbind(detAll, tmp)
        
        rm(tmp)
      }
    }
    
    ## atlanticcod detections ####
    if (inTmp == "atlanticcod" ){
      detTmp = detFiles[grepl(inTmp, detFiles)] 
      tmp = read.csv(detTmp)
      if (nrow(tmp) >0 ) {
        if (length( colnames(tmp) ) == 3 ){
          colnames(tmp) = c("ISOStartTime","ISOEndTime","Label" )
          
          if (tmp$Label[1] != 0)  {
            tmp$Start = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOStartTime)), tz = "GMT" )
            tmp$End   = as.POSIXct( gsub(".000Z", "", gsub("T", " ", tmp$ISOEndTime)),   tz = "GMT" )
            tmp$Site = st
            tmp$Dep  = dpl
            tmp$Yr  = year(tmp$Start )
            tmp$Mth = month(tmp$Start )
            tmp$DurS = as.numeric(as.character( difftime(tmp$End, tmp$Start, units = "secs" )) )
            tmp$DurH = tmp$DurS/3600
            tmp$Type = paste0( inTmp, "_bio") 
            detAll = rbind(detAll, tmp)
            rm(tmp)
          }
        }  
      }
    }
    
  }
  
  ## MERGE DETS with HMD ####
  HMDdet = NULL
  ck2 = NULL
  utypes = unique(detAll$Type)
  HMDcheck = NULL
  
  for (ii in 1:length(inHMD)){ #loop through daily files
    
    ###read in daily file ####
    inFile = inHMD[ii]
    inHMDcsv = read.csv(inFile)
    ck = 1440 - dim(inHMDcsv)[1] # are there extra rows?
    # this is not always correct because file names change.... only used for labeling, so did not change
    nsubString = lengths( strsplit( basename( inFile ), "_") )
    if (nsubString == 7){
      dy = as.Date ( gsub (".csv","", sapply(strsplit( basename( inFile ), "_"), "[[", 4) ), format="%Y%m%d" )
      
    }else{
      dy = as.Date ( gsub (".csv","", sapply(strsplit( basename( inFile ), "_"), "[[", 5) ), format="%Y%m%d" )
    }
    
    colnames(inHMDcsv)[1] = "dateTime"
    inHMDcsv$dateTime = as.POSIXct(   inHMDcsv$dateTime, format = "%d-%b-%Y %H:%M:%S" , tz = "GMT" ) # Date format: format the date (? will netCDF files be the same?)
    fq = as.numeric(as.character( gsub("X","", colnames(inHMDcsv[3:ncol(inHMDcsv)] )) ) ) # Frequency range: truncate to 100-2000 Hz
    str = which(fq == stFQ)+2      #  colnames(inHMDcsv)[st]
    ed =  which(fq == edFQ)+2     #  colnames(inHMDcsv)[ed]
    
    ###check and remove rows that split a given minute into sections  ####
    # Check: length( unique( inHMDcsv$dateTime) ) # they are all unique!! wtf
    # extra rows happen when a minute has extra second 61 seconds for a given minute so splits into different rows
    # solution: only keep 00 seconds for a specific minute
    inHMDcsv$HR  = hour(inHMDcsv$dateTime)
    inHMDcsv$MIT = minute(inHMDcsv$dateTime)
    inHMDcsv$SEC = second(inHMDcsv$dateTime)
    #remove any minutes that are not at 00 seconds
    iextra   = which( inHMDcsv$SEC == 0 ) # data to keep
    inHMDcsv2 = inHMDcsv[iextra ,]
    #remove any minutes that are not full 60 seconds
    ikeep = which( inHMDcsv2[,2] > 58 ) # which( inHMDcsv2[,2] < 59)
    inHMDcsv2 = inHMDcsv2[ikeep ,]
    dupTimes = sum( duplicated(inHMDcsv2$dateTime))
    HMDcheck  = rbind( HMDcheck, cbind(basename(inFile), ck, nrow(inHMDcsv), nrow(inHMDcsv2),dupTimes ) ) 
    
    cat("Processing... ",st," on " ,as.character(dy), "[", ii, " of ", length(inHMD)," days ]", ck, " extra minutes","\n" )
    
    ###truncate HMD data ####
    inHMDdata = as.data.frame( inHMDcsv2[, c(1, str:ed )] )
    fq = as.numeric(as.character( gsub("X","", colnames(inHMDdata[2:ncol(inHMDdata)] )) ) ) # Frequency range: truncate to 100-2000 Hz
    
    rm(ed,str, inHMDcsv,inHMDcsv2)
    
    ### (optional) plots spectra ####
    if (pltf == 1) {
      medSPLm = reshape::melt (inHMDdata, id.vars = c("dateTime"),  measure.vars = colnames(inHMDdata)[2:ncol(inHMDdata)] )
      colnames( medSPLm)  = c("date", "Fq", "SPL")
      medSPLm$Fq = as.numeric(as.character( gsub("X","", medSPLm$Fq ) ) ) #head(medSPLm)
      
      ggplot(medSPLm, aes(x = Fq, y=SPL, group = date ) ) +
        geom_line(alpha = .2 ) + 
        scale_x_log10() +
        ylab("1-min HMD")+ xlab("Frequency (Hz)")+
        theme_minimal() +
        ggtitle(paste0( st, " on ", dy) ) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    ### loop through detections ####
    # to find all all minutes that match the detection period
    inHMDdata$Dets = 0
    inHMDdata$Type = "none"
    
    # not all detections as labeled because only 1-day, not whole deployment
    for (bb in 1: nrow(detAll) )
    {
      
      # find all TOLs rows that fall with the detection period
      if ( detAll$DurS [bb] <= 60 )  # if detection is less than a minutes, need different logic... because only one row of data
      {
       ttime = as.POSIXct(  format ( detAll$Start[bb], "%Y-%m-%d %H:%M:00" )  , format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")
        
       idx =  which( inHMDdata$dateTime  == ttime  )
        
      } else {
        idx =  which( inHMDdata$dateTime  >= detAll$Start[bb] & inHMDdata$dateTime + 60 < detAll$End[bb] )
      }
      
      if (length(idx) > 0 ) 
      {
        inHMDdata$Dets[idx] =  inHMDdata$Dets[idx] + 1  #can be overlapping because all detections!
        inHMDdata$Type[idx] =  paste( inHMDdata$Type[idx], detAll$Type[bb] , sep = ";") # keep track of types
      }
      
    }
    cat("Detection types present: ", length( unique( inHMDdata$Type ) )-1 ,"\n" )
    
    ##ACOUSTIC SCENE LABELS ####
    inHMDdata$Category = "Ambient"
    inHMDdata$Bio = 0
    inHMDdata$Bio[grepl("bio", inHMDdata$Type)] = "1" # rows with bio
    inHMDdata$Ant = 0
    inHMDdata$Ant[grepl("anthro", inHMDdata$Type)] = "1" # rows with bio
    
    inHMDdata$Category[inHMDdata$Ant > 0  & inHMDdata$Bio > 0] = "Bio+Anthro"
    inHMDdata$Category[inHMDdata$Ant > 0  & inHMDdata$Bio == 0] = "Anthro"
    inHMDdata$Category[inHMDdata$Ant == 0 & inHMDdata$Bio > 0 ] = "Bio"
    #as.data.frame(colnames((inHMDdata) ))
    #unique( inHMDdata$Category )
    
    ## CALCULATE third-octave band ####
    str125 = which(fq == 112)+1      
    colnames(inHMDdata)[str125]
    ed124 =  which(fq == 141)+1    
    colnames(inHMDdata)[ed124]
    tol = inHMDdata[,str125: ed124]
   # getBandSquaredSoundPressure - this function sums squared sound pressures to determine the in-band totals. 
    inHMDdata$TOL_125_SPL = 10*log10 (rowSums( 10^(tol/10) ) )
   # getBandSquaredSoundPressure - this function sums squared sound pressures to determine the in-band totals. 
    inHMDdata$TOL_125_PSD = 10*log10 (rowSums( 10^(tol/10) )/ncol(tol) )
    
    ## ADD AIS TRANSIT ####
    # MATCH AIS transit with TOL minute data
    inHMDdata$AIS = 0
    inHMDdata$AISs = 0
    inHMDdata$AISm = 0
    inHMDdata$AISl = 0
    inHMDdata$AISu = 0
    inHMDdata$AISsogs = -999 # all
    inHMDdata$AISLsogs = -999 # large
    inHMDdata$AISOsogs = -999 # other
    
    for (aa in 1:nrow ( AIStranOC ) ){  # aa = 344
      
      # find all TOLs rows that fall with the AIS vessel transit period
      idx =  which( inHMDdata$dateTime >= AIStranOC$Start[aa] & inHMDdata$dateTime <= AIStranOC$End[aa] )
      
      for (x in 1:length(idx)) {
        
        # counts, if AIS transit was already added to the TOL
        inHMDdata$AIS[idx[x]] = inHMDdata$AIS[idx[x]] + 1
        
        # vessel size categories
        if(AIStranOC$loa[aa] < 50){
          inHMDdata$AISs[idx[x]] = inHMDdata$AISs[idx[x]] + 1  
          inHMDdata$AISOsogs[idx[x]] = paste(inHMDdata$AISsogs[idx[x]], AIStranOC$avg_sog_dw[aa], sep = ";")} #small
        
        if(AIStranOC$loa[aa] >= 50 & AIStranOC$loa[aa] < 100){ 
          inHMDdata$AISm[idx[x]] = inHMDdata$AISm[idx[x]] + 1  
          inHMDdata$AISOsogs[idx[x]] = paste(inHMDdata$AISsogs[idx[x]], AIStranOC$avg_sog_dw[aa], sep = ";")} #medium
        
        if(AIStranOC$loa[aa] >= 100){ 
          inHMDdata$AISl[idx[x]] = inHMDdata$AISl[idx[x]] + 1 
          inHMDdata$AISLsogs[idx[x]] = paste(inHMDdata$AISsogs[idx[x]], AIStranOC$avg_sog_dw[aa], sep = ";")
          } #large
        
        if(is.na(AIStranOC$loa[aa]) ){ 
          inHMDdata$AISu[idx[x]] = inHMDdata$AIS[idx[x]] + 1  }  #unknown
        
        #all vessel speeds
        inHMDdata$AISsogs[idx[x]] = paste(inHMDdata$AISsogs[idx[x]], AIStranOC$avg_sog_dw[aa], sep = ";")
        
        
        
      }
    }
    
    
    ## LABEL ####
    inHMDdata$mth = month(inHMDdata$dateTime)
    inHMDdata$Category2[inHMDdata$AIS > 0  & inHMDdata$Ant > 0 ] =    "A. Vessel- AIS nearby"  # ais + detection
    inHMDdata$Category2[inHMDdata$AIS == 0 & inHMDdata$Ant > 0 ] =    "B. Vessel- unk"         # no ais but detection
    inHMDdata$Category2[inHMDdata$AIS > 0  & inHMDdata$Ant == 0] =    "C. Ambient- AIS nearby" # ais but no detection-- why??
    inHMDdata$Category2[inHMDdata$AIS == 0 & inHMDdata$Ant == 0] =    "D. Ambient"
    as.data.frame( inHMDdata %>% group_by(Category2, mth) %>% tally() )
    
    inHMDdata$Label2[inHMDdata$Ant > 0 ] =    "A. Vessel Detection" 
    inHMDdata$Label2[inHMDdata$Ant == 0 ] =   "B. Ambient"
    
    as.data.frame( inHMDdata %>% group_by(Label2, mth) %>% tally() )
    
    inHMDdata$Slow = "No" 
    inHMDdata$Slow[inHMDdata$mth == 3 | inHMDdata$mth == 4  ] =    "Slowdown" 
    
    
    ## WRITE OUT DAILY FILES ####
    inTmp = gsub(".csv","",basename(inFile))
    write.csv(inHMDdata , paste0(dirOut,"\\", inTmp, "_LFAS_AIS.csv" ) )
    
    HMDdet = rbind(HMDdet, inHMDdata)  # all data in one file--- way to big!
  } ## !! end a daily loop
  
  ## WRITE OUT ALL DATA ####
  DC = Sys.Date()
  colnames(HMDcheck) = c("FileName","MissingMins","secondsInFile_all","secondsInFile","DuplicatedTimes")
  write.csv(HMDcheck , paste0(dirOut,"\\", st,"_",dpl, "_HMDcheck_", ver,  "_", DC, ".csv" ) )
  save(HMDdet, file = paste0(dirOut, "\\HMDdetLF_", st, "_", dpl , "_", DC, "_", ver, ".Rda") )
  
}


unique( HMDdet$Type )
unique( HMDdet$Bio )
unique( HMDdet$Category )
