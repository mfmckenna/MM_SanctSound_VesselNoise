# VSR data integration

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
rm(list=ls()) 

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
#GET DATA ####
site1 = "OC02"
siteNot = "SB03"

# HMD+
inDir = (  paste0("F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction\\", site1 ))
outDir = "F:\\SanctSound\\analysis\\combineFiles_VesselSpeedReduction"
inFiles = list.files( inDir, pattern = "HMDdetLF", full.names = T)
inFiles = inFiles[!grepl("SB03", inFiles)] #remove 1 day files
#inFiles = inFiles[grepl("2023-10-15", inFiles)] #remove 1 day files

pltf = 0
fqr  = "LF"  #append this to output names
site = "All"
DC = Sys.Date()

#WIND
inDirW = (  "F:\\SanctSound\\analysis\\ERDAP_wind" )
inFilesW = list.files( inDirW, pattern = "env", full.names = T)
inFilesW = inFilesW[!grepl(siteNot, inFilesW)]
WINDdata = read.csv(inFilesW)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
#TRIM DATA ####
VSRdata = NULL
for (f in 1: length(inFiles)) { # f = 6 for testing
  load( inFiles[f])
  Depl =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 3) #site name
  HMDdet$Depl = Depl
  st =  sapply(strsplit(basename( inFiles[f]), "_"), "[[", 2) #site name
  HMDdet$Site = st

  tmp = HMDdet[,c(1,1000:1018)]
  head(tmp)
  
  VSRdata = rbind (VSRdata, tmp )
}
head(VSRdata)

#_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
# ADD WIND SPEED TO MINUTE DATA
as.data.frame(colnames( WINDdata) )
ixd = which(!is.na(WINDdata$wind_speed))
WINDdata = WINDdata[ixd, ]
WINDdata$time = gsub("[+]00:00", "", WINDdata$time )
WINDdata$dateTime = as.POSIXct( WINDdata$time, format = "%Y-%m-%d %H:%M:%S" , tz = "GMT")

VSRdata$wind_speed = NA
VSRdata$sea_water_temperature = NA
VSRdata$sea_surface_wave_significant_height = NA

 
save(VSRdata,  file= paste0(outDir, "\\VSRdata_" ,site1, "_", DC, ".Rda") )
head(VSRdata)

