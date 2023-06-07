# using hourly or daily values to make Figure 2 in vessel manuscript

keep = as.data.frame ( rbind(
  c("SB01",03,"Y"), c("SB02",03,"Y"), c("SB03",03,"Y"),
  c("PM08",1,"N") , c("HI03",1,"N"),  c("HI01",1,"N"), c("HI04",12,"N"),
  c("OC04",08,"N"), c("OC03",11,"N"), c("OC02",03,"Y"), c("OC01",11,"N"),
  c("MB03",03,"Y"), c("MB02",03,"N"), c("MB01",03,"N"),
  c("GR03",5,"N"), c("GR02",5,"N"), c("GR01",5,"N"),
  c("FK04",3,"N"), c("FK03",1,"N"),  c("FK02",1,"N") , c("FK01",1,"N"),
  c("CI05",4,"Y"),  c("CI04",4,"N"),  c("CI03",11,"N"), c("CI02",6,"N"),   c("CI01",4,"N")) )
colnames(keep) = c("Site","Mth","ShipLane") 
keep$Mth =  as.numeric( as.character(keep$Mth) )
keep$Site = as.character(keep$Site) 
keep$ShipLane = as.character(keep$ShipLane)

outputKeep = NULL
for (kk in 1:nrow(keep)){
  tmp = output4[ output4$Site == keep[kk,1] & output4$Mth == keep[kk,2],] 
  tmp$ShipLane = keep[kk,3]
  outputKeep = rbind(outputKeep,tmp)
}
colnames(outputKeep) = (c(colnames(output4),"ShipLane") )
outputKeep$Site = as.character(outputKeep$Site ) 

# HOW DID I CALCULATE these values!!!

# SB03_TOLVesselDetections_HR_2018-11-12to2021-07-27_ver2022-11-23
# SB03_SPLVesselDetectionsAIS_Day_2018-11-12to2020-11-30_ver2022-11-23

wd = "C:\\Users\\megan\\Documents\\combineFiles_VesselManuscript\\" # "F:\\SanctSound\\data2\\combineFiles\\"  # Summary2019Month_ver2022-01-21.csv
setwd(wd)
infile = paste0(wd,"SB03_SPLVesselDetectionsAIS_Day_2018-11-12to2020-11-30_ver2022-11-23.csv") #choose.files() #output from 1b_process_VesselDetectionsAIS.R paste0(wd,"Summary 2019Month_ver2022-01-21.csv")
outputDay = read.csv(infile)
as.data.frame(colnames(outputDay))
outputDay$PercentVessel_daily
outputDay$Day
