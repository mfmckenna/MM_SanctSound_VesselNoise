# AIS summaries for VSR noise reduction co-benefit

rm(list=ls())

library(dplyr)
library(lubridate)

sOI = c("SB03", "OC02")

# AIS transits ####
# F:\\SanctSound\\data\\AIS_transits #smp_transit_data.csv 
AIStran = read.csv("F:\\SanctSound\\analysis\\AIS\\AIS_transits\\smp_transit_data_updateFormat.csv") 
AIStran$Start = as.POSIXct( gsub("[+]00", "", AIStran$start_time_utc), tz = "GMT" ) 
AIStran$End   = as.POSIXct( gsub("[+]00", "", AIStran$end_time_utc), tz = "GMT" ) 
AIStran$Year = year(AIStran$Start )
AIStran$Mth = month(AIStran$Start )
AIStran$Yr_Mth = paste(AIStran$Year , AIStran$Mth, sep = "_" )

## SB03: summarize vessel info by month ####
AIStranSB = AIStran[ AIStran$loc_id == sOI[1],]
# Total transit by year-month
#copied to https://docs.google.com/spreadsheets/d/10SviwvcmT3XlaYuDfpQRLVs9heEVo0EsUSqp-hpbocY/edit#gid=583051745
outTotal = as.data.frame( AIStranSB %>% group_by(Yr_Mth) %>% tally() )

# By type
outType  = as.data.frame( AIStranSB %>% group_by(Yr_Mth, type) %>% tally() )
outType[ outType$type == "Cargo",]
outType[ outType$type == "Tanker",]

# By speed
as.data.frame( AIStranSB[ AIStranSB$avg_sog_dw >10,] %>% group_by(Yr_Mth) %>% tally() )
as.data.frame( AIStranSB[ AIStranSB$avg_sog_dw <=10,] %>% group_by(Yr_Mth) %>% tally() )

## OC02: summarize vessel info by month ####
AIStranSB = AIStran[ AIStran$loc_id == sOI[2],]
# Total transit by year-month
#copied to https://docs.google.com/spreadsheets/d/10SviwvcmT3XlaYuDfpQRLVs9heEVo0EsUSqp-hpbocY/edit#gid=583051745
outTotal = as.data.frame( AIStranSB %>% group_by(Yr_Mth) %>% tally() )

# By type
outType  = as.data.frame( AIStranSB %>% group_by(Yr_Mth, type) %>% tally() )
outType[ outType$type == "Cargo",]
outType[ outType$type == "Tanker",]

# By speed
as.data.frame( AIStranSB[ AIStranSB$avg_sog_dw >10,] %>% group_by(Yr_Mth) %>% tally() )
as.data.frame( AIStranSB[ AIStranSB$avg_sog_dw <=10,] %>% group_by(Yr_Mth) %>% tally() )

# AIS 10 km summaries ####
# F:\\SanctSound\\data\\AIS_By_Region # AIS_CI_2018_10_to_2021_04.csv
ss = 2
tDir = "F:\\SanctSound\\analysis\\AIS\\AIS_By_Region\\"
inFiles = ( list.files(path=tDir, pattern = substr( sOI[ss],1,2) , full.names=TRUE, recursive = TRUE) )

AISsum = read.csv(inFiles[4]  )

AISsum = AISsum[ AISsum$LOC_ID == sOI[ss],]

AISsum$DATE = as.Date( AISsum$DATE, format = "%m/%d/%Y") 
AISsum$Year = year(AISsum$DATE )
AISsum$Mth = month(AISsum$DATE )
AISsum$Yr_Mth = paste(AISsum$Year , AISsum$Mth, sep = "_" )

head(AISsum)
# get monthly summaries of unique vessels in each size class- S, M, L NA
AISsumt = AISsum[,c(13, 3,5,7,9)]
head(AISsumt)
AISsumUV = as.data.frame ( AISsumt %>% group_by(Yr_Mth) %>% summarise(across(everything(), list(sum))) )
AISsumUV$Total = rowSums(AISsumUV[,2:5])
AISsumUV$perLarge = (AISsumUV$LOA_L_UV_1/AISsumUV$Total) *100
AISsumUV
#copied to https://docs.google.com/spreadsheets/d/10SviwvcmT3XlaYuDfpQRLVs9heEVo0EsUSqp-hpbocY/edit#gid=583051745
