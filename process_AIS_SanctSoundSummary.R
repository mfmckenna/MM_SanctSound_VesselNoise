# AIS summaries for VSR noise reduction co-benefit

rm(list=ls())

sOI = c("SB03", "OC02")

# AIS transits ####
# F:\\SanctSound\\data\\AIS_transits #smp_transit_data.csv 
AIStran = read.csv("F:\\SanctSound\\data\\AIS_transits\\smp_transit_data.csv") 
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
