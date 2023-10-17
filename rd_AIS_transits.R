AIStran = read.csv( paste0(topDir, "analysis\\AIS\\AIS_transits\\smp_transit_data.csv") ) 
AIStranOC = AIStran[ AIStran$loc_id == site,]
AIStranOC$Start = as.POSIXct( gsub("[+]00", "", AIStranOC$start_time_utc), tz = "GMT" ) 
AIStranOC$End   = as.POSIXct( gsub("[+]00", "", AIStranOC$end_time_utc), tz = "GMT" ) 
min( AIStranOC$Start)
max( AIStranOC$Start)

rm(AIStran)