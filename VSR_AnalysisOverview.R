# Vessel Speed Reduction 
# SB03- 2019, 2020, 2021, 2022 (no AIS, yet)

topDir =  "F:\\SanctSound\\"
site = "SB03"

# 1. Match HMD with vessel information (detections and AIS) + truncate to low-frequency
# F:/CODE/GitHub/SoundscapeScenes/1_HMDwithDets_DayOut_SB03.R
# input: "MinRes.csv" , AIS summaries
# output: HMDdetLF_ each deployment

# 2. Match with WIND
# CODE: F:/CODE/GitHub/SoundscapeScenes/1_HMD_VSR.R
# input :  "HMDdetLF_", ERDAP_wind
# output : VSRdata.R

# 3.1 ECDF noise conditions by category + participation
# CODE: F:/CODE/GitHub/MM_SanctSound_VesselNoise/process_VSR_participation.R
# input : "VSRdata"
# output : 

# 3.2 Noise exceedance analysis + graphics
# CODE: F:/CODE/GitHub/MM_SanctSound_VesselNoise/process_VesselDetections_HMD-1min_VSR_SB03.R
# input : "VSRdata", smp_transit_data.csv, "*hips.csv"
# output : outputVD_site, outputVDne_, plots






