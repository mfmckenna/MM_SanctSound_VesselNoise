# open netCDF file

# SanctSound ships

library(ncdf4)
ncfile <- "F:\\SanctSound\\SB03_17\\detections\\SanctSound_SB03_17_ships.nc"
nc <- nc_open(ncfile)
nc$var$ships_presence$
  
  
  variable_name <- ncvar_get(nc, "variable_name")