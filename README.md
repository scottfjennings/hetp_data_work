# hetp_data_work
several files with code to manage, manipulate, visualize and analyze data from the ACR Heron and Egret Telemetry Project



## code
the few files that might be regularly used prior to formal analysis or further plotting are:  

* interpolate_bird_tides.R - determine the predicted tidal height (at Blake's Landing) at the timestamp of each bird GPS location. NOTE: this relies on code external to hetp_data_work (C:\Users\scott.jennings\Documents\Projects\water_levels\tides\code\make_subord_station_predictions.R)  

* add_covariates.R - create some covariates and merge with hetp data  

* calculate_ODBA.R - calculate Overall Dynamic Body Acceleration from Movebank format Accelerometer data  

* clip_write_data_files.R - separate full downloaded data (from Movebank) into 1 Bird X 1 month chunks; these chunks are a little easier for my laptop to work with than the full data sets.


## workflows  

* calculate ODBA:    
  1. create monthly RDS files and summaries with clip_write_data_files.R  
  2. do actual calculations with calculate_ODBA.R  
  3. add_covariates.R to combine ODBA with GPS data
  
* add tide level, daylight and other covariates to GPS data:  
  1. interpolate_bird_tides.R
  2. add_covariates.R