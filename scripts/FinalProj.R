# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ROCR)
library(ggplot2)

### set the seed to 1234
set.seed(1234)

### read data
all_data <- read.csv("./data/NYPD_Complaint_Data_Historic.csv")

### Drop columns from all_data
all_data <- all_data %>%
  select(-CMPLNT_NUM, -PD_CD, -JURISDICTION_CODE, -KY_CD, -PARKS_NM,
         -Lat_Lon, -STATION_NAME, -HOUSING_PSA, -HADEVELOPT, 
         -X_COORD_CD, -Y_COORD_CD,
         -TRANSIT_DISTRICT)
nrow(all_data) # 7825499
ncol(all_data) # 23

### rename columns (new_name: ORIGINAL_NAME):
all_data <- all_data %>%
  rename(occurrence_start_data = CMPLNT_FR_DT, 
         occurrence_start_time = CMPLNT_FR_TM, 
         occurrence_finish_data = CMPLNT_TO_DT,
         occurrence_finish_time = CMPLNT_TO_TM, 
         occurrence_precinct = ADDR_PCT_CD, 
         report_police_date = RPT_DT, 
         offense_desc= OFNS_DESC,
         pd_desc= PD_DESC,
         completed_or_attempted = CRM_ATPT_CPTD_CD,
         offense_level = LAW_CAT_CD, 
         borough_name = BORO_NM, 
         specific_location = LOC_OF_OCCUR_DESC,
         premises = PREM_TYP_DESC,
         jurisdiction = JURIS_DESC,      
         suspect_age = SUSP_AGE_GROUP,
         suspect_race =  SUSP_RACE,   
         suspect_sex = SUSP_SEX,        
         lat = Latitude,
         lot =  Longitude,    
         patrol_borough = PATROL_BORO,
         victim_age = VIC_AGE_GROUP,
         victim_race =  VIC_RACE,
         victim_sex =  VIC_SEX)

tmp1 <- all_data %>%
  mutate(occurrence_start_time = paste(occurrence_start_data, 
                                       occurrence_start_time),
         occurrence_finish_time = paste(occurrence_finish_data, 
                                        occurrence_finish_time))







