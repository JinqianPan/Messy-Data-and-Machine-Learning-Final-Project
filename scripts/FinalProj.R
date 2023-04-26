# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ROCR)
library(ggplot2)

### set the seed to 1234
set.seed(1234)

# Data cleaning
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
  rename(occurrence_start_date = CMPLNT_FR_DT, 
         occurrence_start_time = CMPLNT_FR_TM, 
         occurrence_finish_date = CMPLNT_TO_DT,
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

### We start from time
### How many NA in the time
length(which(all_data$occurrence_start_date == "")) # 655
length(which(all_data$occurrence_start_time == "")) # 48
length(which(all_data$occurrence_finish_date == "")) # 1744294
length(which(all_data$occurrence_finish_time == "")) # 1739479
length(which(all_data$occurrence_finish_time == "" & 
               all_data$occurrence_finish_date == "")) # 1738432
length(which(all_data$occurrence_finish_time == "" | 
               all_data$occurrence_finish_date == "")) # 1745341
length(which((all_data$occurrence_finish_time == "" & 
                all_data$occurrence_finish_date != "") | 
               (all_data$occurrence_finish_time != "" & 
                  all_data$occurrence_finish_date == ""))) # 6909

### Remove the NA of occurrence_start_date and occurrence_start_time
### Remove the rows only have occurrence_finish_time or occurrence_finish_date
all_data <- all_data %>%
  filter(occurrence_start_date != "", all_data$occurrence_start_time != "") %>%
  filter(!((occurrence_finish_time == "" & occurrence_finish_date != "") |
           (occurrence_finish_time != "" & occurrence_finish_date == "")))

### How many NA in the occurrence precinct
length(which(is.na(tmp1$occurrence_precinct)))
### Remove the NA of occurrence_precinct
all_data <- all_data %>%
  filter(!is.na(occurrence_precinct))

### Transfer time to Date data type
### Add the occurrence_year and record whether have occurrence_finish_time
all_data <- all_data %>%
  mutate(occurrence_start_time = paste(occurrence_start_date, 
                                       occurrence_start_time),
         occurrence_finish_time = paste(occurrence_finish_date, 
                                        occurrence_finish_time),
         finish_time = ifelse(occurrence_finish_time==" ", 0, 1),
         occurrence_finish_time = ifelse(occurrence_finish_time==" ", 
                                         occurrence_start_time,
                                         occurrence_finish_time),
         occurrence_start_time = mdy_hms(occurrence_start_time, tz="EST"),
         occurrence_finish_time = mdy_hms(occurrence_finish_time, tz="EST"),
         report_police_date = mdy(report_police_date),
         occurrence_year = year(occurrence_start_time)) %>%
  select(-occurrence_start_date, -occurrence_finish_date)

table(all_data$occurrence_year)
all_data <- all_data %>%
  filter(occurrence_year >= 2006)
###
# gender, race, age
all_data <- all_data %>% 
  mutate(suspect_sex = case_when(suspect_sex=="F" ~ "female", 
                                 suspect_sex=="M" ~ "male",
                                 suspect_sex=="D" ~ "diverse", 
                                 suspect_sex=="E" ~ "gender free",
                                 suspect_sex=="U" ~ "unknown", 
                                 suspect_sex=="" ~ "unknown"),
         victim_sex = case_when(victim_sex=="F" ~ "female", 
                                victim_sex=="M" ~ "male",
                                victim_sex=="D" ~ "diverse", 
                                victim_sex=="E" ~ "gender free",
                                victim_sex=="U" ~ "unknown", 
                                victim_sex=="" ~ "unknown"),
         suspect_race = tolower(suspect_race),
         victim_race = tolower(victim_race),
         suspect_age = tolower(suspect_age),
         victim_age = tolower(victim_age),
         suspect_race = ifelse(suspect_race == "", "unknown", suspect_race),
         victim_race = ifelse(victim_race == "", "unknown", victim_race),
         suspect_age = ifelse(suspect_age == "", "unknown", suspect_age),
         victim_age = ifelse(victim_age == "", "unknown", victim_age))

# offense_desc, pd_desc, completed_or_attempted, 
# offense_level, borough_name, premises
all_data <- all_data %>% 
  mutate(offense_level = tolower(offense_level),
         offense_desc = tolower(offense_desc),
         offense_desc = ifelse(offense_desc == "", "unknown", offense_desc),
         pd_desc = tolower(pd_desc),
         pd_desc = ifelse(pd_desc == "", "unknown", pd_desc),
         completed_or_attempted = tolower(completed_or_attempted),
         completed_or_attempted = ifelse(completed_or_attempted == "", 
                                         "unknown", 
                                         completed_or_attempted),
         borough_name = tolower(borough_name),
         borough_name = ifelse(borough_name == "", "unknown", borough_name),
         specific_location = tolower(specific_location),
         specific_location = ifelse(specific_location == "", 
                                    "unknown", 
                                    specific_location),
         premises = tolower(premises),
         premises = ifelse(premises == "", "unknown", premises),
         jurisdiction = tolower(jurisdiction),
         patrol_borough = tolower(patrol_borough),
         patrol_borough = ifelse(patrol_borough == "", 
                                 "unknown", 
                                 patrol_borough),)

##### Unknown
all_data_5un <- all_data[rowSums(all_data[, 5:23] == "unknown") <= 5, ]
nrow(all_data) - nrow(all_data_5un) # 516691

colnames(all_data_5un)
## Split data
lon_lat_data <- all_data_5un[, c("lot", "lat")]

columns <- c("occurrence_year", "occurrence_start_time", 
             "occurrence_finish_time", "finish_time", "report_police_date", 
             "occurrence_precinct", "offense_desc", 
             "pd_desc", "completed_or_attempted", 
             "offense_level", "jurisdiction", 
             "borough_name", "patrol_borough", "specific_location", "premises",
             "suspect_age", "suspect_race", "suspect_sex",
             "victim_age", "victim_race", "victim_sex")
complaint_data <- all_data_5un[, columns]


### Save to Rdata
save(complaint_data, lon_lat_data, file="final.RData")










