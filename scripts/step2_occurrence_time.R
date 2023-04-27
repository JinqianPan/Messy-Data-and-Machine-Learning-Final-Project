# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ggplot2)

## Load cleaned data
load("./data/cleaned_data.Rdata")


### 
occurrence_time <- complaint_data %>%
  mutate(occurrence_hour = hour(occurrence_start_time),
         occurrence_month = month(occurrence_start_time)) %>%
  select(occurrence_year, occurrence_month, occurrence_hour) %>%
  rename(year = occurrence_year,
         month = occurrence_month, 
         hour = occurrence_hour)

#####
occurrence_year_hour <- occurrence_time %>%
  group_by(year, hour) %>%
  summarise(n = n())
  
p1 <- ggplot(occurrence_year_hour, aes(x=hour, y=n)) +
  geom_line() +
  facet_wrap(~year) +
  labs(y="Number of Complaint", x="Hour")
p1

# ggsave(p1,
#        filename='./figures/complaint_over_hour.png',
#        width=15,
#        height=10)

#####
occurrence_year_month <- occurrence_time %>%
  group_by(year, month) %>%
  summarise(n = n())

p2 <- ggplot(occurrence_year_month, aes(x=month, y=n)) +
  geom_line() +
  facet_wrap(~year) + 
  labs(y="Number of Complaint", x="Month")
p2

# ggsave(p2,
#        filename='./figures/complaint_over_month.png',
#        width=15,
#        height=10)
