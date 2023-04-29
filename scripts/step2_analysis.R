# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ggplot2)
library(Rmisc)

## Load cleaned data
load("./data/cleaned_data.Rdata")


### 
occurrence_time <- complaint_data %>%
  mutate(occurrence_hour = hour(occurrence_start_time),
         occurrence_month = month(occurrence_start_time)) %>%
  select(occurrence_year, occurrence_month, occurrence_hour) %>%
  dplyr::rename(year = occurrence_year,
                month = occurrence_month, 
                hour = occurrence_hour)

##### 2006-2019 24 hours vs Complaint Number
occurrence_year_hour <- occurrence_time %>%
  group_by(year, hour) %>%
  dplyr::summarise(n = n())
  
p1 <- ggplot(occurrence_year_hour, aes(x=hour, y=n)) +
  geom_line() +
  facet_wrap(~year) +
  labs(y="Number of Complaint", x="Hour")
p1

# ggsave(p1,
#        filename='./figures/complaint_over_hour.png',
#        width=10,
#        height=5)

##### 2006-2019 12 months vs Complaint Number
occurrence_year_month <- occurrence_time %>%
  group_by(year, month) %>%
  dplyr::summarise(n = n())

p2 <- ggplot(occurrence_year_month, aes(x=month, y=n)) +
  geom_line() +
  facet_wrap(~year) + 
  labs(y="Number of Complaint", x="Month")
p2
# 
# ggsave(p2,
#        filename='./figures/complaint_over_month.png',
#        width=10,
#        height=5)

##### Offense level vs borough_name/ 24 hour
borough_name_hour_level <- complaint_data %>%
  mutate(occurrence_hour = hour(occurrence_start_time)) %>%
  select(occurrence_hour, borough_name, offense_level) %>%
  filter(borough_name != "unknown") %>%
  group_by(borough_name, offense_level, occurrence_hour) %>%
  dplyr::summarize(count = n())

p3 <- ggplot(borough_name_hour_level, aes(x=occurrence_hour, 
                                          y=count, 
                                          colour=offense_level)) +
  geom_line() +
  facet_wrap(~borough_name) +
  labs(y="Number of Complaint", x="Hour") +
  theme(legend.position = c(0.85, 0.25))
p3

# ggsave(p3,
#        filename='./figures/borough_name_hour_level.png',
#        width=10,
#        height=5)

###### Graph for offense level and race
table(complaint_data$offense_level)

rownum <- which(complaint_data$offense_level == "violation")
table(complaint_data$suspect_race[rownum])

rownum <- which(complaint_data$offense_level == "misdemeanor")
table(complaint_data$suspect_race[rownum])

rownum <- which(complaint_data$offense_level == "felony")
table(complaint_data$suspect_race[rownum])

offense_level_race <- complaint_data %>%
  filter(suspect_race != "other", 
         suspect_race != "american indian/alaskan native",
         suspect_race != "unknown") %>%
  select(offense_level, suspect_race) %>%
  group_by(offense_level, suspect_race) %>%
  dplyr::summarise(n = n())

differernt_race <- complaint_data %>%
  filter(suspect_race != "unknown",
         victim_race != "unknown") %>%
  mutate(drace = 
           ifelse(suspect_race == victim_race, "same race", "diff race")) %>%
  select(offense_level, drace) %>%
  group_by(offense_level, drace) %>%
  dplyr::summarise(n = n())

p4 <- ggplot(offense_level_race, aes(x=suspect_race, y=n, fill=offense_level)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("The Number of Offense") + 
  xlab("Suspect Race") +
  guides(fill=FALSE)

p5 <- ggplot(offense_level_race, aes(x=suspect_race, y=n, fill=offense_level)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("The Probability of Offense") +
  xlab("Suspect Race") +
  guides(fill=FALSE)

p6 <- ggplot(differernt_race, aes(x=drace, y=n, fill=offense_level)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("The Number of Offense") +
  xlab("Whether Suspect And Victim Same Race")

# png(file='./figures/offense_level_race.png', width=1500, height=500, res=100)
multiplot(p4, p5, p6, cols=3)
# dev.off()

