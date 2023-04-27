# Final Project

### Graph for offense level and race

# Load package
library(dplyr)
library(lubridate)
library(ggplot2)
library(Rmisc)

## Load cleaned data
load("./data/cleaned_data.Rdata")


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

p1 <- ggplot(offense_level_race, aes(x=suspect_race, y=n, fill=offense_level)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("The Number of Offense") + 
  guides(fill=FALSE)

p2 <- ggplot(offense_level_race, aes(x=suspect_race, y=n, fill=offense_level)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("The Probability of Offense")

# png(file='./figures/offense_level_race.png', width=1500, height=500, res=100)
multiplot(p1, p2, cols=2)
# dev.off()






