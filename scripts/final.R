# Final Project

# Load package
library(dplyr)
library(lubridate)
library(ggplot2)

## Load cleaned data
load("./data/cleaned_data.Rdata")


rownum = which(complaint_data$premises == "street")
table(complaint_data$specific_location[rownum])


