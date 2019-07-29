#####################
Script Section 1
#####################

library(tidyverse)
library(ggplot2)
library(readr)
library(lubridate)

## READ IN##
Parking_citations_raw <- read_csv("Parking_Citations.csv")

names(Parking_citations_raw)
str(Parking_citations_raw)

## Separate date and time to easi id 2019 dates
Parking_citations <- separate(Parking_citations_raw,
                              ViolDate, into = c("ViolDate", "ViolTime", "AM_PM"),
                              sep = " ")
# unite AM/PM with time (previously separated due to above call)
Parking_citations <- unite(Parking_citations, col = "ViolTime",
c("ViolTime", "AM_PM"), sep = " ", remove = T)

## Convert Violdate to date format
Parking_citations$ViolDate <- mdy(Parking_citations$ViolDate)

tail(Parking_citations$ViolDate)
min(Parking_citations$ViolDate)
max(Parking_citations$ViolDate)
class(Parking_citations$ViolDate)
min(Parking_citations$ViolDate, na.rm = T)
max(Parking_citations$ViolDate, na.rm = T)

## id how many violations are 2019 - and whats the earliest day.
Parking_citations_2 %>%
  filter(grepl("2019", Parking_citations$ViolDate))

# subset out 2019 violations
Parking_citations_3 <- subset(Parking_citations, ViolDate < "2019-01-01")
head(Parking_citations_3$ViolFine, 40)
head(Parking_citations_3$ViolFine, 100)
tail(Parking_citations_3$ViolFine, 100)

# Mean violation 
mean(Parking_citations_3$ViolFine) #--> 49.16335
