# Setup ----
## Load packages ----
library(data.table)
library(dtplyr)
library(dplyr)

## Load data ----
stations <- read.csv("https://noaa-isd-pds.s3.amazonaws.com/isd-history.csv")
stations$USAF <- as.integer(stations$USAF)

# Dealing with blanks and 999999
stations$USAF[stations$USAF == 999999] <- NA
stations$CTRY[stations$CTRY == ""] <- NA
stations$STATE[stations$STATE == ""] <- NA

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, c('USAF', 'CTRY', 'STATE')])

# Dropping NAs
stations <- stations[!is.na(stations$USAF), ]

# Removing duplicates
stations <- stations[!duplicated(stations$USAF), ]

