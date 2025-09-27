#  1) Reading in Data
met <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Lab/Lab 3/data/met_all.gz")


#  2) Dimensions, headers, footers
dim(met)
head(met)
tail(met)

?dim

ncol(met)
nrow(met)


#  3) Variables
str(met)


# 4) Key Variables
table(met$year)
table(met$day)
table(met$hour)
summary(met$temp)
summary(met$elev)
summary(met$wind.sp)

met$elev[met$elev==9999.0] <- NA
summary(met$elev)



met <- met[met$temp > -40, ]
met2 <- met[order(met$temp), ]
head(met2)


-----------

table(met$year)
table(met$day)
table(met$hour)
summary(met$temp)
summary(met$elev)
summary(met$wind.sp)

## Fixing Missing Data
met[met$elev==9999.0] <- NA
summary(met$elev)

### Different Code
met$elev[met$elev == 9999.0] <- NA
summary(met$elev)


## Min. Temp Data
met <- met[temp>-40]
met2 <- met[order(temp)]
head(met2)

variable.names(met)

### Different Code
met <- met[met$temp>-40, ]
met2 <- met[order(met$temp), ]
head(met2)

any(met$temp == -40)  # Should return FALSE

summary(met)

# 5) Change Temp Min.
met <- met[met$temp>-15, ]
met2 <- met[order(met$temp), ]
head(met2)


----------

met <- met[met$temp>-15, ]
met2 <- met[order(met$temp), ]
head(met2)


# 6) Summary Statistics
elev <- met[met$elev==max(met$elev, na.rm=TRUE), ]
summary(elev)

cor(elev$temp, elev$wind.sp, use="complete")
cor(elev$temp, elev$hour, use="complete")
cor(elev$wind.sp, elev$day, use="complete")
cor(elev$wind.sp, elev$hour, use="complete")
cor(elev$temp, elev$day, use="complete")


# 7) Exploratory graphs
hist(met$elev, breaks=100)
hist(met$temp)
hist(met$wind.sp)

## Location of weather station with highest elevation
library(leaflet)
leaflet(elev) %>%
  addProviderTiles('OpenStreetMap') %>% 
  addCircles(lat=~lat,lng=~lon, opacity=1, fillOpacity=1, radius=100)

## Time series of temperature and wind speed
library(lubridate)
elev$date <- with(elev, ymd_h(paste(year, month, day, hour, sep= ' ')))
summary(elev$date)
elev <- elev[order(date)]
head(elev)

plot(elev$date, elev$temp, type='l')
plot(elev$date, elev$wind.sp, type='l')

### Different Code
library(lubridate)
elev$date <- with(elev, ymd_h(paste(year, month, day, hour, sep= ' ')))
summary(elev$date)
elev <- elev[order(elev$date), ]
head(elev)

plot(elev$date, elev$temp, type='l')
plot(elev$date, elev$wind.sp, type='l')

