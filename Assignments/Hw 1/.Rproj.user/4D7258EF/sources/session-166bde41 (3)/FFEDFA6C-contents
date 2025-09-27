# All things loaded ----

# Load datasets
data_2002 <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Assignments/Hw 1/data/2002.csv")
data_2022 <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Assignments/Hw 1/data/2022.csv")

# Add Year column
data_2002$Year <- 2002
data_2022$Year <- 2022

# Rename PM2.5 column for simplicity
names(data_2002)[names(data_2002) == "Daily.Mean.PM2.5.Concentration"] <- "PM25"
names(data_2022)[names(data_2022) == "Daily.Mean.PM2.5.Concentration"] <- "PM25"

# Combine datasets
combined_data <- rbind(data_2002, data_2022)

# Load required libraries
library(ggplot2)
library(dplyr)

# Rename latitude and longitude
combined_data <- combined_data %>%
  rename(Lat = Site.Latitude,
         Lon = Site.Longitude)

# Interactive map
library(leaflet)
library(dplyr)

sites_2002 <- combined_data %>% filter(Year == 2002)
sites_2022 <- combined_data %>% filter(Year == 2022)


# Filter for negative PM2.5 values
neg_2002 <- combined_data[combined_data$Year == 2002 & combined_data$PM25 < 0, ]
neg_2022 <- combined_data[combined_data$Year == 2022 & combined_data$PM25 < 0, ]

# Make sure Date is in Date format
neg_2002$Date <- as.Date(neg_2002$Date)
neg_2022$Date <- as.Date(neg_2022$Date)


# Filter for California only
ca_data <- combined_data

# Filter for California counties only
county_data <- ca_data %>% filter(!is.na(County))

## Faceted line plot
combined_data$Date <- as.Date(combined_data$Date)
combined_data$Month <- as.numeric(format(combined_data$Date, "%m"))


# Filter for LA County
la_sites <- ca_data %>% filter(County == "Los Angeles")
la_data <- combined_data %>%
  filter(County == "Los Angeles", PM25 >= 0, !is.na(Date))

la_data <- combined_data %>%
  filter(County == "Los Angeles", PM25 >= 0, !is.na(Date), !is.na(PM25))


install.packages("ggridges")
library(ggridges)

ca_data$Date <- as.Date(ca_data$Date)
ca_data$Month <- factor(format(ca_data$Date, "%m"),
                        levels = sprintf("%02d", 1:12),
                        labels = month.abb)

library(dplyr)
library(tidyr)



-------
# Step 1 ----

# Load datasets

data_2002 <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Assignments/Hw 1/data/2002.csv")
data_2022 <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Assignments/Hw 1/data/2022.csv")

# Dimensions
dim(data_2002)
dim(data_2022)

# Headers and footers
head(data_2002)
tail(data_2002)

head(data_2022)
tail(data_2022)

# Variable names and types
names(data_2002)
str(data_2002)

names(data_2022)
str(data_2022)

# Distribution of PM2.5
summary(data_2002$Daily.Mean.PM2.5.Concentration)
hist(data_2002$Daily.Mean.PM2.5.Concentration,
     main = "PM2.5 Distribution - 2002",
     xlab = "Daily Mean PM2.5",
     col = "lightblue")

summary(data_2022$Daily.Mean.PM2.5.Concentration)
hist(data_2022$Daily.Mean.PM2.5.Concentration,
     main = "PM2.5 Distribution - 2022",
     xlab = "Daily Mean PM2.5",
     col = "lightgreen")

-------
# Step 2 ----

# Add Year column
data_2002$Year <- 2002
data_2022$Year <- 2022

# Rename PM2.5 column for simplicity
names(data_2002)[names(data_2002) == "Daily.Mean.PM2.5.Concentration"] <- "PM25"
names(data_2022)[names(data_2022) == "Daily.Mean.PM2.5.Concentration"] <- "PM25"

# Combine datasets
combined_data <- rbind(data_2002, data_2022)

# Confirm structure
str(combined_data)

# Preview combined data
head(combined_data)
tail(combined_data)

# Summary of PM2.5 by year
summary(combined_data$PM25)

-------
# Step 3 ----

# Load required libraries
library(ggplot2)
library(dplyr)

# Rename latitude and longitude
combined_data <- combined_data %>%
  rename(Lat = Site.Latitude,
         Lon = Site.Longitude)

sites_2002 <- combined_data %>% filter(Year == 2002)
sites_2022 <- combined_data %>% filter(Year == 2022)


# Interactive map: 2002
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = sites_2002,
                   ~Lon, ~Lat,
                   color = "darkblue",
                   radius = 3,
                   label = ~paste("2002 Site:", Local.Site.Name),
                   group = "2002")

# Interactive map: 2022
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = sites_2022,
                   ~Lon, ~Lat,
                   color = "tomato",
                   radius = 3,
                   label = ~paste("2022 Site:", Local.Site.Name),
                   group = "2022")


--------
# Step 4 ----

# Missing values
sum(is.na(combined_data$PM25))

# Negative values (not possible for PM2.5)
sum(combined_data$PM25 < 0)

# Very high values (above 1000 µg/m³)
sum(combined_data$PM25 > 1000)

# Missing values by year
tapply(is.na(combined_data$PM25), combined_data$Year, sum)

# Negative values by year
tapply(combined_data$PM25 < 0, combined_data$Year, sum)

# Very high values by year
tapply(combined_data$PM25 > 1000, combined_data$Year, sum)

# Total observations by year
table(combined_data$Year)

## Temporal obs
# Find rows w/ neg values
neg_2022 <- combined_data[combined_data$Year == 2022 & combined_data$PM25 < 0, ]

# Show the dates
neg_2022$Date

# Count negative values by date
neg_counts <- table(neg_2022$Date)

## Plot
library(ggplot2)

# Filter for negative PM2.5 values
neg_2002 <- combined_data[combined_data$Year == 2002 & combined_data$PM25 < 0, ]
neg_2022 <- combined_data[combined_data$Year == 2022 & combined_data$PM25 < 0, ]

# Make sure Date is in Date format
neg_2002$Date <- as.Date(neg_2002$Date)
neg_2022$Date <- as.Date(neg_2022$Date)


## Hist for 2002
ggplot(neg_2002, aes(x = Date)) +
  geom_histogram(binwidth = 7, fill = "darkred", color = "white") +
  labs(title = "Negative PM2.5 Values in 2002",
       x = "Date", y = "Count") +
  theme_minimal()

## Hist for 2022
ggplot(neg_2022, aes(x = Date)) +
  geom_histogram(binwidth = 30, fill = "darkred", color = "white") +
  labs(title = "Negative PM2.5 Values in 2022",
       x = "Date", y = "Count") +
  theme_minimal()

-------
# Step 5 ----
## Statewide ----
library(ggplot2)
library(dplyr)

# Filter for California only
ca_data <- combined_data

#### Boxplot of PM2.5 by year ----
ggplot(ca_data, aes(x = factor(Year), y = PM25)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "PM2.5 Distribution in California: 2002 vs. 2022",
       x = "Year", y = "Daily PM2.5") +
  theme_minimal()

#### Line plot of monthly averages ----
ca_data$Date <- as.Date(ca_data$Date)
ca_data$Month <- factor(format(ca_data$Date, "%m"),
                        levels = sprintf("%02d", 1:12),
                        labels = month.abb)

ggplot(ca_data, aes(x = Month, y = PM25, group = Year, color = factor(Year))) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "Monthly PM2.5 in California: 2002 vs 2022",
       x = "Month", y = "Mean PM2.5", color = "Year") +
  theme_minimal()

#### Histogram of PM2.5 Values ----
ggplot(ca_data, aes(x = PM25, fill = factor(Year))) +
  geom_histogram(binwidth = 2, alpha = 0.6, position = "identity") +
  labs(title = "PM2.5 Histogram in California",
       x = "Daily PM2.5", fill = "Year") +
  theme_minimal()

#### Obs count ----
ca_data %>%
  filter(PM25 >= 0) %>%
  count(Year) %>%
  ggplot(aes(x = factor(Year), y = n, fill = factor(Year))) +
  geom_col() +
  labs(title = "Valid PM2.5 Observations by Year",
       x = "Year", y = "Count") +
  theme_minimal()


## Summary statistics ----
#### Basic summary ----
ca_data %>%
  group_by(Year) %>%
  summarize(
    count = n(),
    missing = sum(is.na(PM25)),
    negative = sum(PM25 < 0, na.rm = TRUE),
    mean = mean(PM25, na.rm = TRUE),
    median = median(PM25, na.rm = TRUE),
    sd = sd(PM25, na.rm = TRUE),
    min = min(PM25, na.rm = TRUE),
    max = max(PM25, na.rm = TRUE)
  )

#### Summary of extreme values ----
ca_data %>%
  filter(PM25 >= 0) %>%
  group_by(Year) %>%
  summarize(
    above_35 = sum(PM25 > 35, na.rm = TRUE),
    above_55 = sum(PM25 > 55, na.rm = TRUE),
    above_100 = sum(PM25 > 100, na.rm = TRUE)
  )

#### Monthly avgs ----
ca_data$Date <- as.Date(ca_data$Date)
ca_data$Month <- factor(format(ca_data$Date, "%m"),
                        levels = sprintf("%02d", 1:12),
                        labels = month.abb)

ca_data %>%
  mutate(Date = as.Date(Date),
         Month = factor(format(Date, "%m"),
                        levels = sprintf("%02d", 1:12),
                        labels = month.abb)) %>%
  drop_na(PM25, Date) %>%
  filter(PM25 >= 0) %>%
  group_by(Year, Month) %>%
  summarize(monthly_avg = mean(PM25), .groups = "drop") %>%
  arrange(Year, Month) %>%
  print(n = Inf)

#### Obs counts by year ----
ca_data %>%
  filter(PM25 >= 0) %>%
  count(Year)



## County ----
#### Filter for California counties only
county_data <- ca_data %>% filter(!is.na(County))

#### Faceted line plot ----
combined_data$Date <- as.Date(combined_data$Date)
combined_data$Month <- as.numeric(format(combined_data$Date, "%m"))

ggplot(combined_data, aes(x = Month, y = PM25, group = Year, color = factor(Year))) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~County) +
  labs(title = "Monthly PM2.5 Trend by County: 2002 vs 2022",
       x = "Month", y = "Mean PM2.5", color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 14),
        legend.position = "bottom")

#### Ridgeline plot by country and year ----
combined_data %>%
  filter(PM25 >= 0, !is.na(County)) %>%
  ggplot(aes(x = PM25, y = County, fill = factor(Year))) +
  geom_density_ridges(alpha = 0.6) +
  labs(title = "PM2.5 Distribution by County",
       x = "Daily PM2.5", y = "County", fill = "Year") +
  theme_minimal()

#### Observation counts ----
combined_data %>%
  filter(PM25 >= 0, !is.na(County)) %>%
  count(County, Year) %>%
  ggplot(aes(x = n, y = reorder(County, n), fill = factor(Year))) +
  geom_col(position = "dodge") +
  labs(title = "Observation Counts by County",
       x = "Number of Observations", y = "County", fill = "Year") +
  theme_minimal()


## Summary stats ----
#### Basic by year ----
combined_data %>%
  filter(!is.na(PM25), PM25 >= 0, !is.na(County)) %>%
  group_by(County, Year) %>%
  summarise(
    count = n(),
    mean = mean(PM25),
    median = median(PM25),
    sd = sd(PM25),
    min = min(PM25),
    max = max(PM25),
    .groups = "drop"
  ) %>%
  arrange(County, Year) %>%
  print(n = Inf)

#### Extreme values by county ----
combined_data %>%
  filter(!is.na(PM25), PM25 >= 0, !is.na(County)) %>%
  group_by(County, Year) %>%
  summarise(
    above_35 = sum(PM25 > 35),
    above_55 = sum(PM25 > 55),
    above_100 = sum(PM25 > 100),
    .groups = "drop"
  ) %>%
  arrange(County, Year) %>%
  print(n = Inf)

#### Obs count by county ----
combined_data %>%
  filter(!is.na(PM25), PM25 >= 0, !is.na(County)) %>%
  count(County, Year) %>%
  pivot_wider(names_from = Year, values_from = n) %>%
  arrange(County) %>%
  print(n = Inf)



## LA County ----
#### Filter for LA County
la_sites <- ca_data %>% filter(County == "Los Angeles")

la_data <- combined_data %>%
  filter(County == "Los Angeles", PM25 >= 0, !is.na(Date))

#### Boxplot of PM2.5 by Year ----
ggplot(la_data, aes(x = factor(Year), y = PM25)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "PM2.5 Distribution in LA County: 2002 vs 2022",
       x = "Year", y = "Daily PM2.5") +
  theme_minimal()

#### Monthly Trend Line ----
la_data$Month <- factor(format(as.Date(la_data$Date), "%m"),
                        levels = sprintf("%02d", 1:12),
                        labels = month.abb)

ggplot(la_data, aes(x = Month, y = PM25, group = Year, color = factor(Year))) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(title = "Monthly PM2.5 in LA County: 2002 vs 2022",
       x = "Month", y = "Mean PM2.5", color = "Year") +
  theme_minimal()


#### Daily Time Series ----
ggplot(la_data, aes(x = as.Date(Date), y = PM25, color = factor(Year))) +
  geom_line(alpha = 0.4) +
  labs(title = "Daily PM2.5 in LA County",
       x = "Month (MM format)", y = "PM2.5", color = "Year") +
  theme_minimal()

#### Obs count ----
la_data %>%
  count(Year) %>%
  ggplot(aes(x = factor(Year), y = n, fill = factor(Year))) +
  geom_col() +
  labs(title = "Valid PM2.5 Observations in LA County",
       x = "Year", y = "Count") +
  theme_minimal()



## Summary stats ----
la_data <- combined_data %>%
  filter(County == "Los Angeles", PM25 >= 0, !is.na(Date), !is.na(PM25))

#### Basic summary ----
la_data %>%
  group_by(Year) %>%
  summarise(
    count = n(),
    mean = mean(PM25),
    median = median(PM25),
    sd = sd(PM25),
    min = min(PM25),
    max = max(PM25),
    .groups = "drop"
  )

#### Extreme value counts ----
la_data %>%
  group_by(Year) %>%
  summarise(
    above_35 = sum(PM25 > 35),
    above_55 = sum(PM25 > 55),
    above_100 = sum(PM25 > 100),
    .groups = "drop"
  )

#### Monthly averages ----
la_data %>%
  mutate(Month = factor(format(as.Date(Date), "%m"),
                        levels = sprintf("%02d", 1:12),
                        labels = month.abb)) %>%
  group_by(Year, Month) %>%
  summarise(monthly_avg = mean(PM25), .groups = "drop") %>%
  arrange(Year, Month) %>%
  print(n = Inf)

#### Obs counts ----
la_data %>%
  count(Year)





