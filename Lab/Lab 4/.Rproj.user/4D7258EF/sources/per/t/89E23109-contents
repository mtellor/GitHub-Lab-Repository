# 1) Read in data ----
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(leaflet)

met <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Lab/GitHub-Lab-Repository/Lab 3/data/met_all.gz")
met <- data.table::fread("met_all.gz")


# 2) Preparing data ----
met <- met[temp > -17][elev == 9999.0, elev := NA]
met[, week := week(as.Date(paste(year, month, day, sep = "-")))]
met <- met[week == min(week, na.rm = TRUE)]

met_avg <- met[,.(temp=mean(temp,na.rm=TRUE), rh=mean(rh,na.rm=TRUE), 
                  wind.sp=mean(wind.sp,na.rm=TRUE), 
                  vis.dist=mean(vis.dist,na.rm=TRUE), 
                  dew.point = mean(dew.point, na.rm=TRUE), lat=mean(lat), 
                  lon=mean(lon), 
                  elev=mean(elev,na.rm=TRUE)), by="USAFID"]

met_avg$elev_cat <- ifelse(met_avg$elev> 252, "high", "low")

met_avg$region <- ifelse(met_avg$lon > -98 & met_avg$lat >39.71, "north east",
                         ifelse(met_avg$lon > -98 & met_avg$lat < 39.71, 
                                "south east",
                                ifelse(met_avg$lon < -98 & met_avg$lat >39.71, 
                                       "north west", "south west")))

table(met_avg$region)

# 3) Examine relative humidity and dew point ----
met_avg %>%
  filter(!(dew.point %in% NA)) %>%
  ggplot()+
  geom_violin (mapping = aes(y=dew.point, x=1))+
  facet_wrap(~region,nrow = 2)

met_avg %>%
  filter(!(dew.point %in% NA)) %>%
  ggplot()+
  geom_boxplot(mapping = aes(y=rh, fill=region)) +
  facet_wrap(~region, nrow=2)

# 4) Association between dew point and wind speed ----
met_avg %>%
  filter(!(region %in% NA)) %>%
  ggplot(mapping = aes(x=dew.point, y= rh, color=region))+
  geom_jitter() + 
  stat_smooth(method=lm)

# 5) Barplots of weather stations by elevation ----
met_avg %>%
  filter(!(region %in% NA)) %>%
  ggplot()+
  geom_bar(mapping=aes(x=elev_cat,fill=region), position = "dodge")+
  scale_fill_brewer(palette = "PuOr")+
  labs(title="Number of weather stations by elevation category and region", 
       x="Elevation Category", y= "Count")+
  theme_bw()

# 6) Examine mean dew point and wind speed by region ----
met_avg %>%
  filter(!(region %in% NA)) %>%
  ggplot(mapping=aes(x=region, y=dew.point)) +
  stat_summary(fun.data="mean_sdl", geom="errorbar") +
  stat_summary(fun.data="mean_sdl")

met_avg %>%
  filter(!(region %in% NA)) %>% 
  ggplot(mapping=aes(x=region, y=wind.sp)) +
  stat_summary(fun.data="mean_sdl", geom="errorbar") + 
  stat_summary(fun.data="mean_sdl")

# 7) Spatial trends in relative humidity ----
met_avg2<-met_avg[!is.na(rh)]

top5 <- met_avg2[rank(-rh) <= 10]

rh_pal = colorNumeric(c('blue','purple','red'), domain=met_avg2$rh)
leaflet(met_avg2) %>%
  addProviderTiles('OpenStreetMap') %>%
  addCircles(lat=~lat, lng=~lon, color=~rh_pal(rh), 
             label=~paste0(round(rh,2), ' rh'), opacity=1,fillOpacity=1, 
             radius=500) %>%
  addMarkers(lat=~lat, lng=~lon, label=~paste0(round(rh,2), ' rh'), 
             data = top5) %>%
  addLegend('bottomleft',pal=rh_pal, values=met_avg2$rh, 
            title="Relative Humidity", opacity=1)

# 8) Plot of choice: Average temperature by region ----
library(ggplot2)
library(ggtext)

ggplot(met_avg, aes(x = region, y = temp)) +
  geom_boxplot(fill = "skyblue", color = "navyblue") +
  labs(
    title = "<b>Average Temperature by Region</b><br>
    <span style='font-size:10pt'>This plot compares mean temperatures across U.S. regions.<br>
    <span style='color:darkred;'>Note:</span> Regions are based on latitude and longitude cutoffs.</span>",
    x = "Region<br><span style='font-size:8pt'>Defined by lat/lon thresholds</span>",
    y = "Temperature (°C)<br><span style='font-size:8pt'>Averaged across each weather station</span>"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 13,
      lineheight = 1.2,
      padding = margin(6, 6, 6, 6),
      margin = margin(0, 0, 8, 0),
      fill = "lightyellow"
    ),
    axis.title.x = element_textbox_simple(
      padding = margin(4, 4, 4, 4),
      margin = margin(6, 0, 0, 0),
      fill = "azure1"
    ),
    axis.title.y = element_textbox_simple(
      orientation = "left-rotated",
      padding = margin(4, 4, 4, 4),
      margin = margin(0, 0, 6, 0),
      fill = "lightsteelblue1"
    )
  )
