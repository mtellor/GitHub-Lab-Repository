# Reading the data, filtering, and replacing bad values with NAs
met <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/Lab/Lab 3/data/met_all.gz")
met <- met[met$temp > -10, ]
met$elev[met$elev == 9999.0] <- NA

# Creating an aggregated version of the dataset
library(dplyr)
met_avg <- summarize(met,
                     temp =     mean(temp, na.rm = TRUE),
                     rh =       mean(rh, na.rm = TRUE),
                     wind.sp =  mean(wind.sp, na.rm = TRUE),
                     vis.dist = mean(vis.dist, na.rm = TRUE),
                     lat =      mean(lat, na.rm = TRUE),
                     lon =      mean(lon, na.rm = TRUE),
                     elev =     mean(elev, na.rm = TRUE),
                     .by = c(USAFID, day))

# New variables using ifelse()
met_avg$region <- ifelse(met_avg$lon > -98, "east", "west")
met_avg$elev_cat <- ifelse(met_avg$elev > 252, "high", "low")

# Using the cut() function to create more than 2 categories
met_avg$vis_cat <- cut(met_avg$vis.dist,
                       breaks = c(0, 1000, 6000, 10000, Inf),
                       labels = c("fog", "mist", "haze", "clear"),
                       right = FALSE)

# Basic Sctrplt
## R Basic
plot(met_avg$temp, met_avg$rh)

## GGPlot
library(ggplot2)
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh))

# Aesthetics - color
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, color = region))

# Adding transparency
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, alpha = 0.3))

# Point shape
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh, shape = region))

# Manual control of aesthetics - color
ggplot(data = met_avg) + 
  geom_point(mapping = aes(x = temp, y = rh), color = "navy")

# Adding pts to plot
plot(1:10, pch = 16)
points(10:1, pch = 16, col = 2)

# Facets
## 1
met_avg[!is.na(met_avg$region), ] |> 
  ggplot() + 
  geom_point(mapping = aes(x = temp, y = rh, color=region)) + 
  facet_wrap(~ region, nrow = 1)

## 2
met_avg[!is.na(met_avg$region) & !is.na(met_avg$elev_cat), ] |> 
  ggplot() + 
  geom_point(mapping = aes(x = temp, y = rh)) + 
  facet_grid(region ~ elev_cat)

## 3
layout(matrix(1:2, nrow=1))
plot(met_avg$temp[which(met_avg$region == 'east')], met_avg$rh[which(met_avg$region == 'east')], pch = 16, col = 2)
plot(met_avg$temp[which(met_avg$region == 'west')], met_avg$rh[which(met_avg$region == 'west')], pch = 16, col = 4)

# Geometric objects
## 1
library(cowplot)

scatterplot <- ggplot(data = met_avg) + geom_point(mapping = aes(x = temp, y = rh))
lineplot    <- ggplot(data = met_avg) + geom_smooth(mapping = aes(x = temp, y = rh))

plot_grid(scatterplot, lineplot, labels = "AUTO")

## 2
ggplot(data = met_avg) + 
  geom_smooth(mapping = aes(x = temp, y = rh, linetype = region))

# Histograms
## R Base
hist(met_avg$temp)

## ggplot
ggplot(met_avg) + 
  geom_histogram(mapping = aes(x = temp))

# Box plots
## R Base
boxplot(met_avg$temp ~ met_avg$elev_cat)

## ggplot
met_avg[!is.na(met_avg$elev_cat), ] %>% 
  ggplot()+
  geom_boxplot(mapping=aes(x=elev_cat, y=temp, fill=elev_cat)) 

# Lineplots
## R base
plot(met_avg$day[met_avg$elev==4113], met_avg$temp[met_avg$elev==4113], type = 'l')

### adding line to existing plot
plot(1:10, pch = 16)
lines(10:1, col = 2, lwd = 3) # add a thick red line

## ggplot
ggplot(data = met_avg[met_avg$elev==4113, ])+
  geom_line(mapping=aes(x=day, y=temp))

# Polygons
world_map <- map_data("world")
ggplot(data = world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "darkgray", color = "white")

us_map <- map_data("state")
ggplot(data = us_map, aes(x = long, y = lat, fill = region)) +
  geom_polygon(color = "white")

# Multiple geoms
## 1
met_avg[!is.na(met_avg$region), ] %>%
  ggplot() + 
  geom_point(mapping = aes(x = temp, y = rh, color = region))+
  geom_smooth(mapping = aes(x = temp, y = rh, linetype = region))

## 2
met_avg[!is.na(met_avg$region), ] %>%
  ggplot(mapping = aes(x = temp, y = rh, color=region, linetype=region)) +
  geom_point() + 
  geom_smooth()

### Arguments in geom (smooth)
met_avg[!is.na(met_avg$region), ] %>%
  ggplot(mapping = aes(x = temp, y = rh, color = region, linetype = region)) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, col = "black")

## 3
met_avg[!is.na(met_avg$region), ] %>%
  ggplot(mapping = aes(x = temp, y = rh)) + 
  geom_point(mapping = aes(color = region)) + 
  geom_smooth()

## 4
met_avg[!is.na(met_avg$vis_cat), ] %>%
  ggplot(mapping = aes(x = temp, y = rh, alpha = 0.5)) + 
  geom_point(mapping = aes(color = vis_cat)) + 
  geom_smooth(se = FALSE)

# Bar Charts
## 1
### R base
tab <- table(met_avg$vis_cat)
barplot(tab)

OR

barplot(table(met_avg$vis_cat))

## ggplot
met_avg %>%
  filter(!is.na(met_avg$vis_cat)) %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = vis_cat))

### for propotions instead of counts:
met_avg[!is.na(met_avg$vis_cat), ] %>%
  ggplot() + 
  geom_bar(mapping = aes(x = vis_cat, y = stat(prop), group = 1))

# coloring barchart
met_avg[!is.na(met_avg$vis_cat), ] %>%
  ggplot() + 
  geom_bar(mapping = aes(x = vis_cat, color = vis_cat, fill=vis_cat))

## coloring stacked bar plots
### R base
tab <- table(met_avg$region[!is.na(met_avg$vis_cat) & met_avg$vis_cat != "clear"],
             met_avg$vis_cat[!is.na(met_avg$vis_cat) & met_avg$vis_cat != "clear"])
barplot(tab, col = c(2,4))

### ggplot
met_avg[!is.na(met_avg$vis_cat) & met_avg$vis_cat != "clear", ] %>%
  ggplot() + 
  geom_bar(mapping = aes(x = vis_cat, fill = region))+
  scale_fill_viridis_d() 

# bars side by side
met_avg[!is.na(met_avg$vis_cat) & met_avg$vis_cat != "clear", ] %>%
  ggplot() + 
  geom_bar(mapping = aes(x = elev_cat, fill = vis_cat), position = "dodge")

# Min, max, median
l <- met_avg[!is.na(met_avg$vis_cat) & met_avg$vis_cat != "clear", ] %>%
  ggplot() + 
  stat_summary(mapping = aes(x = vis_cat, y = temp),
               fun.min = min,
               fun.max = max,
               fun = median)

l

# Jittering - Position adjustments
nojitter <- ggplot(data = met_avg[1:1000,]) + 
  geom_point(mapping = aes(x = vis_cat, y = temp))

jitter <- ggplot(data = met_avg[1:1000,]) + 
  geom_point(mapping = aes(x = vis_cat, y = temp), position = "jitter")

plot_grid(nojitter, jitter, labels = "AUTO")

# Coordinate systems
## Flip plots
unflipped <- ggplot(data = met_avg) + 
  geom_boxplot(mapping = aes(x = vis_cat, y = temp))

flipped <- ggplot(data = met_avg) + 
  geom_boxplot(mapping = aes(x = vis_cat, y = temp)) +
  coord_flip()

plot_grid(unflipped, flipped, labels = "AUTO")

## Pie charts and bar chart flips
bar <- ggplot(data = met_avg) + 
  geom_bar(mapping = aes(x = elev_cat, fill = elev_cat), show.legend = FALSE, width = 1) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()

bar + coord_polar()


# Modifying labels
ggplot(met_avg[!is.na(met_avg$region), ]) +
  geom_point(aes(temp, rh, color = region)) + 
  labs(title = "Weather Station Data") + 
  labs(x = expression("Temperature" *~ degree * C), y = "Relative Humidity")

# Changing theme
ggplot(met_avg[!is.na(met_avg$region), ]) +
  geom_point(aes(temp, rh, color = region)) + 
  labs(title = "Weather Station Data") + 
  labs(x = expression("Temperature"*~degree*C), y = "Relative Humidity")+
  theme_ceruluean(base_family = "Times")

# Changing the legend
ggplot(met_avg[!is.na(met_avg$region), ]) +
  geom_point(aes(temp, rh, color = region)) + 
  labs(title = "Weather Station Data",x = expression("Temperature"*~degree*C), y = "Relative Humidity")+
  scale_color_manual(name="Region", labels=c("East", "West"), values=c("east"="yellow3", "west"="darkred"))+
  theme_bw(base_family = "Times")

# Changing color scales
ggplot(data = met_avg) + 
  geom_point(mapping=aes(x=temp, y=rh, color=elev))+
  scale_color_gradient(low="blue", high="red")

library(viridis)
ggplot(data=met_avg) + 
  geom_point(mapping= aes(x=temp, y=rh, color = cut(elev, b=5))) + 
  scale_color_manual(values = viridis::viridis(6))

# Maps with leafleft (interactive maps)
library(leaflet)
met_avg2 <- summarize(met,
                      temp =     mean(temp, na.rm = TRUE),
                      lat =      mean(lat, na.rm = TRUE),
                      lon =      mean(lon, na.rm = TRUE),
                      .by = c(USAFID))
met_avg2 <- met_avg2[!is.na(met_avg2$temp), ]

# Generating a color palette
temp.pal <- colorNumeric(c('darkgreen','goldenrod','brown'), domain=met_avg2$temp)

temp.pal(20)

temp.pal

tempmap <- leaflet(met_avg2) %>% 
  # The looks of the Map
  addProviderTiles('CartoDB.Positron') %>% 
  # Some circles
  addCircles(
    lat = ~lat, lng=~lon,
    # HERE IS OUR PAL!
    label = ~paste0(round(temp,2), ' C'), color = ~ temp.pal(temp),
    opacity = 1, fillOpacity = 1, radius = 500
  ) %>%
  # And a pretty legend
  addLegend('bottomleft', pal=temp.pal, values=met_avg2$temp,
            title='Temperature, C', opacity=1)

tempmap



