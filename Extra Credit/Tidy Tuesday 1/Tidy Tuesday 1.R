install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-08-26')

billboard <- tuesdata$billboard
topics <- tuesdata$topics

# View Variables
names(billboard)


# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the CSV file
billboard <- read.csv("billboard.csv")

# Convert 'date' column to year
billboard <- billboard %>%
  mutate(year = year(ymd(date)))

# Calculate average song length per year
avg_length_by_year <- billboard %>%
  group_by(year) %>%
  summarize(avg_length_sec = mean(length_sec, na.rm = TRUE)) %>%
  arrange(year)

# Plot the trend
ggplot(avg_length_by_year, aes(x = year, y = avg_length_sec)) +
  geom_line(color = "#4682B4", size = 1) +
  geom_point(color = "black", size = 1.75) +
  labs(
    title = "Average Song Length Over Time",
    x = "Year",
    y = "Average Length (seconds)"
  ) +
  theme_minimal()
