# Load data directly from GitHub ----
country_lists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/country_lists.csv')
rank_by_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv')

library(httr)
library(tidyverse)
library(jsonlite)
library(tidyverse)
install.packages("maps")  # if you haven't already
library(tidyverse)
library(maps)

# first httr request to get ranking data

req <- GET("api.henleypassportindex.com/api/v3/countries")

parsed <- req$content |> 
  rawToChar() |> 
  fromJSON()

# Data by year was nested into a list. 
# Here we separate them out so that each year has its own row.

rank_by_year <- parsed$countries |> 
  filter(has_data) |> 
  tidyr::unnest_longer(col = data) |> 
  select(code, country, region, data, year = data_id) |> 
  unnest_wider(col = data)


# The data for country_lists is pulled in with multiple httr requests.
# We can run a query for a single country at a time, and we get back a list
# with the country's name and code, and several data frames that show the 
# names and codes of the countries a passport owner has different visa access to. 

# We can define a function to transform the list into a data frame with columns that contain 
# these additional data frames. In order to meet Tidy Tuesday requirements,
# this data was transformed into JSON to be saved into a csv file.

list_to_nested_df <- function(input_list) {
  processed_data <- lapply(input_list, function(x) {
    if(is.data.frame(x)) {
      toJSON(I(list(x)))
    } else {
      x 
    }
  })
  
  df <- data.frame(processed_data)
  
  return(df)
}

## Code not used ----
country_lists <- data.frame()

for (i in unique(rank_by_year$code)) {
  print(i)
  
  req2 <- GET(paste0("api.henleypassportindex.com/api/v3/visa-single/", i))
  parsed2 <- req2$content |> 
    rawToChar() |> 
    fromJSON()
  
  add <- list_to_nested_df(parsed2)
  country_lists <-  rbind(add, country_lists)
  
  Sys.sleep(2)
}

# Top 10 Countries Visa-Free Plot ----
library(tidyverse)

# Filter for the most recent year
latest_year <- max(rank_by_year$year, na.rm = TRUE)

top_passports <- rank_by_year %>%
  filter(year == latest_year) %>%
  arrange(desc(visa_free_count)) %>%
  slice_head(n = 10)

# Plot
ggplot(top_passports, aes(x = reorder(country, visa_free_count), y = visa_free_count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = paste("Top 10 Passports by Visa-Free Access in", latest_year),
    x = "Country",
    y = "Visa-Free Destinations"
  ) +
  theme_minimal()


# World Map: Top vs. Bottom 20 Passports by Visa-Free Access -----

# Load required packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Get latest year
latest_year <- max(rank_by_year$year, na.rm = TRUE)

# Filter and rank
rank_latest <- rank_by_year %>%
  filter(year == latest_year) %>%
  select(code, country, visa_free_count)

# Top and bottom 20
top_20 <- rank_latest %>%
  arrange(desc(visa_free_count)) %>%
  slice_head(n = 20) %>%
  mutate(group = "Top 20")

bottom_20 <- rank_latest %>%
  arrange(visa_free_count) %>%
  slice_head(n = 20) %>%
  mutate(group = "Bottom 20")

# Combine and fix country names
combined <- bind_rows(top_20, bottom_20) %>%
  mutate(country = case_when(
    country == "United States of America" ~ "United States",
    country == "South Korea" ~ "South Korea",
    country == "North Korea" ~ "North Korea",
    country == "Russia" ~ "Russia",
    country == "Czechia" ~ "Czech Republic",
    TRUE ~ country
  ))

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join and keep only matched countries
highlighted <- world %>%
  left_join(combined, by = c("name" = "country")) %>%
  filter(!is.na(group))

# Plot merged map
ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80", size = 0.2) +  # base map
  geom_sf(data = highlighted, aes(fill = visa_free_count), color = "white", size = 0.3) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(
    title = paste("Top 20 vs. Bottom 20 Passports by Visa-Free Access in", latest_year),
    fill = "Visa-Free Count"
  ) +
  theme_minimal()




