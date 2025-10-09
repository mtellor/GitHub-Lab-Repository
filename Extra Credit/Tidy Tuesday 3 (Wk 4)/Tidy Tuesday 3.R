# Data & Packages ----

library(tidyverse)

# Load data
cuisines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-16/cuisines.csv')

# Summarize and filter
top_cuisines <- cuisines %>%
  group_by(country) %>%
  summarise(
    avg_rating = mean(avg_rating, na.rm = TRUE),
    n = n()
  ) %>%
  filter(n >= 10) %>%
  arrange(desc(avg_rating)) %>%
  slice_head(n = 20)

# Plot ----
ggplot(top_cuisines, aes(x = avg_rating, y = fct_reorder(country, avg_rating))) +
  geom_point(size = 4, color = "#FF6F61") +
  geom_segment(aes(x = 0, xend = avg_rating, y = country, yend = country),
               color = "gray80", size = 1) +
  labs(
    title = "Top 20 Cuisines by Average Recipe Rating",
    x = "Average Rating (out of 5)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot w/ labels ----
ggplot(top_cuisines, aes(x = avg_rating, y = fct_reorder(country, avg_rating))) +
  geom_point(size = 4, color = "#FF6F61") +
  geom_segment(aes(x = 0, xend = avg_rating, y = country, yend = country),
               color = "gray80", size = 1) +
  geom_text(aes(label = sprintf("%.2f", avg_rating)), hjust = -.5, size = 4, color = "black") +
  labs(
    title = "Top 20 Cuisines by Average Recipe Rating",
    x = "Average Rating (out of 5)",
    y = NULL
  ) +
  xlim(0, max(top_cuisines$avg_rating) + 0.5) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
