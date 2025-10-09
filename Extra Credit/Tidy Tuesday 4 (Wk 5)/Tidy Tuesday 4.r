library(tidyverse)

# Load September ratings
sep <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv")

# Federation name mapping (expand as needed)
fed_names <- tribble(
  ~fed, ~country,
  "AND", "Andorra",
  "BIH", "Bosnia and Herzegovina",
  "FIN", "Finland",
  "KOS", "Kosovo",
  "NED", "Netherlands",
  "SRB", "Serbia"
)

# Merge full names
sep_named <- sep %>%
  left_join(fed_names, by = "fed") %>%
  filter(!is.na(country))  # keep only mapped federations

# Plot with fixed y-axis and full names
ggplot(sep_named, aes(x = rating, fill = country)) +
  geom_density(alpha = 0.7, color = NA) +
  facet_wrap(~ country, scales = "fixed") +  # fixed y-axis
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Top 6 federations by average rating",
    x = "Elo Rating",
    y = "Rating Distribution"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )



