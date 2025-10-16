# =======================================
# Durham Fire Stations Map
# =======================================

# Load libraries
library(tidygeocoder)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)
library(ggrepel)  # for nice label placement

# --------------------------------
# 1️⃣ Read CSV and geocode
# --------------------------------
fire_stations <- read.csv("addresses_to_geocode.csv", stringsAsFactors = FALSE)

# Geocode addresses using OpenStreetMap
fire_stations_geo <- fire_stations %>%
  geocode(address = Physical.Location, method = "osm") %>%
  filter(!is.na(lat) & !is.na(long))

# Create short labels (S1, S2, etc.)
fire_stations_geo <- fire_stations_geo %>%
  mutate(ShortLabel = paste0("S", gsub("Station ", "", Station)))

# Convert to spatial data
fire_stations_points <- st_as_sf(fire_stations_geo, coords = c("long", "lat"), crs = 4326)

# --------------------------------
# 2️⃣ Durham County boundary
# --------------------------------
durham_boundary <- counties(state = "NC", cb = TRUE) %>%
  subset(NAME == "Durham") %>%
  st_transform(crs = 4326)

# --------------------------------
# 3️⃣ Create convex hulls for battalion areas
# --------------------------------
battalion_hulls <- fire_stations_points %>%
  group_by(Battalion) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_convex_hull()

# --------------------------------
# 4️⃣ Plot map
# --------------------------------
ggplot() +
  # Durham boundary
  geom_sf(data = durham_boundary, fill = "gray95", color = "black", linewidth = 0.5) +
  
  # Battalion polygons (convex hulls)
  geom_sf(data = battalion_hulls, aes(fill = factor(Battalion)), alpha = 0.15, color = NA) +
  
  # Fire stations
  geom_sf(data = fire_stations_points, aes(color = factor(Battalion)), size = 4, shape = 21, stroke = 1.5, fill = "white") +
  
  # Station labels (short form)
  geom_text_repel(
    data = fire_stations_geo,
    aes(x = long, y = lat, label = ShortLabel, color = factor(Battalion)),
    size = 3.5, fontface = "bold", nudge_y = 0.003, show.legend = FALSE
  ) +
  
  # Colors and fills
  scale_color_brewer(palette = "Set1", name = "Battalion") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  
  # Titles and captions
  labs(
    title = "Durham Fire Stations by Battalion",
    subtitle = "Geocoded from address data",
    caption = "Source: Durham Fire Department"
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid = element_line(color = "gray85")
  )

# --------------------------------
# 5️⃣ Save the map as PNG
# --------------------------------
ggsave("Durham_Fire_Stations_Map.png", width = 10, height = 8)
