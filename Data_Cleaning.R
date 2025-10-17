# =======================================
# Durham Fire Stations Map
# =======================================

# Load libraries
library(tidygeocoder)
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)
library(ggrepel)

# --------------------------------
# 1️⃣ Read CSV and geocode
# --------------------------------
fire_stations <- read.csv("addresses_to_geocode.csv", stringsAsFactors = FALSE)

fire_stations_geo <- fire_stations %>%
  geocode(address = `Physical.Location`, method = "osm") %>%
  filter(!is.na(lat) & !is.na(long))

write.csv(fire_stations_geo, "Fire_Stations_Geocoded.csv", row.names = FALSE)

fire_stations_geo <- fire_stations_geo %>%
  mutate(ShortLabel = paste0("S", gsub("Station ", "", Station)))

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
  geom_sf(data = durham_boundary, fill = "gray95", color = "black", linewidth = 0.5) +
  geom_sf(data = battalion_hulls, aes(fill = factor(Battalion)), alpha = 0.15, color = NA) +
  geom_sf(data = fire_stations_points, aes(color = factor(Battalion)), size = 4, shape = 21, stroke = 1.5, fill = "white") +
  geom_text_repel(
    data = cbind(fire_stations_points, st_coordinates(fire_stations_points)),
    aes(X, Y, label = ShortLabel, color = factor(Battalion)),
    size = 3.5, fontface = "bold", show.legend = FALSE
  ) +
  scale_color_brewer(palette = "Set1", name = "Battalion") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(
    title = "Durham Fire Stations by Battalion",
    subtitle = "Geocoded from address data",
    caption = "Source: Durham Fire Department",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid = element_line(color = "gray85"),
    # 👇 Rotate longitude axis labels 90 degrees
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# --------------------------------
# 5️⃣ Save the map as PNG
# --------------------------------
ggsave("Durham_Fire_Stations_Map.png", width = 10, height = 8)
