## ---- Globe inset centered on Madison, WI ----
center_lon <- -89.4012
center_lat <- 43.0731

# World polygons -> sf (from the `maps` package you already use)
world <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) |>
  st_set_crs(4326) |>
  st_make_valid()

# Point of interest
center_pt <- st_sfc(st_point(c(center_lon, center_lat)), crs = 4326)

# Visible hemisphere = everything within ~90 degrees (great-circle) of the center point
# radius in meters, just under a quarter of Earth's circumference
hemisphere <- st_buffer(center_pt, dist = 9800000)

world_visible <- suppressWarnings(st_intersection(world, hemisphere))

# Orthographic projection centered on the point
ortho_crs <- sprintf("+proj=ortho +lat_0=%f +lon_0=%f", center_lat, center_lon)

world_globe   <- st_transform(world_visible, crs = ortho_crs)
globe_outline <- st_transform(hemisphere, crs = ortho_crs)      # the circular "ocean" backdrop
point_globe   <- st_transform(center_pt, crs = ortho_crs)

# Graticule (lat/lon grid lines) for polish
grat <- st_graticule(lon = seq(-180, 180, 20), lat = seq(-80, 80, 20)) |>
  st_intersection(hemisphere) |>
  st_transform(ortho_crs)

us_inset <- ggplot() +
  geom_sf(data = globe_outline, fill = "#eaf3fa", color = NA) +          # ocean/sky
  geom_sf(data = grat, color = "grey85", linewidth = 0.15) +             # graticule
  geom_sf(data = world_globe, fill = "grey80", color = "white", linewidth = 0.15) +
  geom_sf(data = point_globe, color = "#D7191C", size = 2.2) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA, linewidth = 0.4),
    plot.margin = margin(3, 3, 3, 3)
  )
