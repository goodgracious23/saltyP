library(sf)       # For handling spatial data
library(terra)    # For raster data
library(tmap)     # For map visualization
library(FedData)
library(ggspatial)
library(tidyverse)

# DC land use
DCland = st_read('data_GIS/DaneCounty/LandUse2020/LandUse2020_crop.shp')
# Load lakes
DClakes = st_read('data_GIS/DaneCounty/LakesPonds/LakesPonds_wID.shp') |> 
  filter(ID %in% c(652, 641, 4267, 4266, 2718, 3964, 3910)) |> 
  # filter(NAME %in% c('Tiedeman Pond', "Stricker's Pond", 'Lake Wingra')) |> 
  st_transform(st_crs(DCland))

DClakes_centroid = DClakes |> 
  filter(ID != 4266) |> 
  st_centroid()

# Lake Wingra sampling points 
# Create the points (lon, lat)
wingra_pts <- data.frame(lon = c(-89.42865, -89.42500),
  lat = c(43.05344, 43.05300))

wingra_pts_sf <- st_as_sf(wingra_pts, coords = c("lon", "lat"), crs = 4326) |> 
  st_transform(st_crs(DCland))

# Load watersheds, transform CRS
wingraWS = st_read('data_GIS/YaharaBasins/Wingra_Basin.shp') |> 
  st_transform(st_crs(DCland))
# Madison hydrology
hydroWS = st_read('data_GIS/Madison_Hydro/ModeledAreas.shp') |> 
  dplyr::filter(OBJECTID %in% c(50, 9)) |> 
  st_zm(drop = TRUE, what = "ZM") |> 
  st_transform(st_crs(DCland))

pondsWS = st_read('data_GIS/ponds.shp') |> 
  st_transform(st_crs(DCland))

WS = bind_rows(wingraWS, hydroWS, pondsWS)

# Get bounding box for both
bb <- st_bbox(WS)
bb_transform = st_transform(bb, st_crs(DCland))

# Crop DC land
DClanduse <- st_crop(DCland, bb_transform) |> 
  mutate(GENERALIZE = factor(GENERALIZE,
         levels = c("WATER","WOODLANDS","OPEN LAND","RECREATION","AGRICULTURE","RESIDENTIAL","COMMERCIAL","WHOLESALE AND RETAIL TRADE","INDUSTRIAL","MANUFACTURING","INSTITUTIONAL/GOVERNMENTAL","TRANSPORTATION, COMMUNICATIONS AND UTILITIES","VACANT SUBDIVIDED LAND","UNDER CONSTRUCTION", "MINERAL EXTRACTION"),
         labels = c("Water","Woodlands","Open land","Recreation","Agriculture","Residential","Commercial","Wholesale and retail trade","Industrial","Manufacturing","Institutional/governmental","Transportation, communications and utilities","Vacant subdivided land","Under construction", 'Mineral Extraction')
  )) |> 
  mutate(GENERALIZE = fct_collapse(GENERALIZE,
               "Green Space" = c("Woodlands", "Open land", "Recreation"),
               "Industrial" = c("Industrial", "Manufacturing"),
               "Transportation" = "Transportation, communications and utilities",
               "Commercial" = c("Commercial", "Wholesale and retail trade"), 
               "Vacant" = c("Vacant subdivided land"),
               "Governmental" = c("Institutional/governmental")
  )) |> 
  filter(GENERALIZE != 'Under construction')
table(DClanduse$GENERALIZE)

DCland_dissolved <- DClanduse %>%
  group_by(GENERALIZE) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

landuse_colors <- c(
  "Agriculture" = "#E6D98C",
  "Commercial" = "#FB9A99",
  "Industrial" = "#B15928",
  "Manufacturing" = "#8C510A",
  "Governmental" = "#c2a3d6",
  "Residential" = "#dbc7a4",
  "Green Space" = "#B2DF8A",
  "Water" = "#1F78B4",
  "Transportation" = "#636363",
  # "Under construction" = "#FF7F00",
  "Mineral Extraction" = 'grey80',
  "Vacant" = "#FFFF99"
)

# # Test land use plot
# ggplot(DCland_dissolved) +
#   geom_sf(aes(fill = GENERALIZE), color = NA) +
#   scale_fill_manual(values = landuse_colors, name = "Land Use") +
#   theme_bw() +
#   theme(legend.title = element_text(size = 10),
#         legend.text = element_text(size = 8))

# Crop raster and mask to watershed 
cropped_landuseWI <- st_intersection(DCland_dissolved, WS)

# Final plot
ggplot(DCland_dissolved) +
  geom_sf(aes(fill = GENERALIZE), color = NA, alpha = 0.3) +

  geom_sf(data = cropped_landuseWI, aes(fill = GENERALIZE), color = NA) +
  scale_fill_manual(values = landuse_colors, name = "Land Use") +
  geom_sf(data = WS, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = DClakes, fill = '#5475A8') +
  # geom_sf(data = DClakes_centroid, fill = 'gold', shape = 21) +

  annotate('text',x = 812000, y = 480371, label = "Lake Wingra", 
           size = 2.5, color = 'black', fontface = 2, hjust = 0) +
  annotate('text',x = 799000, y = 487298.2, label = "Stricker Pond",
           size = 2.5, color = 'black', fontface = 2, hjust = 0) +
  annotate('text',x = 797000, y = 488570.5, label = "Tiedeman Pond",
           size = 2.5, color = 'black', fontface = 2, hjust = 0) +
  annotate('text',x = 795000, y = 491500, label = "Lakeview Pond",
           size = 2.5, color = 'black', fontface = 2, hjust = 0) +
  annotate('text',x = 799000, y = 499000, label = "Orchid Pond",
           size = 2.5, color = 'black', fontface = 2, hjust = 0) +
  annotate('text',x = 791000, y = 467000, label = "Elver Pond",
           size = 2.5, color = 'black', fontface = 2, hjust = 0) +
  
  annotate(geom = "segment", x = 811759.7, y = 475138.8, xend = 812000, yend = 480371, linewidth = 0.3) + #Wingra
  annotate(geom = "segment", x = 787679.6, y = 487298.2, xend = 799000, yend = 487298.2, linewidth = 0.3) + #Stricker
  annotate(geom = "segment", x = 793301.3, y = 491501.3, xend = 795000, yend = 491500, linewidth = 0.3) + #Lakeview
  annotate(geom = "segment", x = 795899.9, y = 498889, xend = 799000, yend = 499000, linewidth = 0.3) + #Orchid
  annotate(geom = "segment", x = 788960, y = 469485.4, xend = 791000, yend = 467000, linewidth = 0.3) + #Elver
  annotate(geom = "segment", x = 789112.8, y = 488570.5, xend = 797000, yend = 488570.5, linewidth = 0.3) + #Tiedman
  
  geom_sf(data = wingra_pts_sf, color = "black", size = 1.5, shape = 18) + #wingra points
  
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "tr", width_hint = 0.3, height = unit(0.1, "cm")) +  # Adds scale bar
  theme_bw(base_size = 10) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.4,'cm'),
        # legend.position = 'bottom',
        axis.title = element_blank())

ggsave('figures/Map_2023.png', width = 6, height = 4, dpi = 500)

#  # Land use stats 
# landuse_merge %>% ungroup() %>% 
#   mutate(group = str_detect(landuse, "Developed")) %>% 
#   mutate(tot = n()) %>% 
#   group_by(group) %>% 
#   summarise(n = n()/first(tot))
# 
# # 74.1% Developed land use vs other
# # 54.7% "low-high intensity developed" 
# 
# landuse_merge %>% ungroup() %>% 
#   mutate(group = str_detect(landuse, "Open Space")) %>% 
#   mutate(tot = n()) %>% 
#   group_by(group) %>% 
#   summarise(n = n()/first(tot))
