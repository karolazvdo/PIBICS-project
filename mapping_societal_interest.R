# =============================================
# MAPPING SPECIES SOCIETAL INTEREST (WIKIPEDIA PAGEVIEWS)
# Karoline Azevedo, for PIBICS project 07 de Julho de 2025
# =============================================
##
# Install and load packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

# Load required packages
p_load(sf, terra, raster, dplyr, tidyr, ggplot2, viridis, rnaturalearth)

# ---------------------------------------------
# 1. DATA PREPARATION
# ---------------------------------------------

# Load pageview dataset
species_views <- read.csv("interesse_nacional_mamiferos.csv")

# Load species range shapefile (IUCN)
# >> Update the path below according to your machine
iucn_ranges <- read_sf("C:/Users/drive/OneDrive/Documentos/scripts no git/PIBICS-project/MAMMALS_TERRESTRIAL_ONLY.shp")

# Filter only species present in the Wikipedia dataset
ranges_filtered <- iucn_ranges %>%
  filter(sci_name %in% species_views$article)

# Group duplicated species geometries into a single feature
ranges_union <- ranges_filtered %>%
  group_by(id_no) %>%
  summarise(geometry = st_union(geometry))

# Join back attributes and keep only one record per species
ranges_union <- ranges_union %>%
  left_join(ranges_filtered %>% st_drop_geometry(), by = "id_no") %>%
  distinct(id_no, .keep_all = TRUE)

# Merge with pageview data
ranges_union <- ranges_union %>%
  left_join(species_views, by = c("sci_name" = "article"))

# Keep only extant or probably extant species
ranges_union <- ranges_union %>%
  filter(presence %in% c("1", "2", "3"))

# Optional: keep only selected columns [check your necessary columns]
ranges_union <- ranges_union[, c(1:3, 17:22, 24, 28:33)]

# Save cleaned shapefile (optional)
st_write(ranges_union, "species_ranges_cleaned.shp", driver = "ESRI Shapefile", overwrite = TRUE)

# ---------------------------------------------
# 2. RASTERIZATION
# ---------------------------------------------

# Create raster template with ~1km resolution (0.00833 degrees)
bbox <- st_bbox(ranges_union) %>%
  st_as_sfc() %>%
  st_transform(crs = st_crs(ranges_union)) %>%
  st_buffer(1.0) %>%
  st_bbox()

r_template <- rast(
  resolution = 0.00833,
  xmin = bbox$xmin, xmax = bbox$xmax,
  ymin = bbox$ymin, ymax = bbox$ymax,
  crs = st_crs(ranges_union)$wkt
)

# Rasterize mean pageviews (field: check name for views column)
v <- vect(ranges_union)
r_interest <- rasterize(v, r_template, field = "acesso_medio", fun = "sum", background = NA)
r_interest[is.na(r_interest)] <- 0

# Rasterize species richness (1 per species)
ranges_union$presenca <- 1
v_rich <- vect(ranges_union)
r_richness <- rasterize(v_rich, r_template, field = "presenca", fun = "sum", background = NA)
r_richness[is.na(r_richness)] <- 0

# ---------------------------------------------
# 3. MASKING TO BRAZIL AND CONVERSION TO DATAFRAME
# ---------------------------------------------

# Load Brazil boundary
brazil <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf") %>%
  st_transform(crs = st_crs(ranges_union))

# Mask interest raster to Brazil
r_interest_masked <- mask(r_interest, vect(brazil))
r_richness_masked <- mask(r_richness, vect(brazil))

# Convert to data.frame (1 row per raster cell)
df_interest <- as.data.frame(r_interest_masked, xy = TRUE, cells = TRUE, na.rm = TRUE) %>%
  rename(media_pt = acesso_medio) #for mapping interest

df_richness <- as.data.frame(r_richness_masked, xy = TRUE, cells = TRUE, na.rm = TRUE) %>%
  rename(richness = presenca) # for mapping richness

# ---------------------------------------------
# 4. VISUALIZATION
# ---------------------------------------------

# Plot species interest using ggplot2
# change 'df_interest' for 'df_richness' and fill 'media_pt' for fill 'richness' to mapping richness
map_interest <- ggplot(df_interest, aes(x = x, y = y, fill = media_pt)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis(name = "Species interest", na.value = "transparent") +
  labs(
    title = "Species societal interest (Wikipedia pageviews)",
    x = "", y = ""
  ) +
  theme_minimal()

# Display map
print(map_interest)

# ---------------------------------------------
# 5. EXPORT RESULTS
# ---------------------------------------------

# Save map as PDF
ggsave("map_species_interest.pdf", plot = map_interest, width = 8, height = 6, units = "in")

# Save interest data as CSV
write.csv(df_interest, "table_species_interest.csv", row.names = FALSE)
