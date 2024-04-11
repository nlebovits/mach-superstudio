library(tidyverse)
library(tigris)
library(sf)
library(janitor)
library(tidycensus)
library(tmap)
library(FedData)
library(terra)
library(exactextractr)
library(sfdep)

cejst <- st_read('./data/cejst/usa/usa.shp')

nj_cejst <- cejst %>% 
  filter(SF == "New Jersey") %>%
  clean_names() %>%
  rename(geoid = geoid10) %>%
  filter(st_is_valid(geometry),
         !st_is_empty(geometry)) %>% 
  select(c(p200_i_pfs, fld_pfs)) %>%
  rename(pov_rt = p200_i_pfs,
         flood_risk = fld_pfs) %>%
  mutate(nb = st_knn(st_point_on_surface(geometry), 5),
         wt = st_weights(nb),
         flood_lag = st_lag(flood_risk, nb, wt, na_ok = TRUE),
         flood_risk = ifelse(is.na(flood_risk), flood_lag, flood_risk)
  ) %>% 
  select(flood_risk)

out <- st_transform(nj_cejst, crs = 'epsg:3424')


folder_path <- "./data/nj_flood_risk"
file_name <- "nj_flood_risk.shp"
file_path <- paste0(folder_path, "/", file_name)

# Check if the folder exists, if not create it
if (!dir.exists(folder_path)) {
  dir.create(folder_path, recursive = TRUE)
}

# Now, explicitly specify the driver as "ESRI Shapefile"
st_write(out, dsn = file_path, driver = "ESRI Shapefile", layer_options = "OVERWRITE=true")


zip_file <- "./data/nj_flood_risk.zip"

# Zip the directory
zip(zipfile = zip_file, files = folder_path, flags = "-r")

tm_shape(out) +
  tm_polygons(col = 'flood_risk', border.alpha = 0, lwd = 0.01, palette = 'viridis', style = 'jenks')
