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

options(tigris_use_cache = TRUE, scipen = 999)

## load cleaned nj muni pop data
nj_muni_pop <- st_read('./data/nj_muni_pop_wide.geojson')

### import and aggregate nlcd data------------------------------------
nj <- states() %>% filter(NAME == "New Jersey")

nj_nlcd <- get_nlcd(
  template = nj,
  label = "nj",
  year = 2021,
  dataset = "landcover",
  landmass = "L48",
  extraction.dir = file.path(tempdir(), "FedData", "extractions", "nlcd", "nj"),
  raster.options = c("COMPRESS=DEFLATE", "ZLEVEL=9"),
  force.redo = FALSE
)

nj_geom <- nj_muni_pop %>% select(geoid)
nj_geom$land_cover <- exact_extract(nj_nlcd, nj_muni_pop, 'frac')
nj_muni_nlcd <- nj_geom %>% st_drop_geometry()
nj_muni_nlcd <- tidyr::unnest(nj_muni_nlcd, land_cover)

nlcd_names <- c(
  geoid = "geoid",
  namelsad = "place_name",
  frac_11 = "Open Water",
  frac_12 = "Perennial Ice/Snow",
  frac_21 = "Developed, Open Space",
  frac_22 = "Developed, Low Intensity",
  frac_23 = "Developed, Medium Intensity",
  frac_24 = "Developed, High Intensity",
  frac_31 = "Barren Land (Rock/Sand/Clay)",
  frac_41 = "Deciduous Forest",
  frac_42 = "Evergreen Forest",
  frac_43 = "Mixed Forest",
  frac_51 = "Dwarf Scrub",
  frac_52 = "Shrub/Scrub",
  frac_71 = "Grassland/Herbaceous",
  frac_72 = "Sedge/Herbaceous",
  frac_73 = "Lichens",
  frac_74 = "Moss",
  frac_81 = "Pasture/Hay",
  frac_82 = "Cultivated Crops",
  frac_90 = "Woody Wetlands",
  frac_95 = "Emergent Herbaceous Wetlands"
)

# Rename the columns of your dataframe
names(nj_muni_nlcd) <- nlcd_names[names(nj_muni_nlcd)]
nj_muni_nlcd <- nj_muni_nlcd %>% 
                  clean_names() 

nj_muni_pop_w_nlcd <- nj_muni_pop %>%
                  left_join(nj_muni_nlcd, by = "geoid")


### add cejst
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
                     pov_lag = st_lag(pov_rt, nb, wt, na_ok = TRUE),
                     flood_lag = st_lag(flood_risk, nb, wt, na_ok = TRUE),
                     pov_rt = ifelse(is.na(pov_rt), pov_lag, pov_rt),
                     flood_risk = ifelse(is.na(flood_risk), flood_lag, flood_risk)
              ) %>% 
              select(pov_rt,
                     flood_risk)
                     

nj_muni_pop_w_nlcd <- st_transform(nj_muni_pop_w_nlcd, crs = st_crs(nj_cejst))

final <- st_interpolate_aw(nj_cejst, nj_muni_pop_w_nlcd, extensive = FALSE)
final <- aggregate(final, by = list(nj_muni_pop_w_nlcd$geoid), FUN = sum) |> rename(geoid = Group.1)

tm_shape(final) +
  tm_polygons(col = 'flood_risk', border.alpha = 0, lwd = 0.1, style = 'jenks', palette = 'viridis') +
  tm_layout(legend.outside = TRUE,
            frame = FALSE)

final <- left_join(final, st_drop_geometry(nj_muni_pop_w_nlcd), by = 'geoid')

### write outfile
st_write(final, "./data/clustering_data.geojson")