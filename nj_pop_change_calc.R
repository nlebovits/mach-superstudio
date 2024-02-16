### (this file has to be run from Allison Lasster's remote desktop)

### import census data

source("config.R")

required_packages <- c("devtools", "remotes", "tidyverse", "sf","here", "data.table", "conflicted", "tidycensus", "tigris")
install_and_load_packages(required_packages)

filter <- dplyr::filter
select <- dplyr::select


### read dat----------------------------------
census_files_path <- "D:/census_dat_1990_to_2020"
shapefiles <- list.files(path = census_files_path, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
shapefiles_without_county <- shapefiles[!grepl("county", shapefiles, ignore.case = TRUE)]

read_shapefile <- function(shp) {
  st_read(shp, quiet = TRUE)
}

shapefile_list <- lapply(shapefiles_without_county, read_shapefile)
dat <- fread("D:/census_dat_1990_to_2020/nhgis0002_csv/nhgis0002_ts_nominal_tract.csv")


### fns-------------------------------------
dat_to_geom <- function(shapefile_num, geoid_col, pop_col){
  gis <- shapefile_list[[shapefile_num]]
  
  # Convert geoid_col and pop_col to string if they are not
  geoid_col <- deparse(substitute(geoid_col))
  pop_col <- deparse(substitute(pop_col))
  
  dat_out <- dat %>%
    select(!!sym(geoid_col), !!sym(pop_col)) %>%  # Use sym() to convert strings to symbols
    left_join(gis, by = setNames("GISJOIN", geoid_col)) %>%
    na.omit() %>%
    st_sf(sf_column_name = "geometry")
  
  return(dat_out)
}


### agg dat---------------------------------------
dat90 <- dat_to_geom(1, GJOIN1990, AV0AA1990)
dat20 <- dat_to_geom(4, GJOIN2020, AV0AA2020)

states <- states()

nj <- states %>% filter(NAME == "New Jersey")
nj <- st_transform(nj, crs = st_crs(dat20))

dat90_nj <- dat90[nj, ]
dat20_nj <- dat20[nj, ]

dat90_nj_to_20 <- interpolate_pw(
  from = dat90_nj,
  to = dat20_nj,
  to_id = "GJOIN2020",
  weights = dat20_nj,
  weight_column = "AV0AA2020",
  crs = st_crs(dat20),
  extensive = TRUE
)

full_dat <- dat20_nj %>%
  left_join(st_drop_geometry(dat90_nj_to_20)) %>%
  mutate(pct_thirty_yr_pop_change = (AV0AA2020 - AV0AA1990) / AV0AA1990) %>%
  mutate(pct_thirty_yr_pop_change = ifelse(is.infinite(pct_thirty_yr_pop_change) | is.na(pct_thirty_yr_pop_change), NA, pct_thirty_yr_pop_change))


library(tmap)

tm_shape(full_dat) +
  tm_polygons(col = 'pct_thirty_yr_pop_change', border.alpha = 0, palette = "viridis", style = "cont", midpoint = NA) +
tm_shape(nj) +
  tm_borders()+
  tm_layout(frame = FALSE,
            legend.outside = TRUE)

st_write(full_dat, file.path(box_path, "nj_pop_change_1990_to_2020.geojson"))