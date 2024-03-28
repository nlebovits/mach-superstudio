library(tidyverse)
library(tigris)
library(sf)
library(janitor)
library(tidycensus)

options(tigris_use_cache = TRUE, scipen = 999)
library(tmap)

# municipalities <- 

nj <- states() %>% filter(NAME == "New Jersey")



nj_places <- places(state = "NJ")

nj_urban_areas <- urban_areas() # %>% filter(STATEFP == 34)

nj_county_subdivisions <- county_subdivisions("NJ") %>% clean_names()

nj_muni_pop <- read_csv('nj_muni_pop_1940_2020.csv') %>% clean_names()

combined <- full_join(nj_county_subdivisions,
                      nj_muni_pop,
                      by = c("namelsad" = "municipality"))



install.packages("FedData")
library(FedData)
library(terra)

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

combined2 <- st_transform(combined, st_crs(nj_nlcd)) %>% filter(!st_is_empty(geometry))

combined_nlcd <- terra::extract(nj_nlcd, combined2)

combined_nlcd |> 
  group_by(ID, levels) |>
  count()

install.packages('exactextractr')
library(exactextractr)

nj_county_subdivisions$land_cover_frac <- exact_extract(nj_nlcd, nj_county_subdivisions, 'frac')


# tm_shape(combined) +
#   tm_polygons(col = "x2000", palette = "viridis")



v10 <- load_variables(2010, "sf1")
acs20 <- load_variables(2020, "acs5")

vacancy20 <- get_acs(geography = "county subdivision",
                     state = "NJ",
                     variables = c("B25004_001",
                                   "B25004_008"),
                     year = 2020,
                     output = 'wide') %>%
            rename(tot = B25004_001E,
                   vac = B25004_008E) %>%
            mutate(pct_res_vac = vac / tot * 100) %>%
            clean_names() %>%
            select(geoid, pct_res_vac)

pop20 <- get_decennial(geography = "county subdivision",
                       state = "NJ",
                       variables = "P1_001N", 
                       year = 2020,
                       sumfile = "dhc",
                       geometry = TRUE)%>% 
  rename(tot_pop_2020 = value)

pop10 <- get_decennial(geography = "county subdivision",
                       state = "NJ",
                       variables = "P001001", 
                       year = 2010,
                       sumfile = "sf1") %>% 
  rename(tot_pop_2010 = value) %>%
  select(GEOID, tot_pop_2010)

pop00 <- get_decennial(geography = "county subdivision",
                       state = "NJ",
                       variables = "P001001", 
                       year = 2000,
                       sumfile = "sf1") %>% 
        rename(tot_pop_2000 = value) %>%
        select(GEOID, tot_pop_2000)

pop00to20 <- pop20 %>%
              left_join(pop10, by = "GEOID") %>%
              left_join(pop00, by = "GEOID") %>%
                mutate(pct_pop_change = (tot_pop_2020 - tot_pop_2000) / tot_pop_2000)

# tm_shape(st_make_valid(pop00to20)) +
#   tm_polygons(col = "pct_pop_change", palette = "viridis", style = "jenks")

# Preparing the data
full <- combined %>%
  full_join(st_drop_geometry(pop00to20), by = c("geoid" = "GEOID")) %>%
  select(geoid, namelsad, NAME, x1940, x1950, x1960, x1970, x1980, x1990, tot_pop_2000, tot_pop_2010, tot_pop_2020) %>%
  rename(tot_pop_1940 = x1940, tot_pop_1950 = x1950, tot_pop_1960 = x1960, tot_pop_1970 = x1970, tot_pop_1980 = x1980, tot_pop_1990 = x1990) %>%
  st_make_valid() %>%
  mutate(size_class = case_when(
    tot_pop_2020 > 50000 ~ "Large",
    tot_pop_2020 <= 50000 & tot_pop_2020 > 10000 ~ "Medium",
    tot_pop_2020 <= 10000 & tot_pop_2020 > 5000 ~ "Small",
    TRUE ~ "Very Small"
  )) %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(pct_pop_change_1950_to_2020 = (tot_pop_2020 - tot_pop_1950) / tot_pop_1950,
         pct_pop_change_1990_to_2020 = (tot_pop_2020 - tot_pop_1990) / tot_pop_1990)






### -------------------------------------------------
library(ggpubr)

filtered_data <- full %>%
  filter(pct_pop_change_1950_to_2020 < 500, pop_change_lag < 200)

# Use ggscatter for plotting with separate correlation coefficients for each facet
ggscatter(
  data = filtered_data, 
  x = 'pct_pop_change_1950_to_2020', 
  y = 'pop_change_lag', 
  add = "reg.line", 
  conf.int = TRUE, 
  cor.coef = TRUE, 
  cor.method = "spearman",
  facet.by = "size_class", 
  short.panel.labs = FALSE
)

hmm <- full %>%
        filter(is.na(tot_pop_1940))

tm_shape(st_make_valid(full)) +
  tm_polygons(col = "pct_pop_change_1950_to_2020", style = "jenks", palette = "viridis")

long_data <- full %>%
  pivot_longer(
    cols = starts_with("tot_pop_"),
    names_to = "year",
    values_to = "tot_pop",
    names_prefix = "tot_pop_",
    names_pattern = "(\\d+)$"
  ) %>%
  mutate(year = as.numeric(year)) %>%
  st_make_valid()

### ----------------------------------------------------



### -----------------------------------------------------

ggscatter(
  data = filtered_data, 
  x = 'tot_pop', 
  y = 'pct_pop_change_1990_to_2020', 
  add = "reg.line", 
  conf.int = TRUE, 
  cor.coef = TRUE, 
  cor.method = "spearman",
  facet.by = "size_class", 
  short.panel.labs = FALSE
)

# Now, plot with conditional coloring
ggplot(long_data %>% filter(!is.na(color_group)), aes(x = year, y = tot_pop, group = namelsad)) +
  geom_line(aes(color = color_group)) + # Color based on the new color_group column
  scale_color_identity() + # Use the actual color names stored in color_group
  stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", size = 1) + # Average line
  facet_wrap(~size_class + color_group, scales = "free", nrow = 2) +
  theme(legend.position = "none") +
  theme_minimal()





