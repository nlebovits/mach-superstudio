library(tidyverse)
library(tigris)
library(sf)
library(janitor)
library(tidycensus)
library(tmap)
library(FedData)
library(terra)
library(exactextractr)


options(tigris_use_cache = TRUE, scipen = 999)


### import muni pop data------------------------------------
nj_muni_pop <- read_csv('nj_muni_pop_1940_2020.csv') %>% clean_names()


### import census boundaries + data------------------------------------

# boundaries
nj_county_subdivisions <- county_subdivisions("NJ") %>% clean_names() %>% select(geoid, namelsad)



combined <- full_join(nj_county_subdivisions,
                      nj_muni_pop,
                      by = c("namelsad" = "municipality"))

# census data

filtered_acs20 <- acs20 %>%
  filter(str_detect(name, " B25136"))

vacancy20 <- get_acs(geography = "county subdivision",
                     state = "NJ",
                     variables = c("B25002_001", # total units
                                   "B25002_003"), # vacant units
                     year = 2020,
                     output = 'wide') %>%
  rename(tot = B25002_001E,
         vac = B25002_003E) %>%
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

nj_county_subdivisions <- county_subdivisions("NJ") %>% clean_names() %>% select(geoid, namelsad)

nj_county_subdivisions$land_cover <- exact_extract(nj_nlcd, nj_county_subdivisions, 'frac')

nj_muni_nlcd <- nj_county_subdivisions %>% st_drop_geometry()

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
nj_muni_nlcd <- nj_muni_nlcd %>% clean_names()


### combine data---------------------------------

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
         pct_pop_change_1950_to_1990 = (tot_pop_1990 - tot_pop_1950) / tot_pop_1950,
         pct_pop_change_1980_to_2020 = (tot_pop_2020 - tot_pop_1980) / tot_pop_1980,
         pct_pop_change_1990_to_2020 = (tot_pop_2020 - tot_pop_1990) / tot_pop_1990)

final <- full %>%
          full_join(nj_muni_nlcd %>% select(-place_name), by = "geoid") %>%
          full_join(vacancy20, by = "geoid")

ggplot(final %>% filter(pct_pop_change_1950_to_2020 < 1000), aes(x = pct_pop_change_1950_to_2020, y = pct_res_vac)) +
  geom_point() +
  geom_smooth(method = "lm")

ggscatter(
  data = final %>% filter(pct_pop_change_1950_to_1990 < 300), 
  x = 'pct_pop_change_1950_to_2020', 
  y = 'developed_high_intensity', 
  add = "reg.line", 
  conf.int = TRUE, 
  cor.coef = TRUE, 
  cor.method = "spearman",
  facet.by = "size_class", 
  short.panel.labs = FALSE
)

ggscatter(
  data = final %>% filter(pct_pop_change_1950_to_1990 < 300), 
  x = 'pct_pop_change_1950_to_2020', 
  y = 'pct_pop_change_1980_to_2020', 
  add = "reg.line", 
  conf.int = TRUE, 
  cor.coef = TRUE, 
  cor.method = "spearman",
  # facet.by = "size_class", 
  short.panel.labs = FALSE
)

ggscatter(
  data = final %>% filter(pct_pop_change_1950_to_1990 < 300), 
  x = 'pct_pop_change_1950_to_2020', 
  y = 'pct_res_vac', 
  add = "reg.line", 
  conf.int = TRUE, 
  cor.coef = TRUE, 
  cor.method = "spearman",
  facet.by = "size_class", 
  short.panel.labs = FALSE
)

tm_shape(final) +
  tm_polygons(col = "pct_res_vac", palette = "viridis", style = "jenks") +
  tm_layout(legend.outside = TRUE,
            frame = FALSE)

tm_shape(final) +
  tm_polygons(col = "emergent_herbaceous_wetlands", palette = "viridis", style = "jenks") +
  tm_layout(legend.outside = TRUE,
            frame = FALSE)

ggplot(final, aes(x = developed_high_intensity, y = size_class)) +
  # geom_violin() +
  geom_boxplot() +
  theme_minimal()

### fit model------------------------------------

# Load necessary libraries
library(randomForest)
library(caret)
library(dplyr)

# Assuming 'final' is your dataframe
# Selecting predictors and the target variable
predictors <- c(paste0("tot_pop_", c(#"1940", "1950", "1960", "1970", "1980", "1990", 
                                     "2000", "2010")),
                "open_water", "developed_open_space", "developed_low_intensity", "developed_medium_intensity",
                "developed_high_intensity", "barren_land_rock_sand_clay", "deciduous_forest",
                "evergreen_forest", "mixed_forest", "shrub_scrub", "grassland_herbaceous", 
                "pasture_hay", "cultivated_crops", "woody_wetlands", "emergent_herbaceous_wetlands")

# Assuming you're predicting 'tot_pop_2020'
target <- "tot_pop_2020"

# Filter out any rows with NA in the target column
data_for_modeling <- final %>%
  select(all_of(c(predictors, target))) %>%
  na.omit() %>%
  st_drop_geometry()

# Splitting data into training and testing sets
set.seed(123) # For reproducibility
training_indices <- createDataPartition(data_for_modeling[[target]], p = 0.8, list = FALSE)
train_data <- data_for_modeling[training_indices, ]
test_data <- data_for_modeling[-training_indices, ]

# Training the Random Forest model
rf_model <- randomForest(x = train_data[, predictors], y = train_data[[target]], ntree = 500)

# Predicting on the test set
predictions <- predict(rf_model, newdata = test_data[, predictors])

# Evaluating the model
model_evaluation <- postResample(pred = predictions, obs = test_data[[target]])

# Print model evaluation
print(model_evaluation)


# Create a data frame with actual and predicted values for plotting
plot_data <- data.frame(Actual = test_data[[target]], Predicted = predictions)

# Plot actual vs predicted values
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +  # Add points with a bit of transparency
  geom_abline(color = "red", linetype = "dashed") +  # Add a dashed red abline for reference
  labs(title = "Actual vs Predicted Values", x = "Actual", y = "Predicted") +
  theme_minimal() #+  # Use a minimal theme
  # geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid")  # Add a linear regression line



### back project------------------------------------
years_to_impute <- c("1940", "1950", "1960", "1970", "1980", "1990")
land_cover_predictors <- c("open_water", "developed_open_space", "developed_low_intensity",
                           "developed_medium_intensity", "developed_high_intensity",
                           "barren_land_rock_sand_clay", "deciduous_forest", "evergreen_forest",
                           "mixed_forest", "shrub_scrub", "grassland_herbaceous", "pasture_hay",
                           "cultivated_crops", "woody_wetlands", "emergent_herbaceous_wetlands")

for (year in years_to_impute) {
  target_column <- paste0("tot_pop_", year)
  
  # Exclude the current target year from the predictors
  current_predictors <- c(land_cover_predictors, paste0("tot_pop_", setdiff(years_to_impute, year)))
  
  # Identify rows with NA for the target year
  na_indices <- is.na(final[[target_column]])
  
  # Skip if no NAs for the current year
  if (sum(na_indices) == 0) next
  
  # Prepare data for prediction
  predict_data <- final[na_indices, current_predictors, drop = FALSE]
  
  # Ensure all predictors are numeric
  predict_data[] <- lapply(predict_data, function(x) as.numeric(as.character(x)))
  
  # Impute missing values in predictors. Here using mean imputation as an example; adjust as necessary.
  predict_data[] <- lapply(predict_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
  
  # Check if NA imputation was successful
  if(any(sapply(predict_data, function(x) any(is.na(x))))) {
    stop("NA values still present in predictors after imputation.")
  }
  
  # Predict the missing values
  predicted_values <- predict(rf_model, newdata = predict_data)
  
  # Insert the predicted values back into the original dataframe
  final[na_indices, target_column] <- predicted_values
}




### pull in NRI data---------------------------
nri <- st_read("./data/usa/NRI_Shapefile_CensusTracts.shp")
nj_nri <- nri %>% filter(STATE == "New Jersey") %>% 
                  mutate(ALR_TOT = ALR_VALA + ALR_VALB + ALR_VALP) %>% 
                  select(ALR_TOT)

nj_nri <- st_interpolate_aw(nj_nri, st_transform(nj_county_subdivisions, st_crs(nj_nri)), extensive = TRUE)

tm_shape(nj_nri) +
  tm_polygons(col = "ALR_TOT", palette = 'viridis', style = 'jenks')

usa_tracts_1910 <- st_read('./data/nhgis0003_shape/US_tract_1910.shp')

tm_shape(usa_tracts_1910) +
  tm_polygons(col = "ALR_TOT", palette = 'viridis', style = 'jenks')

csv_nj <- read_csv('~/GitHub/mach-superstudio/data/nhgis0004_csv/nhgis0004_ts_nominal_cty_sub.csv') %>% filter(STATE == "New Jersey")

csv_nj_long <- csv_nj %>%
  select(geoid = GJOIN2020, contains("B78AA")) %>%
  pivot_longer(
    cols = -geoid, 
    names_to = "year", 
    values_to = "pop"
  ) %>%
  mutate(year = sub("B78AA", "", year))%>%
  mutate(size_class = case_when(
    pop > 50000 ~ "Large",
    pop <= 50000 & pop > 10000 ~ "Medium",
    pop <= 10000 & pop > 5000 ~ "Small",
    TRUE ~ "Very Small"
  ))

ggplot(csv_nj_long, aes(x = year, y = pop, group = geoid)) +
  geom_line(aes(color = "darkgrey")) + # Color based on the new color_group column
  scale_color_identity() + # Use the actual color names stored in color_group
  stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", size = 1) + # Average line
  facet_wrap(~size_class, scales = "free", nrow = 2) +
  theme(legend.position = "none") +
  theme_minimal()

### scratch-------------------------------------
# municipalities <- 








# tm_shape(combined) +
#   tm_polygons(col = "x2000", palette = "viridis")




tm_shape(final) +
  tm_polygons(col = "pct_pop_change_1950_to_2020", palette = "viridis", style = "jenks")

tm_shape(final) +
  tm_polygons(col = "tot_pop_2000", palette = "viridis", style = "jenks")

tm_shape(final) +
  tm_polygons(col = "tot_pop_2020", palette = "viridis", style = "jenks")

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

ggplot(long_data, aes(x = year, y = tot_pop, group = namelsad)) +
  geom_line(aes(color = "darkgrey")) + # Color based on the new color_group column
  scale_color_identity() + # Use the actual color names stored in color_group
  stat_summary(fun = median, geom = "line", aes(group = 1), color = "darkred", size = 1) + # Average line
  facet_wrap(~size_class, scales = "free", nrow = 2) +
  theme(legend.position = "none") +
  theme_minimal()

hmm <- long_data %>% filter(is.na(tot_pop))

hmm2 <- full %>% filter(is.na(tot_pop_1940))

