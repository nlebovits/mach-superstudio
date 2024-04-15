##### FUNCTIONS----------------------------------------
clust_dat_prep <- function(data, vars) {
  out_data <- data %>%
    st_drop_geometry() %>%
    select(all_of(vars), "pwsid") %>%
    na.omit() %>%
    mutate(across(contains("delta"), ~ .x + 1)) %>% # add 1 to delta columns
    mutate(across(contains("delta"), log1p), # log transform delta columns
           across(contains("score"), log1p), # log transform score_2020 columns
           across(all_of(vars), ~case_when(
             cur_column() == "pct_thirty_yr_pop_change" ~ sign(.x) * log1p(abs(.x)),
             cur_column() %in% c("ssa_intakes", "DesalVolMGD", "utilcount_20m") ~ ifelse(.x > 0, 1, 0),
             TRUE ~ log(.x + 0.0001)
           ))) %>%
    mutate_if(is.numeric, scale) # standardize numeric columns
  
  return(out_data)
}


maj_vote_2_map <- function(cc, data) {
  maj_vote <- data.frame(cc)
  maj_vote$final_class <- majority_voting(maj_vote, is.relabelled = FALSE)
  
  data$final_class <- maj_vote$final_class
  final_class_df <- data %>%
    select(geoid, final_class) %>%
    mutate(final_class = as.character(final_class))
  
  final_class_df <- inner_join(final_class_df, dat, by = 'geoid') %>% st_sf()
}


# Define clustering iteration function
iterate <- function(vars, nk){
  iter_dat <- clust_dat_prep(dat, vars)
  cc <- consensus_cluster(iter_dat %>% select(-pwsid), nk = nk, reps = 10, algorithms = 'km', progress = TRUE)
  final_class_df <- maj_vote_2_map(cc, iter_dat)
  
  return(final_class_df)
}



### function to load base data given a list of desired sttates-----------------------------
load_suppliers <- function(states_vector){
  full_dataset_path <- file.path(box_outpath, "joined_data.RData")
  dat <- readRDS(full_dataset_path)
  dat <- dat %>% 
    dplyr::filter(state_code %in% states_vector,
                  population_served_count > 500) %>%
    st_transform(crs = crs) %>%
    dplyr::select(-c(geometry_lat, geometry_long)) %>% 
    mutate(tier = as.character(tier),
           relocated_centroid = as.character(relocated_centroid))
  
  return(dat)
}


### function for custom plots -------------------------------------------------------------
plot_cor <- function(select_vars){
  corrplot(cor(dat_no_geom %>% dplyr::select(all_of(select_vars)) %>% na.omit()), 
           method = "number", 
           type = "lower", 
           tl.col = "black", 
           tl.cex = 0.7, 
           number.cex = .5)
}

elbow_plot <- function(vars, set_name){
  data_frame <- as.data.frame(dat_no_geom_standard)
  print(
    fviz_nbclust(data_frame |> dplyr::select(all_of(vars)), kmeans, method = "wss") +
      ggtitle(paste("Elbow plot for", set_name))
  )
}



### functions for aggregating data to clusters-----------------------------------------
aggregate_data <- function(data, vars_to_keep, cluster_variable, standardized = TRUE) {
  
  if (standardized) {
    grouped_data <-
      aggregate(data[vars_to_keep], by = list(cluster = data[[cluster_variable]]), FUN = mean)
  } else {
    grouped_data <-
      aggregate(data[vars_to_keep], by = list(cluster = data[[cluster_variable]]), FUN = mean)
  }
  return(grouped_data)
}


# Define a function for styling the data
style_data <- function(data) {
  styled_data <- data %>%
    mutate(across(where(is.numeric), ~ round(., 2))) %>%
    mutate(across(where(is.numeric), ~ color_bar("lightblue")(.))) %>%
    dplyr::select(cluster, everything())
  return(styled_data)
}


# Define a function for generating the table with formatting
generate_table <- function(data) {
  table_html <- data %>%
    kable("html", escape = FALSE) %>%
    kable_styling("hover", full_width = TRUE) %>%
    column_spec(ncol(data), width = "3cm")
  return(table_html)
}


### function to style table data-----------------------------------------
kablerize <- function(data){
  data %>% 
    kable("html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12)
}

### function to generate breaks for qualitative classification-----------------------------------------
get_validation_clusts <- function(data){
  vars <- c("medIncome", "pctBlack", "pctHispanic", "pctLackPlumbing", "population_served_count", "SDWAviolations", "healthViolations", "SOVI_SCORE", "RESL_SCORE", "pct_urban")
  df <- data.frame(var = vars, mean = numeric(length(vars)), sd = numeric(length(vars)))
  
  for (var in vars){
    df$mean[df$var == var] <- mean(data[[var]], na.rm = TRUE)
    df$sd[df$var == var] <- sd(data[[var]], na.rm = TRUE)
  }
  
  return(df)
}

### function to roughly classify clusters based on data-----------------------------------------
interpret_cluster <- function(column_value, variable_name, valid_clusts) {
  mean_val <- valid_clusts$mean[valid_clusts$var == variable_name]
  sd_val <- valid_clusts$sd[valid_clusts$var == variable_name]
  
  if (length(mean_val) == 0 || length(sd_val) == 0) {
    stop("Variable name not found in valid_clusts!")
  }
  
  if (column_value > (mean_val + 0.5 * sd_val)) {
    return("Very High")
  } else if (column_value < (mean_val - 0.5 * sd_val)) {
    return("Very Low")
  } else if (column_value > (mean_val + 0.25 * sd_val)) {
    return("High")
  } else if (column_value < (mean_val - 0.25 * sd_val)) {
    return("Low")
  } else {
    return("Medium")
  }
}




### function to check if any variables in the dataframe have zero variance-----------------------------------------
find_zero_variance_vars <- function(df) {
  # Identify numeric columns with zero variance
  zero_variance_vars <- sapply(df, function(col) {
    if (is.numeric(col) && var(col, na.rm = TRUE) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Return the names of those columns
  return(names(df)[zero_variance_vars])
}



box_plot <- function(data, var, color){
  title <- paste0("Distribution of ", var)
  suppressMessages(
    ggplot(data) +
      geom_boxplot(aes_string(y = var, x = , color = color, fill = color), alpha = 0.3) +
      scale_color_manual(values = cluster_palette, name = "Cluster") +
      scale_fill_manual(values = cluster_palette, name = "Cluster") +
      #scale_x_log10() +
      labs(title = title) +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(family = "Times", size = 16, face = "bold"),
            legend.text = element_text(family = "Times", size = 12),
            axis.text = element_text(family = "Times", size = 12))
  )
}

### function to calculate adjusted rand index across each clustering run

# what do I need?

# ARI for each clustering run
# then average ARI across all runs for a given set of variables (each financial, physical, and climate)

## allison also wants to know if taking out a variable would change clustering