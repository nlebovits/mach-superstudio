library(utils)

# this file contains 

# 1) defined functions
# 2) general variables

# (this order makes sense because the functions are called to create some of the variables)


### functions

# load packages and install missing ones
# this is useful so that people can run scripts from anywhere without needing to reinstall stuff
install_and_load_packages <- function(required_packages) {
  installed_packages <- installed.packages()[, "Package"]
  missing_packages <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    if ("boxrdrive" %in% missing_packages) {
      suppressWarnings({
        remotes::install_github("r-box/boxrdrive")
      })
      if (!require(boxrdrive, quietly = TRUE)) {
        warning("'boxrdrive' package is not available for this version of R. Please check the installation.")
      }
    }
    install.packages(setdiff(missing_packages, "boxrdrive"), quietly = TRUE)
  }
  
  for(package.i in required_packages){
    suppressPackageStartupMessages(
      library(
        package.i, 
        character.only = TRUE
      )
    )
  }
}



# load required packages to actually declare these variables
required_packages <- c("tidyverse", "here", "boxrdrive", "sf", "lwgeom")
install_and_load_packages(required_packages)

options(scipen = 999, tigris_use_cache = TRUE)

project_directory <- here()

general_palette <- c(
  "1" = "#052238",
  "2" = "#00b1bf",
  "3" = "#477119",
  "4" = "#9abf4e",
  "5" = "#ff4f1e",
  "6" = "#ffb81e",
  "7" = "#f0ecdc",
  "8" = "#d7c5b3",
  "9" = "#613029",
  "10" = "#281a15"
)

cluster_palette <- c(
  "3" = "#9abf4e",
  "2" = "#ffb81e",
  "1" = "#ff4f1e"
)