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

  for (package.i in required_packages) {
    suppressPackageStartupMessages(
      library(
        package.i,
        character.only = TRUE
      )
    )
  }
}



# load required packages to actually declare these variables
required_packages <- c("tidyverse", "here", "boxrdrive", "sf", "lwgeom", "monochromeR")
install_and_load_packages(required_packages)

options(scipen = 999, tigris_use_cache = TRUE)

project_directory <- here()

general_palette <- c(
  "#052238",
  "#00b1bf",
  "#477119",
  "#9abf4e",
  "#ff4f1e",
  "#ffb81e",
  "#f0ecdc",
  "#d7c5b3",
  "#613029",
  "#281a15"
)

cluster_palette <- c(
  "2" = "#9abf4e",
  "1" = "#ffb81e",
  "3" = "#ff4f1e"
)

mono_5_orange <- rev(generate_palette(general_palette[5], modification = "go_lighter", n_colours = 5, view_palette = FALSE))
mono_5_green <- rev(generate_palette(general_palette[3], modification = "go_lighter", n_colours = 5, view_palette = FALSE))
