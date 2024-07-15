# This script installs the required packages for this project. It should only need to be run once
# before running the rest of the scripts.

required_packages <- c(
  "sf",
  "rnaturalearth",
  "remotes",
  # This one gave an error trying to install: "Error: package 'rnaturalearthhires' is not available"
  # The line at the bottom installs it using GitHub.
  # "rnaturalearthhires",
  "rnaturalearthdata",
  "tmap",
  "osmdata",
  "tidycensus",
  "dplyr",
  "readr",
  "tidyr",
  "ggplot2",
  "sjPlot"
)

# Must type 'Y' in the console and hit enter when asked "Do you want to proceed? [Y/n]:"
install.packages(required_packages)


# Installing the `rnaturalearthhires` package, which gave an error
remotes::install_github("ropensci/rnaturalearthhires")