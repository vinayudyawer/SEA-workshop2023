## Software Installation

## Packages that are on CRAN

install.packages(c("tidyverse",
                   "sf",
                   "ggspatial",
                   "leaflet",
                   "remotes"))

## Install packages from GitHub and other external repositories

remotes::install_github("r-spatial/mapview", build_vignettes = TRUE)

remotes::install_github("rossdwyer/VTrack", build_vignettes = TRUE)

remotes::install_github('IMOS-AnimalTracking/remora', build_vignettes = TRUE)

install.packages("aniMotum", 
                 repos = c("https://cloud.r-project.org",
                           "https://ianjonsen.r-universe.dev"),
                 dependencies = TRUE)
