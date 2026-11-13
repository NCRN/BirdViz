# create a reproducible R environment for this project
# the environment is independent from other projects
#
# install.packages('renv')
library(renv)
renv::init()
pkgs <- c(
  'shiny',
  'leaflet',
  'dplyr',
  'magrittr',
  'purrr',
  'DT',
  'tidyr',
  'shinyjs',
  'htmlwidgets',
  'Hmisc',
  'jsonlite',
  'readr'
)
renv::install(pkgs, rebuild=T, prompt=F)
renv::install("NCRN/NCRNBirds@master", rebuild = TRUE, prompt = FALSE)
renv::snapshot()
# restart your R session (ctrl + shift + F10)
