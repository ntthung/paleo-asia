# ---- Load packages and source files-------------

library(magrittr)
library(ggplot2)
library(cowplot)
library(geosphere)
# library(maptools)
# library(rgdal)
library(sf)
library(data.table)
library(doParallel)
library(ldsr)
library(VSURF)
source('geo_functions.R') # Functions for geographical operations

# Maps and plots -----------------------------------------------

#' Paste "degree West / East" to the x-axis labels
pasteLong <- function(x) {
  ifelse(x < 0, paste0(-x, "\u00B0", "W"), ifelse(x > 0, paste0(x, "\u00B0", "E"), paste0(x, '\u00B0')))
}
#' Paste "degree North / South" to the y-axis labels
pasteLat <- function(x) {
  ifelse(x < 0, paste0(-x, "\u00B0", "S"), ifelse(x > 0, paste0(x, "\u00B0", "N"), paste0(x, '\u00B0')))
}

theme_set(theme_cowplot(font_size = 10, font_family = "", line_size = 0.2))