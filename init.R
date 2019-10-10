# ---- Load packages and source files-------------

library(magrittr)
library(ggplot2)
library(cowplot)
library(geosphere)
# library(maptools)
# library(rgdal)
# library(sf)
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

# Other utilities ---------------------------------

# distance <- function(a, b) sqrt(sum((a-b)^2))
# 
# normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}
# 
# rm_null <- function(x) x[!sapply(x, is.null)]
# 
# make_cormat <- function(dt, group.name, var.name, type = 'both') {
#   
#   sigma <- dt %>% 
#     dcast(year ~ get(group.name), value.var = var.name, drop = FALSE) %>%
#     .[, 2:ncol(.)] %>%
#     cor(use = 'pairwise.complete.obs') %>% 
#     melt() %>% 
#     as.data.table() %>% 
#     .[, .(v1 = as.character(Var1),
#           v2 = as.character(Var2),
#           value)]
#   ans <- switch(type,
#          'upper' = {sigma[v1 > v2]},
#          'lower' = {sigma[v1 <= v2]},
#          'both' = {sigma})
#   ans
# }
