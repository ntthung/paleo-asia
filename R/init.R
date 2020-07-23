# ---- Load packages and source files-------------

library(ldsr)                       # Streamflow reconstruction
library(data.table)                 # Data wrangling
library(ggplot2)                    # Plotting
library(patchwork)                  # Arranging plots
library(cowplot)                    # Arranging and annotating plots
library(magrittr)                   # Piping
library(VSURF)                      # Input variable selection
library(sf)                         # Mapping
library(geosphere)                  # Geodetic distance calculation
library(foreach)                    # Parallel computations
library(doFuture)                   # Parallel computations
library(future)                     # Parallel computations
library(glue)                       # String handling

# Maps and plots ---------------------------------------------------

pasteLong <- function(x) ifelse(x < 0, paste0(-x, "\u00B0", "W"), ifelse(x > 0, paste0(x, "\u00B0", "E"), paste0(x, '\u00B0')))
pasteLat <- function(x) ifelse(x < 0, paste0(-x, "\u00B0", "S"), ifelse(x > 0, paste0(x, "\u00B0", "N"), paste0(x, '\u00B0')))
blues <- RColorBrewer::brewer.pal(9, 'Blues')

pasteKWF <- function(x) ifelse(x == '0.10', paste('kwf =', x), x)
pasteP <- function(x) ifelse(x == '0', paste('p =', x), x)

megadroughts <- data.table(firstYear = c(1345, 1401, 1638, 1756, 1790, 1876),
                           finalYear = c(1374, 1425, 1641, 1768, 1796, 1878),
                           name = c('Angkor I', 'Angkor II', 'Ming Dynasty', 
                                    'Strange Parallel', 'East India', 'Great'))

# Conversion functions to work with plot labels
ID_to_name_basin <- function(x) instQmeta[x, paste0(name, ' (', basin, ')')]
ID_to_name <- function(x) instQmeta[x, name]
ID_to_basin <- function(x) instQmeta[x, basin]
trim_ID <- function(x) paste0(substr(x, 1, 3), substr(x, 7, 10))

my_theme <- theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'))
theme_set(my_theme)

# Other utilities ---------------------------------

distance <- function(a, b) sqrt(sum((a-b)^2))

normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}

standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

hinkley <- function(x, type = 2) {
  scale <- if (type == 1) sd(x, na.rm = TRUE) else diff(quantile(x, c(0.25, 0.75), na.rm = TRUE, type = 8, names = FALSE))
  (mean(x, na.rm = TRUE) - median(x, na.rm = TRUE)) / scale
}

lapplyrbind <- function(x, fun, ..., id = 'id') rbindlist(lapply(x, fun, ...), idcol = id)

absRange <- function(x) {
  r <- range(x)
  absMax <- max(abs(r))
  c(-absMax, absMax)
}

`%ni%` <- Negate(`%in%`)

regionFillPal <- c(CA  = '#ffd5a2',
                   EA  = '#b2df8a',
                   WA  = '#cedfbe',
                   CN  = '#c9dae3',
                   SEA = '#e3cce2',
                   SA  = '#fdf5ca')