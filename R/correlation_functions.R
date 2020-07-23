#' Plot the SST correlation map
#' 
#' @param sst.cor A data.table containing correlation with every SST point
#' @param signif.cor Where correlation is significant (p < 0.05)
#' @param land An sf object representing the land area
#' @param row.season Whether to layout the plot by row or by column
#' @return A plot, one row (or column) for each season, colour coded for correlation, and regions with significant correlations are encircled.
plot_sst_cor <- function(sst.cor, signif.cor, land, title = NULL, row.season = TRUE, 
                         limits = NULL, nBreaks = 7) {
  
  # To keep the colour scale symmetric
  if (is.null(limits)) limits <- absRange(sst.cor$rho)
  
  p <- ggplot(sst.cor) +
    geom_raster(aes(long, lat, fill = rho)) +
    # Significance boxes
    geom_segment(aes(x = long - 1, xend = long + 1, y = lat + 1, yend = lat + 1), 
                 size = 0.1, colour = 'gray40',
                 data = signif.cor[top == TRUE]) +
    geom_segment(aes(x = long - 1, xend = long + 1, y = lat - 1, yend = lat - 1), 
                 size = 0.1, colour = 'gray40',
                 data = signif.cor[bottom == TRUE]) +
    geom_segment(aes(x = long - 1, xend = long - 1, y = lat - 1, yend = lat + 1), 
                 size = 0.1, colour = 'gray40',
                 data = signif.cor[left == TRUE]) +
    geom_segment(aes(x = long + 1, xend = long + 1, y = lat - 1, yend = lat + 1), 
                 size = 0.1, colour = 'gray40',
                 data = signif.cor[right == TRUE]) +
    # Land
    geom_segment(aes(x = long - 1, xend = long + 1, y = lat + 1, yend = lat + 1), 
                 size = 0.1, colour = 'gray90',
                 data = sstLand[top == TRUE]) +
    geom_segment(aes(x = long - 1, xend = long + 1, y = lat - 1, yend = lat - 1), 
                 size = 0.1, colour = 'gray90',
                 data = sstLand[bottom == TRUE]) +
    geom_segment(aes(x = long - 1, xend = long - 1, y = lat - 1, yend = lat + 1), 
                 size = 0.1, colour = 'gray90',
                 data = sstLand[left == TRUE]) +
    geom_segment(aes(x = long + 1, xend = long + 1, y = lat - 1, yend = lat + 1), 
                 size = 0.1, colour = 'gray90',
                 data = sstLand[right == TRUE]) +
    labs(x = NULL, y = NULL, title = title) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(2, 'cm'),
          legend.title = element_text(hjust = 1, vjust = 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1, limits = limits,
                         breaks = scales::pretty_breaks(nBreaks)) +
    coord_quickmap()
  
  if (row.season) 
    p + facet_grid(vars(season), vars(river), switch = 'y')
  else
    p + facet_grid(vars(river), vars(season))
}

#' Get the boundaries of areas of significant correlation
#' 
#' Returns columsn of top, down, bottom, left, TRUE for that respective border
signif_area <- function(cor.dt, dx, dy) {
  
  cor.dt[{signif}, list(long, lat,
                        top    = .SD[.(long,      lat + dy), is.na(point)],
                        bottom = .SD[.(long,      lat - dy), is.na(point)],
                        left   = .SD[.(long - dx, lat     ), is.na(point)],
                        right  = .SD[.(long + dx, lat     ), is.na(point)])]
}