#' Get the boundaries of areas of significant correlation
#' 
#' Returns columsn of top, down, bottom, left, TRUE for that respective border
#' cor.dt must have long and lat as keys to make use of data.table's speed
signif_area <- function(cor.dt, dx, dy) {
  
  cor.dt[{signif}, list(long, lat,
                        top    = .SD[.(long,      lat + dy), is.na(point)],
                        bottom = .SD[.(long,      lat - dy), is.na(point)],
                        left   = .SD[.(long - dx, lat     ), is.na(point)],
                        right  = .SD[.(long + dx, lat     ), is.na(point)])]
}


#' Plot the SST correlation map
#' 
#' @param sst.cor A data.table containing correlation with every SST point
#' @param signif.cor Where correlation is significant (p < 0.05)
#' @param land An sf object representing the land area
#' @param row.season Whether to layout the plot by row or by column
#' @param limits Limits for the colour scale
#' @return A plot, one row (or column) for each season, colour coded for correlation, and regions with significant correlations are encircled.
plot_sst_cor <- function(sst.cor, signif.cor, land, row.season = TRUE, limits = NULL) {
  
  # To keep the colour scale symmetric
  if (is.null(limits)) limits <- absRange(sst.cor$cor)
  p <- ggplot(sst.cor) +
    geom_raster(aes(long, lat, fill = cor)) +
    # Significance boxes
    geom_segment(aes(x = long - 1, xend = long + 1, y = lat + 1, yend = lat + 1), 
                 size = 0.1,
                 data = signif.cor[top == TRUE]) +
    geom_segment(aes(x = long - 1, xend = long + 1, y = lat - 1, yend = lat - 1), 
                 size = 0.1,
                 data = signif.cor[bottom == TRUE]) +
    geom_segment(aes(x = long - 1, xend = long - 1, y = lat - 1, yend = lat + 1), 
                 size = 0.1,
                 data = signif.cor[left == TRUE]) +
    geom_segment(aes(x = long + 1, xend = long + 1, y = lat - 1, yend = lat + 1), 
                 size = 0.1,
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
    labs(x = NULL, y = NULL) +
    theme_cowplot(font_size = 12) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    #       legend.position = 'bottom',
    #       legend.key.width = unit(2, 'cm'),
    #       legend.title = element_text(hjust = 1, vjust = 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1, limits = limits,
                         breaks = scales::pretty_breaks(7)) +
    coord_quickmap(ylim = c(-60, 60))
  
  if (row.season) 
    p + facet_grid(vars(season), vars(river), switch = 'y')
  else
    p + facet_grid(vars(river), vars(season))
}

#' Calculate correlation with seasonal SST
#' 
#' @param dt Data.table containing streamflow or catchment state time series
#' @param s Name of season
#' @param lag Lag between SST and streamflow
#' @param varName Either `Qa` for streamflow or `X` for catchment state
#' @return A data.table containing correlation and p-values.
get_sst_cor <- function(Q, s, lag, varName) {
  
  # Lag from the point of view of streamflow
  # Lag = -1 means SST of last year and streamflow of this year
  
  commonYears <- intersect(Q$year, 1855:2012)
  N <- length(commonYears)
  if (lag == 0) {
    sstYear <- qYear <- commonYears
  } else if (lag == -1) {
    sstYear <- commonYears[1:(N-1)]
    qYear <- commonYears[2:N]
  } else if (lag == -2) { # lag == -2
    sstYear <- commonYears[1:(N-2)]
    qYear <- commonYears[3:N]
  } else { # lab == +1
    sstYear <- commonYears[2:N]
    qYear <- commonYears[1:(N-1)]
  }
  river <- Q[1, river]
  lagChar <- if (lag == 0) '' else if (lag == 1) ' (+1)' else paste0(' (', lag, ')')
  Q <- Q[year %in% qYear, get(varName)]
  DT <- sst[year %in% sstYear & season == s][, q := Q, by = .(long, lat)]
  
  ans <- DT[,
            {
              ct <- cor.test(q, sst, na.rm = TRUE)
              list(cor = ct$estimate, p.value = ct$p.value)
            },
            by = .(long, lat)
            ][, ':='(season = paste0(s, lagChar),
                     river = river)]
  ans[]
}
