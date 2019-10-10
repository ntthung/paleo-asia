#' Get standardized streamflow index (log-transform, substract mean, divide by standard deviation of the inst. period). Group the results by basins of interest and then by regions.
#' 
#' @param rec Reconstruction results, tallied from `read_station`
#' @param instQsummary Mean and standard deviation of log-transformed flow
#' @inheritParams read_station
get_Ystd <- function(rec, instQsummary, instQxy) {
  Ystd <- rec[,
              .(year, 
                Y = {
                  s <- .BY
                  mu <- instQsummary[ID == s, mu]
                  sigma <- instQsummary[ID == s, sigma]
                  (log(Qa) - mu) / sigma
                }),
              by = ID] %>%
    merge(instQxy[, .(region, basin, river, name, ID, code)]) %>% 
    .[order(code, decreasing = TRUE)]
  Ystd[, order := .GRP, by = code]  
  Ystd[]
}

#' Plot flow history heatmap
#' @param Ystd Standardized streamflow index, from `get_Ystd`
#' @param endYear The last yera of the plot (in case we want to plot only the paleo period)
#' @param nSA Number of stations in South Asia
#' @param nSEA Number of stations in Southeast Asia
#' @param nCN Number of stations in China
#' @param nWA Number of stations in West Asia
#' @param nEA Number of stations in East Asia
#' @param nCA Number of stations in Central Asia
flow_history <- function(Ystd, endYear, nSA, nSEA, nCN, nWA, nEA, nCA, 
                         plotLower = TRUE, plotGap = FALSE, raster = TRUE) {
  
  # Function to plot specific periods
  fh_period <- function(firstYear, lastYear, breaks, xPosition = 'bottom', 
                        # rightMargin = 1,
                        showLegend = FALSE, showAreas = FALSE) {
    
    # Heatmap
    ggplot(Ystd[between(year, firstYear, lastYear)], aes(year, order)) +
      { if (raster) geom_raster(aes(fill = Y), colour = NA)
        else geom_tile(aes(fill = Y), width = 1, height = 1, colour = NA) } +
      scale_fill_gradient2(name = 'Standardized streamflow',
                           low = 'salmon4', mid = 'white', high = 'darkgreen', limits = limits) +
      # Region lines
      geom_hline(yintercept = nSA + 0.5, colour = 'gray50', size = 0.3) + 
      geom_hline(yintercept = nSA + nSEA + 0.5, colour = 'gray50', size = 0.3) +
      geom_hline(yintercept = nSA + nSEA + nCN + 0.5, colour = 'gray50', size = 0.3) +
      geom_hline(yintercept = nSA + nSEA + nCN + nWA + 0.5, colour = 'gray50', size = 0.3) + 
      geom_hline(yintercept = nSA + nSEA + nCN + nWA + nEA + 0.5, colour = 'gray50', size = 0.3) + 
      scale_x_continuous(expand = c(0, 0), breaks = breaks, position = xPosition) +
      scale_y_continuous(breaks = c(nSA / 2,
                                    nSA + nSEA / 2,
                                    nSA + nSEA + nCN / 2,
                                    nSA + nSEA + nCN + nWA / 2,
                                    nSA + nSEA + nCN + nWA + nEA / 2,
                                    nSA + nSEA + nCN + nWA + nEA + nCA / 2),
                         labels = c('SA', 'SEA', 'CN', 'WA', 'EA', 'CA'),
                         expand = c(0, 0)) +
      labs(x = NULL, y = 'Streamflow stations') +
      theme(axis.line.y = element_blank(),
            axis.ticks.y = element_blank()) +
      if (showLegend) {
        theme(legend.position = 'top',
              legend.key.width = unit(1.3, 'cm'),
              legend.key.height = unit(0.3, 'cm'),
              legend.box.margin = margin(0, 0, 0, 0))
      } else {
        theme(legend.position = 'none')
      } +
      if (showAreas) {
      theme(axis.text.y = element_text(vjust = 0.5))
      } else {
      theme(axis.text.y = element_blank(),
      axis.title.y = element_blank())
      }
  }  
  
  # Line segments marking special periods
  segments <- data.table(x = c(1257, 1345, 1401, 1452, 1638, 1756, 1790, 1815, 1876),
                         xend = c(1258, 1365, 1425, 1453, 1641, 1768, 1796, 1816, 1878))
  
  # Limits for colour scale
  absYmax <- Ystd[, c(-min(Y), max(Y))] %>% max()
  limits <- c(-absYmax, absYmax)
  
  # Whether special periods will be plotted below the overall plot
  if (plotLower) {
    overall <- fh_period(1200, endYear, 
                         breaks = seq(1200, 2000, 50), xPosition = 'top', #rightMargin = 0,
                         showLegend = TRUE, showAreas = TRUE) +
      geom_segment(aes(x = x, xend = xend, y = 0.2, yend = 0.2), size = 0.3,
                   data = segments, colour = 'steelblue')
    se <- fh_period(1257, 1259, breaks = 1257, showAreas = TRUE)
    ak1 <- fh_period(1345, 1365, breaks = seq(1345, 1365, 5))
    ak2 <- fh_period(1401, 1425, breaks = seq(1405, 1425, 5))
    kwe <- fh_period(1452, 1454, breaks = 1452)
    md <- fh_period(1638, 1641, breaks = 1640)
    sp <- fh_period(1756, 1768, breaks = c(1760, 1765))
    ei <- fh_period(1790, 1796, breaks = c(1790, 1795))
    te <- fh_period(1815, 1817, breaks = 1815)
    vg <- fh_period(1876, 1878, breaks = 1877)#, rightMargn = 0)
    
    mgd <- plot_grid(se, ak1, ak2, kwe, md, sp, ei, te, vg,
                     rel_widths = c(8, 18, 21, 4, 5, 13, 7, 4, 4),
                     nrow = 1, align = 'h')
    if (plotGap) {
      plot_grid(overall, NULL, mgd, ncol = 1, rel_heights = c(1, 0.2, 0.8))
    } else {
      plot_grid(overall, mgd, ncol = 1, rel_heights = c(1, 0.8))
    }
  } else {
    segmentTop <- max(Ystd$order) + 0.5
    fh_period(1200, endYear, 
              breaks = seq(1200, 2000, 50), xPosition = 'bottom',
              showLegend = TRUE, showAreas = TRUE) +
      geom_segment(aes(x = x, xend = xend, y = segmentTop, yend = segmentTop), size = 0.3,
                   data = segments, colour = 'steelblue')
  }
}

#' Plot the SST correlation map
#' 
#' @param sst.cor A data.table containing correlation with every SST point
#' @param signif.cor Where correlation is significant (p < 0.05)
#' @param land An sf object representing the land area
#' @param row.season Whether to layout the plot by row or by column
#' @return A plot, one row (or column) for each season, colour coded for correlation, and regions with significant correlations are encircled.
plot_sst_cor <- function(sst.cor, signif.cor, land, title = NULL, row.season = TRUE) {
  
  # To keep the colour scale symmetric
  limits <- c(-0.54, 0.54)
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
    labs(x = NULL, y = NULL, title = title) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'bottom',
          legend.key.width = unit(2, 'cm'),
          legend.title = element_text(hjust = 1, vjust = 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1, limits = limits,
                         breaks = scales::pretty_breaks(7)) +
    coord_quickmap()
    # coord_map(projection = 'azequalarea')
  
  if (row.season) 
    p + facet_grid(season ~ river, switch = 'y')
  else
    p + facet_grid(river ~ season)
}

#' Calculate correlation with seasonal SST
#' 
#' @param dt Data.table containing streamflow or catchment state time series
#' @param s Name of season
#' @param lag Lag between SST and streamflow
#' @param varName Either `Qa` for streamflow or `X` for catchment state
#' @return A data.table containing correlation and p-values.
get_sst_cor <- function(dt, s, lag, varName) {
  
  # Lag from the point of view of streamflow
  # Lag = -1 means SST of last year and streamflow of this year
  if (lag == 0) {
    sstYear <- qYear <- 1855:2012
  } else if (lag == -1) {
    sstYear <- 1855:2011
    qYear <- 1856:2012
  } else if (lag == -2) { # lag == -2
    sstYear <- 1855:2010
    qYear <- 1857:2012
  } else { # lab == +1
    sstYear <- 1856:2012
    qYear <- 1855:2011
  }
  
  lagChar <- if (lag == 0) '' else if (lag == 1) ' (+1)' else paste0(' (', lag, ')')
  ans <- sst[year %in% sstYear & season == s
           ][, 
             {
               dt[year %in% qYear,
                  {
                    ct <- cor.test(get(varName), msst)
                    list(cor = ct$estimate, p.value = ct$p.value)
                  }]
             },
             by = .(long, lat)
             ][, ':='(season = paste0(s, lagChar),
                      river = dt[1, river])]
  ans[]
}