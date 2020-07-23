flow_history <- function(recResults, metaData, trans,
                         startYear = 1200, endYear = 2012, breaks = NULL, 
                         what = 'flow',
                         stationNames = FALSE,
                         stdType = c('reconst', 'inst'), instSummary = NULL,
                         plotSegments = TRUE, plotLower = TRUE, plotGap = FALSE) {
  
  fh_period <- function(firstYear, lastYear, breaks, xPosition = 'bottom', 
                        showLegend = FALSE, showAreas = FALSE) {
    ggplot(rec[between(year, firstYear, lastYear)]) +
      geom_tile(aes(year, I(y_start), fill = Y, height = I(height)), 
                width = 1, colour = NA) +
      {if (stationNames) geom_text(aes(1100, I(y_start), label = ID), 
                                   family = 'mono', size = 3, hjust = 0)} +
      geom_hline(aes(yintercept = top), data = regionSep[-1], colour = 'gray50', size = 0.3) +
      scale_x_continuous(expand = c(0, 0), breaks = breaks, position = xPosition) +
      scale_y_continuous(breaks = regionSep$mid,
                         labels = regionSep$region,
                         expand = c(0, 0)) +
      scale_fill_gradient2(name = if (what == 'state') 'Catchment state' else 'Standardized streamflow',
                           low = '#6b473e', mid = 'snow1', high = '#125a0d', 
                           breaks = seq(-3, 3, 1),
                           limits = limits, na.value = 'gray90') +
      labs(x = NULL, y = 'Streamflow stations') +
      theme_cowplot(font_size = 10) +
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
  
  rec <- copy(recResults[between(year, startYear, endYear)])
  meta <- copy(metaData[order(code)])
  rec <- rec[ID %in% metaData$ID]

  meta[, height := 1]
  
  meta[.N, y_start := height / 2]
  for (i in (nrow(meta) - 1) : 1) {
    meta$y_start[i] <- 
      meta$y_start[i + 1] + meta$height[i + 1] / 2 + meta$height[i] / 2
  }
  regionSep <- meta[, .(top = .SD[1, {y_start + height/2}]), by = region]
  regionSep[.N, mid := top / 2]
  for (i in (nrow(regionSep) - 1) : 1) 
    regionSep$mid[i] <- (regionSep$top[i + 1] + regionSep$top[i]) / 2
  
  rec[, 
      Y := if (what == 'state') X / sd(X) else {
        s <- .BY
        tf <- trans[s, trans]
        y <- if (tf == 'log') log(Q) else Q
        if (stdType == 'reconst') standardize(y) else {
          (y - instSummary[s, mu]) / instSummary[s, sigma]
        }
      },
      by = ID]
  
  rec %<>% merge(meta[, .(ID, height, y_start, code)])
  
  segments <- data.table(x    = c(1257, 1345, 1401, 1452, 1638, 1756, 1790, 1815, 1876),
                         xend = c(1258, 1375, 1425, 1453, 1641, 1768, 1796, 1816, 1878))
  
  limits <- absRange(rec$Y)
  if (is.null(breaks)) breaks <- seq(1200, 2000, 50)
  
  if (plotLower) {
    overall <- fh_period(1200, endYear, 
                         breaks = breaks, xPosition = 'top', #rightMargin = 0,
                         showLegend = TRUE, showAreas = TRUE) +
    if (plotSegments) {
      geom_segment(aes(x = x, xend = xend, y = 0.125, yend = 0.125), size = 0.25,
                   data = segments, colour = 'steelblue')
    }
    se  <- fh_period(1257, 1259, breaks = 1257, showAreas = TRUE)
    ak1 <- fh_period(1345, 1375, breaks = seq(1345, 1375, 5))
    ak2 <- fh_period(1401, 1425, breaks = seq(1405, 1420, 5))
    kwe <- fh_period(1452, 1454, breaks = 1452)
    md  <- fh_period(1638, 1641, breaks = 1640)
    sp  <- fh_period(1756, 1768, breaks = c(1760, 1765))
    ei  <- fh_period(1790, 1796, breaks = c(1790, 1795))
    te  <- fh_period(1815, 1817, breaks = 1815)
    vg  <- fh_period(1876, 1878, breaks = 1877)
    
    mgd <- se + ak1 + ak2 + kwe + md + sp + ei + te + vg +
      plot_layout(widths = c(3, 31, 25, 3, 4, 13, 7, 3, 3), nrow = 1)
    
    if (plotGap) {
      overall + plot_spacer() + mgd + plot_layout(ncol = 1, heights = c(1, 0.2, 0.8))
    } else {
      overall + mgd + plot_layout(ncol = 1, heights = c(1, 0.8))
    }
  } else {
    segmentTop <- meta[1, {height/2 + y_start}]
    fh_period(1200, endYear, 
              breaks = breaks, xPosition = 'bottom',
              showLegend = TRUE, showAreas = TRUE) +
      if (plotSegments) {
        geom_segment(aes(x = x, xend = xend, y = segmentTop, yend = segmentTop), size = 0.3,
                   data = segments, colour = 'steelblue')
      }
  }
}