#' Find the vector of breaks to divide `x` into bins.
#' 
#' @param x A numeric vector
#' @param n Number of breaks. Number of bins is `n - 1`
#' @param style Binning style. See `classInt::classIntervals`
#' @return A vector of the upper bound of each bin
getBreaks <- function(x, n, style) {
  
  if (length(x) == 2) {
    brks <- x
  } else {
    brks <- classInt::classIntervals(x, n-1, style, intervalClosure = 'right')$brks
    # If there is an extremely low value, the first class will contain only one point, left and right bounds are the same. We handle this class separately by expanding it to the left.
    # I tried adding a dummy class but it's a bad idea. When oding equal interval we avoid having holes in the colour scale anyway.
    if (brks[1] == brks[2]) brks[2] <- brks[2] + 0.5*(brks[3] - brks[2])
    brks  
  }
}

#' Plot the reconstruction skill metrics on the map as well as the jitter plot
#' 
#' @param scores data.table containing all scores
#' @param m String, name of metric
#' @bottom Specify the bottom plot
#' @posNegBreaks If `TRUE`, breaks are calculated separately for positive and negative values
#' @breakStyle Binning style. See `classInt::classIntervals`
#' @numBreaks Number of breaks. If `posNegBreaks == TRUE`, `numBreaks` is a vector of length 2, first element is the number of positive breaks, second is the number of negative breaks. Otherwise, `numBreaks` is an integer.
#' @return A plot with two rows: first row is the map, second row is the jitter plot, and a legend.
metric_map <- function(scores, m, # sorted = FALSE, 
                       dotSize = 2, alpha = 1,
                       breakStyle = c('equal', 'jenks'),
                       numBreaks = c(6, 6), 
                       manualNegBreaks = NULL,
                       manualPosBreaks = NULL,
                       splitRegions = FALSE,
                       annotateLong = TRUE,
                       annotateLat = TRUE,
                       bgMap = NULL,
                       labels = NULL,
                       rel = c(1, 1),
                       direction = 'vertical',
                       histDirection = 'vertical') {
  
  dt <- if (splitRegions) scores[, .(ID, long, lat, region, metric = get(m))] else scores[, .(ID, long, lat, metric = get(m))]
  
  titleText <- 
    if (m == 'KGE') 'Kling-Gupta Efficiency' else
      if (m == 'RE') 'Reduction of Error' else 
        if (m == 'CE') 'Coefficient of Efficiency' else ''
  title <- ggdraw() + draw_label(titleText, fontface = 'bold')
  
  if (length(numBreaks)==2) {
    if (breakStyle == 'jenks') {
      # Right-closed intervals, so 0 belongs to negBrks
      posBrks <- getBreaks(dt[metric > 0, metric], numBreaks[1], breakStyle)[-1]
      negBrks <- getBreaks(c(dt[metric <= 0, metric], 0), numBreaks[2], breakStyle)
    } else {
      posBrks <- getBreaks(c(0, dt[metric > 0, metric]), numBreaks[1], breakStyle)
      negBrks <- getBreaks(c(dt[metric <= 0, metric], 0), numBreaks[2], breakStyle)
    }
    if (!is.null(manualNegBreaks)) negBrks <- manualNegBreaks
    if (!is.null(manualPosBreaks)) posBrks <- manualPosBreaks
    brks <- c(negBrks, posBrks)
    # The RdBu palettes only allows 11 classes. 
    # Here I use the Reds and Blues palettes separately so that I can have more classes
    posPal <- RColorBrewer::brewer.pal(numBreaks[1], 'Blues')
    negPal <- RColorBrewer::brewer.pal(numBreaks[2], 'Reds') %>% rev()
    pal <- c(negPal, posPal)
  } else {
    brks <- getBreaks(dt[, metric], numBreaks, breakStyle)
    pal <- RColorBrewer::brewer.pal(numBreaks, 'Blues')
  }
  
  labs <- round(brks, 2)  
  # Add a space in front of positive numbers to facilitate alignment with negative numbers
  labs <- ifelse(labs < 0, as.character(labs), paste0(' ', labs)) 
  dt[, c('binCode', 'binLab', 'binColour') := {
    binCode <- findInterval(metric, brks, rightmost.closed = TRUE, left.open = TRUE) + 1
    list(binCode, brks[binCode], pal[binCode]
    )
  }]
  
  if (splitRegions) {
    histDT <- dt[, regionCount := .N, region
                 ][,.(binColour = first(binColour), 
                      count = .N), 
                   by = .(region, binCode)
                   ][order(region, binCode)]  
  } else {
    Ns <- nrow(instQxy)
    histDT <- dt[,.(binColour = first(binColour), 
                    count = .N), 
                   by = binCode
               ][order(binCode)]
  }
  
  histDT[, c('binStart', 'binEnd') := list(brks[binCode-1], brks[binCode])]
    
  mapPlot <- if (is.null(bgMap))  {
    # ggplot(dt[order(metric, decreasing = TRUE)], aes(long, lat)) +
    ggplot(dt, aes(long, lat)) +
      plot_asia() +
      geom_point(aes(colour = I(binColour)), alpha = 1, size = dotSize) +
      scale_x_continuous(labels = pasteLong, expand = c(0, 0)) +
      scale_y_continuous(labels = pasteLat, expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      coord_quickmap() +
      theme(legend.position = 'none') +
      panel_border('black', size = 0.2)
  } else {
    ggplot(dt) +
      geom_sf(data = bgMap, colour = 'gray', size = 0.1) +
      geom_point(aes(long, lat, colour = I(binColour)), alpha = 1, size = dotSize) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      coord_sf() +
      theme(legend.position = 'none') +
      panel_border('black', size = 0.2)
  }
  
  if (annotateLat == FALSE) mapPlot <- mapPlot + theme(axis.text.y = element_text(colour = 'white'))
  if (annotateLong == FALSE) mapPlot <- mapPlot + theme(axis.text.x = element_text(colour = 'white'))
  
  # Start with horizontal bars
  histPlot <- ggplot(histDT) +
    geom_rect(aes(ymin = binStart, ymax = binEnd, xmin = 0, xmax = count,
                  fill = I(binColour))) +
    scale_y_continuous(name = paste(m, '[-]'), breaks = brks, labels = labs) +
    theme(legend.position = 'none')
  
  if (splitRegions) {
    if (histDirection == 'vertical') { # Histograms are laid out vertically, each has horizontal bars
      histPlot <- histPlot +
        facet_wrap(~region, nrow = 1) +
        scale_x_continuous(name = 'Station count', breaks = scales::pretty_breaks(3)) +
        theme(panel.spacing.x = unit(3, 'mm'))
    } else {
      histPlot <- histPlot +
        facet_wrap(~region, ncol = 1) +
        scale_x_continuous(name = 'Station count', breaks = scales::pretty_breaks(3)) +
        coord_flip() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5),
              panel.grid.major.y = element_line(colour = 'gray80', size = 0.2))
    }
  } else {
    histPlot <- histPlot + 
      scale_x_continuous(name = 'Station count', breaks = scales::pretty_breaks(5), limits = c(0, 15), expand = c(0, 0)) +
      coord_flip() +
      theme(axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5),
            panel.grid.major.y = element_line(colour = 'gray80', size = 0.2))
  }
  if (direction == 'vertical') {
    plot_grid(mapPlot + 
                labs(title = titleText), 
              histPlot, 
              labels = labels, label_size = 10,
              ncol = 1, rel_heights = rel)
  } else {
    plots <- plot_grid(mapPlot, histPlot, ncol = 2, rel_widths = rel, labels = labels, label_size = 11)
    plot_grid(title, plots, ncol = 1, rel_heights = c(0.2, 1))
  }
}