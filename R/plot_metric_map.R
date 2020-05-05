#' Plot the reconstruction skill metrics on the map as well as the jitter plot
#' 
#' @param scores data.table containing all scores.
#' @param metricName String, name of metric.
#' @param numClasses Number of colour classes. Either a vector of length 2---first element is the number of negative colour classes, second is the number of positive colour classes---or an integer.
#' @param bgMap Background map, an `sf` object. Can be `NULL`.
#' @param histPosition Position of the histogram, either 'right' or 'bottom'.
#' @param histBarDirection Direction of the histogram bars, either 'vertical' or 'horizontal'.
#' @param relativeSizes The relative sizes of the map and the histogram; a vector of length 2.
#' @param dotSize Size of dots on the maps.
#' @param pretty If `TRUE`, the breaks are calculated using pretty breaks; otherwise they are calculated as equal intervals.
#' @return A plot with two components: first component is the map, second component is the histogram which doubles up as a legend.
 metric_map <- function(scores, metricName, numClasses, bgMap = NULL,
                       manualBreaks = NULL, labDigits = 2,
                       histPosition = 'right',
                       histBarDirection = 'horizontal',
                       relativeSizes = c(1, 0.5),
                       maxCount = 10,
                       dotSize = 2, pretty = FALSE) {
  
  metric <- scores[, get(metricName)]
  titleText <- switch(metricName,
                      'R2' = 'R\u00B2',
                      'RE' = 'Reduction of Error',
                      'CE' = 'Coefficient of Efficiency',
                      'KGE' = 'Kling-Gupta Efficiency')
  if (is.null(manualBreaks)) {
    if (length(numClasses) == 2) {
      if (pretty) {
        breaks <- pretty(metric, sum(numClasses)) 
        # Updated numClasses as pretty may result in different number of classes
        numClasses <- c(sum(breaks < 0), sum(breaks > 0))
      } else {
        breaks <- c(seq(min(metric), 0, length.out = numClasses[1] + 1),     # Negative classes, including 0
                    seq(0, max(metric), length.out = numClasses[2] + 1)[-1]) # Positive classes, omitting 0
      }
      negPal <- if (numClasses[1] < 3) {
          rev(RColorBrewer::brewer.pal(3, 'Reds')[1:numClasses[1]])
        } else {
          rev(RColorBrewer::brewer.pal(numClasses[1], 'Reds'))
        }
      posPal <- if (numClasses[2] < 3) {
          RColorBrewer::brewer.pal(3, 'Blues')[1:numClasses[2]]
        } else {
          RColorBrewer::brewer.pal(numClasses[2], 'Blues')
        }
      pal <- c(negPal, posPal)   
    } else {
      if (pretty) {
        breaks <- pretty(metric, numClasses)
        numClasses <- length(breaks) - 1
      } else {
        breaks <- seq(min(metric), max(metric), length.out = numClasses + 1)
      }
      pal <- RColorBrewer::brewer.pal(numClasses, 'Blues')
    }  
  } else {
    breaks <- manualBreaks
    numClasses <- length(breaks) - 1
    pal <- RColorBrewer::brewer.pal(numClasses, 'Blues')
  }
  
  labs <- format(round(breaks, labDigits), digits = labDigits)
  cuts <- cut(metric, breaks, include.lowest = TRUE)
  pal <- pal[sort(unique(as.integer(cuts)))] # Remove empty classes
  
  p1 <- ggplot(scores)
  
  if (!is.null(bgMap)) {
    p1 <- p1 + geom_sf(data = bgMap, colour = 'gray', size = 0.1) + coord_sf()
  } else {
    p1 <- p1 + coord_quickmap()
  }
  
  p1 <- p1 +
    geom_point(aes(long, lat, colour = cuts), size = dotSize) +
    scale_x_continuous(expand = c(0, 0), labels = pasteLong) +
    scale_y_continuous(expand = c(0, 0), labels = pasteLat) +
    scale_color_manual(values = pal, guide = 'none') +
    labs(subtitle = titleText, x = NULL, y = NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill = NA),
          panel.background = element_blank())
  
  p2 <- if (pretty || !is.null(manualBreaks)) {
    ggplot(scores) +
      geom_bar(aes(.data[[metricName]], fill = cuts)) +
      # If I put labs here, ggplot throws an error "breaks and labs must have the same length". Weird.
      scale_x_binned(breaks = breaks, expand = c(0, 0)) +
      scale_fill_manual(values = pal, guide = 'none') +
      theme_minimal() +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = 'white'),
            panel.ontop = TRUE)
  } else {
    ggplot(scores) +
      geom_bar(aes(.data[[metricName]], fill = cuts)) +
      scale_x_binned(breaks = breaks, labels = labs, expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), breaks = seq(0, maxCount, 2), limits = c(0, maxCount)) +
      scale_fill_manual(values = pal, guide = 'none') +
      theme_minimal() +
      theme(panel.background = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = 'white', size = 0.1),
            panel.ontop = TRUE)
  }
  
  if (histBarDirection == 'horizontal') {
    p2 <- p2 + labs(x = NULL) + coord_flip() + theme(axis.ticks.y = element_blank())
  } else {
    p2 <- p2 + 
      labs(x = paste(metricName, '[-]'), y = 'Station count [-]') + 
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5))
  }
  
  if (histPosition == 'right') {
    p1 + p2 + plot_layout(widths = relativeSizes)
  } else {
    p1 + p2 + plot_layout(heights = relativeSizes)
  }
}