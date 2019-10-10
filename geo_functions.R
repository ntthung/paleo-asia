#' Get a grid cell containing the point of interest
#' 
#' Get the KWF cell that contains a point on the map
#' @poi The point of interest, a vector of (long, lat)
#' @gridDT A data.table containing the gridded data
#' @return A data.table with one row: the grid cell that contains the point. If the point lies in the sea (as sometimes happens with MADA v1) then the cell nearest to it is returned.
get_cell <- function(poi, gridDT) {
  
  p <- gridDT[between(poi[1], x1, x2) & between(poi[2], y1, y2)]
  if (nrow(p) == 0) {
    d <- geosphere::distGeo(gridDT[, .(long, lat)], poi)
    p <- gridDT[which.min(d)]
  }
  p[1]
}

#' Get MADA grid points in catchment boundary
#' @param mada.xy A data.frame of MADA grid point coordinates
#' @param catchment.boundary A data.frame of catchment boundary
picb <- function(mada.xy, catchment.boundary) {
  which(point.in.polygon(mada.xy$long, mada.xy$lat, 
                         catchment.boundary$long, catchment.boundary$lat) > 0)
}

#' Get MADA points around a point of intesrt by KWF and geodesic distances (and catchment boundary, if provided)
#' 
#' Get the MADA grid points that are within thresholds from point p.
#' @inheritParams kwf_cell
#' @param kwf.max Maximum KWF distance
#' @param geo.max Maximum geodesic distance
#' @param mada.xy Data.table containing coordinates of MADA grid points
#' @param catchment.boundary sp object containing catchment boundary
get_mada_by_kwf <- function(poi, mada.kwf.cells, kwf, kwf.max, geo.max, catchment.boundary = NULL) {
  
  # Get the KWF cell containing p
  poiCell <- get_cell(poi, kwf)[1, c(arid, seas, snow)]
  # Calculate distances
  ans <- mada.kwf.cells[, .(kwf.dist = distance(c(arid, seas, snow), poiCell),
                            geo.dist = distGeo(c(long, lat), poi) / 1000),
                        by =  point
                        # Filter points
                      ][kwf.dist <= kwf.max & geo.dist <= geo.max, point]
  # Get points in catchment boundary if necessary (for large basins)
  if (!is.null(catchment.boundary)) {
    ans <- union(ans, picb(mada.kwf.cells, catchment.boundary))
  }
  ans
}

#' Get the KWF cells in the range
#' @param  range A vector of (xmin, xmax, ymin, ymax)
kwf_cell_in_range <- function(range) kwf[between(long, range[1], range[2]) & between(lat, range[3], range[4])]

#' Plot points in catchment boundary
#' 
#' @inheritParams pcib
#' @param cb.sf An sf object of catchment boundary, for plotting
#' @param xlim A vector of length 2, longitude limit for plot
#' @param ylim A vector of length 2, lattitude limit for plot
#' @param points Points to be plotted
plot_picb <- function(mada.xy, cb, cb.sf, points, xlim, ylim, caption) {
  ggplot(mada.xy[points]) + 
    plot_asia(xlim, ylim) +
    geom_sf(data = cb.sf, fill = 'palegreen', colour = NA) +
    geom_point(aes(long, lat), colour = 'black', shape = 3) +
    geom_text(aes(long, lat, label = ID), nudge_x = 1, size = 2 ) +
    coord_sf() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = NULL, y = NULL, caption = caption) +
    theme(panel.grid.major = element_line(colour = 'white'), 
          panel.background = element_blank(),
          plot.caption = element_text(hjust = 0)) +
    panel_border('black')
}

#' Weighted PCA based on correlation, a la Cook et al (2010). Each column of X will be weighted by an exponent r^p, i.e. 
#' \eqn{W_i = X_i*r_i^p}}
#' @param X A matrix
#' @param Y Output to calculate correlation with. A data.table (year, Qa)
#' @param p Exponent
wPCA <- function(X, rho = 1, p = 0) {
  
  # Negative correlation will cause a problem when raised to a real power
  if (length(rho) > 1) X <- sapply(1:ncol(X), function(k) X[, k]*abs(rho[k])^p)
  # According to Cook et al (2010) SI, when p = 0 we do PCA with correlation matrix form,
  # and when p != 0 we do PCA with covariance matrix form.
  summary(prcomp(X, scale. = (p == 0)))
}

#' Get potentially useful PCs out of PCA model. We retain up to 95% variance.
#' 
#' @param pc.model Results of `prcomp`
#' @`
get_PCs <- function(pc.model, use.eigen = TRUE) {
  
  big.var <- which(pc.model$importance[3,] >= 0.95)[1]
  kept <- if (use.eigen) {
    big.eigen <- which(pc.model$sdev < 1)[1] - 1
    min(c(big.eigen, big.var))
  } else big.var
  PC <- data.table(pc.model$x[ , 1:kept])
  if (ncol(PC) == 1) colnames(PC) <- 'PC1'
  PC  
}

#' Raw input for weighted PCA
#' 
#' @inheritParams get_mada_by_kwf
#' @param s Station name
#' @param mada MADA matrix
#' @param cor.mat Correlation matrix between MADA and streamflow
raw_input <- function(s, poi, mada, cor.mat, mada.kwf.cells, kwf, kwf.max, geo.max, catchment.boundary = NULL) {
  
  points <- get_mada_by_kwf(poi, madaKwfCells, kwf, kwf.max, geo.max, catchment.boundary)
  list(sub.mada = mada[, points],
       rho = cor.mat[ID == s, -1] %>% unlist() %>% .[points])
}

#' Obtain the input matrix from a subregion of the MADA based on the KWF and GEO criteria.
#' Steps:
#' \enumerate{
#'   \item Obtain all points within radius `geo.max` and climate distance `kwf.max`
#'   \item Perfrom weighted PCA
#'   \item Perform input variable selection, either using backward stepwise linear regression or VSURF
#'   \item Tranpose into the correct matrix format: row for variable, column for time
#' }
#' @inheritParams wPCA
#' @param Qa Streamflow data.table (year, Qa)
#' @param years Years in the MADA
#' @param sel.var If `TRUE`, VSURF is used, else backward stepwise selection
#' @param parallel If `TRUE`, run in parallel using numcore - 1.
#' @return A matrix of input
get_u <- function(X, rho, p, Qa, years, sel.var = FALSE, parallel = FALSE) {
  
  PC <- wPCA(X, rho, p) %>% get_PCs()
  if (sel.var) {
    PC[, year := years]
    # Variable selection
    dt <- merge(Qa, PC, by = 'year')[, -'year']
    if (ncol(dt) > 2) {
      vsurf.fit <- VSURF(Qa ~ ., data = dt, na.action = na.omit, parallel = parallel)
      sv <- if (is.null(vsurf.fit$varselect.pred)) vsurf.fit$varselect.interp else vsurf.fit$varselect.pred
      sv <- sort(sv)
    } else sv <- 1
  } else {
    # Build benchmark 1 to get the PC, we don't need the CV scores
    bm1 <- PCR_reconstruction(Qa, PC, CV.reps = 1)
    sv <- bm1$selected %>% substr(3,4) %>% as.integer()
  }
  t(PC[, ..sv])
}

#' Get the boundaries of areas of significant correlation
#' 
#' Idea: a cell is a top border if the cell above it is not significant, left border if the cell to the right is not significant, and so on.
#' Algorithm (naive, but good because it can be vectorized): for each significant cell `i`, add the cell above it, then check for duplicate. If it's duplicated, then the cell above it is also a significant cell, then cell `i` is not a top border. Repeat for other directions.
#' @param cor.dt A data.table of correlation results. Need three columns: `long`, `lat`, and `p.value`
#' @param dx Longitudial resolution
#' @param dy Lattitudial resolution
#' @param signif.level Significant level (alpha)
#' @return The original data.table with added columns:top, down, bottom, left. Cell i has top = TRUE if it is a top border, and so on.
signif_area <- function(cor.dt, dx, dy, signif.level = 0.05) {
  
  cor.dt[p.value < signif.level,
       ][, 
         ':='(
           top = {
             apply(.SD, 1, function(v) {
               dt2 <- rbind(.SD, t(c(v['long'], v['lat'] + dy)))
               di <- which(duplicated(dt2))
               length(di) == 0
             }) 
           },
           bottom = {
             apply(.SD, 1, function(v) {
               dt2 <- rbind(.SD, t(c(v['long'], v['lat'] - dy)))
               di <- which(duplicated(dt2))
               length(di) == 0
             }) 
           },
           left = {
             apply(.SD, 1, function(v) {
               dt2 <- rbind(.SD, t(c(v['long'] - dx, v['lat'])))
               di <- which(duplicated(dt2))
               length(di) == 0
             }) 
           },
           right = {
             apply(.SD, 1, function(v) {
               dt2 <- rbind(.SD, t(c(v['long'] + dx, v['lat'])))
               di <- which(duplicated(dt2))
               length(di) == 0
             }) 
           }
         ),
         .SDcols = c('long', 'lat')][]
}
