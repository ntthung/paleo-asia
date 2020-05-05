pasteLong <- function(x) { # Paste "degree West / East" to the x-axis labels
  ifelse(x < 0, paste0(-x, "\u00B0", "W"), ifelse(x > 0, paste0(x, "\u00B0", "E"), paste0(x, '\u00B0')))
}
pasteLat <- function(x) { # Paste "degree North / South" to the y-axis labels
  ifelse(x < 0, paste0(-x, "\u00B0", "S"), ifelse(x > 0, paste0(x, "\u00B0", "N"), paste0(x, '\u00B0')))
}

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

#' Get MADA points around a point of intesrt by KWF and geodesic distances (and catchment boundary, if provided)
#' 
#' Get the MADA grid points that are within thresholds from point p.
#' @inheritParams kwf_cell
#' @param kwf.max Maximum KWF distance
#' @param geo.max Maximum geodesic distance
#' @param mada.xy Data.table containing coordinates of MADA grid points
#' @param catchment.boundary sp object containing catchment boundary
get_mada_by_kwf <- function(poi, mada.kwf.cells, kwf, kwf.max, geo.max, catchment.boundary = NULL) {
  distance <- function(a, b) sqrt(sum((a-b)^2))
  # Get the KWF cell containing p
  poiCell <- get_cell(poi, kwf)[1, c(arid, seas, snow)]
  # Calculate distances
  ans <- mada.kwf.cells[, .(kwf.dist = distance(c(arid, seas, snow), poiCell),
                            geo.dist = geosphere::distGeo(c(long, lat), poi) / 1000),
                        by =  point
                        # Filter points
                        ][kwf.dist <= kwf.max & geo.dist <= geo.max, point]
  # Get points in catchment boundary if necessary (for large basins)
  if (!is.null(catchment.boundary)) {
    ans <- union(ans, picb(mada.kwf.cells, catchment.boundary))
  }
  ans
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
#' @param use.eigen IF TRUE, only PCs whose eigenvalue > 1 are kept.
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

#' Input variable selection
#' 
#' Four methods are in use: Variable Selection Using Random Forests (VSURF) from the `VSURF` package, forward stepwise selection from the `FWDselect` package, and forward and backward selection using branch and bound from the `leaps` package. The latter three use BIC as the selection criterion.
#' @param X Input variables, can be a matrix or a data.frame. One variable per column. Must have column names.
#' @param Y Output variable
#' @param method Either "VSURF", "FWDselect", leaps forward", or "leaps backward".
#' @param nvmax Maximum number of variables; not applicable for the VSURF method.
#' @param parallel Only applicable if VSURF or FWDselect is used. Default = `FALSE`.
#' @return If X has column names and `use.name == TRUE`, the names of the selected variables are returned, otherwise the selected column numbers are returned.
input_selection <- function(X, Y, method, nvmax, parallel = FALSE) {
  
  colNames <- colnames(X)
  if (is.null(colNames)) {
    stop("Error in input_selection: X must have column names")
  }
  colNums <- if (ncol(X) == 1) 1 else {
    switch(method,
           'VSURF' = {
             vsurf.fit <- VSURF::VSURF(X, Y, na.action = na.omit, parallel = parallel, verbose = FALSE)
             sv <- if (is.null(vsurf.fit$varselect.pred)) vsurf.fit$varselect.interp else vsurf.fit$varselect.pred
             sort(sv)
           },
           'FWDselect' = {
             nv <- min(ncol(X) - 1, nvmax) # ncol(X) > 1 here
             #FWDselect doesn't work with data.table
             X <- as.data.frame(X)
             fwd.fit <- FWDselect::qselection(X, Y, 1:nv, criterion = 'bic', cluster = parallel)
             sv <- fwd.fit$selection[which.min(fwd.fit$bic)]
             # qselection returns the selected variables in a single string, 
             #$ where the variable names are separated by commas, so we need to do strsplit
             sv <- strsplit(as.character(sv), ', ')[[1]]
             sort(match(sv, colNames))
           },
           'leaps forward' = {
             ivs <- summary(leaps::regsubsets(X, Y, method = 'forward', nvmax = nvmax))
             idx <- which.min(ivs$bic)
             which(ivs$which[idx, -1])
           },
           'leaps backward' = {
             ivs <- summary(leaps::regsubsets(X, Y, method = 'backward', nvmax = nvmax))
             idx <- which.min(ivs$bic)
             which(ivs$which[idx, -1])
           },
           stop("Error in input_selection: method not supported; method can only be 'VSURF', 'FWDselect', 'leaps forward', or 'leaps backward'"))
  } 
  colNames[colNums]
}