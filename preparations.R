### Not run

# Correlation matrix between MADA and streamflow
NM <- nrow(mada2mat)
corMat <- instQ[, cor(Qa, mada2mat[(NM-.N+1):NM, ], use = 'complete.obs') %>% as.data.frame(),
                by = ID]
remove(NM)
saveRDS(corMat, 'data/corMat.RDS')

# Get the KWF cells of MADA grid points 
madaKwfCells <- mada2xy[, get_cell(c(long, lat), kwf), by = point][, .(point, long, lat, arid, seas, snow)]
saveRDS(madaKwfCells, 'data/madaKwfCells.RDS')

# Cross-validation points 

cvPoints <- lapply(split(instQ, by = 'ID'),
                   function(dt) {
                     obsInd <- which(!is.na(dt$Qa))
                     nSamp <- ceiling(length(obsInd) / 4)
                     replicate(100, sample(obsInd, nSamp), simplify = FALSE)
                   })
saveRDS(cvPoints, 'data/cv_points.RDS')

### End (not run)