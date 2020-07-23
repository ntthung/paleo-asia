library(data.table)
library(ldsr)
library(foreach)
library(glue)

# Hyperparameters
kwfRange <- c('0.10' = 0.1, '0.15' = 0.15, '0.20' = 0.2, '0.25' = 0.25, '0.30' = 0.3)
pRange <- c('0' = 0, '0.5' = 0.5, '2/3' = 2/3, '1' = 1, '1.5' = 1.5, '2' = 2)
names(kwfNames) <- kwfNames <- names(kwfRange) # For use in lapply() to keep list names
names(pNames) <- pNames <- names(pRange)

# Utils
hinkley <- function(x) (mean(x, na.rm = TRUE) - median(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
lapplyrbind <- function(x, fun, ..., id = 'id') rbindlist(lapply(x, fun, ...), idcol = id)

# Streamflow
instQ <- fread('data/instQ.csv', key = 'ID')
instQmeta <- fread('data/instQ_meta.csv', key = 'ID')
cvPoints <- readRDS('data/cvPoints_indiv_seed.RDS')
trans <- instQ[, .(trans = ifelse(abs(hinkley(log(Qa))) < abs(hinkley(Qa)), 'log', 'none')), 
               by = ID]

# Change the indices if you want to build reconstructions for a subset of stations only
# For example, to reconstruct only the first station, use stationIndices <- 1
stationIndices <- 1:nrow(instQmeta)
names(stationIDs) <- stationIDs <- instQmeta$ID[stationIndices]

# Input selection
espc <- readRDS('results/ensemble_selected_PCs.RDS')

num.restarts <- 100
method <- 'VSURF'

doFuture::registerDoFuture()
future::plan(future::multiprocess)

ldsEnsembleYcv <- 
  foreach(s = stationIDs, .packages = c('data.table', 'ldsr'), .combine = rbind) %:%
    foreach(kwfMax = kwfNames, .combine = rbind) %:%
      foreach(p = pNames, .combine = rbind) %dopar% {
        Qa <- instQ[s]
        Z <- cvPoints[[s]]
        transform <- trans[ID == s, trans]
        obs <- if (transform == 'log') log(Qa$Qa) else Qa$Qa
        mu <- mean(obs, na.rm = TRUE)
        instPeriod <- which(1200:2012 %in% Qa$year)
        y <- t(c(rep(NA, Qa[1, year] - 1200),   # Before the instrumental period
                 obs - mu,                      # Instrumental period
                 rep(NA, 2012 - Qa[.N, year]))) # After the instrumental period
        u <- t(espc[[s]][[kwfMax]][[p]])
        Ycv <- lapplyrbind(Z, function(z) 
          data.table(year = Qa$year,
                     Q = one_lds_cv(z, instPeriod, mu, y, u, u, 'EM', num.restarts, use.raw = TRUE)),
          id = 'rep') 
        Ycv[, ID := s][, kwf := kwfMax][, p := p]
        # Different ensembles
        out <- list(Ycv = Ycv, Z = Z)
        if (p == '2/3') p <- '23'
        saveRDS(out, glue('results/{s}_{kwfMax}_{p}_{num.restarts}restarts.RDS'))
        Ycv[]
  }
saveRDS(ldsEnsembleYcv, 'results/ensemble_cv_rerun.RDS')