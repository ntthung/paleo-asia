# I have provided the results in the folder `results`. Run the script below only if you wnat to recalculate the results. It will take a few minutes.

### NOT RUN

# Select stations

outletStations <- c('Wadenapalli', 'C.2', 'Stung Treng', 'Datong')
outletLookup <- instQxy[name %in% outletStations]
outlet <- merge(allRec, outletLookup, by = 'ID')
selectedRivers <- c('Krishna', 'Chao Phraya', 'Mekong', 'Yangtze')

sst <- readRDS('data/sst.RDS') # seasonal SST from NOAA ERSST v5
sstLand <- readRDS('data/sstLand.RDS')  # Land cells

nbCores <- detectCores() - 1
cl <- makeCluster(nbCores)
registerDoParallel(cl)

# Calculate correlations
sstCor <- 
  foreach(r = selectedRivers, .combine = rbind, .packages = 'data.table') %:%
  foreach(season = c('DJF', 'MAM', 'JJA', 'SON'), .combine = rbind) %:%
  foreach(lag = -1:1, .combine = rbind) %dopar%
  get_sst_cor(outlet[river == r], season, lag, 'Qa')

sstCor$season %<>% factor(levels = c('DJF (-1)', 'MAM (-1)', 'JJA (-1)', 'SON (-1)',
                                     'DJF',      'MAM',      'JJA',      'SON',
                                     'DJF (+1)', 'MAM (+1)', 'JJA (+1)', 'SON (+1)'))
sstCor$river %<>% factor(levels = selectedRivers)

# Find significant areas
caseList <- split(sstCor, by = c('river', 'season'))
sstSignifArea <- foreach(case = caseList, .combine = rbind) %dopar%
  signif_area(case, 2, 2)

stopCluster(cl)

saveRDS(sstCor, 'results/sstCor.RDS')
saveRDS(sstSignifArea, 'results/sstSignifArea.RDS')

### END NOT RUN