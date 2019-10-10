args <- commandArgs(trailingOnly = TRUE)
ind <- as.integer(args)
kwf.range <- seq(0.2, 0.3, 0.05)
p.range <- c(0, 0.5, 2/3, 1, 1.5, 2)

source('init.R')
source('read_data.R')

s <- instQxy[ind, ID]
poi <- instQxy[ind, c(long, lat)]
Qa <- instQ[ID == s, .(year, Qa)]
Z <- cvPoints[[s]]
rawInputs <- lapply(kwf.range, function(kwf.max) 
  raw_input(s, poi, mada2mat, corMat, madaKwfCells, kwf, kwf.max, 2500))

nbCores <- detectCores() - 1
cl <- makeCluster(nbCores)
registerDoParallel(cl)
  u.list <- foreach(input = rawInputs, .packages = c('data.table', 'magrittr', 'ldsr', 'VSURF')) %:%
    foreach(p = p.range) %dopar%
      get_u(input$sub.mada, input$rho, p, Qa, 1200:2012, sel.var = TRUE)
stopCluster(cl)

u.list <- unlist(u.list, recursive = FALSE)
fit <- LDS_ensemble(Qa, u.list, u.list, start.year = 1200,
                    num.restarts = 20, parallel = TRUE, return.raw = TRUE)
cv <- cvLDS_ensemble(Qa, u.list, u.list, start.year = 1200, Z = Z,
                     num.restarts = 20, parallel = TRUE)

ensemble <- list(fit = fit,
                 cv = cv,
                 uList = u.list)

saveRDS(ensemble, paste0('results/', s, '.RDS'))