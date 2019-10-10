#' Read a station's reconstruction results
#' 
#' @param folder The folder containing results
#' @param s Name of station
#' @return A data.table with columns year, X, Qa, ID
read_station <- function(folder, s) {
  
  rec <- readRDS(paste0(folder, s, '.RDS'))$fit$rec
  setnames(rec, old = 'Q', new = 'Qa')
  rec[, ID := s][]
}

#' Read performance scores. Separate the stations into regions.
#' 
#' @param folder The folder containing results
#' @param instQxy A data.table containing metadata
#' @return A data.table of the mean performance score over all cross validation runs
read_scores <- function(folder, instQxy) {
  
  lapply(instQxy$ID, function(s) {
    readRDS(paste0(folder, s, '.RDS'))$cv$metrics %>%
      t() %>% 
      as.data.table() %>% 
      .[, ID := s]
  }) %>%
    rbindlist() %>%
    merge(instQxy[, .(ID, long, lat, name, country)], by = 'ID') 
}