lapplyrbind <- function(x, fun, ..., id = NULL) rbindlist(lapply(x, fun, ...), idcol = id)

absRange <- function(x) {
  r <- range(x)
  absMax <- max(abs(r))
  c(-absMax, absMax)
}

hinkley <- function(x) (mean(x, na.rm = TRUE) - median(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)