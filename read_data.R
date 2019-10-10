# MADA v2
mada2mat <- readRDS('data/mada2mat.RDS') # Matrix format for faster calculations
mada2xy <- readRDS('data/mada2xy.RDS')   # Coordinates
mada2 <- readRDS('data/mada2.RDS')       # Data.table format for easy wrangling

# Streamflow
instQ <- fread('data/instQ.csv')         # Annual streamflow
instQxy <- fread('data/instQxy.csv')     # Coordinates and other metadata
instQxyCN <- fread('data/instQxyCN.csv') # Separate metadata for Chinese stations. I cannot share their instrumental data due to restrictions, but I provided their reconstruction results.
instQsummary <- fread('data/instQsummary.csv') # Summary statistics of log-transformed streamflow

# Climate classification system (Knoben et al, 2018)
kwf <- readRDS('data/kwf.RDS')

# To avoid repeated calculations on the computing cluster, I've pre-calculated the following data. If you want to recalculate or customize them, please use the script preparations.R

# Uncomment for recalculation (takes about 6 seconds)
# source('preparations.R')

# Correlation between MADA and streamflow
corMat <- readRDS('data/corMat.RDS')

# Cross-validation points
cvPoints <- readRDS('data/cv_points.RDS')

# KWF cell of each MADA point
madaKwfCells <- readRDS('data/madaKwfCells.RDS') 