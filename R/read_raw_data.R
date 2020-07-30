min.length <- 40
min.length.JPIN <- 45

# METADATA ------------------------------------------------------------------------------------

# GSIM-meta-full
gmf <- fread('D:/GSIM/GSIM_metadata/GSIM_catalog/GSIM_metadata.csv',
             drop = c('reference.db', 'reference.no', 'grdb.merge', 'paired.db',  'altitude',
                      'river.dist', 'station.dist', 'latlon.dist', 'bin.latlon.dist', 'mean.dist')) %>% 
  setnames(old = c('gsim.no', 'latitude', 'longitude'), new = c('ID', 'lat', 'long')) %>% 
  .[between(lat, min(mada2xy$lat), max(mada2xy$lat)) & 
      between(long, min(mada2xy$long), max(mada2xy$long))] %>% 
  # Some station and river names are capitalized. Change to Title Case for consistency
  # .[, station := str_title_case(str_lower_case(station))] %>%
  # .[, river := str_title_case(str_lower_case(river))] %>%
  # Fix P1 coordinates
  .[ID == 'TH_0000041', c('long', 'lat') := list(99.00443, 18.78766)]

# Change names to match Mekong data
gmf[ID == 'LA_0000002', station := 'Luang Prabang']
gmf[ID == 'LA_0000001', station := 'Ban Mixay']
gmf[ID == 'LA_0000005', station := 'Vientiane']

# Calculate time series length (inc. NAs)
gmf[, 
    target.length := {
      last.year <- ifelse(year.end < end.year, year.end, end.year)
      last.year - year.start + 1
    } 
    ]

# GSIM-meta (initial selection)
gm <- gmf[target.length >= min.length & frac.missing.days <= 0.03][ID != 'TH_0000129']

# Shorten Japanese name
gm[country == 'JP', station := {
  paren <- regexpr('(', station, fixed = TRUE)[1]
  if (paren > 1) substr(station, 1, paren-2) else station
}, by = ID]
gm[ID == 'JP_0001029', station := 'Gomazuru']
gm[ID == 'JP_0000167', station := 'Toshibetsugawa']
gm[ID == 'JP_0000092', station := 'Naka Aibetsu']
gm[ID == 'JP_0000092', station := 'Lower Toikanbetsu']

# Note we don't have meta for some Chinese stations, Angat, Hwachon, and Hardinge Bridge

# STREAMFLOW -------------------------------------------------------------------

# First, read special stations
# Then, read GSIM stations that have at least 39 years of data (excluding NAs), except those
# already in the special stations

# _Thailand ------------------------------------------------------------------

#         P.1           P.4A          P.14          N.1           C.2           C.13
ths <- c('TH_0000041', 'TH_0000070', 'TH_0000044', 'TH_0000003', 'TH_0000178', 'TH_0000177',
         #      P.21          P.5         
         'TH_0000050', 'TH_0000071')

TH <- rbindlist(lapply(ths, function(s) 
  # Thailand's data are in million cubic meter per month and is in calendar year
  fread(paste0('01_Data/GSIM_extra/',s,'.csv')) %>% 
    'colnames<-'(c('year', 1:12)) %>% 
    melt(id = 'year', variable.name = 'month', value.name = 'Qm') %>% 
    .[order(year, month)] %>% 
    .[, month := as.integer(month)] %>% 
    water_to_calendar_year(3) %>% 
    .[, .(Qa = sum(Qm) / 3600 / 24 / 365 * 1e6), by = year] %>% 
    .[, ID := s]
))

# _China -------------------------------------------------------------------

# CN <- rbindlist(lapply(CN.lookup$name, function(s) {
#     dt <- gdata::read.xls('01_Data/China/annual_average_flow.xls',
#                    sheet = tolower(s),
#                    perl = 'C:/Strawberry/perl/bin/perl.exe') %>%
#         as.data.table() %>%
#         setnames(s, 'Qa') %>%
#         .[days < 335, Qa := NA] %>%
#         .[year <= end.year, .(year, Qa)] %>%
#         .[, ID := CN.lookup[name == s, ID]]
#     dt[]
# }))
# write.csv(CN, '01_Data/GSIM_extra/China.csv', row.names = FALSE)

CN <- fread('01_Data/GSIM_extra/China.csv') 

# _Bangladesh -----------------------------------------------

# # Hardinge Bridge
# # Check data quality ---
# # Mukund's data
# dt1 <- fread('01_Data/Bangladesh/Selected/Qganges_1950-2009.txt')[year <= end.year]
# # Kamal's data
# dt2 <- fread('01_Data/Bangladesh/Selected/BD_SW90.csv')[year <= end.year]
# # In general, the two dataset agree with each other quite well, but there are many differences
# dt <- merge(dt1, dt2, by = c('year', 'month', 'day'), all.x = TRUE, all.y = TRUE)
# ggplot(dt, aes(discharge.x, discharge.y)) + geom_point()
# # Use Mukund's data and replace missing data there with Kamal's data
# dt[discharge.x == -9999, discharge.x := discharge.y]
# dt[!is.na(discharge.x), .N, by = year]
# # Missing years: 1971, 1972, 2003
# # 2001: missing March, use average of Feb and April
# # 2004: use Kamal's data
# # end.year: missing 14 days, use monthly average
# # Combine data ---
# BD <- copy(dt1)[discharge == -9999, discharge := NA]
# # Fill for 2001
# BD[year == 2001 & month == 2 & day == 28,
#    discharge := mean(BD[year == 2001 & month == 2 & day < 28, discharge])]
# BD[year == 2001 & month == 3,
#    discharge := (BD[year == 2001 & month == 2, mean(discharge)] +
#                  BD[year == 2001 & month == 4, mean(discharge)]) / 2]
# # Fill for 2004
# BD[year == 2004, discharge := dt2[year == 2004, discharge]]
# # Fill for end.year
# BD[year == end.year & month == 11 & is.na(discharge),
#    discharge := mean(BD[year == end.year & month == 11, discharge], na.rm = TRUE)]
# BD[year == end.year & month == 12 & is.na(discharge),
#    discharge := mean(BD[year == end.year & month == 12, discharge], na.rm = TRUE)]
# 
# BD <- BD[year <= 2004, .(Qa = mean(discharge), ID = 'BD_0000002'), by = year]
# write.csv(BD, paste0('01_Data/GSIM_extra/BD_0000002.csv'), row.names = F)
# 
# # Bahadurabad
# BD1 <- fread('01_Data/Bangladesh/Selected/Q_brahma_1956_2016.csv')[
#     year <= 2006, .(year, Qa = rowMeans(.SD), ID = 'BD_0000001'), .SDcols = Jan:Dec]
# write.csv(BD1, paste0('01_Data/GSIM_extra/BD_0000001.csv'), row.names = F)

BD <- rbind(fread('01_Data/GSIM_extra/BD_0000001.csv'),
            fread('01_Data/GSIM_extra/BD_0000002.csv'))

# _Korea ----------------------------------------------------

KR <- fread('01_Data/GSIM_extra/KR_0000005.csv')[year <= end.year, 
                                                 .(Qa = mean(Qm), ID = 'KR_0000005'), 
                                                 by = year]

# _Mekong --------------------------------------------------------------------------------------

# All Mekong data
mekong <- lapply(c('Cambodia', 'Laos', 'Thailand', 'Vietnam'), function(cty) {

    dt <- gdata::read.xls('01_Data/Mekong/mekong hydrological data.xls',
                   sheet = cty, na.strings = "",
                   perl = 'C:/Strawberry/perl/bin/perl.exe') %>%
        as.data.table() %>%
        setnames(old = 'Date.and.Time', new = 'year') %>%
        .[year <= 2006] %>%
        melt(id.var = 'year', variable.name = 'MRCID', value.name = 'Qa') %>%
        .[, MRCID := substr(MRCID, 2, 7)] %>%
        .[Qa < 0, Qa := NA]
    dt[]
}) %>% rbindlist()
mekong.pass <- mekong[, .(ndata = sum(!is.na(Qa))), by = MRCID][ndata > min.length, MRCID]
mekong <- mekong[MRCID %in% mekong.pass
               ][, .(year, Qa, MRCID)
               ][!(MRCID %in% c('019810', '033410')) # These stations do not have coordinates
               ][!(year >= 2003 & is.na(Qa))]

mekong.lookup <- gdata::read.xls('01_Data/Mekong/mekong hydrological data.xls',
                          sheet = 'Location',
                          perl = 'C:/Strawberry/perl/bin/perl.exe',
                          row.names = NULL,
                          colClasses = c('character', NA, NA, NA), stringsAsFactors = FALSE) %>%
  as.data.table() %>%
  setnames(c('MRCID', 'name', 'oldLat', 'oldLong')) %>%
  .[MRCID %in% mekong$MRCID] %>% 
  merge(gmf[, .(ID, name = station, long, lat)], by = 'name', all.x = TRUE)
mekong.lookup[name == 'Savannakhet', ':='(ID = 'LA_0000016', long = oldLong, lat = oldLat)]

mekong <- merge(mekong, mekong.lookup[, .(MRCID, ID)], by = 'MRCID', all.x = TRUE)[, -'MRCID']

# write.csv(mekong, '01_Data/GSIM_extra/Mekong.csv', row.names = FALSE)
# saveRDS(mekong.lookup, '03_Intermediate/mekong_lookup.rds')

# Mekong is the only case that comes with country
# mekong <- fread('01_Data/GSIM_extra/Mekong.csv')

# _Philippines ---------------------------------------------

PH <- fread('01_Data/GSIM_extra/PH_0000006.csv')[
  year <= end.year, .(year, Qa = rowMeans(.SD), ID = 'PH_0000006'), .SDcols = Jan:Dec]

# Mongolia ----------------------------------------------------------------

MN <- fread('01_Data/Mongolia/YeruuRiverInstNoOutlierCleaned.flow') %>%
  .[, .(year,
        Qa = rowMeans(.SD) / 100,
        ID = 'MN_0000002'),
    .SDcols = 2:13]

# MN <- fread('01_Data/Mongolia/yeruu2012_m3_s.csv') %>% 
#   .[year %in% 1959:end.year, .(year, Qa = obs_m3s, ID = 'MN_0000002')]

# Indonesia ---------------------------------------------------------------

# Cross check with GSIM
# IndoGsim <- read_Qa(gmf[river == 'Ci Tarum', ID])
# The Citarum station in the GSIM dataset is upstream of what we have. We can also see this from the coordinates.

Indo <- fread('01_Data/Indonesia/Citarum-streamflow.txt') %>% 
  .[, .(year = Tahun,
        Qa = rowMeans(.SD),
        ID = 'ID_0000024'),
    .SDcols = 2:13]


# Pakistan ------------------------------------------------------------------------------------

kachora <- fread('01_Data/Indus/Kachora.csv')
kachora[, ID := 'PK_0000001']
kachora[, country := 'PK']
pb <- fread('01_Data/Indus/Partab_Bridge.csv')
pb[, ID := 'PK_0000002']
pb[, country := 'PK']
PK <- rbind(kachora, pb)

# _Other stations (GSIM)-------------------------------------------------------

read_Qa <- function(s, return.type='dt') {
  
  # Read data file
  #   s: a string for station name
  # Two options to return, either a list or a data.table
  
  dt <- as.data.table(read.csv(file = paste0('D:/GSIM/GSIM_indices/TIMESERIES/yearly/', s,'.year'), 
                               skip = 21, sep = ',', stringsAsFactors = F))
  dt[n.missing > 30, MEAN := NA]
  dt <- dt[, .(year = year(date),
               Qa = as.numeric(MEAN))]
  
  out <- if (return.type == 'dt') dt[, ID := s] else list(ID = s, Qa = dt)
  out
}

# not.read <- c('BD_0000001',
#               'CN_0000180', 'CN_0000181',  'CN_0000189',
#               ths)
# to.read <- setdiff(gm$ID, not.read)

to.read <- gm$ID
instQ_raw <- rbindlist(lapply(to.read, read_Qa))
# Fix outlier for RU_422
# The outlier is May flow. Take that as monthly average and sum up all to get annual
# gsim[ID == 'RU_0000422' & year == 1986, Qa := 2791.831]

IN_summary <- instQ_raw[ID %like% 'IN', 
                        .(Q = mean(Qa, na.rm = TRUE)),
                        by = ID
                      ][gm, on = 'ID', nomatch = NULL
                      ][, .(ID, Q, river, station, long, lat)]

# _Combine ---------------------------------------------------------------------

instQ <- rbind(instQ, BD, CN, KR, PH, TH, MN, Indo, mekong, PK) %>% 
  .[order(ID)] %>% 
  .[year <= end.year] %>% 
  .[, country := substr(ID, 1, 2)]

# Final filter to get stations with the required non-missing and non-zero data points
# List of stations that pass the criteria
pass <- instQ[
  # Count number of non-NAs and zeros
  !is.na(Qa), .(.N, country = country[1], zero = sum(Qa == 0)), by = ID 
  # Select stations with non-zeros data only, Also Filter by length again. 
  # The first filter may result in stations with early starting year but 
  # with a lot of missing data in between.
  ][zero == 0 & 
      ((country %in% c('JP', 'IN') & N >= min.length.JPIN) | (!(country %in% c('JP', 'IN')) & N >= min.length))]
# Take top k stations (in terms of data points) for each country
# if (top.k > 0) pass <- pass[order(-N), head(.SD, top.k), by = country]
# Arrange by ID
pass <- pass[order(ID)]
# Filtering  
instQ <- instQ[ID %in% pass$ID][order(ID)]
gm <- gm[ID %in% pass$ID]

# Some stations do not meet the selection criteria in GSIM meta but do meet with my own data,
# so I add them back
gm <- rbind(gm, gmf[ID %in% c('BD_0000001',
                              'CN_0000180', 'CN_0000181', 'CN_0000189',
                              setdiff(ths, gm$ID),
                              mekong$ID)])
# Use station as name for Thailand
gm[ID %in% ths, station := paired.db.no]

# Number of stations
# length(unique(instQ$ID))
# 89

# Number of stations per country
# instQ[, head(.SD, 1), by = ID][, .N, by = country]

# Minimum station length per country
# instQ[, .(.N, country = country[1]), by = ID][, .(minN = min(N)), by = country]
# A good cutoff length is 41 where we can keep Citarum but remove many stations from Japan

# COORDINATES ---------------------------------------------------------------------------------

# Additional stations ----------------------------
# TH.lookup <- gm[ID %in% ths, .(ID, paired.db.no, long, lat, station)] %>% 
#   setnames(old = c('paired.db.no'), new = 'name') %>% 
#   .[, station := station %>% tolower %>% lettercase::str_ucfirst()]
special.xy <- data.table(
  ID = c(
    'BD_0000002', # Hardinge Bridge
    'CN_0000190', # Cuntan
    'CN_0000191', # Hankou
    'CN_0000192', # Luosan
    'CN_0000193', # Wanxian
    'CN_0000194', # Wulong
    'CN_0000195', # Yichang
    'KR_0000005', # Hwachon
    'PH_0000006', # Angat
    'MN_0000002', # Yeruu
    'ID_0000024', # Citarum
    'PK_0000001', # Kachora
    'PK_0000002'  # Partab Bridge
  ),
  long = c(
    89.02965, # BD_0000002
    106.60,   # CN_0000190
    114.2833, # CN_0000191
    113.3667, # CN_0000192
    108.424,  # CN_0000193
    107.76,   # CN_0000194
    111.2833, # CN_0000195
    127.7763, # KR_0000005
    121.2,    # PH_0000006
    106.65,   # MN_0000002
    107.29,   # ID_0000024
    75.25,    # PK_0000001
    74.63     # PK_0000002
  ),
  lat = c(
    24.068145, # BD_0000002
    29.36,     # CN_0000190
    30.5833,   # CN_0000191
    29.6667,   # CN_0000192
    30.765,    # CN_0000193
    29.322,    # CN_0000194
    30.7,      # CN_0000195
    38.11625,  # KR_0000005
    15.0,      # PH_0000005
    49.74,     # MN_0000002
    -6.29,     # ID_0000024
    35.27,     # PK_0000001
    25.78      # PK_0000002
  ),
  name = c('Hardinge Bridge',
           'Cuntan',
           'Hankou',
           'Luoshan',
           'Wanxian',
           'Wulong',
           'Yichang',
           'Hwachon',
           'Angat',
           'Yeruu',
           'Citarum')
) %>% .[, country := substr(ID, 1, 2)]
# Coordinate sources
# BD_0000002: Google Maps
# Cuntan:  https://www.researchgate.net/publication/228401168_forecastinG_water_levels_at_the_YanGtZe_river_with_neural_networKs, also here there's a picture https://www.researchgate.net/publication/258884759_Journal_of_Applied_Mathematics/figures?lo=1 and I can trace this picture on Google Map
# Hankou: https://www.tandfonline.com/doi/pdf/10.1623/hysj.49.2.247.34831
# Luoshan, Yichang: https://www.researchgate.net/publication/324627218_A_new_method_for_calculating_ecological_flow_Distribution_flow_method
# Wanxian: Google Maps
# Wulong: Google Maps, compared to https://link.springer.com/article/10.1007/s12517-019-4262-y

Chinese catchment areas: https://www.sciencedirect.com/science/article/pii/S167492781630065X



# Lookups ----------------------------------------------------------------------

# mekong.lookup <- readRDS('03_Intermediate/mekong_lookup.rds')
# mekong.lookup[, river := 'Mekong']

BD.lookup <- rbind(gm[ID == 'BD_0000001', .(ID, long ,lat, country)],
                   special.xy[ID == 'BD_0000002', .(ID, long, lat, country)]) %>% 
  .[, ':='(name = c('Bahadurabad', 'Hardinge Bridge'),
           river = c('Brahmaputra', 'Ganges'))]

CN.lookup <- rbind(gm[ID %in% c('CN_0000180', 'CN_0000181', 'CN_0000189'), .(ID, long, lat, country)], special.xy[2:7, c(1:3, 5)]) %>% 
  merge(data.table(ID = c('CN_0000180',  # Datong
                          'CN_0000181',  # Bengbu
                          'CN_0000189',  # Boluo
                          'CN_0000190',  # Cuntan
                          'CN_0000191',  # Hankou
                          'CN_0000192',  # Luosan
                          'CN_0000193',  # Wanxian
                          'CN_0000194',  # Wulong
                          'CN_0000195'), # Yichang,
                   name = c('Datong',
                            'Bengbu',
                            'Boluo',
                            'Cuntan', 
                            'Hankou', 
                            'Luoshan', 
                            'Wanxian', 
                            'Wulong', 
                            'Yichang'),
                   river = c('Yangtze',
                             'Yangtze', 'East',
                             rep('Yangtze', 6))), 
        by = 'ID')

CP.lookup <- gm[ID %in% ths,
                .(ID, long, lat, country, name = station, river = 'Chao Phraya')]

# lookups <- rbind(mekong.lookup[], BD.lookup, CN.lookup, CP.lookup)

# GSIM coordinates ----------------------------------------------------------------------------

instQxy <- gm[ID %in% instQ$ID, .(ID, long, lat, name = station, country)] %>% 
  rbind(special.xy,
        mekong.lookup[ID == 'LA_0000016', .(ID, long, lat, name, country = 'LA')]) %>% 
  .[order(ID)]


# KWF ---------------------------------------------------------------------

# Read data & filter points in the MADA domain
kwf <- fread('01_Data/KWFclimate.csv')[between(long, min(mada2xy$long), max(mada2xy$long)) & 
                                         between(lat, min(mada2xy$lat), max(mada2xy$lat))]
# Create RGB plot colour & create cell corner coordinates
kwf[, ':='(col = rgb(.SD), 
           ID = 1:.N,
           x1 = long - 0.25, 
           x2 = long + 0.25,
           y1 = lat  - 0.25,
           y2 = lat  + 0.25),
    .SDcols = 3:5]


