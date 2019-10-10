### MAIN PAPER 

# Setup --------------------------------------------------

source('init.R')
source('read_data.R')
source('postprocess_functions.R')
source('read_results_functions.R')
source('plot_metric_map.R')
folder <- 'results/'

# Add metadata for China
instQxy <- rbind(instQxy, instQxyCN)

allRec <- lapply(instQxy$ID, read_station, folder = folder) %>% 
  rbindlist(use.names = TRUE)

# FIGURE 2 - performance scores -----------------------------

# Backgroupd map
bgMap <- sf::st_read('data/mada-coastline.gpkg')
scores <- read_scores(folder, instQxy)

scorePlot <- plot_grid(
  metric_map(scores, 'RE', splitRegions = FALSE, histDirection = 'horizontal',
             dotSize = 0.5, bgMap = bgMap,
             labels = c('a)', 'b)'),
             breakStyle = 'equal', numBreaks = 9, rel = c(1.25, 1)),
  metric_map(scores, 'CE', splitRegions = FALSE, histDirection = 'horizontal',
             dotSize = 0.5, bgMap = bgMap,
             labels = c('c)', 'd)'),
             breakStyle = 'equal', numBreaks = c(9, 3), rel = c(1.25, 1), 
             manualNegBreaks = c(-0.126, -0.064, 0),
             annotateLat = FALSE),
  nrow = 1, align = 'hv', axis = 'trbl'
)

ggsave2('figures/Figure2.pdf', scorePlot, device = cairo_pdf,
       width = 120, height = 100, unit = 'mm')

# FIGURE 3 - Flow history -----------------------------------

Ystd <- get_Ystd(allRec, instQsummary, instQxy)
endYear <- 2012

nSA <- length(Ystd[region == 'SA', unique(code)])
nSEA <- length(Ystd[region == 'SEA', unique(code)])
nCN <- length(Ystd[region == 'CN', unique(code)])
nWA <- length(Ystd[region == 'WA', unique(code)])
nEA <- length(Ystd[region == 'EA', unique(code)])
nCA <- length(Ystd[region == 'CA', unique(code)])

fh <- flow_history(Ystd, 2012, nSA, nSEA, nCN, nWA, nEA, nCA, plotGap = TRUE, raster = FALSE)
ggsave2('figures/Figure3.pdf', fh, width = 240, height = 140, unit = 'mm')

# FIGURE 4 - Correlation maps -----------------------------------------------

sstCor <- readRDS('results/sstCor.RDS')
sstSignifArea <- readRDS('results/sstSignifArea.RDS')
sstCorForPlot <- sstCor[as.numeric(season) %in% 3:9]
sstSignifAreaForPlot <- sstSignifArea[as.numeric(season) %in% 3:9]

corPlot <- plot_sst_cor(sstCorForPlot, sstSignifAreaForPlot, sstLand) + 
  theme(legend.position = 'right', 
        legend.key.width = unit(0.4, 'cm'),
        legend.key.height = unit(1, 'cm'),
        axis.line = element_blank(),
        text = element_text(family = ""))

annotatedCorPlot <- ggdraw(corPlot + theme(plot.margin = margin(l = 0.7, unit = 'cm'))) +
  draw_label('Decaying ENSO', x = 0.02, y = 0.75, angle = 90, size = 10) +
  draw_label('Ongoing ENSO', x = 0.02, y = 0.3, angle = 90, size = 10)

ggsave2('figures/Figure4.pdf', annotatedCorPlot, 
        device = cairo_pdf,
        width = 180, height = 130, unit = 'mm')