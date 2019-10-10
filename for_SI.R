# FIGURE S1 -----------------------------------------------------------------

# Paper count 

ppCount <- fread('data/paper-count.csv')

s1 <- ggplot(ppCount, aes(year, count)) +
  geom_bar(stat = 'identity', fill = 'steelblue', width = 0.75) +
  labs(x = 'Year', y = 'Number of papers') +
  scale_x_continuous(breaks = 2006:2019) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray90')) 
ggsave('figures/FigureS1.pdf',
       s1,
       width = 15, height = 6, units = 'cm')

# FIGURE S2 -----------------------------------------------------------------

# Correlation between PDSI and streamflow

selectedName <- c('Wadenapalli', 'C.2', 'Stung Treng', 'Datong')
selectedID <- instQxy[name %in% selectedName, ID]

nbCores <- detectCores() - 1
cl <- makeCluster(nbCores)
registerDoParallel(cl)

corDT <- 
  foreach(madaPoint = split(mada2, by = 'point'),
          .packages = 'data.table', .combine = rbind) %:%
    foreach(Qa = split(instQ[ID %in% selectedID], by = 'ID'), .combine = rbind) %dopar% {
      dt <- merge(madaPoint, Qa, by = 'year')
      corResults <- cor.test(dt$pdsi, dt$Qa, use = 'complete.obs')
      cbind(mada.long = mada2xy[madaPoint[1, point], long],
            mada.lat = mada2xy[madaPoint[1, point], lat],
            instQxy[ID == Qa[1, ID], .(ID, long, lat)],
            rho = corResults$estimate,
            p.value = corResults$p.value)
    }

setnames(corDT, c('mada.long', 'mada.lat', 'long', 'lat'), c('long', 'lat', 'Qlong', 'Qlat'))
corDT %<>% merge(instQxy[, .(ID, name)], by = 'ID')
corDT$name %<>% factor(levels = selectedName)

corDTsignif <- foreach(station = split(corDT, by = 'name'), .packages = 'data.table', .combine = rbind) %dopar%
  signif_area(station, dx = 1, dy = 1)

# Seach area

sxy <- instQxy[name %in% selectedName]
sxy$name %<>% factor(levels = selectedName)
kwf.range <- c(0.2, 0.25)

inputPoints <- 
  foreach(s = sxy %>% split(by = 'name'), 
          .packages = c('data.table', 'geosphere'),
          .combine = rbind) %:%
  foreach(kwf.max = kwf.range, .combine = rbind) %dopar% {
    poi <- s[, c(long, lat)]
    points <- get_mada_by_kwf(poi, madaKwfCells, kwf, kwf.max, 2500)
    data.table(name = s$name,
               kwf = kwf.max,
               mada2xy[points, .(long, lat)])
    
  }

stopCluster(cl)

# Plots
inputPoints[, kwf := paste0('d[KWF] == ', kwf)]
inputPoints$name %<>% factor(levels = selectedName)

corPlot <- ggplot(corDT) +
  geom_raster(aes(long, lat, fill = rho)) +
  geom_segment(aes(x = long - 0.5, xend = long + 0.5, y = lat + 0.5, yend = lat + 0.5),
               size = 0.1,
               data = corDTsignif[top == TRUE]) +
  geom_segment(aes(x = long - 0.5, xend = long + 0.5, y = lat - 0.5, yend = lat - 0.5),
               size = 0.1,
               data = corDTsignif[bottom == TRUE]) +
  geom_segment(aes(x = long - 0.5, xend = long - 0.5, y = lat - 0.5, yend = lat + 0.5),
               size = 0.1,
               data = corDTsignif[left == TRUE]) +
  geom_segment(aes(x = long + 0.5, xend = long + 0.5, y = lat - 0.5, yend = lat + 0.5),
               size = 0.1,
               data = corDTsignif[right == TRUE]) +
  geom_point(aes(Qlong, Qlat), colour = 'red') +
  scale_x_continuous(expand = c(0, 0), labels = pasteLong) +
  scale_y_continuous(expand = c(0, 0), labels = pasteLat) +
  scale_fill_distiller(name = 'Correlation', palette = 'RdBu', direction = 1, 
                       breaks = scales::pretty_breaks(5), limits = c(-0.772, 0.772)) +
  coord_quickmap() +
  labs(x = NULL, y = NULL) +
  facet_wrap(~name, ncol = 1, strip.position = 'right') +
  theme(#axis.text = element_blank(),
        #axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = 'top',
        legend.key.width = unit(0.6, 'cm'),
        strip.background = element_blank()) +
  panel_border('black', size = 0.1)

selectedPointPlot <- ggplot(inputPoints, aes(long, lat)) +
  geom_tile(data = mada2xy, width = 1, height = 1, fill = 'gray') +
  geom_tile(fill = '#4daf4a', width = 1, height = 1) +
  geom_point(data = sxy, colour = 'red') +
  scale_x_continuous(expand = c(0, 0), labels = pasteLong) +
  scale_y_continuous(expand = c(0, 0), position = 'right') +
  coord_quickmap() +
  facet_grid(name ~ kwf, switch = 'y',
             labeller = labeller(kwf = label_parsed)) +
  theme(# axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  panel_border('black', size = 0.1) +
  labs(title = 'Selected MADA grid points')

s2 <- plot_grid(selectedPointPlot, corPlot, ncol = 2, align = 'hv', axis = 'trbl', rel_widths = c(1.75, 1),
                labels = c('a)', 'b)'), label_size = 10)

s2a <- ggdraw(s2) +
  draw_label('(Krishna)',     0.62, 0.8,  size = 10, angle = 90) +
  draw_label('(Chao Phraya)', 0.62, 0.58, size = 10, angle = 90) + 
  draw_label('(Mekong)',      0.62, 0.355, size = 10, angle = 90) +
  draw_label('(Yangtze)',     0.62, 0.145,  size = 10, angle = 90)

ggsave2('figures/FigureS2.pdf', s2a, width = 17, height = 18, unit = 'cm')


# FIGURE S3-----------------------------------------------------------------

selectedRivers <- c('Krishna', 'Chao Phraya', 'Mekong', 'Yangtze')

mtmRes <- lapply(selectedRivers, function(x) {
    dt <- as.matrix(outlet[river == x, .(year, log(Qa))])
    ans <- astrochron::mtm(dt, output = 1, verbose = FALSE, genplot = FALSE) %>% as.data.table() 
    ans[, river := x]
  }) %>% 
  rbindlist() 
mtmRes$river %<>% factor(levels = selectedRivers)

mtmPlot <- ggplot(mtmRes[Frequency > 0.004 & river %in% selectedRivers]) +
  geom_line(aes(log2(1/Frequency), Power * 1e6, colour = 'Streamflow'), size = 0.3) +
  geom_line(aes(log2(1/Frequency), AR1_90_power * 1e6, 
                colour = 'AR(1) noise (90% CL)'), size = 0.3) +
  scale_x_reverse(breaks = log2(2^(c(1:4, 6, 8))), labels = as.character(2^(c(1:4, 6, 8)))) +
  scale_y_log10() +
  scale_colour_manual(name = NULL, values = c('red', 'black')) +
  labs(x = "Period [years]", y = 'Power') +
  facet_wrap(~river, scales = 'free_y', nrow = 1) +
  theme(legend.text = element_text(size = 9),
        legend.position = 'bottom',
        legend.key.width = unit(1, 'cm'))

ggsave2('figures/FigureS3.pdf', mtmPlot, width = 17, height = 6, units = 'cm')
