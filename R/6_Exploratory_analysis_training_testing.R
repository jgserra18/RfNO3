require('ggridges')
require('ggplot2')
require('dplyr')

# annual modeling ----

db_annual = preprocess_2000_2019_dataset(split_ratio = 0.85)

## distribution ----

#' @source https://r-charts.com/distribution/ggridges/
 
train_dist = ggplot(data=db_annual$db$train_set, aes(x=resultObservedValue, y=factor(year), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
 # geom_density_ridges2(panel_scaling = 2, color=4, lwd=0.5, linetype=1, fill='white') + 
   stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.10,0.5,0.9)) +
   scale_fill_gradient(low = "white", high = "forestgreen",
                       name = "Tail prob.") + 
  scale_x_continuous(trans='pseudo_log', breaks=c(0,1, 5, 10, 25,50,100, 500), limits=c(0,600), expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  labs(x=expression(Groundwater~concentration~'(mg'~NO[3]^-{}~L^{-1}*')'),
       y='') +
  facet_wrap(.~'Training') + 
  theme_ridges(grid = T, ) + 
  theme(text=element_text(size=14, family='serif'),
        axis.title = element_text(size=15),
        axis.ticks.length = unit(0.25,'cm'),
        axis.ticks = element_line(color='black'),
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))

test_dist = ggplot(data=db_annual$db$test_set, aes(x=resultObservedValue, y=factor(year), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  # geom_density_ridges2(panel_scaling = 2, color=4, lwd=0.5, linetype=1, fill='white') + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.10,0.5,0.9)) +
  scale_fill_gradient(low = "white", high = "forestgreen",
                      name = "Tail prob.") + 
  scale_x_continuous(trans='pseudo_log', breaks=c(0,1, 5, 10, 25,50,100, 500), limits=c(0,600), expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0)) + 
  labs(x=expression(Groundwater~concentration~'(mg'~NO[3]^-{}~L^{-1}*')'),
       y='') +
  facet_wrap(.~'Testing') + 
  theme_ridges(grid = T, ) + 
  theme(text=element_text(size=14, family='serif'),
        axis.title = element_text(size=15),
        axis.ticks.length = unit(0.25,'cm'),
        axis.ticks = element_line(color='black'),
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))
test_dist

dist = ggpubr::ggarrange(train_dist, test_dist,ncol=2, common.legend = T, legend = 'right')
ggsave(plot = dist, filename = './Output/Plots/Distribution_annual.jpeg',dpi=1000, height = 7, width = 9)

## number of data points ----

n_train = db_annual$db$train_set %>%
  group_by(year) %>%
  summarize(n=n())
n_train$part = 'Training'
n_test = db_annual$db$test_set %>%
  group_by(year) %>%
  summarize(n=n())
n_test$part = 'Testing'

datapoints = rbind(n_train, n_test)

all = datapoints %>%
  group_by(year) %>%
  summarize(n=sum(n))
all$part = 'All'

datapoints = rbind(datapoints, all)

no_dp_plot = ggplot(datapoints, aes(x=year,y=n, color=part)) + 
  geom_line(size=1) + 
  labs(x='',y='Number of data points', color='Partitioning') +
  scale_y_continuous(limits=c(0,40000), breaks=seq(0,40000,5000), expand=c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  theme_light() + 
  theme(axis.ticks.length = unit(0.25,'cm'),
        text=element_text(size = 14, family='serif'),
        axis.title = element_text(size=15),
        legend.title = element_text(size=14),
        legend.text = element_text(size=13))
no_dp_plot
ggsave(filename = './Output/Plots/No_points_annual.jpeg',plot = no_dp_plot, dpi=600, height = 4, width = 4)

datapoints

## spatial distribution ----

library(rworldmap)
wr = getMap(resolution = 'high')
wr = sf::st_as_sf(wr)
wr = st_crop(wr, xmin=-15, xmax=35, ymin=30, ymax=71)

SPtrain =SpatialPoints(db_annual$db$train_set[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
SPdftrain =SpatialPointsDataFrame(SPtrain, db_annual$db$train_set)
SPdftrain =st_as_sf(SPdftrain)
SPdftrain = st_buffer(SPdftrain, 0.01)

SPtest =SpatialPoints(db_annual$db$test_set[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
SPdftest =SpatialPointsDataFrame(SPtest, db_annual$db$test_set)
SPdftest =st_as_sf(SPdftest)
SPdftest = st_buffer(SPdftest, 0.01)

SPdftrain$obs = cut(SPdftrain$resultObservedValue, c(0,10,25,50,100,+Inf))
SPdftest$obs = cut(SPdftest$resultObservedValue, c(0,10,25,50,100,+Inf))

map_train = ggplot() + 
  geom_sf(data=wr, fill='gray') + 
  geom_point(data=SPdftrain, aes(x=lon, y=lat, colour=obs), size=0.5, alpha=0.75) + 
  scale_colour_manual(values=c('blue1','green','yellow','orange1','red1'), drop=F, na.translate=F,
                    labels = c('<10','10-25','25-50','50-100','>100'),
                    guide = guide_legend(
                      title.position = 'top',
                      title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                      label.hjust = 0.5,
                      nrow = 5, 
                      reverse = F,
                      label.position = "right"
                    )) +
  guides(colour = guide_legend(override.aes = list(size=3.5))) + 
  coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
  labs(x='',y='', colour= expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1}))))) + 
  facet_wrap(.~'Training') + 
  theme_light() + 
  theme(panel.background = element_rect(fill='lightblue'),
        text=element_text(size=13, family='serif'),
        strip.text = element_text(size=16))

map_test = ggplot() + 
  geom_sf(data=wr, fill='gray') + 
  geom_point(data=SPdftest, aes(x=lon, y=lat, colour=obs), size=0.5, alpha=0.75) + 
  scale_colour_manual(values=c('blue1','green','yellow','orange1','red1'), drop=F, na.translate=F,
                      labels = c('<10','10-25','25-50','50-100','>100'),
                      guide = guide_legend(
                        title.position = 'top',
                        title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                        label.hjust = 0.5,
                        nrow = 5, 
                        reverse = F,
                        label.position = "right"
                      )) +
  guides(colour = guide_legend(override.aes = list(size=3.5))) + 
  coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
  labs(x='',y='', colour= expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1}))))) + 
  facet_wrap(.~'Testing') + 
  theme_light() + 
  theme(panel.background = element_rect(fill='lightblue'),
        text=element_text(size=13, family='serif'),
        strip.text = element_text(size=16))
maps = ggpubr::ggarrange(map_train, map_test,nrow=2, common.legend = T, legend = 'right')
ggsave(plot = maps, filename = './Output/Plots/Maps_annual_training.jpeg',dpi=1000, height = 11, width = 6)


# seasonal modeling ----

## distribution ----
db_seasonal = lapply(c('winter','spring','summer','autumn'), get_seasonal_db)
names(db_seasonal) = c('winter','spring','summer','autumn')

seasonal_dist = function(db,
                         season) {
  
  train_p = ggplot(data=db[[season]]$train_set, aes(x=resultObservedValue, y=factor(year), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
    # geom_density_ridges2(panel_scaling = 2, color=4, lwd=0.5, linetype=1, fill='white') + 
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.10,0.5,0.9)) +
    scale_fill_gradient(low = "white", high = "forestgreen",
                        name = "Tail prob.") + 
    scale_x_continuous(trans='pseudo_log', breaks=c(0,1, 5, 10, 25,50,100, 500), limits=c(0,600), expand=c(0,0)) + 
    scale_y_discrete(expand=c(0,0)) + 
    labs(x=expression(Groundwater~concentration~'(mg'~NO[3]^-{}~L^{-1}*')'),
         y='') +
    facet_wrap(.~'Training') + 
    theme_ridges(grid = T, ) + 
    theme(text=element_text(size=14, family='serif'),
          axis.title = element_text(size=15),
          axis.ticks.length = unit(0.25,'cm'),
          axis.ticks = element_line(color='black'),
          strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))
  
  test_p = test_dist = ggplot(data=db[[season]]$test_set, aes(x=resultObservedValue, y=factor(year), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
    # geom_density_ridges2(panel_scaling = 2, color=4, lwd=0.5, linetype=1, fill='white') + 
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.10,0.5,0.9)) +
    scale_fill_gradient(low = "white", high = "forestgreen",
                        name = "Tail prob.") + 
    scale_x_continuous(trans='pseudo_log', breaks=c(0,1, 5, 10, 25,50,100, 500), limits=c(0,600), expand=c(0,0)) + 
    scale_y_discrete(expand=c(0,0)) + 
    labs(x=expression(Groundwater~concentration~'(mg'~NO[3]^-{}~L^{-1}*')'),
         y='') +
    facet_wrap(.~'Testing') + 
    theme_ridges(grid = T, ) + 
    theme(text=element_text(size=14, family='serif'),
          axis.title = element_text(size=15),
          axis.ticks.length = unit(0.25,'cm'),
          axis.ticks = element_line(color='black'),
          strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))
  dist = ggpubr::ggarrange(train_p, test_p,ncol=2, common.legend = T, legend = 'right')
  
  return(dist)
}

seasonal_no_points = function(db, season) {
  
  n_train = db[[season]]$train_set %>%
    group_by(year) %>%
    summarize(n=n())
  n_train$part = 'Training'
  
  n_test = db[[season]]$test_set %>%
    group_by(year) %>%
    summarize(n=n())
  n_test$part = 'Testing'
  
  datapoints = rbind(n_train, n_test)
  
  all = datapoints %>%
    group_by(year) %>%
    summarize(n=sum(n))
  all$part = 'All'
  datapoints = rbind(datapoints, all)
  no_dp_plot = ggplot(datapoints, aes(x=year,y=n, color=part)) + 
    geom_line(size=1) + 
    labs(x='',y='Number of data points', color='Partitioning') +
    scale_y_continuous(limits=c(0,10500), breaks=seq(0,10000,2500), expand=c(0,0)) +
    scale_x_continuous(expand = c(0,0)) + 
    theme_light() + 
    theme(axis.ticks.length = unit(0.25,'cm'),
          text=element_text(size = 14, family='serif'),
          axis.title = element_text(size=15),
          legend.title = element_text(size=14),
          legend.text = element_text(size=13))
  return(no_dp_plot)  
}


seasonal_spatial_dist = function(db, season) {
  
  SPtrain =SpatialPoints(db[[season]]$train_set[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdftrain =SpatialPointsDataFrame(SPtrain, db[[season]]$train_set)
  SPdftrain =st_as_sf(SPdftrain)
  SPdftrain = st_buffer(SPdftrain, 0.01)
  
  SPtest =SpatialPoints(db[[season]]$test_set[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdftest =SpatialPointsDataFrame(SPtest, db[[season]]$test_set)
  SPdftest =st_as_sf(SPdftest)
  SPdftest = st_buffer(SPdftest, 0.01)
  
  SPdftrain$obs = cut(SPdftrain$resultObservedValue, c(0,10,25,50,100,+Inf))
  SPdftest$obs = cut(SPdftest$resultObservedValue, c(0,10,25,50,100,+Inf))
  
  map_train = ggplot() + 
    geom_sf(data=wr, fill='gray') + 
    geom_point(data=SPdftrain, aes(x=lon, y=lat, colour=obs), size=0.5, alpha=0.75) + 
    scale_colour_manual(values=c('blue1','green','yellow','orange1','red1'), drop=F, na.translate=F,
                        labels = c('<10','10-25','25-50','50-100','>100'),
                        guide = guide_legend(
                          title.position = 'top',
                          title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                          label.hjust = 0.5,
                          nrow = 5, 
                          reverse = F,
                          label.position = "right"
                        )) +
    guides(colour = guide_legend(override.aes = list(size=3.5))) + 
    coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
    labs(x='',y='', colour= expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1}))))) + 
    facet_wrap(.~'Training') + 
    theme_light() + 
    theme(panel.background = element_rect(fill='lightblue'),
          text=element_text(size=13, family='serif'),
          strip.text = element_text(size=16))
  
  map_test = ggplot() + 
    geom_sf(data=wr, fill='gray') + 
    geom_point(data=SPdftest, aes(x=lon, y=lat, colour=obs), size=0.5, alpha=0.75) + 
    scale_colour_manual(values=c('blue1','green','yellow','orange1','red1'), drop=F, na.translate=F,
                        labels = c('<10','10-25','25-50','50-100','>100'),
                        guide = guide_legend(
                          title.position = 'top',
                          title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                          label.hjust = 0.5,
                          nrow = 5, 
                          reverse = F,
                          label.position = "right"
                        )) +
    guides(colour = guide_legend(override.aes = list(size=3.5))) + 
    coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
    labs(x='',y='', colour= expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1}))))) + 
    facet_wrap(.~'Testing') + 
    theme_light() + 
    theme(panel.background = element_rect(fill='lightblue'),
          text=element_text(size=13, family='serif'),
          strip.text = element_text(size=16))
  maps = ggpubr::ggarrange(map_train, map_test,nrow=2, common.legend = T, legend = 'right')
  return(maps)
}




lapply(c('winter','spring','summer','autumn'), function(x) {
  p_dist = seasonal_dist(db = db_seasonal, season = x)
  ggsave(filename = paste0('./Output/Plots/Seasonal_dist_',x,'.jpeg'), plot = p_dist, dpi=600,height = 7, width = 9)
  
  p_points = seasonal_no_points(db = db_seasonal, season=x)
  ggsave(filename = paste0('./Output/Plots/No_points_annual_',x,'.jpeg'),plot = p_points, dpi=600, height = 4, width = 4)
  
  p_sp_dist = seasonal_spatial_dist(db_seasonal, x)
  ggsave(plot = p_sp_dist, filename = paste0('./Output/Plots/Maps_seasonal_',x,'.jpeg'),dpi=1000, height = 11, width = 6)
})
