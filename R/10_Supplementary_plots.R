source('./src/3_Random_forest.R')

library(ggplot2)
library(rworldmap)

wr = getMap(resolution = 'high')
wr = sf::st_as_sf(wr)
wr = st_crop(wr, xmin=-15, xmax=35, ymin=30, ymax=71)
dir.create(path = './Output/Plots/Supplementary_plots')


db = preprocess_2000_2019_dataset(split_ratio = 0.85)
exp_names = db$exp_names

training_p = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_point(data=db$db$train_set, aes(x=lon,y=lat, colour=cut(resultObservedValue, c(0,10,25,40,50,+Inf))),
             size=0.05) +
  scale_colour_manual(values=c('blue1','green1','yellow1','orange1','red1'),
                    labels=c('<10','10-25','25-40','40-50','>50')) + 
  guides(color=guide_legend(override.aes = list(size=3))) + 
 # guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~'Training stations') + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_light() + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',colour=expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1}))))) + 
  theme(legend.position = 'right',
        axis.text =  element_text(size=11),
        legend.background = element_rect(color=NA),
        #  legend.box = element_blank(),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        text=element_text(family='serif'),
        strip.text = element_text(size=14),
        panel.background = element_rect(fill='lightblue1'),
        legend.margin = margin(0,0,0,0,'cm'),
        plot.margin = margin(0,0,0,0,'cm'))

testing_p = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_point(data=db$db$test_set, aes(x=lon,y=lat, colour=cut(resultObservedValue, c(0,10,25,40,50,+Inf))),
             size=0.05) +
  scale_colour_manual(values=c('blue1','green1','yellow1','orange1','red1'),
                      labels=c('<10','10-25','25-40','40-50','>50')) + 
  guides(color=guide_legend(override.aes = list(size=3))) + 
  # guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~'Testing stations') + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_light() + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',colour=expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1}))))) + 
  theme(legend.position = 'right',
        axis.text =  element_text(size=11),
        legend.background = element_rect(color=NA),
        #  legend.box = element_blank(),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        text=element_text(family='serif'),
        strip.text = element_text(size=14),
        panel.background = element_rect(fill='lightblue1'),
        legend.margin = margin(0,0,0,0,'cm'),
        plot.margin = margin(0,0,0,0,'cm'))

stations_p = ggpubr::ggarrange(training_p, testing_p, ncol=2, common.legend = T, legend = 'bottom')
ggsave(filename = './Output/Plots/Supplementary_plots/Annual_stations1.jpeg',dpi=1000, plot = stations_p, height = 7, width = 9)


# annual scatterplot ----

rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new4.rds')

db$db$train_set$prediction =  predict(rf_model, db$db$train_set[,exp_names, with=F])$predictions
db$db$test_set$prediction =  predict(rf_model, db$db$test_set[,exp_names, with=F])$predictions
db$db$train_set$partition ='Training'; db$db$test_set$partition = 'Testing'

scatter_df = data.table::rbindlist(list(db$db$train_set, db$db$test_set))
SP =SpatialPoints(scatter_df[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
SPdf =SpatialPointsDataFrame(SP, scatter_df)
SPdf =st_as_sf(SPdf)
SPdf = st_buffer(SPdf, 0.01)

eu = st_read('./Data/EU/EU.shp'); eu = st_transform(eu, crs='+proj=longlat')
eu = st_make_valid(eu)
station_countries = st_join(SPdf[,'monitoringSiteIdentifier'], eu[,'GEOUNIT'])
station_countries$geometry = NULL
scatter_df = scatter_df[, c('resultObservedValue','prediction','year','partition')]
scatter_df = cbind(scatter_df,station_countries)

require('ggrepel')

p = ggplot(subset(scatter_df, partition=='Testing'), aes(x=resultObservedValue, y=prediction)) + 
  #eom_smooth(method='lm') + 
  geom_point() + 
  geom_point(data=subset(scatter_df, partition=='Testing' & resultObservedValue-prediction>35 & resultObservedValue>200), 
             aes(color=GEOUNIT)) + 
  geom_text_repel(data=subset(scatter_df, partition=='Testing' & resultObservedValue-prediction>35 & resultObservedValue>200), 
                   aes(label=GEOUNIT, color=GEOUNIT), force_pull = 5, max.overlaps = 16, family='serif', size=3) + 
  geom_abline(slope = 1, intercept = 0, color='black') +
  scale_color_viridis_d() + 
  facet_wrap(.~year, ncol=4) + 
  scale_x_continuous(breaks=seq(0,500,150), limits=c(0,500), expand=c(0,0)) +
  scale_y_continuous(breaks=seq(0,600,150), limits=c(0,605), expand=c(0,0)) + 
  theme_light() + 
  labs(x='Observed',y='Predictions', color='Countries') + 
  theme(text = element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.2,'cm'),
        legend.position = 'none',
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        strip.text = element_text(size=14))
p
ggsave(plot=p, filename = './Output/Plots/Supplementary_plots/Testing_scatterplot.jpeg',dpi=600, height = 13, width = 10)

# seasonal model training and testing ----

season_stations = lapply(c('winter','spring','autumn','summer'), function(x) {
  df = get_seasonal_db(x)
  df$train_set$season = x
  df$test_set$season = x
  return(df)
})
names(season_stations) = c('winter','spring','autumn','summer')

season_train = lapply(c('winter','spring','autumn','summer'), function(x) {
  season_stations[[x]][['train_set']]
})
season_test= lapply(c('winter','spring','autumn','summer'), function(x) {
  season_stations[[x]][['test_set']]
})
season_train = data.table::rbindlist(season_train)
season_test = data.table::rbindlist(season_test)


season_test_p = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_point(data=season_test, aes(x=lon,y=lat, colour=cut(resultObservedValue, c(0,10,25,50,100,+Inf))),
             size=1) +
  scale_colour_manual(values=c('blue1','green1','yellow1','red1','purple'),
                      labels=c('<10','10-25','25-50','50-100','>100')) + 
  # guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~season) + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_light() + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',colour=expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))),
       title='Testing') + 
  theme(legend.position = 'right',
        axis.text =  element_text(size=11),
        plot.title = element_text(size=20, face='bold'),
        legend.background = element_rect(color=NA),
        #  legend.box = element_blank(),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        text=element_text(family='serif'),
        strip.text = element_text(size=14),
        panel.background = element_rect(fill='lightblue1'),
        legend.margin = margin(0,0,0,0,'cm'),
        plot.margin = margin(0,0,0,0,'cm'))

season_train_p = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_point(data=season_train, aes(x=lon,y=lat, colour=cut(resultObservedValue, c(0,10,25,50,100,+Inf))),
             size=1) +
  scale_colour_manual(values=c('blue1','green1','yellow1','red1','purple'),
                      labels=c('<10','10-25','25-50','50-100','>100')) + 
  # guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~season) + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_light() + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',colour=expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))),
       title='Training') + 
  theme(legend.position = 'right',
        axis.text =  element_text(size=11),
        plot.title = element_text(size=20, face='bold'),
        legend.background = element_rect(color=NA),
        #  legend.box = element_blank(),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        text=element_text(family='serif'),
        strip.text = element_text(size=14),
        panel.background = element_rect(fill='lightblue1'),
        legend.margin = margin(0,0,0,0,'cm'),
        plot.margin = margin(0,0,0,0,'cm'))
season_train_p
seasonal_p = ggpubr::ggarrange(season_train_p,season_test_p, nrow=2)
ggsave(filename = './Output/Plots/Supplementary_plots/Seasonal_stations.jpeg',dpi=1000, height = 15, plot = seasonal_p)

# seasonal validation ----

val_season = lapply(c('winter','spring','autumn','summer'), function(x) {
  v = validate_final_Seasonal_rf_model(exp_names, x)
  v = v$test_metrics$annual_metrics
  v$season = x
  return(v)
})
val_season = data.table::rbindlist(val_season)

val_season %>%
  group_by(season) %>%
  summarise(
    avg=mean(kgem),
    sd = sd(kgem)
  )
mean(c(0.552,0.514,0.556,0.406))

val_season_p = reshape2::melt(val_season, c('yrs','season'))
val_season_p$f = factor(val_season_p$variable, labels=c('r','RMSE','Beta','Upsilon','KGEM'))

ggplot(val_season_p, aes(x=yrs,y=value, color=season)) + 
  geom_line(size=1) + 
  scale_color_manual(values= wesanderson::wes_palette("Darjeeling2", n = 4)) +
  facet_wrap(.~f, scales = 'free_x', ncol = 2, labeller = label_parsed) + 
  labs(x='Years',y='Metric performance in the testing set', colour='Season') + 
  theme_light() + 
  theme(legend.position = c(0.7,0.15),
        text=element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.25,'cm'),
        legend.title = element_text(size=15, face='bold'),
        legend.text = element_text(size=14),
        axis.title = element_text(size=16, face = 'bold'),
        strip.text = element_text(size=17),
        axis.text = element_text(size=14)) +
  coord_flip()
ggsave(filename = './Output/Plots/Supplementary_plots/Seasonal_validation.jpeg',dpi=1000)



# nvz ----
