eu = st_read('./Data/EU/EU.shp')
eu = st_transform(eu, crs='+proj=longlat')
eu = st_make_valid(eu)

db_annual = preprocess_2000_2019_dataset(0.85)
exp_names = db_annual[['exp_names']]
db_annual = db_annual[['db']]
rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new4.rds')

db_annual$test_set$prediction =  predict(rf_model, db_annual$test_set[,exp_names, with=F])$predictions

SP =SpatialPoints(db_annual$test_set[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
SPdf =SpatialPointsDataFrame(SP, db_annual$test_set)
SPdf =st_as_sf(SPdf)
SPdf = st_buffer(SPdf, 0.01)


SPdf = st_join(SPdf, eu[,c('GEOUNIT')])
countries = unique(SPdf$GEOUNIT)
countries = na.omit(countries)

country_metrics = lapply(countries, function(x) {
  print(x)
  return(compute_rf_metrics(db = subset(SPdf, GEOUNIT==x), exp_names = exp_names))
})
names(country_metrics) = countries

nr = sapply(countries, function(x) {
  print(x)
  return(nrow(subset(SPdf, GEOUNIT==x)))
})


d = data.table::rbindlist(country_metrics)
d$country = countries
d$no_points = nr
d = subset(d, !country %in% c('Republic of Serbia','Bosnia and Herzegovina','Kosovo','Montenegro','Albania'))
View(d)
rshp_d = reshape2::melt(d, c('country'))
rshp_d$f = factor(rshp_d$variable, labels=c('Beta','r','Upsilon','KGEM','RMSE','Data_points'))
rshp_d = rshp_d[-which(rshp_d$variable=='kgem' & rshp_d$value<0),]

ggplot(rshp_d, aes(x=forcats::fct_reorder(country, value), y=value)) + 
  geom_point(color='red1',size=3) +
  geom_segment(aes(xend=forcats::fct_reorder(country, value), y=0, yend=value)) + 
  facet_wrap(.~f, ncol = 2, labeller = label_parsed, scales='free_x') + 
  labs(x='', y='Performance metrics per country in the testing partition') +
  theme_light() + 
  theme(legend.position = 'none',
        text=element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.25,'cm'),
        axis.title = element_text(size=16, face = 'bold'),
        strip.text = element_text(size=13),
        axis.text = element_text(size=12)) +
  coord_flip()
View(d)
ggsave(filename = './Output/Plots/Testing_partition_countries.jpeg',dpi=1000, height = 13, width = 8)
