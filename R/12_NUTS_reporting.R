

# model NUTS3 ----
no3 = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')

n3= st_read('c:/Users/João Serra/Desktop/NUTS_RG_20M_2021_3035.shp')
n3 = st_transform(n3, crs='+proj=longlat')

n3 = st_crop(n3, no3)
no3 = no3[[which(as.numeric(gsub('no3_gw_','',names(no3)))>2015)]]
no3 = mean(no3)

n3$mean_model = exactextractr::exact_extract(no3, n3, 'mean')


mean_model_16_19_n3 = ggplot(n3, aes(fill=cut(mean_model, c(0,10, 25,40,50,+Inf)))) + 
  geom_sf(color='white',size=0.01) + 
  scale_fill_manual(labels=c('<10','10-25','25-40','40-50','>50'),
                    values=c('blue1','forestgreen','yellow','orange1','red1')) + 
  labs(fill='Model (mg/L)',
       title='Model average 2016-2019') + 
  theme_light() + 
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  theme(legend.position = 'right',
        axis.text =  element_text(size=11),
        title = element_text(size=18, face='bold'),
        legend.background = element_rect(color=NA),
        #  legend.box = element_blank(),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        text=element_text(family='serif'),
        strip.text = element_text(size=14),
      #  panel.background = element_rect(fill='lightblue1'),
        legend.margin = margin(0,0,0,0,'cm'),
        plot.margin = margin(0,0,0,0,'cm'))
mean_model_16_19_n3
ggsave(filename = './Output/Plots/Supplementary_plots/NUTS_model_2016_2019.jpeg',dpi=1000)

# network NUTS3 ----

db = readRDS('./Data/explanatory vaars/All_dataset_new.rds')
db = subset(db, year>2015)

SP =SpatialPoints(db[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
SPdf =SpatialPointsDataFrame(SP, db)
SPdf =st_as_sf(SPdf)
SPdf = st_buffer(SPdf, 0.04166667)

r_tmp = raster('./Output/NO3/rf/Model0019_1961_2019.tif')
r_tmp = raster(ext=extent(r_tmp), res=res(r_tmp), crs=crs(r_tmp))

store = lapply(2016:2019, function(x) {
  d = subset(SPdf, year==x)
  test = st_join(n3, d[, c('lon','lat','resultObservedValue')]) %>%
    group_by(NUTS_ID) %>%
    summarise(mean(resultObservedValue))
  test$geometry = NULL
  names(test)[2] = paste0('obs_',x)
  n3 = merge(n3, test, 'NUTS_ID')
  
})

for (x in 2016:2019) {
  print(x)
  d = subset(SPdf, year==x)
  test = st_join(n3, d[, c('lon','lat','resultObservedValue')]) %>%
    group_by(NUTS_ID) %>%
    summarise(mean(resultObservedValue))
  test$geometry = NULL
  names(test)[2] = paste0('obs_',x)
  n3 = merge(n3, test, 'NUTS_ID')
}

names(n3)[which(names(n3) =='obs_2016.x')] = 'obs_2016'
d = n3[, c('NUTS_ID',paste0('obs_',2016:2019))]

p1=ggplot(d, aes(fill=cut(obs_2016, c(0,25,40,50,+Inf)))) + 
  geom_sf() + 
  scale_fill_manual(labels=c('<25','25-40','40-50','>50'),
                    values=c('forestgreen','yellow1','orange1','red1')) + 
  labs(fill='Observation (mg/L)')
p2=ggplot(d, aes(fill=cut(obs_2017, c(0,25,40,50,+Inf)))) + 
  geom_sf() + 
  scale_fill_manual(labels=c('<25','25-40','40-50','>50'),
                    values=c('forestgreen','yellow1','orange1','red1')) + 
  labs(fill='Observation (mg/L)')
p3=ggplot(d, aes(fill=cut(obs_2018, c(0,25,40,50,+Inf)))) + 
  geom_sf() + 
  scale_fill_manual(labels=c('<25','25-40','40-50','>50'),
                    values=c('forestgreen','yellow1','orange1','red1')) + 
  labs(fill='Observation (mg/L)')
p4=ggplot(d, aes(fill=cut(obs_2019, c(0,25,40,50,+Inf)))) + 
  geom_sf() + 
  scale_fill_manual(labels=c('<25','25-40','40-50','>50'),
                    values=c('forestgreen','yellow1','orange1','red1')) + 
  labs(fill='Observation (mg/L)')

pp = ggpubr::ggarrange(p1,p2,p3,p4, ncol=2, nrow=2,common.legend = T, legend = 'right',
                       labels = paste0('Year ',2016:2019))
pp
ggsave(filename = './Output/Plots/Supplementary_plots/NUTS_obs_2016_2019.jpeg',dpi=1000, height = 11, width = 9)

