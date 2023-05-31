compile_explanatory_variables_causality = function(yr=c(1961,2019),
                                                   period=c('historical','present')) {
  
  
  print('Gathering explanatory variables .......')
  clim = get_climatic_params(yr)
  n_manag = terra::rast(ifelse(period=='historical', './Data/RF_causality/1961_1990_agri_manag.tif','./Data/RF_causality/2000_2019_agri_manag.tif'))
  lu = terra::rast(ifelse(period=='historical', './Data/RF_causality/1961_1990_lu.tif','./Data/RF_causality/2000_2019_lu.tif'))
  #lu = get_lu_params(yr)
 # n_manag = get_management_params(yr)
  soil = get_soil_params()
  gw = get_groundwater_params()
  wtd = get_wtd()
  
  print('Starting to preprocess and standardize .......')
  all = c(clim, lu, n_manag, soil, gw, wtd)
  # mn = as.numeric(terra::global(all,'min', na.rm=T)[,1])
  # mx = as.numeric(terra::global(all,'max', na.rm=T)[,1])
  
  # all = (all-mn)/(mx-mn)
  all
}


store_annual_dataset_causality = function(yrs=2000:2019,
                                          period) {

  # dir.create(path = './Data/explanatory vaars/stacks/')
  store = lapply(yrs, function(yr) {
    print(yr)
    
    station = get_eea_year(yr)
    exp_vars = compile_explanatory_variables_causality(yr, period)
    #exp_vars = terra::rast(paste0('./Data/explanatory vaars/stacks/Stack_',yr,'.tif'))
    names(exp_vars) = gsub(' ','_', names(exp_vars))
    names(exp_vars) = gsub('-','.', names(exp_vars))
    df_exp_vars = extract_exp_vars(exp_vars, station)
    coords = df_exp_vars[, c('monitoringSiteIdentifier','lon','lat')]
    df_exp_vars$monitoringSiteIdentifier = NULL
    df_exp_vars = na.omit(df_exp_vars)
    df_exp_vars = plyr::join(df_exp_vars, coords)
    
    return(df_exp_vars)
  })
  db = data.table::rbindlist(store)
  
  saveRDS(object = db, paste0('./Data/explanatory vaars/All_dataset_RF_causality_',period,'.rds'))
}


preprocess_RF_causality = function(split_ratio=0.8,
                                   period=c('historical','present')) {
  #' @description reads and partitions all the dataset into 80%training, 20% testing
  
  # read raw dataset for 2000-2019 ----
  #db = readRDS('./Data/explanatory vaars/All_dataset.rds')
  db = readRDS(paste0('./Data/explanatory vaars/All_dataset_RF_causality_',period,'.rds'))
  exp_names = names(db)[6:ncol(db)-1]
  #exp_names = names(db)[6:ncol(db)]
  
  # partition it ----
  set.seed(300)
  db = partition_dataset(db, split_ratio)
  
  return(list(
    exp_names = exp_names,
    db = db
  ))
}

db_historical = preprocess_RF_causality(0.85,'historical')
exp_names = db_historical$exp_names
db_historical = db_historical$db

rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new4.rds')

pred = lapply(1992:2019, function(yr) {
  
  exp_vars = compile_explanatory_variables_causality(yr, period = 'historical')
  
  names(exp_vars) = gsub(' ','_', names(exp_vars))
  names(exp_vars) = gsub('-','.', names(exp_vars))
  exp_names = names(exp_vars)
  exp_vars = terra::as.data.frame(exp_vars, xy=T, na.rm=T)
  
  print('Starting model prediction ----------')
  pred_general_out = predict(rf_model, exp_vars)
  df_pred = data.frame(x=exp_vars$x, y=exp_vars$y, pred=pred_general_out$predictions)
  print('Finishing -----------')
  r_pred = terra::rast(df_pred, crs='+proj=longlat')
  rm(list=c('exp_vars','pred_general_out','df'))
  return(r_pred)
})
pred = terra::rast(pred)
names(pred) = paste0('X',1992:2019)
writeRaster(pred, './Output/NO3/rf_causality/1992_2019_historical_ND_new4.tif')


no3 = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')
names(no3) = paste0('X',1961:2019)
no3 = no3[[which(as.numeric(gsub('X','',names(no3)))>1991)]]

hist = terra::rast('./Output/NO3/rf_causality/1992_2019_historical_ND_new4.tif')
pres = terra::rast('./Output/NO3/rf_causality/1992_2019_present_ND_new4.tif')

hotsp_hist = round(terra::global(hist>=50, 'sum', na.rm=T)[,1] * (3162*3162)*0.000001, 0) #km2
hotsp_pres = round(terra::global(pres>=50, 'sum', na.rm=T)[,1] * (3162*3162)*0.000001, 0) # km2
hotsp_no3 = round(terra::global(no3>=50, 'sum', na.rm=T)[,1] * (3162*3162)*0.000001, 0) # km2


hotspots = data.frame(yrs=1992:2019,
                      hist = hotsp_hist,
                      pres = hotsp_pres,
                      baseline = hotsp_no3)

desc = c('"Baseline"'='black',
         '1961-1990 average'='red1',
         '2000-2019 average'='forestgreen')
ggplot(hotspots, aes(x=yrs)) + 
  geom_line(aes(y=baseline/1000,colour='"Baseline"'), size = 1) + 
  geom_line(aes(y=hist/1000, colour='1961-1990 average'), size = 1, linetype='dashed') + 
  geom_line(aes(y=pres/1000, colour='2000-2019 average'), size = 1,linetype='dashed') + 
  scale_y_continuous(limits=c(70,300)) + 
  scale_colour_manual(values=desc) + 
  labs(x='',y=expression('Hotspots (1000'~km^{2}~yr^{-1}*')'), colour='Scenario') + 
  theme_light() + 
  theme(text=element_text(size=14, family='serif'),
        axis.title = element_text(size=15, face = 'bold'),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'),
        axis.ticks = element_line(color='black'),
        legend.position = 'right',
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')),
        legend.background = element_blank(),
        legend.box.background = element_blank())
ggsave(filename = './Output/Plots/Supplementary_plots/EU_scenario.jpeg',dpi=1000)

baseline_df= round(exactextractr::exact_extract(no3>=50, eu_df, 'sum') * (3162*3162)*0.000001, 0)
names(baseline_df) = paste0('X',1992:2019)
baseline_df$cntry = eu_df$GEOUNIT

hist_df= round(exactextractr::exact_extract(hist>=50, eu_df, 'sum') * (3162*3162)*0.000001, 0)
names(hist_df) = paste0('X',1992:2019)
hist_df$cntry = eu_df$GEOUNIT

present_df= round(exactextractr::exact_extract(pres>=50, eu_df, 'sum') * (3162*3162)*0.000001, 0)
names(present_df) = paste0('X',1992:2019)
present_df$cntry = eu_df$GEOUNIT

present_df = reshape2::melt(present_df, 'cntry')
hist_df = reshape2::melt(hist_df, 'cntry')
baseline_df = reshape2::melt(baseline_df, 'cntry')

names(baseline_df)[3] = 'baseline'
baseline_df$hist = hist_df$value
baseline_df$pres = present_df$value

sb = subset(baseline_df, cntry %in% c('Austria','Belgium','Bulgaria','Czech Republic','Denmark','France','Germany','Greece','Spain',
                                      'Italy','Portugal','Poland','Netherlands','Romania'))
names(sb)[2] ='yr'
sb$yr = as.numeric(gsub('X','',sb$yr))
#sb = reshape2::melt(sb, c('cntry','yr'))

ggplot(sb, aes(x=yr)) + 
 # geom_line(size=1.2) + 
  geom_line(aes(y=baseline,colour='"Baseline"'), size = 1) + 
  geom_line(aes(y=hist, colour='1961-1990 average'), size = 1,linetype='dashed') + 
  geom_line(aes(y=pres, colour='2000-2019 average'), size = 1,linetype='dashed') + 
  scale_colour_manual(values=desc) + 
  facet_wrap(.~cntry, scales = 'free_y') + 
  labs(x='',y=expression('Hotspots (1000'~km^{2}~yr^{-1}*')'), colour='Scenario') + 
  theme_light() + 
  theme(text=element_text(size=14, family='serif'),
        axis.title = element_text(size=15, face = 'bold'),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'),
        axis.ticks = element_line(color='black'),
        legend.position = c(0.8,0.1),
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')),
        legend.background = element_blank(),
        legend.box.background = element_blank())
ggsave(filename = './Output/Plots/RF_causality/Hotspots.jpeg',dpi=1000)



# static ----

n_in_hist = terra::rast('./Data/RF_causality/1961_1990_agri_manag.tif')
n_in_hist = n_in_hist[[-1]]
n_in_hist = sum(n_in_hist)
n_in_hist = round(exactextractr::exact_extract(n_in_hist, eu_df, 'sum'),0)
n_in_hist = sapply(paste0('X',1992:2019), function(x) n_in_hist)
n_in_hist = as.data.frame(n_in_hist)
names(n_in_hist) = paste0('X',1992:2019)
n_in_hist$cntry = eu_df$GEOUNIT
n_in_hist = reshape2::melt(n_in_hist, 'cntry')

n_in_present = terra::rast('./Data/RF_causality/2000_2019_agri_manag.tif')
n_in_present = n_in_present[[-1]]
n_in_present = sum(n_in_present)
n_in_present = round(exactextractr::exact_extract(n_in_present, eu_df, 'sum'), 0)
n_in_present = sapply(paste0('X',1992:2019), function(x) n_in_present)
n_in_present = as.data.frame(n_in_present)
names(n_in_present) = paste0('X',1992:2019)
n_in_present$cntry = eu_df$GEOUNIT
n_in_present = reshape2::melt(n_in_present, 'cntry')

# temporal ----

n_in_temp = list.files(path = './Data/explanatory vaars/spatiotemporal/',pattern='crop|past',full.names = T)

yrs_id = 1961:2019
yrs_id = which(yrs_id>1991)

n_in_temp = terra::rast(lapply(yrs_id, function(x) {
  
  sum(terra::rast(lapply(n_in_temp, function(y) {
    return(terra::rast(y)[[x]])
  })))
}))
names(n_in_temp) = paste0('X',1992:2019)
n_in_temp = round(exactextractr::exact_extract(n_in_temp, eu_df, 'sum'), 0)
names(n_in_temp) = paste0('X',1992:2019)
n_in_temp$cntry = eu_df$GEOUNIT
n_in_temp = reshape2::melt(n_in_temp, 'cntry')
n_in_temp$variable = as.numeric(gsub('X','',n_in_temp$variable))
names(n_in_temp)[3] = 'baseline'
n_in_temp$hist = n_in_hist$value
n_in_temp$pres = n_in_present$value
n_in_temp = subset(n_in_temp, cntry %in% c('Austria','Belgium','Bulgaria','Czech Republic','Denmark','France','Germany','Greece','Spain',
                                      'Italy','Portugal','Poland','Netherlands','Romania'))
ggplot(n_in_temp, aes(x=variable)) + 
  # geom_line(size=1.2) + 
  geom_line(aes(y=baseline,colour='"Baseline"'), size = 1) + 
  geom_line(aes(y=hist, colour='1961-1990 average'), size = 1) + 
  geom_line(aes(y=pres, colour='2000-2019 average'), size = 1) + 
  scale_colour_manual(values=desc) + 
  facet_wrap(.~cntry, scales = 'free_y') + 
  theme_light() + 
  labs(x='',y='Nman + Nfert (kg N yr-1)', colour='') + 
  theme(legend.position = c(0.75,0.05))


ggsave(filename = './Output/Plots/RF_causality/Ninputs.jpeg',dpi=1000)



# static ----

gnb_hist = terra::rast('./Data/RF_causality/1961_1990_agri_manag.tif')
gnb_hist = gnb_hist[[1]]
gnb_hist = round(exactextractr::exact_extract(gnb_hist, eu_df, 'mean'),0)#, quantiles=c(0.10,0.25,0.5,0.75,0.9)),0)
gnb_hist = sapply(paste0('X',1992:2019), function(x) gnb_hist)
gnb_hist = as.data.frame(gnb_hist)
names(gnb_hist) = paste0('X',1992:2019)
gnb_hist$cntry = eu_df$GEOUNIT
gnb_hist = reshape2::melt(gnb_hist, 'cntry')

gnb_pres = terra::rast('./Data/RF_causality/2000_2019_agri_manag.tif')
gnb_pres = gnb_pres[[1]]
gnb_pres = round(exactextractr::exact_extract(gnb_pres, eu_df, 'mean'),0)#, quantiles=c(0.10,0.25,0.5,0.75,0.9)),0)
gnb_pres = sapply(paste0('X',1992:2019), function(x) gnb_pres)
gnb_pres = as.data.frame(gnb_pres)
names(gnb_pres) = paste0('X',1992:2019)
gnb_pres$cntry = eu_df$GEOUNIT
gnb_pres = reshape2::melt(gnb_pres, 'cntry')

gnb_temp = terra::rast('./Data/explanatory vaars/spatiotemporal/N_sur_total_kg_ha_grid_area_1961_2019.tif')
gnb_temp = gnb_temp[[yrs_id]]
gnb_temp = exactextractr::exact_extract(gnb_temp, eu_df, 'mean')
names(gnb_temp) = paste0('X',1992:2019)
gnb_temp$cntry = eu_df$GEOUNIT
gnb_temp = reshape2::melt(gnb_temp, 'cntry')
gnb_temp$variable = as.numeric(gsub('X','',gnb_temp$variable))
names(gnb_temp)[3] = 'baseline'
gnb_temp$hist = gnb_hist$value
gnb_temp$pres = gnb_pres$value
gnb_temp = subset(gnb_temp, cntry %in% c('Austria','Belgium','Bulgaria','Czech Republic','Denmark','France','Germany','Greece','Spain',
                                           'Italy','Portugal','Poland','Netherlands','Romania'))
ggplot(gnb_temp, aes(x=variable)) + 
  # geom_line(size=1.2) + 
  geom_line(aes(y=baseline,colour='"Baseline"'), size = 1) + 
  geom_line(aes(y=hist, colour='1961-1990 average'), size = 1) + 
  geom_line(aes(y=pres, colour='2000-2019 average'), size = 1) + 
  scale_colour_manual(values=desc) + 
  facet_wrap(.~cntry, scales = 'free_y') + 
  theme_light() + 
  labs(x='',y='Mean N surplus (kg N ha-1 yr-1)', colour='') + 
  theme(legend.position = c(0.75,0.05))

ggsave(filename = './Output/Plots/RF_causality/Nsurplus.jpeg',dpi=1000)




# data analysis ----

hist = terra::rast('./Output/NO3/rf_causality/1992_2019_historical_ND_new4.tif')
present = terra::rast('./Output/NO3/rf_causality/1992_2019_present_ND_new4.tif')

baseline = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')
names(baseline) = paste0('X',1961:2019)
baseline = baseline[[which(as.numeric(gsub('X','',names(baseline)))>1991)]]

h_hist = hist; h_hist[h_hist<50] = 0; h_hist[h_hist>=50] = 1
h_present = present; h_present[h_present<50] = 0; h_present[h_present>=50] = 1
h_baseline = baseline; h_baseline[h_baseline<50] = 0; h_baseline[h_baseline>=50] = 1

### Europe ----

hotspot_hist = terra::global(h_hist, 'sum',na.rm=T) * 3162*3162*0.000001/1000 # 1000 km2
hotspot_present  = terra::global(h_present, 'sum',na.rm=T) * 3162*3162*0.000001/1000 # 1000 km2
hotspot_baseline= terra::global(h_baseline, 'sum',na.rm=T) * 3162*3162*0.000001/1000 # 1000 km2

hotspot_df = data.frame(yrs=1992:2019,
                        historical = hotspot_hist,
                        present = hotspot_present,
                        baseline = hotspot_baseline)
names(hotspot_df) = c('yrs','historical','present','baseline')

cor.test(hotspot_df$baseline, hotspot_df$historical, method = 'spearman')
cor.test(hotspot_df$baseline, hotspot_df$historical,method = 'pearson')

rsh_hotspot_df = reshape2::melt(hotspot_df, 'yrs')

ggplot(rsh_hotspot_df, aes(x=yrs,y=value, colour=variable)) + 
  geom_line(size=1.1) + 
  scale_colour_manual(values=c('historical'='red1',
                        'present'='forestgreen',
                        'baseline'='black')) + 
  scale_y_continuous(limits=c(0,270), breaks=seq(0, 300, 50), expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(1991,2019, 5), limits=c(1991, 2019.5)) + 
  labs(x='',y=expression('Hotspots (1000'~km^{2}~yr^{-1}*')'), linetype="Including '15", color='Periods') + 
  # coord_flip() + 
  #  annotate(geom='text', x=1963.5, y=58, label= 'y = -0.46x + 1131,\n       rho = -0.35 (P = 0.007)', family='serif', size = 4.3) + 
  #theme_ridges(grid = T) + 
  theme_light() + 
  theme(text=element_text(size=14, family='serif'),
        axis.title = element_text(size=15, face = 'bold'),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'),
        axis.ticks = element_line(color='black'),
        legend.position = 'right',
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')),
        legend.background = element_blank(),
        legend.box.background = element_blank())

mean(hotspot_df$historical); sd(hotspot_df$historical)
mean(hotspot_df$present); sd(hotspot_df$present)
mean(hotspot_df$baseline); sd(hotspot_df$baseline)

rsp = reshape2::melt(hotspot_df, 'yrs')
write.csv(hotspot_df, './Output/NO3/rf_causality/hotspot_df.csv', row.names = F)
rsp$variable = factor(rsp$variable)
model = aov(value~variable, data = rsp)

ggplot(rsp, aes(x=variable,y=value)) + geom_boxplot()

ref = subset(rsp, variable=='baseline')[,'value']
t.test(ref, subset(rsp, variable=='historical')[,'value'])


### Countries ----
  eu_df = read_sf('./Data/EU/EU.shp')
  eu_df = st_transform(eu_df, crs='+proj=longlat')
  
  df_baseline = exactextractr::exact_extract(h_baseline, eu_df, 'sum')* 3162*3162*0.000001/1000 # 1000 km2
  df_baseline$cntry = eu_df$GEOUNIT
  df_baseline = reshape2::melt(df_baseline, 'cntry')
  df_baseline$period = 'baseline'
  
  df_historical = exactextractr::exact_extract(h_hist, eu_df, 'sum')* 3162*3162*0.000001/1000 # 1000 km2
  df_historical$cntry = eu_df$GEOUNIT
  df_historical = reshape2::melt(df_historical, 'cntry')
  df_historical$period = 'historical'
  
  df_present = exactextractr::exact_extract(h_present, eu_df, 'sum')* 3162*3162*0.000001/1000 # 1000 km2
  df_present$cntry = eu_df$GEOUNIT
  df_present = reshape2::melt(df_present, 'cntry')
  df_present$period = 'present'
  
  data.frame(yrs=1992:2019,
             dif=(subset(df_historical, cntry=='Italy')[,'value']/
    subset(df_baseline, cntry=='Italy')[,'value']-1)*2)
  
  df = rbind(df_present, df_historical, df_baseline)
  df$variable = as.numeric(gsub('sum.X','',df$variable))
  cnt = c('Spain','Italy','Greece','Bulgaria','France','Belgium','Hungary','Poland',
          'Croatia','Portugal','Netherlands','Czech Republic','Romania','Austria','Slovakia',
          'United Kingdom','Germany','Denmark', 'Ireland','Slovenia','Sweden')
  
  df = df[which(df$cntry %in% cnt),]


ggplot(df, aes(x=variable,y=value*1000, colour=period)) + 
  geom_line(size=1.1) + 
  scale_colour_manual(values=c('historical'='red1',
                               'present'='forestgreen',
                               'baseline'='black'),
                      labels = c('historical'='1961-1990',
                                 'present'='2000-2019',
                                 'baseline'='Baseline')) + 
  facet_wrap(.~cntry, scales = 'free_y', ncol = 4) + 
  scale_y_continuous(expand=c(0,0), limits=function(y) c(0,max(y)+0.1*max(y))) + 
  scale_x_continuous(limits=c(1992,2019.5), breaks=c(1992,2000,2010, 2019), expand=c(0,0)) + 
  labs(x='',y=expression('Hotspots ('*km^{2}~yr^{-1}*')'),
       colour='Scenario')  +
  theme_light() + 
  theme(
    axis.title = element_text(size=17, face = 'bold'),
    legend.title = element_text(size=17, face='bold', ),
    legend.text = element_text(size=15),
    axis.ticks.length = unit(0.25,'cm'),
    axis.ticks = element_line(color='black'),
    legend.position = c(0.6,0.05),
    legend.direction = 'horizontal',
    strip.text = element_text(size=12, margin=margin(0.1,0,0.1,0,'cm')),
    text=element_text(size=14, family='serif'),
    legend.background = element_blank(),
    legend.box.background = element_blank())
ggsave(filename = './Output/Plots/RF_causality/Hotspots_country.jpeg',dpi=1000, height = 7, width = 11)

ggplot(subset(df, cntry=='Portugal'), aes(x=variable,y=value*1000, colour=period)) + 
  geom_line(size=1.1)


mean(1-subset(df, cntry=='Netherlands' & period=='historical')[,'value']/
  subset(df, cntry=='Netherlands' & period=='baseline')[,'value'])
mean(1-subset(df, cntry=='France' & period=='historical')[,'value']/
       subset(df, cntry=='France' & period=='baseline')[,'value'])


mean(1-subset(df, cntry=='Portugal' & period=='baseline' & variable>2003)[,'value']/
       subset(df, cntry=='Portugal' & period=='historical' & variable>2003)[,'value'])

anov = lapply(cnt, function(x) {

  ref = subset(df, cntry==x & period=='baseline')[,'value']
  
  ref = subset(rsp, variable=='baseline')[,'value']
  
  t.test(ref, subset(rsp, variable=='historical')[,'value'])
  return(model)
})


write.csv(df_tukey, './Output/NO3/rf_causality/hotspot_tukey.csv')
