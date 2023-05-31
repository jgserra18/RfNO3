source('./src/3_Random_forest.R')
source('./src/3_XGBoost.R')
source('./src/4_hypertune_seasonal_parameters.R')

require('ggplot2')

dir.create(path = './Output/NO3/rf/validation')

#TODO: 
# rf metrics performance ----


compute_rf_metrics = function(db, 
                              rf_model=NULL,
                              exp_names) {
  #' @source https://iopscience.iop.org/article/10.1088/2515-7620/acabb7#ercacabb7s3 
  
  if (missing(rf_model)==F) { db$prediction =  predict(rf_model, db[,exp_names, with=F])$predictions }

  bias = mean(db$prediction)/mean(db$resultObservedValue)
  r = (sd(db$prediction)/mean(db$prediction))/(sd(db$resultObservedValue)/mean(db$resultObservedValue))
  cor = summary(lm(resultObservedValue~prediction,data=db))$r.squared
  
  kgem = 1-sqrt((cor-1)^2+(bias-1)^2+(r-1)^2)
  
  return(list(
    bias = bias,
    r = r,
    cor = cor,
    kgem = kgem,
    rmse = Metrics::rmse(actual = db$resultObservedValue, predicted = db$prediction)
  ))
}

## all model ----

db_annual = preprocess_2000_2019_dataset(split_ratio = 0.85)
exp_names = db_annual[['exp_names']]
db_annual = db_annual[['db']]
rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new4.rds')
rf_all_perf = compute_rf_metrics(db = db_annual$test_set, rf_model = rf_model, exp_names = exp_names)
rm(list=c('db_annual','rf_model')); gc()


# validating generalized annual model 2000-2019 ----
## RF ----

#data = preprocess_2000_2019_dataset()
#db = data[['db']]
#exp_names = data[['exp_names']]

annual_rf_validation = validate_final_rf_model(db = db_annual, exp_names = exp_names, rf_model)
saveRDS(annual_rf_validation, './Output/NO3/rf/validation/annual_validation4.rds')
annual_rf_validation = readRDS('./Output/NO3/rf/validation/annual_validation.rds')


performance_df = reshape2::melt(annual_rf_validation$test_metrics$annual_metrics, 'yrs')
performance_df$f = factor(performance_df$variable, labels=c('r','RMSE','Beta','Upsilon','KGEM'))

validation_annual = ggplot(performance_df, aes(x=yrs, y=round(value, 2), color=variable)) + 
  geom_smooth(method='lm', linetype='dashed') + 
  geom_line(size=0.7) + 
  scale_color_manual(values= wesanderson::wes_palette("Darjeeling1", n = 5)) +
  facet_wrap(.~f, scales = 'free_y', ncol = 2, labeller = label_parsed) + 
  labs(x='Years',y='') + #y='Metric performance in the testing set') + 
  theme_light() + 
  theme(legend.position = 'none',
        text=element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.25,'cm'),
        axis.title = element_text(size=14, face = 'bold'),
        strip.text = element_text(size=13),
        axis.text = element_text(size=12)) #+
  #coord_flip()
validation_annual
ggsave(plot = validation_annual, filename = './Output/Plots/Performance_testing.jpeg',dpi=1000, height = 6.9, width = 6.6)




annual_rf_map_validation = validate_annual_maps(yrs = 2000:2019)
#saveRDS(annual_rf_map_validation, './Output/NO3/rf/validation/annual_maps_validation.rds')
annual_rf_map_validation = readRDS('./Output/NO3/rf/validation/annual_maps_validation.rds')

db = annual_rf_map_validation$annual_all_df
names(db)[5] = 'prediction'

annual_maps_perf = lapply(2000:2019, function(x) {
  
  compute_rf_metrics(db = subset(db, yr==x), exp_names = exp_names)
})
names(annual_maps_perf) = 2000:2019

annual_maps_perf = data.table::rbindlist(annual_maps_perf)
annual_maps_perf$yrs = 2000:2019
annual_maps_perf = reshape2::melt(annual_maps_perf, 'yrs')
annual_maps_perf$f = factor(annual_maps_perf$variable, labels=c("Beta", 'r','Upsilon','KGEM','RMSE'))


p = ggplot(annual_maps_perf, aes(x=yrs, y=value, color=variable)) + 
  geom_line(size=1.2) + 
  geom_smooth(method='lm') + 
 # geom_point(size=3) + 
  scale_color_manual(values= wesanderson::wes_palette("Darjeeling1", n = 5)) +
  facet_wrap(.~f, scales = 'free_y', ncol = 2, labeller = label_parsed) + 
  labs(x='Years',y='Metric performance') + 
  scale_x_continuous(breaks=seq(2000,2019,5)) + 
  theme_light() + 
  theme(legend.position = 'none',
        text=element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.25,'cm'),
        axis.title = element_text(size=16, face = 'bold'),
        strip.text = element_text(size=17),
        axis.text = element_text(size=14))
p

ggsave(plot = p, filename = './Output/Plots/Performance_years.jpeg',dpi=1000, height = 6.9, width = 6.6)


##xgboost ----
dir.create(path = './Output/NO3/xgboost/validation')

db = readRDS('./Data/explanatory vaars/All_dataset.rds')
set.seed(300)
db = partition_dataset(db)

#annual_xgboost_validation = validate_final_xgboos_model(db = db, exp_names = exp_names)
#saveRDS(annual_xgboost_validation, './Output/NO3/xgboost/validation/annual_validation.rds')
annual_xgboost_validation = readRDS('./Output/NO3/xgboost/validation/annual_validation.rds')



# validating seasonal annual model 2000-2019 ----

store_eea = readRDS('./Data/explanatory vaars/Seasonal_datasets_2000_2019.rds')
exp_names = names(store_eea)[9:ncol(store_eea)]

seasonal_rf_validation = lapply(c('winter','autumn','summer','spring'), function(x) validate_final_Seasonal_rf_model(exp_names, x))
names(seasonal_rf_validation) = c('winter','autumn','summer','spring')


# PLOTTING ----

## annual performance xgboost vs rf @model ----
dir.create(path = './Output/Plots/')

rf_train = annual_rf_validation$train_metrics$annual_metrics; rf_train$model = 'Random forest'; rf_train$part = 'Training'
rf_test = annual_rf_validation$test_metrics$annual_metrics; rf_test$model = 'Random forest'; rf_test$part = 'Testing'

xgb_train = annual_xgboost_validation$train_metrics$annual_metrics; xgb_train$model = 'XGBoost'; xgb_train$part = 'Training'
xgb_test = annual_xgboost_validation$test_metrics$annual_metrics; xgb_test$model = 'XGBoost'; xgb_test$part = 'Testing'

metrics = data.table::rbindlist(list(rf_train, rf_test, xgb_train, xgb_test))

r2 = ggplot(metrics,
       aes(x=yrs,y=r2, color=model)) + 
  geom_smooth(method='lm', fill='gray', size=1)+ 
  geom_line(size=1.5) + 
  facet_wrap(.~part) + 
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) + 
  theme_light() + 
  labs(x='',y=expression(R^{2}), color='Model') + 
  theme(text = element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.2,'cm'),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        strip.text = element_text(size=16))
r2
rmse = ggplot(metrics,
              aes(x=yrs,y=rmse, color=model)) + 
  geom_smooth(method='lm', fill='gray', size=1)+ 
  geom_line(size=1.5) + 
  facet_wrap(.~part) + 
  scale_y_continuous(limits=c(0,50), expand=c(0,0)) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) + 
  theme_light() + 
  labs(x='',y=expression(RMSE), color='Model') + 
  theme(text = element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.2,'cm'),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        strip.text = element_text(size=16))

all_performance_plot = ggpubr::ggarrange(r2, rmse, nrow=2, common.legend = T, legend = 'bottom')
ggsave(plot = all_performance_plot, filename = './Output/Plots/R2_comparison_xgboost_rf.jpeg',dpi=600)

# seasonal metric performance ----

## annual r2 and rmse@model ----
perf = annual_rf_map_validation$df_metrics
perf = perf[, c('yrs','train_r2','test_r2','train_rmse','test_rmse')]

train_df = perf[, c('yrs','train_r2','train_rmse')]; names(train_df) = c('yrs','r2','rmse'); train_df$part = 'Training'
test_df = perf[, c('yrs','test_r2','test_rmse')]; names(test_df) = c('yrs','r2','rmse'); test_df$part = 'Testing'

perf = data.table::rbindlist(list(train_df, test_df))

r2 = ggplot(subset(perf, yrs>1999), aes(x=yrs, y=r2)) + 
  geom_smooth(method='lm',fill='gray', colour='green4', size=1) + 
  geom_line(size=1.5, colour='green4') +
  facet_wrap(.~part) + 
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) + 
  theme_light() + 
  labs(x='',y=expression(R^{2}), color='Model') + 
  theme(text = element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.2,'cm'),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        strip.text = element_text(size=16))
r2
rmse = ggplot(subset(perf, yrs>1999), aes(x=yrs, y=rmse)) + 
  geom_smooth(method='lm',fill='gray', colour='orange3', size=1) + 
  geom_line(size=1.5, colour='orange3') +
  facet_wrap(.~part) + 
  scale_y_continuous(limits=c(0,25), expand=c(0,0)) + 
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) + 
  labs(x='',y=expression(RMSE), color='Model') + 
  theme_light() + 
  theme(text = element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.2,'cm'),
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        strip.text = element_text(size=16))
rmse
seasonal_performance_plot = ggpubr::ggarrange(r2, rmse, nrow=2)
ggsave(plot = seasonal_performance_plot, filename = './Output/Plots/Seasonal_map_performance.jpeg',dpi=600)

## scatterplot seasonal ----

train = annual_rf_map_validation$annual_train_df; train = subset(train, yr>1999); train$train = 'Training'
test = annual_rf_map_validation$annual_test_df; test = subset(test, yr>1999); test$train = 'Testing'
df_scatter = data.table::rbindlist(list(train, test))

scatter = ggplot(df_scatter, aes(x=pred_no3, y=resultObservedValue)) + 
  geom_point(aes(color=train), size = 0.6) +
  geom_abline(slope = 1, intercept = 0, colour='black') +
  geom_smooth(method='lm', colour='forestgreen',se=F) + 
  facet_wrap(yr~train, ncol = 6) + 
  labs(x=expression('Predicted ('*mg~NO[3]^-{}~L^{-1}*')'),
       y=expression('Observed ('*mg~NO[3]^-{}~L^{-1}*')'),
       colour='Partition') + 
  scale_y_continuous(expand=c(0,0), breaks=seq(0,600,200), limits=c(0,605)) + 
  scale_x_continuous(expand=c(0,0), breaks=seq(0,400,200), limits=c(0,530)) + 
  theme_light() + 
  theme(legend.position = 'none',
        legend.direction = 'horizontal',
        text = element_text(size=13, family='serif'),
        axis.ticks.length = unit(0.2,'cm'),
        axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        strip.text = element_text(size=11,
                                  margin = margin(0,0,0,0, "cm")))
scatter
ggsave(plot = scatter, filename = './Output/Plots/Seasonal_scatterplot.jpeg',dpi=600)

# gis maps ----

library(rworldmap)
require('ggthemes')
library(ggnewscale) # new_scale_fill


wr = getMap(resolution = 'high')
wr = sf::st_as_sf(wr)
wr = st_crop(wr, xmin=-15, xmax=35, ymin=30, ymax=71)

prepare_gis_map = function(filepath) {
    
  no3_yr = data.table::fread(filepath)
  no3_yr = no3_yr[, c('x','y', paste0('X',c(1961,1970,1980,1990,1992,2000,2004, 2010,2019)), 'Pearson','Pvalue'), with=F]

  no3_yr$X1961 = cut(no3_yr$X1961, breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X1970 = cut(no3_yr$X1970,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X1980 = cut(no3_yr$X1980,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X1990 = cut(no3_yr$X1990,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X1992 = cut(no3_yr$X1992,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X2000 = cut(no3_yr$X2000,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X2004 = cut(no3_yr$X2004,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X2010 = cut(no3_yr$X2010,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$X2019 = cut(no3_yr$X2019,  breaks=c(0,10,25,40,50,+Inf))
  no3_yr$Pvalue = ifelse(no3_yr$Pvalue<=0.05, '<=0.05','>0.05') 
  
  return(no3_yr)
}

plot_selected_yrs = function(dt_no3) {
  
  annual_p = lapply(paste0('X',c(1992,2004,2010,2019)), function(yr) {
    
    if (yr=='X2019') leg = T else leg = F
    p = ggplot() + 
      geom_sf(data=wr, fill='gray') + 
      geom_tile(data=om, aes(x=x,y=y, fill=fill), show.legend = F) +
      scale_fill_distiller(palette='Blues') + 
      new_scale_fill() + 
      geom_tile(data=dt_no3, aes_string(x='x',y='y', fill=as.character(yr)),show.legend = leg) + 
      scale_fill_manual(values=c('blue1','green','yellow','red'), drop=F, na.translate=F,
                        labels = c('<10','10-25','25-50','>50'),
                        guide = guide_legend(
                          title.position = 'top',
                          title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                          label.hjust = 0.5,
                          nrow = 4,
                          reverse = F,
                          label.position = "right"
                        )) + 
      scale_y_continuous(breaks=c(35,48,60,70)) + 
      scale_x_continuous(breaks=c(-10,5,20,34)) + 
      geom_sf(data=wr, fill=NA, color='azure4', size=2) +
      coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +      facet_wrap(.~gsub('X','', as.character(yr))) +
      labs(x='',y='') + 
      theme_stata(scheme = 's1mono') + 
      theme(legend.position = 'right',
            axis.text =  element_text(size=11),
            text=element_text(family='serif'),
            strip.text = element_text(size=14),
            panel.background = element_rect(fill='lightblue1'))
    
    return(p)
  })
  return(annual_p)
}


plot_metrics = function(dt_no3) {

  metrics_p = list(
    pearson = ggplot() + 
      geom_sf(data=wr, fill='gray') + 
      geom_tile(data=dt_no3, aes(x=x,y=y, fill=Pearson)) + 
      scale_fill_viridis_c() + 
      facet_wrap(.~gsub('X','', as.character(yr))) +
      labs(x='',y='', fill='Rho') + 
      theme_light() + 
      coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
      theme(text=element_text(family='serif'),
            strip.text = element_text(size=14),
            panel.background = element_rect(fill='lightblue1')),
    pval = ggplot() + 
      geom_sf(data=wr, fill='gray') + 
      geom_tile(data=dt_no3, aes_string(x='x',y='y', fill='Pvalue')) + 
      scale_fill_manual(values=c('forestgreen','gray'), drop=F, na.translate=F,
                        labels = c('<=0.05','>0.05'),
                        guide = guide_legend(
                          title.position = 'top',
                          title =  'P-value', 
                          label.hjust = 0.5,
                          nrow = 2,
                          reverse = F,
                          label.position = "right"
                        )) + 
      facet_wrap(.~gsub('X','', as.character(yr))) +
    coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
      labs(x='',y='') + 
      theme_light() + 
      theme(text=element_text(family='serif'),
            strip.text = element_text(size=14),
            panel.background = element_rect(fill='lightblue1'))
  )
  return(metrics_p)
}

fnames = list.files(path = './Output/NO3/rf/', pattern = '.csv')
fnames = gsub('.csv','', fnames)

files = list.files(path = './Output/NO3/rf/', pattern = '.csv', full.names = T)

lapply(1:length(files), function(x) {
  dt = prepare_gis_map(files[x])
  p_yrs = plot_selected_yrs(dt); p_yrs = ggpubr::ggarrange(plotlist = p_yrs, nrow=3, ncol=3, common.legend = T, legend = 'right')
  
  p_metrics = plot_metrics(dt); p_metrics = ggpubr::ggarrange(plotlist = p_metrics, ncol=1, nrow=2)
  
  ggsave(filename = paste0('./Output/Plots/Maps_',fnames[x],'.jpeg'),dpi=1200, plot = p_yrs, height = 9, width = 7.5)
  ggsave(filename = paste0('./Output/Plots/Maps_',fnames[x],'.tiff'),dpi=1200, compress='lzw', plot = p_yrs, height = 9, width = 7.5)
  
  ggsave(filename = paste0('./Output/Plots/Metrics_',fnames[x],'.jpeg'),dpi=1000, plot = p_metrics, height = 9, width = 4.5)
})


gis_map_plot = function(yrs=c(1961,1970,1980,1990,2000,2010,2019)) {
  
  dt_no3 = prepare_gis_map('./Output/NO3/rf/Model0019_new4_1961_2019.csv')
  
  yrs=c(1992,2004,2010, 2019)
  store_p = lapply(c(paste0('X',yrs)), function(yr) {
    print(yr)
    if (yr=='Pearson') {
      p = ggplot() + 
        geom_sf(data=wr, fill='gray') + 
        geom_tile(data=dt_no3, aes(x=x,y=y, fill=Pearson)) + 
        scale_fill_viridis_c() + 
        facet_wrap(.~'Pearson coefficient') +
        labs(x='',y='', fill='Rho') + 
        theme_light() + 
        coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
        theme(text=element_text(family='serif'),
              strip.text = element_text(size=14),
              panel.background = element_rect(fill='lightblue1'))
    }
    else if (yr=='Pvalue') {
      p = ggplot() + 
        geom_sf(data=wr, fill='gray') + 
        geom_tile(data=dt_no3, aes_string(x='x',y='y', fill='Pvalue')) + 
        scale_fill_manual(values=c('forestgreen','gray'), drop=F, na.translate=F,
                          labels = c('<=0.05','>0.05'),
                          guide = guide_legend(
                            title.position = 'top',
                            title =  'P-value', 
                            label.hjust = 0.5,
                            nrow = 2,
                            reverse = F,
                            label.position = "right"
                          )) + 
        facet_wrap(.~'P-value') +
        coord_sf(xlim = c(-15,34.58), ylim = c(34.6, 71.0), expand=F, clip='on', crs = "+proj=longlat") +
        labs(x='',y='') + 
        theme_light() + 
        theme(text=element_text(family='serif'),
              strip.text = element_text(size=14),
              panel.background = element_rect(fill='lightblue1'))
    }
    else {
      p = ggplot() + 
        geom_sf(data=wr, fill='gray') + 
        geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
        scale_fill_distiller(palette='Blues') + 
        new_scale_fill() + 
        geom_tile(data=dt_no3, aes_string(x='x',y='y', fill=as.character(yr)), show.legend = F) + 
        scale_fill_manual(values=c('blue1','green','yellow','orange1','red'), drop=F, na.translate=F,
                          labels = c('<10','10-25','25-40','40-50','>50'),
                          guide = guide_legend(
                            title.position = 'top',
                            title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                            label.hjust = 0.5,
                            nrow = 5,
                            reverse = F,
                            label.position = "right"
                          )) + 
        geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
        coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
        facet_wrap(.~gsub('X','', as.character(yr))) +
        labs(x='',y='') + 
        theme_light() + 
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
    }
    return(p)
  })

  #pp = ggpubr::ggarrange(plotlist = store_p,legend = 'right', ncol=3, nrow=3, common.legend = T)
  pp = ggpubr::ggarrange(plotlist = store_p,legend = 'right', nrow=2, ncol=2, common.legend = T)
}

leg = ggplot(no3, aes(x=x,y=y,fill=cut(X2019, c(0,10,25,40,50,+Inf)))) + 
  geom_tile() + 
  scale_fill_manual(values=c('blue1','green','yellow','orange1','red'), drop=F, na.translate=F,
                    labels = c('<10','10-25','25-40','40-50','>50'),
                    guide = guide_legend(
                      title.position = 'top',
                      title =  expression(atop('Groundwater', paste((mg~NO[3]^-{}~L^{-1})))), 
                      label.hjust = 0.5,
                      nrow = 5,
                      reverse = F,
                      label.position = "right"
                    )) + 
  theme_light() + 
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
leg = ggpubr::get_legend(leg)
ggpubr::as_ggplot(leg)
ggsave(filename='./Output/Plots/legend_vert.jpeg',dpi=1000)

gis = gis_map_plot(yrs = c(1961,1991,2019))

#ggsave(plot=pp, filename='./Output/Plots/Snapshop_1961_1991_2019.jpeg',dpi=1000, height = 7, width = 10)
ggsave(plot=pp, filename='./Output/Plots/Snapshop_1961_1991_2019_vert.jpeg',dpi=1000, height = 8.5, width = 6.5)



# variable importance rf ----
require('dplyr')

rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new3.rds')
imp_df = data.frame(exp=exp_names,
                    imp = matrix(rf_model$variable.importance))
imp_df = imp_df %>%
  arrange(!imp) %>%
  top_n(15)

imp_df =imp_df %>%
  arrange(-imp)
imp_df
imp_df$xlabel = c('Aquifers',
           'Bulk density 0 cm',
           'SOC 10cm ',
           'Latitude',
           'Longitude',
           'SOC 0 cm',
           'Flow length downstream',
           'Rainfed crops',
           'Precipitation December',
           'Irrigated crops',
           'Thickness',
           'Soil pH 0cm',
           'NO3 input from fertilisers',
           'C3 grassland',
           'Manure applied to cropland')

imp_p = ggplot(imp_df, aes(x=forcats::fct_reorder(xlabel, imp),
                   y=imp)) + 
  geom_point(colour='red1', size = 2.5) + 
  geom_segment(aes(x=xlabel, xend=xlabel, y=0, yend=imp), color='black') +
  coord_flip() + 
  theme_light() + 
  scale_y_continuous(expand=c(0,0), limits=c(0,9e6)) + 
  labs(x='Predictors',y='Importance') + 
  facet_wrap(.~'Top 10 predictors') + 
  theme(axis.ticks.y  = element_line(unit(0,'cm')),
        axis.title = element_text(size=14, face = 'bold'),
        text = element_text(family='serif'),
        axis.text = element_text(size=10.5),
        strip.text = element_text(size=13))
imp_p
ggsave(plot=imp_p, filename = './Output/Plots/RF_importance_top15.jpeg',dpi=1000, height = 6, width = 5.6)       

fig1 = ggpubr::ggarrange(validation_annual, imp_p, labels=c('a','b'), 
                         font.label = list(size=14, family='serif'),nrow=2, heights = c(1.95,1))
fig1
ggsave(plot=fig1, filename = './Output/Plots/FIG_2.jpeg',dpi=1000, height = 8, width = 6)
