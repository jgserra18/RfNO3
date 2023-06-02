rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new4.rds')

db = preprocess_2000_2019_dataset(split_ratio = 0.85)
exp_names = db$exp_names
db = db$db


# INDIVIDUAL HEURISTIC SA ----

sensitivity_analysis_variable = function(col_name) {
  #' sensitivity analysis for a given variable

  no3_sensitivity = terra::rast(lapply(1992:2019, function(x) {

    predictors = compile_explanatory_variables(yr = x)
    names(predictors) = gsub(' ','_', names(predictors))
    names(predictors) = gsub('-','.', names(predictors))

    predictors = terra::as.data.frame(predictors, xy=T, na.rm=T)
    pred_chang = predictors[[col_name]] # store predictor to change so it can be changed freely in the loop below

    sensitivity_out = lapply(c(0.75,0.9,0.95,1.05,1.1,1.25), function(param_change) {
      boxplot(predictors[,col_name])

      predictors[,col_name] = pred_chang + abs(pred_chang)*(param_change-1)
      print('Starting model prediction ----------')
      pred_general_out = predict(rf_model, predictors)
      df_pred = data.frame(x=predictors$x, y=predictors$y, pred=pred_general_out$predictions)
      print('Finishing -----------')
      r_pred = terra::rast(df_pred, crs='+proj=longlat')

      names(r_pred) = paste0(col_name, '_',x, '_', param_change)
      return(r_pred)
      rm(list=c('pred_general_out','df_pred'))
      gc()
    })
    r_sensitivity_out = terra::rast(sensitivity_out)
    return(r_sensitivity_out)
  }))

  return(no3_sensitivity)
}



compute_individial_heuristic_SA = function(cols=c('N_sur_total_kg_ha_grid_area',
                                                  'nfer_crop_no3', 'nfer_past_no3', 'nman_app_crop')) {
  cols=c('N_sur_total_kg_ha_grid_area')
  sapply(cols, function(col_name) {
    sa_pred = sensitivity_analysis_variable(col_name)
    terra::writeRaster(sa_pred, paste0('./Output/NO3/rf_causality/SA_', col_name, '.tif'))
  })
}

# JOINT SA ----

cols=c('N_sur_total_kg_ha_grid_area',
       'nfer_crop_no3', 'nfer_past_no3', 'nman_app_crop')


no3_sensitivity = lapply(1992:2019, function(x) {

  predictors = compile_explanatory_variables(yr = x)
  names(predictors) = gsub(' ','_', names(predictors))
  names(predictors) = gsub('-','.', names(predictors))

  predictors = terra::as.data.frame(predictors, xy=T, na.rm=T)
  pred_chang = sapply(cols, function(x) predictors[[x]])  # store predictor to change so it can be changed freely in the loop below
  pred_chang = as.data.frame(pred_chang); names(pred_chang) = cols

  sensitivity_out = #lapply(c(0.75,0.9,0.95,1.05,1.1,1.25), function(param_change) {
    lapply(0, function(param_change) {
    predictors[,cols] = sapply(cols, function(x) pred_chang[,x] * param_change)
    print('Starting model prediction ----------')
    pred_general_out = predict(rf_model, predictors)
    df_pred = data.frame(x=predictors$x, y=predictors$y, pred=pred_general_out$predictions)
    print('Finishing -----------')
    r_pred = terra::rast(df_pred, crs='+proj=longlat')

    names(r_pred) = paste0('All_',x, '_', param_change)
    return(r_pred)
    rm(list=c('pred_general_out','df_pred'))
    gc()
  })
  r_sensitivity_out = terra::rast(sensitivity_out)
  return(r_sensitivity_out)
  rm(list = c('predictors'))
})
d = terra::rast(no3_sensitivity)
terra::writeRaster(d, paste0('./Output/NO3/rf_causality/SA_0_all.tif'))

compute_hotspots_EU = function(r_file,
                               filename) {

  fn = names(r_file)
  param_change = as.character(c(0.75,0.9,0.95,1.05,1.1,1.25))
  hotspots = data.frame(matrix(nrow=28, ncol=length(param_change)))
  names(hotspots) = param_change

  hotspots[, param_change] = sapply(param_change, function(x) {

    pattern = which(grepl(paste0('_',x),fn))

    if (length(pattern)!=28) {
      pattern = pattern[which(substr(fn[pattern], start = nchar(fn[pattern])-nchar(x), stop = nchar(fn[pattern])) %in% paste0('_',x))]
    }
    round(terra::global(r_file[[pattern]]>=50, 'sum', na.rm=T)[,1]*(3162*3162)*0.000001/1000,1) # 1000 km2 yr-1
  })
  hotspots$yrs = 1992:2019
  hotspots$param = filename
  hotspots
}

baseline = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')
names(baseline) = paste0('X',1961:2019)
baseline = baseline[[which(as.numeric(gsub('X','', names(baseline)))>1991)]]
df = data.frame(yrs=1992:2019,
                value = terra::global(baseline>=50,'sum',na.rm=T)*(3162*3162)*0.000001/1000)

SA = list.files(path = './Output/NO3/rf_causality/',pattern = 'SA_',full.names = T); fn = list.files(path = './Output/NO3/rf_causality/',pattern = 'SA_')
SA = SA[-which(grepl('aux',SA))]; fn = fn[-which(grepl('aux',fn))]; fn = gsub('.tif','',fn)
SA = lapply(SA, terra::rast)
names(SA) = fn

nm = c('All','N surplus','N fertiliser cropland','N fertiliser grassland','N manure')
SA_t = lapply(1:length(SA), function(x) {
  print(x)
  return(compute_hotspots_EU(r_file = SA[[x]], filename = nm[x]))
})


SA_t = data.table::rbindlist(SA_t)
r_SA_t = reshape2::melt(SA_t, c('yrs','param'))
r_SA_t = subset(r_SA_t, variable != '0')
r_SA_t$baseline = df$sum
r_SA_t$f = r_SA_t$value/r_SA_t$baseline

df_sa_eu = r_SA_t %>%
  group_by(param, variable) %>%
  summarize(avg=mean((f-1)*100),
            sd=sd((f-1)*100))
View(df_sa_eu)


ggplot(df_sa_eu, aes(x=param, y=avg, colour=variable, group=variable)) +
  geom_point(size=3, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.2,
                position=position_dodge(width=0.3)) +
  scale_colour_viridis_d(option = 'H') +
  coord_flip() +
  scale_y_continuous(limits=c(-10,15)) +
  labs(x='',y='Change in baseline (%)', colour='Modifier') +
  theme_light() +
  theme(text = element_text(family='serif'),
        axis.title = element_text(size=17, face='bold'),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=16),
        legend.title = element_text(size=15, face='bold'),
        legend.text = element_text(size=13))
ggsave(filename = './Output/Plots/RF_causality/EU_param_SA.jpeg',dpi=1000)

sa_p_eu = ggplot() +
  geom_hline(yintercept = 1, linetype='dashed',colour='black', size = 1.1) +
  #geom_line(data=df, aes(x=yrs,y=sum), colour='black', size=1.1) +
  geom_line(data=r_SA_t, aes(x=yrs, y=f, colour=factor(variable)), size=1.1, alpha=1) +
  scale_colour_viridis_d(option = 'C') +
  facet_wrap(.~param, scales = 'free_y') +
  scale_y_continuous(limits=c(0.9,1.15), breaks=c(0.9,0.95,1,1.05,1.1,1.15)) +
  theme_bw() +
  labs(x='',y='Fraction relative to baseline',colour='Variation') +
  scale_x_continuous(breaks=c(1992,2000,2010,2019), expand=c(0,0)) +
  theme(#legend.position = c(0.85, 0.65),
    text=element_text(size=14, family='serif'),
    axis.title = element_text(size=18, face='bold'),
    legend.title = element_text(size=16, face='bold'),
    legend.position = c(0.85, 0.25),
    legend.text = element_text(size=13),
    axis.ticks.length = unit(0.25,'cm'),
    panel.grid.major = element_line(color = 'gray'),
    axis.ticks = element_line(color='black'),
    strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))
sa_p_eu
ggsave(filename = './Output/Plots/RF_causality/SA_EU.jpeg',dpi=1000)


# country SA all ----
eu_df = read_sf('./Data/EU/EU.shp')
eu_df = st_transform(eu_df, crs='+proj=longlat')

baseline = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')
names(baseline) = paste0('X',1961:2019)
baseline = baseline[[which(as.numeric(gsub('X','', names(baseline)))>1991)]]
baseline_df = round(exactextractr::exact_extract(baseline>=50, eu_df, 'sum')*(3162*3162)*0.000001/1000, 2)
baseline_df$country = eu_df$GEOUNIT
names(baseline_df) = c(paste0('X',1992:2019), 'country')
baseline_df = reshape2::melt(baseline_df, 'country')




prepare_SA_df = function(r_sa,
                         filename) {

  fn = names(r_sa)
  param=as.character(c(0.75,0.9,0.95,1.05,1.1,1.25))
  sa = lapply(param, function(x) {

    pattern = which(grepl(paste0('_',x),fn))
    if (length(pattern)!=28) {
      pattern = pattern[which(substr(fn[pattern], start = nchar(fn[pattern])-nchar(x), stop = nchar(fn[pattern])) %in% paste0('_',x))]
    }
    return(r_sa[[pattern]])
  })
  names(sa) = paste0('X',param)

  sa_df = lapply(paste0('X',param), function(x) {

    df = round(exactextractr::exact_extract(sa[[x]]>=50, eu_df, 'sum')*(3162*3162)*0.000001/1000, 2)
    df$country = eu_df$GEOUNIT
    names(df) = c(paste0('X',1992:2019), 'country')
    df = reshape2::melt(df, 'country')
    df$param = gsub('X','', x)
    df
  })
  sa_df = data.table::rbindlist(sa_df)
  names(baseline_df)[3] = 'baseline'

  df = plyr::join(sa_df, baseline_df)
  df$f = df$value/df$baseline

  cntr = c('Netherlands', 'Germany', 'Romania', 'Belgium', 'Denmark','Austria','Poland',
           'Portugal','Spain','France','Italy','Czech Republic',
           'Greece','Bulgaria','Hungary','Croatia',
           'Slovenia','United Kingdom')
  df = df[which(df$country %in% cntr),]
  df$yrs = as.numeric(gsub('X','',df$variable))
  df$fname = filename

  df_sa_cntry = df %>%
    group_by(country, param, fname) %>%
    summarize(avg=mean((f-1)*100),
              sd=sd((f-1)*100))

  return(list(
    sa_df = df,
    sa_df_country = df_sa_cntry
  ))
}


sa_links = data.frame(path=c('./Output/NO3/rf_causality/SA_nfer_crop_no3.tif',
                             './Output/NO3/rf_causality/SA_N_sur_total_kg_ha_grid_area.tif',
                             './Output/NO3/rf_causality/SA_nfer_past_no3.tif',
                             './Output/NO3/rf_causality/SA_nman_app_crop.tif',
                             './Output/NO3/rf_causality/SA_all.tif'),
                      name = c('N fertiliser cropland',
                               'N surplus',
                               'N fertiliser grassland',
                               'N manure',
                               'All'))

sa_store = lapply(1:nrow(sa_links), function(x) {

  sa = terra::rast(sa_links[x,'path'])
  sa = prepare_SA_df(sa, sa_links[x,'name'])
  return(sa)
})

sa_df_cntry = data.table::rbindlist(lapply(sa_store, function(x) x$sa_df_country))
sa_df =  data.table::rbindlist(lapply(sa_store, function(x) x$sa_df))
head(sa_df)

View(sa_df%>%
  filter(param=='1.25' & fname=='All') %>%
  group_by(country, fname) %>%
  summarize(avg = mean(f-1)*100,
            max = max(f-1)*100,
            min = max(f-1)*100,
            sd = sd(f-1)*100))
d = sa_df%>%
       filter(fname=='All') %>%
       group_by(country, param) %>%
       summarize(avg = mean(f-1)*100,
                 max = max(f-1)*100,
                 min = min(f-1)*100)
write.csv(d, './this.csv',row.names = F)
ggplot(sa_df_cntry, aes(x=fname, y=avg, colour=param, group=param)) +
  geom_point(size=1.5, position = position_dodge(width=0.8)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.2,
                position=position_dodge(width=0.8)) +
  scale_colour_viridis_d(option = 'H') +
  facet_wrap(.~country, scales = 'free_x') +
  coord_flip() +
  #scale_y_continuous(limits=c(-60,55)) +
  labs(x='',y='Change in baseline (%)', colour='Modifier') +
  theme_light() +
  theme(text = element_text(family='serif'),
        strip.text = element_text(size=13),
        axis.title = element_text(size=17, face='bold'),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=16),
        legend.title = element_text(size=15, face='bold'),
        legend.text = element_text(size=13))

ggsave(filename = './Output/Plots/RF_causality/Country_param_SA.jpeg',dpi=1000, height = 8.5, width = 13)


 ggplot() +
  geom_hline(yintercept = 1, linetype='dashed',colour='black', size = 1.1) +
  #geom_line(data=df, aes(x=yrs,y=sum), colour='black', size=1.1) +
  geom_line(data=df, aes(x=yrs, y=f, colour=factor(param)), size=1.1, alpha=1) +
  scale_colour_viridis_d(option = 'C') +
  facet_wrap(.~country,scales = 'fixed') +
  #scale_y_continuous(limits=c(0.5,1.5), breaks=c(0.9,0.95,1,1.05,1.1,1.15)) +
  theme_bw() +
  labs(x='',y='Fraction relative to baseline',colour='Variation') +
  scale_x_continuous(breaks=c(2000,2010,2019), expand=c(0,0)) +
  theme(#legend.position = c(0.85, 0.65),
    text=element_text(size=14, family='serif'),
    axis.title = element_text(size=18, face='bold'),
    legend.title = element_text(size=16, face='bold'),
    legend.position = c(0.8, 0.05),
    legend.direction = 'horizontal',
    legend.text = element_text(size=13),
    axis.ticks.length = unit(0.25,'cm'),
    panel.grid.major = element_line(color = 'gray'),
    axis.ticks = element_line(color='black'),
    strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))

ggsave(filename = './Output/Plots/RF_causality/SA_country.jpeg',dpi=1000, height = 10, width = 12)


# statistican analysis SA ----
library(purrr)
library(broom)?

## Europe ----

mod=as.character(c(0.75,0.9,0.95,1.05,1.1,1.25))

eu_hot = data.frame(yrs=1992:2019,
                    param='baseline',
                    value=terra::global(baseline>=50,'sum',na.rm=T)*(3162*3162)*0.000001/1000)
names(eu_hot)[3] = 'value'
eu_SA_t = r_SA_t[, c('yrs','param','variable','value')]
subset(eu_SA_t, variable=='0.75' & param=='All')

params = unique(r_SA_t$param)

anova_param = function() {

  anovas = lapply(mod, function(x)   {

    new_df = subset(eu_SA_t, variable==x)[, c('yrs','param','value')]
    new_df = rbind(new_df, eu_hot)
    anv = aov(value~param, data = new_df)
    head(new_df)
    ref = subset(new_df, param=='baseline')[, 'value']

    fit = lm(ref~subset(new_df, param=='All')[,'value'])
    anv = anova(fit)
    tukey
  })
  names(anovas) = paste0('X',mod)

  anovas = map_df(anovas, tidy, .id = 'formulae')
  anovas$param = params
  return(anovas)
}

anova_params = lapply(params, function(x) anova_param(x))
anova_params = data.table::rbindlist(anova_params)
View(anova_params)
