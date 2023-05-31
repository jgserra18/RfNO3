require('ggplot2')
library("tidyverse")

eu_df = read_sf('./Data/EU/EU.shp')
eu_df = st_transform(eu_df, crs='+proj=longlat')

regions_to_country = function(region) {
  
  if (region=='south') reg = c('Portugal','Spain','Italy','France')
  if (region=='west') reg = c('Belgium','Netherlands','United Kingdom','Ireland','Germany')
  if (region=='north') reg = c('Sweden','Norway','Finland','Denmark')
  if (region=='central') reg = c('Switzerland','Austria','Poland','Hungary','Czech Republic','Estonia','Lithuania','Slovakia','Latvia','Croatia','Slovenia')
  if (region=='east') reg = c('Romania','Moldova','Bulgaria','Greece','Macedonia','Moldova','Republic of Serbia','Albania')
  
  return(reg)
}

subset_regions = function(eu_df, 
                          region=c('south','west','north','central','east')) {
  #' @source https://en.wikipedia.org/wiki/Eastern_Europe#/media/File:Grossgliederung_Europas-en.svg
   
  reg = regions_to_country(region)
  eu_df = eu_df[which(eu_df$GEOUNIT %in% reg),]
  return(eu_df)
}

extract_data_exact = function(r_file,
                              region,
                              sb_eu_df,
                              fun=c('mean','quantile'),
                              quant=c(0.1,0.5,0.9)) {
  
  countries = regions_to_country(region)
  
  store = lapply(countries, function(x) {
    
    sb = subset(sb_eu_df, GEOUNIT==x)
    sb_r = terra::mask(terra::crop(r_file, terra::ext(sb)), sb)
    
    if (fun=='mean') {
      ext = round(exactextractr::exact_extract(sb_r, sb, fun='mean'), 1)  
    }
    else {
      ext = round(exactextractr::exact_extract(sb_r, sb, fun='quantile', quantiles=quant), 1)
    }
    names(ext) = 1961:2019
    ext$country = x
    ext$region = region
    ext$param = ifelse(fun=='mean','Average',
                       ifelse(fun=='quantile' & quant==0.5, 'Median',
                              paste0(quant*100, 'th quantile')))
    ext = reshape2::melt(ext, c('region','param', 'country'))
    names(ext)[5] = unique(ext$param)
    
    return(ext)
  })
  store = data.table::rbindlist(store)
  return(store)
}

prepare_regional_data = function(r_file,
                                 region) {
  
  sb_eu_df = subset_regions(eu_df, region)
  
  q_10 = extract_data_exact(r_file, region, sb_eu_df, fun='quantile', quant=0.01) 
  q_50 = extract_data_exact(r_file, region, sb_eu_df, fun='quantile', quant=0.5)[,5]
  q_90 = extract_data_exact(r_file, region, sb_eu_df, fun='quantile', quant=0.99)[,5]
  q_avg = extract_data_exact(r_file, region, sb_eu_df, fun='mean')[,5]
  
  store = list(q_10, q_50, q_avg, q_90)
  store = Reduce(function(...) cbind(...), store)
  store$variable = as.numeric(as.character(store$variable))
  names(store)[c(5, 6,7,8)] = c('q_10','Median','Average','q_90')
  
  return(store)
  rm(list=c('q_10','q_50','q_90','q_avg'))
}

r_file = terra::rast('./Output/NO3/rf/Model0019_new_1961_2019.tif')

regional_store = lapply(c('south','west','north','central','east'), function(x) { 
  prepare_regional_data(r_file = r_file, region = x)
})
regional_store = data.table::rbindlist(regional_store)
regional_store = subset(regional_store, variable>1992)
cnt = c('Portugal','Spain','France','Germany','Sweden','Greece','Italy','Netherlands','Austria','United Kingdom',
        'Romania','Denmark','Ireland','Bulgaria','Czech Republic','Poland')

sb_regional_store = subset(regional_store, country %in% cnt)
d = summary(lm(Average~variable, data=subset(sb_regional_store, country==x)))

t = data.frame(country=cnt,
               eqs = sapply(cnt, function(x) {
                     coefs = lm(Average~variable, data=subset(sb_regional_store, country==x))$coefficients
                     p = summary(lm(Average~variable, data=subset(sb_regional_store, country==x)))$coefficient[2,4]
                     r2 = round(summary(lm(Average~variable, data=subset(sb_regional_store, country==x)))$r.squared, 2)
                     
                     if (coefs[1]<0) {
                       eq = paste0('y = ',round(coefs[2],2),'x - ', round(coefs[1]*-1,0), '\np = ',round(p,3), ', r = ', r2)
                     }
                     else {
                       eq = paste0('y = ',round(coefs[2],2),'x + ', round(coefs[1],0), '\np = ',round(p,3), ', r = ', r2)
                     }
                     
                     return(eq)
               })
)

p = ggplot(sb_regional_store, aes(x=as.numeric(variable))) + 
  geom_smooth(aes(y=Average), method='lm',colour='black',size=1, se=F) +
  geom_hline(yintercept = 50, linetype='dashed',colour='black') + 
  geom_line(aes(y=Median, color=country, linetype='Median'), size=1.3) + 
  geom_line(aes(y=Average, color=country, linetype='Average'), size = 1.3) + 
  scale_linetype_manual(values=c('Average'='dashed',
                                 'Median'='solid')) +
  geom_ribbon(aes(ymin=q_10, ymax=q_90, fill=country),alpha=.2, show.legend=F) + 
  scale_y_continuous(limits=c(0,250), breaks=seq(0, 250, 25), expand=c(0,0)) +
  scale_x_continuous(limits=c(1992,2019), breaks=seq(1992,2019,10), expand=c(0,0)) + 
  facet_wrap(.~country,ncol = 4) + 
  theme_light() + 
  labs(x='', y=expression(NO[3]^-{}~concentration~'(mg '*L^{-1}*')')) + 
  theme(legend.position = 'none',
        text = element_text(family='serif'),
        axis.ticks.length = unit(0.25,'cm'),
        strip.text = element_text(size=12),
        axis.title = element_text(size=13, face='bold'),
        axis.text = element_text(size=11))

p = p + 
  geom_text(data=t, 
            aes(x=1992,y=125, label = eqs),
            hjust=-.1,
            vjust=-1,
            family='serif',
            size=3.5)
p
ggsave(plot=p,filename = './Output/Plots/Country_trend.jpeg',dpi=1000)


