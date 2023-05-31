no3 = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')

eu_df = read_sf('./Data/EU/EU.shp')
eu_df = st_transform(eu_df, crs='+proj=longlat')
nvz = read_sf('./Data/EU/NVZ.shp')

cntr = c('Netherlands', 'Germany', 'Finland', 'Romania', 'Belgium', 'Denmark','Austria','Ireland','Lithuania','Poland')

cntr_hot_store = lapply(cntr, function(x) {
  print(x)
  sb = subset(eu_df, GEOUNIT==x)
  no3_sb = terra::mask(terra::crop(no3, terra::ext(sb)), sb)
  hotsp_cntry = terra::global(no3_sb>=50, 'sum', na.rm=T)[,1]
  
  cntry_df = data.frame(yrs=1961:2019,
                        cntry=x,
                        No_hot_nvz = NA,
                        No_hot_cntry = hotsp_cntry)  
  cntry_df$No_hot_not_nvz = NA
  
  return(cntry_df)
})
cntr_hot_store = data.table::rbindlist(cntr_hot_store)
cntr_hot_store = reshape2::melt(cntr_hot_store,c('yrs','cntry'))
  
  
nvz_cntry = c('Portugal','Spain','France','Italy','Czech Republic','Sweden',
 'Greece','Bulgaria','Hungary','Croatia','Estonia',
'Latvia','Slovenia','United Kingdom', 'Slovakia')

hotspot_store = lapply(nvz_cntry, function(x) {
  print(x)

  sb = subset(eu_df, GEOUNIT==x)
  no3_sb = terra::mask(terra::crop(no3, terra::ext(sb)), sb)
  no3_sb_nvz = terra::mask(terra::crop(no3_sb, terra::ext(nvz)), nvz)

  hotsp_nvz = terra::global(no3_sb_nvz>=50, 'sum', na.rm=T)[,1]
  hotsp_cntry = terra::global(no3_sb>=50, 'sum', na.rm=T)[,1]
  
  cntry_df = data.frame(yrs=1961:2019,
                        cntry=x,
                        No_hot_nvz = hotsp_nvz,
                        No_hot_cntry = hotsp_cntry)  
  cntry_df$No_hot_not_nvz = cntry_df$No_hot_cntry-cntry_df$No_hot_nvz
  
  return(cntry_df)
})
hotspot_store = data.table::rbindlist(hotspot_store)
hotspot_store = reshape2::melt(hotspot_store,c('yrs','cntry'))

df = rbind(hotspot_store,
      cntr_hot_store)
sd(subset(df_cntry, cntry=='Italy' & variable=='No_hot_cntry')$value)
cor.test(1992:2019,subset(df_cntry, cntry=='Belgium' & variable=='No_hot_cntry')$value, method='spearman')

mean(subset(df, cntry=='Spain' & variable=='No_hot_not_nvz')$value/
  subset(df, cntry=='Spain' & variable=='No_hot_nvz')$value)
sd(subset(df, cntry=='Spain' & variable=='No_hot_not_nvz')$value/
       subset(df, cntry=='Spain' & variable=='No_hot_nvz')$value)

df$r_variable = factor(df$variable, levels=c(
                                                                   'No_hot_not_nvz',
                                                                   'No_hot_nvz',
                                                                   'No_hot_cntry'))

df_cntry = subset(df, variable=='No_hot_cntry')
df_cntry = subset(df_cntry, yrs>1991)

c = unique(df_cntry$cntry)

national_metric_hotspot = function(yr_in,yr_out) {
  
  sb_df = subset(df_cntry, yrs>=yr_in & yrs<=yr_out)
  rho = sapply(c, function(x) cor.test(yr_in:yr_out, subset(sb_df, cntry==x)$value, method='spearman')$estimate)
  p = sapply(c, function(x) cor.test(yr_in:yr_out, subset(sb_df, cntry==x)$value, method='spearman')$p.value)
  m_df = data.frame(cntry=c,
                    rho = round(as.numeric(rho), 2),
                    pval = round(as.numeric(p), 3))
  names(m_df) = c('cntry',paste0('Rho_',yr_in,'_',yr_out), paste0('Pval_',yr_in,'_',yr_out))
  m_df
}
m9203 = national_metric_hotspot(1992,2003)
m0419 = national_metric_hotspot(2004,2019)
m9219 = national_metric_hotspot(1992,2019)
m = cbind(m9203, m0419, m9219)
m$X1992_2003 = paste0(m$Rho_1992_2003, ' (', m$Pval_1992_2003,')')
m$X2004_2019 = paste0(m$Rho_2004_2019, ' (', m$Pval_2004_2019,')')
m$X1992_2019 = paste0(m$Rho_1992_2019, ' (', m$Pval_1992_2019,')')
write.csv(m, './Output/Plots/Country_hotspot_metrics.csv', row.names = F)

sb_df = subset(df_cntry, yrs>=2004 & yrs<=2019)
esp_df = data.frame(yrs=2004:2019, 
                    value = subset(sb_df, cntry=='Spain')$value)
esp_df = esp_df[-12,]


df$value = df$value*(3162*3162)*0.000001
p = ggplot(subset(df, cntry!='Lithuania' & cntry!='Ireland' & cntry != 'Finland' & yrs>=1992), aes(x=yrs,y=value, 
                          colour=r_variable,
                          order=factor(r_variable))) + 
#  geom_rect(aes(xmin = 1991, xmax=2022, ymin = 0, ymax=+Inf), color=NA, fill='azure2',alpha=0.05) + 
  geom_line(size=1.1) + 
  scale_colour_manual(#values=wesanderson::wes_palette('Darjeeling1',n=3),
                      values = c('No_hot_cntry'='red4',
                                 'No_hot_not_nvz'='green4',
                                 'No_hot_nvz'='purple4'),
                         labels=c('No_hot_cntry'='Country',
                                  'No_hot_not_nvz'='Not in NVZ',
                                  'No_hot_nvz'='In NVZ'),
                      guide = guide_legend(title.position = 'top')) +
  facet_wrap(.~cntry, scales = 'free_y', ncol = 4) + 
  scale_y_continuous(expand=c(0,0), limits=function(y) c(0,max(y)+0.1*max(y))) + 
  scale_x_continuous(limits=c(1992,2019.5), breaks=c(1992,2000,2010, 2019), expand=c(0,0)) + 
  labs(x='',y=expression('Hotspots ('*km^{2}~yr^{-1}*')'),
       colour='Spatial unit')  +
  theme_light() + 
  theme(
        axis.title = element_text(size=17, face = 'bold'),
        legend.title = element_text(size=17, face='bold', ),
        legend.text = element_text(size=15),
        axis.ticks.length = unit(0.25,'cm'),
        axis.ticks = element_line(color='black'),
        legend.position = c(0.75, 0.05),
        legend.direction = 'horizontal',
        strip.text = element_text(size=12, margin=margin(0.1,0,0.1,0,'cm')),
        text=element_text(size=14, family='serif'),
        legend.background = element_blank(),
        legend.box.background = element_blank())
p
ggsave(plot = p, filename = './Output/Plots/Country_hotspots_trend.jpeg',dpi=1000, width = 9.95, height = 7.71)


# efficiency of NVZ in pollution ----

sb_df = subset(df, yrs>1991)
agg_sb_df = aggregate(value~cntry+variable, FUN='sum',data=sb_df)

countries = unique(sb_df$cntry)

eff_df = data.frame(country = countries,
           eff = sapply(countries, function(x) {
             sb_cntry = subset(agg_sb_df, cntry==x)
             return(round(sb_cntry[1,'value']/sb_cntry[2,'value']*100, 0))
           })
)
eff_df = na.omit(eff_df)
View(eff_df)
nvz = st_read('./Data/EU/NVZ.shp')
nvz$area = st_area(nvz)*0.000001 #km2

aggregate(area~ thematicId,data=nvz, FUN='sum')

ee = st_read('C:\\Users\\João Serra\\Downloads\\ProtectedArea\\ProtectedArea.shp')
ee = subset(ee, zoneType=='nitrateVulnerableZone')
ee = st_make_valid(ee)
names(ee)
ee = ee[, c('cYear','localId','thematicId','sizeValue','sizeUom','country')]

d = ee
d$geometry = NULL
d = d[, c('country','sizeValue_1000km2')]
names(d)[1] = 'ISO_A2'


eu = st_read('./Data/EU/EU.shp')
#eu = merge(eu, d, 'ISO_A2')
eu$country_area = st_area(eu)*0.000001 #km2
eu$f_nvz_area = round(eu$sizeValue_1000km2*1000/eu$country_area*100, 1)
data.frame(eu$GEOUNIT,
           eu$country_area)

View(data.frame(country=eu$GEOUNIT, 
                nvz_area = eu$sizeValue_1000km2,
           f_nvz_area=eu$f_nvz_area))

# assess hotspot trends@spatial units ----

require('dplyr')

summarise_metrics = function(df) {
  
  #yrs1 = 1961:2019;
  #yrs2 = 1961:1991; 
  yrs3 = 1992:2003; yrs4 = 2004:2019; yrs5 = 1992:2019
  time = list(yrs3,yrs4,yrs5)
  
  store = lapply(time, function(x) {
    
    sb = subset(df, yrs %in% x)
    sb = na.omit(sb)
    sb = sb %>%
      group_by(cntry, variable) %>%
      summarise(
        rho = cor.test(value, yrs, method = 'spearman')$estimate,
        pval = cor.test(value, yrs, method = 'spearman')$p.value
      )
  })
  names(store) = time
  return(store)
}

metrics_df = summarise_metrics(df)
df
View(subset(metrics_df$`1992:2019`, cntry %in% c('Portugal','Hungary')))
write.csv(df, './Output/NO3/rf/Hotspots_temporal_country.csv',row.names = F)
View(metrics_df$`1992:2019`)

View(df %>%
  group_by(cntry) %>%
  filter(variable=='No_hot_cntry' & yrs>1991) %>%
  summarise(
    avg = round(mean(value), 0),
    sd= round(sd(value), 0)
  )
)


# NVZs plot ----

wta = subset(eu_df, GEOUNIT %in%  c('Netherlands', 'Germany', 'Finland', 'Romania', 'Belgium', 'Denmark','Austria','Ireland','Lithuania','Poland'))

ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_sf(data=wta, fill='orange1', colour=NA) + 
  geom_sf(data=nvz, fill='forestgreen',colour=NA) + 
  geom_sf(data=subset(eu_df, GEOUNIT=='Poland'), fill='orange1',colour=NA) + 
  geom_sf(data=wr, fill=NA, lwd=0.1) + 
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
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

ggsave(filename = './Output/Plots/Supplementary_plots/NVZ_plot.jpeg',dpi=1000, height = 10, width = 10)



# mean no3 hotspot ----

no3 = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')

eu_df = read_sf('./Data/EU/EU.shp')
eu_df = st_transform(eu_df, crs='+proj=longlat')

cntr = c('Netherlands', 'Germany', 'Finland', 'Romania', 'Belgium', 'Denmark','Austria','Ireland','Lithuania','Poland',
         'Portugal','Spain','France','Italy','Czech Republic','Sweden',
         'Greece','Bulgaria','Hungary','Croatia','Estonia',
         'Latvia','Slovenia','United Kingdom','Europe')

cntr_hot_store = lapply(cntr, function(x) {
  print(x)
  if (x=='Europe') {
    sb = eu_df
    
    no3_sb = no3
    no3_sb = as.data.frame(no3_sb, xy=T, na.rm=T);names(no3_sb) = c('x','y',paste0('X',1961:2019))
    th25 = sapply(paste0('X',1961:2019), function(x) as.numeric(quantile(no3_sb[, x], probs=0.25, na.rm=T))) 
    th50 = sapply(paste0('X',1961:2019), function(x) as.numeric(quantile(no3_sb[, x], probs=0.50, na.rm=T))) 
    th75 = sapply(paste0('X',1961:2019), function(x) as.numeric(quantile(no3_sb[, x], probs=0.75, na.rm=T)))
    
  } else { 
    sb = subset(eu_df, GEOUNIT==x) 
    no3_sb = terra::mask(terra::crop(no3, terra::ext(sb)), sb)
    th25 = exactextractr::exact_extract(no3_sb,sb,fun='quantile',quantiles=0.10)
    th50 = exactextractr::exact_extract(no3_sb,sb,fun='quantile',quantiles=0.50)
    th75 = exactextractr::exact_extract(no3_sb,sb,fun='quantile',quantiles=0.90)
  }
  
  cntry_df = data.frame(yrs=1961:2019,
                        cntry=x,
                        th25 = as.numeric(th25),
                        th50 = as.numeric(th50),
                        th75 = as.numeric(th75))  
  cntry_df
  
  return(cntry_df)
})
cntr_hot_store = data.table::rbindlist(cntr_hot_store)


slope = sapply(cntr, function(x) {
    summary(lm(th50~yrs, subset(cntr_hot_store, cntry==x)))$coefficients[2,1]
})
sd = sapply(cntr, function(x) {
  summary(lm(th50~yrs, subset(cntr_hot_store, cntry==x)))$coefficients[2,2]
})

dat_text = data.frame(
  label = paste0('Slope: ', round(slope,2)),
  cntry = cntr,
  x=1973,
  y=90
)

dat_text = dat_text[order(dat_text$cntry),]

p1 = ggplot(cntr_hot_store, aes(x=yrs,y=th50)) + 
  geom_hline(yintercept = 50, colour='red1', linetype='dashed') + 
  geom_ribbon(aes(xmin=yrs,xmax=yrs,ymin=th25, ymax=th75,colour=cntry, fill=cntry), alpha=.35, colour=NA) + 
  geom_line(aes(colour=cntry, fill=cntry), size=1.1) + 
  geom_smooth(method='lm', colour='black',linetype='dashed') + 
 # geom_text(data=dat_text, aes(label=label), x=1970, y=80) +
  facet_wrap(.~cntry, ncol = 3) + 
  scale_y_continuous(limits=c(0,150), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + 
  theme_light() + 
  labs(x='',y=expression(NO[3]^-{}~'concentration (mg '*L^{-1}*')')) + 
  theme(
    axis.title = element_text(size=17, face = 'bold'),
    legend.title = element_text(size=17, face='bold', ),
    legend.text = element_text(size=15),
    axis.ticks.length = unit(0.25,'cm'),
    axis.ticks = element_line(color='black'),
    legend.position = 'none',
    legend.direction = 'horizontal',
    strip.text = element_text(size=12, margin=margin(0.1,0,0.1,0,'cm')),
    text=element_text(size=14, family='serif'),
    legend.background = element_blank(),
    legend.box.background = element_blank())
p1
p1 + 
  geom_text(data=dat_text, aes(label=label, x=x, y=y), colour='black', family='serif') 
ggsave(filename = './Output/Plots/Supplementary_plots/Mean_NO3_country.jpeg',dpi=1000, height = 15, width = 9)  

# no3 hotspots spatial units ----

no3 = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')
names(no3) = paste0('X',1961:2019)
no3 = no3[[which(as.numeric(gsub('X','',names(no3)))>1991)]]
no3[no3<50] = 0
no3[no3>=50] = 1

mean(terra::global(no3, 'sum',na.rm=T)[,1]*(3162*3162)*0.000001/1000)
sd(terra::global(no3, 'sum',na.rm=T)[,1]*(3162*3162)*0.000001/1000)

cor.test(1992:2019, terra::global(no3, 'sum',na.rm=T)[,1], method='spearman')
no3_hotspots = sum(no3)
c_no3_hot = no3_hotspots; c_no3_hot[c_no3_hot>1] = 1
terra::global(c_no3_hot, 'sum',na.rm=T)[,1] * (3162*3162)*0.000001/1000 # total uniuquye hotspots

nvz_no3_hotspots = terra::mask(terra::crop(no3_hotspots, terra::ext(nvz)), nvz)
wta = subset(eu_df, GEOUNIT %in%  c('Netherlands', 'Germany', 'Finland', 'Romania', 'Belgium', 'Denmark','Austria','Ireland','Lithuania','Poland'))
wta_no3_hotspots = terra::mask(terra::crop(no3_hotspots, terra::ext(wta)), wta)

c_wta_no3_hotspots = wta_no3_hotspots; c_wta_no3_hotspots[c_wta_no3_hotspots>1] = 1
c_nvz_no3_hotspots = nvz_no3_hotspots; c_nvz_no3_hotspots[c_nvz_no3_hotspots>1] = 1

out = terra::mask(c_no3_hot, wta, inverse=T)
out = terra::mask(out, nvz, inverse=T)

out_total = terra::global(out, 'sum',na.rm=T)[,1] * (3162*3162)*0.000001/1000
out_wta = terra::global(c_wta_no3_hotspots, 'sum',na.rm=T)[,1] * (3162*3162)*0.000001/1000
out_nvz = terra::global(c_nvz_no3_hotspots, 'sum',na.rm=T)[,1] * (3162*3162)*0.000001/1000
out_total/sum(c(out_total,out_wta,out_nvz))

ggplot(data = data.frame(hsize=3, value=c(out_total,out_wta,out_nvz), group=c('Outside','WTA','NVZ')),
       aes(x=hsize, y=value, fill=group)) + 
  scale_fill_manual(values=c('Outside'='green4',
                             'WTA'='red4',
                             'NVZ'='purple4')) + 
  geom_col() + 
  coord_polar(theta = "y") +
  xlim(c(0.2, 4 + 0.5)) + 
  theme_void() + 
  theme(legend.position = 'none')
ggsave(filename = './Output/Plots/Total_hotspots_circle.jpeg',dpi=1000)
out_total/sum(c(out_total,out_wta,out_nvz))
out_total/sum(c(out_total,out_wta,out_nvz))
out_total

c_out = out; c_out[c_out>0] = 1
hotspots_out = terra::global(c_out, 'sum',na.rm=T)[,1] * (3162*3162)*0.000001/1000
cntry_c_out = round(exactextractr::exact_extract(c_out, eu_df, 'sum')* (3162*3162)*0.000001,0)
f_cntry_c_out = cntry_c_out/(hotspots_out*1000) * 100
c_out_df = data.frame(cntry=eu_df$GEOUNIT,
                      hotspots_out_km2 = cntry_c_out,
                      f_hotspots_out = f_cntry_c_out)
View(c_out_df)


cntry_c_nvz = round(exactextractr::exact_extract(c_nvz_no3_hotspots, eu_df, 'sum')* (3162*3162)*0.000001,0)
cntry_c_wta = round(exactextractr::exact_extract(c_wta_no3_hotspots, eu_df, 'sum')* (3162*3162)*0.000001,0)

country_df = data.frame(cntry=eu_df$GEOUNIT,
                        out = cntry_c_out,
                        nvz = cntry_c_nvz,
                        wta = cntry_c_wta)
country_df = reshape2::melt(country_df, 'cntry')
country_df = country_df[which(country_df$cntry %in% c('Spain','Italy','Greece','Bulgaria','France','Belgium','Hungary','Poland',
                        'Croatia','Portugal','Netherlands','Czech Republic','Romania','Austria','Slovakia',
                        'United Kingdom','Germany','Denmark')),]
country_df$nrow = 1:nrow(country_df)
country_df = country_df[-c(33,35),] # remove poland + romania NVZ

require('ggthemes')
ggplot(country_df, aes(x = forcats::fct_reorder(cntry, value), y = value/1000, fill = variable)) + 
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values=c('out'='green4',
                             'wta'='red4',
                             'nvz'='purple4'))+ 
  coord_flip() + 
  scale_y_continuous(expand=c(0,0),limits=c(0,163.5),
                     breaks=c(0,5,10,25,50,75,100,160)) + #, breaks=c(0,10,50,100,500,1000,4000)) + 
  labs(x='', y=expression(Unique~hotspots~'(1000 '*km^{2}*')')) + 
  theme_pander() + 
  theme(text=element_text(size=14, family='serif'),
      #  axis.title = element_text(size=15, face = 'bold'),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'),
        axis.text.y = element_text(size=20),
        axis.text.x = element_text(size=15),
        axis.title.x = element_text(size=25, face='bold'),
        axis.ticks = element_line(color='black'),
        legend.position = 'none',
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')),
        legend.background = element_blank(),
        legend.box.background = element_blank())
ggsave(filename = './Output/Plots/Unique_hotspots_cntry.jpeg',dpi=1000)

wta_no3_hotspots = as.data.frame(wta_no3_hotspots, xy=T, na.rm=T)
nvz_no3_hotspots = as.data.frame(nvz_no3_hotspots, xy=T, na.rm=T)
out = as.data.frame(no3_hotspots, xy=T, na.rm=T)


require('ggnewscale')

p = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
 geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
 scale_fill_distiller(palette='Blues') + 
 new_scale_fill() + 
  geom_tile(data=out, aes(x=x,y=y, fill=cut(sum, c(0,1,5,10,15,+Inf)))) + 
  scale_fill_brewer(palette='Greens') + 
  new_scale_fill() + 
  geom_tile(data=nvz_no3_hotspots, aes(x=x,y=y, fill=cut(sum, c(0,1,5,10,15,+Inf)))) + 
  scale_fill_brewer(palette='Purples') +
  new_scale_fill() + 
  geom_tile(data=wta_no3_hotspots, aes(x=x,y=y, fill=cut(sum, c(0,1,5,10,15,+Inf)))) + 
  scale_fill_brewer(palette='Reds') + 
  theme_minimal() +
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_stata(scheme = 's1mono') + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',fill='') + 
  theme(legend.position = 'none',
        axis.text =  element_text(size=11),
        legend.background = element_rect(color=NA),
        #  legend.box = element_blank(),
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        text=element_text(family='serif'),
        strip.text = element_text(size=14),
        panel.background = element_rect(fill='gray'),
        legend.margin = margin(0,0,0,0,'cm'),
        plot.margin = margin(0,0,0,0,'cm'))
p
ggsave(plot = p, filename = './Output/Plots/Temporal_hotspot_regions_jpeg.jpg',dpi=1000)


leg1 =  ggplot() + 
  geom_tile(data=na.omit(out), aes(x=x,y=y, fill=cut(sum, c(0,1,5,10,15,+Inf))), na.rm = T) + 
  scale_fill_brewer(palette='Reds', labels=c('1','1-5','5-10','10-15','>15'))  +
  labs(fill='Whole Territory\nApproach') + 
theme(
      axis.text =  element_text(size=11),
      legend.background = element_rect(color=NA),
      #  legend.box = element_blank(),
      legend.title = element_text(size=13),
      legend.text = element_text(size=12),
      text=element_text(family='serif'),
      strip.text = element_text(size=14),
      panel.background = element_rect(fill='gray'),
      legend.margin = margin(0,0,0,0,'cm'),
      plot.margin = margin(0,0,0,0,'cm'))

leg2 =  ggplot() + 
  geom_tile(data=na.omit(wta_no3_hotspots), aes(x=x,y=y, fill=cut(sum, c(0,1,5,10,15,+Inf))), na.rm = T) + 
  scale_fill_brewer(palette='Greens', labels=c('1','1-5','5-10','10-15','>15'))  +
  labs(fill='Outside NVZs') + 
  theme(
    axis.text =  element_text(size=11),
    legend.background = element_rect(color=NA),
    #  legend.box = element_blank(),
    legend.title = element_text(size=13),
    legend.text = element_text(size=12),
    text=element_text(family='serif'),
    strip.text = element_text(size=14),
    panel.background = element_rect(fill='gray'),
    legend.margin = margin(0,0,0,0,'cm'),
    plot.margin = margin(0,0,0,0,'cm'))
leg3 =  ggplot() + 
  geom_tile(data=na.omit(nvz_no3_hotspots), aes(x=x,y=y, fill=cut(sum, c(0,1,5,10,15,+Inf))), na.rm = T) + 
  scale_fill_brewer(palette='Purples', labels=c('1','1-5','5-10','10-15','>15'))  +
  labs(fill='Inside NVZs') + 
  theme(
    axis.text =  element_text(size=11),
    legend.background = element_rect(color=NA),
    #  legend.box = element_blank(),
    legend.title = element_text(size=13),
    legend.text = element_text(size=12),
    text=element_text(family='serif'),
    strip.text = element_text(size=14),
    panel.background = element_rect(fill='gray'),
    legend.margin = margin(0,0,0,0,'cm'),
    plot.margin = margin(0,0,0,0,'cm'))
leg3

library(grid)
library(gridExtra) 

leg1 <- cowplot::get_legend(leg1)
grid.newpage()
grid.draw(leg1)
ggsave(plot = leg1, filename = './Output/Plots/Leg1.jpeg',dpi=1000)

leg2 <- cowplot::get_legend(leg2)
grid.newpage()
grid.draw(leg2)
ggsave(plot = leg2, filename = './Output/Plots/Leg2.jpeg',dpi=1000)

leg3 <- cowplot::get_legend(leg3)
grid.newpage()
grid.draw(leg3)
ggsave(plot = leg3, filename = './Output/Plots/Leg3.jpeg',dpi=1000)
