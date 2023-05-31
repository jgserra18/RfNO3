

# export [NO3] in Europe at 0.04x0.04º for 1961-2019 ----
## includes annual and seasonal concentrations ----

fnames = list.files(path = './Output/NO3/rf/', pattern = 'new')
fnames=fnames[which(grepl('.tif',fnames))]
fnames = gsub('.tif','', fnames)

files = list.files(path = './Output/NO3/rf/', pattern = 'new', full.names = T)
files=files[which(grepl('.tif',files))]

lapply(1:length(files), function(x) {
  
  
  no3_yr = terra::rast(files[x])
  names(no3_yr) = paste0('X',1961:2019)
  no3_yr = as.data.frame(no3_yr, xy=T, na.rm=T)
  
  no3_yr[1:nrow(no3_yr), 'Pearson'] = sapply(1:nrow(no3_yr), function(x) {
    print(x)
    as.numeric(
      cor.test(x=1961:2019, y=as.numeric(no3_yr[x, paste0('X',1961:2019)]), method='spearman')$estimate
    )
  })
  
  no3_yr[1:nrow(no3_yr), 'Pvalue'] = sapply(1:nrow(no3_yr), function(x) {
    print(x)
    as.numeric(
      cor.test(x=1961:2019, y=as.numeric(no3_yr[x, paste0('X',1961:2019)]), method='spearman')$p.value
    )
  })
  
  data.table::fwrite(no3_yr, paste0('./Output/NO3/rf/',fnames[x],'.csv'), row.names = F)
})

# decadal annual no3 ----

no3 = data.table::fread('./Output/NO3/rf/Model0019_new4_1961_2019.csv')

compute_spatial_metrics = function(df,
                                   yrs = 1991:2023) {
  
  df[1:nrow(df), 'Pearson'] = sapply(1:nrow(df), function(x) {
    print(x)
    as.numeric(
      cor.test(x=yrs, y=as.numeric(df[x, paste0('X',yrs),with=F]), method='pearson')$estimate
    )
  })
  
  df[1:nrow(df), 'Pvalue'] = sapply(1:nrow(df), function(x) {
    print(x)
    as.numeric(
      cor.test(x=yrs, y=as.numeric(df[x, paste0('X',yrs), with=F]), method='pearson')$p.value
    )
  })
  return(df)
}



#ridge plot annual ----

require('ggridges')

ridge = reshape::melt(no3[, -c('Pearson','Pvalue')], c('x','y'))
ridge$variable = as.numeric(gsub('X','',ridge$variable))

p = ggplot(data=subset(ridge, variable>=1991), aes(x=value, y=factor(variable), fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  # geom_density_ridges2(panel_scaling = 2, color=4, lwd=0.5, linetype=1, fill='white') + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.75, quantiles=c(0.05, 0.25,0.5,0.75, 0.95)) +
  scale_fill_gradient(low = "white", high = "red4",
                      name = "Tail prob.") + 
  scale_x_continuous(trans='pseudo_log',breaks=c(0, 5, 10, 25,50,100, 150), limits=c(0,200), expand=c(0,0),
                     , sec.axis = sec_axis(trans=~.*1,name=NULL, 
                                           breaks = c(0,5,10,25,50,100, 150))) + 
  scale_y_discrete(expand=c(0,0), breaks=seq(1991,2019,5)) + 
  labs(x=expression(Predictions~'(mg'~NO[3]^-{}~L^{-1}*')'),
       y='') +
  guides(fill=guide_colourbar(barheight = 7)) + 
 # facet_wrap(.~'Training') + 
  theme_light() + 
 # theme_ridges(grid = T, ) + 
  coord_flip() + 
  theme(#legend.position = c(0.85, 0.65),
    text=element_text(size=14, family='serif'),
        axis.title = element_text(size=15, face='bold'),
        legend.title = element_text(size=13),
    legend.position = 'right',
        legend.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'),
    panel.grid.major = element_line(color = 'gray'),
        axis.ticks = element_line(color='black'),
        strip.text = element_text(size=16, margin=margin(0.2,0,0.2,0,'cm')))
p

## annual hotspots@EU ----
require("ggforce")

no_hotspot = as.numeric(sapply(paste0('X',1992:2019), function(x) length(which(no3[,x, with=F]>50))))
desc = 'Number of hotspots likely overestimated'

hotspot_df = data.frame(yrs=1992:2019, 
                        ar = no_hotspot*(3162*3162)*0.000001/1000) # km2
View(hotspot_df)
summary(lm(ar~yrs, hotspot_df))
sd(hotspot_df$ar)
cor.test(subset(hotspot_df, yrs>2003 & yrs!=2015)$yrs, subset(hotspot_df, yrs>2003 & yrs!=2015)$ar, method='spearman')
cor.test(1992:2019, hotspot_df$ar, method='spearman')
lm(ar~yrs, new)
new = subset(hotspot_df, yrs != 2015)
cor.test(new$yrs, new$ar, method='spearman')

cor.test(subset(new, yrs>2003)$yrs, subset(new, yrs>2003)$ar, method='spearman')

lm(ar~yrs, subset(new, yrs>2003))

summary(lm(ar~yrs, subset(new, yrs>2003)))
summary(lm(ar~yrs, new))
summary(lm(ar~yrs, new))$coefficients
-0.39*100

p_hotspot = ggplot(data=subset(hotspot_df, yrs>1991),
       aes(x=yrs,y=ar)) + 
  #"geom_smooth(method='lm', aes(colour='azure4'), alpha=.2, fill='azure4',size=1) +
  geom_smooth(data=subset(hotspot_df, yrs>1991), aes(linetype='solid',colour='blue1'), method='lm',  fill='blue1', alpha=.2, se=F) + 
  geom_smooth(data=subset(hotspot_df, yrs>2003),aes(linetype='solid',colour='forestgreen'), method='lm', fill='forestgreen', alpha=.2,se=F) + 
  geom_smooth(data=subset(hotspot_df, yrs<=2004),aes(linetype='solid',colour='yellow1'), method='lm', fill='yellow1', alpha=.2,se=F) + 
  geom_smooth(data=subset(new, yrs>1991), method='lm', aes(linetype='dashed',colour='blue1'), fill='blue1', alpha=.2, se=F) + 
  geom_smooth(data=subset(new, yrs>2003), method='lm',aes(linetype='dashed', colour='forestgreen'), fill='forestgreen', alpha=.2,  se=F)  + 
  scale_linetype_manual(values = c('solid'='solid',
                                   'dashed'='dashed'),
                        labels = c('solid'="Yes",
                                   'dashed'="No")) +
  scale_colour_manual(values=c('blue1'='blue1',
                                      'forestgreen'='forestgreen',
                                      'yellow1'='yellow1'),
                                      labels=c(#'azure4'='1992-2019',
                                                       'blue1'='1992-2019',
                                                       'yellow1'='1992-2003',
                                                       'forestgreen'='2004-2019')) + 
  geom_line(colour='red4', size=1.1) + 
  geom_mark_ellipse(aes(filter = yrs == 2015, label = '2015', 
                        description = desc), label.width = 10, 
                    label.fontsize = 12,
                    label.family = 'serif') + 
  scale_y_continuous(limits=c(0,285), breaks=seq(0, 300, 50), expand=c(0,0)) +
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
p_hotspot

pp = ggpubr::ggarrange(p, p_hotspot, ncol=1, labels=c('a','b'), font.label = list(family='serif'), 
                       widths = c(1,1))
ggsave(plot=pp, filename = './Output/Plots/EU_assessment_logaxis_reversed.jpeg',dpi=1000, height = 7, width = 9)
#ggsave(plot=p, filename = './Output/Plots/Ridge_prediction_dist.jpeg',dpi=1000, height = 8, width = 5.3)

# 9 plots (60-91,92-2003,2004-2019) ----
# pearson map, 2 col (p value in small and distribution)


#metrics_60_91 = compute_spatial_metrics(no3, 1961:1991)
metrics_92_2003 = compute_spatial_metrics(no3, 1992:2003)
metrics_04_19 = compute_spatial_metrics(no3, 2004:2019)
metrics_92_19 = compute_spatial_metrics(no3, 1992:2019)

decadal_metrics = data.frame(x=metrics_92_2003$x,
                             y = metrics_92_2003$y,
                            # pearson_60_91 = metrics_60_91$Pearson,
                            # pvalue_60_91 = metrics_60_91$Pvalue,
                             pearson_92_03 = metrics_92_2003$Pearson,
                             pvalue_92_03 = metrics_92_2003$Pvalue,
                             pearson_04_19 = metrics_04_19$Pearson,
                             pvalue_04_19 = metrics_04_19$Pvalue,
                             pearson_92_19 = metrics_92_19$Pearson,
                             pvalue_92_19 = metrics_92_19$Pvalue)
data.table::fwrite(decadal_metrics,'./Output/NO3/rf/Decadal_metrics.csv', row.names = F)
decadal_metrics = data.table::fread('./Output/NO3/rf/Decadal_metrics.csv')
#decadal_metrics = decadal_metrics[, -c('pearson_60_91','pvalue_60_91')]

# metrics outside NVZ+WTA
decadal_metrics_out = terra::rast(decadal_metrics)
decadal_metrics_out = terra::mask(decadal_metrics_out, nvz, inverse=T)
wta = subset(eu_df, GEOUNIT %in% cntr)
decadal_metrics_out = terra::mask(decadal_metrics_out, wta, inverse=T)
decadal_metrics_out = as.data.frame(decadal_metrics_out, xy=T, na.rm=T)
decadal_metrics_out = reshape2::melt(decadal_metrics_out, c('x','y'))

sb_decadal_out  = subset(decadal_metrics_out, grepl('pearson', variable))
sb_decadal_out$f = factor(sb_decadal_out$variable, labels = c('1992-2003','2004-2019', '1992-2019'))

# metrics NVZ
decadal_metrics_nvz = terra::rast(decadal_metrics)
decadal_metrics_wta = terra::mask(decadal_metrics_nvz, wta, inverse=F)
decadal_metrics_nvz = terra::mask(decadal_metrics_nvz, nvz, inverse=F)

decadal_metrics_nvz = as.data.frame(decadal_metrics_nvz, xy=T, na.rm=T)
decadal_metrics_nvz = reshape2::melt(decadal_metrics_nvz, c('x','y'))
decadal_metrics_wta = as.data.frame(decadal_metrics_wta, xy=T, na.rm=T)
decadal_metrics_wta = reshape2::melt(decadal_metrics_wta, c('x','y'))

sb_decadal_nvz  = subset(decadal_metrics_nvz, grepl('pearson', variable))
sb_decadal_nvz$f = factor(sb_decadal_nvz$variable, labels = c('1992-2003','2004-2019', '1992-2019'))
sb_decadal_wta = subset(decadal_metrics_wta, grepl('pearson', variable))
sb_decadal_wta$f = factor(sb_decadal_wta$variable, labels = c('1992-2003','2004-2019', '1992-2019'))

sb_decadal_out$out = 'out'
sb_decadal_nvz$out = 'nvz'
sb_decadal_wta$out = 'wta'

sb_decadal = rbind(sb_decadal_out, sb_decadal_nvz, sb_decadal_wta)

library(ggplot2)
library(rworldmap)

wr = getMap(resolution = 'high')
wr = sf::st_as_sf(wr)
wr = st_crop(wr, xmin=-15, xmax=35, ymin=30, ymax=71)

sb_decadal = subset(sb_decadal, grepl('pearson', sb_decadal$variable))
sb_decadal$f = factor(sb_decadal$variable, labels = c('1992-2003','2004-2019', '1992-2019'))

om = terra::rast('./Data/om.tif')
om = as.data.frame(om, xy=T, na.rm=T)

require('ggnewscale')
require('ggthemes')
require('ggridges')

sb_decadal %>%
  group_by(variable) %>%
  summarize(avg=length(which(value<0))/length(value)*100)

p1new = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
  scale_fill_distiller(palette='Blues') + 
  new_scale_fill() + 
  geom_tile(data=subset(sb_decadal, out=='out'), aes(x=x,y=y, fill=value)) +
  scale_fill_distiller(palette='PRGn', name=expression('Rho'~NVZ[Outside])) +
  new_scale_fill() +
  geom_tile(data=subset(sb_decadal, out!='out'), aes(x=x,y=y, fill=value)) +
  scale_fill_distiller(palette='RdGy', name=expression('Rho'~NVZ[Inside])) +
 # guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~f, ncol = 3) + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_stata(scheme = 's1mono') + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',fill='Rho') + 
  theme(legend.position = 'right',
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

p1new = ggpubr::ggarrange(p1new, labels='a', font.label = list(size=14, family='serif'))
ggsave(plot=p1new,'./Output/Plots/UPDATED_TRENDS.jpeg',dpi=1000, height = 8, width = 10)

p2.1 = ggplot(data=subset(sb_decadal, grepl('pearson', variable)), 
                 aes(x=value, y=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')), 
                     fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.05, 0.25,0.5,0.75, 0.95)) +
  scale_fill_distiller(palette='PRGn',
                      name = "Tail prob.", guide='colorbar') + 
  guides(fill=guide_colorbar(barwidth = 10)) + 
  scale_x_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(x='',
       y='') +
  # facet_wrap(.~'Training') + 
  theme_light() + 
  #theme_ridges(grid = T, center_axis_labels = T)  + 
  theme(text=element_text(size=13, family='serif'),
        legend.position = 'bottom',
        #  legend.direction = 'horizontal',.
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))

p2.2 = ggplot(data=subset(sb_decadal, grepl('pearson', variable)), aes(y=value, 
                                                             x=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')))) + 
  geom_boxplot(fill='gray',color='purple4',alpha=0.8,) +
  coord_flip() + 
  stat_summary(fun.y = 'mean') + 
  scale_y_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_x_discrete(labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(y='Rho - All',
       x='') +
  theme_light()  + 
  theme(text=element_text(size=13, family='serif'),
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))

p3.1 = ggplot(data=subset(sb_decadal, grepl('pearson', variable) & out!='out'), 
              aes(x=value, y=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')), 
                  fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.05, 0.25,0.5,0.75, 0.95)) +
  scale_fill_distiller(palette='RdGy',
                       name = "Tail prob.", guide='colorbar') + 
  guides(fill=guide_colorbar(barwidth = 10)) + 
  scale_x_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(x='',
       y='') +
  # facet_wrap(.~'Training') + 
  theme_light() + 
  #theme_ridges(grid = T, center_axis_labels = T)  + 
  theme(text=element_text(size=13, family='serif'),
        legend.position = 'bottom',
        #  legend.direction = 'horizontal',.
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))
p3.2 = ggplot(data=subset(sb_decadal, grepl('pearson', variable)), aes(y=value, 
                                                                                    x=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')))) + 
  geom_boxplot(fill='gray',color='red4',alpha=0.8,) +
  coord_flip() + 
  stat_summary(fun.y = 'mean') + 
  scale_y_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_x_discrete(labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(y='Rho - NVZ',
       x='') +
  theme_light()  + 
  theme(text=element_text(size=13, family='serif'),
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))
p3.2

p2 = ggpubr::ggarrange(p2.1,p2.2, ncol=1, heights = c(2.5,1), common.legend = F, legend = 'bottom')
p3 = ggpubr::ggarrange(p3.1,p3.2, ncol=1, heights = c(2.5,1), common.legend = F, legend = 'bottom')
p4 = ggpubr::ggarrange(p3,p2, ncol=2, labels=c('b','c'), font.label = list(size=14, family='serif'))#, heights = c(2,1))
p1234 = ggpubr::ggarrange(p1new,p4, ncol=1)#, heights = c(2,1))
p4
ggsave(plot=p4,'./Output/Plots/Trends_period_test.jpeg',dpi=1000)






# old plots ----
p1 = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
  scale_fill_distiller(palette='Blues') + 
  new_scale_fill() + 
  geom_tile(data=sb_decadal, aes(x=x,y=y, fill=value)) +
  scale_fill_viridis_c() +
  guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~f, ncol = 3) + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_stata(scheme = 's1mono') + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',fill='Rho') + 
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
p1
p2.1 = ggplot(data=subset(decadal_metrics, grepl('pearson', variable)), 
              aes(x=value, y=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')), 
                  fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.05, 0.25,0.5,0.75, 0.95)) +
  scale_fill_gradient(low = "white", high = "purple4",
                      name = "Tail prob.", guide='colorbar') + 
  guides(fill=guide_colorbar(barwidth = 10)) + 
  scale_x_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(x='',
       y='') +
  # facet_wrap(.~'Training') + 
  theme_light() + 
  #theme_ridges(grid = T, center_axis_labels = T)  + 
  theme(text=element_text(size=13, family='serif'),
       legend.position = 'bottom',
      #  legend.direction = 'horizontal',.
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
      plot.margin = margin(0,0.5,0,0,'cm'))
p2.1

p2.2 = ggplot(data=subset(decadal_metrics, grepl('pearson', variable)), aes(y=value, 
                                                                            x=factor(as.character(variable), 
                                                                                     level=c('pearson_92_03','pearson_04_19','pearson_92_19')))) + 
  geom_boxplot(fill='purple4',color='purple4',alpha=0.8,) +
  coord_flip() + 
  stat_summary(fun.y = 'mean') + 
  scale_y_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_x_discrete(labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(y='Rho - All',
       x='') +
  theme_light()  + 
  theme(text=element_text(size=13, family='serif'),
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))
p2.2
p2 = ggpubr::ggarrange(p2.1,p2.2, ncol=1, heights = c(1.5,1), common.legend = T, legend = 'bottom') 
p12 = ggpubr::ggarrange(p1, p2, ncol=1, labels = c('a','b'), font.label = list(size=14, family='serif'))#, heights = c(2,1))

ggsave(plot=p12,'./Output/Plots/Trends_period_plot.jpeg',dpi=1000, height = 7.3, width = 7)


## nvz plot ----

get_nvz_spatial_metrics = function(df) {
  

  period2 = compute_spatial_metrics(df = data.table::as.data.table(df), yrs = 1992:2003)[, c('Pearson','Pvalue')]
  period3 = compute_spatial_metrics(df = data.table::as.data.table(df), yrs = 2004:2019)[, c('Pearson','Pvalue')]
  period1 = compute_spatial_metrics(df = data.table::as.data.table(df), yrs = 1992:2019)[, c('Pearson','Pvalue')]
  
  df$pearson_92_03 = period2$Pearson
  df$pvalue_92_03 = period2$Pvalue
  df$pearson_04_19 = period3$Pearson
  df$pvalue_04_19 = period3$Pvalue
  df$pearson92_19 = period1$Pearson
  df$pvalue_92_19 = period1$Pvalue
  return(df)
}

nvz = st_read('./Data/EU/NVZ.shp')
nvz = st_simplify(nvz, 1000)
#nvz =  st_union(nvz)

no3 = terra::rast('./Output/NO3/rf/Model0019_new4_1961_2019.tif')
no3 = terra::mask(terra::crop(no3, terra::ext(nvz)), nvz)
names(no3) = paste0('X',1961:2019)
no3 = no3[[which(as.numeric(gsub('X','',names(no3)))>1991)]]

no3_nvz = as.data.frame(no3, xy=T, na.rm=T)
names(no3_nvz) = c('x','y',paste0('X',1992:2019))

no3_nvz = get_nvz_spatial_metrics(no3_nvz)
no3_nvz = reshape2::melt(no3_nvz, c('x','y'))
no3_nvz = subset(no3_nvz, grepl('pearson', variable))
no3_nvz$f = factor(no3_nvz$variable, labels = c('1992-2003','2004-2019','1992-2019'))

p3 = ggplot() + 
  geom_sf(data=wr, fill='gray', lwd=0.1) + 
  geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
  scale_fill_distiller(palette='Blues') + 
  new_scale_fill() + 
  geom_tile(data=no3_nvz, aes(x=x,y=y, fill=value)) +
  scale_fill_viridis_c() +
  guides(fill=guide_colorbar(barwidth = 0.7, barheight = 5)) + 
  theme_minimal() +
  facet_wrap(.~f, ncol = 3) + 
  scale_y_continuous(breaks=c(35,48,61.5,70)) + 
  scale_x_continuous(breaks=c(-10,5,20)) + 
  theme_stata(scheme = 's1mono') + 
  geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
  coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
  labs(x='',y='',fill='Rho') + 
  theme(legend.position = 'right',
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
p3

p4.1 = ggplot(data=subset(no3_nvz, grepl('pearson', variable)), 
              aes(x=value, y=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')), 
                  fill = 0.5 - abs(0.5 - stat(ecdf)))) + 
  # geom_density_ridges2(panel_scaling = 2, color=4, lwd=0.5, linetype=1, fill='white') + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantile_lines = T, alpha=0.6, quantiles=c(0.05, 0.25,0.5,0.75, 0.95)) +
  scale_fill_gradient(low = "white", high = "purple4",
                      name = "Tail prob.", guide='colorbar') + 
  guides(fill=guide_colorbar(barwidth = 10)) + 
  scale_x_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_y_discrete(expand=c(0,0), labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(x='',
       y='') +
  # facet_wrap(.~'Training') + 
  theme_light() + 
  #theme_ridges(grid = T, center_axis_labels = T)  + 
  theme(text=element_text(size=13, family='serif'),
        legend.position = 'bottom',
        #  legend.direction = 'horizontal',.
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))
p4.1
p4.2 = ggplot(data=subset(no3_nvz, grepl('pearson', variable)), aes(y=value, 
                                                                    x=factor(as.character(variable), level=c('pearson_92_03','pearson_04_19','pearson_92_19')))) + 
  geom_boxplot(fill='purple4',color='purple4',alpha=0.8,) +
  coord_flip() + 
  stat_summary(fun.y = 'mean') + 
  scale_y_continuous(trans='pseudo_log', breaks=seq(-1,1,0.25), limits=c(-1,1), expand=c(0,0)) + 
  scale_x_discrete(labels=c('1992-2003','2004-2019','1992-2019')) + 
  labs(y='Rho - NVZ',
       x='') +
  theme_light()  + 
  theme(text=element_text(size=13, family='serif'),
        axis.title = element_text(size=14, face='bold'),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(0.25,'cm'), 
        legend.title = element_text(size=13),
        legend.text = element_text(size=12),
        plot.margin = margin(0,0.5,0,0,'cm'))
p4.2
p4 = ggpubr::ggarrange(p4.1,p4.2, ncol=1, heights = c(1.5,1), common.legend = T, legend = 'bottom')
p34 = ggpubr::ggarrange(p3,p4, ncol=1, labels=c('c','d'), font.label = list(size=14, family='serif'))#, heights = c(2,1))

p1234 = ggpubr::ggarrange(p12,p34, ncol=1)#, heights = c(2,1))
ggsave(plot=p1234,'./Output/Plots/Trends_period_plot111.jpeg',dpi=1000, height = 14, width = 7)


