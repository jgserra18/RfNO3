

this_paper = terra::rast('./Output/NO3/rf/Model0019_1961_2019.tif')
names(this_paper) = paste0('X',1961:2019)
this_paper = this_paper[[which(as.numeric(gsub('X','',names(this_paper)))>1994)]]

# Serra et al 2023b - Portugal@1995-2019 ----0
pt_serra = list.files(path = 'g:/O meu disco/LULCC_PhD/Nutrients_v2/Output/Irrigation/Nitrate_modelling/GW/', pattern='.tif',full.names=T)
pt_serra = pt_serra[which(!grepl('aux',pt_serra))]
pt_serra = terra::rast(pt_serra)
names(pt_serra) = paste0('X',1995:2019)
pt_serra = terra::project(pt_serra, this_paper, 'ngb')
pt_serra = as.data.frame(pt_serra, xy=T, na.rm=T)
pt_serra = reshape2::melt(pt_serra, c('x','y'))
names(pt_serra)[4] = 'values_serra_pt'

this_paper_pt = terra::mask(terra::crop(this_paper, terra::ext(pt_serra)), pt_serra)
this_paper_pt = as.data.frame(this_paper_pt, xy=T, na.rm=T)
this_paper_pt = reshape2::melt(this_paper_pt, c('x','y'))
names(this_paper_pt)[4] = 'values_this_pt'

pt_df = plyr::join(pt_serra, this_paper_pt)
pt_df$variable = as.numeric(gsub('X','',pt_df$variable))

sapply(1995:2019, function(yr) {
  
  summary(lm(values_this_pt~values_serra_pt, data=subset(pt_df, variable==yr)))$r.squared
})

plot(c(this_paper_pt$no3_gw_2019, pt_serra$X2019))

ggplot(pt_df, aes(x=values_serra_pt, y=values_this_pt)) + 
  geom_hex() +
  geom_smooth(method='lm', color='forestgreen') + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(.~variable, ncol=4)



# Serra et al 2023a - EU@2000-2010 ----
eu_serra = list.files(path = 'g:/O meu disco/IrrigatioN_v2/Output/NO3_concentrations2/GW/',pattern='.tif',full.names = T)
eu_serra = terra::rast(eu_serra)
eu_serra = terra::project(eu_serra, this_paper, 'ngb')

this_paper = this_paper[[which(as.numeric(gsub('X','',names(this_paper)))>1999)]]
this_paper = this_paper[[which(as.numeric(gsub('X','',names(this_paper)))<2011)]]
eu_serra = terra::mask(terra::crop(eu_serra, terra::ext(this_paper)), this_paper)
this_paper = terra::resample(this_paper, eu_serra, 'ngb')

this_paper = as.data.frame(this_paper, xy=T, na.rm=T)
this_paper = reshape2::melt(this_paper, c('x','y'))
names(this_paper)[4] = 'values_this_eu'
this_paper$variable = as.numeric(gsub('X','',this_paper$variable))

eu_serra = as.data.frame(eu_serra, xy=T, na.rm=T)
eu_serra = reshape2::melt(eu_serra, c('x','y'))
eu_serra$variable = as.numeric(gsub('RF_NO3_GW_','',eu_serra$variable))

eu_df = plyr::join(this_paper, eu_serra)
eu_df = na.omit(eu_df)


r2 = sapply(2000:2010, function(yr) {
  print(yr)
  summary(lm(values_this_eu~value, data=subset(eu_df, variable==yr)))$r.squared
})
r2


eu = st_read('./Data/EU/EU.shp')
eu = st_transform(eu, crs='+proj=longlat')
cntr = c('Netherlands', 'Germany', 'Finland', 'Romania', 'Belgium', 'Denmark','Austria','Ireland')

no3 = terra::rast('./Output/NO3/rf/Model0019_1961_2019.tif')

get_spatial_units = function(no3) {
  
  r_nvz = fasterize(sf = nvz, raster = raster(no3))
  r_nvz_calc = r_nvz
  r_nvz[r_nvz==1] = 2
  r_nvz[is.na(r_nvz[])] = 1
  r_nvz[r_nvz==2] = NA
  r_nvz = mask(crop(r_nvz, extent(eu)), eu)
  
  r_out_no3 = terra::rast(r_nvz) * no3
  r_out_no3[r_out_no3<50] = 0
  r_out_no3[r_out_no3>=50] = 1
  s_r_out_no3_threshold = sum(r_out_no3)
  
  sb = subset(eu_df, !GEOUNIT %in% cntr)
  nvz_cntr = subset(eu_df, GEOUNIT %in% cntr)
  
  
  r_in_no3 =  terra::rast(r_nvz_calc) * no3
  r_in_no3[r_in_no3<50] = 0
  r_in_no3[r_in_no3>=50] = 1
  r_in_no3 = sum(r_in_no3)
  df_in_no3  = as.data.frame(r_in_no3, xy=T, na.rm=T)
  
  r_country_no3 = terra::mask(terra::crop(no3, terra::ext(nvz_cntr)), nvz_cntr)
  r_country_no3[r_country_no3<50] = 0
  r_country_no3[r_country_no3>=50] = 1
  r_country_no3 = sum(r_country_no3)
  df_country_no3= as.data.frame(r_country_no3, xy=T, na.rm=T)

  s_r_out_no3_threshold = terra::mask(terra::crop(s_r_out_no3_threshold, terra::ext(sb)), sb)
  df_out_no3_threshold = as.data.frame(s_r_out_no3_threshold, xy=T, na.rm=T)
  
  df_in_no3$cuta = cut(df_in_no3$sum, c(-Inf,0, 1,2, 5, 10,+Inf))
  df_out_no3_threshold$cutc = cut(df_out_no3_threshold$sum, c(-Inf,0, 1,2, 5, 10,+Inf))
  df_country_no3$cutb = cut(df_country_no3$sum, c(-Inf,0, 1,2, 5, 10,+Inf))
  
  return(list(
    in_nvz = df_in_no3,
    out_nvz = df_out_no3_threshold,
    country_nvz = df_country_no3
  ))  
}

hotspot_mapping = function(df,
                           f) {
  
  temp_hotspot = ggplot() + 
    geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
    scale_fill_distiller(palette='Blues') + 
    new_scale_fill() + 
    geom_sf(data=wr, fill='gray', lwd=0.1) + 
    geom_tile(data=df$out_nvz, aes(x=x,y=y, fill =cutc), show.legend = F) + 
    scale_fill_brewer(palette = 'YlGn',labels=c('0','1','1-2','2-5','5-10','>10')) +
    new_scale_fill() + 
    geom_tile(data=df$country_nvz, aes(x=x,y=y, fill =cutb), show.legend = F) + 
    scale_fill_brewer(palette = 'RdPu',labels=c('0','1','1-2','2-5','5-10','>10')) +
    new_scale_fill() + 
    facet_wrap(.~as.character(f)) + 
    geom_tile(data=df$in_nvz, aes(x=x,y=y, fill =cuta), show.legend = F) + 
    scale_fill_brewer(palette = 'OrRd',labels=c('0','1','1-2','2-5','5-10','>10')) + 
    scale_y_continuous(breaks=c(35,48,61.5,70)) + 
    scale_x_continuous(breaks=c(-10,5,20)) + 
    theme_stata(scheme = 's1mono') + 
    geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
    coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
    labs(x='',y='') + 
    theme(legend.position = 'right',
          axis.text =  element_text(size=11),
          legend.background = element_rect(color=NA),
          #  legend.box = element_blank(),
          legend.title = element_text(size=13),
          legend.text = element_text(size=12),
          text=element_text(family='serif'),
          strip.text = element_text(size=14),
          panel.background = element_rect(fill='azure1'),
          legend.margin = margin(0,0,0,0,'cm'),
          plot.margin = margin(0,0,0,0,'cm'))
  temp_hotspot
}


no3 = terra::rast('./Output/NO3/rf/Model0019_1961_2019.tif')
names(no3) = as.numeric(gsub('no3_gw_','', names(no3)))

no3_91 = no3[[which(names(no3)<1992)]]
no3_91 = get_spatial_units(no3_91)

no3_03 = no3[[which(names(no3)>1991 & names(no3)<2004)]];
no3_03 = get_spatial_units(no3_03)

no3_19 = no3[[which(names(no3)>2003)]];
no3_19 = get_spatial_units(no3_19)


p_no3_91 = hotspot_mapping(no3_91, '1961-1991')
p_no3_03 = hotspot_mapping(no3_03, '1992-2003')
p_no3_19 = hotspot_mapping(no3_19, '2004-2019')
p_no3 = ggpubr::ggarrange(p_no3_91, p_no3_03, p_no3_19, ncol=3)
ggsave(plot=p_no3,'./Output/Plots/Periods_hotspots_plot.jpeg',dpi=1000, height = 4, width = 7)


leg1 = ggplot() + 
  geom_tile(data=no3_91$out_nvz, aes(x=x,y=y, fill =cutc), show.legend = T) + 
  scale_fill_brewer(palette = 'YlGn',labels=c('0','1','1-2','2-5','5-10','>10')) + 
  labs(fill='Outside NVZ') +
  theme(text=element_text(size=13, family='serif'))
leg2 = ggplot() + 
  geom_tile(data=no3_91$in_nvz, aes(x=x,y=y, fill =cuta), show.legend = T) + 
  scale_fill_brewer(palette = 'OrRd',labels=c('0','1','1-2','2-5','5-10','>10')) + 
  labs(fill='Inside NVZ') +
  theme(text=element_text(size=13, family='serif'))
leg3 = ggplot() + 
  geom_tile(data=no3_91$country_nvz, aes(x=x,y=y, fill =cutb), show.legend = T) + 
  scale_fill_brewer(palette = 'RdPu',labels=c('0','1','1-2','2-5','5-10','>10')) + 
  labs(fill='Whole country\napproach') +
  theme(text=element_text(size=13, family='serif'))


leg1 = cowplot::get_legend(leg1)
leg2 = cowplot::get_legend(leg2)
leg3 = cowplot::get_legend(leg3)

grid::grid.newpage()
grid::grid.draw(leg1)
ggsave(plot = leg1, filename = './Output/Plots/legend1.jpeg',dpi=1000)

grid::grid.newpage()
grid::grid.draw(leg2)
ggsave(plot = leg2, filename = './Output/Plots/legend2.jpeg',dpi=1000)

grid::grid.newpage()
grid::grid.draw(leg3)
ggsave(plot = leg3, filename = './Output/Plots/legend3.jpeg',dpi=1000)




trash_store = function() {
  
  no3 = terra::rast('./Output/NO3/rf/Model0019_1961_2019.tif')
  names(no3) = as.numeric(gsub('no3_gw_','', names(no3)))
  no3 = no3[[which(names(no3)>1991)]]
  
  
  r_nvz = fasterize(sf = nvz, raster = raster(no3))
  r_nvz_calc = r_nvz
  r_nvz[r_nvz==1] = 2
  r_nvz[is.na(r_nvz[])] = 1
  r_nvz[r_nvz==2] = NA
  r_nvz = mask(crop(r_nvz, extent(eu)), eu)
  
  r_out_no3 = terra::rast(r_nvz) * no3
  r_out_no3[r_out_no3<50] = 0
  r_out_no3[r_out_no3>=50] = 1
  s_r_out_no3_threshold = sum(r_out_no3)
  
  sb = subset(eu_df, !GEOUNIT %in% cntr)
  nvz_cntr = subset(eu_df, GEOUNIT %in% cntr)
  
  
  
  r_in_no3 =  terra::rast(r_nvz_calc) * no3
  r_in_no3[r_in_no3<50] = 0
  r_in_no3[r_in_no3>=50] = 1
  r_in_no3 = sum(r_in_no3)
  df_in_no3  = as.data.frame(r_in_no3, xy=T, na.rm=T)
  df_in_no3$cuta = cut(df_in_no3$sum, c(-Inf,0, 1,5, 15,30,+Inf))
  
  
  r_country_no3 = terra::mask(terra::crop(no3, terra::ext(nvz_cntr)), nvz_cntr)
  r_country_no3[r_country_no3<50] = 0
  r_country_no3[r_country_no3>=50] = 1
  r_country_no3 = sum(r_country_no3)
  df_country_no3= as.data.frame(r_country_no3, xy=T, na.rm=T)
  df_country_no3$cutb = cut(df_country_no3$sum, c(-Inf,0, 1,5, 15,30,+Inf))
  
  
  s_r_out_no3_threshold = terra::mask(terra::crop(s_r_out_no3_threshold, terra::ext(sb)), sb)
  df_out_no3_threshold = as.data.frame(s_r_out_no3_threshold, xy=T, na.rm=T)
  df_out_no3_threshold$cutc = cut(df_out_no3_threshold$sum, c(-Inf,0, 1,5, 15,30,+Inf))
  
  s_r_out_no3_threshold[s_r_out_no3_threshold==0] = NA
  
  d = data.frame(cntry=sb$GEOUNIT, 
             count=exactextractr::exact_extract(s_r_out_no3_threshold,sb, 'count')*3.3*3.3)
  sum(d$count)
  View(d)
  require('ggnewscale')
  library(ggplot2)
  library(rworldmap)
  require('ggthemes')
  
  wr = getMap(resolution = 'high')
  wr = sf::st_as_sf(wr)
  wr = st_crop(wr, xmin=-15, xmax=35, ymin=30, ymax=71)
  
  om = terra::rast('./Data/om.tif')
  om = as.data.frame(om, xy=T, na.rm=T)

  
  temp_hotspot = ggplot() + 
    geom_tile(data=om, aes(x=x,y=y, fill=om), show.legend=F) +
    scale_fill_distiller(palette='Blues') + 
    new_scale_fill() + 
    geom_sf(data=wr, fill='gray', lwd=0.1) + 
    geom_tile(data=df_out_no3_threshold, aes(x=x,y=y, fill =cutc), show.legend = F) + 
    scale_fill_brewer(palette = 'YlGn',labels=c('0','1','<5','5-15','15-30','>30')) +
    new_scale_fill() + 
    geom_tile(data=df_country_no3, aes(x=x,y=y, fill =cutb), show.legend = F) + 
    scale_fill_brewer(palette = 'RdPu',labels=c('0','1','<5','5-15','15-30','>30')) +
    new_scale_fill() + 
    geom_tile(data=df_in_no3, aes(x=x,y=y, fill =cuta), show.legend = F) + 
    scale_fill_brewer(palette = 'OrRd',labels=c('0','1','<5','5-15','15-30','>30')) + 
    scale_y_continuous(breaks=c(35,48,61.5,70)) + 
    scale_x_continuous(breaks=c(-10,5,20)) + 
    theme_stata(scheme = 's1mono') + 
    geom_sf(data=wr, fill=NA, color='azure4', lwd=0.1) +
    coord_sf(xlim = c(-12,34.58), ylim = c(34.3, 71.3), expand=F, clip='on', crs = "+proj=longlat") +   
    labs(x='',y='') + 
    theme(legend.position = 'right',
          axis.text =  element_text(size=11),
          legend.background = element_rect(color=NA),
          #  legend.box = element_blank(),
          legend.title = element_text(size=13),
          legend.text = element_text(size=12),
          text=element_text(family='serif'),
          strip.text = element_text(size=14),
          panel.background = element_rect(fill='azure1'),
          legend.margin = margin(0,0,0,0,'cm'),
          plot.margin = margin(0,0,0,0,'cm'))
  temp_hotspot
  ggsave(plot=temp_hotspot, filename='./Output/Plots/Temporal_hotspot_regions.jpeg',dpi=1000)
  
  leg1 = ggplot() + 
    geom_tile(data=df_out_no3_threshold, aes(x=x,y=y, fill =cutc), show.legend = T) + 
    scale_fill_brewer(palette = 'YlGn',labels=c('0','1','<5','5-15','15-30','>30')) + 
    labs(fill='Outside NVZ') +
    theme(text=element_text(size=13, family='serif'))
  leg2 = ggplot() + 
    geom_tile(data=df_in_no3, aes(x=x,y=y, fill =cuta), show.legend = T) + 
    scale_fill_brewer(palette = 'OrRd',labels=c('0','1','<5','5-15','15-30','>30')) + 
    labs(fill='Inside NVZ') +
    theme(text=element_text(size=13, family='serif'))
  leg3 = ggplot() + 
    geom_tile(data=df_country_no3, aes(x=x,y=y, fill =cutb), show.legend = T) + 
    scale_fill_brewer(palette = 'RdPu',labels=c('0','1','<5','5-15','15-30','>30')) + 
    labs(fill='Whole country\napproach') +
    theme(text=element_text(size=13, family='serif'))
  
  
  leg1 = cowplot::get_legend(leg1)
  leg2 = cowplot::get_legend(leg2)
  leg3 = cowplot::get_legend(leg3)
  
  grid::grid.newpage()
  grid::grid.draw(leg1)
  ggsave(plot = leg1, filename = './Output/Plots/legend1.jpeg',dpi=1000)
  
  grid::grid.newpage()
  grid::grid.draw(leg2)
  ggsave(plot = leg2, filename = './Output/Plots/legend2.jpeg',dpi=1000)
  
  grid::grid.newpage()
  grid::grid.draw(leg3)
  ggsave(plot = leg3, filename = './Output/Plots/legend3.jpeg',dpi=1000)
}


