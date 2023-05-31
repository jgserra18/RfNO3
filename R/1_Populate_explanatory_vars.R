setwd('g:/O meu disco/no3_long_term/')

#TODO: 

get_groundwater_params = function() {
  
  fn = list.files(path = './Data/explanatory vaars/groundwater/'); fn = fn[which(grepl(pattern = 'GW_depth_mon', x = fn)==F)]; fn = gsub('.tif','', fn)
  
  files = list.files(path = './Data/explanatory vaars/groundwater/', full.names=T); files = files[which(grepl(pattern = 'GW_depth_mon', x = files)==F)]
  files = terra::rast(files); names(files) = fn  
  files = terra::project(files, '+proj=longlat')
  return(files)
}

get_wtd = function() {
  
  files = list.files(path = './Data/explanatory vaars/groundwater/', full.names=T, pattern='GW_depth_mon')
  files = terra::rast(files)
  names(files) = paste0('GW_depth_',1:12)
  return(files)
}

get_soil_params = function() {
  
  fn = list.files(path = './Data/explanatory vaars/soil/');  fn = gsub('.tif','', fn)
  files = list.files(path = './Data/explanatory vaars/soil/', full.names=T)
  files = terra::rast(files); names(files) = fn  
  
  return(files)
}

get_management_params = function(yr=c(1961,2019)) {
  
  yrs = 1961:2019; yr_id = which(yrs==yr)
  fn = list.files(path = './Data/explanatory vaars/spatiotemporal//', pattern='.tif');  fn = gsub('.tif','', fn); fn = gsub('_1961_2019','', fn)
  
  files = list.files(path = './Data/explanatory vaars/spatiotemporal//', full.names=T, pattern='.tif')
  
  files = lapply(files, function(x) terra::rast(x, lyrs=yr_id))
  files = terra::rast(files)
  names(files) = fn
  
  return(files)
}

get_lu_params = function(yr=c(1961,2019)) {
  
  if (yr<2016) {
    yrs = 1961:2015; yr_id = which(yrs==yr)
    
    fn = list.files(path = './Data/explanatory vaars/spatiotemporal/lu_historic/', pattern='.tif');  fn = gsub('.tif','', fn)
    files = list.files(path = './Data/explanatory vaars/spatiotemporal//lu_historic/', full.names=T, pattern='.tif')
  }
  else {
    yrs = 2016:2025; yr_id = which(yrs==yr)
    fn = list.files(path = './Data/explanatory vaars/spatiotemporal/lu_ssp//', pattern='.tif');  fn = gsub('.tif','', fn)
    files = list.files(path = './Data/explanatory vaars/spatiotemporal//lu_ssp/', full.names=T, pattern='.tif')
  }
  files = lapply(files, function(x) terra::rast(x, lyrs=yr_id))
  files = terra::rast(files)
  names(files) = fn
  
  return(files)
}

get_climatic_params = function(yr=c(1961,2019)) {
   
  ids = seq.Date(from = as.Date(paste0('1961',"/1/1")), 
                 to =  as.Date(paste0('2019',"/12/31")),
                 by = "month")
  ids = which(as.numeric(substr(ids, 1,4))==yr)
  
  fn = list.files(path = './Data/explanatory vaars/spatiotemporal/climate/', pattern='.tif');  
  #fn = fn[-which(grepl('aux',fn)==T)];
  fn = gsub('.tif','', fn);
  fn = gsub('_1961_2019','', fn)
  
  files = list.files(path = './Data/explanatory vaars/spatiotemporal//climate//', full.names=T, pattern='.tif')
#  files = files[-which(grepl('aux',files)==T)]
  
  files = lapply(1:length(files), function(x) {
    rfile = terra::rast(files[x], lyrs=ids)
    names(rfile) = paste0(fn[x], '_',1:12)
    return(rfile)
  })
  files = terra::rast(files)
  
  return(files)
}

compile_explanatory_variables = function(yr=c(1961,2019)) {
  
  print('Gathering explanatory variables .......')
  clim = get_climatic_params(yr)
  lu = get_lu_params(yr)
  n_manag = get_management_params(yr)
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

