
store_seasonal_datasets = function(yrs=2000:2019) {
  
  eea = data.table::fread('./Data/EEA_gw/Nitrate_gw_dataset.csv')
  eea = subset(eea, year>1999 & year<2020)

  length(unique(eea$monitoringSiteIdentifier))
  store = lapply(yrs, function(yr) {
    print(yr)
    
    station = subset(eea, parameterWaterBodyCategory=='GW' & resultObservedValue<600 & year==yr & lat != 0 & month != 0)
    station = station[, -c(2,3)]
    
    SP =SpatialPoints(station[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
    SPdf =SpatialPointsDataFrame(SP, station)
    SPdf =st_as_sf(SPdf)
    SPdf = st_buffer(SPdf, 0.01)

    exp_vars = compile_explanatory_variables(yr)
    names(exp_vars) = gsub(' ','_', names(exp_vars))
    names(exp_vars) = gsub('-','.', names(exp_vars))
    
    df_exp_vars = extract_exp_vars(exp_vars, SPdf)
    return(df_exp_vars)
  })
  db = data.table::rbindlist(store)
  
  df = as.data.frame(db)
  
  df[which(is.na(df[,-1])),]
  
  saveRDS(object = db, './Data/explanatory vaars/Seasonal_datasets_2000_2019.rds')
}

store_annual_datasets = function(yrs=2000:2019) {
  
  
  # dir.create(path = './Data/explanatory vaars/stacks/')
  store = lapply(yrs, function(yr) {
    print(yr)
    
    station = get_eea_year(yr)
    exp_vars = compile_explanatory_variables(yr)
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
  saveRDS(object = db, './Data/explanatory vaars/All_dataset_new4.rds')
}


