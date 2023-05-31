library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(vegan)
require(caTools)
require('ranger')
require('ggplot2')

source('./src/1_Populate_explanatory_vars.R')

aggregate_old_dataset_2000_2010 = function(yr) {
  #' @description aggregates annual dataset from N irrigation paper (2000-2010) to the updated Waterbase

  if (yr>=2000 & yr<2011) {
    
    old = read.csv('./Data/EEA_gw/master_file_GW_old.csv')
    old = subset(old, Mean<600 & Year==yr & lat != 0)
    old = old[, c('Station_ID','lon','lat','Year','Mean')]
    old = na.omit(old)
    names(old) = c(' monitoringSiteIdentifier','lon','lat','year','resultObservedValue')
    old$src = 'old'
    
    eea = read.csv('./Data/EEA_gw/Nitrate_gw_dataset.csv')
    eea = subset(eea, parameterWaterBodyCategory=='GW' & resultObservedValue<600 & year==yr & lat != 0)
    eea = eea[, c('monitoringSiteIdentifier','lon','lat','year','resultObservedValue')]
    eea$src = 'new'
    
    db = dplyr::bind_rows(eea, old) #nrow 11780
    return(db)
  }
}

get_eea_year = function(yr,
                        bf_distance=0.01) {

  if (yr>=2000 & yr<2011) {
    eea = aggregate_old_dataset_2000_2010(yr)
    eea$year = yr
    eea = eea[, c('monitoringSiteIdentifier','lon','lat','year','resultObservedValue')]
  }
  else {
    eea = data.table::fread('./Data/EEA_gw/Nitrate_gw_dataset.csv')
    eea = subset(eea, parameterWaterBodyCategory=='GW' & resultObservedValue<600 & year==yr & lat != 0)
    eea = eea[, c('monitoringSiteIdentifier','lon','lat','year','resultObservedValue')]
  }
 # eea = aggregate(resultObservedValue ~ lon + lat + monitoringSiteIdentifier + year, eea, FUN = mean)
  eea = eea %>%
    group_by(monitoringSiteIdentifier, lon,lat, year) %>%
    summarize(resultObservedValue = mean(resultObservedValue))
  SP = SpatialPoints(eea[, c('lon','lat')], proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs'))
  SPdf = SpatialPointsDataFrame(SP, eea)
  SPdf = st_as_sf(SPdf)
  SPdf = st_buffer(SPdf, bf_distance)
  
  return(SPdf)
}


extract_exp_vars = function(rstack_exp_vars,
                            st_SPdf) {
  
  #TODO: check if na.omit(all) influenced previous population functions ---
  names(rstack_exp_vars) = gsub(' ','_', names(rstack_exp_vars))
  
  # if need be delete max_cells_in_memory
  df = exactextractr::exact_extract(rstack_exp_vars, st_SPdf, fun='mean')#,max_cells_in_memory=1e10)
  names(df) = gsub('mean.','',names(df))
  
  st_SPdf = cbind(st_SPdf, df)
  all = st_SPdf
  all$geometry = NULL
  
  if (length(which(grepl('parameterSampleDepth',names(st_SPdf))))==1) {
    print('Not deleting NAs for EEA dataset.')
    return(all)
  }
  else {
    return(all)
  #  return(na.omit(all))
  }
}


partition_dataset = function(df_exp_vars,
                             split_ratio=0.8) {
  
  set.seed(1000)
  train =sample.split(df_exp_vars$lon, SplitRatio = split_ratio)
  
  if (class(df_exp_vars)[1]=='data.frame') {
    train_set =df_exp_vars[train, ]
    test_set =df_exp_vars[-train, ]
  }
  else {
    train_set =df_exp_vars[c(train), ]
    test_set =df_exp_vars[!train, ]
  }

  return(list(
    train_set = train_set,
    test_set = test_set
  ))
}

hypertune_parameters_func = function(formula, training_data) {
  #' @source https://bradleyboehmke.github.io/HOML/random-forest.html#ref-probst2019hyperparameters
  #' @param formula formula
  #' @param training_data training data (eg partitin[[2]])
  #' @description hypertunes using a grid search the optimal parameters as defined by the RMSE
  #' @usage  hypertune_parameters(formula = fm, data = partition[[1]])
  
  hyper_grid_2 =expand.grid(
    mtry       = seq(35,55,5),
    ntrees     = seq(500,2000,300),
    replace    = F,
    node_size  = seq(1,10),
    # sample.fraction = c(.5, .75),                       
    OOB_RMSE  = 0
  )
  nr = 1:nrow(hyper_grid_2)
  # perform grid search
  hyper_grid_2[nr, 'OOB_RMSE'] = sapply(nr, function(i) {
    
    print(paste0('Hypertuning row no. ', i))
    # train model
    model =ranger(
      formula         = formula, 
      data            = training_data, 
      num.trees       = hyper_grid_2$ntrees[i],
      mtry            = hyper_grid_2$mtry[i],
      min.node.size   = hyper_grid_2$node_size[i],
      #   sample.fraction = hyper_grid_2$sample.fraction[i],
      replace         = hyper_grid_2$replace[i],
      seed            = 123,
      num.threads = 4
    )
    return(sqrt(model$prediction.error))
  })

  hyper_grid_2 %>% 
    dplyr::arrange(OOB_RMSE) %>%
    head(10)
  
  return(hyper_grid_2)
}


hypertune_all_parameters = function(yrs=2013:2019) {
  
  lapply(yrs, function(yr) {
    print(paste0(yr,'=================================================================================================='))
    
    station = get_eea_year(yr)
    exp_vars = compile_explanatory_variables(yr)
    df_exp_vars = extract_exp_vars(exp_vars, station)
    df_part_exp_vars = partition_dataset(df_exp_vars)
    
    dir.create(path = './Output/NO3_concentrations/', recursive = T)
    write.csv(df_part_exp_vars[['test_set']], paste0('./Output/NO3_concentrations/test_set_', yr, '.csv'), row.names = F)

    fm =as.formula(paste('resultObservedValue~', paste(names(df_exp_vars), collapse = '+')))
    print('Starting hypertune ....')
    tune = hypertune_parameters_func(formula = fm, training_data = df_part_exp_vars[['train_set']])
    write.csv(tune, paste0('./Output//NO3_concentrations/hyperparams_',yr,'.csv'), row.names = F)
  })
}
#try without removing outliers


compare_prediction_metrics = function(yr,
                                      r_pred,
                                      test_set = NULL) {
  
  if (missing(test_set)==T) { test_set = data.table::fread(paste0('./Output/NO3_concentrations/test_set_',yr,'.csv')) }

  SP =SpatialPoints(test_set[, c('lon','lat')], proj4string = CRS('+proj=longlat'))
  sptest =SpatialPointsDataFrame(SP, test_set)
  sptest =st_as_sf(sptest)
  sptest = st_buffer(sptest, 0.005)
  sptest$pred_no3 =  exactextractr::exact_extract(r_pred, sptest, fun='mean')
  sptest = sptest[, c('monitoringSiteIdentifier','lon','lat','resultObservedValue','pred_no3')]
  sptest = na.omit(sptest)
  r2 =  summary(lm(resultObservedValue~pred_no3, sptest))$r.squared
  rmse = Metrics::rmse(sptest$resultObservedValue, sptest$pred_no3)

  return(list(
    sptest = sptest,
    r2 = r2,
    rmse = rmse
  ))
}

