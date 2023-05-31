require('dplyr')
require('ggplot2')
require('udunits2')
require('sf')
require('sp')


store_eea = readRDS('./Data/explanatory vaars/Seasonal_datasets_2000_2019.rds')
exp_names = names(store_eea)[9:ncol(store_eea)]

#eea = readRDS('./Data/explanatory vaars/Seasonal_datasets_2000_2019.rds')


# number of seasonal data available 
months_df = store_eea %>%
  group_by(year) %>%
 # filter(year>1999) %>%
  count(month)
months_df$f_mon = factor(months_df$month)

ggplot(months_df, aes(x=year, y=n)) + 
  geom_line() + 
  facet_wrap(f_mon~., ncol = 4)


get_seasonal_db = function(season) {
  
  if (season=='winter')  mon = c(12,1,2)
  if (season =='spring') mon = c(3,4,5)
  if (season == 'summer') mon = c(6,7,8)
  if (season == 'autumn') mon = c(9,10,11)
  
  
  seasonal_db = store_eea[which(store_eea$month %in% mon),]
  
  set.seed(300)
  seasonal_db = partition_dataset(seasonal_db)
  seasonal_db$train_set = seasonal_db$train_set[, !c(2,4)]; seasonal_db$train_set  = na.omit(seasonal_db$train_set)
  seasonal_db$test_set = seasonal_db$test_set[, !c(2,4)]; seasonal_db$test_set  = na.omit(seasonal_db$test_set)
  return(seasonal_db)
}

hypertune_seasonal_rf = function(season,
                                 exp_names) {
  
  db = get_seasonal_db(season)
  hyper_grid_2 =expand.grid(
    mtry       =c(40, 45,50, 55),
    ntrees     = c(500, 1000, 1500, 2000),
    replace    = F,
    node_size  = c(5,10),
    # sample.fraction = c(.5, .75),                       
    OOB_RMSE  = 0
  )
  nr = 1:nrow(hyper_grid_2)
  hyper_grid_2[nr, 'OOB_RMSE'] = sapply(nr, function(i) {
    
    print(paste0('Hypertuning row no. ', i))
    model = general_rf_model(db = db, 
                             exp_names = exp_names,
                             ntrees = hyper_grid_2$ntrees[i],
                             mtry = hyper_grid_2$mtry[i],
                             node_size =  hyper_grid_2$node_size[i]
    )[['rf_model']]
    print(sqrt(model$prediction.error))
    return(sqrt(model$prediction.error))
  })
  write.csv(hyper_grid_2,paste0('./Output/NO3/rf/model/hypertuning_',season,'.csv'),row.names = F)
}


get_final_seasonal_rf_model = function(exp_names, season) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description computes best model (per OOBRMSE) and exports it as .rds file
  
  db = get_seasonal_db(season)
  hyper_grid_2 = read.csv(paste0('./Output/NO3/rf/model/hypertuning_', season,'.csv'))
  hyper_grid_2 = hyper_grid_2[which.min(hyper_grid_2$OOB_RMSE),]
  
  rf_model = ranger(y = db$train_set$resultObservedValue,
                    x = db$train_set[,exp_names, with=F], 
                    num.trees = hyper_grid_2$ntrees, 
                    mtry = hyper_grid_2$mtry, 
                    min.node.size=hyper_grid_2$node_size, 
                    replace = F, 
                    num.threads = 4, 
                    local.importance = T, 
                    importance = 'impurity')
  saveRDS(object = rf_model, paste0('./Output/NO3/rf/model/hypertuned_model_',season,'.rds'))
  rm(list='rf_model')
  gc()
}

export_seasonal_best_models = function() {
  
  sapply(c('winter','spring','autumn', 'summer'), function(x) get_final_seasonal_rf_model(exp_names, x))
}

validate_final_Seasonal_rf_model = function(
                                            exp_names,
                                            season) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description validates training and testing partition performance
  
  db = get_seasonal_db(season)
  rf_model = readRDS(paste0('./Output/NO3/rf/model/hypertuned_model_',season,'.rds'))
  
  train_metrics = general_validation_func(db, 'train_set', rf_model, exp_names)
  test_metrics = general_validation_func(db, 'test_set', rf_model, exp_names)
  
  return(list(
    train_metrics = train_metrics,
    test_metrics = test_metrics
  ))
  rm(list=c('rf_model','db'))
  gc()
}

export_seasonal_rf_1961_2019 = function(season,
                                        yrs=1961:2019) {
  #' @param rf_model rf model rds file (can also be rds file for xgboost or other model)
  #' @param yrs modelling yrs
  #' @description aggregates annual spatrasters for yrs param and exports
  
  rf_model = readRDS(paste0('./Output/NO3/rf/model/hypertuned_model_',season,'.rds'))
  
  dir.create('./Output/NO3/rf/', recursive = T)
  all = lapply(1961:2019, function(x) {
    rf = predict_model(rf_model,'rf',x)
  })
  r_pred = terra::rast(all)
  names(r_pred) = paste0('no3_gw_',1961:2019)
  terra::writeRaster(r_pred, paste0('./Output/NO3/rf/Model0019_1961_2019_',season,'.tif'),gdal=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6'))
  rm(list=c('all','r_pred'))
}

