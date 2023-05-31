source('./src/2_hypertune_annual_parameters.R')

library(ggplot2)

preprocess_2000_2019_dataset = function(split_ratio=0.8) {
  #' @description reads and partitions all the dataset into 80%training, 20% testing
  
  # read raw dataset for 2000-2019 ----
  #db = readRDS('./Data/explanatory vaars/All_dataset.rds')
  
  #' @note new3 is the dataset with the current version (13 April) where pet=soil
  #' @note new4 is the new version where ept and soil and properly distinguished
  db = readRDS('./Data/explanatory vaars/All_dataset_new4.rds')
  
  exp_names = names(db)[6:ncol(db)-1]
  #exp_names = names(db)[6:ncol(db)]
  
  # partition it ----
  set.seed(300)
  db = partition_dataset(db, split_ratio)
  
  return(list(
    exp_names = exp_names,
    db = db
  ))
}


general_rf_model = function(db, exp_names, ntrees, mtry, node_size) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @param ntrees
  #' @param mtry
  #' @param node_size
  #' @description applies a ranger model to the defined args
  #' @returns 1 - rf model; 2 - mean error; 3 - r2
  
  rf_model = ranger(y = db$train_set$resultObservedValue,
                    x = db$train_set[,exp_names, with=F], 
                    num.trees = ntrees, 
                    mtry = mtry, 
                    min.node.size=node_size, 
                    replace = F, 
                    num.threads = 4, 
                    local.importance = T, 
                    importance = 'impurity')
  mae = rf_model$prediction.error
  r2 = rf_model$r.squared
  
  return(list(
    rf_model = rf_model,
    mae = mae,
    r2 = r2
  ))
}

hypertune_generalized_rf = function() {
  #' @description hypertunes random forest with very conservative hyperparameters
  
  db = preprocess_2000_2019_dataset(split_ratio = 0.85)
  exp_names = db[['exp_names']]
  db = db[['db']]
  db$train_set[which(is.na(db$train_set$monitoringSiteIdentifier)),'monitoringSiteIdentifier'] = 'unknown'
  db$test_set[which(is.na(db$test_set$monitoringSiteIdentifier)),'monitoringSiteIdentifier'] = 'unknown'
  
  hyper_grid_2 =expand.grid(
    mtry       = c(45,55, 65),
    ntrees     = c(500,750,1000,1500,2000),
    replace    = F,
    node_size  = 5,
    # sample.fraction = c(.5, .75),                       
    OOB_RMSE  = 0
  )
  hyper_grid_2 =expand.grid(
    mtry       = 65,
    ntrees     = 1500,
    replace    = F,
    node_size  = c(1,2,5,8,10),
    # sample.fraction = c(.5, .75),                       
    OOB_RMSE  = 0
  )
  nr = 1:nrow(hyper_grid_2)
  View(hyper_grid_2)
  hyper_grid_2[nr, 'OOB_RMSE'] = sapply(nr, function(i) {
    
    print(paste0('Hypertuning row no. ', i))
    model = general_rf_model(db = db, 
                             exp_names = exp_names,
                             ntrees = hyper_grid_2$ntrees[i],
                             mtry = hyper_grid_2$mtry[i],
                             node_size =  hyper_grid_2$node_size[i]
    )[['rf_model']]
    print(paste0('RMSE = ', sqrt(model$prediction.error)))
    return(sqrt(model$prediction.error))
  })
  write.csv(hyper_grid_2,'./Output/NO3/rf/model/hypertuning_new4.csv',row.names = F)
}

get_final_rf_model = function(db, exp_names) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description computes best model (per OOBRMSE) and exports it as .rds file
  
  hyper_grid_2 = read.csv('./Output/NO3/rf/model/hypertuning_new4.csv')
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
  saveRDS(object = rf_model, './Output/NO3/rf/model/hypertuned_model_new4.rds')
  rm(list='rf_model')
  gc()
}


general_validation_func = function(db,
                                   partition=c('train_set','test_set'),
                                   rf_model,
                                   exp_names) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param partition either train_set or test_set
  #' @param rf_model random forest model; from .rds file
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description computes all and annual metrics (r2, rmse)
  
  if (class(rf_model)=='xgb.Booster') {
    db[[partition]]$prediction = predict(rf_model, as.matrix(db[[partition]][,exp_names, with=F]), reshape = TRUE, outputmargin =0)
  }
  else {
    db[[partition]]$prediction =  predict(rf_model, db[[partition]][,exp_names, with=F])$predictions
  }
  
  # all yr metrics
  r2 =  summary(lm(resultObservedValue~prediction, db[[partition]]))$r.squared
  rmse = Metrics::rmse(db[[partition]]$resultObservedValue, db[[partition]]$prediction)
  bias = mean(db[[partition]]$prediction)/mean(db[[partition]]$resultObservedValue)
  r = (sd(db[[partition]]$prediction)/mean(db[[partition]]$prediction))/(sd(db[[partition]]$resultObservedValue)/mean(db[[partition]]$resultObservedValue))
  kgem = 1-sqrt((r2-1)^2+(bias-1)^2+(r-1)^2)
  
  # annual metrics 
  annual_metrics = data.frame(yrs=2000:2019,
                          r2 = sapply(2000:2019, function(x) { 
                            round(summary(lm(resultObservedValue~prediction, data=subset(db[[partition]], year==x)))$r.squared, 3) 
                          }),
                          rmse = sapply(2000:2019, function(x) { 
                            round(Metrics::rmse(subset(db[[partition]], year==x)$resultObservedValue,subset(db[[partition]], year==x)$prediction), 3) 
                          }),
                          bias = sapply(2000:2019, function(x) { 
                            round(mean(subset(db[[partition]], year==x)$prediction)/mean(subset(db[[partition]], year==x)$resultObservedValue), 3)
                          }),
                          r = sapply(2000:2019, function(x) { 
                            round((sd(subset(db[[partition]], year==x)$prediction)/mean(subset(db[[partition]], year==x)$prediction))/(sd(subset(db[[partition]], year==x)$resultObservedValue)/mean(subset(db[[partition]], year==x)$resultObservedValue)), 3)
                          })
  )
  annual_metrics$kgem = round(1-sqrt((annual_metrics$r2-1)^2+(annual_metrics$bias-1)^2+(annual_metrics$r-1)^2), 3)
  
  validation_plot = ggplot(db[[partition]], aes(x=prediction, y=resultObservedValue)) + 
    geom_point(alpha=.6) + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_smooth(method='lm') + 
    facet_wrap(.~year)
  
  return(list(
    all_r2 = r2,
    all_rmse = rmse,
    all_bias = bias,
    all_r = r,
    all_kgem = kgem,
    annual_metrics = annual_metrics,
    annual_plot = validation_plot
  )) 
}

validate_final_rf_model = function(db,
                                   exp_names,
                                   rf_model) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description validates training and testing partition performance
  
  #rf_model = readRDS('./Output/NO3/rf/model/hypertuned_model_new4.rds')

  train_metrics = general_validation_func(db, 'train_set', rf_model, exp_names)
  test_metrics = general_validation_func(db, 'test_set', rf_model, exp_names)

  return(list(
    train_metrics = train_metrics,
    test_metrics = test_metrics
  ))
#  rm(list='rf_model')
  gc()
}

predict_model = function(model_apply,
                         model_name = c('rf','xgb'),
                         yr) {
  #' @param model_apply either rf_model or xgb_model (may be in .rds format)
  #' @param model_name
  #' @param yr
  #' @description spatially explicit predicts for a given model
  #' @returns spatraster of the no3 concentration for a given year
  #' @unit mg L-1
  
  print('Preprocessing stack ------------')
  
  # exp_vars = terra::rast(paste0('./Data/explanatory vaars/stacks/Stack_',yr,'.tif'))
  exp_vars = compile_explanatory_variables(yr)
  
  names(exp_vars) = gsub(' ','_', names(exp_vars))
  names(exp_vars) = gsub('-','.', names(exp_vars))
  exp_names = names(exp_vars)
  exp_vars = terra::as.data.frame(exp_vars, xy=T, na.rm=T)
  
  print('Starting model prediction ----------')
  if (model_name=='rf') {
    
    pred_general_out = predict(model_apply, exp_vars)
    df_pred = data.frame(x=exp_vars$x, y=exp_vars$y, pred=pred_general_out$predictions)
  }
  else {
    df_pred = data.frame(x=exp_vars$x, 
                         y=exp_vars$y, 
                         pred=predict(model_apply, as.matrix(exp_vars[,3:ncol(exp_vars)]), reshape = TRUE, outputmargin =0))
  }
  print('Finishing -----------')
  r_pred = terra::rast(df_pred, crs='+proj=longlat')
  return(r_pred)
}

export_rf_1961_2019 = function(rf_model,
                               yrs=1961:2019) {
  #' @param rf_model rf model rds file (can also be rds file for xgboost or other model)
  #' @param yrs modelling yrs
  #' @description aggregates annual spatrasters for yrs param and exports
  
  dir.create('./Output/NO3/rf/', recursive = T)
  all = lapply(1961:2019, function(x) {
    rf = predict_model(rf_model,'rf',x)
  })
  r_pred = terra::rast(all)
  names(r_pred) = paste0('no3_gw_',1961:2019)
  terra::writeRaster(r_pred, './Output/NO3/rf/Model0019_new4_1961_2019.tif',gdal=c('COMPRESS=DEFLATE', 'PREDICTOR=2', 'ZLEVEL=6'))
  rm(list=c('all','r_pred'))
}

validate_annual_maps = function(yrs=1990:2019) {
  #' @description validates annual no3 maps using eea database: training (80%) testing (20%) for each year
  
  rf_no3 = terra::rast('./Output/NO3/rf/Model0019_new_1961_2019.tif')
  
  # annual metrics: generate train metrics, testing metrics
  
  annual_metrics = lapply(yrs, function(yr) {
    
    eea = get_eea_year(yr); eea$geometry = NULL; eea_part = partition_dataset(eea); eea_all = eea
    
    no3_yr = rf_no3[[which(grepl(yr, names(rf_no3)))]]
    train_metric = compare_prediction_metrics(yr, no3_yr, eea_part$train_set)
    test_metric = compare_prediction_metrics(yr, no3_yr, eea_part$test_set)
    all_metric = compare_prediction_metrics(yr, no3_yr, eea_all)
    
    return(list(
      eea = eea,
      train_metric = train_metric,
      test_metric = test_metric,
      all_metric = all_metric,
      no3_yr = no3_yr
    ))
  })
  
  # preprocess annual metrics into annual values (r2, rmse) for training and testing partitions
  names(annual_metrics) = paste0('X',yrs)
  training_r2 = sapply(yrs, function(x) { annual_metrics[[paste0('X',x)]]$train_metric$r2 } )
  training_rmse = sapply(yrs, function(x) { annual_metrics[[paste0('X',x)]]$train_metric$rmse} )
  testing_r2 = sapply(yrs, function(x) { annual_metrics[[paste0('X',x)]]$test_metric$r2} )
  testing_rmse = sapply(yrs, function(x) { annual_metrics[[paste0('X',x)]]$test_metric$rmse} )
  all_r2 = sapply(yrs, function(x) { annual_metrics[[paste0('X',x)]]$all_metric$r2} )
  all_rmse = sapply(yrs, function(x) { annual_metrics[[paste0('X',x)]]$all_metric$rmse} )
  
  df_metrics = data.frame(yrs=yrs,
                          train_r2 = training_r2,
                          test_r2 = testing_r2,
                          all_r2 = all_r2,
                          train_rmse = training_rmse,
                          test_rmse = testing_rmse,
                          all_rmse = all_rmse)
  df_metrics_plot = reshape::melt(df_metrics, 'yrs')
  metrics_plot = ggplot(df_metrics_plot, aes(x=yrs, y=value)) + 
    geom_point() + 
    geom_smooth(method='lm') + 
    facet_wrap(.~variable, ncol = 2, scales = 'free_y')
  
  # prepare scatterplots for training sets 
  annual_train_df = lapply(paste0('X',yrs), function(x) {
    
    df = annual_metrics[[x]]$train_metric$sptest; df$geometry = NULL
    df$yr = as.numeric(gsub('X','', x))
    return(df)
  })  
  annual_train_df = data.table::rbindlist(annual_train_df)
  
  annual_train_plot = ggplot(annual_train_df, aes(x=pred_no3, y=resultObservedValue)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) +
    geom_smooth(method='lm') +
    facet_wrap(yr~.)
  
  # prepare scatterplots for testing sets 
  annual_test_df = lapply(paste0('X',yrs), function(x) {
    
    df = annual_metrics[[x]]$test_metric$sptest; df$geometry = NULL
    df$yr = as.numeric(gsub('X','', x))
    return(df)
  })  
  annual_test_df = data.table::rbindlist(annual_test_df)
  
  annual_test_plot = ggplot(annual_test_df, aes(x=pred_no3, y=resultObservedValue)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) +
    geom_smooth(method='lm') +
    facet_wrap(yr~.)
  
  # prepare scatterplots for all sets 
  annual_all_df = lapply(paste0('X',yrs), function(x) {
    
    df = annual_metrics[[x]]$all_metric$sptest; df$geometry = NULL
    df$yr = as.numeric(gsub('X','', x))
    return(df)
  })  
  annual_all_df = data.table::rbindlist(annual_all_df)
  
  annual_all_plot = ggplot(annual_all_df, aes(x=pred_no3, y=resultObservedValue)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0) +
    geom_smooth(method='lm') +
    facet_wrap(yr~.)
  
  return(list(
    df_metrics = df_metrics, # annual r2 and rmse
    metrics_plot = metrics_plot,
    annual_train_df = annual_train_df,
    annual_train_plot = annual_train_plot,
    annual_test_df = annual_test_df,
    annual_test_plot = annual_test_plot,
    annual_all_df = annual_all_df,
    annual_all_plot = annual_all_plot
  ))
}





