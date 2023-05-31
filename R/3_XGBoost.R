library(xgboost)


db = preprocess_2000_2019_dataset(0.85)
exp_names = db$exp_names
db = db$db
hypertune_xgboost = function(db) {
  
  exp_names = names(db$train_set)[6:ncol(db$train_set)-1]
  
  train_x = data.matrix(db$train_set[,exp_names, with=F])
  train_y = db$train_set$resultObservedValue
  
  test_x = data.matrix(db$test_set[,exp_names, with=F])
  test_y = db$test_set$resultObservedValue
  
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  watchlist = list(train=xgb_train, test=xgb_test)
  
  # prepare XGBoost ----
  
  hyper_grid_2 =expand.grid(
    gamma       = seq(0, 5, 0.5),
    eta    = seq(0.1,0.3, 0.1),
  #  subsample    = seq(0.6,1,0.2),
    colsample_bytree  = 0.9,
    max.depth = seq(8, 16, 4),
    rmse  = 0
  )
  
  nr = 1:nrow(hyper_grid_2)
  dir.create(path = './Output/NO3/xgboost/model/', recursive = T)
  write.csv(hyper_grid_2, './Output/NO3/xgboost/model/hypertuning2.csv',row.names = F)
  # perform grid search
  sapply(nr, function(i) {
    
    print(paste0('Hypertuning row no. ', i))
    # train model
    param = list(  objective           = "reg:squarederror",
                   gamma               = hyper_grid_2$gamma[i],
                   booster             = "gbtree",
                   eval_metric         = "rmse",
                   eta                 = hyper_grid_2$eta[i],
                 #  subsample           = hyper_grid_2$subsample[i],
                   colsample_bytree    = hyper_grid_2$colsample_bytree[i],
                   tree_method = 'hist')
    
    xgb_model = xgb.train(data = xgb_train, 
                          params = param,
                          verbose = F, 
                          max.depth = hyper_grid_2$max.depth[i],
                          early_stopping_rounds = 5,
                          watchlist=watchlist, 
                          nrounds = 1000, 
                          nthread = 4)#, booster = "gblinear")
    
    hp = read.csv('./Output/NO3/xgboost/model/hypertuning2.csv')
    hp[i,'rmse'] = xgb_model$best_score
    write.csv(hp, './Output/NO3/xgboost/model/hypertuning2.csv',row.names = F)
    gc()
    rm(list='xgb_model')
  })
}

get_final_xgboos_model = function(db, exp_names) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description computes best model (per rmse) and exports it as .rds file
  
  exp_names = names(db$train_set)[6:ncol(db$train_set)]
  
  train_x = data.matrix(db$train_set[,exp_names, with=F])
  train_y = db$train_set$resultObservedValue
  test_x = data.matrix(db$test_set[,exp_names, with=F])
  test_y = db$test_set$resultObservedValue
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  watchlist = list(train=xgb_train, test=xgb_test)
  
  hyper_grid_2 = read.csv('./Output/NO3/xgboost/model/hypertuning.csv')
  hyper_grid_2 = hyper_grid_2[which.min(hyper_grid_2$rmse),]
  
  param = list(  objective           = "reg:squarederror",
                 gamma               = hyper_grid_2[,'gamma'],
                 booster             = "gbtree",
                 eval_metric         = "rmse",
                 eta                 = hyper_grid_2[,'eta'],
                 subsample           = hyper_grid_2[,'subsample'],
                 colsample_bytree    = hyper_grid_2[,'colsample_bytree'],
                 tree_method = 'hist')
  xgb_model = xgb.train(data = xgb_train, 
                        params = param,
                        verbose = T, 
                        max.depth =  hyper_grid_2[,'max.depth'],
                        early_stopping_rounds = 5,
                        watchlist=watchlist, 
                        nrounds = 1000, 
                        nthread = 4)#, booster = "gblinear")
  
  saveRDS(object = xgb_model, './Output/NO3/xgboost/model/hypertuned_model.rds')
  rm(list='xgb_model')
  gc()
}

validate_final_xgboos_model = function(db,
                                       exp_names) {
  #' @param db training dataset (or everyone?); output from preprocess_2000_2019_dataset
  #' @param exp_names exploratory variable names; output from preprocess_2000_2019_dataset
  #' @description validates training and testing partition performance
  
  xgboost_model = readRDS('./Output/NO3/xgboost/model/hypertuned_model.rds')
  
  train_metrics = general_validation_func(db, 'train_set', xgboost_model, exp_names)
  test_metrics = general_validation_func(db, 'test_set', xgboost_model, exp_names)

  return(list(
    train_metrics = train_metrics,
    test_metrics = test_metrics
  ))
  rm(list='xgboost_model')
  gc()
}


# relative importance ----
var_imp = xgb.importance(
  feature_names = exp_names,
  model = xgb_model)
xgb.plot.importance(var_imp, top_n = 10)


