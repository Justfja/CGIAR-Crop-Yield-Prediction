library(lightgbm)
library(caret)

lgb_oof = rep(0,nrow(df_train))
predte = rep(0,nrow(df_test))
cvscore = c()
int.seed = c(500)#,1235,101,54321,2021)
label2= label

s = Sys.time()
for (i in 1:length(int.seed)) {
  cat("model training",i,"\n")
  
  set.seed(int.seed[i])
  folds = createFolds(label, k = 5)
  
  param = list(objective = "regression",
    metric = "rmse",
  #boost_from_average = "false",
    #tree_learner = "voting",
    feature_fraction = 0.8,
    bagging_freq = 5,
    bagging_fraction = 0.8
    #lambda = 1,
    #alpha = 1
 # max_bin = 50
    # min_data_in_leaf = 90,
  #  min_sum_hessian_in_leaf = 10
    #min_split_gain = 0.1,
  )
  
  for (this.round in 1:length(folds)) {
    cat("model training",i," ","fold ",this.round,"\n")
    valid = c(1:length(label))[unlist(folds[this.round])]
    dev = c(1:length(label))[unlist(folds[1:length(folds)!= this.round])]
    
    dtrain = lgb.Dataset(data = as.matrix(df_train[dev,]),
      label = label2[dev], free_raw_data = F)
    dvalid = lgb.Dataset(data = as.matrix(df_train[valid,]),
      label = label2[valid],free_raw_data= F)
    
    model = lgb.train(data = dtrain,
      params = param,
      nrounds = 2500,
      valids = list(val = dvalid),
      boosting_type = "gbdt",
      learning_rate = 0.01,
      max_depth = -1,
      num_leaves = 25,
      num_threads = 8,
      eval_freq = 1000,
      seed = 1235,
      verbose = 1,
      early_stopping_rounds = 100
    )
    
    pred = predict(model,as.matrix(df_train[valid,]))
    lgb_oof[valid] = pred
    pred_test = predict(model, as.matrix(df_test[,colnames(df_train)]))
    predte = predte +pred_test
    
    cat("model cv rmse score:", model$best_score,"\n")
    cvscore = c(cvscore, model$best_score)
    cat("model cv rmse mean score:",mean(cvscore), "\n")
  }
}
Sys.time() - s








tr=cbind(df_train,label = label)

library(h2o)

h2o.init()
htrain=as.h2o(tr)
dtest = as.h2o(df_test)

aml = h2o.automl(x = seq(1,ncol(df_train)),
  y = ncol(htrain),
  training_frame = htrain,
  project_name="automl3",
  nfolds=5,
  seed=1,
  max_runtime_secs = 3600,
  stopping_metric="RMSE"
)

aml@leaderboard  %>% as.data.frame()
summary(aml@leader)
pred = as.data.frame(h2o.predict(aml@leader,newdata=dtest))
#pred2 = as.data.frame(h2o.predict(aml@leader,newdata=dtest))
write.csv(pred,file = "pred.csv", row.names=F)




