
library(catboost)

 # set.seed(500)
 # folds = createFolds(as.factor(label2),k = 5)

#p = matrix(0,ncol = 7,nrow = 19766)
dtest = catboost.load_pool(as.matrix(df_test[,colnames(df_train)]))

### create a matrix for predictions
p <- rep(0, nrow(df_test))

for (i in 1) {
  
  print(paste0("model training on label ", i))
  
  #label = cv_label[,i]

for(rnd in 1:length(folds)){
  valid = c(1:length(label2))[unlist(folds[rnd])]
  dev = c(1:length(label2))[unlist(folds[1:length(folds) != rnd])]

dtrain = catboost.load_pool(as.matrix(df_train[dev,]), label=label2[dev])
dvalid = catboost.load_pool(as.matrix(df_train[valid,]),label=label2[valid])
  
  params = list(
    iterations = 10000,
    learning_rate = 0.1,
    depth = 4,
    eval_metric = "RMSE",
    loss_function = "RMSE",
    random_seed = 1235,
    use_best_model = TRUE,
    logging_level = "Verbose",
    rsm = 0.8,
    od_type = "Iter",
    od_wait = 100,
    metric_period = 1000
  )
  
  
  
 # set.seed(1235)
  model = catboost.train(learn_pool = dtrain,test_pool = dvalid,
                         params = params)
  
  
  
  pred = catboost.predict(model, pool = dtest)
  
  
  p = p + pred
}   
}


# fit_cat = catboost.cv(dtrain, params = params, fold_count = 5,
#                       partition_random_seed = 1235,
#                       early_stopping_rounds = 100)

library(CatEncoders)
label_enc = LabelEncoder.fit(label)
label3 = transform(label_enc,label) - 1


#set.seed(1235)
model = catboost.train(learn_pool = dtrain,test_pool = NULL,
  params = params)