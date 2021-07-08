
library(xgboost)


dtrain = xgb.DMatrix(as.matrix(da), label=label)
dtest = xgb.DMatrix(as.matrix(de))

param = list(booster = "gbtree",
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  colsample_bytree = 0.8,
  max_depth = 4,
  min_child_weight = 1,
  nthread = 8,
  # base_score = mean(ll),
  gamma = 0,
  subsample = 0.8
)

watchlist= list(train = dtrain)

set.seed(1235)
fit_cv = xgb.cv(params = param,
  data = dtrain,
  watchlist = watchlist,
  nrounds = 5000,
  nfold = 5,
  print_every_n = 500,
  early_stopping_rounds = 100,
  prediction = FALSE,
  maximize = FALSE)


set.seed(1235)
mod.xgb = xgb.train(data = dtrain,params = param,nrounds = fit_cv$best_ntreelimit)
imp = as.data.frame(xgb.importance(feature_names = colnames(df_train),model = mod.xgb))


preds= predict(mod.xgb,dtest)
#pred = round(pred^2)



sub = data.frame(test.id,preds)
colnames(sub) = colnames(samp)
write.csv(sub,file = paste0(subm.dir,"/subm.csv"),row.names = F)



#########






create_nn <- function(ncol_X, ncol_Y) {
  keras_model_sequential() %>% 
    layer_batch_normalization(input_shape = ncol_X) %>% 
    #layer_dropout(0.1) %>% 
    layer_dense(512, "relu") %>% 
    layer_batch_normalization() %>% 
    layer_dropout(0.2) %>% 
    layer_dense(256, "relu") %>% 
    layer_batch_normalization() %>% 
    layer_dense(128, "relu") %>% 
    layer_batch_normalization() %>% 
    layer_dense(ncol_Y) %>% 
    keras::compile(loss = "mse", optimizer = "sgd") 
}


int.seed = c(500)


X= df_train %>% as.matrix()
X_te  = df_test[,colnames(df_train)] %>% as.matrix()

X[is.na(X)] = -1
X_te[is.na(X_te)] = -1

ss = rep(0,nrow(X_te))
Y = label


scores <- c()
for (i in 1:length(int.seed)) {
  cat("model training",i,"\n")
  
  set.seed(int.seed[i])
  folds = createFolds(label, k = 5)
  
  for (this.round in 1:length(folds)) {
    cat("model training",i," ",this.round,"\n")
    valid = c(1:length(label))[unlist(folds[this.round])]
    dev = c(1:length(label))[unlist(folds[1:length(folds)!= this.round])]
    
    
    early_stopping <- callback_early_stopping(patience = 5, min_delta = 1e-05)
    check_point <- callback_model_checkpoint("model.h5", save_best_only = TRUE, verbose = 0, mode = "auto")
    
    
    m_nn <- create_nn(ncol(X), 1)
    hist <- m_nn %>% keras::fit(X[dev,], Y[dev],
      epochs = 100,
      batch_size =32,
      validation_data = list(X[valid, ], Y[valid]),
      callbacks = list(early_stopping, check_point),
      view_metrics = FALSE,
      verbose = 0)
    load_model_weights_hdf5(m_nn, "model.h5")
    scores <- c(scores, min(hist$metrics$val_loss))
    cat("Best val-loss:", min(hist$metrics$val_loss), "at", which.min(hist$metrics$val_loss), "step\n")
    
    ss = ss + predict(m_nn, X_te) 
    
  #  rm(tri, m_nn, hist)
    file.remove("model.h5")
  }
}


cat("\nMean score:", mean(scores), "\n")



### CONVOLUTION 1D

##### MODELLING
model =   keras_model_sequential() %>%
  layer_batch_normalization(input_shape = list(NULL, dim(X)[[-1]])) %>% 
    layer_conv_1d(filters = 128 ,kernel_size = 3,padding = 'same',activation = "relu") %>%
    #layer_batch_normalization() %>%
    layer_max_pooling_1d() %>%
    layer_conv_1d(filters = 64,kernel_size = 3,padding = "same",activation = "relu") %>%
    #layer_batch_normalization() %>%
    layer_max_pooling_1d() %>%
  layer_flatten() %>%
  layer_dense(units = 1) %>%
  keras::compile(loss = "mse", optimizer = "sgd")

m_nn= keras_model_sequential() %>%
  layer_conv_1d(filters=32, kernel_size=5, 
    kernel_regularizer = regularizer_l1(0.001), activation="relu",
    input_shape = list(NULL, dim(X)[[-1]])) %>%
  layer_max_pooling_1d(pool_size=3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5,
    kernel_regularizer = regularizer_l1(0.001),
    activation = "relu") %>%
  layer_gru(units = 32, kernel_regularizer = regularizer_l1(0.001),
    dropout = 0.1, recurrent_dropout = 0.5) %>%
  layer_dense(units = 1) %>%
  keras::compile(loss = "mse", optimizer = "sgd")




library(Cubist)

cub.param <- list(committees = 40, neighbors = 0, rules = 100, sample = 0)

a = cubist.model(df_train,label = label, intest = df_test)


a = cubist.model(da,label = label, intest = de)
a = cubist.model(da2,label = label, intest = de2)



#################
j = unique(df_train$Year)

for(i in 1:length(j)){
  year = j[i]
  
  tr = cbind(df_train,label)
  tr = tr %>% filter(Year == year)
  label2= tr$label
  tr$label = NULL
  dtrain = xgb.DMatrix(as.matrix(tr), label=label2)
  dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))
  
  params = list(booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.1,
    colsample_bytree = 0.8,
    max_depth = 5,
    min_child_weight = 1,
    nthread = 8,
    # base_score = mean(ll),
    gamma = 0,
    subsample = 0.8
  )
  watchlist= list(train = dtrain)
  
  set.seed(1235)
  fit_cv = xgb.cv(params = params,
    data = dtrain,
    watchlist = watchlist,
    nrounds = 3000,
    nfold = 5,
    print_every_n = 500,
    early_stopping_rounds = 100,
    # prediction = TRUE,
    maximize = F)
  
  score = fit_cv$evaluation_log$test_rmse_mean[fit_cv$best_iteration]
  cv = c(cv,score)
  set.seed(1235)
  mod.xgb = xgb.train(data = dtrain,params = params,nrounds = fit_cv$best_iteration)
  
  pred= predict(mod.xgb,dtest)
  predte = predte + pred
}


