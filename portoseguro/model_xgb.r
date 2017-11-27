library(xgboost)
library(MLmetrics)
ds = read.csv("interm_ds.csv")

xgb_normalizedgini <- function(preds, dtrain){
    actual <- getinfo(dtrain, "label")
    score <- NormalizedGini(preds,actual)
    return(list(metric = "NormalizedGini", value = score))
}

perc=0.6

inTrain = createDataPartition(ds$target,p=perc,list=TRUE)[[1]]
training = ds[inTrain,]
testing = ds[-inTrain,]

params = list(eta=0.3,max_depth=5,colsample_bytree=0.7,subsample=.7,gamma=5,objective='reg:logistic',booster='gbtree',eval_metric=xgb_normalizedgini,seed=4321,nthread=-1)

    dtrain = training
    dtest = xgb.DMatrix(as.matrix(testing))
    y=dtrain$target
    dtrain$target = NULL
    dtrain = xgb.DMatrix(as.matrix(dtrain),label=y)
    set.seed(1234)
    xgb_cv = xgb.cv(params,dtrain,early_stopping_rounds=5,nfold=5,nrounds=50,maximize=T)
    xgfit = xgb.train(params,data=dtrain,nrounds=xgb_cv$best_iteration,verbose=F)
    predxgb = predict(xgfit,newdata=dtest)
    p0 = NormalizedGini(pred,xgb,testing$target)
    print(p0)
