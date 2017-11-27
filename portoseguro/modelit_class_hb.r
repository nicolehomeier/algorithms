ds = read.csv("train.csv",na.strings=c("-1","-1.0"))

pattern="bin|cat"
for (i in 1:ncol(ds)){
    if (grepl(pattern,names(ds)[i])){
        ds[,i] = factor(ds[,i])
    }
}

facmode = function(x){
    tab = table(x)
    names(tab)[tab==max(tab)]
}

#feature engineering

ds$nact = apply(ds,1,function(x) sum(is.na(x)))

#replace NAs
for (i in 2:ncol(ds)){
    if (class(ds[,i]) == "factor" & sum(is.na(ds[,i])) > 0){
        ds[which(is.na(ds[,i])),i] = facmode(ds[,i])
    } else if (sum(is.na(ds[,i])) > 0){
        ds[which(is.na(ds[,i])),i] = median(ds[,i],na.rm=T)
    }
}

#ds$target[which(ds$target == 1)] = 'claim'
#ds$target[which(ds$target == 0)] = 'noclaim'

ds$id = NULL

keepvarshigh = c("target","ps_ind_01","ps_ind_03","ps_ind_05_cat","ps_ind_07_bin",
"ps_ind_08_bin","ps_ind_15","ps_ind_16_bin","ps_ind_17_bin","ps_reg_01","ps_reg_02",
"ps_reg_03","ps_car_03_cat","ps_car_05_cat","ps_car_06_cat","ps_car_07_cat","ps_car_12",
"ps_car_13","ps_car_14","ps_car_15","nact")

gbmvars = c("target","ps_car_13","ps_ind_05_cat","ps_ind_17_bin","ps_reg_03","ps_car_03_cat",
"ps_car_07_cat","ps_ind_07_bin","ps_ind_15","ps_ind_03","ps_reg_02","ps_ind_16_bin","nact")

kvh = which(names(ds) %in% keepvarshigh)
gh = which(names(ds) %in% gbmvars)

ds$target = as.factor(ds$target)

perc = 0.5
saveit = data.frame(matrix(nrow=5,ncol=6))

for (i in 1:5){
    useds = ds
    inTrain = createDataPartition(useds$target,p=perc,list=TRUE)[[1]]
    training = useds[inTrain,]
    testing = useds[-inTrain,]
    
    #   params = list(eta=0.09,max_depth=4,colsample_bytree=0.5,min_child_weight=50,subsample=1,gamma=5,objective='binary:logistic',booster='gbtree',eval_metric='auc')
    #  train_tmp = training
    #test = xgb.DMatrix(as.matrix(testing))
    #y=train_tmp$target
    #train_tmp$target = NULL
    #train_tmp = xgb.DMatrix(as.matrix(train_tmp),label=y)
    #xgfit = xgb.train(params,data=train_tmp,nrounds=10,verbose=F)
    #predxgb = predict(xgfit,newdata=test)
    #p0 = normalizedGini(predxgb,testing$target)
    
    print("training")
    glmfit = glm(target~.,training,family=binomial)
    print("predicting")
    pglm = predict(glmfit,type="response")
    predglm = predict(glmfit,newdata=testing,type="response")
    print("checking accuracy")
    p1 = NormalizedGini(predglm,as.numeric(as.character(testing$target)))
    
    print("training")
    ldafit = train(target~.,data=training,method="lda",metric="Kappa",trControl=trainControl(method="cv",number=5),verbose=F)
    print("predicting")
    plda = predict(ldafit,type="prob")
    predlda = predict(ldafit,newdata=testing,type="prob")
    print("checking accuracy")
    p2 = NormalizedGini(predlda[,2],as.numeric(as.character(testing$target)))
    
   
    print("training")
    gbmfit = train(target~.,data=training,method="gbm",metric="Kappa",trControl=trainControl(method="cv",number=2),verbose=F)
    print("predicting")
    pgbm = predict(gbmfit,type="prob")
    predgbm = predict(gbmfit,newdata=testing,type="prob")
    print("checking accuracy")
    p3 = NormalizedGini(predgbm[,2],as.numeric(as.character(testing$target)))
  
  #see if ensemble fitting helps performance
    ensfit = glm(training.target~.,data=data.frame(training$target,pglm,plda[,2],pnn,pgbm[,2]),family=binomial)
    newdata = data.frame(predglm,predlda[,2],prednn,predgbm[,2])
    names(newdata) = c("pglm","plda...2.","pgbm...2.")
    predens = predict(ensfit,newdata=newdata,type="response")
    pens = NormalizedGini(predens,as.numeric(as.character(testing$target)))
    
    mpred = apply(newdata,1,median)
    pmed = NormalizedGini(mpred,as.numeric(as.character(testing$target)))
    
  # fit = train(target~.,data=training,method="regLogistic",trControl=trainControl(method="cv",number=2))
  #predfit = predict(fit,newdata=testing,type="prob")
  #pfit = normGini(as.numeric(as.character(testing$target)),predfit[,2])
  
    saveit[i,] = data.frame(p1,p2,p3,pmed,pens)
    print(paste(p1,p2,p3,pmed,pens))
}
