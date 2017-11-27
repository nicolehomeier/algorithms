library(caret)
library(MLmetrics)
library(plyr)
library(dplyr)
#set.seed(1234)
kfold = 2
hb_perc = 0.25

ds = read.csv("interm_train.csv")
ds$id = NULL
ds$target = as.factor(ds$target)

pattern="bin|cat"
for (i in 1:ncol(ds)){
    if (grepl(pattern,names(ds)[i])){
        ds[,i] = factor(ds[,i])
    }
}

inHB = createDataPartition(ds$target,p=hb_perc,list=TRUE)[[1]]
hb_test = ds[inHB,]
hb_train = ds[-inHB,]

gbm_hb_kfold=0
ens_hb_kfold=0
med_hb_kfold=0

useds = hb_train
groups = createFolds(useds$target,k=kfold,list=TRUE)

for (i in 1:kfold){
    inTest = groups[[i]]
    training = useds[-inTest,]
    testing = useds[inTest,]

#upsample to balance the classes
    clms = which(training$target == 1)
    nulls = which(training$target==0)
    upsamples = sample(length(clms),length(nulls),replace=T)
    newclms = training[clms[upsamples],]
    training = rbind(training[nulls,],newclms)

    print("training")
    glmfit = glm(target~.,training,family=binomial)
    print("predicting")
    pglm = predict(glmfit,type="response")
    predglm = predict(glmfit,newdata=testing,type="response")
    predglm_hb = predict(glmfit,newdata=hb_test,type="response")
    print("checking accuracy")
    p1 = NormalizedGini(predglm,as.numeric(as.character(testing$target)))
    
    print("training")
    ldafit = train(target~.,data=training,method="lda",metric="Kappa",trControl=trainControl(method="cv",number=5),verbose=F)
    print("predicting")
    plda = predict(ldafit,type="prob")
    predlda = predict(ldafit,newdata=testing,type="prob")
    predlda_hb = predict(ldafit,newdata=hb_test,type="prob")
    print("checking accuracy")
    p2 = NormalizedGini(predlda[,2],as.numeric(as.character(testing$target)))
    
    print("training")
       gbmfit = train(target~.,data=training,method="gbm",metric="Kappa",trControl=trainControl(method="cv",number=2),
    tuneGrid=expand.grid(.n.trees=150,.interaction.depth=2,.shrinkage=0.1,.n.minobsinnode=10),verbose=F)
    # gbmfit = gbm(target~.,data=training,n.trees=150,shrinkage=0.1,n.minobsinnode=10)
    print("predicting")
    pgbm = predict(gbmfit,type="prob")
    predgbm = predict(gbmfit,newdata=testing,type="prob")
    predgbm_hb = predict(gbmfit,newdata=hb_test,type="prob")
    print("checking accuracy")
    p3 = NormalizedGini(predgbm[,2],as.numeric(as.character(testing$target)))
  
  #see if ensemble fitting helps performance
    ensfit = glm(training.target~.,data=data.frame(training$target,pglm,plda[,2],pgbm[,2]),family=binomial)
    newdata = data.frame(predglm,predlda[,2],predgbm[,2])
    newdata_hb = data.frame(predglm_hb,predlda_hb[,2],predgbm_hb[,2])
    names(newdata) = c("pglm","plda...2.","pgbm...2.")
    names(newdata_hb) = c("pglm","plda...2.","pgbm...2.")
    predens = predict(ensfit,newdata=newdata,type="response")
    predens_hb = predict(ensfit,newdata=newdata_hb,type="response")
    pens = NormalizedGini(predens,as.numeric(as.character(testing$target)))
    
    mpred = apply(newdata,1,median)
    pmed = NormalizedGini(mpred,as.numeric(as.character(testing$target)))
    mpred_hb = apply(newdata_hb,1,median)
    
  # fit = train(target~.,data=training,method="regLogistic",trControl=trainControl(method="cv",number=2))
  #predfit = predict(fit,newdata=testing,type="prob")
  #pfit = normGini(as.numeric(as.character(testing$target)),predfit[,2])
    print(paste(round(p1,digits=3),round(p2,digits=3),round(p3,digits=3),round(pmed,digits=3),round(pens,digits=3)))
    
    gbm_hb_kfold = gbm_hb_kfold + predgbm_hb[,2]
    ens_hb_kfold = ens_hb_kfold + predens_hb
    med_hb_kfold = med_hb_kfold + mpred_hb
}

gbm_hb_kfold = gbm_hb_kfold/kfold
ens_hb_kfold = ens_hb_kfold/kfold
med_hb_kfold = med_hb_kfold/kfold

#evaluate how the models performed on the hold-back sample
ng_gbm = NormalizedGini(gbm_hb_kfold,as.numeric(as.character(hb_test$target)))
ng_ens = NormalizedGini(ens_hb_kfold,as.numeric(as.character(hb_test$target)))
ng_med = NormalizedGini(med_hb_kfold,as.numeric(as.character(hb_test$target)))
print(round(ng_gbm,digits=3))
print(round(ng_ens,digits=3))
print(round(ng_med,digits=3))

#now let's evaluate whether the performance differences are significant
# first price 'cats' number of risk groups, assuming 'cc' claim cost and 'gain' desired remainder per policy after losses

#order the last testing sample and hold-back sample by the model predictions
ord_gbm = testing$target[order(predgbm[,2],decreasing=T)]
ord_med = testing$target[order(mpred,decreasing=T)]
ord_gbm_hb = hb_test$target[order(gbm_hb_kfold,decreasing=T)]
ord_med_hb = hb_test$target[order(med_hb_kfold,decreasing=T)]

out = data.frame(ord_gbm_hb,ord_med_hb)
out_test = data.frame(ord_gbm,ord_med)
names(out) = c("GBM-5fold-HB","Median-5fold-HB")
names(out_test) = c("GBM-5fold-LastTest","Median-5fold-LastTest")
write.csv(out,"holdback_ordered_output.csv",row.names=F)
write.csv(out_test,"test_ordered_output.csv",row.names=F)

