kvhds$target = as.factor(kvhds$target)


perc = 0.5

saveit = array(,c(10,5))
for (i in 1:10){
    inTrain = createDataPartition(kvhds$target,p=perc,list=TRUE)[[1]]
    training = kvhds[inTrain,]
    testing = kvhds[-inTrain,]
    print("training rf")
    rffit = randomForest(target~.,data=training[1:100000,],ntrees=200,mtry=6)
    print("predicting")
    predrf = predict(rffit,newdata=testing,prob=T)
    predict("checking accuracy")
    normGini(testing$target,predrf)
    
    print("training")
    glmfit = glm(target~.,training,family=binomial)
    print("predicting")
    predglm = predict(glmfit,newdata=testing,prob=T)
    print("checking accuracy")
    p1 = normGini(testing$target,predglm)
    
    print("training")
    rregfit = train(target~.,data=training,method="lda",metric="RMSE",trControl=trainControl(method="cv",number=5))
    print("predicting")
    predrreg = predict(rregfit,newdata=testing,prob=T)
    print("checking accuracy")
    p2 = normGini(testing$target,predrreg)
    
    print("training")
    rpartfit = train(target~.,data=training,method="rpart",trControl=trainControl(method="cv",number=2))
    print("predicting")
    predrpart = predict(gbmfit,newdata=testing,prob=T)
    print("checking accuracy")
    p3 = normGini(testing$target,predrpart)
    print("training")
    gbmfit = train(target~.,data=training,method="gbm",trControl=trainControl(method="cv",number=2))
    print("predicting")
    predgbm = predict(gbmfit,newdata=testing,prob=T)
    print("checking accuracy")
    p4 = normGini(testing$target,predgbm)
  
    
    avgpred = apply(data.frame(predglm,predrreg,predgbm),1,mean)
    p5 = normGini(testing$target,avgpred)
    saveit[i,] = data.frame(p1,p2,p3,p4,p5)
    print(saveit[i,])
}
