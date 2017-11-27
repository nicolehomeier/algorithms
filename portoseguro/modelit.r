
perc = 0.5
for (i in 1:10){
    inTrain = createDataPartition(kvhds$target,p=perc,list=TRUE)[[1]]
    training = kvhds[inTrain,]
    testing = kvhds[-inTrain,]
    #print("training rf")
    #rffit = randomForest(target~.,data=training[1:100000,],ntrees=200,mtry=6)
    #print("predicting")
    #predrf = predict(rffit,newdata=testing)
    #predict("checking accuracy")
    #normGini(testing$target,predrf)
    
    print("training")
    glmfit = glm(target~.,training,family=binomial)
    print("predicting")
    predglm = predict(glmfit,newdata=testing)
    print("checking accuracy")
    p1 = normGini(testing$target,predglm)
    
    print("training")
    rregfit = train(target~.,data=training,method="rlm",metric="RMSE",trControl=trainControl(method="cv",number=5))
    print("predicting")
    predrreg = predict(rregfit,newdata=testing)
    print("checking accuracy")
    p2 = normGini(testing$target,predrreg)
    
    print("training")
    gbmfit = train(target~.,data=training,method="gbm",trControl=trainControl(method="cv",number=2))
    print("predicting")
    predgbm = predict(gbmfit,newdata=testing)
    print("checking accuracy")
    p3 = normGini(testing$target,predgbm)
    
    
    #predict(rffit,newdata=testing,prob=T)
    
    avgpred = apply(data.frame(predglm,predrreg,predgbm),1,mean)
    p4 = normGini(testing$target,avgpred)
    print(paste(p1,p2,p3,p4))
}