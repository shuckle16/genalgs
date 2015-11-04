#### Logistic regresssion / lasso


library(glmnet)
library(SDMTools)

LR <-glmnet(y = ytrain_sm,x=data.matrix(xtrain_sm),family="binomial")


aucstore <- numeric()
for (i in 1:length(LR$lambda) ) {
  predglmnet <- predict(LR,newx=data.matrix(xtrain_val),type="response",s=LR$lambda[i])
  aucstore[i] <- AUC::auc(roc(as.numeric(predglmnet),as.factor(ytrain_val)))
}

plot(1:length(LR$lambda),aucstore,type="l")       
LR.lambda <- LR$lambda[which.max(aucstore)]


LR <-  glmnet(x=data.matrix(xtrain), y=ytrain, family="binomial")
predLRlas <- as.numeric(predict(LR,newx=data.matrix(xtest),type="response",s=LR.lambda))
AUC::auc(roc(predLRlas,as.factor(ytest)))
plot(roc(predLRlas,as.factor(ytest)))


confusion.matrix(obs=ytest,pred = predLRlas)




#### Adaboost

library(ada)


ABmodel <- ada(x=xtrain_sm,y=ytrain_sm,test.x=xtrain_val,test.y=ytrain_val,iter=100)
predAB <- as.numeric(predict(ABmodel,xtest,type="probs")[,2])
AUC::auc(roc(predAB,as.factor(ytest)))
plot(ABmodel,test=TRUE)

cm_ada <-confusion.matrix(obs=ytest,pred=predAB)

(cm_ada[1]+cm_ada[4])/sum(cm_ada)


#### Random Forest (slow)


library(randomForest)

rFmodel <- randomForest(x=xtrain,y=ytrain,  ntree=500, importance=TRUE)

#look at the importance of the variables
importance(rFmodel)[,"MeanDecreaseAccuracy"]
varImpPlot(rFmodel)
#For more imformation about the measures: ?importance

#prediction

predrF <- predict(rFmodel,xtest,type="prob")[,2]
AUC::auc(roc(predrF,ytest))
plot(roc(predrF,ytest))




####### Kernel factory (very slow)


#KF: Kernel Factory 

if (require("kernelFactory")==FALSE) install.packages("kernelFactory") ; library(kernelFactory)
library(AUC)
?kernelFactory

KF <- kernelFactory(xtrain,as.factor(ytrain))

predKF <- predict(KF, newdata=BasetableTEST)

auc(roc(predKF,yTEST))

ls.str(KF)
#class assignment: tune kernelFactory for cp and rp

aucstore <- data.frame(matrix(data=NA,ncol=3,nrow=4))
colnames(aucstore) <- c('AUC',"rp","cp")
head(aucstore)

i <- 0
for (rp in 1:2) {
  for (cp in 1:2){
    i <- i + 1
    KF <- kernelFactory(BasetableTRAIN,as.factor(yTRAIN), rp=rp,cp=cp)
    predKF <- predict(KF, newdata=BasetableTEST)
    aucstore[i,"AUC"] <- auc(roc(predKF,yTEST))
    aucstore[i,"rp"] <- rp
    aucstore[i,"cp"] <- cp
    print(i)
    
  }
}

aucstore





