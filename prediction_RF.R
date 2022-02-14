rm(list=ls())
gc()

library(ISLR)
library(MASS)
library(tree)
library(randomForest)
library(gbm)

Data=read.csv("D:/U of A/2021-winter course/ECE 625/group project/EdmontonRealEstateData.csv")
attach(Data)
basement=as.factor(basement_finished)
garage=as.factor(has_garage)
fireplace=as.factor(has_fireplace)
taxable=as.factor(fully_taxable)
complete=as.factor(fully_complete)
walkout.basement=as.factor(walkout_basement)
air.conditioning=as.factor(air_conditioning)


Data2=data.frame(Data[,-c(2,3,4,5,8,9,10,12,14,15,16,17,18,19,20,21,23,24,25,27,28,29,30,31,34,35,36,37,38,39)],basement,garage,fireplace,taxable,complete,walkout.basement,air.conditioning)
Data2=na.omit(Data2)
dim(Data2)
effective_build_year=as.numeric(Data2$effective_build_year)
building_count=as.numeric(Data2$building_count)
Prediction=rep(0,70620)
Prediction[Data2$X2016.Assessed_Value>Data2$X2015.Assessed_Value]=1
Prediction=as.factor(Prediction)
Data3=data.frame(Data2[,-c(2,4,7,8)],effective_build_year,building_count,Prediction)
names(Data3)
Data3=Data3[,-9]
dim(Data3)
set.seed(1)
train=sample(nrow(Data3),nrow(Data3)/10*9)
test=(-train)

## classification tree
set.seed(1)
tree.fit=tree(Prediction~.,Data3,subset=train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty=0)
tree.fit
tree.pred=predict(tree.fit,Data3[test,],type="class")
table(tree.pred,Prediction[test])
(1348+694)/7062

set.seed(1)
cv.treefit=cv.tree(tree.fit,FUN=prune.misclass)
names(cv.treefit)
cv.treefit
prune.treefit=prune.misclass(tree.fit,best=7)
plot(prune.treefit)
text(prune.treefit,pretty=0)
tree.pred2=predict(prune.treefit,Data3[test,],type="class")
table(tree.pred2,Prediction[test])

set.seed(1)
tree.fit2=tree(Prediction~.-X2015.Assessed_Value,Data3,subset=train)
summary(tree.fit2)
plot(tree.fit2)
text(tree.fit2,pretty=0)
tree.fit2
tree.pred3=predict(tree.fit2,Data3[test,],type="class")
table(tree.pred3,Prediction[test])
(1348+694)/7062

set.seed(1)
cv.treefit2=cv.tree(tree.fit2,FUN=prune.misclass)
names(cv.treefit2)
cv.treefit2
prune.treefit2=prune.misclass(tree.fit2,best=6)
plot(prune.treefit2)
text(prune.treefit2,pretty=0)
tree.pred4=predict(prune.treefit2,Data3[test,],type="class")
table(tree.pred4,Prediction[test])


## random forest classification
set.seed(1)
bagging.fit=randomForest(Prediction~.,data=Data3,subset=train,mtry=17,importance=TRUE,na.action=na.exclude)
bagging.fit
bagging.pred=predict(bagging.fit,newdata=Data3[test,],type="response")
table(bagging.pred,Prediction[test])
(385+305)/7062
varImpPlot(bagging.fit)

set.seed(1)
bagging.fit2=randomForest(Prediction~.-X2015.Assessed_Value,data=Data3,subset=train,mtry=16,importance=TRUE,na.action=na.exclude)
bagging.fit2
bagging.pred2=predict(bagging.fit2,newdata=Data3[test,],type="response")
table(bagging.pred2,Prediction[test])
(413+322)/7062
varImpPlot(bagging.fit2)


set.seed(1)
RF.fit=randomForest(Prediction~.,data=Data3,subset=train,mtry=4,importance=TRUE,na.action=na.exclude)
RF.fit
RF.pred=predict(RF.fit,newdata=Data3[test,],type="response")
table(RF.pred,Prediction[test])
(409+279)/7062
varImpPlot(RF.fit)

## the whole data set
set.seed(1)
RF.fit3=randomForest(Prediction~.,data=Data3,subset=NULL,mtry=4,importance=TRUE,na.action=na.exclude)
RF.fit3
RF.pred3=predict(RF.fit3,type="response")
table(RF.pred3,Prediction)
Price.direction=rep("Decrease",70620)
Price.direction[RF.pred3==1]="Increase"


a=match(Data$taxroll_number,Data3$taxroll_number)
index=which(a!=0)
Data4=Data[index,]
Data5=Data[-index,]

setwd("D:/U of A/2021-winter course/ECE 625/group project")
write.csv(x=Data4,file="Data_keep.csv")
write.csv(x=Data5,file="Data_remove.csv")
write.csv(x=Price.direction,file="assessed.increase1.csv")


set.seed(1)
RF.fit2=randomForest(Prediction~.-X2015.Assessed_Value,data=Data3,subset=train,mtry=4,importance=TRUE,na.action=na.exclude)
RF.fit2
RF.pred2=predict(RF.fit2,newdata=Data3[test,],type="response")
table(RF.pred2,Prediction[test])
(427+286)/7062
varImpPlot(RF.fit2)

## the whole data set
set.seed(1)
RF.fit4=randomForest(Prediction~.-X2015.Assessed_Value,data=Data3,mtry=4,importance=TRUE,na.action=na.exclude)
RF.fit4
RF.pred4=predict(RF.fit4,type="response")
table(RF.pred4,Prediction)
Price.direction2=rep("Decrease",70620)
Price.direction2[RF.pred4==1]="Increase"
write.csv(x=Price.direction2,file="assessed.increase2.csv")



## boosting classification
set.seed(1)
boost.fit=gbm(Prediction~.,data=Data3[train,],distribution="bernoulli",n.trees=500,interaction.depth=6,shrinkage=0.01,verbose=F)
boost.probs=predict(boost.fit,newdata=Data3[test,],n.trees=500,type="response")
boost.pred=rep("Decrease",7062)
boost.pred[boost.probs>.5]="Increase"
table(boost.pred,Prediction[test])
1566/7062
summary(boost.fit)

set.seed(1)
boost.fit2=gbm(Prediction~.-X2015.Assessed_Value,data=Data3[train,],distribution="bernoulli",n.trees=500,interaction.depth=6,shrinkage=0.01,verbose=F)
boost.probs2=predict(boost.fit2,newdata=Data3[test,],n.trees=500,type="response")
boost.pred2=rep("Decrease",7062)
boost.pred2[boost.probs2>.5]="Increase"
table(boost.pred2,Prediction[test])
(1110+516)/7062
summary(boost.fit2)

