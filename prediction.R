rm(list=ls())
gc()

library(ISLR)
library(MASS)
library(e1071)
library(randomForest)

##Data=read.csv("C:/Users/Administrator/Desktop/EdmontonRealEstateData.csv")
Data=read.csv("D:/U of A/2021-winter course/ECE 625/group project/EdmontonRealEstateData.csv")
attach(Data)
basement=as.numeric(basement_finished=="Yes")
garage=as.numeric(has_garage=="Yes")
fireplace=as.numeric(has_fireplace=="Yes")
taxable=as.numeric(fully_taxable=="Yes")
complete=as.numeric(fully_complete=="Yes")
walkout.basement=as.numeric(walkout_basement=="Yes")
air.conditioning=as.numeric(air_conditioning=="Yes")
Data2=data.frame(Data[,-c(2,3,4,5,8,9,10,12,14,15,16,17,18,19,20,21,23,24,25,27,28,29,30,31,34,35,36,37,38,39)],basement,garage,fireplace,taxable,complete,walkout.basement,air.conditioning)
Data2=na.omit(Data2)
dim(Data2)
effective_build_year=as.numeric(Data2$effective_build_year)
building_count=as.numeric(Data2$building_count)
Prediction=rep(0,70620)
Prediction[Data2$X2016.Assessed_Value>Data2$X2015.Assessed_Value]=1
Data3=data.frame(Data2[,-c(2,4,7,8)],effective_build_year,building_count,Prediction)

set.seed(1)
train=sample(nrow(Data3),nrow(Data3)/10*9)
test=(-train)

glm.fit=glm(Prediction~.-X2016.Assessed_Value,data=Data3,family=binomial,subset=train)
summary(glm.fit)
glm.probs=predict(glm.fit,Data3[test,],type="response")
glm.pred=rep(0,7062)
glm.pred[glm.probs>0.5]=1
table(glm.pred,Prediction[test])
error=mean(glm.pred!=Prediction[test])
error

glm.fit2=glm(Prediction~.-X2016.Assessed_Value-net_area-M2_1-M2_2-basement,data=Data3,family=binomial,subset=train)
summary(glm.fit2)
glm.probs2=predict(glm.fit2,Data3[test,],type="response")
glm.pred2=rep(0,7062)
glm.pred2[glm.probs2>0.5]=1
table(glm.pred2,Prediction[test])
error2=mean(glm.pred2!=Prediction[test])
error2



## no assessed 2015
glm.fit3=glm(Prediction~.-X2016.Assessed_Value-X2015.Assessed_Value,data=Data3,family=binomial,subset=train)
summary(glm.fit3)
glm.probs3=predict(glm.fit3,Data3[test,],type="response")
glm.pred3=rep(0,7062)
glm.pred3[glm.probs3>0.5]=1
table(glm.pred3,Prediction[test])
error3=mean(glm.pred3!=Prediction[test])
error3

glm.fit4=glm(Prediction~.-X2016.Assessed_Value-X2015.Assessed_Value-M2_1-basement,data=Data3,family=binomial,subset=train)
summary(glm.fit4)
glm.probs4=predict(glm.fit4,Data3[test,],type="response")
glm.pred4=rep(0,7062)
glm.pred4[glm.probs4>0.5]=1
table(glm.pred4,Prediction[test])
error4=mean(glm.pred4!=Prediction[test])
error4

## LDA
lda.fit=lda(Prediction~.-X2016.Assessed_Value,data=Data3,subset=train)
lda.fit
lda.pred=predict(lda.fit,Data3[test,])
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Prediction[test])
(1614+368)/7062

lda.fit2=lda(Prediction~.-X2016.Assessed_Value-X2015.Assessed_Value,data=Data3,subset=train)
lda.fit2
lda.pred2=predict(lda.fit2,Data3[test,])
names(lda.pred2)
lda.class2=lda.pred2$class
table(lda.class2,Prediction[test])
(1615+371)/7062

## QDA
qda.fit=qda(Prediction~.-X2016.Assessed_Value,data=Data3,subset=train)
qda.fit
qda.pred=predict(qda.fit,Data3[test,])
qda.class=qda.pred$class
table(qda.class,Prediction[test])
(1157+791)/7062

qda.fit2=qda(Prediction~.-X2016.Assessed_Value-X2015.Assessed_Value,data=Data3,subset=train)
qda.fit2
qda.pred2=predict(qda.fit2,Data3[test,])
qda.class2=qda.pred2$class
table(qda.class2,Prediction[test])
(1828+546)/7062


## SVM
x=as.matrix(Data3[,-c(1,2,3,9,19)])
Data3=data.frame(x=x,y=as.factor(Prediction))
svm.fit=svm(y~.,data=Data3[train,],kernel="linear",cost=1,scale=FALSE,probability=TRUE)
svm.pred=attr(predict(svm.fit,Data3[test,],probability=TRUE),'probabilities')[,2]
svm.fit2=svm(y`.,data=Data3[train,],kernel="radial",gamma=2,cost=1,scale=FALSE,probability=TRUE)
svm.pred2=attr(predict(svm.fit2,Data3[test,],probability=TRUE),'probabilities')[,2]







