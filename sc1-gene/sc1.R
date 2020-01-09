set.seed(10)
library(caret)
library(e1071)
library(randomForest)
## 读取数据
fm<-read.table("../../sc1_Phase1_GE_FeatureMatrix.tsv",header=T,row.names = 1)
oc<-read.table("sc1_Phase1_GE_Outcome.tsv",header=T)
ph<-read.table("sc1_Phase1_GE_Phenotype.tsv",header=T,sep="\t")
names(oc)<-c("pid","status")
names(ph)<-c("pid","sex","race","grade","cancerType")
## prepocessing
### 去除方差接近0的基因（病人间基本没差异）
zv=nearZeroVar(fm)
if(length(zv)>0){
	fm<-fm[,-zv]       ## zv为空时，会删掉fm的数据，所以要注意
}
### 去除相关性>0.9
Corr=cor(fm)
hc=findCorrelation(Corr,0.90)
fm_delH<-fm[,-hc]
### 去除线性关系，如1=2+3，删掉3
lc<-findLinearCombos(fm_delH)
fm_delHL<-fm_delH[,-lc$remove]
### 标准化，消除量纲差异（一般基于聚类的算法和模型需要标准化处理）；用knn填补缺失值
Process=preProcess(fm_delHL)
fm_delHLP<-predict(Process,fm_delHL)
### 合并数据
fm_cp<-fm_delHLP
fm_cp$pid<-rownames(fm_cp)
fm_fine<-merge(fm_cp,oc,by.x="pid",by.y="pid",all.x=T)
rownames(fm_fine)<-fm_fine$pid
fm_fine<-fm_fine[,-1]
fm_fine$status<-as.factor(fm_fine$status)

## 特征选择
### 1.对特征重要性排序 
control<-trainControl(method="repeatedcv",number=10,repeats=10,verbose=F,returnResamp="final")
model<-train(status~.,data=fm_fine,method="lvq",preProcess="scale",trControl=control)
importance<-varImp(model,scale=F)
print(importance)
pdf("importance.pdf")
plot(importance)
dev.off()
### 2.feature selection
control<-rfeControl(functions=rfFuncs,method="cv",number=10)
subsets=c(20,30,40,50,60,70,80,90)
fs_profile<-rfe(fm_fine[,1:376],fm_fine[,377],sizes=subsets,rfeControl=control)
print(fs_profile)
print(fs_profile$optVariables)
predictors(fs_profile)
plot(fs_profile,type=c("g","o"))
### 3.重要性和特征选择的结果进行比较

### 4.特征过滤

## 建模及调参
### 数据分割(train和test)
inTrain=createDataPartition(fm_fine$status,p=3/4,list=F)
trainx<-fm_fine[inTrain,1:376]
testx<-fm_fine[-inTrain,1:376]
trainy=fm_fine[inTrain,377]
testy=fm_fine[-inTrain,377]
##featurePlot(trainx[,1:2],trainy,plot='box')
### 建模
#### 交叉验证
fitControl=trainControl(method="repeatedcv",number=10,repeats=10,returnResamp="all")
#### 调参
gbmGrid=expand.grid(.interaction.depth=c(1,3), .n.trees=c(50,100,150,200,250,300), .shrinkage=0.1, .n.minobsinnode=10)
#### 训练模型
gbmFit=train(trainx,trainy,method="gbm",trControl=fitControl,tuneGrid=gbmGrid,verbose=F)
plot(gbmFit)
#### 预测模型
predict(gbmFit,newdata=textx)

### 模型比较
gbmFit2=train(trainx,trainy,method="treebag",trControl=fitControl)
models=list(gbmFit,gbmFit2)
#### 提取test的结果
predValues = extractPrediction(models,testX=testx,testY=testy)
print(predValues)
testValues=subset(predValues,dataType=="Test")
#### 提取test的概率
probValues=extractProb(models,testX=testx,testY=testy)
testProbs=subset(probValues,dataType="Test")
#### 分类混淆矩阵
Pred1=subset(testValues,model=="gbm")
Pred2=subset(testValues,model=="treebag")
confusionMatrix(Pred1$pred,Pred1$obs)
confusionMatrix(Pred2$pred,Pred2$obs)

### ROC图，根据上述模型的比较来决定画哪一个的ROC图
prob1=subset(testProbs,model=="gbm")
library(ROCR)
prob1$label=ifelse(prob1$obs=='Active',yes=1,0)
pred1=prediction(prob1$Active,prob1$label)
pref1=performance(pred1,measure="tpr",x.measure="fpr")
plot(pref1)