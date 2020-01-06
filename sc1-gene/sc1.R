set.seed(1)
library(caret)
## load data
fm<-read.table("sc1_Phase1_GE_FeatureMatrix.tsv",header=T,row.names = 1)
oc<-read.table("sc1_Phase1_GE_Outcome.tsv",header=T)
ph<-read.table("sc1_Phase1_GE_Phenotype.tsv",header=T,sep="\t")
## prepocessing
### 去除方差接近0的基因（病人间基本没差异）
zv=nearZeroVar(fm)
fm<-fm[,-zv]
### 去除相关性>0.9
Corr=cor(fm)
hc=findCorrelation(Corr,0.90)
fm_delH<-fm[,-hc]
### 去除共线性
lc<-findLinearCombos(fm_delH)
fm_delHL<-fm_delH[,-lc$remove]
### 缺失值插值
Process=preProcess(fm_delHL)
fm_delHLP<-predict(Process,fm_delHL)
### 数据随机分块 
inTrain=createDataPartition(oc$SURVIVAL_STATUS,p=3/4,list=F)
trainx<-fm_delHLP[inTrain,]
testx<-fm_delHLP[-inTrain,]
trainy=oc[inTrain,]
testy=oc[-inTrain,]
featurePlot(trainx[,1:2],trainy,plot='box')