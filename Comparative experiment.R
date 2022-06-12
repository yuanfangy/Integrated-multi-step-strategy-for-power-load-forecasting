#对比实验
#对比实验
testXY=create_dataset(a[721:1008,4],look_back)#12月1号
testx=testXY$datax
testzy=(testXY$dataY)*(max-min)+min

#1EMD-SVR
library(EMD)
aad=EMDSVRhybrid(signal,0.903,funct="radial")
ss=aad[[1]]*(max-min)+min
plot(ss,type="l",xlab="小时",ylab="测试集的电力负荷值",col="red")
lines(testzy[,1],col="blue")

MAPE=mean(abs((ss-testzy[,1])/testzy[,1]))#0.06494597
MSE=mean((ss-testzy[,1])^2)#4563728
RMSE=sqrt(MSE)#2136.288
MAE=mean(abs(ss-testzy[,1]))#1578.114
write.csv(ss,file = "ES.csv",row.names = F)
#VMD-SVR
VS<-function(imf){
  trainxY5=create_dataset(imf[1:671],look_back)
  trainx=trainxY5$datax
  trainy=trainxY5$dataY
  testxY5=create_dataset(imf[721:1008],look_back)
  testx=testxY5$datax
  testy=testxY5$dataY
  svr4=svm(trainx,trainy[,1],type = "eps-regression", kernel = "radial")
  imf4=predict(svr4,testx)
  return(imf4)
}
imfv1=VS(imf1)
imfv2=VS(imf2)
imfv3=VS(imf3)
imfv4=VS(imf4)
imfv5=VS(imf5)
imfv6=VS(imf6)

#分量叠加
ZIMF=imfv1+imfv2+imfv3+imfv4+imfv5+imfv6
Zvs=ZIMF*(max-min)+min
plot(Zvs,type="l",xlab="小时",ylab="测试集的电力负荷值",col="red")
lines(testzy[,1],col="blue")

MAPE=mean(abs((Zvs-testzy[,1])/testzy[,1]))#0.02340635
MSE=mean((Zvs-testzy[,1])^2)#520909
RMSE=sqrt(MSE)#721.7403
MAE=mean(abs(Zvs-testzy[,1]))#605.1911
write.csv(Zvs,file = "VS.csv",row.names = F)

#2PSO-SVR
trainxYSVR=create_dataset(signal[1:671],look_back)
trainy=trainxYSVR$dataY
validxYsvr=create_dataset(signal[481:911],look_back)
validx=validxYsvr$datax
validy=validxYsvr$dataY
testxYsvr=create_dataset(signal[721:1008],look_back)
testy=testxYsvr$dataY
#第一步
fit=function(x) {#适应度函数
  model=svm(trainxYSVR$datax,trainy[,1],cost = x[1], gamma =x[2], type = "eps-regression", kernel = "radial")
  mape=mean((predict(model,validx)-validy[,1])^2)
  return (mape)
}
#$par[1]640.8974   0.0010  0.004602235
ps=psoptim(par = rep(NA,2),function(x) fit(x),
           lower = c(0.001,0.001),upper = c(1000,100),control = list(maxit=500))
svrs=svm(trainxYSVR$datax,trainy[,1], type = "eps-regression", kernel = "radial")
presvr=predict(svrs,testxYsvr$datax)*(max-min)+min

plot(presvr,type="l",xlab="小时",ylab="测试集的电力负荷值",col="red")
lines(testzy[,1],col="blue")
MAPE=mean(abs((presvr-testzy[,1])/testzy[,1]))#0.02002251
MSE=mean((presvr-testzy[,1])^2)#270113.2
RMSE=sqrt(MSE)#519.7242
MAE=mean(abs(presvr-testzy[,1]))#415.979
write.csv(presvr,file = "PSOSVR.csv",row.names = F)
#ANN
#library(neuralnet)
library(nnet)
trainxYSVR=create_dataset(signal[1:671],look_back)
trainy=trainxYSVR$dataY
validxYsvr=create_dataset(signal[481:911],look_back)
validx=validxYsvr$datax
validy=validxYsvr$dataY
testxYsvr=create_dataset(signal[721:1008],look_back)
testy=testxYsvr$dataY
for (i in 5:30) {
  model =nnet(trainxYSVR$datax[,160:168],trainxYSVR$dataY[,1], maxit = 500, size =i,trace = FALSE)
  print(mean((predict(model,validx[,160:168])-validy[,1])^2))
}
model =nnet(trainxYSVR$datax[,160:168],trainxYSVR$dataY[,1], maxit = 500, size =18,trace = FALSE)
ANN=predict(model,testxYsvr$datax[,160:168])*(max-min)+min

mean(abs((ANN-testzy[,1])/testzy[,1]))#0.0182745
MSE=mean((ANN-testzy[,1])^2)#341258.4
sqrt(MSE)#584.1733
mean(abs(ANN-testzy[,1]))#462.7314
write.csv(ANN,file = "ANN.csv",row.names = F)

#3VMD-PSO-SVR
VPS<-function(imf){
  trainxY5=create_dataset(imf[1:671],look_back)
  trainx=trainxY5$datax
  trainy=trainxY5$dataY
  validxY5=create_dataset(imf[481:911],look_back)
  validx=validxY5$datax
  validy=validxY5$dataY
  testxY5=create_dataset(imf[721:1008],look_back)
  testx=testxY5$datax
  testy=testxY5$dataY
  #第一步
  fit=function(x) {#适应度函数
    model=svm(trainx,trainy[,1],cost = x[1], gamma =x[2], type = "eps-regression", kernel = "radial")
    mape=mean((predict(model,validx)-validy[,1])^2)
    return (mape)
  }
  #$par[1]41.65426377  0.03549826  3.066668e-05
  ps=psoptim(par = rep(NA,2),function(x) fit(x),
             lower = c(0.001,0.001),upper = c(1000,100),control = list(maxit=500))
  svr4=svm(trainx,trainy[,1],cost =ps$par[1], gamma= ps$par[2], type = "eps-regression", kernel = "radial")
  imf4=predict(svr4, testx)
  return(imf4)
}
imf11=VPS(imf1)
imf22=VPS(imf2)
imf33=VPS(imf3)
imf44=VPS(imf4)
imf55=VPS(imf5)
imf66=VPS(imf6)

#分量叠加
ZIMF=imf11+imf22+imf33+imf44+imf55+imf66
Z=ZIMF*(max-min)+min
plot(Z,type="l",xlab="小时",ylab="测试集的电力负荷值",col="red")
lines(testzy[,1],col="blue")

MAPE=mean(abs((Z-testzy[,1])/testzy[,1]))#0.01145706
MSE=mean((Z-testzy[,1])^2)#140165.1
RMSE=sqrt(MSE)#374.3863
MAE=mean(abs(Z-testzy[,1]))#298.4712
write.csv(Z,file = "VPS.csv",row.names = F)

#4VMD-mRMR-SVR
VmS<-function(imf){
  trainxY5=create_dataset(imf[1:671],look_back)
  trainx=trainxY5$datax
  trainy=trainxY5$dataY
  validxY5=create_dataset(imf[481:911],look_back)
  validx=validxY5$datax
  validy=validxY5$dataY
  testxY5=create_dataset(imf[721:1008],look_back)
  testx=testxY5$datax
  testy=testxY5$dataY
  ge=mRMR.data(data = data.frame(trainy[,1],trainx))
  exect=system.time(fs <- new("mRMRe.Filter", data = ge,
                              target_indices = 1, levels = c(1,1,1,1, 1, 1, 1, 1)))
  trainx4=trainx[,c(solutions(fs)[[1]]-1)]#特征选择
  trainX=data.frame(a =trainx4[,1], b =trainx4[,2],c=trainx4[,3],
                    d =trainx4[,4],e=trainx4[,5],f =trainx4[,6],
                    g =trainx4[,7],h =trainx4[,8])
  validx3=validx[,c(solutions(fs)[[1]]-1)]#特征选择
  validx=data.frame(a =validx3[,1], b =validx3[,2],c=validx3[,3],
                    d =validx3[,4],e=validx3[,5],f =validx3[,6],
                    g =validx3[,7],h =validx3[,8])
  tesx4=testx[,c(solutions(fs)[[1]]-1)]
  testx=data.frame(a =tesx4[,1], b =tesx4[,2],c=tesx4[,3],
                   d =tesx4[,4],e=tesx4[,5],f =tesx4[,6],
                   g =tesx4[,7],h =tesx4[,8])
  svr4=svm(trainX,trainy[,1],type = "eps-regression", kernel = "radial")
  imf4=predict(svr4,testx)
  return(imf4)
}
imfm1=VmS(imf1)
imfm2=VmS(imf2)
imfm3=VmS(imf3)
imfm4=VmS(imf4)
imfm5=VmS(imf5)
imfm6=VmS(imf6)
#分量叠加
ZIMF=imfm1+imfm2+imfm3+imfm4+imfm5+imfm6
Zm=ZIMF*(max-min)+min
plot(Zm,type="l",xlab="小时",ylab="测试集的电力负荷值",col="red")
lines(testzy[,1],col="blue")

MAPE=mean(abs((Zm-testzy[,1])/testzy[,1]))#0.01557542
MSE=mean((Zm-testzy[,1])^2)#247206.9
RMSE=sqrt(MSE)#497.1991
MAE=mean(abs(Zm-testzy[,1]))#396.5836
write.csv(Zm,file = "VmS.csv",row.names = F)

plot(testzy[,1],type="l",xlab="小时",ylab="测试集的电力负荷值",col="blue")
#本文
lines(Zong,type="l",col="red")
#1
lines(Zong2,type="l",col="yellow")
#SVR
lines(presvr,type="l",col="green")
#3VMD-SVR
lines(Zongvs,type="l",col="gray")
#4VMD-PSO-SVR
lines(Zongvps,type="l",col="orange")

#################################################
ANN=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/ANN.csv",sep = ",",header =T)
ES=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/ES.csv",sep = ",",header =T)
PSOSVR=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/PSOSVR.csv",sep = ",",header =T)
SVR=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/SVR.csv",sep = ",",header =T)
VmS=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/VmS.csv",sep = ",",header =T)
VPS=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/VPS.csv",sep = ",",header =T)
VS=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/对比实验/VS.csv",sep = ",",header =T)
VmPS=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/单步预测结果/VmPS.csv",sep = ",",header =T)

library(plotrix)
par(mfrow= c(1,1))
oldpar<-taylor.diagram(testzy[,1],VmPS[,1],sd.arcs=1,ylab="SStandard deviation",cex.axis=1.5)
taylor.diagram(testzy[,1],SVR[,1],add=TRUE,col=1)
taylor.diagram(testzy[,1],ANN[,1],add=TRUE,col=2)
taylor.diagram(testzy[,1],PSOSVR[,1],add=TRUE,col=3)
taylor.diagram(testzy[,1],ES[,1],add=TRUE,col=4)
taylor.diagram(testzy[,1],VS[,1],add=TRUE,col=5)
taylor.diagram(testzy[,1],VmS[,1],add=TRUE,col=6)
taylor.diagram(testzy[,1],VPS[,1],add=TRUE,col=7)
lpos<-1.25*sd(testzy[,1])
legend(lpos,1.9*sd(testzy[,1]),cex=1,text.width=1500,bty="n",
       legend=c("The proposed","SVR","ANN","PSO-SVR","EMD-SVR","VMD-SVR","VMD-mRMR-SVR","VMD-tsPSO-SVR"),pch=19,col=c("red",1,2,3,4,5,6,7))

library(openair)
d=data.frame(obs=testzy[,1],mod=VmPS[,1])
mode1=transform(d,model="The proposed")
d=data.frame(obs=testzy[,1],mod=SVR[,1])
mode11=transform(d,model="SVR")
d=data.frame(obs=testzy[,1],mod=ANN[,1])
mode12=transform(d,model="ANN")
d=data.frame(obs=testzy[,1],mod=PSOSVR[,1])
mode13=transform(d,model="PSO-SVR")
d=data.frame(obs=testzy[,1],mod=ES[,1])
mode14=transform(d,model="EMD-SVR")
d=data.frame(obs=testzy[,1],mod=VS[,1])
mode15=transform(d,model="VMD-SVR")
d=data.frame(obs=testzy[,1],mod=VmS[,1])
mode16=transform(d,model="VMD-mRMR-SVR")
d=data.frame(obs=testzy[,1],mod=VPS[,1])
mode17=transform(d,model="VMD-tsPSO-SVR")
modell=rbind(mode1,mode11,mode12,mode13,mode14,mode15,mode16,mode17)
TaylorDiagram(modell, obs = "obs", mod = "mod", group = "model")
