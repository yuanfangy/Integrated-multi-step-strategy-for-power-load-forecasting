IMF1z=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接/IMF1z.csv",sep = ",",header =T)
IMF2z=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接/IMF2z.csv",sep = ",",header =T)
IMF3z=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接/IMF3z.csv",sep = ",",header =T)
IMF4z=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接/IMF4z.csv",sep = ",",header =T)
IMF5z=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接/IMF5z.csv",sep = ",",header =T)
IMF6z=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接/IMF6z.csv",sep = ",",header =T)


IMF1t=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代/IMF1t.csv",sep = ",",header =T)
IMF2t=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代/IMF2t.csv",sep = ",",header =T)
IMF3t=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代/IMF3t.csv",sep = ",",header =T)
IMF4t=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代/IMF4t.csv",sep = ",",header =T)
IMF5t=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代/IMF5t.csv",sep = ",",header =T)
IMF6t=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代/IMF6t.csv",sep = ",",header =T)

IMF1tz=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代直接/IMF1tz.csv",sep = ",",header =T)
IMF2tz=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代直接/IMF2tz.csv",sep = ",",header =T)
IMF3tz=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代直接/IMF3tz.csv",sep = ",",header =T)
IMF4tz=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代直接/IMF4tz.csv",sep = ",",header =T)
IMF5tz=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代直接/IMF5tz.csv",sep = ",",header =T)
IMF6tz=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代直接/IMF6tz.csv",sep = ",",header =T)


imf1MIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/MIMO/imf1MIMO.csv",sep = ",",header =T)
imf2MIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/MIMO/imf2MIMO.csv",sep = ",",header =T)
imf3MIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/MIMO/imf3MIMO.csv",sep = ",",header =T)
imf4MIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/MIMO/imf4MIMO.csv",sep = ",",header =T)
imf5MIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/MIMO/imf5MIMO.csv",sep = ",",header =T)
imf6MIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/MIMO/imf6MIMO.csv",sep = ",",header =T)


imf1dieMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代MIMO/imf1dieMIMO.csv",sep = ",",header =T)
imf2dieMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代MIMO/imf2dieMIMO.csv",sep = ",",header =T)
imf3dieMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代MIMO/imf3dieMIMO.csv",sep = ",",header =T)
imf4dieMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代MIMO/imf4dieMIMO.csv",sep = ",",header =T)
imf5dieMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代MIMO/imf5dieMIMO.csv",sep = ",",header =T)
imf6dieMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/迭代MIMO/imf6dieMIMO.csv",sep = ",",header =T)

imf1ZMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接MIMO/imf1ZMIMO.csv",sep = ",",header =T)
imf2ZMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接MIMO/imf2ZMIMO.csv",sep = ",",header =T)
imf3ZMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接MIMO/imf3ZMIMO.csv",sep = ",",header =T)
imf4ZMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接MIMO/imf4ZMIMO.csv",sep = ",",header =T)
imf5ZMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接MIMO/imf5ZMIMO.csv",sep = ",",header =T)
imf6ZMIMO=read.csv("C:/Users/Administrator/Desktop/data/电力负荷预测/多步结果/直接MIMO/imf6ZMIMO.csv",sep = ",",header =T)
library(Metrics)  
create_dataset=function(dataset,look_back){
  k=length(dataset)
  datax=matrix(nrow=k-look_back-23,ncol=look_back)
  for(i in 1:ncol(datax)){
    datax[,i]=dataset[i:(k-look_back+i-24)]
  }
  dataY=matrix(nrow=k-look_back-23,ncol=24)
  for(i in 1:ncol(dataY)){
    dataY[,i]=dataset[(look_back+i):(k-24+i)]
  }
  return(list(datax=datax,dataY=dataY))
}
look_back=168
f<-function(imf){
  testxY1=create_dataset(imf[721:1008],look_back)#12月1号
  testx=testxY1$datax
  testy=testxY1$dataY
  return(testy)
}
imf1testy=f(imf1)
imf2testy=f(imf2)
imf3testy=f(imf3)
imf4testy=f(imf4)
imf5testy=f(imf5)
imf6testy=f(imf6)
mean((imf1testy-IMF1tz)^2)
mean((imf2testy-IMF2tz)^2)
mean((imf3testy-IMF3tz)^2)
mean((imf4testy-IMF4tz)^2)
mean((imf5testy-IMF5tz)^2)
mean((imf6testy-IMF6tz)^2)
IMF2t=TG(imf2)
s=IMF1t
#########################################
IMFz=(IMF1z+IMF2z+IMF3z+IMF4z+IMF5z+IMF6z)*(max-min)+min
IMFt=(IMF1t+IMF2t+IMF3t+IMF4t+IMF5t+IMF6t)*(max-min)+min
IMFtz=(IMF1tz+IMF2tz+IMF3tz+IMF4tz+IMF5tz+IMF6tz)*(max-min)+min
imfMIMO=(imf1MIMO+imf2MIMO+imf3MIMO+imf4MIMO+imf5MIMO+imf6MIMO)*(max-min)+min
imfdieMIMO=(imf1dieMIMO+imf2dieMIMO+imf3dieMIMO+imf4dieMIMO+imf5dieMIMO+imf6dieMIMO)*(max-min)+min
imfZMIMO=(imf1ZMIMO+imf2ZMIMO+imf3ZMIMO+imf4ZMIMO+imf5ZMIMO+imf6ZMIMO)*(max-min)+min
z=(imf1dieMIMO+IMF2t+IMF3tz+imf4MIMO+imf5ZMIMO+IMF6z)*(max-min)+min

library(plotrix)
par(mfrow= c(1,1))
oldpar<-taylor.diagram(as.vector(testzy),as.vector(as.matrix(z)),sd.arcs=1,ylab="SStandard deviation",cex.axis=1.5)
taylor.diagram(as.vector(testzy),as.vector(as.matrix(IMFz)),add=TRUE,col=1)
taylor.diagram(as.vector(testzy),as.vector(as.matrix(IMFt)),add=TRUE,col=2)
taylor.diagram(as.vector(testzy),as.vector(as.matrix(IMFtz)),add=TRUE,col=3)
taylor.diagram(as.vector(testzy),as.vector(as.matrix(imfMIMO)),add=TRUE,col=4)
taylor.diagram(as.vector(testzy),as.vector(as.matrix(imfdieMIMO)),add=TRUE,col=5)
taylor.diagram(as.vector(testzy),as.vector(as.matrix(imfZMIMO)),add=TRUE,col=6)
lpos<-1.25*sd(as.vector(testzy))
legend(lpos,1.9*sd(as.vector(testzy)),cex=1,text.width=1500,bty="n",
       legend=c("The proposed","SVR","ANN","PSO-SVR","EMD-SVR","VMD-SVR","VMD-mRMR-SVR","VMD-tsPSO-SVR"),pch=19,col=c("red",1,2,3,4,5,6,7))
###########################
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(z)))
mode1=transform(d,model="The proposed")
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(IMFz)))
mode11=transform(d,model="Dir")
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(IMFt)))
mode12=transform(d,model="Rec")
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(IMFtz)))
mode13=transform(d,model="DirRec")
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(imfMIMO)))
mode14=transform(d,model="MIMO")
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(imfdieMIMO)))
mode15=transform(d,model="RecMO")
d=data.frame(obs=as.vector(testzy),mod=as.vector(as.matrix(imfZMIMO)))
mode16=transform(d,model="DirMO")

modell=rbind(mode1,mode11,mode12,mode13,mode14,mode15,mode16)
TaylorDiagram(modell, obs = "obs", mod = "mod", group = "model")
