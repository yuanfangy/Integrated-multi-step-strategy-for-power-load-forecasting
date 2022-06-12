a=read.csv("C:/Users/Administrator/Desktop/data/CaliforniaElectricityMarketData.csv",sep = ",",header =F)[,1:4]
#归一化
max=max(a[,4])
min=min(a[,4])
a[,4]=(a[,4]-min)/(max-min)

#变分模态分解
library(vmd)
signal=a[,4]
y=6
al=100
v=vmd(signal,alpha=al,DC=FALSE,tol=1e-3,K=y)
l = v$getResult() 
imf1=l$u[,1]
imf2=l$u[,2] 
imf3=l$u[,3]
imf4=l$u[,4]
imf5=l$u[,5]
imf6=l$u[,6]

###########################################
#计算拐点
gedian=function(imf1){
  sum=0
  i=2
  while(i<(length(imf1)-1)){
    if(((imf1[i]-imf1[i-1])*(imf1[i+1]-imf1[i]))<=0)
      {
        sum=sum+1
    }
    i=i+1
  }
  return(sum)
}
gedian(imf5) 
library(tseries)#平稳???
library(fUnitRoots)
library(forecast)#ndiffs(x,test="adf")
for (i in 1:6) {
  print(adfTest(l$u[,i]))
}
for (i in 1:6) {
  print(pp.test(l$u[,i]))
}
write.csv(IMF2t,file = "IMF2t.csv",row.names = F)
write.csv(imf1,file = "imf1.csv",row.names = F)
write.csv(imf2,file = "imf2.csv",row.names = F)
write.csv(imf3,file = "imf3.csv",row.names = F)
write.csv(imf4,file = "imf4.csv",row.names = F)
write.csv(imf5,file = "imf5.csv",row.names = F)
write.csv(imf6,file = "imf6.csv",row.names = F)
look_back=168

#最大相关最小冗余
library("mRMRe")
library(e1071)
library(pso)
#递归直接策略
IMF1tz=TZ(imf1)
IMF2tz=TZ(imf2)
IMF3tz=TZ(imf3)
IMF4tz=TZ(imf4)
IMF5tz=TZ(imf5)
IMF6tz=TZ(imf6)
#递归策略
IMF1t=TG(imf1)
IMF2t=TG(imf2)
IMF3t=TG(imf3)
IMF4t=TG(imf4)
IMF5t=TG(imf5)
IMF6t=TG(imf6)

imf2testy=create_dataset(imf2[721:1008],168)$dataY

mean((imf2testy-IMF2t)^2)
#直接策略
IMF1z=ZJ(imf1)
IMF2z=ZJ(imf2)
IMF3z=ZJ(imf3)
IMF4z=ZJ(imf4)
IMF5z=ZJ(imf5)
IMF6z=ZJ(imf6)
write.csv(IMF4z,file = "IMF4z.csv",row.names = F)
write.csv(IMF5z,file = "IMF5z.csv",row.names = F)
write.csv(IMF6z,file = "IMF6z.csv",row.names = F)
write.csv(IMF1t,file = "IMF1t.csv",row.names = F)
write.csv(IMF2t,file = "IMF2t.csv",row.names = F)
write.csv(IMF3t,file = "IMF3t.csv",row.names = F)
write.csv(IMF4t,file = "IMF4t.csv",row.names = F)
write.csv(IMF5t,file = "IMF5t.csv",row.names = F)
write.csv(IMF6t,file = "IMF6t.csv",row.names = F)
write.csv(IMF1tz,file = "IMF1tz.csv",row.names = F)
write.csv(IMF2tz,file = "IMF2tz.csv",row.names = F)
write.csv(IMF3tz,file = "IMF3tz.csv",row.names = F)
write.csv(IMF4tz,file = "IMF4tz.csv",row.names = F)
write.csv(IMF5tz,file = "IMF5tz.csv",row.names = F)
write.csv(IMF6tz,file = "IMF6tz.csv",row.names = F)
