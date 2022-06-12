#IMF1多步预测
TG<-function(imf){
  IMF1=c()
  trainxY1=create_dataset(imf[1:671],look_back)
  trainx=trainxY1$datax
  trainy=trainxY1$dataY
  validxY1=create_dataset(imf[481:911],look_back)
  validx=validxY1$datax
  validy=validxY1$dataY
  testxY1=create_dataset(imf[721:1008],look_back)#12月1号
  testx=testxY1$datax
  testy=testxY1$dataY
 # trainx=rbind(trainx,validx)
 # trainy=rbind(trainy,validy)
  ge=mRMR.data(data = data.frame(trainy[,1],trainx))
  exect=system.time(fs <- new("mRMRe.Filter", data = ge,
                              target_indices = 1, levels = c(1,1,1,1,1,1,1,1)))
  trainx1=trainx[,c(solutions(fs)[[1]]-1)]#特征选择
  trainX=data.frame(a =trainx1[,1],b=trainx1[,2],c=trainx1[,3],
                    d =trainx1[,4],e=trainx1[,5],f=trainx1[,6],
                    g =trainx1[,7],h=trainx1[,8])
  validx1=validx[,c(solutions(fs)[[1]]-1)]#特征选择
  validX=data.frame(a =validx1[,1],b=validx1[,2],c=validx1[,3],
                    d =validx1[,4],e=validx1[,5],f=validx1[,6],
                    g =validx1[,7],h=validx1[,8])
  tesx1=testx[,c(solutions(fs)[[1]]-1)]
  testX=data.frame(a =tesx1[,1],b=tesx1[,2],c=tesx1[,3],
                   d =tesx1[,4],e=tesx1[,5],f=tesx1[,6],
                   g =tesx1[,7],h=tesx1[,8])
  #第一步
  fit=function(x) {#适应度函数
    model=svm(trainX,trainy[,1],cost = x[1], gamma =x[2], type = "eps-regression", kernel = "radial")
    mape=mean((predict(model,validX)-validy[,1])^2)
    return (mape)
  }
  #150.0841   0.0010 $value[1] 0.001680737
  ps=psoptim(par = rep(NA,2),function(x) fit(x),
             lower = c(0.01,0.0001),upper = c(1000,100),control = list(maxit=200))
  svr1=svm(trainX,trainy[,1],cost =ps$par[1], gamma= ps$par[2], type = "eps-regression", kernel = "radial")
  for (i in 1:24) {#i=1代表第一步
    pred=predict(svr1,testX)
    IMF1=cbind(IMF1,pred)
    testx=cbind(testx,pred)
    testx=testx[,-1]
    tesx1=testx[,c(solutions(fs)[[1]]-1)]
    testX=data.frame(a =tesx1[,1],b=tesx1[,2],c=tesx1[,3],
                     d =tesx1[,4],e=tesx1[,5],f=tesx1[,6],
                     g =tesx1[,7],h=tesx1[,8])
  }
  return(IMF1)
}
