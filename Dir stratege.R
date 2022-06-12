
#IMF1多步预测
ZJ<-function(imf){
  IMF1=c()
  trainxY1=create_dataset(imf[1:671],look_back)
  trainy=trainxY1$dataY
  trainx=trainxY1$datax
  validxY1=create_dataset(imf[481:911],look_back)
  validy=validxY1$dataY
  validx=validxY1$datax
  testxY1=create_dataset(imf[721:1008],look_back)#12月1号
  testy=testxY1$dataY
  testx=testxY1$datax
  #write.csv(trainx,file = "trainx.csv",row.names = F)
  for (i in 1:24) {#i=1代表第一步
    ge=mRMR.data(data = data.frame(trainy[,i],trainx))
    exect=system.time(fs <- new("mRMRe.Filter", data = ge,
                                target_indices = 1, levels = c(1,1,1,1, 1, 1, 1, 1)))
    trainx1=trainxY1$datax[,c(solutions(fs)[[1]]-1)]#特征选择
    trainX=data.frame(a =trainx1[,1], b =trainx1[,2],c=trainx1[,3],
                      d =trainx1[,4],e=trainx1[,5],f =trainx1[,6],
                      g =trainx1[,7],h =trainx1[,8])
    validx1=validxY1$datax[,c(solutions(fs)[[1]]-1)]#特征选择
    validx=data.frame(a =validx1[,1], b =validx1[,2],c=validx1[,3],
                      d =validx1[,4],e=validx1[,5],f =validx1[,6],
                      g =validx1[,7],h =validx1[,8])
    tesx1=testxY1$datax[,c(solutions(fs)[[1]]-1)]
    testx=data.frame(a =tesx1[,1], b =tesx1[,2],c=tesx1[,3],
                     d =tesx1[,4],e=tesx1[,5],f =tesx1[,6],
                     g =tesx1[,7],h =tesx1[,8])
    #第一步
    fit=function(x) {#适应度函数
      model=svm(trainX,trainy[,i],cost = x[1], gamma =x[2], type = "eps-regression", kernel = "radial")
      mape=mean((predict(model,validx)-validxY1$dataY[,i])^2)
      return (mape)
    }
    ps=psoptim(par = rep(NA,2),function(x) fit(x),
               lower = c(0.01,0.001),upper = c(1000,1),control = list(maxit=100))
    svr1=svm(trainX,trainy[,i],cost =ps$par[1], gamma= ps$par[2], type = "eps-regression", kernel = "radial")
    IMF1=cbind(IMF1,predict (svr1, testx))
    print(i)
  }
  return(IMF1)
}
