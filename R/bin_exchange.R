####purmutation function
bin_exchange<-function(k,top_pm,n,zoomn,variable,select,selname,testdata,testlevel,levs,position0,position1,alpha,psigalpha,method,test_fun=bin_multitest,paral)
{
  #psig<-rep(0,(n-abs(k-n))*zoomn+1)
  #pvalue<-matrix(0,((n-abs(k-n))*zoomn+1),m)
  #for(i in 1:((n-abs(k-n))*zoomn+1))

  ###parallel computing
  if(paral=="no")
  {
    testresults<-lapply(1:((n-abs(k-n))*zoomn+1), test_fun,k=k,variable=variable,select=select,testdata=testdata,testlevel=testlevel,levs=levs,position0=position0,position1=position1,method=method,psigalpha=psigalpha)
  }
  else if(paral=="yes")
  {
    require(doParallel)
    ncores<-parallel::detectCores()
    cl<-parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    testresults<-foreach(i=1:((n-abs(k-n))*zoomn+1)) %dopar% test_fun(i,k=k,variable=variable,select=select,testdata=testdata,testlevel=testlevel,levs=levs,position0=position0,position1=position1,method=method,psigalpha=psigalpha)
    parallel::stopCluster(cl)
  }
  else
  {
    stop("Enter something that parallels me!")
  }
  testresults<-lapply(purrr::transpose(testresults), function(l) do.call(rbind, l))
  psig<-testresults[[1]]
  pvalue<-testresults[[2]]
  effectsize<-testresults[[3]]
  dif<-k
  #dif<-sum(abs(as.numeric(group)-as.numeric(testdata[[variable]])))/2
  ###number of significant features
  psigdist<-matrix(0,1,6)
  colnames(psigdist)<-c("mean","std","median","q1","q2","dif")
  psigdist[,1]<-mean(psig)
  if(length(psig)==1)
  {
    psigdist[,2]<-0
  }
  else
  {
    psigdist[,2]<-sd(psig)
  }
  psigdist[,3]<-median(psig)
  psigdist[,4]<-quantile(psig,probs=alpha/2)
  psigdist[,5]<-quantile(psig,probs=(1-alpha/2))
  psigdist[,6]<-dif
  ###top m pvalues
  pvdist<-matrix(0,top_pm,8)
  colnames(pvdist)<-c("mean","std","median","q1","q2","variable","dif","rep")
  pvdist[,1]<-colMeans(pvalue)
  if(dim(pvalue)[1]==1)
  {
    pvdist[,2]<-0
    pvdist[,8]<-pvalue
    # pvdist[,3]<-0
    # pvdist[,4]<-0
  }
  else
  {
    pvdist[,2]<-matrixStats::colSds(pvalue)
    pvdist[,8]<-pvalue[1,]
  }
  pvdist[,3]<-matrixStats::colMedians(pvalue)
  pvdist[,4]<-matrixStats::colQuantiles(pvalue,probs=alpha/2)
  pvdist[,5]<-matrixStats::colQuantiles(pvalue,probs=(1-alpha/2))
  pvdist[,6]<-selname
  pvdist[,7]<-dif
  ###top m pvalues
  esdist<-matrix(0,top_pm,7)
  colnames(esdist)<-c("mean","std","median","q1","q2","variable","dif")
  esdist[,1]<-colMeans(effectsize)
  if(dim(effectsize)[1]==1)
  {
    esdist[,2]<-0
    # esdist[,3]<-0
    # esdist[,4]<-0
  }
  else
  {
    esdist[,2]<-matrixStats::colSds(effectsize)
  }
  esdist[,3]<-matrixStats::colMedians(effectsize)
  esdist[,4]<-matrixStats::colQuantiles(effectsize,probs=alpha/2)
  esdist[,5]<-matrixStats::colQuantiles(effectsize,probs=(1-alpha/2))
  esdist[,6]<-selname
  esdist[,7]<-dif
  return(list(psigdist=psigdist,pvdist=pvdist,esdist=esdist))
}
