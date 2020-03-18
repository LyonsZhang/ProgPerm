###all the permutation for binary categorical response
con_permute_all<-function(variable,testdata,top_pm,zoomn,sequence=NULL,method="kendall",goodtype="goodpvname",alpha=0.05,psigalpha=0.05,initial_fun=con_true_initial,test_fun=con_multitest,exchange_fun=con_exchange,paral="no")
{
  trueinitial<-initial_fun(variable=variable,testdata=testdata,top_pm=top_pm,method=method,goodtype=goodtype)
  select<-trueinitial$select
  selname<-trueinitial$selname
  n<-trueinitial$n
  testlevel<-trueinitial$testlevel

  if(is.null(sequence))
  {
    sequence<-round(seq(0, n, length.out=10))
  }
  #exchange_iter<-function(k){FUN(k,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,method=method,alpha=alpha,paral=paral)}
  require(doParallel)
  ncores<-parallel::detectCores()
  cl<-parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  out<-{foreach(k=sequence) %dopar% exchange_fun(k,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,alpha=alpha,psigalpha=psigalpha,method=method,test_fun=test_fun,paral=paral)}
  parallel::stopCluster(cl)
  #out<-lapply(purrr::transpose(out), function(l) do.call(rbind, l))
  out<-do.call(Map, c(f = rbind, out))
  return(list(alldist=out,selname=selname))
}
