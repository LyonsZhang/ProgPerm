####lowest point calculations
bin_permute_best<-function(variable,testdata,top_pm,zoomn,alpha=0.05,psigalpha=0.05,method="kruskal",goodtype="goodpvname",initial_fun=bin_true_initial,test_fun=bin_multitest,exchange_fun=bin_exchange,paral="yes")
{
  trueinitial<-initial_fun(variable=variable,testdata=testdata,top_pm=top_pm,method=method,goodtype=goodtype)
  select<-trueinitial$select
  selname<-trueinitial$selname
  position0<-trueinitial$position0
  position1<-trueinitial$position1
  levs<-trueinitial$levs
  n<-trueinitial$n
  N<-trueinitial$N
  testlevel<-trueinitial$testlevel
  psig0<-sum(! trueinitial$otu.test$pv>psigalpha,na.rm=T)
  pvalue0<--log10(trueinitial$otu.test$pv[select])
  #effectsize0<-trueinitial$otu.test$es-median(trueinitial$otu.test$es)
  effectsize0<-trueinitial$otu.test$es
  effectsize0<-effectsize0[select]

  #exchange_iter<-function(k){exchange_fun(k,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,position0=position0,position1=position1,alpha=alpha,paral=paral)}
  #require(doParallel)
  #ncores<-parallel::detectCores()
  #cl<-parallel::makeCluster(ncores)
  #doParallel::registerDoParallel(cl)
  #out<-{foreach(k=n) %dopar% exchange_iter(k)}
  out<-exchange_fun(k=n,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,levs=levs,position0=position0,position1=position1,alpha=alpha,psigalpha=psigalpha,method=method,test_fun=test_fun,paral=paral)
  #parallel::stopCluster(cl)
  #out<-lapply(purrr::transpose(out), function(l) do.call(rbind, l))
  goodpsigname<-selname[!(out[["psigdist"]][,"q1"]<=psig0 & psig0<=out[["psigdist"]][,"q2"])]
  goodpvname<-selname[!(out[["pvdist"]][,"q1"]<=pvalue0 & pvalue0<=out[["pvdist"]][,"q2"])]
  goodesname<-selname[!(out[["esdist"]][,"q1"]<=effectsize0 & effectsize0<=out[["esdist"]][,"q2"])]
  return(list(psig0=psig0,pvalue0=pvalue0,effectsize0=effectsize0,selname=selname,worst_case=out,goodpsigname=goodpsigname,goodpvname=goodpvname,goodesname=goodesname))
}
