####lowest point calculations
con_permute_best<-function(variable,testdata,top_pm,zoomn,alpha=0.05,psigalpha=0.05,method="kendall",goodtype="goodpvname",initial_fun=con_true_initial,test_fun=con_multitest,exchange_fun=con_exchange,paral="yes")
{
  # group1<-grep('0',testdata[[variable]])
  # group2<-grep('1',testdata[[variable]])
  # fliplen<-min(length(group1),length(group2))
  # N<-fliplen+1
  # n1<-length(group1) #number of group1
  # n2<-length(group2) #number of group2
  # print("number of group1 is")
  # print(n1)
  # print("number of group2 is")
  # print(n2)
  trueinitial<-initial_fun(variable=variable,testdata=testdata,top_pm=top_pm,method=method,goodtype=goodtype)
  select<-trueinitial$select
  selname<-trueinitial$selname
  n<-trueinitial$n
  testlevel<-trueinitial$testlevel

  psig0<-sum(! trueinitial$otu.test$pv>psigalpha,na.rm=T)
  pvalue0<--log10(trueinitial$otu.test$pv[select])
  effectsize0<-trueinitial$otu.test$es[select]
  correlation0<-trueinitial$otu.test$cr[select]

  #exchange_iter<-function(k){FUN(k,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,method=method,alpha=alpha,paral=paral)}
  #require(doParallel)
  #ncores<-parallel::detectCores()
  #cl<-parallel::makeCluster(ncores)
  #doParallel::registerDoParallel(cl)
  #out<-{foreach(k=n) %dopar% exchange_iter(k)}
  out<-exchange_fun(k=n,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,alpha=alpha,psigalpha=psigalpha,method=method,test_fun=test_fun,paral=paral)
#require(doParallel)
  #parallel::stopCluster(cl)
  #out<-lapply(purrr::transpose(out), function(l) do.call(rbind, l))
  goodpsigname<-selname[!(out[["psigdist"]][,"q1"]<=psig0 & psig0<=out[["psigdist"]][,"q2"])]
  goodpvname<-selname[!(out[["pvdist"]][,"q1"]<=pvalue0 & pvalue0<=out[["pvdist"]][,"q2"])]
  goodesname<-selname[!(out[["esdist"]][,"q1"]<=effectsize0 & effectsize0<=out[["esdist"]][,"q2"])]
  goodcrname<-selname[!(out[["crdist"]][,"q1"]<=correlation0 & correlation0<=out[["crdist"]][,"q2"])]

  return(list(psig0=psig0,pvalue0=pvalue0,effectsize0=effectsize0,correlation0=correlation0,selname=selname,worst_case=out,goodpsigname=goodpsigname,goodpvname=goodpvname,goodesname=goodesname,goodcrname=goodcrname))
}
