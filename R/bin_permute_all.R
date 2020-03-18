###all the permutation for binary categorical response
bin_permute_all<-function(variable,testdata,top_pm,zoomn,sequen=NULL,alpha=0.05,psigalpha=0.05,method="kruskal",goodtype="goodpvname",initial_fun=bin_true_initial,test_fun=bin_multitest,exchange_fun=bin_exchange,paral="no")
{
  #variable="racen";testdata=testdata[,-1];top_pm=50;zoomn=15;alpha=0.05
  #sequen=NULL;alpha=0.05;psigalpha=0.05;method="kruskal";initial_fun=bin_true_initial;test_fun=bin_multitest;exchange_fun=bin_exchange;paral="no";
  trueinitial<-initial_fun(variable=variable,testdata=testdata,top_pm=top_pm,method=method,goodtype=goodtype)
  select<-trueinitial$select
  selname<-trueinitial$selname
  position0<-trueinitial$position0
  position1<-trueinitial$position1
  levs<-trueinitial$levs
  n<-trueinitial$n
  N<-trueinitial$N
  testlevel<-trueinitial$testlevel
  if(is.null(sequen))
  {
    sequen<-seq(0, N-1, 1)
  }
  #exchange_iter<-function(k){FUN(k,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,position0=position0,position1=position1,alpha=alpha,test_fun=testfun,paral=paral)}
  require(doParallel)
  ncores<-parallel::detectCores()
  cl<-parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  out<-{foreach(k=sequen) %dopar% exchange_fun(k,top_pm=top_pm,n=n,zoomn=zoomn,variable=variable,select=select,selname=selname,testdata=testdata,testlevel=testlevel,levs=levs,position0=position0,position1=position1,alpha=alpha,psigalpha=psigalpha,method=method,test_fun=test_fun,paral=paral)}
  parallel::stopCluster(cl)
  out<-do.call(Map, c(f = rbind, out))
  #out<-lapply(purrr::transpose(out), function(l) do.call(rbind, l))
  return(list(alldist=out,selname=selname))
}
