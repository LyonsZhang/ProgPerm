con_multitest<-function(i,k,n,variable,select,testdata,testlevel,method,psigalpha=0.05)
{
  group<-testdata[[variable]]
  #n<-length(group)
  indx<-1:n
  indx0<-sample(indx,n-k)
  indx1<-indx[!indx %in% indx0]
  group[indx1]<-group[sample(indx1)]

  otu.test<-apply(testlevel,2,function(x){
    ct<-cor.test(x=x,y=group,method=method)
    return(data.frame(pv=ct$p.value,es=ct$statistic,cr=ct$estimate))
  })
  otu.test<-do.call(rbind,otu.test)
  #pvalue[,i]<-sort(otu.mk$pv)[1:10]
  psig<-sum(! otu.test$pv>psigalpha,na.rm=T)
  pvalue<--log10(otu.test$pv[select])
  effectsize<-otu.test$es[select]
  correlation<-otu.test$cr[select]
  return(list(psig=psig,pvalue=pvalue,effectsize=effectsize,correlation=correlation))
}
