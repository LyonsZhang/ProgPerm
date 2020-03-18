con_true_initial<-function(variable,testdata,top_pm=50,method="kendall",goodtype="goodpvname")
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
  testlevel<-testdata
  testlevel[[variable]]<-NULL
  # position<-1:nrow(testdata)
  # position0<-position[testdata[[variable]]==0]
  # position1<-position[testdata[[variable]]==1]

  n<-dim(testdata)[1]  ###length of response variable
  #zoomn<-zoom_n  ###choose samples from each purmutations

  ###initialize top m variables
  group<-testdata[[variable]]
  indx<-1:n
  indx0<-sample(indx,n)
  indx1<-indx[!indx %in% indx0]
  group[indx1]<-group[sample(indx1)]

  otu.test<-apply(testlevel,2,function(x){
    ct<-cor.test(x=x,y=group,method=method)
    return(data.frame(pv=ct$p.value,es=ct$statistic,cr=ct$estimate))
  })
  otu.test<-do.call(rbind,otu.test)
  #select<-order(otu.test$pv)[1:top_pm]
  select<-switch(goodtype,
                 goodpvname=order(otu.test$pv)[1:top_pm],
                 goodesname=order(abs(otu.test$es))[1:top_pm],
                 goodcrname=order(abs(otu.test$cr))[1:top_pm],
                 stop("goodtype must be one of the follows: 'goodpvname', 'goodesname' and 'goodcrname'. "))

  selname<-row.names(otu.test)[select]
  #selname<-sub(".[^.]+$", "", selname)
  return(list(select=select,selname=selname,testlevel=testlevel,n=n,otu.test=otu.test))
}
