bin_true_initial<-function(variable,testdata,top_pm=50,method="kruskal",goodtype="goodpvname")
{
  response<-testdata[[variable]]
  levs<-unique(response)
  if(length(levs)!=2)
  {
    stop("The variable does not have two levels.")
  }
  else
  {
    position0<-grep(levs[1],response)
    position1<-grep(levs[2],response)
    fliplen<-min(length(position0),length(position1))
    N<-fliplen+1
    n1<-length(position0) #number of group1
    n2<-length(position1) #number of group2
    # print(paste(paste("number of factor level", levs[1],sep=" "),"is:"," "))
    # print(n1)
    # print(paste(paste("number of factor level", levs[2],sep=" "),"is:"," "))
    # print(n2)
    testlevel<-testdata
    #remove the response variable
    testlevel[[variable]]<-NULL
    # position<-1:nrow(testdata)
    # position0<-position[testdata[[variable]]==0]
    # position1<-position[testdata[[variable]]==1]

    #m<-top_pm ###top 50 pvalues
    n<-round((N+1)/2)  ###lowest purmutation number
    #zoomn<-zoom_n  ###choose samples from each purmutations

    ###initialize top m variables
    #group<-testdata[[variable]]
    group<-factor(response)
    if(method=="kruskal")
    {
      otu.test<-apply(testlevel,2,function(x){
        kt<-kruskal.test(x=x,g=group)
        return(data.frame(pv=kt$p.value,es=kt$statistic/sqrt(n1^2+n2^2)))
      })
      otu.test<-do.call(rbind,otu.test)
    }
    else if(method=="wilcox")
    {
      otu.test<-apply(testlevel,2,function(x){
        options(warn=-1)
        wt<-wilcox.test(x~group)
        return(data.frame(pv=wt$p.value,es=wt$statistic/sqrt(n1^2+n2^2)))
      })
      otu.test<-do.call(rbind,otu.test)
    }
    else{
      stop("Set up method either as 'kruskal' or 'wilcox'!")
    }
    
    select<-switch(goodtype,
                   goodpvname=order(otu.test$pv)[1:top_pm],
                   goodesname=order(abs(otu.test$es))[1:top_pm],
                   stop("goodtype must be one of the follows: 'goodpvname' and 'goodesname'. "))
    
    #select<-order(otu.test$pv)[1:top_pm]
    selname<-row.names(otu.test)[select]
    #print(selname[1:6])
    #selname<-sub(".[^.]+$", "", selname)
  }
  return(list(select=select,selname=selname,position0=position0,position1=position1,levs=levs,testlevel=testlevel,n=n,N=N,otu.test=otu.test))
}
