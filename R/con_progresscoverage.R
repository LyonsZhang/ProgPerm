con_progresscoverage<-function(alloutputs,top_pm,lgndcol=2,pvtitle,savepvfile,pvpicdim,estitle,saveesfile,espicdim,crtitle,savecrfile,crpicdim)
{
  library(ggplot2)
  results<-alloutputs$alldist
  selname0<-alloutputs$selname
  #top_pm<-length(selname)

    if(top_pm>length(selname0))
  {
    stop("top_pm should be smaller than the number of observations.")
  }
  
  ###p-values#####
  df<-as.data.frame(results$psigdist)
  N0<-length(unique(df$dif))-1
  trueorg<-df[df$dif==0,"median"]
  nsig_prop<- sum(df$q1[-1]<=trueorg & trueorg<=df$q2[-1])/N0
  
  ###p-values#####
  df<-as.data.frame(results$pvdist)

  N<-length(unique(df$dif))
  df$dif<-factor(df$dif,levels=sort(unique(df$dif)))
  
  ###subset data
  selname<-selname0[1:top_pm]
  if(top_pm<length(selname0))
  {
    tt<-split(df, df$dif)
    ss<-lapply(tt,function(x){
      return(x[1:top_pm,])
    })
    df<-do.call("rbind", ss)
  }
  

  trueorg<-as.numeric(as.character(df[df$dif==0,"median"]))
  N<-length(unique(df$dif))
  temp<-unique(df$dif)
  temp<-temp[-1]
  #pv_cover1<-matrix(0,N,top_pm)
  #colnames(pv_cover1)<-selname
  pv_cover<-matrix(0,N-1,top_pm)
  colnames(pv_cover)<-selname
  for(i in 1:(N-1))
  {
    #pv_cover1[i,]<-min(as.numeric(as.character(df[df$dif==i,"q1"])))<=trueorg & trueorg<= max(as.numeric(as.character(df[df$dif==i,"q2"])))
    pv_cover[i,]<-as.numeric(as.character(df[df$dif==temp[i],"q1"]))<=trueorg & trueorg<= as.numeric(as.character(df[df$dif==temp[i],"q2"]))
  }
  # pv_prop1<-colSums(pv_cover1)/N
  # df<-data.frame(pv_prop1)
  # df$variable<-selname
  # df<-df[order(df$pv_prop1),] 
  # df$variable<-factor(df$variable,levels=df$variable)
  # ggplot(data=df, aes(x=variable,y=1-pv_prop1,fill=variable))+
  #   geom_bar(stat="identity")
  pv_prop<-colSums(pv_cover)/N0
  df<-data.frame(pv_prop)
  df$variable<-selname
  df<-df[order(df$pv_prop),] 
  df$variable<-factor(df$variable,levels=df$variable)
  ggplot(data=df, aes(x=variable,y=1-pv_prop,fill=variable))+
    geom_bar(stat="identity")+
    ylim(0, 1)+
    ylab("Proportion of noncoverage") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    guides(fill=guide_legend(ncol=lgndcol))+
    #ggtitle(pvtitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold"),
          legend.title=element_text(size=20),
          legend.text=element_text(size=12))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.ticks.length =unit(10,"mm"),
      #axis.ticks.y = element_line(size=0.5),
      axis.line = element_line(colour = "black"),
      #panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1),
      axis.text=element_text(size=16,face="bold"),
      axis.title=element_text(size=20,face="bold"))
  ggsave(filename=savepvfile,width = pvpicdim[1], height = pvpicdim[2])
  
  ###effectsizes#####
  df<-as.data.frame(results$esdist)

  N<-length(unique(df$dif))
  df$dif<-factor(df$dif,levels=sort(unique(df$dif)))
  
  ###subset data
  selname<-selname0[1:top_pm]
  if(top_pm<length(selname0))
  {
    tt<-split(df, df$dif)
    ss<-lapply(tt,function(x){
      return(x[1:top_pm,])
    })
    df<-do.call("rbind", ss)
  }

  trueorg<-as.numeric(as.character(df[df$dif==0,"median"]))
  #N<-max(as.numeric(as.character(df$dif)))
  #es_cover1<-matrix(0,N,top_pm)
  #colnames(es_cover1)<-selname
  es_cover<-matrix(0,N-1,top_pm)
  colnames(es_cover)<-selname
  for(i in 1:(N-1))
  {
    #es_cover1[i,]<-min(as.numeric(as.character(df[df$dif==i,"q1"])))<=trueorg & trueorg<= max(as.numeric(as.character(df[df$dif==i,"q2"])))
    es_cover[i,]<-as.numeric(as.character(df[df$dif==temp[i],"q1"]))<=trueorg & trueorg<= as.numeric(as.character(df[df$dif==temp[i],"q2"]))
  }
  #es_prop1<-colSums(es_cover1)/N
  es_prop<-colSums(es_cover)/N0
  df<-data.frame(es_prop)
  df$variable<-selname
  df<-df[order(df$es_prop),] 
  df$variable<-factor(df$variable,levels=df$variable)
  ggplot(data=df, aes(x=variable,y=1-es_prop,fill=variable))+
    geom_bar(stat="identity")+
    ylim(0, 1)+
    ylab("Proportion of noncoverage") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    guides(fill=guide_legend(ncol=lgndcol))+
    ggtitle(estitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold"),
          legend.title=element_text(size=20),
          legend.text=element_text(size=12))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.ticks.length =unit(10,"mm"),
      #axis.ticks.y = element_line(size=0.5),
      axis.line = element_line(colour = "black"),
      #panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1),
      axis.text=element_text(size=16,face="bold"),
      axis.title=element_text(size=20,face="bold"))
  ggsave(filename=saveesfile,width = espicdim[1], height = espicdim[2])
  
  
  ###correlations#####
  df<-as.data.frame(results$crdist)

  N<-length(unique(df$dif))
  df$dif<-factor(df$dif,levels=sort(unique(df$dif)))
  
  ###subset data
  selname<-selname0[1:top_pm]
  if(top_pm<length(selname0))
  {
    tt<-split(df, df$dif)
    ss<-lapply(tt,function(x){
      return(x[1:top_pm,])
    })
    df<-do.call("rbind", ss)
  }

  trueorg<-as.numeric(as.character(df[df$dif==0,"median"]))
  #N<-max(as.numeric(as.character(df$dif)))
  #es_cover1<-matrix(0,N,top_pm)
  #colnames(es_cover1)<-selname
  cr_cover<-matrix(0,N-1,top_pm)
  colnames(cr_cover)<-selname
  for(i in 1:(N-1))
  {
    #es_cover1[i,]<-min(as.numeric(as.character(df[df$dif==i,"q1"])))<=trueorg & trueorg<= max(as.numeric(as.character(df[df$dif==i,"q2"])))
    cr_cover[i,]<-as.numeric(as.character(df[df$dif==temp[i],"q1"]))<=trueorg & trueorg<= as.numeric(as.character(df[df$dif==temp[i],"q2"]))
  }
  #es_prop1<-colSums(es_cover1)/N
  cr_prop<-colSums(cr_cover)/N0
  df<-data.frame(cr_prop)
  df$variable<-selname
  df<-df[order(df$cr_prop),] 
  df$variable<-factor(df$variable,levels=df$variable)
  ggplot(data=df, aes(x=variable,y=1-cr_prop,fill=variable))+
    geom_bar(stat="identity")+
    ylim(0, 1)+
    ylab("Proportion of noncoverage") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    guides(fill=guide_legend(ncol=2))+
    ggtitle(crtitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold"),
          legend.title=element_text(size=20),
          legend.text=element_text(size=12))+
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.ticks.length =unit(10,"mm"),
      #axis.ticks.y = element_line(size=0.5),
      axis.line = element_line(colour = "black"),
      #panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1),
      axis.text=element_text(size=16,face="bold"),
      axis.title=element_text(size=20,face="bold"))
  ggsave(filename=savecrfile,width = crpicdim[1], height = crpicdim[2])
  return(list(nsig_prop=nsig_prop,pv_prop=pv_prop,es_prop=es_prop,cr_prop=cr_prop))
}