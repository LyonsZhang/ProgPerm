plot_con_permute_best<-function(bestoutputs,pvtitle,savepvfile,pvpicdim,estitle,saveesfile,espicdim,crtitle,savecrfile,crpicdim)
{
  worstpermute<-bestoutputs$worst_case
  top_pm<-length(bestoutputs$selname)

  ##plot of p values
  df1<-data.frame(matrix(0,top_pm,4))
  rownames(df1)<-bestoutputs$selname
  colnames(df1)<-c("trueorg","median","q1","q2")
  df1$variable<-factor(rownames(df1),levels=rownames(df1))
  df1$trueorg<-bestoutputs$pvalue0
  df1$median<-as.numeric(as.character(worstpermute$pvdist[,"median"]))
  df1$q1<-as.numeric(as.character(worstpermute$pvdist[,"q1"]))
  df1$q2<-as.numeric(as.character(worstpermute$pvdist[,"q2"]))
  df1$coverage<-ifelse((df1$q1<=df1$trueorg & df1$trueorg<=df1$q2), "inside","outside")


  ggplot(data=df1, aes(x=variable))+
    geom_errorbar(aes(ymax = q2, ymin = q1)) +
    geom_point(aes(y=median), shape=1, size=3) +
    geom_point(aes(y=trueorg,colour=coverage), shape=16, size=3) +
    ylab("-log10(p-value)") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    coord_flip()+
    ggtitle(pvtitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          legend.title=element_text(size=20),
          legend.text=element_text(size=16))+
    theme(
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

  ##plot of effect sizes
  df2<-data.frame(matrix(0,top_pm,4))
  rownames(df2)<-bestoutputs$selname
  colnames(df2)<-c("trueorg","median","q1","q2")
  df2$variable<-factor(rownames(df2),levels=rownames(df2))
  df2$trueorg<-bestoutputs$effectsize0
  df2$median<-as.numeric(as.character(worstpermute$esdist[,"median"]))
  df2$q1<-as.numeric(as.character(worstpermute$esdist[,"q1"]))
  df2$q2<-as.numeric(as.character(worstpermute$esdist[,"q2"]))
  df2$coverage<-ifelse((df2$q1<=df2$trueorg & df2$trueorg<=df2$q2), "inside","outside")


  ggplot(data=df2, aes(x=variable))+
    geom_errorbar(aes(ymax = q2, ymin = q1)) +
    geom_point(aes(y=median), shape=1, size=3) +
    geom_point(aes(y=trueorg,colour=coverage), shape=16, size=3) +
    ylab("Effect size") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    coord_flip()+
    ggtitle(estitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          legend.title=element_text(size=20),
          legend.text=element_text(size=16))+
    theme(
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

  ##plot of correlations
  df3<-data.frame(matrix(0,top_pm,4))
  rownames(df3)<-bestoutputs$selname
  colnames(df3)<-c("trueorg","median","q1","q2")
  df3$variable<-factor(rownames(df3),levels=rownames(df3))
  df3$trueorg<-bestoutputs$correlation0
  df3$median<-as.numeric(as.character(worstpermute$crdist[,"median"]))
  df3$q1<-as.numeric(as.character(worstpermute$crdist[,"q1"]))
  df3$q2<-as.numeric(as.character(worstpermute$crdist[,"q2"]))
  df3$coverage<-ifelse((df3$q1<=df3$trueorg & df3$trueorg<=df3$q2), "inside","outside")


  ggplot(data=df3, aes(x=variable))+
    geom_errorbar(aes(ymax = q2, ymin = q1)) +
    geom_point(aes(y=median), shape=1, size=3) +
    geom_point(aes(y=trueorg,colour=coverage), shape=16, size=3) +
    ylab("Correlation") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    coord_flip()+
    ggtitle(crtitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          legend.title=element_text(size=20),
          legend.text=element_text(size=16))+
    theme(
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
  return(list(pvtable=df1,estable=df2,crtable=df3))
}


