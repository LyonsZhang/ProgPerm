plot_bin_pv_best<-function(bestoutputs,top_pm,pvtitle,savepvfile,pvpicdim)
{
  worstpermute<-bestoutputs$worst_case
  #top_pm<-length(bestoutputs$selname)

  ##plot of p values
  df1<-data.frame(matrix(0,top_pm,4))
  rownames(df1)<-bestoutputs$selname[1:top_pm]
  colnames(df1)<-c("trueorg","median","q1","q2")
  df1$variable<-factor(rownames(df1),levels=rownames(df1))
  df1$trueorg<-bestoutputs$pvalue0[1:top_pm]
  df1$median<-as.numeric(as.character(worstpermute$pvdist[1:top_pm,"median"]))
  df1$q1<-as.numeric(as.character(worstpermute$pvdist[1:top_pm,"q1"]))
  df1$q2<-as.numeric(as.character(worstpermute$pvdist[1:top_pm,"q2"]))
  df1$coverage<-ifelse((df1$q1<=df1$trueorg & df1$trueorg<=df1$q2), "inside","outside")


  p<-ggplot(data=df1, aes(x=variable))+
    geom_errorbar(aes(ymax = q2, ymin = q1)) +
    geom_point(aes(y=median), shape=1, size=3) +
    geom_point(aes(y=trueorg,colour=coverage), shape=16, size=3) +
    ylab("-log10(p-value)") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    coord_flip()+
    ggtitle(pvtitle) +
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold"),
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
  ggsave(filename=savepvfile,plot=p,width = pvpicdim[1], height = pvpicdim[2])
  return(p)
}
