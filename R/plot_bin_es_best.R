plot_bin_es_best<-function(bestoutputs,top_pm,estitle,saveesfile,espicdim)
{
  worstpermute<-bestoutputs$worst_case
  #top_pm<-length(bestoutputs$selname)
  
  ##plot of effect sizes
  df2<-data.frame(matrix(0,top_pm,4))
  rownames(df2)<-bestoutputs$selname[1:top_pm]
  colnames(df2)<-c("trueorg","median","q1","q2")
  df2$variable<-factor(rownames(df2),levels=rownames(df2))
  df2$trueorg<-bestoutputs$effectsize0[1:top_pm]
  df2$median<-as.numeric(as.character(worstpermute$esdist[1:top_pm,"median"]))
  df2$q1<-as.numeric(as.character(worstpermute$esdist[1:top_pm,"q1"]))
  df2$q2<-as.numeric(as.character(worstpermute$esdist[1:top_pm,"q2"]))
  df2$coverage<-ifelse((df2$q1<=df2$trueorg & df2$trueorg<=df2$q2), "inside","outside")


  p<-ggplot(data=df2, aes(x=variable))+
    geom_errorbar(aes(ymax = q2, ymin = q1)) +
    geom_point(aes(y=median), shape=1, size=3) +
    geom_point(aes(y=trueorg,colour=coverage), shape=16, size=3) +
    ylab("Effect size") +
    xlab(paste("Top",top_pm,"significant features with decreasing order",sep=" "))+
    coord_flip()+
    ggtitle(estitle) +
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
  ggsave(filename=saveesfile,plot=p,width = espicdim[1], height = espicdim[2])
  return(p)
}