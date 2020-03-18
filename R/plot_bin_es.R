####Do the U-shape convergence curve

plot_bin_es<-function(alloutputs,top_pm,lgndcol=2,legend=FALSE,estitle,esyrange=NULL,saveesfile,espicdim)
{
  require(ggplot2)
  results<-alloutputs$alldist
  selname0<-alloutputs$selname
  #top_pm<-length(selname)
  
  if(top_pm>length(selname0))
  {
    stop("top_pm should be smaller than the number of observations.")
  }

  ########plot of effectsize
  df<-as.data.frame(results$esdist)
  N0<-max(as.numeric(as.character(df$dif)))
  ###subset data
  N<-length(unique(df$dif))
  df$dif<-factor(df$dif,levels=as.character(0:(N-1)))
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
  
  if(sum(is.na(df)))
  {
    stop("Missing values exist in esdist")
  }
  else
  {
    df$dif<-as.numeric(as.character(df[,"dif"]))/N0
    df$median<-as.numeric(as.character(df[,"median"]))
    df$q1<-as.numeric(as.character(df[,"q1"]))
    df$q2<-as.numeric(as.character(df[,"q2"]))

    # df$value<--log10(as.numeric(pvmean[,"value"]))
    # df$std1<--log10(as.numeric(pvmean[,"value"])+1.96*as.numeric(pvmean[,"std"]))-df$value
    # df$std2<--log10(as.numeric(pvmean[,"value"])-1.96*as.numeric(pvmean[,"std"]))+df$value
    # df$std<--log10(as.numeric(pvmean[,"std"]))
    df$variable<-factor(df$variable,levels=selname)
    if(is.null(esyrange))
    {
      y_range<-range(min(df$q1)-0.1,max(df$q2)+0.1)
    }else{
      y_range<-esyrange
      }
    p<-ggplot(data=df,
           aes(x=dif, y=median,colour=variable,group=variable)) +
      geom_line()+
      geom_point()+
      geom_pointrange(aes(ymin=q1, ymax=q2))+
      #  geom_errorbar(aes(ymin=value, ymax=value+std1))+
      ylim(y_range)+
      xlab("Proportion of exchanges") +
      ylab(paste("Effect sizes of top", paste(top_pm, "important hits", sep=" "), sep=" "))  +
      ggtitle(estitle) +
      theme_bw()+
      theme(plot.title = element_text(size = 30, face = "bold"),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1),
            axis.text=element_text(size=16,face="bold"),
            axis.title=element_text(size=20,face="bold"))
      if(isFALSE(legend))
      {
        p<-p+theme(legend.position="none")
      }else{
        p<-p+
        guides(col=guide_legend(ncol=lgndcol))+
        theme(legend.title=element_text(size=14),
            legend.text=element_text(size=10))
      }   
    ggsave(filename=saveesfile,plot=p,width = espicdim[1], height = espicdim[2])
  }
  return(p)
}