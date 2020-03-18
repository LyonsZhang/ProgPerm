####Do the U-shape convergence curve

plot_bin_permute_all<-function(alloutputs,top_pm,lgndcol=2,legend=FALSE,psigtitle,psigyrange=NULL,savepsigfile,psigpicdim,pvtitle,pvyrange=NULL,savepvfile,pvpicdim,estitle,esyrange=NULL,saveesfile,espicdim)
{
  require(ggplot2)
  results<-alloutputs$alldist
  selname0<-alloutputs$selname
  #top_pm<-length(selname)
  
  if(top_pm>length(selname0))
  {
    stop("top_pm should be smaller than the number of observations.")
  }


  ########plot of number of significant features
  df<-as.data.frame(results$psigdist)
  df$highlight <- ifelse(df$dif == 0, "real data", "permutation")
  df$highlight<- factor(df$highlight,levels=c("real data", "permutation"))
  mycolours<-c("permutation" = "black", "real data" = "red")
  N0<-max(df$dif)
  if(sum(is.na(df)))
  {
    stop("Missing values exist in psigdist")
  }
  else
  {
    df$dif<-df$dif/N0
    if(is.null(psigyrange))
    {
      y_range<-range(floor(min(df$q1)),ceiling(max(df$q2)))
    }else{
      y_range<-psigyrange
    }
    ggplot(df, aes(x=dif, y=median)) +
      geom_point(size = 5, aes(colour = highlight))+
      scale_color_manual("Scenarios", values = mycolours)+
      geom_pointrange(aes(ymin=q1, ymax=q2,colour=highlight),size=0.5)+
      geom_line(size = 1)+
      xlab("Proportion of exchanges") +
      ylab("Number of significant features") +
      ggtitle(psigtitle) +
      ylim(y_range)+
      theme_bw()+
      theme(plot.title = element_text(size = 30, face = "bold"),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16))+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.line.x = element_line(color="black", size = 1.5),
            axis.line.y = element_line(color="black", size = 1.5),
            axis.text=element_text(size=16,face="bold"),
            axis.title=element_text(size=20,face="bold"))
    ggsave(filename=savepsigfile,width = psigpicdim[1], height = psigpicdim[2])
  }


  ########plot of p-value sequences
  df<-as.data.frame(results$pvdist)
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
    stop("Missing values exist in pvdist")
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
    if(is.null(pvyrange))
    {
      y_range<-range(floor(min(df$q1)),ceiling(max(df$q2)))
    }else{
      y_range<-pvyrange
    } 
    p<-ggplot(data=df,
           aes(x=dif, y=median,colour=variable,group=variable)) +
      geom_line()+
      geom_point()+
      geom_pointrange(aes(ymin=q1, ymax=q2))+
      #  geom_errorbar(aes(ymin=value, ymax=value+std1))+
      ylim(y_range)+
      xlab("Proportion of exchanges") +
      ylab(paste("Log P-values of top", paste(top_pm, "important features", sep=" "), sep=" "))  +
      ggtitle(pvtitle) +
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
      if(!legend)
      {
        p<-p+theme(legend.position="none")
      }else{
        p<-p+
        guides(col=guide_legend(ncol=lgndcol))+
        theme(legend.title=element_text(size=14),
            legend.text=element_text(size=10))
      }   
    ggsave(filename=savepvfile,,plot=p,width = pvpicdim[1], height = pvpicdim[2])
  }

  ########plot of effectsize
  df<-as.data.frame(results$esdist)
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
      ylab(paste("Effect sizes of top", paste(top_pm, "important features", sep=" "), sep=" "))  +
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
      if(!legend)
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
}
