plot_con_permute_sigcurve<-function(alloutputs,samsize,lgndcol=2,psigtitle,savepsigfile,psigpicdim)
{
  require(ggplot2)
  results<-alloutputs$alldist
  selname0<-alloutputs$selname
  #top_pm<-length(selname)
  
  
  ########plot of number of significant features
  df<-as.data.frame(results$psigdist)
  df$highlight <- ifelse(df$dif == 0, "real data", "permutation")
  df$highlight<- factor(df$highlight,levels=c("real data", "permutation"))
  df$prop<-df$median/samsize
  mycolours<-c("permutation" = "black", "real data" = "red")
  N0<-max(df$dif)
  if(sum(is.na(df)))
  {
    stop("Missing values exist in psigdist")
  }
  else
  {
    df$dif<-df$dif/N0
    #y_range<-range(floor(min(df$q1)),ceiling(max(df$q2)))
    auc<-DescTools::AUC(df$dif, df$prop, method = "trapezoid", na.rm = FALSE)
    aoi<-mean(df$prop[c(1,length(df$prop))])
    slope<-diff(df$prop)/diff(df$dif)

    ggplot(df, aes(x=dif, y=prop)) +
      geom_point(size = 5, aes(colour = highlight))+
      scale_color_manual("Scenarios", values = mycolours)+
      geom_pointrange(aes(ymin=q1/samsize, ymax=q2/samsize,colour=highlight),size=0.5)+
      geom_line(size = 1)+
      geom_ribbon(aes(ymin=0, ymax=prop),alpha=0.2, fill = "purple")+
      geom_ribbon(aes(ymin=prop, ymax=prop[1]),alpha=0.2, fill = "green")+
      geom_ribbon(aes(ymin=prop[1], ymax=1),alpha=0.2, fill = "gold")+
      xlab("Proportion of exchanges") +
      ylab("Proportion of significant features") +
      ggtitle(psigtitle) +
      ylim(c(0,1))+
      annotate("text", x = 0.5, y = 0.9, label = paste("aoi=",round(aoi,3)),size=6, colour = "green")+
      annotate("text", x = 0.5, y = 0.8, label = paste("auc=",round(auc,3)),size=6, colour = "purple")+
      annotate("text", x = 0.5, y = 0.7, label = paste("slope=",round(slope[1],3)),size=6, colour = "red")+
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
    ggsave(filename=savepsigfile,width = psigpicdim[1], height = psigpicdim[2],device=cairo_ps)
  
    return(list(auc=auc,aoi=aoi,slope=slope,df=df))
  }
}