####Do the U-shape convergence curve

plot_bin_psig<-function(alloutputs,psigtitle,psigyrange=NULL,savepsigfile,psigpicdim)
{
  require(ggplot2)
  results<-alloutputs$alldist
  selname0<-alloutputs$selname
  
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
    p<-ggplot(df, aes(x=dif, y=median)) +
      geom_point(size = 5, aes(colour = highlight))+
      scale_color_manual("Scenarios", values = mycolours)+
      geom_pointrange(aes(ymin=q1, ymax=q2,colour=highlight),size=0.5)+
      geom_line(size = 1)+
      xlab("Proportion of exchanges") +
      ylab("Number of significant hits") +
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
   return(p)
}