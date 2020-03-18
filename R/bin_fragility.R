bin_fragility<-function(alloutputs,lowindx,top_pm,alpha=0.05,legend=FALSE,lgndcol=2,yrange=NULL,pvtitle=NULL,savepvfile,pvpicdim)
{
  results<-alloutputs$alldist
  selname0<-alloutputs$selname
  #top_pm<-length(selname)
  
  if(top_pm>length(selname0))
  {
    stop("top_pm should be smaller than the number of observations.")
  }
 
  ###p-values#####
  df<-as.data.frame(results$pvdist)
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
  
  temp<-matrix(as.numeric(as.character(df$rep)),N,top_pm,byrow=T)
  #min_N<-which.min(temp[,1])
  dat<-temp[1:lowindx,]
  
  #colnames(temp)<-selname
  df<-data.frame(fragility=integer(top_pm),variable=character(top_pm),stringsAsFactors=FALSE)
  df$fragility<-colSums(dat>-log10(alpha))
  df$variable<-selname
  df<-df[order(-df$fragility),] 
  df$variable<-factor(selname,levels=selname)
  df$group<-factor(df$fragility,levels=sort(unique(df$fragility),decreasing=T))
  
  p<-ggplot(data=df, aes(x=variable,y=fragility,fill=group))+
    geom_bar(stat="identity")
  
  #ggplot_build(p)$data
    #facet_wrap(~group)+
    #ylim(0, 1)+
if(is.null(yrange))
    {
      y_range<-range(0,ceiling(max(df$fragility)))
    }else{
      y_range<-yrange
    }
      
  p<-ggplot(data=df, aes(x=variable,y=fragility,fill=variable))+
    geom_bar(stat="identity")+
    scale_fill_manual(aes(labels = variable), values = ggplot_build(p)$data[[1]]$fill,
                        drop = FALSE)+
    ylab("Fragility Index") +
    xlab(paste("Top",top_pm,"significant hits with decreasing order",sep=" "))+
    #guides(colour=guide_legend(ncol=lgndcol))+
    ggtitle(pvtitle) +
    ylim(y_range)+
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold"))+
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
     if(isFALSE(legend))
      {
        p<-p+theme(legend.position="none")
      }else{
        p<-p+
        guides(col=guide_legend(ncol=lgndcol))+
        theme(legend.title=element_text(size=20),
            legend.text=element_text(size=12))
      }   
  ggsave(filename=savepvfile,plot=p,width = pvpicdim[1], height = pvpicdim[2])
  return(list(p=p,df=df))
}
  
  