dotplot_bin_sig<-function(select,variable,testdata,folder,method="kruskal.test",s=15)
{
  library(ggplot2)
  library(see)
  dir.create(path=folder,showWarnings = FALSE)
  binwidth=(max(testdata[[select]],na.rm=T)/2-mean(testdata[[select]],na.rm=T))/s
  stackratio=1
  dotsize=1.2
  p<-ggplot(testdata, aes_string(x=variable, y=select, fill=variable,group=variable)) +
    #geom_violindot(binwidth=binwidth, size_dots=dotsize)+
    geom_dotplot(binaxis='y', stackdir='center',position="identity", method="dotdensity",binwidth=binwidth,stackratio=stackratio, dotsize=dotsize)+
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE),limits=c(-0.1*max(testdata[[select]]),1.1*max(testdata[[select]])))+
    scale_fill_manual(values=c("#00b0f6","#f8766d"))+
    stat_summary(fun.ymin = function(z) { quantile(z,0.025) },
                 fun.ymax = function(z) { quantile(z,0.975) },
                 fun.y = median, shape=18,size=2,
                 geom="pointrange",color="black",show.legend = FALSE)+
    ggpubr::stat_compare_means(method = method,size=10)+
    #coord_trans(y="log10")+
    #ylim()+
    theme_bw()+
    theme(plot.title = element_text(size = 24, face = "bold"),
          #legend.position = "none"
          legend.title=element_text(size=20),
          legend.text=element_text(size=16)
          )+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1),
          axis.text=element_text(size=16,face="bold"),
          axis.title=element_text(size=20,face="bold"))
  p
  ggsave(filename=paste(folder,"/",select,".pdf",sep=""),width = 10, height = 7)
  return(select)
}


