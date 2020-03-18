scatterplot_con_sig<-function(select,variable,testdata,folder,method="kendall")
{
  dir.create(path=folder,showWarnings = FALSE)
  library("ggpubr")
  library("ggplot2")
  p<-ggscatter(testdata, x=variable, y=select,
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = method,cor.coef.size=8,
            xlab = variable, ylab = select)+
    theme_bw()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          legend.title=element_text(size=30),
          legend.text=element_text(size=16))+
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


