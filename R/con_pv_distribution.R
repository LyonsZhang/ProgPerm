con_pv_distribution<-function(alloutputs,folder,pvtitle,pvpicdim)
{
  library(ggplot2)
  dir.create(path=folder,showWarnings = FALSE)
  df<-alloutputs$alldist$pvdist
  df<-data.frame(df)
  df$pvalue<-10^(-as.numeric(as.character((df[,"rep"]))))
  
  difseq<-unique(alloutputs$alldist$psigdist[,6])
  lowindx<-length(difseq)-1
  
  for(i in 0:lowindx)
  {
    p<-ggplot(df[df$dif==difseq[i+1],], aes(x=pvalue)) + 
      xlim(0,1)+
      geom_histogram(aes(fill=..count..),color="black",bins=21, position="dodge",boundary = 0)
    #scale_x_continuous(breaks = seq(0, 1, 0.2))
    if(i==0)
    {
      temp<-ggplot_build(p)$data[1]
    }
    p+geom_vline(aes(xintercept=0.05, color="red"),linetype="dashed",size=2,show.legend=F)+
      ylim(c(0,1.1*(max(temp[[1]]$y))))+
      ggtitle(paste(pvtitle,"permutation proportion", round(i/lowindx,2),sep=" ")) +
      theme_bw()+
      theme(#legend.position = "none",
        plot.title = element_text(hjust = 0.5,size=30),
        legend.title=element_text(size=20),
        legend.text=element_text(size=12)
      )+
      theme(
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        #axis.ticks.length =unit(10,"mm"),
        #axis.ticks.y = element_line(size=0.5),
        #axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(),
        #axis.line.x.bottom = element_line(color="black", size = 1),
        #axis.line.x.top=element_line(color="black", size = 1),
        #axis.line.y.left = element_line(color="black", size = 1),
        #axis.line.y.right = element_line(color="black", size = 1),
        axis.text=element_text(size=24,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)
      )
    ggsave(filename=paste(folder,"/",round(i/lowindx,2),"_permute","_pvalue.eps",sep=""),width = 7, height = 7)
  }
}


