bin_pv_dist_transit<-function(alloutputs,lowindx,cutoff,folder,pvtitle,pvpicdim)
{
  library(ggplot2)
  dir.create(path=folder,showWarnings = FALSE)
  df<-alloutputs$alldist$pvdist
  df<-data.frame(df)
  df$pvalue<-10^(-as.numeric(as.character((df[,"rep"]))))
  scenario0<-df[df$dif==0,]
  scenario0$Status<-"stay"
  scenario0[which(scenario0$pvalue>cutoff),"Status"]<-paste("leave",cutoff,sep="")
  scenario0<-na.omit(scenario0)
  if(length(unique(scenario0$Status))==1)
  {
    cl0<-"#00BFC4"
  }else
  {
    cl0<-c("#F8766D","#00BFC4")
  }
  #h0<-hist(scenario0$pvalue, breaks = seq(0,1,by=0.05))
  #plot(h0)
  p<-ggplot(scenario0,aes(x=pvalue,fill=Status))+
    xlim(0,1)+
    geom_histogram(color="black",fill="#00BFC4",bins=21, position="stack",boundary = 0)+
    scale_fill_manual(values=cl0)
  sc<-ggplot_build(p)$data[1]
  p<-p+geom_vline(aes(xintercept=cutoff),color="#ff00cc",linetype="dashed",size=2,show.legend=F)+
    geom_text(aes(x=cutoff, label=paste("p=",cutoff,eps=""), y=-0.5), colour="blue", vjust = 1.2,size=5)+
    ylim(c(-0.5,1.1*max(sc[[1]]$y)))
  if(!is.null(pvtitle)){
    p<-p+ggtitle(paste(pvtitle,"permutation proportion", round(0/lowindx,2),sep=" "))
  }
    p+
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
    ggsave(filename=paste(folder,"/",round(0/lowindx,2),"_permute","_pvalue.eps",sep=""),width = 7, height = 7)
  
  list0<-(scenario0$pvalue<cutoff)
  len0<-sum(list0)
  #temp0<-scenario0[1:len0,]
  
  for(i in 1:lowindx)
  {
    scenario<-df[df$dif==i,]  
    temp<-scenario[1:len0,]
    scenario$Status<-"stay"
    scenario[c(temp$pvalue>cutoff,rep(FALSE,dim(scenario)[1]-len0)),"Status"]<-paste("leave",cutoff,sep="")
    if(length(unique(scenario$Status))==2)
    {
      cl<-c("#F8766D","#00BFC4")
    }else
      {
        if(unique(scenario$Status)=="stay")
        {
          cl<-"#00BFC4"
        }else
        {
          cl<-"#F8766D"
        }
      }
    p<-ggplot(scenario,aes(x=pvalue,fill=Status))+
      xlim(0,1)+
      geom_histogram(color="black",bins=21, position="stack",boundary = 0)+
      scale_fill_manual(values=cl)
    p<-p+geom_vline(aes(xintercept=cutoff),color="#ff00cc",linetype="dashed",size=2,show.legend=F)+
      geom_text(aes(x=cutoff, label=paste("p=",cutoff,eps=""), y=-0.5), colour="blue", vjust = 1.2,size=5)+
      ylim(c(-0.5,1.1*max(sc[[1]]$y)))
    if(!is.null(pvtitle)){
      p<-p+ggtitle(paste(pvtitle,"permutation proportion", round(i/lowindx,2),sep=" "))
    }
    p+
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
  
  # hist(scenario$pvalue, breaks = seq(0,1,by=0.05))
  # 
  # library(data.table)
  #   scenario<-data.table(scenario)
  #   scenario[,Group:=cut(pvalue,breaks=seq(0,1,by=0.05),include.lowest=TRUE,labels=as.character(seq(0.025,0.975,by=0.05)))]
  #   ss<-scenario[1:len,]
  #   temp1<-table(scenario$Group)
  #   temp1[1]<-0
  #   temp2<-table(ss$Group)
  #   temp2[1]<-0
  #   barplot(temp1,col="blue")
  #   par(new=TRUE)
  #   barplot(temp2,col="red")
}


