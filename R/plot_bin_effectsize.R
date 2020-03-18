plot_bin_effectsize<-function(bestoutputs,variable,testdata,estitle=NULL,saveesfile,espicdim,goodtype="goodpvname",effecttext=FALSE)
{
  levs<-levels(testdata[[variable]])
  select<-switch(goodtype,
         goodpsigname=bestoutputs$goodpsigname,
         goodpvname=bestoutputs$goodpvname,
         goodesname=bestoutputs$goodesname,
         stop("goodtype must be one of the follows: 'goodpsigname', 'goodpvname' and 'goodesname'."))
  selname<-bestoutputs$selname
  df<-data.frame(matrix(0,length(selname),2))
  row.names(df)<-selname
  colnames(df)<-c("selname","effectsize")
  df$selname<-selname
  df$effectsize<-bestoutputs$effectsize0
  df1<-df[selname %in% select,]
  ranktemp<-apply(testdata[,select],2,rank,ties.method="average")
  rankmean<-aggregate(ranktemp,
            by=list(group=testdata[[variable]]),
            mean)
  df1$signed<-t(sign(rankmean[rankmean$group==levs[2],-1]-rankmean[rankmean$group==levs[1],-1]))
  temp<-df1[df1$signed==-1,]
  temp<- temp[order(temp$effectsize),]
  df2<-rbind(df1[df1$signed==1,],temp)
  df2$signedeffect<-df2$effectsize*df2$signed
  df2$selname<-factor(df2$selname,levels=df2$selname)
  df2[[variable]]<-factor(ifelse(df2$signed==1,levs[2],levs[1]))

  p<-ggplot(df2, aes(selname, signedeffect)) +
    geom_bar(stat = "identity", aes_string(fill = variable)) +
    scale_fill_manual(values=c("#00b0f6","#f8766d"))+
    geom_hline(yintercept=0,size=2)+
    ylab("Effect size") +
    xlab("Selected features with decreasing order")+
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
  if(effecttext==TRUE)
  {
    p<-p+geom_text(aes(label = round(effectsize,2),
                  hjust=0.5,
                  angle = 0))
  }
  ggsave(filename=saveesfile,width = espicdim[1], height = espicdim[2])
  return(p)
}
