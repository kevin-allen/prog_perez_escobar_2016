boxplot.two.conditions <- function(data,vd="condition",vi="measured",
                                   outma=c(1,0,1,1),margin=c(1,2,1,0),
                                   mgp.x=c(1.2,0.5,0), mgp.y=c(1.2,0.5,0),
                                   at.yaxis=seq(0,1,0.2),ylab="",...)
{
  par(oma=outma,mar=margin,mgp=mgp.x)
  boxplot(data[,vi]~data[,vd],
          frame.plot=FALSE,axes=FALSE,outline=FALSE,
          col=c("red","grey"),...)
  
  
  
  par(mgp=mgp.x)
  axis(side = 1, at=1:2, pos=0,tck=-0.05,cex.axis=0.6,labels=c("",""))
  par(mgp=mgp.y)
  axis(side = 2, at=at.yaxis,   las=1,pos=0, tck=-0.05,cex.axis=0.6,xpd=TRUE)
  title(ylab=ylab,mgp=mgp.y)
}

border.cells.figure <- function()
{
  m<-matrix(rbind(c(0.00,0.25,0.87,1.00),
                  c(0.25,0.50,0.87,1.00),
                  c(0.50,0.75,0.87,1.00),
                  c(0.75,1.00,0.87,1.00),
                  c(0.00,0.25,0.75,0.87),
                  c(0.25,0.50,0.75,0.87),
                  c(0.50,0.75,0.75,0.87),
                  c(0.75,1.00,0.75,0.87),
                  c(0.00,0.25,0.63,0.75),
                  c(0.25,0.50,0.63,0.75),
                  c(0.50,0.75,0.63,0.75),
                  c(0.75,1.00,0.63,0.75),
                  c(0.00,0.25,0.50,0.63),
                  c(0.25,0.50,0.50,0.63),
                  c(0.50,0.75,0.50,0.63),
                  c(0.75,1.00,0.50,0.63),
                  c(0.00,0.25,0.37,0.50),
                  c(0.25,0.50,0.37,0.50),
                  c(0.50,0.75,0.37,0.50),
                  c(0.75,1.00,0.37,0.50),
                  c(0.00,0.33,0.18,0.37),
                  c(0.33,0.66,0.18,0.37),
                  c(0.66,1.00,0.18,0.37),
                  c(0.00,0.33,0.00,0.18),
                  c(0.33,0.66,0.00,0.18),
                  c(0.66,1.00,0.00,0.18)
  )
  ,ncol=4)
  fn<-paste(ep@directory,"figures_relectro","border_figure_5.pdf",sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,width=3.25,height=5.5)  
  #x11(width=3.25, height=5.5)
  split.screen(m)
  
  index=1
  for(clu in border.cells.on.figure){
    x<-tmaps[which(tmaps$clu.id==clu),]
    screen(index)
    y<-x[which(x$condition=="l1"),]
    firingRateMapPlot(y, outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    
    screen(index+1)
    y<-x[which(x$condition=="d1"),]
    firingRateMapPlot(y, outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    
    screen(index+2)
    y<-x[which(x$condition=="l2"),]
    firingRateMapPlot(y, outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    
    screen(index+3)
    y<-x[which(x$condition=="d2"),]
    firingRateMapPlot(y, outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    
    index=index+4
  }
  
  ## plot wvl for light and dark ##
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$border==T)] &(tstats$condition=="l1"| tstats$condition=="d1")  ),]
  x$condition<-factor(x$condition,levels=c("l1","d1"))    
  screen(21)
  boxplot.two.conditions(data=x,vd="condition",vi="dm",xlim=c(0,3),ylim=c(0,0.5),ylab="DM",at.yaxis=seq(0,0.5,0.1))
  screen(22)
  boxplot.two.conditions(data=x,vd="condition",vi="polarity",xlim=c(0,3),ylim=c(0,1),ylab="Map polarity")
  
  close.screen(all=TRUE)
  dev.off()
}
border.cells.stats<-function(){
  
  
  print("****************************")
  print("****     border cells    ***")
  print("****************************")
  
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$border==T)]),]
  print("borderness (DM) of border cells during l1")
  print(length(x$dm[which(x$condition=="l1")]))
  print(summary(x$dm[which(x$condition=="l1")]))
  print("borderness (DM) of border cells during d1")
  print(length(x$dm[which(x$condition=="d1")]))
  print(summary(x$dm[which(x$condition=="d1")]))
  print("comparing borderness of border cells between l1 and d1")
  print(wilcox.test(x$dm[which(x$condition=="l1")],
                    x$dm[which(x$condition=="d1")],
                    paired=T))
  print("map polarity border cells during l1")
  print(length(x$polarity[which(x$condition=="l1")]))
  print(summary(x$polarity[which(x$condition=="l1")]))
  print("map polarity border cells during d1")
  print(length(x$polarity[which(x$condition=="d1")]))
  print(summary(x$polarity[which(x$condition=="d1")]))
  print("comparing polarity score of border cells between l1 and d1")
  print(wilcox.test(x$polarity[which(x$condition=="l1")],
                    x$polarity[which(x$condition=="d1")],
                    paired=T))
  print("mean rate of border cells during l1")
  print(length(x$mean.rate[which(x$condition=="l1")]))
  print(summary(x$mean.rate[which(x$condition=="l1")]))
  print("mean rate of border cells during d1")
  print(length(x$mean.rate[which(x$condition=="d1")]))
  print(summary(x$mean.rate[which(x$condition=="d1")]))
  print("comparing border cells mean rate during l1 and d1")
  print(wilcox.test(x$mean.rate[which(x$condition=="l1")],
                    x$mean.rate[which(x$condition=="d1")],
                    paired=T))
  
  print("border cell stats with only mec cells")
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$border==T&cells$region=="mec")]),]
  print("borderness (DM) of border cells during l1 (Only mec cells)")
  print(length(x$dm[which(x$condition=="l1")]))
  print(summary(x$dm[which(x$condition=="l1")]))
  print("borderness (DM) of border cells during d1")
  print(length(x$dm[which(x$condition=="d1")]))
  print(summary(x$dm[which(x$condition=="d1")]))
  print("comparing borderness of border cells between l1 and d1")
  print(wilcox.test(x$dm[which(x$condition=="l1")],
                    x$dm[which(x$condition=="d1")],
                    paired=T))
  print("map polarity border cells during l1 (Only mec cells)")
  print(length(x$polarity[which(x$condition=="l1")]))
  print(summary(x$polarity[which(x$condition=="l1")]))
  print("map polarity border cells during d1")
  print(length(x$polarity[which(x$condition=="d1")]))
  print(summary(x$polarity[which(x$condition=="d1")]))
  print("comparing polarity score of border cells between l1 and d1")
  print(wilcox.test(x$polarity[which(x$condition=="l1")],
                    x$polarity[which(x$condition=="d1")],
                    paired=T))

  
  ## aggregate per animal
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$border==T)]&(tstats$condition=="l1"|tstats$condition=="d1") ),]
  x$mouse<-animalNameFromSessionName(x$session)
  
  print(paste("Aggregate per mouse for DM"))
  x.agg<-aggregate(x$dm,by=list(x$mouse,x$condition),median)
  colnames(x.agg)<-c("mouse","condition","dm")
  n<-aggregate(x$dm,by=list(x$mouse,x$condition),length)
  
  colnames(n)<-c("mouse","condition","n")
  print(paste("N:",length(x.agg$dm[which(x.agg$condition=="l1")])))
  print("DM l1 aggregate")
  print(paste(summary(x.agg$dm[which(x.agg$condition=="l1")])))
  print("DM d1 aggregate")
  print(paste(summary(x.agg$dm[which(x.agg$condition=="d1")])))
  print(wilcox.test(x.agg$dm[which(x.agg$condition=="l1")],
              x.agg$dm[which(x.agg$condition=="d1")],paired=T ))
  

  print(paste("Aggregate per mouse for polarity"))
  x.agg<-aggregate(x$polarity,by=list(x$mouse,x$condition),median)
  colnames(x.agg)<-c("mouse","condition","polarity")
  n<-aggregate(x$polarity,by=list(x$mouse,x$condition),length)
  colnames(n)<-c("mouse","condition","n")
  print("polarity l1 aggregate")
  print(paste(summary(x.agg$polarity[which(x.agg$condition=="l1")])))
  print("DM d1 aggregate")
  print(paste(summary(x.agg$polarity[which(x.agg$condition=="d1")])))
  print(wilcox.test(x.agg$polarity[which(x.agg$condition=="l1")],
              x.agg$polarity[which(x.agg$condition=="d1")],paired=T ))
  
}



##############################################
########### load data.frames #################
##############################################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)
border.cells.on.figure<-c("jp5520-26092015-0108_8",
                          "jp5519-24102015-0108_3",
                          "jp19844-26082015-0108_8",
                          "jp19841-21072015-0108_9",
                          "jp19841-10072015-0108_6") 
border.cells.stats()
border.cells.figure()


rm(tmaps,tstats,border.cells.on.figure)
rm(border.cells.figure,border.cells.stats,
   boxplot.two.conditions,rename.condition.data.frame)
