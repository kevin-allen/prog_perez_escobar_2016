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

hd.cells.stats<-function(){
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$hd==T)]),]
  print(paste("Number of hd cells",length(x$clu.id[which(x$condition=="l1")])))
  print(paste("vector length l1"))
  print(summary(x$hd.vl[which(x$condition=="l1")]))
  print(paste("vector length d1"))
  print(summary(x$hd.vl[which(x$condition=="d1")]))
  print(wilcox.test(x$hd.vl[which(x$condition=="l1")],
              x$hd.vl[which(x$condition=="d1")],paired=T))
  
  print("Using only MEC tetrodes")
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$hd==T&cells$region=="mec")]),]
  print(paste("Number of hd cells",length(x$clu.id[which(x$condition=="l1")])))
  print(paste("vector length l1"))
  print(summary(x$hd.vl[which(x$condition=="l1")]))
  print(paste("vector length d1"))
  print(summary(x$hd.vl[which(x$condition=="d1")]))
  print(wilcox.test(x$hd.vl[which(x$condition=="l1")],
                    x$hd.vl[which(x$condition=="d1")],paired=T))

  print("Aggregate per mouse")
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$hd==T)]),]
  x$mouse<-animalNameFromSessionName(x$session)
  x.agg<-aggregate(x$hd.vl,by=list(x$mouse,x$condition),median)
  colnames(x.agg)<-c("mouse","condition","hd.vl")
  n<-aggregate(x$hd.vl,by=list(x$mouse,x$condition),length)
  colnames(n)<-c("mouse","condition","n")
  summary(x.agg$hd.vl[which(x.agg$condition=="l1")])
  summary(x.agg$hd.vl[which(x.agg$condition=="d1")])
  print(wilcox.test(x.agg$hd.vl[which(x.agg$condition=="l1")],
              x.agg$hd.vl[which(x.agg$condition=="d1")],paired=T))
  
}
hd.cells.figure <- function()
{
  num.cols<-4
  num.rows<-6
  plot.per.page=num.cols*num.rows
  m<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
              rep(seq(1/num.cols,1,1/num.cols),num.rows),
              rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
              rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  
  
  fn<-paste(ep@directory,"figures_relectro","hd_figure_6.pdf",sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,width=3.5,height=5.5)  
  #x11(width=3.5, height=5.5)
  split.screen(m)
  
  index=1
  for(clu in hd.cells.on.figure){
    x<-tmaps[which(tmaps$clu.id==clu),]
    k<-thdhisto[which(thdhisto$clu.id==clu),]
    screen(index)
    l<-k[which(k$condition=="l1"),]
    headDirectionPolarPlot(l, outma=c(2.0,2.0,2.0,2.0),margin=c(0.5,0.5,0.5,0.5))
    screen(index+1)
    l<-k[which(k$condition=="d1"),]
    headDirectionPolarPlot(l, outma=c(2.0,2.0,2.0,2.0),margin=c(0.5,0.5,0.5,0.5))
    screen(index+2)
    l<-k[which(k$condition=="l2"),]
    headDirectionPolarPlot(l, outma=c(2.0,2.0,2.0,2.0),margin=c(0.5,0.5,0.5,0.5))
    screen(index+3)
    l<-k[which(k$condition=="d2"),]
    headDirectionPolarPlot(l, outma=c(2.0,2.0,2.0,2.0),margin=c(0.5,0.5,0.5,0.5))
    index=index+4
  }
  
  ## plot wvl for light and dark ##
  x<-tstats[which(tstats$clu.id%in%cells$cell.id[which(cells$hd==T)] &(tstats$condition=="l1"| tstats$condition=="d1")  ),]
  x$condition<-factor(x$condition,levels=c("l1","d1"))    
  screen(21)
  boxplot.two.conditions(data=x,vd="condition",vi="hd.vl",xlim=c(0,3),ylim=c(0,1),ylab="HD vector length",at.yaxis=seq(0,1,0.2))
  
  close.screen(all=TRUE)
  dev.off()
}



##############################################
########### load data.frames #################
##############################################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
load(paste(ep@directory,"results","thdhisto",sep="/"))
load(paste(ep@directory,"results","bhdhisto",sep="/"))
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)
thdhisto<-unsplit(lapply(split(thdhisto,thdhisto$session),rename.condition.data.frame), thdhisto$session)



hd.cells.on.figure<-c("jp5519-21102015-0108_11",
                      "jp5520-26092015-0108_4",
                      "jp5520-24092015-0108_14",
                      "jp19844-26082015-0108_10",
                      "jp19841-27062015-0108_9") 
hd.cells.stats()
hd.cells.figure()


rm(tmaps,tstats,thdhisto,bhdhisto)
rm(hd.cells.stats,hd.cells.figure,hd.cells.on.figure,
   boxplot.two.conditions,rename.condition.data.frame)
