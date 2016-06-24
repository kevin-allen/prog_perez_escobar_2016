plot.two.distributions <- function(v1,v2,min=-1,max=1,int=0.05,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,plotxlim=c(-1,1),plotylim=c(0,0.4),
                                   outma=c(0.5,0.5,0.2,0.2),margin=c(1.5,1.5,0.5,0.3),mgp=c(0.8,0.3,0),probability=1,
                                   xaxis.at=seq(-1,1,0.50),vline="",
                                   main.title="",mgp.x=c(1,1,1),mgp.y=c(1,1,1),xlab="",ylab="", legend.lab=c(""),...)
{
  par(oma=outma,mar=margin,cex.lab=0.6,cex.axis=0.4,mgp=mgp)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab='',ylab="",...)
  if(probability==1)
  {
    h<-hist(v1,seq(min,max,int),plot=FALSE)
    lines(h$mids, h$counts/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
    h<-hist(v2,seq(min,max,int),plot=FALSE)
    lines(h$mids, h$counts/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=0.75,col="red")
  }
  if(probability==0)
  {
    h<-hist(v1,seq(min,max,int),plot=FALSE)
    lines(h$mids, h$counts, type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
    h<-hist(v2,seq(min,max,int),plot=FALSE)
    lines(h$mids, h$counts, type='l',pch=20,xlab='',ylab='',lwd=0.75,col="red")
  }
  if(probability==2)
  {
    h<-hist(v1,seq(min,max,int),plot=FALSE)
    lines(h$mids, cumsum(h$counts)/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
    h<-hist(v2,seq(min,max,int),plot=FALSE)
    lines(h$mids, cumsum(h$counts)/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=0.75,col="red")
  }
  par(mgp=mgp.x)
  axis(side = 1, at=xaxis.at, pos=axis.x.pos,tck=-0.05,cex.axis=0.60)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
  
  if(vline!="")
  {
    lines(c(vline,vline),plotylim)
  }
  if(main.title!=""){
    title(main=main.title,cex.main=0.6)
  }
  
  if(legend.lab[1]!="")
  {
    legend(x=plotxlim[1],y=plotylim[2],legend.lab,
           cex=0.6,lty=1,bty="n",
           col = seq(1,length(legend.lab)))
  }
}

boxplot.two.conditions <- function(data,vd="condition",vi="measured",outma=c(1,0,1,1),margin=c(2,2,0,0),mgp=c(1.2,0.5,0),...)
{
  par(oma=outma,mar=margin,mgp=mgp)
  boxplot(data[,vi]~data[,vd],las=1,
          frame.plot=FALSE,axes=FALSE,outline=FALSE,
          col=c("red","grey"),...)
  axis(side = 1, at=0:3, tck=-0.05,cex.axis=0.6,labels=c("","light","dark",""))
  axis(side = 2,las=1, tck=-0.05,cex.axis=0.6,xpd=TRUE)
}

boxplot.block.condition.correlation <- function(data,vd1="block",vd2="condition",vi="measured",
                                                outma=c(1,0,1,0),margin=c(2,2,0,0),
                                                mgp=c(1,0.3,0),
                                                ...)
{
  nvd1=length(unique(data[,vd1]))
  nvd2=length(unique(data[,vd2]))
  
  x<-seq(1,nvd1*nvd2)
  axisx.at<-x+floor((x-1)/nvd1)
  axisx.labels=rep(seq(1,nvd1),nvd2)
  
  par(oma=outma,mar=margin,mgp=mgp)
  boxplot(data[,vi]~data[,vd1]*data[,vd2],las=1,
          frame.plot=FALSE,axes=FALSE,outline=FALSE,at=axisx.at,
          col=c(rep("grey",nvd1*2),    rep("red",nvd1*2)),...)
  axis(side = 1, tck=-0.05,cex.axis=0.6,at=axisx.at,labels=axisx.labels)
  axis(side = 2,las=1, tck=-0.05,cex.axis=0.6,xpd=TRUE)
}

barplot.per.mouse <- function(data,dv="dv",iv1="iv1",iv2="iv2",
                              outma=c(1,0,1,0),margin=c(2,2,0,0),
                              mgp=c(1,0.3,0),ylab="",
                              y.at=seq(-0.5,1.5,0.5),...)
{ 
  
  niv1=length(unique(data[,iv1]))
  niv2=length(unique(data[,iv2]))
  
  x<-seq(1,niv1*niv2)
  axisx.at<-x+floor((x-1)/niv1)
  axisx.labels=rep(seq(1,niv1),niv2)
  par(oma=outma,mar=margin,mgp=mgp)
  boxplot(data[,dv]~data[,iv1]*data[,iv2],las=1,
          frame.plot=FALSE,axes=FALSE,outline=FALSE,at=axisx.at,
          col=c("red","grey"),...)
  axis(side = 1, tck=-0.05,cex.axis=0.6,at=axisx.at,labels=axisx.labels)
  axis(side = 2,las=1, tck=-0.05,cex.axis=0.6,xpd=TRUE,at=y.at)
  title(ylab=ylab,mgp=mgp,cex.axis=0.6)
}

grid.cells.figure <- function()
{
  m<-matrix(rbind(c(0.00,0.14,0.83,1.00),
                  c(0.14,0.28,0.83,1.00),
                  c(0.28,0.41,0.83,1.00),
                  c(0.41,0.55,0.83,1.00),
                  c(0.00,0.14,0.67,0.83),
                  c(0.14,0.28,0.67,0.83),
                  c(0.28,0.41,0.67,0.83),
                  c(0.41,0.55,0.67,0.83),
                  c(0.00,0.14,0.50,0.67),
                  c(0.14,0.28,0.50,0.67),
                  c(0.28,0.41,0.50,0.67),
                  c(0.41,0.55,0.50,0.67),
                  c(0.00,0.14,0.33,0.50),
                  c(0.14,0.28,0.33,0.50),
                  c(0.28,0.41,0.33,0.50),
                  c(0.41,0.55,0.33,0.50),
                  c(0.00,0.14,0.17,0.33),
                  c(0.14,0.28,0.17,0.33),
                  c(0.28,0.41,0.17,0.33),
                  c(0.41,0.55,0.17,0.33),
                  c(0.00,0.14,0.00,0.17),
                  c(0.14,0.28,0.00,0.17),
                  c(0.28,0.41,0.00,0.17),
                  c(0.41,0.55,0.00,0.17),
                  
                  c(0.55,0.77,0.75,1.00),
                  c(0.77,1.00,0.75,1.00),
                  c(0.55,0.77,0.45,0.75),
                  c(0.77,1.00,0.45,0.75),
                  c(0.55,1.00,0.20,0.45),
                  c(0.55,0.70,0.00,0.20),
                  c(0.70,0.85,0.00,0.20),
                  c(0.85,1.00,0.00,0.20))
            ,ncol=4)
  fn<-paste(ep@directory,"figures_relectro","grid_cells_figure_2.pdf",sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,width=6,height=5)  
  #x11(width=6, height=5)
  split.screen(m)
  index=1
  for(clu in grid.cells.on.figure){
    
    x<-maps[which(maps$clu.id==clu),]
    screen(index)
    y<-x[which(x$condition=="l1"),]
    firingRateMapPlot(y,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    screen(index+1)
    y<-x[which(x$condition=="d1"),]
    firingRateMapPlot(y,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    screen(index+2)
    y<-x[which(x$condition=="l2"),]
    firingRateMapPlot(y,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    screen(index+3)
    y<-x[which(x$condition=="d2"),]
    firingRateMapPlot(y,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    index=index+4
  }
  
  tstats.grid<-tstats[which(tstats$clu.id%in% cells$cell.id[which(cells$grid==T)]),]
  tstats.shuf<-tstats.shuf[which(tstats.shuf$clu.id%in% cells$cell.id[which(cells$grid==T)]),]
  screen(25)
  ## gc grid score
  
  plot.two.distributions(v1=tstats.grid$grid.score[which(tstats.grid$condition=="d1")],
                         v2=tstats.grid$grid.score[which(tstats.grid$condition=="l1")],
                         min=-1.5,max=2,int=0.075,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,plotxlim=c(-1,1.5),plotylim=c(0,0.30),
                         outma=c(1,1,0.0,0.2),margin=c(1.5,1.7,0.7,0.3),mgp=c(0.5,0.3,0),probability=1,
                         xaxis.at=seq(-1,1.5,0.50),vline="",
                         main.title="",mgp.x=c(0.6,0.3,0.2),mgp.y=c(1,0.4,0.3),xlab="Grid score",ylab="Proportion", legend.lab=c(""))
  ## add shuffling to the plot
  h<-hist(tstats.shuf$grid.score[which(tstats.shuf$condition=="l1")],seq(-1.5,2,0.075),plot=F)
  lines(h$mids, h$counts/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=0.75,col="green")
  
  screen(26)
  ## gc info score
  plot.two.distributions(v1=tstats.grid$info.score[which(tstats.grid$condition=="d1")],
                         v2=tstats.grid$info.score[which(tstats.grid$condition=="l1")],
                         min=0,max=1.5,int=0.075,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,plotxlim=c(0,1.5),plotylim=c(0,0.4),
                         outma=c(1,1,0.0,0.2),margin=c(1.5,1.7,0.7,0.3),mgp=c(0.5,0.3,0),probability=1,
                         xaxis.at=seq(-1,1.5,0.50),vline="",
                         main.title="",mgp.x=c(0.6,0.3,0.2),mgp.y=c(1,0.4,0.3),xlab="Information score",ylab="Proportion", legend.lab=c(""))
  ## add shuffling to the plot
  h<-hist(tstats.shuf$info.score[which(tstats.shuf$condition=="l1")],seq(0,10,0.075),plot=F)
  lines(h$mids, h$counts/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=0.75,col="green")
  
  screen(27)
  tstats.grid$mouse<- animalNameFromSessionName(tstats.grid$session)
  n<-aggregate(tstats.grid$grid.score,by=list(tstats.grid$mouse,tstats.grid$condition),length)
  colnames(n)<-c("mouse","condition","n")
  mouse.sel<-unique(n$mouse[which(n$n>5)]) ### only keep animals with at least 5 grid cells
  tstats.grid<-tstats.grid[which(tstats.grid$mouse%in%mouse.sel),]
  tstats.grid$condition<-factor(tstats.grid$condition,levels=c("l1","d1"))
  barplot.per.mouse(data=tstats.grid,dv="grid.score",iv1="condition",iv2="mouse",ylab="Grid score",ylim=c(-0.5,1.5))
  
  screen(28)
  barplot.per.mouse(data=tstats.grid,dv="info.score",iv1="condition",iv2="mouse",ylab="Information score",ylim=c(0.0,1.5),y.at=c(0,0.5,1,1.5))
  
  screen(29)
  ## block map correlation to l1
  bmc<-blockMapCor
  bmc<-bmc[which(bmc$clu.id%in%cells$cell.id[which(cells$grid==T)]),]
  boxplot.block.condition.correlation(data=bmc,vd1="block",vd2="condition",vi="r",ylab="Map similarity",xlab="10-sec blocks")
  
  ## ifr associations
  screen(30)
  ifrg<-ifrAss[which(ifrAss$clu.id1%in%cells$cell.id[which(cells$grid==T)]&
                       ifrAss$clu.id2%in%cells$cell.id[which(cells$grid==T)]),]
  ifrg<-ifrg[which(ifrg$session%in%sessions$session[which(sessions$num.lights==2)]),]
  x<-data.frame(l1=ifrg$r[which(ifrg$condition=="l1")],
                l2=ifrg$r[which(ifrg$condition=="l2")],
                d1=ifrg$r[which(ifrg$condition=="d1")],
                d2=ifrg$r[which(ifrg$condition=="d2")])
  
  plot.points(data=x,v1="l1",v2="l2",
              xlab="ifr corr. l1",ylab="ifr corr. l2",col="red",
              plotxlim=c(-0.2,0.7),plotylim=c(-0.2,0.7),
              add.text=round(cor(x$l1,x$l2),2))
  screen(31)
  plot.points(data=x,v1="l1",v2="d1",
              xlab="ifr corr. l1",ylab="ifr corr. d1",col="red",
              plotxlim=c(-0.2,0.7),plotylim=c(-0.2,0.7),
              add.text=round(cor(x$l1,x$d1),2))
  screen(32)
  ## gc firing rate
  tstats.grid$condition<-factor(tstats.grid$condition)
  boxplot.two.conditions(data=tstats.grid,vd="condition",vi="mean.rate",outma=c(1,0,1,1),margin=c(2,2,0,0),mgp=c(1.2,0.5,0),
                         ylab="Mean rate (Hz)")
  close.screen(all=TRUE)
  dev.off()
}

plot.points <- function(data,v1="v1",v2="v2",xbin.add=0,axis.y.pos=-.2,axis.x.pos=-.2,axis.y.las=2,
                        main.title="",mgp.x=c(0.5,0.1,0.1),mgp.y=c(0.8,0.2,0.1),xlab="",ylab="",
                        plotxlim=c(-.2,0.6),plotylim=c(-0.2,0.6),outma=c(0.5,0.5,0.5,0.5),margin=c(1.5,1.5,1,0.3),
                        xaxis.at=seq(-0.2,0.7,.2),yaxis.at=seq(-0.2,0.7,.2),cex.point=0.1,
                        add.text="",...)
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot(x=plotxlim,y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="",...)
  points(data[,v1],data[,v2],pch=20,cex=cex.point)
  par(mgp=mgp.x)
  axis(side = 1, at=xaxis.at, pos=axis.x.pos,tck=-0.05,cex.axis=0.60)
  par(mgp=mgp.y)
  axis(side = 2, at=yaxis.at, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
  if(main.title!=""){
    title(main=main.title,cex.main=0.6)
  }
  if(add.text!=""){
    text(labels=add.text,x=0,y=0.55,cex=0.6)
  }
}

grid.cells.stats<-function(){
  #### STATS ALL GRID CELLS ####
  print(paste("Number of grid cells:",length(cells$cell.id[which(cells$grid==T)])))
  tstats.grid<-tstats[which(tstats$clu.id%in% cells$cell.id[which(cells$grid==T)]),]
  length(unique(tstats.grid$clu.id))
  if(length(unique(tstats.grid$clu.id))!=length(cells$cell.id[which(cells$grid==T)])){
    print(paste(length(unique(tstats.grid$clu.id)),length(cells$cell.id[which(cells$grid==T)])))
    stop("length of tstats.grid is wrong")
  }
  print("mean firing rate")
  print("Mean rate of grid cells during l1 trials")
  print(length(tstats.grid$mean.rate[which(tstats.grid$condition=="l1")]))
  print(summary(tstats.grid$mean.rate[which(tstats.grid$condition=="l1")]))
  print("Mean rate of grid cells during d1 trials")
  print(length(tstats.grid$mean.rate[which(tstats.grid$condition=="d1")]))
  print(summary(tstats.grid$mean.rate[which(tstats.grid$condition=="d1")]))
  print("Difference grid score l1 vs d1 trials")
  print(wilcox.test(tstats.grid$mean.rate[which(tstats.grid$condition=="l1")],
                    tstats.grid$mean.rate[which(tstats.grid$condition=="d1")]),paired=T)
  print("Grid score of grid cells during l1 trials")
  print(length(tstats.grid$grid.score[which(tstats.grid$condition=="l1")]))
  print(summary(tstats.grid$grid.score[which(tstats.grid$condition=="l1")]))
  print("Grid score of grid cells during d1 trials")
  print(length(tstats.grid$grid.score[which(tstats.grid$condition=="d1")]))
  print(summary(tstats.grid$grid.score[which(tstats.grid$condition=="d1")]))
  print("Difference grid score l1 vs d1 trials")
  print(wilcox.test(tstats.grid$grid.score[which(tstats.grid$condition=="l1")],
                    tstats.grid$grid.score[which(tstats.grid$condition=="d1")]),paired=T)
  
  print("Info score of grid cells during l1 trials")
  print(length(tstats.grid$info.score[which(tstats.grid$condition=="l1")]))
  print(summary(tstats.grid$info.score[which(tstats.grid$condition=="l1")]))
  print("Info score of grid cells during d1 trials")
  print(length(tstats.grid$info.score[which(tstats.grid$condition=="d1")]))
  print(summary(tstats.grid$info.score[which(tstats.grid$condition=="d1")]))
  print("Difference grid score l1 vs d1 trials")
  print(wilcox.test(tstats.grid$info.score[which(tstats.grid$condition=="l1")],
                    tstats.grid$info.score[which(tstats.grid$condition=="d1")]),paired=T)
  
  #### STATS MEC GRID CELLS ####
  print(paste("Number of mec grid cells:",length(cells$cell.id[which(cells$grid==T&cells$region=="mec")])))
  tstats.grid<-tstats[which(tstats$clu.id%in% cells$cell.id[which(cells$grid==T&cells$region=="mec")]),]
  if(length(unique(tstats.grid$clu.id))!=length(cells$cell.id[which(cells$grid==T&cells$region=="mec")])){
    print(paste(length(unique(tstats.grid$clu.id)),length(cells$cell.id[which(cells$grid==T)])))
    stop("length of tstats.grid is wrong")
  }
  print("Grid score of grid cells during l1 trials")
  print(length(tstats.grid$grid.score[which(tstats.grid$condition=="l1")]))
  print(summary(tstats.grid$grid.score[which(tstats.grid$condition=="l1")]))
  print("Grid score of grid cells during d1 trials")
  print(length(tstats.grid$grid.score[which(tstats.grid$condition=="d1")]))
  print(summary(tstats.grid$grid.score[which(tstats.grid$condition=="d1")]))
  print("Difference grid score l1 vs d1 trials")
  print(wilcox.test(tstats.grid$grid.score[which(tstats.grid$condition=="l1")],
                    tstats.grid$grid.score[which(tstats.grid$condition=="d1")]),paired=T)
  print("Info score of grid cells during l1 trials")
  print(length(tstats.grid$info.score[which(tstats.grid$condition=="l1")]))
  print(summary(tstats.grid$info.score[which(tstats.grid$condition=="l1")]))
  print("Info score of grid cells during d1 trials")
  print(length(tstats.grid$info.score[which(tstats.grid$condition=="d1")]))
  print(summary(tstats.grid$info.score[which(tstats.grid$condition=="d1")]))
  print("Difference grid score l1 vs d1 trials")
  print(wilcox.test(tstats.grid$info.score[which(tstats.grid$condition=="l1")],
                    tstats.grid$info.score[which(tstats.grid$condition=="d1")]),paired=T)
  
  #### STATS MOUSE AGGREGATE ####
  print(paste("Number of mice:",length(unique(cells$mouse))))
  tstats.grid<-tstats[which(tstats$clu.id%in% cells$cell.id[which(cells$grid==T)]),]
  tstats.grid$mouse<-animalNameFromSessionName(tstats.grid$session)
  tstats.agg<-aggregate(tstats.grid$grid.score,by=list(tstats.grid$mouse,tstats.grid$condition),median)
  colnames(tstats.agg)<-c("mouse","condition","grid.score")
  n<-aggregate(tstats.grid$grid.score,by=list(tstats.grid$mouse,tstats.grid$condition),length)
  colnames(n)<-c("mouse","condition","n")
  mouse.sel<-unique(n$mouse[which(n$n>5)]) ### only keep animals with at least 5 grid cells
  tstats.agg<-tstats.agg[which(tstats.agg$mouse%in%mouse.sel),] 
  
  print(paste("Number of mice with at least 5 grid cells:",length(unique(tstats.agg$mouse))))
  print("Grid score of grid cells, one score per mouse, l1 trials")
  print(length(tstats.agg$mouse[which(tstats.agg$condition=="l1")]))
  print(summary(tstats.agg$grid.score[which(tstats.agg$condition=="l1")]))
  print("Grid score of grid cells, one score per mouse, d1 trials")
  print(length(tstats.agg$mouse[which(tstats.agg$condition=="d1")]))
  print(summary(tstats.agg$grid.score[which(tstats.agg$condition=="d1")]))
  print(wilcox.test(tstats.agg$grid.score[which(tstats.agg$condition=="l1")],
                    tstats.agg$grid.score[which(tstats.agg$condition=="d1")],paired=T)) 
  
  tstats.agg<-aggregate(tstats.grid$info.score,by=list(tstats.grid$mouse,tstats.grid$condition),median)
  colnames(tstats.agg)<-c("mouse","condition","info.score")
  n<-aggregate(tstats.grid$grid.score,by=list(tstats.grid$mouse,tstats.grid$condition),length)
  colnames(n)<-c("mouse","condition","n")
  mouse.sel<-unique(n$mouse[which(n$n>5)]) ### only keep animals with at least 5 grid cells
  tstats.agg<-tstats.agg[which(tstats.agg$mouse%in%mouse.sel),] 
  
  print(paste("Number of mice with at least 5 grid cells:",length(unique(tstats.agg$mouse))))
  print("Info score of grid cells, one score per mouse, l1 trials")
  print(length(tstats.agg$mouse[which(tstats.agg$condition=="l1")]))
  print(summary(tstats.agg$info.score[which(tstats.agg$condition=="l1")]))
  print("Info score of grid cells, one score per mouse, d1 trials")
  print(length(tstats.agg$mouse[which(tstats.agg$condition=="d1")]))
  print(summary(tstats.agg$info.score[which(tstats.agg$condition=="d1")]))
  print(wilcox.test(tstats.agg$info.score[which(tstats.agg$condition=="l1")],
                    tstats.agg$info.score[which(tstats.agg$condition=="d1")],paired=T)) 
  
  print("Map stability")
  print("D1 trials compare to L1")
  bmc<-blockMapCor
  bmc<-bmc[which(bmc$clu.id%in%cells$cell.id[which(cells$grid==T)]),] ## keep only grid cells 
  print(paste("n grid cells with 2 lights:",length(bmc$r[which(bmc$condition=="d1"&bmc$block==1)])))
  print(summary((bmc$r[which(bmc$condition=="d1"&bmc$block==1)])))
  print(summary((bmc$r[which(bmc$condition=="d1"&bmc$block==2)])))
  print(summary((bmc$r[which(bmc$condition=="d1"&bmc$block==3)])))
  print(summary((bmc$r[which(bmc$condition=="d1"&bmc$block==4)])))
  print(wilcox.test(bmc$r[which(bmc$condition=="d1"&bmc$block==1)],
                    bmc$r[which(bmc$condition=="d1"&bmc$block==2)],paired=T))
  print(wilcox.test(bmc$r[which(bmc$condition=="d1"&bmc$block==2)],
                    bmc$r[which(bmc$condition=="d1"&bmc$block==3)],paired=T))
  print(wilcox.test(bmc$r[which(bmc$condition=="d1"&bmc$block==3)],
                    bmc$r[which(bmc$condition=="d1"&bmc$block==4)],paired=T))
  print(wilcox.test(bmc$r[which(bmc$condition=="d1"&bmc$block==4)],
                    bmc$r[which(bmc$condition=="d1"&bmc$block==5)],paired=T))
  print("L1 trials compare to L1")
  print(paste("n grid cells with 2 lights:",length(bmc$r[which(bmc$condition=="l1"&bmc$block==1)])))
  print(summary(bmc$r[which(bmc$condition=="l1"&bmc$block==1)]))
  print(summary(bmc$r[which(bmc$condition=="l1"&bmc$block==2)]))
  print(wilcox.test(bmc$r[which(bmc$condition=="l1"&bmc$block==1)],
                    bmc$r[which(bmc$condition=="l1"&bmc$block==2)], paired =T))
  print(wilcox.test(bmc$r[which(bmc$condition=="l1"&bmc$block==2)],
                    bmc$r[which(bmc$condition=="l1"&bmc$block==3)], paired =T))
  
  
  
  print(paste("linear mixed effect model"))
  tstats.grid<-tstats[which(tstats$clu.id%in% cells$cell.id[which(cells$grid==T)]),]
  tstats.grid$condition<-factor(tstats.grid$condition)
  tstats.grid$mouse<- animalNameFromSessionName(tstats.grid$session)
  ## data on which the model is based
  #boxplot(grid.score ~ condition*mouse,
  #        col=c("gray","red"),tstats.grid)
  gc.model<-lmer(grid.score~condition+(1|mouse)+(1|session),data=tstats.grid)
  print(summary(gc.model))
  ## 
  gc.model.null<-lmer(grid.score~(1|mouse)+(1|session),data=tstats.grid, REML=FALSE)
  gc.model<-lmer(grid.score~condition+(1|mouse)+(1|session),data=tstats.grid,REML=FALSE)
  print(anova(gc.model.null,gc.model))
  ## test the variation of residuals in the two conditions
  print(leveneTest(y=resid(gc.model),group=tstats.grid$condition))
  
  
  ## pairs of grid cells
  print(rep("ifr associations",5))
  ifrg<-ifrAss[which(ifrAss$clu.id1%in%cells$cell.id[which(cells$grid==T)]&
                       ifrAss$clu.id2%in%cells$cell.id[which(cells$grid==T)]),]
  ifrg<-ifrg[which(ifrg$session%in%sessions$session[which(sessions$num.lights==2)]),]
  
  print(paste("Number of grid cell pairs:",length(ifrg$r[which(ifrg$condition=="l1")])))
  print("correlation l1 vs l2")
  cor.test(ifrg$r[which(ifrg$condition=="l1")],
           ifrg$r[which(ifrg$condition=="l2")])
  print("correlation l1 vs d1")
  cor.test(ifrg$r[which(ifrg$condition=="l1")],
           ifrg$r[which(ifrg$condition=="d1")])
  print("Difference between the 2 correlation coefficients")
  cor.diff(cor(ifrg$r[which(ifrg$condition=="l1")],
               ifrg$r[which(ifrg$condition=="l2")]),
           length(ifrg$r[which(ifrg$condition=="l1")]),
           cor(ifrg$r[which(ifrg$condition=="l1")],
               ifrg$r[which(ifrg$condition=="d1")]),
           length(ifrg$r[which(ifrg$condition=="l1")]))
}




################################################
################# START HERE ###################
################################################
load(paste(ep@resultsDirectory,"sessions",sep="/"))
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
load(paste(ep@resultsDirectory,"tstats.shuf",sep="/"))
load(paste(ep@resultsDirectory,"blockMapCor",sep="/"))
load(paste(ep@resultsDirectory,"ifrAss",sep="/"))
source("~/repo/pintegration/analysis/relectro/rename_condition.R") # define function

#############################
#### keep only l1 and d1 ####
#############################
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tstats.shuf<-unsplit(lapply(split(tstats.shuf,tstats.shuf$session),rename.condition.data.frame), tstats.shuf$session)
ifrAss$session<- sessionNameFromCluId(ifrAss$clu.id1)
ifrAss<-unsplit(lapply(split(ifrAss,ifrAss$session),rename.condition.data.frame), ifrAss$session)
tstats<-tstats[which(tstats$condition=="l1"|tstats$condition=="d1"),]
tstats.shuf<-tstats.shuf[which(tstats.shuf$condition=="l1"| tstats.shuf$condition=="d1"),]

grid.cells.on.figure<-c("jp19841-12072015-0108_7",
                        "jp693-12062015-0108_2",
                        "jp19841-13072015-0108_3",
                        "jp19843-30072015-0108_7",
                        "jp19841-02072015-0108_12",
                        "jp5519-15102015-0108_3") 
#grid.cells.on.figure%in%unique(tmaps$clu.id)
maps<-tmaps[which(tmaps$clu.id%in%grid.cells.on.figure),]

grid.cells.stats()
grid.cells.figure()

         
## clean up when done
rm(grid.cells.stats,grid.cells.figure,grid.cells.on.figure,maps)
rm(boxplot.block.condition.correlation,
   boxplot.two.conditions,
   plot.two.distributions)
rm(sessions,tmaps,tstats,tstats.shuf,blockMapCor,ifrAss)
rm(plot.points,barplot.per.mouse)
rm(rename.condition.data.frame)