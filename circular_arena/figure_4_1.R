normalize.rate <-function(x){
  # return a data frame in which the rate is from 0 to 1
  x$rate<-x$rate/max(x$rate)
  return(x)
}
plot.speed.tuning.curve.l.d<-function(x,
                                        axis.y.pos=0,axis.x.pos=40,axis.y.las=2,
                                        mgp.x=c(1,0.3,0.2),mgp.y=c(1,0.3,0.2),xlab="",ylab="",
                                        plotxlim=c(0,30),plotylim=c(40,80),
                                        outma=c(1,1,0.5,0),margin=c(2,2,1,0.3),
                                        xaxis.at=seq(0,30,5),yaxis.at=seq(40,80,10),...)
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=axis.x.pos, at=xaxis.at, tck=-0.05,cex.axis=0.60,mgp=mgp.x)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.y)
  x$mid.speed<-x$min.speed+(x$max.speed-x$min.speed)/2
  x1<-x[which(x$condition=="l"),]
  lines (x1$mid.speed,x1$rate,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="red")
  x1<-x[which(x$condition=="d"),]
  lines (x1$mid.speed,x1$rate,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
}
distribution.shuffled.threshold.plot<-function(x,s,t,
                                               min=-0.8,max=0.8,int=0.025,axis.y.pos=-0.4,axis.x.pos=0,axis.y.las=2,
                                               plotxlim=c(-0.4,0.8),plotylim=c(0,0.2),outma=c(1,1,1,0),margin=c(2,2,1,1),mgp=c(1.2,0.3,0),
                                               probability=1, xaxis.at=seq(-0.4,0.8,0.4),ylab="",xlab="",
                                               data.color="purple1",...)
{
  # plot a histogram of shuffled data with a line of the real data and a vertical bar indicating the threshold
  # x: vector of real data
  # s: vector of shuffled data
  # t: threshold
  
  par(oma=outma,mar=margin,mgp=mgp,cex.lab=0.6,cex.axis=0.6)
  #  plot (x=plotxlim, y=plotylim, type='n',axes=FALSE, pch=20,lwd=1,xlab="",ylab="",...)
  ## plot shuffled distribution
  h<-hist(s,seq(min,max,int),plot=FALSE)
  if(probability==1)
  {plot(h$mids, h$counts/sum(h$counts),xlim=plotxlim,ylim=plotylim,axes=FALSE, type='h',pch=20,xlab='',ylab='',lwd=0.75)}
  else
  {plot(h$mids, h$counts,xlim=plotxlim,ylim=plotylim, type='h',pch=20,axes=FALSE,xlab='',ylab='',lwd=0.75)}
  # plot data distribution
  h<-hist(x,seq(min,max,int),plot=FALSE)
  if(probability==1)
  {lines(h$mids, h$counts/sum(h$counts), type='l',pch=20,xlab='',ylab='',lwd=1,col=data.color)}
  else
  {lines(h$mids, h$counts, type='l',pch=20,xlab='',ylab='',lwd=1,col=data.color)}
  # plot the threshold
  lines(c(t,t),c(0,plotylim[2]),ylab="",xlab="",lty=2,lwd=2)
  axis(side = 1, at=xaxis.at, pos=axis.x.pos,tck=-0.05,cex.axis=0.60)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  title(xlab=xlab,mgp=mgp)
  title(ylab=ylab,mgp=mgp)
}
plot.hist.one.distribution<-function(x, min=-0.8,max=0.8,int=0.025,axis.y.pos=-0.5,axis.x.pos=0,axis.y.las=2,
                                     plotxlim=c(-0.5,1),plotylim=c(0,20),outma=c(1,1,1,0),margin=c(2,2,1,1),
                                     mgp.x=c(1.2,0.3,0),mgp.y=c(1.2,0.3,0),
                                     probability=1, xaxis.at=seq(-0.4,0.8,0.4),ylab="",xlab="",vline="",...)
{
  # plot a histogram
  # x: vector of real data
  par(oma=outma,mar=margin,cex.lab=0.6,cex.axis=0.6)
  hist(x,xlim=plotxlim,ylim=plotylim,breaks=seq(min,max,int),axes=F,labels=F,main="",xlab="",ylab="",...)
  if(vline!="")
  {
    lines(c(vline,vline),c(plotylim[1],plotylim[2]),col="green",lty=5,lwd=2)
  }
  axis(side = 1, at=xaxis.at, pos=axis.x.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.x)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.y)
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
}
plot.mean.sem.per.group.time <- function(data,dv="dv",time="iv",group="group",xbin.add=0,
                                         axis.y.pos=-180,axis.x.pos=0,axis.y.las=2,
                                         arrow.lwd=0.25,arrow.length=0.005,
                                         main.title="",mgp.x=c(1,0.3,0.2),mgp.y=c(1,0.3,0.2),xlab="",ylab="",
                                         plotxlim=c(-180,180),plotylim=c(0,30),outma=c(1,1,0.5,0),margin=c(2,2,1,0.3),
                                         xaxis.at=seq(-180,180,90),yaxis.at=seq(0,1,0.2),cex.point=0.1,legend.lab=c(""),...)
{
  group.list <- unique(data[,group])
  mean.dv <- tapply(data[,dv],list(data[,time],data[,group]),mean,na.rm=T)
  sem.dv <-  tapply(data[,dv],list(data[,time],data[,group]),sem)
  tp <- as.numeric(names(table(data[,time])))
  
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  
  par(mgp=mgp.x)
  axis(side = 1, pos=axis.x.pos, at=xaxis.at, tck=-0.05,cex.axis=0.60)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  
  index=1
  for (g in seq(1,length(group.list)))
  {
    lines (tp,mean.dv[,g],type='l',pch=20,xlab='',ylab='',lwd=0.75,col=index)
    index=index+1
  }
  index=1
  a=0
  for (g in seq(1,length(group.list)))
  {
    for (i in seq(tp))
    {
      arrows(tp[i],mean.dv[i,g]-sem.dv[i,g],tp[i],mean.dv[i,g]+sem.dv[i,g],code=3,angle=90,length=arrow.length,lwd=arrow.lwd,col=index)
    }
    index=index+1
  }
  
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
  if(main.title!=""){
    title(main=main.title,cex.main=0.6)
  }
  if(legend.lab[1]!="")
  {
    legend(x=plotxlim[1],y=plotylim[2],legend.lab,
           cex=0.6,lty=1,bty="n",
           col = seq(1,length(group.list)))
    
  }
}
speed.cells.figure<-function()
{
  m<-rbind(c(0.00,0.20,0.80,1.00),
           c(0.00,0.20,0.60,0.80),
           c(0.00,0.20,0.40,0.60),
           c(0.00,0.20,0.20,0.40),
           c(0.00,0.20,0.00,0.20),
           
           c(0.20,0.40,0.80,1.00),
           c(0.20,0.40,0.60,0.80),
           c(0.20,0.40,0.40,0.60),
           c(0.20,0.40,0.20,0.40),
           c(0.20,0.40,0.00,0.20),
           
           c(0.40,0.65,0.80,1.00),
           c(0.40,0.65,0.60,0.80),
           c(0.40,0.65,0.40,0.60),
           c(0.40,0.65,0.20,0.40),
           c(0.40,0.65,0.00,0.20),
           
           c(0.70,1.00,0.80,1.00),
           c(0.70,1.00,0.60,0.80),
           c(0.70,1.00,0.40,0.60),
           c(0.70,1.00,0.20,0.40),
           c(0.70,1.00,0.00,0.20))    
  fn<-paste(ep@directory,"figures_relectro","speed_figure_4_1.pdf",sep="/")
  print(paste("creating",fn))
  pdf(file=fn,onefile=TRUE,width=5,height=6)
  #x11(width=5,height=6)
  split.screen(m)
  ### examples of speed cells
  for(i in 1:length(speed.cells))
  {
    conds<-sort(unique(maps.examples$condition[which(maps.examples$clu.id==speed.cells[i])]))
    ## rate maps
    screen(i)
    x<-maps.examples[which(maps.examples$clu.id==speed.cells[i]&maps.examples$condition==conds[3]),]
    firingRateMapPlot(x,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    screen(5+i)
    x<-maps.examples[which(maps.examples$clu.id==speed.cells[i]&maps.examples$condition==conds[1]),]
    firingRateMapPlot(x,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    
    ## tuning curves
    screen(10+i)
    x<-srTuningCurve[which(srTuningCurve$clu.id==speed.cells[i]&(srTuningCurve$condition=="l"|srTuningCurve$condition=="d")),]
    max.y<-ceiling(max(x$rate,na.rm=T))
    if(max.y>20){
      max.y<-signif(max.y,digits=1)
    }
    min.y<-signif(floor(min(x$rate,na.rm=T)),digits=1)
    plot.speed.tuning.curve.l.d(x,xlab="Speed (cm/sec)",ylab="Firing rate (Hz)",margin=c(2,2,1,0.3),
                                plotylim=c(min.y,max.y),axis.x.pos=min.y)
    
  }
  # distribution and shuffle speed score mec
  screen(16)
  x<- bstats$speed.score
  y<- bstats.shuf$speed.score
  distribution.shuffled.threshold.plot(x=x,y,t=as.numeric(quantile(y,0.95,na.rm=T)),
                                       plotylim=c(0,0.25),xlab="Speed score",ylab="Probability",
                                       margin=c(2,2,1,1),outma=c(1,1,0.5,0))
  
  # mean and sem of speed tuning curves mec
  screen(17)
  cond<-c("d","l")
  rss<-srTuningCurve[which(srTuningCurve$clu.id%in%cells$cell.id[which(cells$speed==T)]),]
  rssn<-unsplit(lapply(split(rss,rss$clu.id),normalize.rate), rss$clu.id) # normalize rate
  plot.mean.sem.per.group.time(data=rssn,dv="rate",time="mid",group="condition",xbin.add=0,
                               axis.y.pos=0,axis.x.pos=0.4,axis.y.las=2,
                               arrow.lwd=0.25,arrow.length=0.005,
                               main.title="",mgp.x=c(0.8,0.3,0.2),mgp.y=c(1.0,0.3,0.1),
                               plotxlim=c(0,30),plotylim=c(0.4,1),outma=c(1,1,0.5,0),margin=c(2,2,1,1),
                               xaxis.at=seq(0,30,5),yaxis.at=seq(0,1,.2),cex.point=0.1,legend.lab=c("Dark","Light"),
                               xlab="Speed (cm/sec)",ylab="Firing rate (norm.)")
  # plot the distribution of changes in slope between light and dark
  screen(18)
  x<-speedRateT[which(speedRateT$clu.id%in%cells$cell.id[which(cells$speed==T)]),]
  plot.hist.one.distribution(x$s.l-x$s.d,min=-2,max=3,int=0.025,
                             plotxlim=c(-0.5,0.75),plotylim=c(0,80),
                             axis.y.pos=-0.5,
                             xaxis.at=seq(-0.5,.75,0.25),
                             ylab="Neurons",xlab=expression(Delta * " slope (l1 - d1)"),
                             vline=0, col="grey",
                             mgp.x=c(1.0,0.5,0), mgp.y=c(1.2,0.9,0))
  # plot the distribution of changes in speed score between light and dark
  screen(19)
  plot.hist.one.distribution(x$r.l-x$r.d,min=-2,max=3,int=0.02,
                             plotxlim=c(-0.25,0.5),plotylim=c(0,60),
                             xaxis.at=seq(-0.25,.5,0.25),
                             axis.y.pos=-0.25,
                             ylab="Neurons",xlab=expression(Delta * " speed score (l1 - d1)"),
                             vline=0, col="grey",
                             mgp.x=c(1.0,0.5,0), mgp.y=c(1.2,0.9,0))
  # plot the distribution of changes in intercept
  screen(20)
  plot.hist.one.distribution(x$i.l-x$i.d,min=-20,max=40,int=1,
                             plotxlim=c(-15,20),plotylim=c(0,100),
                             axis.y.pos=-15,axis.x.pos=0,
                             xaxis.at=seq(-15,20,5),
                             ylab="Neurons",xlab=expression(Delta * " intercept (l1 - d1)"),
                             vline=0, col="grey",
                             mgp.x=c(1.0,0.5,0), mgp.y=c(1.2,0.9,0),
                             margin=c(2,2,1,1))
  close.screen(all=TRUE)
  dev.off()
}

speed.cell.stats<-function(){
  ## sanity check
  print("******************************")
  print("*** speed-modulated cells ****")
  print("******************************")
  
  print(paste("Number of cells:",length(cells$cell.id)))
  print(paste("Number of speed-modulated cells:",sum(cells$speed==T)))
  print(paste("Proportion speed-modulated cells:",round(sum(cells$speed)/length(cells$cell.id), 4)))
 
  print("correlation between speed score and information score during l1")
  print(cor.test(tstats$info.score[which(tstats$condition=="l1")],
                 tstats$grid.score[which(tstats$condition=="l1")]))
 
  print("correlation speed scores and mean firing rates during l1")
  print(cor.test(tstats$mean.rate[which(tstats$condition=="l1")],
                 tstats$grid.score[which(tstats$condition=="l1")]))
  
  print("grid cells that are also speed-modulated cells")
  print(paste(length(cells$cell.id[which(cells$grid==T&cells$speed==T)]),"out of", 
              length(cells$cell.id[which(cells$grid==T)])))
  print(paste("proportion of grid cells:",round(length(cells$cell.id[which(cells$grid==T&cells$speed==T)])/length(cells$cell.id[which(cells$grid==T)]),4)))
  
  
  print("irregular spatial selective cells that are also speed-modulated cells")
  print(paste(length(cells$cell.id[which(cells$place==T&cells$speed==T)]),"out of", 
              length(cells$cell.id[which(cells$place==T)])))
  print(paste("proportion of iss cells:",round(length(cells$cell.id[which(cells$place==T&cells$speed==T)])/length(cells$cell.id[which(cells$place==T)]),4)))
  
  
  print("border cells that are also speed-modulated cells")
  print(paste(length(cells$cell.id[which(cells$border==T&cells$speed==T)]),"out of", 
              length(cells$cell.id[which(cells$border==T)])))
  print(paste("proportion of border cells:",round(length(cells$cell.id[which(cells$border==T&cells$speed==T)])/length(cells$cell.id[which(cells$border==T)]),4)))
  
  print("hd cells that are also speed-modulated cells")
  print(paste(length(cells$cell.id[which(cells$hd==T&cells$speed==T)]),"out of", 
              length(cells$cell.id[which(cells$hd==T)])))
  print(paste("proportion of border cells:",round(length(cells$cell.id[which(cells$hd==T&cells$speed==T)])/length(cells$cell.id[which(cells$hd==T)]),4)))
  
  print("mean rate > 10 cells that are also speed-modulated cells")
  print(paste(length(cells$cell.id[which(cells$mean.rate>10&cells$speed==T)]),"out of",
              length(cells$cell.id[which(cells$mean.rate>10)])))
  print(paste("proportion of mean rate > 10 cells s:",round(length(cells$cell.id[which(cells$mean.rate>10&cells$speed==T)])/
                                                            length(cells$cell.id[which(cells$mean.rate>10)]),4)))
  
  x<-speedRateT[which(speedRateT$clu.id%in%cells$cell.id[which(cells$speed==T)]),]
  print("change in slope between l1 and d1")
  print(paste("Number of speed cells:",length(x$s.l)))
  print("slope l1")
  print(summary(x$s.l))
  print("slope d1")
  print(summary(x$s.d))
  print("slope change")
  print(summary(x$s.l-x$s.d))
  print(wilcox.test(x$s.l,x$s.d,paired=T))
  
  print("change in intercept between l1 and d1")
  print("intercept l1")
  print(summary(x$i.l))
  print("intercept d1")
  print(summary(x$i.d))
  print("intercept change")
  print(summary(x$i.l-x$i.d))
  print(wilcox.test(x$i.l,x$i.d,paired=T))
  
  print("change in speed scores between l1 and d1")
  print("r l1")
  print(summary(x$r.l))
  print("r d1")
  print(summary(x$r.d))
  print("r change")
  print(summary(x$r.l-x$r.d))
  print(wilcox.test(x$r.l,x$r.d,paired=T))
  
  print("change in mean firing rate between l1 and d1")  
  print(paste("Number of speed cells",length(tstats$mean.rate[which(tstats$condition=="l1")])))
  print("rate l1")
  print(summary(tstats$mean.rate[which(tstats$condition=="l1")]))
  print("rate d1")
  print(summary(tstats$mean.rate[which(tstats$condition=="d1")]))
  print(wilcox.test(tstats$mean.rate[which(tstats$condition=="l1")],
                    tstats$mean.rate[which(tstats$condition=="d1")],paired=T))
  
  ch<-(tstats$mean.rate[which(tstats$condition=="l1")]-tstats$mean.rate[which(tstats$condition=="d1")])/tstats$mean.rate[which(tstats$condition=="d1")]*100
  print("change in % form d1 to l1 trials")
  print(summary(ch))
  
  print("change in running speed between light and dark")
  print(paste("Number of sessions:",
              length(rSpeed$session[which(rSpeed$condition=="l1")])))
  print("speed in l1")
  print(summary(rSpeed$mean.speed[which(rSpeed$condition=="l1")]))
  print("speed in d1")
  print(summary(rSpeed$mean.speed[which(rSpeed$condition=="d1")]))
  print(wilcox.test(rSpeed$mean.speed[which(rSpeed$condition=="l1")],
                    rSpeed$mean.speed[which(rSpeed$condition=="d1")],
                    paired = T))
  
  
  print("Stats when only considering mec tetrodes")
  x<-speedRateT[which(speedRateT$clu.id%in%cells$cell.id[which(cells$speed==T&cells$region=="mec")]),]
  print("change in slope between l1 and d1")
  print(paste("Number of speed cells:",length(x$s.l)))
  print("slope l1")
  print(summary(x$s.l))
  print("slope d1")
  print(summary(x$s.d))
  print("slope change")
  print(summary(x$s.l-x$s.d))
  print(wilcox.test(x$s.l,x$s.d,paired=T))
  
  print("change in intercept between l1 and d1")
  print("intercept l1")
  print(summary(x$i.l))
  print("intercept d1")
  print(summary(x$i.d))
  print("intercept change")
  print(summary(x$i.l-x$i.d))
  print(wilcox.test(x$i.l,x$i.d,paired=T))
  
  print("change in speed scores between l1 and d1")
  print("r l1")
  print(summary(x$r.l))
  print("r d1")
  print(summary(x$r.d))
  print("r change")
  print(summary(x$r.l-x$r.d))
  print(wilcox.test(x$r.l,x$r.d,paired=T))
  
  print(rep("mec",15))
  print("Stats when only considering mec tetrodes")
  x<-speedRateT[which(speedRateT$clu.id%in%cells$cell.id[which(cells$speed==T&cells$region=="mec")]),]
  print("change in slope between l1 and d1")
  print(paste("Number of speed cells:",length(x$s.l)))
  print("slope l1")
  print(summary(x$s.l))
  print("slope d1")
  print(summary(x$s.d))
  print("slope change")
  print(summary(x$s.l-x$s.d))
  print(wilcox.test(x$s.l,x$s.d,paired=T))
  
  print("change in intercept between l1 and d1")
  print("intercept l1")
  print(summary(x$i.l))
  print("intercept d1")
  print(summary(x$i.d))
  print("intercept change")
  print(summary(x$i.l-x$i.d))
  print(wilcox.test(x$i.l,x$i.d,paired=T))
  
  print("change in speed scores between l1 and d1")
  print("r l1")
  print(summary(x$r.l))
  print("r d1")
  print(summary(x$r.d))
  print("r change")
  print(summary(x$r.l-x$r.d))
  print(wilcox.test(x$r.l,x$r.d,paired=T))
  
  
  print("mouse aggregate")
  x<-speedRateT[which(speedRateT$clu.id%in%cells$cell.id[which(cells$speed==T)]),]
  x$session<-sessionNameFromCluId(x$clu.id)
  x$mouse<- animalNameFromSessionName(x$session)
  x$r.diff<-x$r.l-x$r.d
  x$i.diff<-x$i.l-x$i.d
  x$s.diff<-x$s.l-x$s.d 
  
  print("Difference of speed scores")
  x.agg<-aggregate(x$r.diff,by=list(x$mouse),median)
  colnames(x.agg)<-c("mouse","r.diff")
  print(summary(x.agg))
  print(wilcox.test(x.agg$r.diff))
  
  print("Difference of intercept")
  x.agg<-aggregate(x$i.diff,by=list(x$mouse),median)
  colnames(x.agg)<-c("mouse","i.diff")
  print(summary(x.agg))
  print(wilcox.test(x.agg$i.diff))
 
  print("Difference of slope")
  x.agg<-aggregate(x$s.diff,by=list(x$mouse),median)
  colnames(x.agg)<-c("mouse","s.diff")
  print(summary(x.agg))
  print(wilcox.test(x.agg$s.diff))
}


##############################################
########### load data.frames #################
##############################################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@resultsDirectory,"sessions",sep="/"))
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"speedRateT",sep="/"))
load(paste(ep@resultsDirectory,"srTuningCurve",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
load(paste(ep@resultsDirectory,"bstats",sep="/"))
load(paste(ep@resultsDirectory,"bstats.shuf",sep="/"))
load(paste(ep@resultsDirectory,"rSpeed",sep="/"))

tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
bstats<-unsplit(lapply(split(bstats,bstats$session),rename.condition.data.frame), bstats$session)

if(length(speedRateT$clu.id)!=length(cells$cell.id))
  stop("problem with length of speedRateT")

## 5 examples of speed cells
speed.cells<-c("jp19841-04072015-0108_6",
               "jp19841-27062015-0108_10",
               "jp693-11062015-0108_4",
               "jp19841-18072015-0108_6",
               "jp693-17062015-0108_7")
maps.examples<-tmaps[which(tmaps$clu.id%in%speed.cells),]
speed.cells %in% tmaps$clu.id

## get the stats
speed.cell.stats()
speed.cells.figure()

rm(speed.cell.stats,speed.cells.figure,speed.cells,maps.examples)
rm(sessions,tmaps,speedRateT,srTuningCurve,tstats,bstats,bstats.shuf,rSpeed)
rm(normalize.rate,plot.hist.one.distribution,plot.mean.sem.per.group.time,plot.speed.tuning.curve.l.d,rename.condition.data.frame)
rm(distribution.shuffled.threshold.plot)
