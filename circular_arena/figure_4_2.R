detect.crosscorrelation.peak<-function(x){
  detection.period=c(0,4)
  baseline.period1=c(-10,0)
  baseline.period2=c(10,50)
  min.spikes=300  
  if(sum(x$count)<min.spikes)
  {
    return(NA)
  }
  
  peak.index<-x$time>detection.period[1]&x$time<detection.period[2]
  baseline.index<-(x$time>baseline.period1[1]&x$time<baseline.period1[2]) | (x$time>baseline.period2[1]&x$time<baseline.period2[2])
  count.peak<-x$count[peak.index]
  count.baseline<-x$count[baseline.index]
  
  m<-mean(count.baseline)
  std<-sd(count.baseline)
  peak<-max(count.peak)  
  peak.baseline<-max(count.baseline)
  z<-(peak-m)/std
  
  ## if there is a peak in the baseline, remove the pair
  if(peak.baseline>(peak*0.75)){
    z=NA}
  
  return(z)
}

normalize.rate <-function(x){
  # return a data frame in which the rate is from 0 to 1
  x$rate<-x$rate/max(x$rate)
  return(x)
}
plot.xcorrelation<-function(significant.pairs,significant.xc){
  plot.per.page=30
  num.cols<-5
  num.rows<-6
  m<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
              rep(seq(1/num.cols,1,1/num.cols),num.rows),
              rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
              rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  fn=paste(ep@directory,"figures_relectro","significant_connections.pdf",sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,paper="a4",width=8,height=10)
  index=1
  for (cellid in significant.pairs) ## any cell list
  {
    if(index==1)
    {
      split.screen(m)  
    }
    screen(index)
    ## insert your plot function here
    x<-significant.xc[which(significant.xc$pair.id==cellid),]
    par(mar=c(2,2,1,0.3), oma=c(1,1,1,1),cex.lab=0.6,cex.axis=0.6)
    plot(x=x$time,y=x$count,type='l',xlim=c(-20,20))
    lines(x=x$time[which(x$time>0&x$time<4)],y=x$count[which(x$time>0&x$time<4)],col="red")    
    if(index==plot.per.page)
    {
      close.screen( all = TRUE )
      index=0
    }
    index=index+1
  }
  close.screen(all = TRUE)
  dev.off()
}
stats.pre.post<-function(){
  
  print(paste("Number of pairs:",length(unique(stcc$pair.id))))
  print(paste("Number of within tetrode pairs:",length(pairs$pair.id[which(pairs$pre.tetrode.id==pairs$post.tetrode.id)])))
  print(paste("Number of between tetrode pairs:",length(pairs$pair.id[which(pairs$pre.tetrode.id!=pairs$post.tetrode.id)])))
  print(paste("Number of within hemisphere pairs:",length(pairs$pair.id[which(pairs$pre.hemisphere==pairs$post.hemisphere)])))
  print(paste("Number of between hemisphere pairs:",length(pairs$pair.id[which(pairs$pre.hemisphere!=pairs$post.hemisphere)])))
  
  print(paste("Number of connected pairs:",sum(pairs$connected)))
  print(paste("Number of connected pairs within tetrode:",sum(pairs$connected[which(pairs$pre.tetrode.id==pairs$post.tetrode.id)])))
  print(paste("Number of connected pairs between tetrodes:",sum(pairs$connected[which(pairs$pre.tetrode.id!=pairs$post.tetrode.id)])))
  print(paste("Number of connected pairs within hemisphere:",sum(pairs$connected[which(pairs$pre.hemisphere==pairs$post.hemisphere)])))
  print(paste("Number of connected pairs between hemispheres:",sum(pairs$connected[which(pairs$pre.hemisphere!=pairs$post.hemisphere)])))
  
  print(paste("Probability to be connected within a tetrode:",
              round(sum(pairs$connected[which(pairs$pre.tetrode.id==pairs$post.tetrode.id)])/
                      length(pairs$connected[which(pairs$pre.tetrode.id==pairs$post.tetrode.id)]),4)))            
  print(paste("Probability to be connected across tetrodes:",
              round(sum(pairs$connected[which(pairs$pre.tetrode.id!=pairs$post.tetrode.id)])/
                      length(pairs$connected[which(pairs$pre.tetrode.id!=pairs$post.tetrode.id)]),4)))            
  
  print(paste("Number of cells that are postsynaptic:",length(cells$cell.id[which(cells$postsynaptic==T)])))
  print(paste("Number of speed cells that are postsynaptic:",length(cells$cell.id[which(cells$speed==T&cells$postsynaptic==T)])))
  print(paste("Number of cells that are presynaptic:",length(cells$cell.id[which(cells$presynaptic==T)])))
  print(paste("Number of speed cells that are presynaptic:",length(cells$cell.id[which(cells$speed==T&cells$presynaptic==T)])))
  
  m<-matrix(c(length(cells$cell.id[which(cells$speed==T&cells$presynaptic==T)]),
              length(cells$cell.id[which(cells$speed==F&cells$presynaptic==T)]),
              length(cells$cell.id[which(cells$speed==T&cells$postsynaptic==T)]),
              length(cells$cell.id[which(cells$speed==F&cells$postsynaptic==T)])),ncol=2)
  
  print(paste("Presynaptic speed cells:", m[1,1]))
  print(paste("Presynaptic non-speed cells:", m[2,1]))
  print(paste("Postsynaptic speed cells:", m[1,2]))
  print(paste("Postsynaptic non-speed cells:", m[2,2]))
  print(paste("Proportion speed cells as presynaptic:", round(m[1,1]/(m[1,1]+m[2,1]),3)))
  print(paste("Proportion speed cells as postsynaptic:", round(m[1,2]/(m[1,2]+m[2,2]),3)))
  print(chisq.test(m))
  
  ## PV neurons
  print("Chi-sq pv cells, proportion of speed cells in PV vs proportion of PV in ni")
  print(chisq.test(matrix(c(72,68,308,688),ncol=2)))
  
  ######################################
  ## presynaptic cells to speed cells ##
  ######################################
  print("Presynaptic cells to speed cells")
  pre.to.speed<-pairs$pre.id[which(pairs$connected==T& pairs$post.id%in%cells$cell.id[which(cells$speed==T)] )]
  print("Mean firing rate l1 vs d1")
  print(paste("n pre cells to speed:",length(unique(pre.to.speed))))
  print(summary(tstats$mean.rate[which(tstats$clu.id%in%pre.to.speed&tstats$condition=="l1")]))
  print(summary(tstats$mean.rate[which(tstats$clu.id%in%pre.to.speed&tstats$condition=="d1")]))
  print(wilcox.test(tstats$mean.rate[which(tstats$clu.id%in%pre.to.speed&tstats$condition=="l1")],
                    tstats$mean.rate[which(tstats$clu.id%in%pre.to.speed&tstats$condition=="d1")],
                    paired=T))
  
  
  ##############################
  ## postsynaptic speed cells ##
  ##############################
  print("Postsynaptic speed cells")
  print("Mean firing rate l1 vs d1")
  speed.post<-cells$cell.id[which(cells$speed==T&cells$postsynaptic==T)]
  print(paste("n speed post cells:",length(unique(speed.post))))
  print(summary(tstats$mean.rate[which(tstats$clu.id%in%speed.post&tstats$condition=="l1")] ))
  print(summary(tstats$mean.rate[which(tstats$clu.id%in%speed.post&tstats$condition=="d1")] ))
  print(wilcox.test(tstats$mean.rate[which(tstats$clu.id%in%speed.post&tstats$condition=="l1")],
                    tstats$mean.rate[which(tstats$clu.id%in%speed.post&tstats$condition=="d1")],paired=T))
  
  
  x<-srTuningCurve[which(srTuningCurve$clu.id%in%pre.to.speed),]
  x<-unsplit(lapply(split(x,x$clu.id),normalize.rate), x$clu.id) # normalize rate
  print(paste("Number of pre to speed cells:",length(unique(x$clu.id))))
  
  print("Comparing firing rate 2.5 vs 27.5, light")
  length(x$rate[which(x$condition=="l"&x$mid==2.5)])
  summary(x$rate[which(x$condition=="l"&x$mid==2.5)])
  summary(x$rate[which(x$condition=="l"&x$mid==27.5)])
  wilcox.test(x$rate[which(x$condition=="l"&x$mid==2.5)],
              x$rate[which(x$condition=="l"&x$mid==27.5)],paired=T)
  print("Comparing firing rate 2.5 vs 27.5, dark")
  length(x$rate[which(x$condition=="d"&x$mid==2.5)])
  summary(x$rate[which(x$condition=="d"&x$mid==2.5)])
  summary(x$rate[which(x$condition=="d"&x$mid==27.5)])
  wilcox.test(x$rate[which(x$condition=="d"&x$mid==2.5)],
              x$rate[which(x$condition=="d"&x$mid==27.5)],paired=T)
}





boxplot.two.conditions <- function(data,vd="condition",vi="measured",
                                   outma=c(0.5,0.5,0.5,0),margin=c(1,1.7,0.5,0.3),
                                   mgp.x=c(1.2,0.5,0), mgp.y=c(1,0.5,0),
                                   at.yaxis=seq(0,1,0.2),ylab="",...)
{
  #set xlim to c(0,3) and outline=T or F when calling the function
  par(oma=outma,mar=margin,mgp=mgp.x)
  boxplot(data[,vi]~data[,vd],
          frame.plot=FALSE,axes=FALSE,
          col=c("red","gray"),xlim=c(0,3),...)
  par(mgp=mgp.x)
  axis(side = 1, at=1:2, pos=0,tck=-0.05,cex.axis=0.6,labels=c("",""))
  par(mgp=mgp.y)
  axis(side = 2, at=at.yaxis,   las=1,pos=0, tck=-0.05,cex.axis=0.6,xpd=TRUE)
  title(ylab=ylab,mgp=mgp.y)
}

plot.spike.time.correlation<-function(x,
                                      axis.y.pos=-25,axis.x.pos=0,axis.y.las=2,
                                      mgp.x=c(0.7,0.2,0.1),mgp.y=c(1,0.3,0.2),xlab="",ylab="",
                                      plotxlim=c(-25,25),plotylim=c(0,0.06),
                                      outma=c(0.5,0.5,0.5,0),margin=c(1.7,1.7,0.5,0.3),
                                      xaxis.at=seq(-30,30,10),yaxis.at=seq(0,0.06,0.01),
                                      red.peak="",...)
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=axis.x.pos, at=xaxis.at, tck=-0.05,cex.axis=0.60,mgp=mgp.x)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.y)
  lines (x$time,x$prob,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
  if(red.peak!="")
  {
    lines(x$time[which(x$time>0&x$time<4)],x$prob[which(x$time>0&x$time<4)],col="blue")
  }
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

speed.pre.post.figure<-function(){
  m<-rbind(c(0.00,0.30,0.75,1.00),
           c(0.00,0.30,0.50,0.75),
           c(0.00,0.30,0.25,0.50),
           
           c(0.30,0.60,0.75,1.00),
           c(0.30,0.60,0.50,0.75),
           c(0.30,0.60,0.25,0.50),
           
           c(0.00,0.60,0.00,0.25),
           
           c(0.60,1.00,0.75,1.00),
           c(0.60,1.00,0.50,0.75),
           c(0.60,1.00,0.25,0.50),
           c(0.60,1.00,0.00,0.25))
  fn=paste(ep@directory,"figures_relectro", "speed_pre_post_figure_4_2.pdf",sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,width=3,height=4)
  #x11(width=3,height=4)
  split.screen(m)
  ## keep only pairs with a post speed cells
  speed.post<-cells$cell.id[which(cells$speed==T&cells$postsynaptic==T)]
  
  pairs.c<-pairs[which(pairs$post.id%in%speed.post&pairs$connected==T),]
  
  cc<-stcc[which(stcc$pair.id%in%pairs.c$pair.id),]
  
  pn<-14 ## illustrated pair
  
  screen(2)
  x<-stac[which(stac$clu.id==pairs.c$pre.id[pn]&stac$time>-25&stac$time<25),]
  plot.spike.time.correlation(x,xlab="Time (ms)",ylab="Spike probability",
                              plotylim = c(0,0.03),plotxlim=c(-25,25),axis.y.pos=-25,
                              xaxis.at=seq(-30,30,10))
  screen(5)
  x<-maps[which(maps$clu.id==pairs.c$pre.id[pn]&maps$condition=="l1"),]
  firingRateMapPlot(x,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
  
  screen(3)
  x<-stac[which(stac$clu.id==pairs.c$post.id[pn]&stac$time>-25&stac$time<25),]
  plot.spike.time.correlation(x,xlab="Time (ms)",ylab="Spike probability",
                              plotylim = c(0,0.04),plotxlim=c(-25,25),axis.y.pos=-25,
                              xaxis.at=seq(-30,30,10))
  screen(6)
  x<-maps[which(maps$clu.id==pairs.c$post.id[pn]&maps$condition=="l1"),]
  firingRateMapPlot(x,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
  
  screen(7)
  x<-stccp[which(stccp$clu.id1==pairs.c$pre.id[pn]&
                   stccp$clu.id2==pairs.c$post.id[pn]),]
  plot.spike.time.correlation(x,xlab="Time (ms)",ylab="Spike probability",
                              plotylim = c(0,0.1),plotxlim=c(-50,50),axis.y.pos=-50,
                              xaxis.at=seq(-50,50,25),red.peak="1")
  screen(9)
  pre.to.speed<-pairs$pre.id[which(pairs$connected==T& pairs$post.id%in%cells$cell.id[which(cells$speed==T)] )]
  x<-tstats[which(tstats$clu.id%in%pre.to.speed&(tstats$condition=="l1"|tstats$condition=="d1")),]
  x$condition<-factor(x$condition,levels=c("l1","d1"))
  boxplot.two.conditions(data=x,vd="condition",vi="mean.rate", at.yaxis=seq(0,8,1),ylab="Mean rate (Hz)",xxlim=c(0,3),outline=F,ylim=c(0,8))
  
  screen(10)
  speed.post<-cells$cell.id[which(cells$speed==T&cells$postsynaptic==T)]
  x<-tstats[which(tstats$clu.id%in%speed.post&(tstats$condition=="l1"|tstats$condition=="d1")),]
  x$condition<-factor(x$condition,levels=c("l1","d1"))
  boxplot.two.conditions(data=x,vd="condition",vi="mean.rate", at.yaxis=seq(0,80,10),ylab="Mean rate (Hz)",ylim=c(0,80),outline=F)
  
  screen(11)
  x<-srTuningCurve[which(srTuningCurve$clu.id%in%pre.to.speed),]
  x<-unsplit(lapply(split(x,x$clu.id),normalize.rate), x$clu.id) # normalize rate
  plot.mean.sem.per.group.time(data=x,dv="rate",time="mid",group="condition",xbin.add=0,
                               axis.y.pos=0,axis.x.pos=0.3,axis.y.las=2,
                               arrow.lwd=0.25,arrow.length=0.005,
                               main.title="",mgp.x=c(0.8,0.3,0.2),mgp.y=c(1.0,0.3,0.1),
                               plotxlim=c(0,30),plotylim=c(0.3,1),
                               outma=c(0.5,0.5,0.5,0),margin=c(1.7,1.7,0.5,0.3),
                               xaxis.at=seq(0,30,5),yaxis.at=seq(0,1,.2),cex.point=0.1,legend.lab=c("Dark","Light"),
                               xlab="Speed (cm/sec)",ylab="Firing rate (norm.)")
  
  
  close.screen(all=TRUE)
  dev.off()
}




##############################################
########### load data.frames #################
##############################################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"srTuningCurve",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
load(paste(ep@resultsDirectory,"stcc",sep="/"))
load(paste(ep@resultsDirectory,"stac",sep="/"))
load(paste(ep@resultsDirectory,"stccp",sep="/"))


tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)



## create a data.frame with information (tetrode, hemisphere) about the pairs
pairs<-data.frame(pair.id=unique(stcc$pair.id))
pairs$pre.id<-unlist(strsplit(as.character(pairs$pair.id),".",fixed=T))[seq(1,length(pairs$pair.id)*2,by=2)]
pairs$post.id<-unlist(strsplit(as.character(pairs$pair.id),".",fixed=T))[seq(2,length(pairs$pair.id)*2,by=2)]
pairs<-merge(pairs,cells[,c("cell.id","hemisphere","tetrode.id")],by.x="pre.id",by.y="cell.id")
colnames(pairs)<-c("pre.id","pair.id","post.id","pre.hemisphere","pre.tetrode.id")
pairs<-merge(pairs,cells[,c("cell.id","hemisphere","tetrode.id")],by.x="post.id",by.y="cell.id")
colnames(pairs)<-c("post.id","pre.id","pair.id","pre.hemisphere","pre.tetrode.id","post.hemisphere","post.tetrode.id")

## identify connected pairs
b<-by(stcc,list(stcc$pair.id),detect.crosscorrelation.peak)
cc.peak<-data.frame(pair.id=names(b),peak=as.numeric(b))
if(length(cc.peak$peak)!=length(pairs$pair.id))
  stop("problem with length of cc.peak")
pairs<-merge(pairs,cc.peak)
pairs$connected<-FALSE
pairs$connected[which(pairs$peak>5)]<-TRUE
presynaptic.cells<-unique(pairs$pre.id[which(pairs$connected==T)])
postsynaptic.cells<-unique(pairs$post.id[which(pairs$connected==T)])

## identify presynaptic and postsynaptic in cells
cells$presynaptic<-FALSE
cells$postsynaptic<-FALSE
cells$presynaptic[which(cells$cell.id%in%presynaptic.cells)]<-TRUE
cells$postsynaptic[which(cells$cell.id%in%postsynaptic.cells)]<-TRUE

### inspect the stcc of connected cells 
plot.xcorrelation(significant.pairs=pairs$pair.id[which(pairs$connected==T)],
                  significant.xc=stcc[which(stcc$pair.id%in%pairs$pair.id[which(pairs$connected==T)]),])
maps<-tmaps[which(tmaps$clu.id%in%cells$cell.id[which(cells$presynaptic==T|cells$postsynaptic==T)]),]

## get the stats 
stats.pre.post()
## get the figure
speed.pre.post.figure()

rm(cc.peak,maps,pairs,srTuningCurve,stac,stcc,stccp,tmaps,tstats)
rm(b,postsynaptic.cells,presynaptic.cells)
rm(boxplot.two.conditions,detect.crosscorrelation.peak,normalize.rate,plot.mean.sem.per.group.time,plot.spike.time.correlation,plot.xcorrelation,
   rename.condition.data.frame,speed.pre.post.figure,stats.pre.post)
