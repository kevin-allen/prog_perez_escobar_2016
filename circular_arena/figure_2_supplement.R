plot.line<-function(x,vx,vy,
                    axis.y.pos=-25,axis.x.pos=0,axis.y.las=2,
                    mgp.x=c(0.5,0.1,0.1),mgp.y=c(0.8,0.2,0.1),xlab="",ylab="",
                    plotxlim=c(-25,25),plotylim=c(0,0.06),
                    outma=c(0.5,0.5,0.2,0.2),margin=c(1.5,1.5,0.5,0.3),
                    xaxis.at=seq(-30,30,10),yaxis.at=seq(0,0.06,0.01),...)
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=axis.x.pos, at=xaxis.at, tck=-0.05,cex.axis=0.60,mgp=mgp.x)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.y)
  lines (x[,vx],x[,vy],type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
}
plot.points <- function(data,v1="v1",v2="v2",xbin.add=0,axis.y.pos=-.2,axis.x.pos=-.2,axis.y.las=2,
                        main.title="",mgp.x=c(0.5,0.1,0.1),mgp.y=c(0.8,0.2,0.1),xlab="",ylab="",
                        plotxlim=c(-.2,0.6),plotylim=c(-0.2,0.6), outma=c(0.5,0.5,0.2,0.2),margin=c(1.5,1.5,0.5,0.3),
                        xaxis.at=seq(-0.2,0.6,.2),yaxis.at=seq(-0.2,0.6,.2),cex.point=0.1,
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
    text(labels=add.text,x=0.1,y=0.5,cex=0.6)
  }
}
plot.two.distributions <- function(v1,v2,min=-1,max=1,int=0.05,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,plotxlim=c(-1,1),plotylim=c(0,0.4),
                                   outma=c(0.5,0.5,0.2,0.2),margin=c(1.5,1.5,0.5,0.3),mgp=c(0.8,0.3,0),probability=1,
                                   xaxis.at=seq(-1,1,0.50),vline="",mgp.x=c(0.5,0.1,0.1),mgp.y=c(0.8,0.2,0.1),
                                   main.title="",xlab="",ylab="", legend.lab=c(""),...)
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
plot.sdm.all<-function(fn="page.full.plot"){
  num.cols<-3
  num.rows<-7
  plot.per.page=num.cols*num.rows
  mm<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
               rep(seq(1/num.cols,1,1/num.cols),num.rows),
               rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
               rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  fn=paste(ep@directory,"figures_relectro",fn,sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,paper="a4",width=8,height=10)
  #x11(width=4, height=8)
  index=1
  for (cellid in cell.list) ## any cell list
  {
    if(index==1)
    {
      split.screen(mm)  
    }
    
    screen(index)
    m<-tmaps[which(tmaps$clu.id==cellid&tmaps$condition=="l1"),]
    firingRateMapPlot(m=m,name=cellid)
    
    screen(index+1)
    m<-tmaps[which(tmaps$clu.id==cellid&tmaps$condition=="d1"),]
    firingRateMapPlot(m=m)
    
    screen(index+2)
    m<-head(sdm[which(sdm$clu.id==cellid),],n=10000)
    plot.points(m,v1="time",v2="sdm",
                axis.y.pos=-60,axis.x.pos=0,plotxlim=c(-60,120),plotylim=c(0,4),
                xaxis.at=seq(-60,120,30),yaxis.at=seq(0,4,1),
                xlab="Time (sec)",ylab="sdm")
    lines(c(0,0),c(0,4),col="red")
    index=index+2
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
grid.cells.figure <- function()
{
  m<-matrix(rbind(c(0.00,0.20,0.75,1.00),
                  c(0.20,0.40,0.75,1.00),
                  c(0.40,0.80,0.75,1.00),
                  
                  c(0.00,0.20,0.50,0.75),
                  c(0.20,0.40,0.50,0.75),
                  c(0.40,0.80,0.50,0.75),
                  
                  c(0.00,0.20,0.25,0.50),
                  c(0.20,0.40,0.25,0.50),
                  c(0.40,0.80,0.25,0.50),
                  
                  c(0.00,0.20,0.00,0.25),
                  c(0.20,0.40,0.00,0.25),
                  c(0.40,0.80,0.00,0.25),
                  
                  c(0.80,1.00,0.75,1.00),
                  c(0.80,1.00,0.50,0.75),
                  c(0.80,1.00,0.25,0.50))
            ,ncol=4)
  fn<-paste(ep@directory,"figures_relectro","sdm_figure.pdf",sep="/")
  print(paste("generating",fn))
  pdf(file=fn,onefile=TRUE,width=6,height=5)  
  #  x11(width=6, height=5)
  split.screen(m)
  index=1
  for(clu in grid.cells.on.figure){
    x<-tmaps[which(tmaps$clu.id==clu),]
    
    screen(index)
    y<-x[which(x$condition=="l1"),]
    firingRateMapPlot(y,outma=c(2.0,2.0,2.0,2.0),margin=c(1,1,1,1))
    screen(index+1)
    y<-x[which(x$condition=="d1"),]
    firingRateMapPlot(y,outma=c(2.0,2.0,2.0,2.0),margin=c(1,1,1,1))
    screen(index+2)
    x<-head(sdm[which(sdm$clu.id==clu),],n=10000)
    plot.points(x,v1="time",v2="sdm",
                axis.y.pos=-60,axis.x.pos=0,plotxlim=c(-60,120),plotylim=c(0,4),
                xaxis.at=seq(-60,120,30),yaxis.at=seq(0,4,1),
                xlab="Time (sec)",ylab="SDM")
    
    index=index+3
  }
  
  ## plot distribution of sdm for l1 and d1
  l<-sdm$sdm[which(sdm$time<0)]
  d<-sdm$sdm[which(sdm$time>0)]
  
  screen(13)  ### plot distribution of sdm for l1 and d1
  plot.two.distributions(v1=d,v2=l,min=0,max=6,int=0.1,axis.y.pos=0,axis.x.pos=0,
                         plotxlim=c(0,5),plotylim=c(0,.06),
                         xaxis.at=seq(0,5,1),xlab="SDM",ylab="Probability",legend.lab=c("d1","l1"))
  
  screen(14) ### plot mean sdm * time
  int=1
  min.t=-60
  max.t=119
  blocks<-length(seq(min.t,max.t,int))
  mean.sdm<-numeric(length = blocks)
  mean.sdm.shuf<-numeric(length = blocks)
  index=1
  for(s in seq(min.t,max.t,int)){
    e=s+int
    mean.sdm[index]<-mean(sdm$sdm[which(sdm$time>s&sdm$time<e)])
    mean.sdm.shuf[index]<-mean(sdm.shuf$sdm[which(sdm.shuf$time>s&sdm.shuf$time<e)])
    index=index+1
  }
  x<-data.frame(x=seq(min.t,max.t,int)+int/2,
                y=mean.sdm)
  plot.line(x,vx="x",vy="y",
            axis.y.pos=-60,axis.x.pos=1,axis.y.las=2,
            xlab="Time (sec)",ylab="SDM",
            plotxlim=c(-60,120),plotylim=c(1,2),
            xaxis.at=seq(-60,120,60),yaxis.at=seq(1,2,0.2))
  
  lines(seq(min.t,max.t,int)+int/2,
        mean.sdm.shuf,col="grey")
  
  
  screen(15) ### plot mean sdm * time
  x<-x[which(x$x>-30&x<60),]
  plot.line(x,vx="x",vy="y",
            axis.y.pos=-30,axis.x.pos=1,axis.y.las=2,
            xlab="Time (sec)",ylab="SDM",
            plotxlim=c(-30,60),plotylim=c(1,2),
            xaxis.at=seq(-30,60,15),yaxis.at=seq(1,2,0.2))
  xx<-sdm[which(sdm$time>0&sdm$time<10),]
  lm1<-lm(xx$sdm~xx$time)
  cor.test(xx$sdm,xx$time)
  ablineclip(lm1,x1=0,x2=10,col="red")
  text(x=5,y=1.1,labels=paste(round(lm1$coefficients[[2]],4),"SDM/sec"),cex=0.4)
  
  xx<-sdm[which(sdm$time>10&sdm$time<60),]
  lm1<-lm(xx$sdm~xx$time)
  cor.test(xx$sdm,xx$time)
  ablineclip(lm1,x1=10,x2=60,col="red")
  text(x=30,y=1.8,labels=paste(round(lm1$coefficients[[2]],4),"SDM/sec"),cex=0.4)
  
  close.screen(all=TRUE)
  dev.off()
}


################################################
################# START HERE ###################
################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@resultsDirectory,"sdm",sep="/"))
load(paste(ep@resultsDirectory,"sdm.shuf",sep="/"))
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)

gc<-unique(sdm$clu.id)
tmaps<-tmaps[which(tmaps$clu.id%in%gc),]

##########################################
### plot all grid cells with their sdm ###
##########################################

print("***************************************************")
print("** spike distance metric, figure 2 supplement  ****")
print("***************************************************")

cell.list<-gc
plot.sdm.all(fn="sdm_all_grid.pdf")

grid.cells.on.figure<-c("jp19841-01072015-0108_4",
                "jp19841-02072015-0108_13",
                "jp19843-30072015-0108_11",
                "jp693-11062015-0108_17")
grid.cells.figure()


## compare sdm l1 and d1
print("compare sdm during l1 and d1")
print("negative time = l1, positive time = d1")
print("l1")
print(length(sdm$sdm[which(sdm$time<0)]))
print(summary(sdm$sdm[which(sdm$time<0)]))
print("d1")
print(length((sdm$sdm[which(sdm$time>0)])))
print(summary(sdm$sdm[which(sdm$time>0)]))
print(wilcox.test(sdm$sdm[which(sdm$time<0)],
            sdm$sdm[which(sdm$time>0)]))
## compare sdm.shuf l1 and d1
#summary(sdm.shuf$sdm[which(sdm$time<0)])
#summary(sdm.shuf$sdm[which(sdm$time>0)])
#wilcox.test(sdm.shuf$sdm[which(sdm$time<0)],
#            sdm.shuf$sdm[which(sdm$time>0)])


## compare sdm.shuf and sdm at different time points
print("compare sdm with sdm.shuf at different time points")
int=10
min.t=-60
max.t=119
for(s in seq(min.t,max.t,int)){
  e=s+int
  print(paste(s,e))
  print(summary(sdm$sdm[which(sdm$time>s&sdm$time<e)]))
  print(summary(sdm.shuf$sdm[which(sdm.shuf$time>s&sdm.shuf$time<e)]))
  print(wilcox.test(sdm$sdm[which(sdm$time>s&sdm$time<e)],
           sdm.shuf$sdm[which(sdm.shuf$time>s&sdm.shuf$time<e)]))
}



rm(cell.list,e,gc,int,max.t,min.t,s)
rm(sdm,tmaps,sdm.shuf)
rm(grid.cells.on.figure,plot.line,plot.points,plot.sdm.all,plot.two.distributions,rename.condition.data.frame,grid.cells.figure)
