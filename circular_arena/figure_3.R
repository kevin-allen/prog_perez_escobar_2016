
##############################################
### figure on distance code                ###
##############################################
plot.grid.distance.coding.figure<-function(){
  m<-matrix(rbind(
    c(0.00,0.12,0.75,1.00), # c1 s1
    c(0.12,0.23,0.75,1.00),
    c(0.23,0.35,0.75,1.00),
    c(0.35,0.47,0.75,1.00),
    c(0.47,0.58,0.75,1.00),
    c(0.58,0.70,0.75,1.00),
    
    c(0.00,0.12,0.50,0.75), #c2 s7
    c(0.12,0.23,0.50,0.75),
    c(0.23,0.35,0.50,0.75),
    c(0.35,0.47,0.50,0.75),
    c(0.47,0.58,0.50,0.75),
    c(0.58,0.70,0.50,0.75),
    
    c(0.00,0.12,0.25,0.50), #c3 s13
    c(0.12,0.23,0.25,0.50),
    c(0.23,0.35,0.25,0.50),
    c(0.35,0.47,0.25,0.50),
    c(0.47,0.58,0.25,0.50),
    c(0.58,0.70,0.25,0.50),
    
    c(0.00,0.12,0.00,0.25), #c4 s19
    c(0.12,0.23,0.00,0.25),
    c(0.23,0.35,0.00,0.25),
    c(0.35,0.47,0.00,0.25),
    c(0.47,0.58,0.00,0.25),
    c(0.58,0.70,0.00,0.25),
    
    c(0.75,0.90,0.75,1.00), # 25
    c(0.75,0.90,0.50,0.75),
    c(0.75,0.85,0.40,0.60),
    c(0.85,0.95,0.40,0.60),
    c(0.75,0.90,0.20,0.40)),ncol=4)
  
  fn<-paste(ep@directory,"figures_relectro","figure_3.pdf",sep="/")  
  print(paste("creating",fn))
  pdf(file=fn,width=6,height=4.5)
  #x11(width=6,height=5.5)
  split.screen(m)
  cellids<-c("jp19841-30062015-0108_10","jp19843-30072015-0108_11","jp19841-15072015-0108_6","jp19844-13082015-0108_4")
  
  index=0
  for (cellid in cellids){
    map.t<-map.trials[which(map.trials$clu.id==cellid),]
    st<-stmr[which(stmr$clu.id==cellid),] # maps that have been trimmed at edges (1.5 distance)
    tc<-sort(unique(map.t$condition))
    vn<-dr[which(dr$clu.id==cellid),]
    ds<-dc[which(dc$clu.id==cellid),]
    
    screen(index+1)
    x<-map.t[which(map.t$condition==tc[3]),]
    if(index==0){ ## add scale bar
      x$rate[which(x$y>=0&x$y<=1&x$x>0&x$x<10)] <- 0
    }
    firingRateMapPlot(x,name="l1",outma=c(1,1,1,1),margin=c(0.5,0.5,0.5,0.5))
    
    
    screen(index+2)
    x<-st[which(st$condition=="l1"),]
    if(index==0){ ## add scale bar
      x$rate[which(x$y>=0&x$y<=1&x$x>0&x$x<10)] <- 0
    }
    firingRateMapPlot(x,name="l1",outma=c(1,1,1,1),margin=c(0.5,0.5,0.5,0.5))
    
    
    screen(index+3)
    ## distance tuning
    
    x<-vn[which(vn$condition=="l1"),]
    max.y<-5*(floor(max(vn$rate[which(vn$distance<60)],na.rm=T)/5)+1)
    rate.distance.one.condition.plot(x,plotxlim=c(0,60),plotylim=c(0,max.y),x.axis.at=seq(0,60,30))
    
    
    screen(index+4)
    x<-map.t[which(map.t$condition==tc[1]),]
    firingRateMapPlot(x,name="d1",outma=c(1,1,1,1),margin=c(0.5,0.5,0.5,0.5))
    
    screen(index+5)
    x<-st[which(st$condition=="d1"),]
    firingRateMapPlot(x,name="d1",outma=c(1,1,1,1),margin=c(0.5,0.5,0.5,0.5))
    
    screen(index+6)
    ## ditance tuning
    x<-vn[which(vn$condition=="d1"),]
    rate.distance.one.condition.plot(x,plotxlim=c(0,60),plotylim=c(0,max.y),x.axis.at=seq(0,60,30))
    
    index=index+6    
  }
  
  ## print the mean normalized distance rate
  screen(index+1)
  x<-drn[which((drn$condition=="l1") | (drn$condition=="d1")),]
  
  plot.mean.sem.per.group.time(data=x,dv="rate",time="distance",group="condition",ylab="Firing rate (Hz)", xlab="Distance (norm)",
                               outma=c(0,0.5,0.5,0), mgp.x=c(.2,0.0,0),mgp.y=c(0.6,0.4,0),margin=c(1.2,1.2,1,0.3),ylim=c(0,8),
                               legend.lab=c("Dark","Light"))
  
  ## print the distance score
  screen(index+2)
  x<-dc
  x$real.shuf<-"real"
  y<-dcShuf
  y$real.shuf<-"shuf"
  x<-rbind(x,y)
  boxplot.distance.code.condition(x,ylim=c(0,0.8),mpg=c(1,0.2,0),margin=c(1.2,1.6,1,0.3),outma=c(0,0.5,0.5,0),
                                  ylab="Distance score")
  
  #   ## print the correlation of peak distance
  #   pdnns<-pdnn[pdnn$id%in%dc$clu.id[which(dc$condition=="d1"&dc$distance.score>0.15)],]
  #   
  #   b<-data.frame(id=pdnns$id[which(pdnns$condition=="l1")],
  #                 pl1=pdnns$peak.distance[which(pdnns$condition=="l1")],
  #                 pd1=pdnns$peak.distance[which(pdnns$condition=="d1")],
  #                 pl2=pdnns$peak.distance[which(pdnns$condition=="l2")],
  #                 pd2=pdnns$peak.distance[which(pdnns$condition=="d2")])
  #   screen(index+3)
  #   plot.points(data=b,v1="pl1",v2="pl2",xbin.add=0,axis.y.pos=25,axis.x.pos=25,axis.y.las=2,
  #               main.title="",mgp.x=c(0.3,0.1,0.0),mgp.y=c(0.6,0.2,0.1),xlab="Peak l1 (cm)",ylab="Peak l2 (cm)",
  #               plotxlim=c(25,60),plotylim=c(25,60),outma=c(0,0.5,0.5,0),margin=c(1.2,1.2,1,0.3), col="blue",
  #               xaxis.at=seq(25,60,5),yaxis.at=seq(25,60,5),cex.point=0.1,add.text=paste("r: ",round(cor(b$pl1,b$pl2),2)))
  #   screen(index+4)
  #   plot.points(data=b,v1="pl1",v2="pd1",xbin.add=0,axis.y.pos=25,axis.x.pos=25,axis.y.las=2,
  #               main.title="",mgp.x=c(0.3,0.1,0.0),mgp.y=c(0.6,0.2,0.1),xlab="Peak l1 (cm)",ylab="Peak d1 (cm)",
  #               plotxlim=c(25,60),plotylim=c(25,60),outma=c(0,0.5,0.5,0),margin=c(1.2,1.2,1,0.3), col="blue",
  #               xaxis.at=seq(25,60,5),yaxis.at=seq(25,60,5),cex.point=0.1,add.text=paste("r: ",round(cor(b$pl1,b$pd1),2)))
  #   
  #   
  #   ## print the peak distance per cell
  #   screen(index+5)
  #   head(pd)
  #   head(dc)
  #   plot.two.distributions(pd$peak.distance[which(pd$condition=="l1"&pd$clu.id%in%dc$clu.id[which(dc$condition=="d1"&dc$distance.score>0.15)])],
  #                          pd$peak.distance[which(pd$condition=="l1"&pd$clu.id%in%dc$clu.id[which(dc$condition=="l1"&dc$distance.score>0.15)])],
  #                          min=0.6,max=1.5,int=0.1,axis.y.pos=0.4,axis.x.pos=0,plotxlim=c(0.4,1.6),plotylim = c(0,0.5),
  #                          outma=c(0,0.5,0.5,0),margin=c(1.2,1.2,1,0.3), mgp.x=c(0.2,0.0,0),mgp.y=c(0.6,0.4,0),
  #                          xaxis.at=seq(0.4,1.6,0.2),xlab="Distance at peak",ylab="Probability")
  #   
  close.screen( all = TRUE )
  dev.off() 
}


distance.rate.df<-function(x){
  ## return the tuning curve of the neuron
  ## get size of the map
  vlen=40 ## from 0 to 40 bins
  na.ratio=1
  xr<-range(x$x)
  yr<-range(x$y)
  midx<-(xr[2]-xr[1])/2
  midy<-(yr[2]-yr[1])/2
  xlen<-length(unique(x$x))
  ylen<-length(unique(x$y))
  dm<-sqrt((x$x-midx)^2+(x$y-midy)^2)
  rd<-data.frame(clu.id=rep(unique(x$clu.id),vlen),condition=rep(unique(x$condition),vlen),
                 distance=1:vlen,rate=NA)
  for (dis in seq(1,vlen)){
    ## check how many valid bins we have compare to how many valid bin there are
    rd$rate[dis]<-NA
    l<-length(x$rate[which(dm>dis-1&dm<=dis)])
    if(l>0)
    {
      nal<-sum(is.na(x$rate[which(dm>dis-1&dm<dis)]))
      if(nal/l<na.ratio){
        rd$rate[dis]<-mean(x$rate[which(dm>dis-1&dm<dis)],na.rm=T)
      }
    }
  }
  rd$distance<-rd$distance*2 # because a bin = 2 x 2 cm
  return(rd)
}
best.baseline.spacing<-function(x){
  ## return the spacing of the baseline with the highest grid score
  return(x$grid.spacing[which.max(x$grid.score)])
}

distance.score<-function(x){
  ## interpolate the firing rate at 1 and 0.5 from the data
  index.below<-sum(x$distance<1)
  d1<-x$distance[index.below]
  d2<-x$distance[index.below+1]
  pr<-(1-d1)/(d2-d1) # proportion of distance between d1 and d2 to get to 1
  p=x$rate[index.below]+ ((x$rate[index.below+1]-x$rate[index.below])*pr) ## rate at 1
  
  index.below<-sum(x$distance<0.5)
  d1<-x$distance[index.below]
  d2<-x$distance[index.below+1]
  pr<-(0.5-d1)/(d2-d1) # proportion of distance between d1 and d2 to get to 1
  ph=x$rate[index.below]+ ((x$rate[index.below+1]-x$rate[index.below])*pr) ## rate at 1
  
  return((p-ph)/(p+ph))
}




# Figure on the encoding of distance by grid cells
st.map.keep.up.to.normalized.distance<-function(x,spacing,normalized.distance.limit){
  ## get size of the map
  xr<-range(x$x)
  yr<-range(x$y)
  midx<-(xr[2]-xr[1])/2
  midy<-(yr[2]-yr[1])/2
  s<-spacing$spacing[which(as.character(spacing$id)==as.character(unique(x$id)))]
  max.dist<-s*normalized.distance.limit
  x$rate[which( (2*sqrt((x$x-midx)^2+(x$y-midy)^2))>max.dist)]<-NA # 2* because 2*2 cm bins
  return(x)
}

sem<-function(x){sd(x,na.rm=T)/sqrt(length(x))}
cor.diff <- function(r1,n1,r2,n2 ){
  Z1 <- 0.5 * log( (1+r1)/(1-r1) )
  Z2 <- 0.5 * log( (1+r2)/(1-r2) )
  diff   <- Z1 - Z2
  SEdiff <- sqrt( 1/(n1 - 3) + 1/(n2 - 3) )
  diff.Z  <- diff/SEdiff
  p <- 2*pnorm( abs(diff.Z), lower=F)
  cat( "Difference between ",r1," and ",r2,", two-tailed p value:", p , "\n" )
} 

boxplot.distance.code.condition <- function(data,outma=c(1,1,1,1),margin=c(2,2,1,1),mgp=c(1,0.2,0),...)
{
  par(oma=outma,mar=margin,mgp=mgp)
  data <- subset(data, (data$condition=='l1' | data$condition=='d1'))
  data$condition <- factor(data$condition,c("l1","d1")) # order factor
  data$real.shuf <- factor(data$real.shuf,c("real","shuf"))
  boxplot(data$distance.score~data$condition*data$real.shuf,las=1,
          frame.plot=FALSE,axes=FALSE,outline=FALSE,
          col=c("red","gray","red","gray"),
          at=c(1,2,4,5),ylim=c(-0.4,0.6),...)
  
  # axis(side = 1, at=0:2,pos=0, tck=-0.05,cex.axis=0.6,labels=c("","",""))
  par(mgp=mgp)
  axis(side = 2, at=seq(-0.4,0.6,by=0.2),las=1, pos=0,tck=-0.05,cex.axis=0.6,xpd=TRUE)
}

plot.grid.map.distance<- function(){
  fn=paste(ep@directory,"figures_relectro","grid_cells_distance_tuning_curves.pdf",sep="/")
  print(paste("creating",fn))
  pdf(file=fn,onefile=TRUE,paper="a4",width=8,height=10)
  #x11(width=7, height=10)
  cells.per.page=6
  num.columns=6
  xs<-seq(0,1-(1/num.columns),1/num.columns)
  xe<-seq((1/num.columns),1,1/num.columns)
  ys<-seq(1-(1/cells.per.page),0,-1/cells.per.page)
  ye<-seq(1,(1/cells.per.page),-1/cells.per.page)
  m<-matrix(c(rep(xs,cells.per.page),rep(xe,cells.per.page),rep(ys,each=num.columns),rep(ye,each=num.columns)),ncol=4)
  index=1
  
  for (cellid in grid.cells) # instead of head.direction.cells
  {
    if(index==1)
    {
      split.screen(m)  
    }
    map.t<-map.trials[which(map.trials$clu.id==cellid),]
    st<-stmr[which(stmr$clu.id==cellid),]
    
    v<-dr[dr$clu.id==cellid,]
    vn<-drn[drn$clu.id==cellid,]
    tc<-sort(unique(map.t$condition))
    
    cell.index<-(index-1)*num.columns
    
    screen(cell.index+1)
    x<-map.t[which(map.t$condition==tc[1]),]
    firingRateMapPlot(x,name=paste(cellid,tc[1]))  
    
    screen(cell.index+2)
    x<-map.t[which(map.t$condition==tc[3]),]
    firingRateMapPlot(x,name=paste(floor(spacing$spacing[which(spacing$clu.id==cellid)]),"cm ,",tc[3]))
    
    screen(cell.index+3)
    x<-st[which(st$condition=="d1"),]
    firingRateMapPlot(x,name="d1")
    
    screen(cell.index+4)
    x<-st[which(st$condition=="l1"),]
    firingRateMapPlot(x,name="l1")
    
    screen(cell.index+5)
    rate.distance.condition.plot(v)
    
    screen(cell.index+6)
    rate.distance.condition.plot(vn,plotxlim=c(0,1.5),x.axis.at=seq(0,1.5,0.5))
    #main.title=paste(ds$condition[which(ds$condition==sort(unique(vn$condition))[1])] ,
    #                ":",
    #               round(ds$distance.score[which(ds$condition==sort(unique(vn$condition))[1])],digits=2), "   ",
    #              ds$condition[which(ds$condition==sort(unique(vn$condition))[3])] , 
    #             ":",
    #            round(ds$distance.score[which(ds$condition==sort(unique(vn$condition))[3])],digits=2),sep=""))
    if(index==cells.per.page)
    {
      close.screen( all = TRUE )
      index=0
    }
    index=index+1
  }
  close.screen(all = TRUE)
  
  dev.off()
}
firing.rate.map.plot <- function(df,outma=c(.3,.3,.3,.3),margin=c(1,.5,1,.5),axis.x.mgp=c(0,0,0),axis.y.mgp=c(0,0,0),
                                 cex.x.axis=0.6,cex.y.axis=0.6,cex.lab=0.6,xlab="",ylab="",show.xlab=TRUE,main.title="")
{
  par(oma=outma,mar=margin,cex.lab=0.6)
  jet.colors = colorRampPalette(c("#00007F", "blue","#007FFF",  "cyan", "#7FFF7F", "yellow", "#FF7F00","red"))
  xlen <- length(unique(df$x))
  ylen <- length(unique(df$y))
  zzz <- matrix(df$rate,nrow=ylen,ncol=xlen)
  image(unique(df$y),unique(df$x),zzz,zlim=c(0,max(df$rate,na.rm=T)), col=jet.colors(200),xlab='',ylab='',axes=FALSE)
  mtext(paste(round(max(df$rate,na.rm=T),digits=2),"Hz"),side=3,at=median(unique(df$x)),line=0.0,cex=cex.x.axis)
  if(main.title!="")
  {
    mtext(main.title,side=3,at=median(unique(df$x)),line=0.5,cex=cex.x.axis)
  }
}


plot.mean.sem.per.group.time <- function(data,dv="dv",time="iv",group="group",xbin.add=0,
                                         axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                         arrow.lwd=0.25,arrow.length=0.005,
                                         main.title="",mgp.x=c(1,0.3,0.2),mgp.y=c(1,0.3,0.2),xlab="",ylab="",
                                         plotxlim=c(0,1.5),plotylim=c(0,10),outma=c(1,1,0.5,0),margin=c(2,2,1,0.3),
                                         xaxis.at=seq(0,8,1),yaxis.at=seq(0,20,5),cex.point=0.1,
                                         legend.lab=c(""),...)
{
  group.list <- unique(data[,group])
  mean.dv <- tapply(data[,dv],list(data[,time],data[,group]),mean,na.rm=T)
  sem.dv <-  tapply(data[,dv],list(data[,time],data[,group]),sem)
  tp <- as.numeric(names(table(data[,time])))
  
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  
  par(mgp=mgp.x)
  axis(side = 1, pos=axis.x.pos, tck=-0.05,cex.axis=0.60)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  
  index=1
  for (g in group.list)
  {
    lines (tp,mean.dv[,g],type='l',pch=20,xlab='',ylab='',lwd=0.75,col=index)
    index=index+1
  }
  index=1
  for (g in group.list)
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
    mtext(main.title,side=3,at=median(unique(tp))/2,line=0.1,cex=0.6)
  }
  
  
  if(legend.lab[1]!="")
  {
    legend(x=(plotxlim[1]+plotxlim[2])/3,y=plotylim[2],legend.lab,
           cex=0.4,lty=1,bty="n",
           col = seq(1,length(group.list)))
  }
}

boxplot.distance.code.condition <- function(data,outma=c(1,1,1,1),margin=c(2,2,1,1),mgp=c(1,0.2,0),...)
{
  par(oma=outma,mar=margin,mgp=mgp)
  data <- subset(data, (data$condition=='l1' | data$condition=='d1'))
  data$condition <- factor(data$condition,c("l1","d1")) # order factor
  data$real.shuf <- factor(data$real.shuf,c("real","shuf"))
  boxplot(data$distance.score~data$condition*data$real.shuf,las=1,
          frame.plot=FALSE,axes=FALSE,outline=FALSE,
          col=c("red","gray","red","gray"),
          at=c(1,2,4,5),ylim=c(-0.2,0.8),...)
  
  # axis(side = 1, at=0:2,pos=0, tck=-0.05,cex.axis=0.6,labels=c("","",""))
  par(mgp=mgp)
  axis(side = 2, at=seq(-0.2,0.8,by=0.2),las=1, pos=0,tck=-0.05,cex.axis=0.6,xpd=TRUE)
}

rate.distance.condition.plot <- function(x, axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                         plotxlim=c(0,80),x.axis.at=seq(0,80,10),
                                         outma=c(1,1,1,1),margin=c(2,2,1,1),mgp=c(0.8,0.3,0),
                                         main.title="",...)
{
  cd<-sort(unique(x$condition))
  plotylim=c(0,max(x$rate,na.rm=T))
  tp <- unique(x$distance)
  par(oma=outma,mar=margin,mgp=mgp,cex.lab=0.6,cex.axis=0.4)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="",...)
  axis(side = 1, at = x.axis.at,pos=axis.x.pos,tck=-0.05,cex.axis=0.60)
  axis(side = 2, at = seq(0,50,5),las=2, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  lines (tp,x$rate[which(x$condition==cd[1])],type='l',pch=20,col='black',xlab='',ylab='',lwd=0.75)
  #  lines (tp,x$rate[which(x$condition==cd[2])],type='l',pch=20,col='black',xlab='',ylab='',lwd=0.75)
  lines (tp,x$rate[which(x$condition==cd[3])],type='l',pch=20,col='red',xlab='',ylab='',lwd=0.75,lty=2)
  #  lines (tp,x$rate[which(x$condition==cd[4])],type='l',pch=20,col='red',xlab='',ylab='',lwd=0.75,lty=2)
  title(ylab='Firing rate (Hz)')
  title(xlab='Distance (cm)')
  if(main.title!="")
  {
    mtext(main.title,side=3,at=median(unique(tp))/2,line=0.1,cex=0.6)
  }
}


peak.distance<-function(x){
  #return the distance with a peak firing rate
  #0.5 < distance < 1.5
  y<-x[which(x$distance>0.5&x$distance<1.5),]
  return(y$distance[which.max(y$rate)])
}
## get the peak distance for the two conditions, not normalized from 0 to 1.5 distance
peak.distance.not.normalized<-function(x,y){
  #return the distance with a peak firing rate
  # y has the spacing of baseline to limit the max search
  id<-as.character(unique(x$clu.id))
  sp<-y$spacing[which(y$clu.id==id)]
  y<-x[which(x$distance>sp/2&x$distance<sp*1.5),]
  return(y$distance[which.max(y$rate)])
}




plot.two.distributions <- function(v1,v2,min=-1,max=1,int=0.05,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,plotxlim=c(-1,1),plotylim=c(0,0.4),
                                   outma=c(1,1,1,1),margin=c(2,2,1,1),mgp=c(0.8,0.1,0),probability=1,xaxis.at=seq(-1,1,0.50),vline="",
                                   main.title="",mgp.x=c(1,1,1),mgp.y=c(1,1,1),xlab="",ylab="",...)
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
  axis(side = 1, at=xaxis.at, pos=axis.x.pos,tck=-0.05,cex.axis=0.60,mgp=mgp)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp)
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
  
  if(vline!="")
  {
    lines(c(vline,vline),plotylim)
  }
  if(main.title!=""){
    title(main=main.title,cex.main=0.6)
  }
}



plot.points <- function(data,v1="v1",v2="v2",xbin.add=0,axis.y.pos=-.2,axis.x.pos=-.2,axis.y.las=2,
                        main.title="",mgp.x=c(0.5,0.1,0.1),mgp.y=c(0.8,0.2,0.1),xlab="",ylab="",
                        plotxlim=c(-.2,0.6),plotylim=c(-0.2,0.6),outma=c(0.5,0.5,0.5,0.5),margin=c(1.5,1.5,1,0.3),
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
    text(labels=add.text,x=plotxlim[1]+(plotxlim[2]-plotxlim[1])/4 ,y=plotylim[2],cex=0.6)
  }
}




rate.distance.one.condition.plot <- function(x, axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                             plotxlim=c(0,80),plotylim=c(0,10),x.axis.at=seq(0,60,30),
                                             outma=c(0.5,1,0.5,1),margin=c(1.5,1.5,0.5,0.5),mgp=c(0.6,0.3,0),
                                             main.title="",...)
{
  tp <- unique(x$distance)
  par(oma=outma,mar=margin,mgp=mgp,cex.lab=0.6,cex.axis=0.4)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="",...)
  axis(side = 1, at = x.axis.at,pos=axis.x.pos,tck=-0.05,cex.axis=0.60)
  axis(side = 2, at = seq(0,50,5),las=2, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  lines (tp,x$rate,type='l',pch=20,col='black',xlab='',ylab='',lwd=0.75)
  title(ylab='Rate (Hz)')
  title(xlab='Distance (cm)')
  if(main.title!="")
  {
    mtext(main.title,side=3,at=median(unique(tp))/2,line=0.1,cex=0.6)
  }
}



print.stats.distance<-function(){
  print("***********************")
  print("*** Distance score ****")
  print("***********************")
  print("l1")
  print(paste("n:",length(dc$distance.score[which(dc$condition=="l1")])))
  print(summary(dc$distance.score[which(dc$condition=="l1")]))
  print("d1")
  print(paste("n:",length(dc$distance.score[which(dc$condition=="d1")])))
  print(summary(dc$distance.score[which(dc$condition=="d1")]))
  print("Differenc of distance score l1 and d1")
  print(wilcox.test(dc$distance.score[which(dc$condition=="l1")],
                    dc$distance.score[which(dc$condition=="d1")],paired=T))
  print("Differenc of distance score l1 and shuffled l1")
  print(wilcox.test(dc$distance.score[which(dc$condition=="l1")],
                    dcShuf$distance.score[which(dcShuf$condition=="l1")],paired=T))
  print("Difference of distance score d1 and shuffled d1")
  length(dc$distance.score[which(dc$condition=="d1")])
  length(dcShuf$distance.score[which(dc$condition=="d1")])
  print(wilcox.test(dc$distance.score[which(dc$condition=="d1")],
                    dcShuf$distance.score[which(dcShuf$condition=="d1")],paired=T))
}



####################
####################
#### start here ####
####################
####################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
#load data files
load(paste(ep@resultsDirectory,"trigMaps",sep="/"))
load(paste(ep@resultsDirectory,"trigMapsShuf",sep="/"))
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"bstats",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
load(paste(ep@resultsDirectory,"sessions",sep="/"))
stm<-trigMaps
stg<-trigMapsShuf
map.trials<-tmaps
baseline.info<-bstats
rm(trigMaps,trigMapsShuf,tmaps,bstats)

## keep only grid cells in these data.frames.
stm<-stm[which(stm$clu.id %in%  cells$cell.id[which(cells$grid==TRUE)]),]
stg<-stg[which(stg$clu.id %in%  cells$cell.id[which(cells$grid==TRUE)]),]
map.trials<-map.trials[which(map.trials$clu.id %in%  cells$cell.id[which(cells$grid==TRUE)]),]
baseline.info<-baseline.info[which(baseline.info$clu.id %in%  cells$cell.id[which(cells$grid==TRUE)]),]
tstats<-tstats[which(tstats$clu.id %in% cells$cell.id[which(cells$grid==TRUE)]),]
baseline.info$clu.id<-factor(baseline.info$clu.id)
tstats$clu.id<-factor(tstats$clu.id)
tstats$session<-factor(tstats$session)
stm$rate[which(stm$rate==-1.0)] <- NA
stg$rate[which(stg$rate==-1.0)] <- NA


## keep only sessions with 2 lights
stm$session<-sessionNameFromCluId(stm$clu.id)
stg$session<-sessionNameFromCluId(stg$clu.id)

stm<-stm[which(stm$session %in% sessions$session[which(sessions$num.lights==2)]),]
stg<-stg[which(stg$session %in% sessions$session[which(sessions$num.lights==2)]),]
stm$clu.id<-factor(stm$clu.id)
stg$clu.id<-factor(stg$clu.id)

print(paste("Number of grid cells with 2 lights:",length(unique(stm$clu.id))))
#############################################################################
## claculate the rate * distance from center of the spike triggered map #####
#############################################################################


dr<-lapply(split(stm,list(stm$clu.id,stm$condition)),distance.rate.df)
dr<-do.call("rbind",dr)
drShuf<-lapply(split(stg,list(stg$clu.id,stg$condition)),distance.rate.df)
drShuf<-do.call("rbind",drShuf)

######################################################################################
## get the spacing of the grid cells during the baseline with the highest grid score ##
#######################################################################################
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tstats<-tstats[which(tstats$condition=="l2"),]
spacing<-data.frame(clu.id=tstats$clu.id,spacing=tstats$grid.spacing,grid.score=tstats$grid.score)
length(spacing$clu.id)

####################################
####################################
max.spacing=50
min.spacing=20
min.grid.score=0.3
grid.cells<-as.character(unique(dr$clu.id)[unique(dr$clu.id)%in%spacing$clu.id[which(spacing$spacing<max.spacing&
                                                                                       spacing$spacing>min.spacing&
                                                                                       spacing$grid.score>min.grid.score)]])
print(paste("Number of grid cells with spacing < 50 cm",length(grid.cells)))

## remove grid cells with large spacing from distance estimate
dr<-dr[which(dr$clu.id%in%grid.cells),]
dr$clu.id<-as.character(dr$clu.id)
drShuf<-drShuf[which(drShuf$clu.id%in%grid.cells),]
drShuf$clu.id<-factor(drShuf$clu.id,levels=unique(dr$clu.id))


stg<-stg[which(stg$clu.id%in%grid.cells),]
stm<-stm[which(stm$clu.id%in%grid.cells),]
print(paste("Number of grid cells with spacing smaller than 50 cm:",length(unique(stg$clu.id))))



##########################################################
## normalize distance so that a unit is in grid spacing ##
##########################################################
normalize.distance.to.spacing<-function(x,s){
  ## normalize distance to spacing
  ## get same distance points for all neurons
  if(length(x$clu.id)==0)
    stop(paste("length of x is 0"))
  s<-spacing$spacing[which(spacing$clu.id==as.character(unique(x$clu.id)))]
  if(max(x$distance)/s<1.5)
    stop(paste("max distance is smaller than s*1.5"))
  x$distance<-x$distance/s
  distance.interval<-0.05
  distances<-seq(0.10,1.5,0.05)
  y<-data.frame(clu.id=unique(x$clu.id),condition=unique(x$condition),
                distance=distances,rate=NA)
  index=1
  for(d in distances){
    ## interpolate the firing rate at d
    index.below<-sum(x$distance<d)
    d1<-x$distance[index.below]
    d2<-x$distance[index.below+1]
    pr<-(d-d1)/(d2-d1) # proportion of distance between d1 and d2 to get to 1
    y$rate[index]<- x$rate[index.below]+ ((x$rate[index.below+1]-x$rate[index.below])*pr)
    index=index+1
  }
  return(y)
}

drn<-lapply(split(dr,list(dr$clu.id,dr$condition)),normalize.distance.to.spacing,spacing)
drn<-do.call("rbind",drn)
drnShuf<-lapply(split(drShuf,list(drShuf$clu.id,drShuf$condition)),normalize.distance.to.spacing,spacing)
drnShuf<-do.call("rbind",drnShuf)




###################################
## calculate the distance score ###
###################################
b<-by(drn,list(drn$clu.id,drn$condition),distance.score)
dc<-data.frame(clu.id=unlist(dimnames(b)[1]), ## will repeat
               condition=rep(unlist(dimnames(b)[2]),each=length(unlist(dimnames(b)[1]))),
               distance.score=as.numeric(b))
b<-by(drnShuf,list(drnShuf$clu.id,drnShuf$condition),distance.score)
dcShuf<-data.frame(clu.id=unlist(dimnames(b)[1]), ## will repeat
               condition=rep(unlist(dimnames(b)[2]),each=length(unlist(dimnames(b)[1]))),
               distance.score=as.numeric(b))

####################################
## peak distance (0.5 < d < 1.5) ###
####################################
b<-by(drn,list(drn$clu.id,drn$condition),peak.distance)
pd<-data.frame(clu.id=unlist(dimnames(b)[1]),
               condition=rep(unlist(dimnames(b)[2]),each=length(unlist(dimnames(b)[1]))),
               peak.distance=as.numeric(b))
b<-by(dr,list(dr$clu.id,dr$condition),peak.distance.not.normalized,spacing)
pdnn<-data.frame(id=unlist(dimnames(b)[1]),
                 condition=rep(unlist(dimnames(b)[2]),each=length(unlist(dimnames(b)[1]))),
                 peak.distance=as.numeric(b))

#########################################
### keep only 0 - 1.5 in the stm1 maps ##
#########################################
ndl<-1.4
stmr<-stm
stmr$clu.id<-factor(stmr$clu.id)
stmr$condition<-factor(stmr$condition)
spacings<-spacing[which(spacing$id%in%unique(stmr$clu.id)),]
stmr<-unsplit(lapply(split(stmr,list(stmr$clu.id,stmr$condition)),st.map.keep.up.to.normalized.distance,spacings,ndl),list(stmr$clu.id,stmr$condition))
rm(ndl,spacings)



############################################## 
#### print all selected grid cells       #####
#### grid.cells.distance.coding.vector.pdf ###
##############################################
plot.grid.map.distance()
plot.grid.distance.coding.figure()


##############
### stats ####
##############
print.stats.distance()


#######################
### delete objects ####
#######################
rm(dc,dr,drn,pd,spacing,stg,stm,baseline.info,sessions)
rm(b,grid.cells,max.spacing)
rm(dcShuf,drnShuf,drShuf,map.trials,pdnn,stmr,tstats,min.grid.score,min.spacing,st.map.keep.up.to.normalized.distance)
rm(boxplot.distance.code.condition,distance.rate.df)
rm(best.baseline.spacing,
   cor.diff,
   distance.score,
   firing.rate.map.plot,
   normalize.distance.to.spacing,
   peak.distance,
   peak.distance.not.normalized,
   plot.grid.distance.coding.figure,
   plot.grid.map.distance,
   rename.condition.data.frame,
   plot.mean.sem.per.group.time,
   plot.points,
   plot.two.distributions,
   print.stats.distance,
   rate.distance.condition.plot,
   rate.distance.one.condition.plot,
   sem)