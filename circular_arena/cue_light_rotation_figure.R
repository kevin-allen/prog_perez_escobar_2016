sem<-function(x){sd(x,na.rm=T)/sqrt(length(x))}

firing.rate.map.plot <- function(df,outma=c(.5,.1,.5,.1),margin=c(1,.1,1.5,.1),axis.x.mgp=c(0,0,0),axis.y.mgp=c(0,0,0),cex.x.axis=0.6,cex.y.axis=0.6,cex.lab=0.6,xlab="",ylab="",show.xlab=TRUE,main.title="")
{
  par(oma=outma,mar=margin,cex.lab=0.6)
  jet.colors = colorRampPalette(c("#00007F", "blue","#007FFF",  "cyan", "#7FFF7F", "yellow", "#FF7F00","red"))
  xlen <- length(unique(df$x))
  ylen <- length(unique(df$y))
  zzz <- matrix(df$rate,nrow=ylen,ncol=xlen)
  image(unique(df$y),unique(df$x),zzz,zlim=c(0,max(df$rate)), col=jet.colors(200),xlab='',ylab='',axes=FALSE)
  mtext(paste(round(max(df$rate),digits=2),"Hz"),side=3,at=median(unique(df$x)),line=0.3,cex=cex.x.axis)
}
head.direction.tuning.curve.conditions.polar.plot <- function(df,outma=c(0,0,1,0),margin=c(1,1,1,1),axis.x.mgp=c(1,0.3,0),axis.y.mgp=c(2.2,0.6,0),cex.x.axis=0.6,cex.y.axis=0.6,cex.lab=0.6,xlab="",ylab="",show.xlab=TRUE,main.title="")
{
  radlim=max(df$rate)   
  par(oma=outma,cex.lab=cex.lab,cex.axis=cex.x.axis)
  cond<-head(sort(unique(df$condition),decreasing=TRUE),n=2)
  df1<-subset(df,condition==cond[1])  
  oldpar <- polar.plot(df1$rate,
                       polar.pos=df1$deg,
                       labels=seq(0,270,90),label.pos=c(0,90,180,270),start=90,
                       clockwise=T,rp.type="p",
                       show.grid=T,show.radial.grid=T,
                       radial.lim=c(0,radlim),show.grid.labels=0,
                       xlab="",ylab="",line.col=4,mar=margin)
  
  df1<-subset(df,condition==cond[2])  
  polar.plot(df1$rate,
             polar.pos=df1$deg,
             labels=seq(0,270,90),label.pos=c(0,90,180,270),start=90,
             clockwise=T,rp.type="p",
             show.grid=T,show.radial.grid=T,
             radial.lim=c(0,radlim),show.grid.labels=0,
             xlab="",ylab="",line.col=2,mar=margin,add=T)
  
  mtext(paste(round(radlim,digits=2),"Hz"),side=3,at=0,line=0.3,cex=cex.x.axis)
  par(oldpar)
}

plot.mean.sem.per.angle <- function(data,dv="dv",time="iv",axis.y.pos=0,axis.x.pos=-.2,axis.y.las=2,plotxlim=c(0,360),plotylim=c(-0.2,0.6),arrow.lwd=0.25,arrow.length=0.005,outma=c(0,0,0,0),margin=c(2,1.8,0,0.3),mgp=c(0.8,0.3,0),vline=90,...)
{
  mean.dv <- tapply(data[,dv],list(data[,time]),mean)
  sem.dv <-  tapply(data[,dv],list(data[,time]),sem)
  tp <- as.numeric(names(table(data[,time])))
  par(mgp=mgp,mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,...)
  axis(side = 1, pos=axis.x.pos, tck=-0.05,cex.axis=0.60, at=seq(0,360,90))
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  lines (tp,mean.dv,type='l',pch=20,xlab='',ylab='',lwd=0.75)
  for (i in seq(tp))
  {
    arrows(tp[i],mean.dv[i]-sem.dv[i],tp[i],mean.dv[i]+sem.dv[i],code=3,angle=90,length=arrow.length,lwd=arrow.lwd)
  }
  lines(c(vline,vline),c(plotylim[1],plotylim[2]),col="red",pch=22)
}

sem<-function(x){sd(x,na.rm=T)/sqrt(length(x))}
plot.mean.sem.per.group.time <- function(data,dv="dv",time="iv",group="group",xbin.add=0,
                                         axis.y.pos=-500,axis.x.pos=0,axis.y.las=2,
                                         arrow.lwd=0.25,arrow.length=0.005,
                                         main.title="",mgp.x=c(0.65,0.3,0.2),mgp.y=c(1,0.3,0.2),
                                         xlab="",ylab="",
                                         plotxlim=c(-500,1500),plotylim=c(0,25),
                                         outma=c(1,0.5,0.5,0),margin=c(1.5,1.5,0.3,0.3),
                                         xaxis.at=seq(-500,1500,500),yaxis.at=seq(0,30,10),
                                         cex.point=0.1,legend.lab=c(""),col.index="",...)
{
  group.list <- sort(unique(data[,group]))
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
    if(col.index[1]==""){
      lines (tp,mean.dv[,g],type='l',pch=20,xlab='',ylab='',lwd=0.75,col=index)
    }
    else{
      lines (tp,mean.dv[,g],type='l',pch=20,xlab='',ylab='',lwd=0.75,col=col.index[index])
    }
    index=index+1
  }
  index=1
  a=0
  for (g in seq(1,length(group.list)))
  {
    if(col.index[1]==""){
      for (i in seq(tp))
      {
        arrows(tp[i],mean.dv[i,g]-sem.dv[i,g],tp[i],mean.dv[i,g]+sem.dv[i,g],code=3,angle=90,length=arrow.length,lwd=arrow.lwd,col=index)
      }
    }
    else{
      for (i in seq(tp))
      {
        arrows(tp[i],mean.dv[i,g]-sem.dv[i,g],tp[i],mean.dv[i,g]+sem.dv[i,g],code=3,angle=90,length=arrow.length,lwd=arrow.lwd,col=col.index[index])
      }
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
    if(col.index[1]==""){
      legend(x=plotxlim[1],y=plotylim[2],legend.lab,
             cex=0.6,lty=1,bty="n",
             col = seq(1,length(group.list)))
    }
    else{
      legend(x=plotxlim[1],y=plotylim[2],legend.lab,
             cex=0.6,lty=1,bty="n",
             col = col.index)
    }
  }
}



clu.light.rotation.complete.figure <- function(){
  pdf(file="pintegration_cue_control_figure.pdf",onefile=TRUE,width=3.5,height=3)
  #x11(width=7, height=4)
  m <- rbind(c(0.00,0.15,0.66,1.00), # gc 90  1
             c(0.15,0.30,0.66,1.00), # gc 90 2
             c(0.00,0.15,0.33,0.66), # gc 180 3
             c(0.15,0.30,0.33,0.66), # gc 180 4
             c(0.00,0.30,0.00,0.33), # pop gc 5
             
             c(0.30,0.45,0.66,1.00), # issc 90  6
             c(0.45,0.60,0.66,1.00), # issc 90 7
             c(0.30,0.45,0.33,0.66), # issc 180 8
             c(0.45,0.60,0.33,0.66), # issc 180 9
             c(0.30,0.60,0.00,0.33), # pop issc 10
             
             c(0.60,0.75,0.66,1.00), # border 90 11
             c(0.75,0.90,0.66,1.00), # border 90 12
             c(0.60,0.75,0.33,0.66), # border 90 13
             c(0.75,0.90,0.33,0.66), # border 90 14
             c(0.60,0.90,0.00,0.33) # pop border 15
             
  )
  split.screen(m)

  
  ## plot the grid cells
  index<-1
  for(cellid in cell.list.gc){
    jj<-subset(gc.sel,id==cellid)
    cond<-head(sort(unique(jj$condition),decreasing=TRUE),n=2)
    for(c in cond){
      screen(index)
      ii<-subset(jj,condition==c)  
      firing.rate.map.plot(ii)
      index=index+1
    }
  }
  
  ## population grid cells
  screen(5)
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==90)]&cells$grid==T&cells$region=="mec"),]
  map.rot.sel<-map.rot[which(map.rot$id%in%c$clu_id),]
  map.rot.sel$rot=90
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation=="180")]&cells$grid==T&cells$region=="mec"),]
  map.rot.sel1<-map.rot[which(map.rot$id%in%c$clu_id),]
  map.rot.sel1$rot=180
  map.rot.sel<-rbind(map.rot.sel,map.rot.sel1)
  plot.mean.sem.per.group.time(data=map.rot.sel,dv="r",time="deg",group="rot",xbin.add=0,
                               axis.y.pos=0,axis.x.pos=-0.2,axis.y.las=2,
                               arrow.lwd=0.25,arrow.length=0.005,
                               main.title="",mgp.x=c(0.4,0.15,0.1),mgp.y=c(1,0.3,0.2),
                               xlab="Rotation (deg)",ylab="Map similarity (r)",
                               plotxlim=c(0,360),plotylim=c(-0.2,1.0),
                               outma=c(0.5,0.3,0.3,0),margin=c(1.5,1.7,0.5,0.3),
                               xaxis.at=seq(0,360,90),yaxis.at=seq(-0.2,1.0,0.2),
                               cex.point=0.1,legend.lab=c("90","180"),col.index=c(3,8))
  
  ## plot the irregular spatially selective
  index<-6
  for(cellid in cell.list.issc){
    jj<-subset(issc.sel,id==cellid)
    cond<-head(sort(unique(jj$condition),decreasing=TRUE),n=2)
    for(c in cond){
      screen(index)
      ii<-subset(jj,condition==c)  
      firing.rate.map.plot(ii)
      index=index+1
    }
  }
  
  ## population irregular spatially selective cells
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==90)]&cells$place==T&cells$grid==F&cells$region=="mec"),]
  map.rot.sel<-map.rot[which(map.rot$id%in%c$clu_id),]
  map.rot.sel$rot=90
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==180)]&cells$place==T&cells$grid==F&cells$region=="mec"),]
  map.rot.sel1<-map.rot[which(map.rot$id%in%c$clu_id),]
  map.rot.sel1$rot=180
  map.rot.sel<-rbind(map.rot.sel,map.rot.sel1)
  screen(10)
  plot.mean.sem.per.group.time(data=map.rot.sel,dv="r",time="deg",group="rot",xbin.add=0,
                               axis.y.pos=0,axis.x.pos=-0.2,axis.y.las=2,
                               arrow.lwd=0.25,arrow.length=0.005,
                               main.title="",mgp.x=c(0.4,0.15,0.1),mgp.y=c(1,0.3,0.2),
                               xlab="Rotation (deg)",ylab="Map similarity (r)",
                               plotxlim=c(0,360),plotylim=c(-0.2,1.0),
                               outma=c(0.5,0.3,0.3,0),margin=c(1.5,1.7,0.5,0.3),
                               xaxis.at=seq(0,360,90),yaxis.at=seq(-0.2,1.0,0.2),
                               cex.point=0.1,legend.lab=c("90","180"),col.index=c(3,8))
  
  
  ### plot the border cells
  index<-11
  for(cellid in cell.list.border){
    jj<-subset(border.sel,id==cellid)
    cond<-head(sort(unique(jj$condition),decreasing=TRUE),n=2)
    for(c in cond){
      screen(index)
      ii<-subset(jj,condition==c)  
      firing.rate.map.plot(ii)
      index=index+1
    }
  }
  
  
  
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==90)]&cells$border==T&cells$region=="mec"),]
  map.rot.sel<-map.rot[which(map.rot$id%in%c$clu_id),]
  map.rot.sel$rot=90
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==180)]&cells$border==T&cells$region=="mec"),]
  map.rot.sel1<-map.rot[which(map.rot$id%in%c$clu_id),]
  map.rot.sel1$rot=180
  map.rot.sel<-rbind(map.rot.sel,map.rot.sel1)
  screen(15)
  plot.mean.sem.per.group.time(data=map.rot.sel,dv="r",time="deg",group="rot",xbin.add=0,
                               axis.y.pos=0,axis.x.pos=-0.2,axis.y.las=2,
                               arrow.lwd=0.25,arrow.length=0.005,
                               main.title="",mgp.x=c(0.4,0.15,0.1),mgp.y=c(1,0.3,0.2),
                               xlab="Rotation (deg)",ylab="Map similarity (r)",
                               plotxlim=c(0,360),plotylim=c(-0.2,1.0),
                               outma=c(0.5,0.3,0.3,0),margin=c(1.5,1.7,0.5,0.3),
                               xaxis.at=seq(0,360,90),yaxis.at=seq(-0.2,1.0,0.2),
                               cex.point=0.1,legend.lab=c("90","180"),col.index=c(3,8))
  
  
  
  
  
  
  close.screen( all = TRUE )
  dev.off()
}

####################################
######### START HERE ###############
####################################

## load the trial maps ##

## load the map rotation r ##





if(!exists("map.trials")){
  map.trials<-read.table("map_merged_trials.table",header=TRUE)
}
# map rotation r
map.rot<-read.table("hd_light_map_rotation_r_trials.table",header=T)



## plot the border cells 2 cells, 2 light conditions on the same graph
cell.list.border <- c("jp19841-27062015-0108_6",
                  "jp5520-24092015-0108_18")
border.sel<-map.trials[which(map.trials$id%in%cell.list.border),]

cell.list.issc<- c("jp2643-20032015-0108_9", #90 bc
                 "jp2643-25032015-0108_10") # 180 irr
issc.sel<-map.trials[which(map.trials$id%in%cell.list.issc),]

cell.list.gc<- c("jp693-11062015-0108_7", #90
                 "jp693-12062015-0108_2") #180
gc.sel<-map.trials[which(map.trials$id%in%cell.list.gc),]


# generate the figure
clu.light.rotation.complete.figure()














































########################################################
## do the stats for the stats for the population data ##
########################################################
#### hd cells ###
for (ang in c(90,180)){
  c<-cells[cells$session%in%sessions$session[which(sessions$rotation==ang)],]
  c<-c[which(c$hd==TRUE&c$region=="adn"),]
  x<-circular(rad(c$ccf.peak),type="angles",units="radians",template="none",modulo="asis",zero=0,rotation="counter")
  print(paste("Number of HD cells from adn with ",ang, " deg rotation: ",length(x)))
  print(paste("Rho: ",rho.circular(x)))
  print(paste("Mean: ", deg(mean.circular(x))))
  print(rayleigh.test(x))
}

#### grid cells ####
for(ang in c(90,180)){
  c<-cells[cells$session%in%sessions$session[which(sessions$rotation==ang)],]
  c<-c[which(c$grid==TRUE&c$region=="mec"),]
  map.rot.sel<-map.rot[which(map.rot$id%in%c$clu_id),]
  d<-unique(map.rot.sel$deg)
  angle.peak<-d[as.numeric(tapply(map.rot.sel$r,list(map.rot.sel$id),which.max))]
  x<-circular(rad(angle.peak),type="angles",units="radians",template="none",modulo="asis",zero=0,rotation="counter")
  print(paste("Number of grid cells from mec with ",ang," deg rotation:",length(x)))
  print(paste("Rho: ",rho.circular(x)))
  print(paste("Mean: ", deg(mean.circular(x))))
  print(rayleigh.test(x)) 
}
#### place cells ####
for(ang in c(90,180)){
  c<-cells[cells$session%in%sessions$session[which(sessions$rotation==ang)],]
  c<-c[which(c$place==TRUE&c$region=="ca1"),]
  map.rot.sel<-map.rot[which(map.rot$id%in%c$clu_id),]
  d<-unique(map.rot.sel$deg)
  angle.peak<-d[as.numeric(tapply(map.rot.sel$r,list(map.rot.sel$id),which.max))]
  x<-circular(rad(angle.peak),type="angles",units="radians",template="none",modulo="asis",zero=0,rotation="counter")
  print(paste("Number of place cells from ca1 with ",ang," deg rotation:",length(x)))
  print(paste("Rho: ",rho.circular(x)))
  print(paste("Mean: ", deg(mean.circular(x))))
  print(rayleigh.test(x))  
}


#### speed cells ####
for(ang in c(90,180)){
  c<-cells[cells$session%in%sessions$session[which(sessions$rotation==ang)],]
  c<-c[which(c$linear.speed==TRUE&c$region=="mec"),]
  map.rot.sel<-map.rot[which(map.rot$id%in%c$clu_id),]
  d<-unique(map.rot.sel$deg)
  angle.peak<-d[as.numeric(tapply(map.rot.sel$r,list(map.rot.sel$id),which.max))]
  x<-circular(rad(angle.peak),type="angles",units="radians",template="none",modulo="asis",zero=0,rotation="counter")
  print(paste("Number of speed cells from mec with ",ang," deg rotation:",length(x)))
  print(paste("Rho: ",rho.circular(x)))
  print(paste("Mean: ", deg(mean.circular(x))))
  print(rayleigh.test(x))  
}





## need to do some clean up when done
rm(c,x,d,angle.peak,grid.ori,ang,
   hd.sel, cell.list.hd,pc.sel,cell.list.pc,gc.sel,cell.list.gc,
   hd.histo.trials,
   map.rot,
   map.rot.sel,
   map.trials,
   clu.light.rotation.complete.figure,
   firing.rate.map.plot,
   head.direction.ccf.peaks.plot,
   head.direction.tuning.curve.polar.plot,
   head.direction.tuning.curve.conditions.polar.plot,
   plot.distribution,
   plot.mean.sem.per.angle, 
   plot.points,
   sem,
   plot.two.distributions,
   head.direction.crosscorrelation.linear.plot)
