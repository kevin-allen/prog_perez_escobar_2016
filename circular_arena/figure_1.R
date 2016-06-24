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
  fn=paste(ep@directory,"figures_relectro","cue_rotation_figure_1.pdf",sep="/")
  pdf(file=fn,onefile=TRUE,width=3.5,height=3)
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
    jj<-subset(gc.sel,clu.id==cellid)
    cond<-head(sort(unique(jj$condition),decreasing=TRUE),n=2)
    for(c in cond){
      screen(index)
      ii<-subset(jj,condition==c)  
      firingRateMapPlot(ii,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
      index=index+1
    }
  }
  
#   ## population grid cells
  screen(5)
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==90)]&cells$grid==T),]
  map.rot.sel<- mapRotationLong[which(mapRotationLong$clu.id%in%c$cell.id),]
  map.rot.sel$rot=90
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==180)]&cells$grid==T),]
  map.rot.sel1<-mapRotationLong[which(mapRotationLong$clu.id%in%c$cell.id),]
  map.rot.sel1$rot=180
  map.rot.sel<-rbind(map.rot.sel,map.rot.sel1)
  plot.mean.sem.per.group.time(data=map.rot.sel,dv="r",time="degree",group="rot",xbin.add=0,
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
    jj<-subset(issc.sel,clu.id==cellid)
    cond<-head(sort(unique(jj$condition),decreasing=TRUE),n=2)
    for(c in cond){
      screen(index)
      ii<-subset(jj,condition==c)  
      firingRateMapPlot(ii,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
      index=index+1
    }
  }
#   
#   ## population irregular spatially selective cells
  screen(10)
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==90)]&cells$place==T),]
  map.rot.sel<- mapRotationLong[which(mapRotationLong$clu.id%in%c$cell.id),]
  map.rot.sel$rot=90
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==180)]&cells$place==T),]
  map.rot.sel1<-mapRotationLong[which(mapRotationLong$clu.id%in%c$cell.id),]
  map.rot.sel1$rot=180
  map.rot.sel<-rbind(map.rot.sel,map.rot.sel1)
  plot.mean.sem.per.group.time(data=map.rot.sel,dv="r",time="degree",group="rot",xbin.add=0,
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
    jj<-subset(border.sel,clu.id==cellid)
    cond<-head(sort(unique(jj$condition),decreasing=TRUE),n=2)
    for(c in cond){
      screen(index)
      ii<-subset(jj,condition==c)  
      firingRateMapPlot(ii,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
      index=index+1
    }
  }
  
  ## population border cells
  screen(15)
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==90)]&cells$border==T),]
  map.rot.sel<- mapRotationLong[which(mapRotationLong$clu.id%in%c$cell.id),]
  map.rot.sel$rot=90
  c<-cells[which(cells$session%in%sessions$session[which(sessions$rotation==180)]&cells$border==T),]
  map.rot.sel1<-mapRotationLong[which(mapRotationLong$clu.id%in%c$cell.id),]
  map.rot.sel1$rot=180
  map.rot.sel<-rbind(map.rot.sel,map.rot.sel1)
  plot.mean.sem.per.group.time(data=map.rot.sel,dv="r",time="degree",group="rot",xbin.add=0,
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
load(paste(ep@resultsDirectory,"sessions",sep="/"))
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"mapRotation",sep="/"))
mapRotationLong<-reshape(mapRotation,direction="long",varying=list(colnames(mapRotation)[1:36]),times=seq(0,350,10),timevar="degree",v.names=c("r"))

## examples of border cells
cell.list.border <- c("jp5519-24102015-0108_3",
                  "jp19841-07072015-0108_8")
border.sel<-tmaps[which(tmaps$clu.id%in%cell.list.border),]

## examples of irr cells
cell.list.issc<- c("jp5519-21102015-0108_7", #90 irr
                 "jp19841-10072015-0108_3") # 180 irr
issc.sel<-tmaps[which(tmaps$clu.id%in%cell.list.issc),]

cell.list.gc<- c("jp19841-01072015-0108_9", #90
                 "jp19841-15072015-0108_6") #180
gc.sel<-tmaps[which(tmaps$clu.id%in%cell.list.gc),]

# generate the figure
clu.light.rotation.complete.figure()

# clean
rm(cell.list.border,cell.list.issc, cell.list.gc,
   border.sel, issc.sel, gc.sel,
   clu.light.rotation.complete.figure,
   mapRotation, mapRotationLong, sessions,tmaps,
   sem,plot.mean.sem.per.group.time)

