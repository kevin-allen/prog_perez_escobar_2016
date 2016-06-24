#############################################################
### plot the firing rate maps of place cells in a pdf file ###
#############################################################
firing.rate.map.plot <- function(df, outma=c(1,1,1,1),margin=c(0.5,0.5,0.5,0.5),axis.x.mgp=c(0,0,0),axis.y.mgp=c(0,0,0),
                                 cex.x.axis=0.6,cex.y.axis=0.6,cex.lab=0.6,xlab="",ylab="",show.xlab=TRUE,main.title="",peak.rate.prefix="",
                                 transpose=F)
{
  
  jet.colors = colorRampPalette(c("#00007F", "blue","#007FFF",  "cyan", "#7FFF7F", "yellow", "#FF7F00","red"))
  #print(paste(unique(df$clu.id),max(df$rate,na.rm=T)))
  par(oma=outma,mar=margin)
  xlen <- length(unique(df$x))
  ylen <- length(unique(df$y))
  if(transpose==F){
    zzz <- matrix(df$rate,nrow=xlen,ncol=ylen)
    image(unique(df$x),unique(df$y),zzz,zlim=c(0,max(df$rate,na.rm=T)), col=jet.colors(200),xlab='',ylab='',axes=FALSE)
  } else{
    zzz <- t(matrix(df$rate,nrow=xlen,ncol=ylen))
    image(unique(df$y),unique(df$x),zzz,zlim=c(0,max(df$rate,na.rm=T)), col=jet.colors(200),xlab='',ylab='',axes=FALSE)
  }
  mtext(paste(peak.rate.prefix,round(max(df$rate,na.rm=T),digits=2),"Hz"),side=3,at=median(unique(df$x)),line=-0.1,cex=0.5)
  if(main.title!="")
  {
    mtext(main.title,side=3,at=median(unique(df$x)),line=0.3,cex=0.4)
  }
}


linear.rate.plot.light<-function(x,axis.y.pos=-25,axis.x.pos=0,axis.y.las=2,
                          mgp.x=c(0.5,0.05,0.0),mgp.y=c(.7,0.3,0.2),xlab="",ylab="",
                          plotxlim=c(0,80),plotylim=c(0,50),
                          outma=c(1,0.6,1,1.5),margin=c(1.5,1.5,0.8,0.5),
                          xaxis.at=seq(0,50,10),yaxis.at=seq(0,50,10),...)
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=axis.x.pos, at=xaxis.at, tck=-0.05,cex.axis=0.60,mgp=mgp.x)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.y)
  lines (x$x[which(x$condition=="l1")],x$rate[which(x$condition=="l1")],type='l',pch=20,xlab='',ylab='',lwd=0.75,col="blue")
  lines (x$x[which(x$condition=="l2")],x$rate[which(x$condition=="l2")],type='l',pch=20,xlab='',ylab='',lwd=0.75,col="red")
  lines (x$x[which(x$condition=="d")],x$rate[which(x$condition=="d")],type='l',pch=20,xlab='',ylab='',lwd=0.75,col="black")
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
}



plot.page.full.maps<-function(fn="one_d_maps.pdf",cell.list=cell.list,maps=s,lt2d=lt2,lt1d=lt1){
  num.rows<-8
  ### this defines the organisation of screens for 1 cell (one row)
  row.height<-1/num.rows
  x1<-c(0.00,0.20,0.20,0.20,0.40,0.60,0.60,0.60,0.80) ## x1 within its row
  x2<-c(0.20,0.40,0.40,0.40,0.60,0.80,0.80,0.80,1.00) ## x2 within its row
  y1<-c(0.00,0.65,0.35,0.05,0.00,0.65,0.35,0.05,0.00) ## y1 withing its row
  y2<-c(1.00,0.95,0.65,0.35,1.00,0.95,0.65,0.35,1.00) ## y2 withing its row
  ## making the matrix for the entire page
  m<-matrix(c(rep(x1,num.rows),
  rep(x2,num.rows),
  rep(seq(1-row.height,0,-row.height),each=length(x1))+row.height*y1,
  rep(seq(1-row.height,0,-row.height),each=length(x1))+row.height*y2),ncol=4)
  
  plot.per.page=length(x1)*num.rows
  
  pdf(file=fn,onefile=TRUE,paper="a4",height=10, width = 6)
  
  index=1
  for (cellid in cell.list) ## any cell list
  {
    #print(cellid)
    if(index==1)
    {
      split.screen(m)  
    }
    ## sqr70 map
    x<-maps[which(maps$clu.id==cellid),]
    screen(index)
    firing.rate.map.plot(df=x,main.title=cellid,outma=c(1,1,1,1),margin=c(0.75,0.5,1.0,0.5))

    ## lin maps direction 1
    x<-lt2d[which(lt2d$clu.id==cellid&lt2d$direction==0&lt2d$condition=="l1"),]
    screen(index+1)
    firing.rate.map.plot(df=x,transpose = T)
    x<-lt2d[which(lt2d$clu.id==cellid&lt2d$direction==0&lt2d$condition=="l2"),]
    screen(index+2)
    firing.rate.map.plot(df=x,transpose = T)
    x<-lt2d[which(lt2d$clu.id==cellid&lt2d$direction==0&lt2d$condition=="d"),]
    screen(index+3)
    firing.rate.map.plot(df=x,transpose = T)

    x<-lt1d[which(lt1d$clu.id==cellid&lt1d$direction==0),]
    screen(index+4)
    maxy<-max(x$rate)
    linear.rate.plot.light(x=x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                          xlab="Position (cm)",ylab="Rate",
                          plotxlim=c(0,80),plotylim=c(0,maxy),
                          xaxis.at=seq(0,80,20),yaxis.at=seq(0,maxy,5))

    
    ## lin maps direction 2
    x<-lt2d[which(lt2d$clu.id==cellid&lt2d$direction==1&lt2d$condition=="l1"),]
    screen(index+5)
    firing.rate.map.plot(df=x,transpose = T)
    x<-lt2d[which(lt2d$clu.id==cellid&lt2d$direction==1&lt2d$condition=="l2"),]
    screen(index+6)
    firing.rate.map.plot(df=x,transpose = T)
    x<-lt2d[which(lt2d$clu.id==cellid&lt2d$direction==1&lt2d$condition=="d"),]
    screen(index+7)
    firing.rate.map.plot(df=x,transpose = T)
    
    x<-lt1d[which(lt1d$clu.id==cellid&lt1d$direction==1),]
    screen(index+8)
    maxy<-max(x$rate)
    linear.rate.plot.light(x=x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                           xlab="Position (cm)",ylab="Rate",
                           plotxlim=c(0,80),plotylim=c(0,maxy),
                           xaxis.at=seq(0,80,20),yaxis.at=seq(0,maxy,5))
    
    index=index+8
    
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


## load all the firing rate maps
load(paste(ep@directory,"results","sqr70.maps",sep="/"))
load(paste(ep@directory,"results","lt.maps.2d",sep="/"))
load(paste(ep@directory,"results","lt.maps.1d",sep="/"))

## save in a figures directory
dir<-paste(ep@directory,"figures",sep="/")
if(!dir.exists(dir)){
  dir.create(dir)
}

## plot all cells
fn=paste(dir,"maps_light.pdf",sep="/")
print(paste("Plotting maps in different light conditions",fn))
plot.page.full.maps(fn=fn,cell.list=unique(sqr70.maps$clu.id),maps=sqr70.maps,lt2d=lt.maps.2d,lt1d=lt.maps.1d)


## clean the environment
rm(plot.page.full.maps,firing.rate.map.plot,linear.rate.plot.light,fn)
rm(sqr70.maps,lt.maps.1d,lt.maps.2d,dir)
