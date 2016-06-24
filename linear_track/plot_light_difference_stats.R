#############################################################
### plot the firing rate maps of place cells in a pdf file ###
#############################################################
firing.rate.map.plot <- function(df, outma=c(1,1,1,1),margin=c(0.5,0.5,0.5,0.5),axis.x.mgp=c(0,0,0),axis.y.mgp=c(0,0,0),
                                 cex.x.axis=0.6,cex.y.axis=0.6,cex.lab=0.6,xlab="",ylab="",show.xlab=TRUE,main.title="",peak.rate.prefix="",
                                 transpose=F)
{
  
  jet.colors = colorRampPalette(c("#00007F", "blue","#007FFF",  "cyan", "#7FFF7F", "yellow", "#FF7F00","red"))
 # print(paste(unique(df$clu.id),max(df$rate,na.rm=T)))
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



plot.page.full.same.graph<-function(fn="page.full.plot",cell.list=cell.list,
                                    diff.sign=diff.sign,
                                    maps.1d=maps.1d,
                                    maps.2d=maps.2d){
  num.cols<-5
  num.rows<-8
  plot.per.page=num.cols*num.rows
  m<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
              rep(seq(1/num.cols,1,1/num.cols),num.rows),
              rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
              rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  pdf(file=fn,onefile=TRUE,paper="a4",width=8,height=10)
  index=1
  for (cellid in cell.list) ## any cell list
  {
  #  print(cellid)
    if(index==1)
    {
      split.screen(m)  
    }
    
    screen(index)
    x<-maps.2d[which(maps.2d$clu.id==cellid),]
    if(length(x$x)!=0)
    {
      screen(index)
      firing.rate.map.plot(df=x,main.title=cellid,outma=c(1,1,1,1),margin=c(0.75,0.5,1.0,0.5))
    
    screen(index+1)
    x<-maps.1d[which(maps.1d$clu.id==cellid&maps.1d$direction==0),]
    maxy<-max(x$rate)
    linear.rate.plot.light(x=x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                           xlab="Position (cm)",ylab="Rate",
                           plotxlim=c(0,80),plotylim=c(0,maxy),
                           xaxis.at=seq(0,80,20),yaxis.at=seq(0,maxy,5))

    screen(index+2)
    x<-diff.sign[which(diff.sign$cell.id==cellid&diff.sign$direction==0),]
    linear.diff.plot.light.shuf.obs(x)
    
    screen(index+3)
    x<-maps.1d[which(maps.1d$clu.id==cellid&maps.1d$direction==1),]
    maxy<-max(x$rate)
    linear.rate.plot.light(x=x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                           xlab="Position (cm)",ylab="Rate",
                           plotxlim=c(0,80),plotylim=c(0,maxy),
                           xaxis.at=seq(0,80,20),yaxis.at=seq(0,maxy,5))

    screen(index+4)
    x<-diff.sign[which(diff.sign$cell.id==cellid&diff.sign$direction==1),]
    linear.diff.plot.light.shuf.obs(x)
    }else{
      print(paste("problem with",cellid))
    }
    index=index+4   
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



linear.diff.plot.light.shuf.obs<-function(x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                          mgp.x=c(0.5,0.05,0.0),mgp.y=c(.7,0.3,0.2),xlab="Position (cm)",ylab="Sign difference (Hz",
                                          plotxlim=c(0,80),main.title="",
                                          outma=c(1,0.6,1,1.5),margin=c(1.5,1.5,0.8,0.5),
                                          xaxis.at=seq(0,80,10),yaxis.at=seq(0,50,10),...)
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plotylim<-c(0,max(x[,c(4:9)],na.rm=T))
  plot (x=plotxlim, y=plotylim,type='n', axes=FALSE, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=axis.x.pos, at=xaxis.at, tck=-0.05,cex.axis=0.60,mgp=mgp.x)
  par(mgp=mgp.y)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60,mgp=mgp.y)
  
  lines (x$x,x$l1.l2.obs,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="green")
  lines (x$x,x$l1.d.obs,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="darkblue")
  lines (x$x,x$l2.d.obs,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="darkred")
  
  lines (x$x,x$l1.l2.shuf,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="green",lty=2)
  lines (x$x,x$l1.d.shuf,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="darkblue",lty=2)
  lines (x$x,x$l2.d.shuf,type='l',pch=20,xlab='',ylab='',lwd=0.75,col="darkred",lty=2)
  
  
  sign.x<-x$x[which(x$l1.l2.obs>x$l1.l2.shuf)]
  points(sign.x,rep(0,length(sign.x)),pch=8,col="green")
  
  
  title(xlab=xlab,mgp=mgp.x)
  title(ylab=ylab,mgp=mgp.y)
  if(main.title!="")
  {
    mtext(main.title,side=3,at=median(unique(x$x)),line=0.3,cex=0.4)
  }
}

#######################
### read data files ###
#######################
## load all the firing rate maps
load(paste(ep@directory,"results","sqr70.maps",sep="/"))
load(paste(ep@directory,"results","lt.maps.2d",sep="/"))
load(paste(ep@directory,"results","lt.maps.1d",sep="/"))
load(paste(ep@directory,"results","lt.diff.sign",sep="/"))
load(paste(ep@directory,"results","lt.obs.diff",sep="/"))

if(length(lt.obs.diff$cell.id)!=length(lt.diff.sign$cell.id))
  stop("problem with length of lt.obs.diff and lt.diff.sign")

diff.sign<-merge(lt.diff.sign,lt.obs.diff,by=c("cell.id","direction","x"))
diff.sign<-diff.sign[order(diff.sign$cell.id,diff.sign$x),]
diff.sign$x<-diff.sign$x*2 # to cm




## save in a figures directory
dir<-paste(ep@directory,"figures",sep="/")
if(!dir.exists(dir)){
  dir.create(dir)
}

# plot all cells with rate
print(paste("writing", paste(dir,"sign_diff_lt.pdf",sep="/")))
plot.page.full.same.graph(fn=paste(dir,"sign_diff_lt.pdf",sep="/"),cell.list=cells$cell.id,
                          diff.sign=diff.sign,
                          maps.1d=lt.maps.1d,
                          maps.2d=sqr70.maps)

rm(plot.page.full.same.graph,linear.rate.plot.light,linear.diff.plot.light.shuf.obs,firing.rate.map.plot)
rm(diff.sign,lt.diff.sign,lt.obs.diff,lt.maps.1d,lt.maps.2d,sqr70.maps,dir)

