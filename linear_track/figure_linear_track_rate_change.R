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

plot.hist.one.distribution<-function(x, min=-0.8,max=0.8,int=0.025,axis.y.pos=-0.5,axis.x.pos=0,axis.y.las=2,
                                     plotxlim=c(-0.5,1),plotylim=c(0,20),outma=c(0.5,0.5,0.5,0),margin=c(1.5,1.7,0.5,0.3),
                                     mgp.x=c(0.6,0.05,0),mgp.y=c(1.2,0.3,0),
                                     probability=1, xaxis.at=seq(-1,1,1),ylab="",xlab="",vline="",...)
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
boxplot.three.conditions <- function(data,vd="condition",vi="measured",
                                     outma=c(0.5,0.5,0.5,0),margin=c(1,1.7,0.5,0.3),
                                     mgp.x=c(1.2,0.5,0), mgp.y=c(1,0.5,0),
                                     at.yaxis=seq(0,1,0.2),ylab="",...)
{
  #set xlim to c(0,3) and outline=T or F when calling the function
  par(oma=outma,mar=margin,mgp=mgp.x)
  boxplot(data[,vi]~data[,vd],
          frame.plot=FALSE,axes=FALSE,
          col=c("red","blue","gray40"),xlim=c(0,4),ylim=c(0,1.0),...)
  par(mgp=mgp.x)
  #  axis(side = 1, at=1:3, pos=0,tck=-0.05,cex.axis=0.6,labels=c("","",""))
  par(mgp=mgp.y)
  axis(side = 2, at=at.yaxis,   las=1,pos=0, tck=-0.05,cex.axis=0.6,xpd=TRUE)
  title(ylab=ylab,mgp=mgp.y)
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
          col=c("gray80","gray40"),xlim=c(0,3),...)
  par(mgp=mgp.x)
  axis(side = 1, at=1:2, pos=0,tck=-0.05,cex.axis=0.6,labels=c("",""))
  par(mgp=mgp.y)
  axis(side = 2, at=at.yaxis,   las=1,pos=0, tck=-0.05,cex.axis=0.6,xpd=TRUE)
  title(ylab=ylab,mgp=mgp.y)
}

barplot.percent.change<-function(m,outma=c(0.5,0.5,0.5,0),margin=c(1,1.7,0.5,1.0), mgp.x=c(1.2,0.3,0),mgp.y=c(1.2,0.3,0),main.title="",ylim=c(0,1)){
  par(oma=outma,mar=margin,mgp=mgp.x,cex.lab=0.6,cex.axis=0.6)
  barplot(m,ylim=ylim,ylab="",xlab="",axes=F,cex.axis=0.60)
  axis(side = 1, pos=0,tck=0,mgp=mgp.x,labels=F)
  axis(side = 2, las=2, pos=0,tck=-0.05,mgp=mgp.y,cex.axis=0.60)
  title(ylab="Rate changing neurons (%)")
  if(main.title!="")
  {
    mtext(main.title,side=3,line=0.3,cex=0.5)
  }
}

map.correlation<-function(x){
  if(length(unique(x$condition))!=3)
    stop(paste(x$clu.id[1],"not 3 conditions"))
  x$rate[which(x$rate==-1.0)]<-NA
  if(sum(x$rate,na.rm=T)==0.0) 
    return()
  r.l1.l2<-cor(x$rate[which(x$condition=="l1")],
               x$rate[which(x$condition=="l2")],use="complete.obs")
  r.l1.d<-cor(x$rate[which(x$condition=="l1")],
              x$rate[which(x$condition=="d")],use="complete.obs")
  return(list(r.l1.l2=r.l1.l2,r.l1.d=r.l1.d))
}
info.map.corr.stats<-function(){
  spa.cells<-cells$cell.id[which(cells$grid==T|cells$border==T|cells$place==T)]
  ## prepare information scores, only one direction
  print("information scores of spatially selective neurons (grid, border or place) for l1, l2 and d")
  lt.stats.1d<-lt.stats.1d[which(lt.stats.1d$direction==0&lt.stats.1d$clu.id%in%spa.cells),]
  lt.stats.1d$condition<-factor(lt.stats.1d$condition,levels=c("l1","l2","d"))
  print(length(lt.stats.1d$infoScore[which(lt.stats.1d$condition=="l1")]))
  print("l1")
  print(summary(lt.stats.1d$infoScore[which(lt.stats.1d$condition=="l1")]))
  print("l2")
  print(summary(lt.stats.1d$infoScore[which(lt.stats.1d$condition=="l2")]))
  print("d")
  print(summary(lt.stats.1d$infoScore[which(lt.stats.1d$condition=="d")]))
  print(wilcox.test(lt.stats.1d$infoScore[which(lt.stats.1d$condition=="l1")],
                    lt.stats.1d$infoScore[which(lt.stats.1d$condition=="l2")],paired=T))
  print(wilcox.test(lt.stats.1d$infoScore[which(lt.stats.1d$condition=="l1")],
                    lt.stats.1d$infoScore[which(lt.stats.1d$condition=="d")],paired=T))
  
  
  ### get correlations of firing rate maps between conditions ###
  ### keep only grid cells ###
  print("correlation between l1.l2 and l1.d maps")
  lt.m1dd<-lt.maps.1d[which(lt.maps.1d$direction==0&lt.maps.1d$clu.id%in%spa.cells),]
  list.res<-by(lt.m1dd,lt.m1dd$clu.id,map.correlation)
  r.l1.l2<-as.numeric(do.call("rbind", sapply(list.res,function(x){x["r.l1.l2"]})))
  r.l1.d<-as.numeric(do.call("rbind", sapply(list.res,function(x){x["r.l1.d"]})))
  print("r for l1 vs l2 maps")
  print(summary(r.l1.l2))
  print("r for l1 vs d maps")
  print(summary(r.l1.d))
  print(wilcox.test(r.l1.l2,r.l1.d,paired = T))
}

figure_linear_track<-function(){
  ### this defines the organisation of screens for 1 cell (one row)
  num.cols=8
  num.rows=7
  x1<-seq(0,1-(1/num.cols),1/num.cols) # for each col, one row
  x2<-x1+(1/num.cols) # for each col, one row
  y1<-seq(1-(1/num.rows),0,-1/num.rows)
  y2<-y1+(1/num.rows)
  
  ## making the matrix for the entire page, only put 5 rows for now, examples
  m<-matrix(c(rep(x1,5),
              rep(x2,5),
              rep(y1[1:5],each=num.cols),
              rep(y2[1:5],each=num.cols)),ncol=4)
  
  ## for pop graphs
  n<-matrix(c(c(0.0,0.15,0.30,0.45),
              c(0.15,0.30,0.45,0.60),
              c(0.0,0.0,0.0,0.0),
              c(0.15,0.15,0.15,0.15)),ncol=4)
  m<-rbind(m,n)
  pdf(file=fn,onefile=TRUE,height=7, width = 7)
  split.screen(m) 
  
  index=1
  cell.no=1
  for (cellid in cell.list) ## any cell list
  {
    ## sqr70 map
    x<-maps[which(maps$clu.id==cellid),]
    screen(index)
    firing.rate.map.plot(df=x,main.title=cellid,outma=c(1,1,1,1),margin=c(0.75,0.5,1.0,0.5))
    
    x<-lt1d[which(lt1d$clu.id==cellid&lt1d$direction==0),]
    screen(index+1)
    maxy<-max(x$rate)
    linear.rate.plot.light(x=x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                           xlab="Position (cm)",ylab="Rate",
                           plotxlim=c(0,80),plotylim=c(0,maxy),
                           xaxis.at=seq(0,80,20),yaxis.at=seq(0,maxy,5))
    
    ## lin maps direction 2
    x<-lt1d[which(lt1d$clu.id==cellid&lt1d$direction==1),]
    screen(index+2)
    maxy<-max(x$rate)
    linear.rate.plot.light(x=x,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                           xlab="Position (cm)",ylab="Rate",
                           plotxlim=c(0,80),plotylim=c(0,maxy),
                           xaxis.at=seq(0,80,20),yaxis.at=seq(0,maxy,5))
    if(cell.no%%2==0)
      index=index+3
    else{
      index=index+4
    }
  }
  
  screen(index)
  boxplot.three.conditions(data=lt.stats.1d, 
                           vd="condition", vi="infoScore",
                           ylab="Information Score",
                           outline=F)
  screen(index+1)
  
  spa.cells<-cells$cell.id[which(cells$grid==T|cells$border==T|cells$place==T)]
  lt.m1dd<-lt.maps.1d[which(lt.maps.1d$direction==0&lt.maps.1d$clu.id%in%spa.cells),]
  list.res<-by(lt.m1dd,lt.m1dd$clu.id,map.correlation)
  r.l1.l2<-as.numeric(do.call("rbind", sapply(list.res,function(x){x["r.l1.l2"]})))
  r.l1.d<-as.numeric(do.call("rbind", sapply(list.res,function(x){x["r.l1.d"]})))
  
  plot.hist.one.distribution(r.l1.l2,min=-1.0,max=1.0,int=0.1,axis.y.pos=-1.0,axis.x.pos=0,
                             plotxlim=c(-1.0,1.0),plotylim=c(0,150),
                             ylab="Neurons",xlab="Map stability (l1 vs l2)",
                             col="grey")
  
  
  
  l1.l2.m<-matrix(ncol=6,nrow=2)
  rownames(l1.l2.m)<-c("ja","nein")
  colnames(l1.l2.m)<-c("grid","place","border","hd","speed","uid")
  l1.l2.m["ja","grid"] <-sum(cells$l1.l2[which(cells$grid==T)])
  l1.l2.m["nein","grid"] <-length(cells$l1.l2[which(cells$grid==T)])-sum(cells$l1.l2[which(cells$place==T)])
  l1.l2.m["ja","place"] <-sum(cells$l1.l2[which(cells$place==T)])
  l1.l2.m["nein","place"] <-length(cells$l1.l2[which(cells$place==T)])-sum(cells$l1.l2[which(cells$place==T)])
  l1.l2.m["ja","border"] <-sum(cells$l1.l2[which(cells$border==T)])
  l1.l2.m["nein","border"] <-length(cells$l1.l2[which(cells$border==T)])-sum(cells$l1.l2[which(cells$border==T)])
  l1.l2.m["ja","hd"] <-sum(cells$l1.l2[which(cells$hd==T)])
  l1.l2.m["nein","hd"] <-length(cells$l1.l2[which(cells$hd==T)])-sum(cells$l1.l2[which(cells$hd==T)])
  l1.l2.m["ja","speed"] <-sum(cells$l1.l2[which(cells$speed==T)])
  l1.l2.m["nein","speed"] <-length(cells$l1.l2[which(cells$speed==T)])-sum(cells$l1.l2[which(cells$speed==T)])
  l1.l2.m["ja","uid"] <-sum(cells$l1.l2[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
  l1.l2.m["nein","uid"] <-length(cells$l1.l2[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])-
    sum(cells$l1.l2[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
  
  l1.d.m<-matrix(ncol=6,nrow=2)
  rownames(l1.d.m)<-c("ja","nein")
  colnames(l1.d.m)<-c("grid","place","border","hd","speed","uid")
  l1.d.m["ja","grid"] <-sum(cells$l1.d[which(cells$grid==T)])
  l1.d.m["nein","grid"] <-length(cells$l1.d[which(cells$grid==T)])-sum(cells$l1.d[which(cells$place==T)])
  l1.d.m["ja","place"] <-sum(cells$l1.d[which(cells$place==T)])
  l1.d.m["nein","place"] <-length(cells$l1.d[which(cells$place==T)])-sum(cells$l1.d[which(cells$place==T)])
  l1.d.m["ja","border"] <-sum(cells$l1.d[which(cells$border==T)])
  l1.d.m["nein","border"] <-length(cells$l1.d[which(cells$border==T)])-sum(cells$l1.d[which(cells$border==T)])
  l1.d.m["ja","hd"] <-sum(cells$l1.d[which(cells$hd==T)])
  l1.d.m["nein","hd"] <-length(cells$l1.d[which(cells$hd==T)])-sum(cells$l1.d[which(cells$hd==T)])
  l1.d.m["ja","speed"] <-sum(cells$l1.d[which(cells$speed==T)])
  l1.d.m["nein","speed"] <-length(cells$l1.d[which(cells$speed==T)])-sum(cells$l1.d[which(cells$speed==T)])
  l1.d.m["ja","uid"] <-sum(cells$l1.d[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
  l1.d.m["nein","uid"] <-length(cells$l1.d[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])-
    sum(cells$l1.d[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
  
  
  screen(index+2)
  prop.change<-apply(l1.d.m,2,function(x){x[1]/sum(x)})
  barplot.percent.change(prop.change,ylim=c(0,1))
  screen(index+3)
  prop.change<-apply(l1.l2.m,2,function(x){x[1]/sum(x)})
  barplot.percent.change(prop.change,ylim=c(0,1))
  
  close.screen(all = TRUE)
  dev.off()
}


## load all the firing rate maps
load(paste(ep@directory,"results","sqr70.maps",sep="/"))
load(paste(ep@directory,"results","lt.maps.2d",sep="/"))
load(paste(ep@directory,"results","lt.maps.1d",sep="/"))
load(paste(ep@directory,"results","lt.stats.1d",sep="/"))



## save in a figures directory
dir<-paste(ep@directory,"figures",sep="/")
if(!dir.exists(dir)){
  dir.create(dir)
}

info.map.corr.stats()

## plot all cells
fn=paste(dir,"figure_map_examples.pdf",sep="/")
print(paste("Making figure",fn))
rm(dir)
## get the list of example cells
cell.list<-c("jp4312-27032016-0107_7",
                   "jp4312-24032016-0107_9",
                   "jp4312-21032016-0107_5", 
                   "jp4312-19032016-0107_13", 
                   "jp4312-13032016-0107_11",
                   "jp4298-23022016-0107_6",
                   "jp4298-13022016-0105_7", 
                   "jp4298-15022016-0106_7", 
                   "jp4103-25032016-0107_3", 
                   "jp4298-15022016-0106_9")

maps<-sqr70.maps
lt1d<-lt.maps.1d
figure_linear_track()


## clean the environment
rm(maps,lt1d,cell.list,figure_linear_track,fn,
   sqr70.maps,lt.maps.2d,lt.maps.1d,lt.stats.1d)
rm(barplot.percent.change,
   boxplot.three.conditions,
   boxplot.two.conditions,
   firing.rate.map.plot,
   info.map.corr.stats,
   linear.rate.plot.light,
   map.correlation,
   plot.hist.one.distribution)