rate.change.stats<-function(){
  
  print("**********************")
  print("**** rate change  ****")
  print("**********************")
  
  print("Testing if rate changes index significantly different than 0")
  print(length(rateChange$index))
  print(summary(rateChange$index))
  print(wilcox.test(rateChange$index))
  
  print("Testing if rate changes index significantly different than 0, only mec cells")
  x<-rateChange$index[which(rateChange$clu.id%in%cells$cell.id[which(cells$region=="mec")])]
  print(length(x))    
  print(summary(x))
  print(wilcox.test(x))
  
  print("Testing if rate changes index significantly different than 0, mouse aggregate")
  rateChange$session<- sessionNameFromCluId(rateChange$clu.id)
  rateChange$mouse<- animalNameFromSessionName(rateChange$session)
  x.agg<-aggregate(rateChange$index,by=list(rateChange$mouse),median)
  colnames(x.agg)<-c("mouse","index")
  print(length(x.agg$mouse))    
  print(summary(x.agg$index))
  print(wilcox.test(x.agg$index))
  
  print("Proportion of cells with significant rate change index")
  print(paste("number of neurons:",length(cells$mouse)))
  print(paste("Number of light sensitive neurons:", sum(cells$light)))
  print(paste("Proportion of all neurons:", round(sum(cells$light)/length(cells$light),3)))
  
  print("Proportion of mec cells with significant rate change index")
  x<-cells[which(cells$region=="mec"),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))

  print("Significant cells with significant increased or decreased rate")
  x<-rateChange$index[which(rateChange$clu.id%in%cells$cell.id[which(cells$light==T)])]
  print(paste("Number of significant neurons:",length(x)))
  print(paste("Number of significant increase:",sum(x>0)))
  print(paste("Number of significant increase:",sum(x<0)))
  print(paste("Proportion of increase out of significant:",round(sum(x>0)/length(x),4)))
  print(paste("Proportion of decrease out of significant:",round(sum(x<0)/length(x),4)))
  print(chisq.test(c(sum(x>0),sum(x<0))))
  
    
  print("Proportion of grid cells")
  x<-cells[which(cells$grid==T),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))
  
  print("Proportion of place cells")
  x<-cells[which(cells$place==T),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))
  
  print("Proportion of border cells")
  x<-cells[which(cells$border==T),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))
  
  print("Proportion of HD cells")
  x<-cells[which(cells$hd==T),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))
  
  print("Proportion of speed cells")
  x<-cells[which(cells$speed==T),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))
  
  
  print("chisq.test on the proportion of significant neurons")
  m<-matrix(nrow=2,ncol=6)
  rownames(m)<-c("ja","nein")
  colnames(m)<-c("g","p","b","h","s","u")
  m[1,1]<-sum(cells$light[which(cells$grid==T)])
  m[2,1]<-sum(!cells$light[which(cells$grid==T)])
  m[1,2]<-sum(cells$light[which(cells$place==T)])
  m[2,2]<-sum(!cells$light[which(cells$place==T)])
  m[1,3]<-sum(cells$light[which(cells$border==T)])
  m[2,3]<-sum(!cells$light[which(cells$border==T)])
  m[1,4]<-sum(cells$light[which(cells$hd==T)])
  m[2,4]<-sum(!cells$light[which(cells$hd==T)])
  m[1,5]<-sum(cells$light[which(cells$speed==T)])
  m[2,5]<-sum(!cells$light[which(cells$speed==T)])
  m[1,6]<-sum(cells$light[which(cells$grid==F&cells$place==F&cells$border==F&cells$hd==F&cells$speed==F)])
  m[2,6]<-sum(!cells$light[which(cells$grid==F&cells$place==F&cells$border==F&cells$hd==F&cells$speed==F)])
  print(m)
  print(chisq.test(m))
  
  
  print("Proportion of neurons with mean firing rate > 10 Hz")
  x<-cells[which(cells$mean.rate>10),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3)))
  
  print("Proportion of neurons with mean firing rate < 5 Hz")
  x<-cells[which(cells$mean.rate< 5),]
  print(paste("number of neurons:",length(x$mouse)))
  print(paste("Number of light sensitive neurons:", sum(x$light)))
  print(paste("Proportion of all neurons:", round(sum(x$light)/length(x$light),3))) 
  
  m<-matrix(ncol=2,nrow=2)
  rownames(m)<-c("ja","nein")
  colnames(m)<-c("p","i")
  m[1,1]<-sum(cells$light[which(cells$mean.rate< 5)])
  m[2,1]<-sum(!cells$light[which(cells$mean.rate< 5)])
  m[1,2]<-sum(cells$light[which(cells$mean.rate>10)])
  m[2,2]<-sum(!cells$light[which(cells$mean.rate>10)])
  print(apply(m,2,sum))
  print(m)
  chisq.test(m)
  
  
  print("Difference of rate change index between neurons with mean firing rate < 5 and > 10")
  x<-rateChange$index[which(rateChange$clu.id%in%cells$cell.id[which(cells$mean.rate>10)])]
  y<-rateChange$index[which(rateChange$clu.id%in%cells$cell.id[which(cells$mean.rate<5)])]
  print("above 10 Hz")
  print(length(x))
  print(summary(x))
  print("below 10 Hz")
  print(length(y))
  print(summary(y))
  print(ks.test(x,y))
  print(wilcox.test(x,y))
  
  print("Correlation between absolute value of firing rate change index and grid spacing")
  a<-data.frame(clu.id=rateChange$clu.id,index=rateChange$index)
  b<-data.frame(clu.id=tstats$clu.id[which(tstats$condition=="l2")],
             spacing=tstats$grid.spacing[which(tstats$condition=="l2")])
  c<-merge(a,b)
  x<-c[which(c$clu.id%in%cells$cell.id[which(cells$grid==T)]),]
  print(paste("Number of grid cells:",length(x$clu.id)))
  print(cor.test(x$spacing,abs(x$index)))
}

plot.rate.condition<-function(x,
                              mgp.x=c(1,0.3,0.2),mgp.y=c(1,0.3,0.2),xlab="",ylab="",
                              outma=c(1,1,0.5,0),margin=c(1,2,1,0.3))
{
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot(x=c(0,3),y=c(0,ceiling(max(c(x$ratel,x$rated)))),type='n', axes=F, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=0, tck=-0.05,cex.axis=0.60,mgp=c(0.8,0.2,0.1),at=c(1,2))
  axis(side = 2, las=2, pos=0,tck=-0.05,cex.axis=0.60,mgp=c(0.8,0.2,0.1))
  rect(xleft=0.6,0,1.4,x$ratel,col="red")
  rect(xleft=1.6,0,2.4,x$rated,col="gray")
  title(ylab="Rate (Hz)",mgp=c(0.8,0.2,0.1))
  text(x=1,y=ceiling(max(c(x$ratel,x$rated))),labels=round(x$index,2),cex=0.6)
  
}
plot.light.selective.category<-function(mgp.x=c(1,0.3,0.2),mgp.y=c(1,0.3,0.2),xlab="",ylab="",
                                        outma=c(1,1,0.5,0),margin=c(2,2,1,0.3))
{
  x<-cells[which(cells$grid==T),]
  gridP<-sum(x$light)/length(x$light)
  x<-cells[which(cells$place==T),]
  placeP<-sum(x$light)/length(x$light)
  x<-cells[which(cells$border==T),]
  borderP<-sum(x$light)/length(x$light)
  x<-cells[which(cells$hd==T),]
  hdP<-sum(x$light)/length(x$light)
  x<-cells[which(cells$speed==T),]
  speedP<-sum(x$light)/length(x$light)
  x<-cells[which(cells$grid==F&cells$place==F&cells$border==F&cells$hd==F&cells$speed==F),]
  uidP<-sum(x$light)/length(x$light)
  prob<-c(gridP,placeP,borderP,hdP,speedP,uidP)
  
  par(mar=margin, oma=outma,cex.lab=0.6,cex.axis=0.6)
  plot(x=c(0,7),y=c(0,ceiling(max(prob))),type='n', axes=F, pch=20,lwd=1,xlab="",ylab="")
  axis(side = 1, pos=0, tck=-0.05,cex.axis=0.60,mgp=c(0.8,0.2,0.1),at=c(1,2,3,4,5,6),labels=c("g","p","b","hd","s","uid"))
  axis(side = 2, las=2, pos=0,tck=-0.05,cex.axis=0.60,mgp=c(0.8,0.2,0.1))
  rect(xleft=0.6,0,1.4,prob[1],col="gray")
  rect(xleft=1.6,0,2.4,prob[2],col="gray")
  rect(xleft=2.6,0,3.4,prob[3],col="gray")
  rect(xleft=3.6,0,4.4,prob[4],col="gray")
  rect(xleft=4.6,0,5.4,prob[5],col="gray")
  rect(xleft=5.6,0,6.4,prob[6],col="gray")
  title(ylab="Sign. Neurons",mgp=c(0.8,0.2,0.1))
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
rate.change.figure<-function(){
  m<-rbind(c(0.00,0.10,0.80,1.00),
           c(0.10,0.20,0.80,1.00),
           c(0.20,0.30,0.80,1.00),
           c(0.30,0.40,0.80,1.00),
           c(0.40,0.50,0.80,1.00),
           c(0.50,0.60,0.80,1.00),
           
           c(0.00,0.10,0.60,0.80),
           c(0.10,0.20,0.60,0.80),
           c(0.20,0.30,0.60,0.80),
           c(0.30,0.40,0.60,0.80),
           c(0.40,0.50,0.60,0.80),
           c(0.50,0.60,0.60,0.80),
           
           c(0.00,0.10,0.40,0.60),
           c(0.10,0.20,0.40,0.60),
           c(0.20,0.30,0.40,0.60),
           c(0.30,0.40,0.40,0.60),
           c(0.40,0.50,0.40,0.60),
           c(0.50,0.60,0.40,0.60),
           
           c(0.00,0.25,0.10,0.35),
           c(0.25,0.50,0.10,0.35))
  
  fn<-paste(ep@directory,"figures_relectro","rate_figure_7.pdf",sep="/")
  print(paste("creating",fn))
  pdf(file=fn,onefile=TRUE,width=6,height=5)
  #x11(width=7,height=8)
  split.screen(m)
  ### examples of speed cells
  for(i in 1:length(sel.cells))
  {
    ## rate maps
    index=(i-1)*3+1
    screen(index)
    x<-maps.examples[which(maps.examples$clu.id==sel.cells[i]&maps.examples$condition=="l1"),]
    firingRateMapPlot(x,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    screen(index+1)
    x<-maps.examples[which(maps.examples$clu.id==sel.cells[i]&maps.examples$condition=="d1"),]
    firingRateMapPlot(x,outma=c(2.0,1.0,2.0,1.0),margin=c(0.6,0.1,0.6,0.1))
    x<-rateChange[which(rateChange$clu.id==sel.cells[i]),]
    screen(index+2) 
    plot.rate.condition(x)  
  }
  screen(19)
  plot.hist.one.distribution(rateChange$index, min=-1,max=1,int=0.05,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,
                             plotxlim=c(-1,1),plotylim=c(0,150),outma=c(1,1,1,0),margin=c(2,2,1,1),
                             mgp.x=c(1.2,0.3,0),mgp.y=c(1.2,0.3,0),
                             probability=1, xaxis.at=seq(-1,1,0.5),ylab="Neurons",xlab=expression((lambda[l]-lambda[d])/(lambda[l]+lambda[d])),vline=0)
  screen(20)
  plot.light.selective.category()
  close.screen(all=TRUE)
  dev.off()
}
##############################################
########### load data.frames #################
##############################################
source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@resultsDirectory,"tmaps",sep="/"))
load(paste(ep@resultsDirectory,"tstats",sep="/"))
load(paste(ep@resultsDirectory,"rateChange",sep="/"))
load(paste(ep@resultsDirectory,"rateChangeShuf",sep="/"))
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)
xx<-tmaps

### get the threshold for each cell
b<-by(rateChangeShuf$index,list(rateChangeShuf$clu.id),quantile,0.99)
c<-by(rateChangeShuf$index,list(rateChangeShuf$clu.id),quantile,0.01)
t<-data.frame(clu.id=names(b),ht=as.numeric(b),lt=as.numeric(c))
rateChange<-merge(rateChange,t)
rateChange$sign<-ifelse(rateChange$index<rateChange$lt|rateChange$index>rateChange$ht,TRUE,FALSE)
cells$light<-FALSE
cells$light[which(cells$cell.id%in%rateChange$clu.id[which(rateChange$sign==T)])]<-TRUE

## do the stats
rate.change.stats()


## cells for figure ##
sel.cells<-c("jp19841-01072015-0108_9","jp19841-12072015-0108_7","jp19841-21072015-0108_9",
             "jp19841-12072015-0108_17","jp19844-12082015-0108_3","jp19841-07072015-0108_4")
maps.examples<-xx[which(xx$clu.id%in%sel.cells),]
rate.change.figure()


rm(rate.change.stats,rate.change.figure)
rm(t,rateChange,tmaps,tstats,rateChangeShuf)
rm(a,b,c,d,sel.cells,maps.examples,xx,e)
rm(plot.hist.one.distribution,
   plot.light.selective.category,
   plot.rate.condition,
   rename.condition.data.frame)
