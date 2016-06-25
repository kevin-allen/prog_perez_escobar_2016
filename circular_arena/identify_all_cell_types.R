distribution.shuffled.threshold.plot<-function(x,s,t,
                                               min=-0.8,max=0.8,int=0.025,axis.y.pos=-0.4,axis.x.pos=0,axis.y.las=2,
                                               plotxlim=c(-0.4,0.8),plotylim=c(0,0.2),outma=c(1.5,1,1.5,0),margin=c(1.5,2,0.7,1),
                                               mgpx=c(0.5,0.05,0),mgpy=c(1.2,0.3,0),
                                               probability=1, xaxis.at=seq(-0.4,0.8,0.4),ylab="",xlab="",
                                               data.color="purple1",
                                               add.text="",add.text.x=0, add.text.y=0,...){
  # plot a histogram of shuffled data with a line of the real data and a vertical bar indicating the threshold
  # x: vector of real data
  # s: vector of shuffled data
  # t: threshold
  par(cex.lab=0.6,cex.axis=0.6)
  par(mar=margin)
  par(oma=outma)
  
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
  if(t!=""){
    lines(c(t,t),c(0,plotylim[2]),ylab="",xlab="",lty=2,lwd=1,col=2)
  }
  if(add.text!=""){
    text(labels=add.text,x=add.text.x,y=add.text.y,cex=0.6)
  }
  par(mgp=mgpx)
  axis(side = 1, at=xaxis.at, pos=axis.x.pos,tck=-0.05,cex.axis=0.60)
  par(mgp=mgpy)
  axis(side = 2, las=axis.y.las, pos=axis.y.pos,tck=-0.05,cex.axis=0.60)
  title(xlab=xlab,mgp=mgpx)
  title(ylab=ylab,mgp=mgpy)
}

thresholds.figure<-function()
{
  if(!dir.exists(paste(ep@directory,"figures_relectro",sep="/"))){
    dir.create(paste(ep@directory,"figures_relectro",sep="/"))
  }
  print(paste("generating",paste(ep@directory,"figures_relectro","thresholds.figure.pdf",sep="/")))
  pdf(file=paste(ep@directory,"figures_relectro","thresholds.figure.pdf",sep="/"),onefile=TRUE,width=5,height=6)
  #x11(width=6,height=6)
  num.cols<-4
  num.rows<-4
  m<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
              rep(seq(1/num.cols,1,1/num.cols),num.rows),
              rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
              rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  
  split.screen(m)
  
  screen(1)
  distribution.shuffled.threshold.plot(x=bstats$info.score,s=bstats.shuf$info.score,t=bt.info.score,
                                       min=0,max=20,int=0.075,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,3),plotylim=c(0,0.25),
                                       probability=1, xaxis.at=seq(0,4,1),ylab="Probability",xlab="Info score",
                                       data.color="purple1",
                                       add.text = round(bt.info.score,3),add.text.x = 2,add.text.y = 0.25)
  screen(2)
  distribution.shuffled.threshold.plot(x=bstats$grid.score,s=bstats.shuf$grid.score,t=bt.grid.score,
                                       min=-1.5,max=2,int=0.05,axis.y.pos=-0.8,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(-0.8,1.6),plotylim=c(0,0.15),
                                       probability=1, xaxis.at=seq(-0.8,1.6,0.4),ylab="Probability",xlab="Grid score",
                                       data.color="purple1",
                                       add.text = round(bt.grid.score,3),add.text.x = 1,add.text.y = 0.15)
  screen(3)
  distribution.shuffled.threshold.plot(x=bstats$cm.5,s=bstats.shuf$cm.5,t="",
                                       min=0,max=1,int=0.025,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,1),plotylim=c(0,0.2),
                                       probability=1, xaxis.at=seq(0,1,0.2),ylab="Probability",xlab=expression("CM"[0.5]),
                                       data.color="purple1")
  screen(4)
  distribution.shuffled.threshold.plot(x=bstats$dm,s=bstats.shuf$dm,t="",
                                       min=0,max=1,int=0.03,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,1),plotylim=c(0,0.35),
                                       probability=1, xaxis.at=seq(0,1,0.2),ylab="Probability",xlab="DM",
                                       data.color="purple1")
  screen(5)
  distribution.shuffled.threshold.plot(x=bstats$hd.vl,s=bstats.shuf$hd.vl,t=bt.hd.vl,
                                       min=-0.5,max=1,int=0.03,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,1),plotylim=c(0,0.35),
                                       probability=1, xaxis.at=seq(0,1,0.2),ylab="Probability",xlab="HD selectivity",
                                       data.color="purple1",
                                       add.text = round(bt.hd.vl,3),add.text.x = 0.6,add.text.y = 0.35)
  
  screen(6)
  distribution.shuffled.threshold.plot(x=bstats$speed.score,s=bstats.shuf$speed.score,t=bt.speed.score,
                                       min=-1,max=1,int=0.02,axis.y.pos=-0.4,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(-0.4,0.4),plotylim=c(0,0.35),
                                       probability=1, xaxis.at=seq(-0.4,0.4,0.2),ylab="Probability",xlab="Speed score",
                                       data.color="purple1",
                                       add.text = round(bt.speed.score,3),add.text.x = 0.25,add.text.y = 0.35)
  
  screen(7)
  distribution.shuffled.threshold.plot(x=bstats$circular.border.score,s=bstats.shuf$circular.border.score,t=bt.circular.border.score,
                                       min=-1,max=1,int=0.04,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(-1,1),plotylim=c(0,0.15),
                                       probability=1, xaxis.at=seq(-1,1,0.5),ylab="Probability",xlab="Border score",
                                       data.color="purple1",
                                       add.text = round(bt.circular.border.score,3),add.text.x = -0.25,add.text.y = 0.15)
  
  
  
  
  
  
  screen(9)
  distribution.shuffled.threshold.plot(x=tstats$info.score,s=tstats.shuf$info.score,t=tt.info.score,
                                       min=0,max=15,int=0.075,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,3),plotylim=c(0,0.25),
                                       probability=1, xaxis.at=seq(0,4,1),ylab="Probability",xlab="Info score",
                                       data.color="purple1",
                                       add.text = round(tt.info.score,3),add.text.x = 2,add.text.y = 0.25)
  
  screen(10)
  distribution.shuffled.threshold.plot(x=tstats$grid.score,s=tstats.shuf$grid.score,t=tt.grid.score,
                                       min=-1,max=2,int=0.05,axis.y.pos=-0.8,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(-0.8,1.6),plotylim=c(0,0.15),
                                       probability=1, xaxis.at=seq(-0.8,1.6,0.4),ylab="Probability",xlab="Grid score",
                                       data.color="purple1",
                                       add.text = round(tt.grid.score,3),add.text.x = 1,add.text.y = 0.15)
  screen(11)
  distribution.shuffled.threshold.plot(x=tstats$cm.5,s=tstats.shuf$cm.5,t="",
                                       min=0,max=1,int=0.025,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,1),plotylim=c(0,0.2),
                                       probability=1, xaxis.at=seq(0,1,0.2),ylab="Probability",xlab=expression("CM"[0.5]),
                                       data.color="purple1")
  screen(12)
  distribution.shuffled.threshold.plot(x=tstats$dm,s=tstats.shuf$dm,t="",
                                       min=0,max=1,int=0.03,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,1),plotylim=c(0,0.35),
                                       probability=1, xaxis.at=seq(0,1,0.2),ylab="Probability",xlab="DM",
                                       data.color="purple1")
  
  screen(13)
  distribution.shuffled.threshold.plot(x=tstats$hd.vl,s=tstats.shuf$hd.vl,t=tt.hd.vl,
                                       min=-0.5,max=1,int=0.03,axis.y.pos=0,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(0,1),plotylim=c(0,0.35),
                                       probability=1, xaxis.at=seq(0,1,0.2),ylab="Probability",xlab="HD selectivity",
                                       data.color="purple1",
                                       add.text = round(tt.hd.vl,3),add.text.x = 0.6,add.text.y = 0.35)
  
  screen(14)
  distribution.shuffled.threshold.plot(x=tstats$speed.score,s=tstats.shuf$speed.score,t=tt.speed.score,
                                       min=-1,max=1,int=0.02,axis.y.pos=-0.4,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(-0.4,0.4),plotylim=c(0,0.35),
                                       probability=1, xaxis.at=seq(-0.4,0.4,0.2),ylab="Probability",xlab="Speed score",
                                       data.color="purple1",
                                       add.text = round(tt.speed.score,3),add.text.x = 0.25,add.text.y = 0.35)
  
  screen(15)
  distribution.shuffled.threshold.plot(x=tstats$circular.border.score,s=tstats.shuf$circular.border.score,t=tt.circular.border.score,
                                       min=-1,max=1,int=0.04,axis.y.pos=-1,axis.x.pos=0,axis.y.las=2,
                                       plotxlim=c(-1,1),plotylim=c(0,0.15),
                                       probability=1, xaxis.at=seq(-1,1,0.5),ylab="Probability",xlab="Border score",
                                       data.color="purple1",
                                       add.text = round(tt.circular.border.score,3),add.text.x = -0.25,add.text.y = 0.15)
  close.screen(all=TRUE)
  dev.off()
}


#####################
### start here ######
#####################

source("~/repo/prog_perez_escobar_2016/circular_arena/rename_condition.R") # define function
load(paste(ep@directory,"results","bstats",sep="/"))
load(paste(ep@directory,"results","tstats",sep="/"))
load(paste(ep@directory,"results","bstats.shuf",sep="/"))
load(paste(ep@directory,"results","tstats.shuf",sep="/"))
load(paste(ep@directory,"results","sessions",sep="/"))

if(length(cells$cell.id)!=length(unique(bstats$clu.id)))
  stop("problem with length of bstats")
if(length(cells$cell.id)!=length(unique(tstats$clu.id)))
  stop("problem with length of tstats")

# use only l2 for detection to avoid bias
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tstats.shuf<-unsplit(lapply(split(tstats.shuf,tstats.shuf$session),rename.condition.data.frame), tstats.shuf$session)
tstats<-tstats[which(tstats$condition=="l2"),]
tstats.shuf<-tstats.shuf[which(tstats.shuf$condition=="l2"),]

if(length(unique(tstats$session))!=length(sessions$session[which(sessions$num.lights==2)]))
  stop("problem with the number of sesssions with 2 lights")

###################################################
## Modify border score for circular environment  ##
###################################################
bstats$cm.5<-(1-abs(0.5-bstats$cm)*2)
bstats$circular.border.score<-(bstats$cm.5-bstats$dm)/(bstats$cm.5+bstats$dm)
bstats.shuf$cm.5<-(1-abs(0.5-bstats.shuf$cm)*2)
bstats.shuf$circular.border.score<-(bstats.shuf$cm.5-bstats.shuf$dm)/(bstats.shuf$cm.5+bstats.shuf$dm)

tstats$cm.5<-(1-abs(0.5-tstats$cm)*2)
tstats$circular.border.score<-(tstats$cm.5-tstats$dm)/(tstats$cm.5+tstats$dm)
tstats.shuf$cm.5<-(1-abs(0.5-tstats.shuf$cm)*2)
tstats.shuf$circular.border.score<-(tstats.shuf$cm.5-tstats.shuf$dm)/(tstats.shuf$cm.5+tstats.shuf$dm)


############################
## significance levels  ####
############################
prob=0.95
# for baseline
bt.grid.score<-quantile(bstats.shuf$grid.score,prob)
bt.info.score<-quantile(bstats.shuf$info.score,prob)
bt.hd.vl<-quantile(bstats.shuf$hd.vl,prob,na.rm=T)
bt.speed.score<-quantile(bstats.shuf$speed.score,prob)
bt.circular.border.score<-quantile(bstats.shuf$circular.border.score,prob,na.rm=T)
bt.border.score<-quantile(bstats.shuf$border.score,prob,na.rm=T)
# for trials
tt.grid.score<-quantile(tstats.shuf$grid.score,prob)
tt.info.score<-quantile(tstats.shuf$info.score,prob)
tt.hd.vl<-quantile(tstats.shuf$hd.vl,prob,na.rm=T)
tt.speed.score<-quantile(tstats.shuf$speed.score,prob)
tt.circular.border.score<-quantile(tstats.shuf$circular.border.score,prob,na.rm=T)
tt.border.score<-quantile(tstats.shuf$circular.border.score,prob,na.rm=T)
###
print(paste("threshold from shuffling with a probability of",prob, "for baseline and trials"))
print(paste("grid threshold:",round(bt.grid.score,4),round(tt.grid.score,4)))
print(paste("info threshold:",round(bt.info.score,4),round(tt.info.score,4)))
print(paste("hd threshold:",round(bt.hd.vl,4),round(tt.hd.vl,4)))
print(paste("speed threshold:",round(bt.speed.score,4),round(tt.speed.score,4)))
print(paste("circular border score threshold:",round(bt.circular.border.score,4),round(tt.circular.border.score,4)))

###################################################################################################
### plot the distributions of shuffled scores, threshold and real scores for baseline and l2 ######
###################################################################################################
thresholds.figure()



#############################################################
### detection: either 2 baseline above thresholds or ########
### one baseline and l2                              ########
#############################################################
print("A cell needs to meet criteria on both baselines or on one baseline and l2")

##########################
### detect grid cells ####
##########################
### use b1, b2 and l2, above threshold in at least 2 out of 3
### use 95 percentile of grid score
## get the two baseline on the same line
dd<-data.frame(bstats$clu.id,bstats$baseline,bstats$grid.score)
colnames(dd) <- c("clu.id","baseline.no","grid.score")
dd <- reshape(dd,idvar="clu.id",v.names=c("grid.score"),timevar= "baseline.no",direction= "wide")

## for cells with 1 light, they need the two positive baselines
list1<-as.character(dd$clu.id[which(  dd$grid.score.1>bt.grid.score&
                                      dd$grid.score.2>bt.grid.score)])
## for cells with 2 lights, they need at least 2 out of 3 (b1, b2, and l2)
list2<-as.character(dd$clu.id[which((dd$grid.score.1>bt.grid.score)  |
                                    (dd$grid.score.2>bt.grid.score) )])
list3<-as.character(tstats$clu.id[which(tstats$grid.score>tt.grid.score)])
list2<-list2[list2%in%list3]
## selection on baselines and l2
gc<-unique(c(list1,list2)) ## part of list1 or list2
cells$grid<-FALSE
cells$grid[which(cells$cell.id%in%gc)]<-TRUE
## selection only baselines
gc<-unique(c(list1)) ## part of list1 or list2
cells$grid.b<-FALSE
cells$grid.b[which(cells$cell.id%in%gc)]<-TRUE
print(paste("Number of grid cells (baseline and l2):",sum(cells$grid)))
print(paste("Number of grid cells (baseline only):",sum(cells$grid.b)))
rm(dd,list1,list2,list3,gc)

###########################
### detect border cells ###
###########################
## 2 baselines or one baseline and l2
dd<-data.frame(bstats$clu.id, bstats$baseline, bstats$circular.border.score,bstats$polarity)
colnames(dd) <- c("clu.id","baseline.no","circular.border.score","polarity")
dd <- reshape(dd,idvar="clu.id",v.names=c("circular.border.score","polarity"),timevar= "baseline.no",direction= "wide")

## get scores for 2 light trials
list1<-as.character(dd$clu.id[which(dd$circular.border.score.1>bt.circular.border.score&
                                      dd$circular.border.score.2>bt.circular.border.score)])

list2<-as.character(dd$clu.id[which(dd$circular.border.score.1>bt.circular.border.score|
                                      dd$circular.border.score.2>bt.circular.border.score)])
list3<-as.character(tstats$clu.id[which(tstats$circular.border.score>tt.circular.border.score)])

list2<-list2[list2%in%list3]
bc<-unique(c(list1,list2)) ## part of list1 or list2
cells$border<-FALSE
cells$border[which(cells$cell.id%in%bc)]<-TRUE
bc<-unique(c(list1)) ## part of list1
cells$border.b<-FALSE
cells$border.b[which(cells$cell.id%in%bc)]<-TRUE
print(paste("Number of border cells (baseline and l2):",sum(cells$border)))
print(paste("Number of border cells (baseline only):",sum(cells$border.b)))
rm(dd,bc,list1,list2,list3)

##########################
### detect place cells ###
##########################
# cells with information score > t.info during both baseline
dd<-data.frame(bstats$clu.id, bstats$baseline, bstats$info.score)
colnames(dd) <- c("clu.id","baseline.no","info.score")
dd <- reshape(dd,idvar="clu.id",v.names=c("info.score"),timevar= "baseline.no",direction= "wide")
list1<-as.character(dd$clu.id[which(dd$info.score.1>bt.info.score&
                                   dd$info.score.2>bt.info.score)])
list2<-as.character(dd$clu.id[which(dd$info.score.1>bt.info.score|
                                      dd$info.score.2>bt.info.score)])
list3<-as.character(tstats$clu.id[which(tstats$info.score>tt.info.score)])

list2<-list2[list2%in%list3]
pc<-unique(c(list1,list2)) ## part of list1 or list2
cells$place<-FALSE
cells$place[which(cells$cell.id%in%pc)]<-TRUE
print(paste("Number of place cells:",sum(cells$place)))
cells$place[which(cells$grid==T|cells$border==T)]<-FALSE
print(paste("Number of place cells after removing grid and border cells:",sum(cells$place)))
rm(dd,pc,list1,list2,list3)


####################################
### detect speed-modulated cells ###
####################################
dd<-data.frame(bstats$clu.id, bstats$baseline, bstats$speed.score)
colnames(dd) <- c("clu.id","baseline.no","speed.score")
dd <- reshape(dd,idvar="clu.id",v.names=c("speed.score"),timevar= "baseline.no",direction= "wide")
list1<-as.character(dd$clu.id[which(dd$speed.score.1>bt.speed.score&
                                      dd$speed.score.2>bt.speed.score)])
list2<-as.character(dd$clu.id[which(dd$speed.score.1>bt.speed.score|
                                      dd$speed.score.2>bt.speed.score)])
list3<-as.character(tstats$clu.id[which(tstats$speed.score>tt.speed.score)])
list2<-list2[list2%in%list3]
sc<-unique(c(list1,list2)) ## part of list1 or list2
cells$speed<-FALSE
cells$speed[which(cells$cell.id%in%sc)]<-TRUE
print(paste("Number of speed cells:",sum(cells$speed)))
rm(dd,sc,list1,list2,list3)


###################################
### detect head direction cells ###
###################################
dd<-data.frame(bstats$clu.id, bstats$baseline, bstats$hd.vl,bstats$hd.peak.rate)
colnames(dd) <- c("clu.id","baseline.no","hd.vl","hd.peak.rate")
dd <- reshape(dd,idvar="clu.id",v.names=c("hd.vl","hd.peak.rate"),timevar= "baseline.no",direction= "wide")
list1<-as.character(dd$clu.id[which(dd$hd.vl.1>bt.hd.vl& dd$hd.peak.rate.1>5 &
                                  dd$hd.vl.2>bt.hd.vl& dd$hd.peak.rate.2>5)])
list2<-as.character(dd$clu.id[which(dd$hd.vl.1>bt.hd.vl& dd$hd.peak.rate.1>5 |
                                      dd$hd.vl.2>bt.hd.vl& dd$hd.peak.rate.2>5)])
list3<-as.character(tstats$clu.id[which(tstats$hd.vl>tt.hd.vl&tstats$hd.peak.rate>5)])
list2<-list2[list2%in%list3]
hdc<-unique(c(list1,list2)) ## part of list1 or list2
cells$hd<-FALSE
cells$hd[which(cells$cell.id%in%hdc)]<-TRUE
print(paste("Number of hd cells:",sum(cells$hd)))
rm(dd,hdc,list1,list2,list3)



### clean up.
rm(distribution.shuffled.threshold.plot,thresholds.figure)
rm(bstats,tstats,bstats.shuf,tstats.shuf,sessions,prob)
rm(bt.grid.score,bt.speed.score,bt.hd.vl,bt.info.score,bt.circular.border.score,bt.border.score)
rm(tt.grid.score,tt.speed.score,tt.hd.vl,tt.info.score,tt.circular.border.score,tt.border.score)
rm(rename.condition.data.frame)
