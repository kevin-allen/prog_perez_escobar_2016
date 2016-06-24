spikeErrorDistanceMetric<-function(rs,cells){
  print(rs@session)
  
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  cg<-myList$cg
  sp<-myList$sp
  hd<-myList$hd
  
  ## get the trial intervals
  tfile<-paste(paste(rs@path,rs@session,sep="/"),".light_trials_intervals",sep="")
  if(!file.exists(tfile))
    stop(paste("file missing:",tfile))
  int<-read.table(file=tfile,header=F)
  colnames(int)<-c("no","condition","start","end")
  if(length(int$no)!=60)
    stop("length of int$no is not 60")
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
  
  
  
  ## select some cells
  gc<-as.character(cells$cell.id[which(cells$session==rs@session&cells$grid==T)])
  if(length(gc)==0)
    stop(paste("spikeDistanceMetric: length(gc)==0",rs@session))
  cl<-as.numeric(unlist(strsplit(gc,split='_'))[seq(2,length(gc)*2,2)])
  st<-setCellList(st,cellList=cl)
  
  cond<-sort(unique(int$condition))
  if(length(cond)==4){
    int.l1<-int[which(int$condition==cond[3]),]
    int.d1<-int[which(int$condition==cond[1]),]
  }
  if(length(cond)==2){
    int.l1<-int[which(int$condition==cond[2]),]
    int.d1<-int[which(int$condition==cond[1]),]
  }
  
  l1.sec=60
  ## set the intervals to the first half of l1 for detection of the field
  st<-setIntervals(st,s=int.l1$start,e=int.l1$start+(l1.sec*rs@samplingRate))
  
  ## get the map for field detection
  sp<-firingRateMap2d(sp,st,pt)
  
  ## get the spike distance metric for each spike relative to l1-d1 transitions
  s<-int.d1$start-(l1.sec*rs@samplingRate) ## include the second half of l1 as positive control
  e<-int.d1$end
  pField<-75
  
  ## only keep data from the half l1 and all d1
  pt<-setInvalidOutsideInterval(pt,s=s,e=e)
  
  ## do it for real data
  sdm<-spikeDistanceMetric(sp,st,pt,startIntervals=s,endIntervals=e,percentileField=pField)
  sdm$clu.id<-paste(rs@session,sdm$clu,sep="_")
  sdm$time<-sdm$time-l1.sec
 
  ## do it after shifting the position data, as a control 
  pt<-shiftPositionRandom(pt)
  sdm.shuf<-spikeDistanceMetric(sp,st,pt,startIntervals=s,endIntervals=e,percentileField=pField)
  sdm.shuf$clu.id<-paste(rs@session,sdm.shuf$clu,sep="_")
  sdm.shuf$time<-sdm.shuf$time-l1.sec
  
  return(list(sdm=sdm,sdm.shuf=sdm.shuf))
}