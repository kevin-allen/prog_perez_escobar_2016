trialSpikeTriggeredMaps<-function(rs){
  print(paste(rs@session,rs@path))
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
 
  allConds<-sort(as.character(unique(int$condition))) ## sort alphabetically
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
  
  index=1
  for (cond in allConds){
    stc<-setIntervals(st=st,s=int$start[which(int$condition==cond)],e=int$end[which(int$condition==cond)])
    sp<-spikeTriggeredFiringRateMap2d(sp,stc,pt,minIsiMs=0,maxIsiMs=10000)
    maps<-mapsAsDataFrame(sp) ## get the maps out before shuffling
    pts<-shiftPositionRandom(pt)
    sp<-spikeTriggeredFiringRateMap2d(sp,stc,pts,minIsiMs=0,maxIsiMs=10000)
    mapsS<-mapsAsDataFrame(sp) ## get the maps out before shuffling
    maps$condition<-cond
    mapsS$condition<-cond
    if(index==1){
      trigMaps<-maps
      trigMapsShuf<-mapsS
    } else {
      trigMaps<-rbind(trigMaps,maps)
      trigMapsShuf<-rbind(trigMapsShuf,mapsS)
    }
    index=index+1
  }
  
  rename.condition.data.frame <- function(x){
    # return a data frame in which condition is changed to l1 l2 d1 and d2
    c<-sort(as.character(unique(x$condition))) # d1 d2 l1 l2 ## alphabetically
    x$condition<-as.character(x$condition)
    if(length(c)==4){
      x$condition[which(x$condition==c[1])]<-"d1"
      x$condition[which(x$condition==c[2])]<-"d2"
      x$condition[which(x$condition==c[3])]<-"l1"
      x$condition[which(x$condition==c[4])]<-"l2"
    }
    else{
      x$condition[which(x$condition==c[1])]<-"d1"
      x$condition[which(x$condition==c[2])]<-"l1"
    }
    x$condition<-as.factor(x$condition)
    return(x)
  }
  trigMaps<-rename.condition.data.frame(trigMaps)
  trigMapsShuf<-rename.condition.data.frame(trigMapsShuf)
  
  return(list(trigMaps=trigMaps,trigMapsShuf=trigMapsShuf))
}