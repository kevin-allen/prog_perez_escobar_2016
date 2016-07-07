speedRateTrials<-function(rs){
  print(rs@path)
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  cg<-myList$cg
  sp<-myList$sp
  
  ## get the trial intervals
  tfile<-paste(paste(rs@path,rs@session,sep="/"),".light_trials_intervals",sep="")
  if(!file.exists(tfile))
    stop(paste("file missing:",tfile))
  int<-read.table(file=tfile,header=F)
  colnames(int)<-c("no","condition","start","end")
  if(length(int$no)!=60)
    stop("length of int$no is not 60")
  
  ## only consider l1 and d1
  c<-sort(as.character(unique(int$condition))) # d1 d2 l1 l2 or d1 l1 ## alphabetically
  if(length(c)==2){
    intd<-int[which(int$condition==c[1]),]
    intl<-int[which(int$condition==c[2]),]
  }
  if(length(c)==4){
    intd<-int[which(int$condition==c[1]),]
    intl<-int[which(int$condition==c[3]),]
  }
  if(length(c)!=2&length(c)!=4)
    stop("problem with the number of conditions")
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
  
  st<-ifr(st) ## get the ifr for the entire recording session
    
  stl<-setIntervals(st=st,s=intl$start,e=intl$end) ## set limiting intervals
  std<-setIntervals(st=st,s=intd$start,e=intd$end) ## set limiting intervals

  ## speed rate regression in light and dark
  spl<-speedScore(sp,stl,pt,minSpeed=3,maxSpeed=100,runLm=T)
  spd<-speedScore(sp,std,pt,minSpeed=3,maxSpeed=100,runLm=T)
  
  ## create a data.frame with data for each cell
  speedRateT<-data.frame(clu.id=cg@id,
                         r.l=spl@speedScore,
                         i.l=spl@speedRateIntercept,
                         s.l=spl@speedRateSlope,
                         r.d=spd@speedScore,
                         i.d=spd@speedRateIntercept,
                         s.d=spd@speedRateSlope)
  
  srl<-speedRateTuningCurve(sp,stl,pt,minSpeed=3,maxSpeed=100)
  srl$condition="l"
  srd<-speedRateTuningCurve(sp,std,pt,minSpeed=3,maxSpeed=100)
  srd$condition="d"
  df<-rbind(srl,srd)
  df$clu.id<-rep(cg@id,each=length(unique(df$mid)))
  return(list(speedRateT=speedRateT,srTuningCurve=df))
}