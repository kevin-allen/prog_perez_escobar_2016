meanRateChangeTrials<-function(rs){
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
  
  ### with real conditions
  intd<-int[which(grepl("d",as.character(int$condition))),]
  intl<-int[which(grepl("l",as.character(int$condition))),]
  stl<-setIntervals(st=st,s=intl$start,e=intl$end) ## set limiting intervals
  std<-setIntervals(st=st,s=intd$start,e=intd$end) ## set limiting intervals
  stl<-meanFiringRate(stl)
  std<-meanFiringRate(std)
  rateChange<-data.frame(clu.id=cg@id,
             ratel=stl@meanFiringRate,
             rated=std@meanFiringRate,
             index=(stl@meanFiringRate-std@meanFiringRate)/(stl@meanFiringRate+std@meanFiringRate))
  
  ### shuffling
  nShuf=500
  rateChangeShuf<-data.frame(clu.id=rep(cg@id,nShuf),
                      shuf=rep(1:nShuf,each=length(cg@id)),
                      ratel=NA,
                      rated=NA,
                      index=NA)
  for(i in 1:nShuf){
    int$condition<-sample(int$condition) # shuffle the trial id
    intd<-int[which(grepl("d",as.character(int$condition))),]
    intl<-int[which(grepl("l",as.character(int$condition))),]
    stl<-setIntervals(st=st,s=intl$start,e=intl$end) ## set limiting intervals
    std<-setIntervals(st=st,s=intd$start,e=intd$end) ## set limiting intervals
    stl<-meanFiringRate(stl)
    std<-meanFiringRate(std)
    rateChangeShuf$ratel[which(rateChangeShuf$shuf==i)]<-stl@meanFiringRate
    rateChangeShuf$rated[which(rateChangeShuf$shuf==i)]<-std@meanFiringRate
    rateChangeShuf$index[which(rateChangeShuf$shuf==i)]<-(stl@meanFiringRate-std@meanFiringRate)/(stl@meanFiringRate+std@meanFiringRate)
  }
  
  return(list(rateChange=rateChange,rateChangeShuf=rateChangeShuf))
}