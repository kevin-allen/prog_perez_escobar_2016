runningSpeedTrials<-function(rs){
  print(paste(rs@session,rs@path))
  pt<-new("Positrack",session=rs@session,path=rs@path)
  pt<-loadPositrack(pt)
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
  ptd<-setInvalidOutsideInterval(pt,s=intd$start,e=intd$end)
  ptl<-setInvalidOutsideInterval(pt,s=intl$start,e=intl$end)
  
  ## create a data.frame
  rSpeed<-data.frame(session=rs@session,
                     condition=c("l1","d1"),
                     mean.speed=c(mean(ptl@speed,na.rm=T),
                                  mean(ptd@speed,na.rm=T)))
  return(list(rSpeed=rSpeed))
}