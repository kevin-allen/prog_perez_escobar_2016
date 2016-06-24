hdPropertiesTrials<-function(rs){
  print(rs@session)
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  cg<-myList$cg
  sp<-myList$sp
  hd<-myList$hd
  
  ## get the baseline intervals
  tfile<-paste(paste(rs@path,rs@session,sep="/"),".light_trials_intervals",sep="")
  if(!file.exists(tfile))
    stop(paste("file missing:",tfile))
  int<-read.table(file=tfile,header=F)
  colnames(int)<-c("no","condition","start","end")
  if(length(int$no)!=60)
    stop("length of int$no is not 60")
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
  index=1
  for(cond in unique(int$condition)){
    ## limit analysis to this condition
    st<-setIntervals(st=st,s=int$start[which(int$condition==cond)],e=int$end[which(int$condition==cond)])
    ptt<-setInvalidOutsideInterval(pt,s=int$start[which(int$condition==cond)],e=int$end[which(int$condition==cond)])
    ## get the spatial properties during the sqr70 in order to identify the neurons
    

    ## need the head direction selectivity for open field, histo, vector length and shuffled ##
    hd<-headDirectionStats(hd,st,ptt)
    hdHisto<-hdHistoAsDataFrame(hd)
    hdHisto$condition=cond
    hdHisto$session=st@session

    ### save results in data.frames
    if(index==1){
      tHdHisto<-hdHisto
      
    } else{
      tHdHisto<-rbind(tHdHisto,hdHisto)
    }
    index=index+1
  }
  return(list(thdhisto=tHdHisto))
}