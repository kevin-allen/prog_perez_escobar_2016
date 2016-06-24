ifrAssociationsTrials<-function(rs){
  print(rs@session)
  
  st<-new("SpikeTrain",session=rs@session,path=rs@path)
  st<-loadSpikeTrain(st)
  
  
  ## get the trial intervals
  tfile<-paste(paste(rs@path,rs@session,sep="/"),".light_trials_intervals",sep="")
  if(!file.exists(tfile))
    stop(paste("file missing:",tfile))
  int<-read.table(file=tfile,header=F)
  colnames(int)<-c("no","condition","start","end")
  if(length(int$no)!=60)
    stop("length of int$no is not 60")
  
  
  ## calculate the ifr for the entire session
  st<-ifr(st)
  index=1
  for(cond in unique(int$condition)){
    ## limit analysis to this condition
    st<-setIntervals(st=st,s=int$start[which(int$condition==cond)],e=int$end[which(int$condition==cond)])
    ass<-ifrAssociation(st)
    ass$clu.id1<-paste(rs@session,ass$clu.id1,sep="_")
    ass$clu.id2<-paste(rs@session,ass$clu.id2,sep="_")
    ass$condition<-cond
    if(index==1)
      ifrAss<-ass
    else
      ifrAss<-rbind(ifrAss,ass)
    index=index+1
  }
 return(list(ifrAss=ifrAss))
}