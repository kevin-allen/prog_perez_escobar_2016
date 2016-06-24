trialMapRotation<-function(rs){
  #devtools::load_all("~/repo/relectro/")
  print(paste(rs@session,rs@path))
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
  
  # we need 4 condition in total
  if(length(unique(int$condition))!=4){
    return(list(mapRotation=NA))
  }
  # find the two light conditions
  lcond<-sort(unique(int$condition)[grepl("l",as.character(unique(int$condition)))])
  if(length(lcond)!=2)
    stop("length(lcond) != 2")
  
  # sort the lcond so that 90 rotations are always clockwise
  if(lcond[1]=="l1"& lcond[2]=="l4")
    lcond<-rev(lcond)
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
  
  ## get spike trains with interval limited to the two light conditions
  st1<-setIntervals(st=st,s=int$start[which(int$condition==lcond[1])],e=int$end[which(int$condition==lcond[1])])
  st2<-setIntervals(st=st,s=int$start[which(int$condition==lcond[2])],e=int$end[which(int$condition==lcond[2])])
  
  ## get the firing rate maps for the 2 light conditions
  sp1<-firingRateMap2d(sp,st1,pt)
  sp2<-firingRateMap2d(sp,st2,pt)
  
  ## get the correlation between l1 and l2 after rotating l2
  m<-firingRateMapCorrelationRotation(sp1,sp2)
  df<-data.frame(m)
  colnames(df)<-colnames(m)
  df$clu.id<-cg@id
  
  return(list(mapRotation=df))
}