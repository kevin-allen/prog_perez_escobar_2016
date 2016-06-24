trialBlockMapCorrelation<-function(rs){
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
  # we need 4 conditions in total
  if(length(unique(int$condition))!=4){
    return(list(blockMapCor=NA))
  }
  allConds<-sort(as.character(unique(int$condition))) ## sort alphabetically, l1 is 3
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
  
  ## make template maps
  stl1<-setIntervals(st=st,s=int$start[which(int$condition==allConds[3])],e=int$end[which(int$condition==allConds[3])])
  spl1<-firingRateMap2d(sp,stl1,pt)
  blockDurationSec<-10
  numBlocks<-12
  df<-data.frame(clu.id=rep(cg@id,numBlocks*4),
                 session=rs@session,
                 condition=rep(allConds,each=numBlocks*st@nCells),
                 block=rep(1:numBlocks,each=st@nCells),
                 r=NA)
  
  for(cond in allConds){## loop for conditions
    cint<-int[which(int$condition==cond),]
    for(block in 1:numBlocks){ ## loop for blocks
      bStart<-blockDurationSec*st@samplingRate*(block-1)
      bEnd<-blockDurationSec*st@samplingRate*(block)
      stb<-setIntervals(st=st,s=cint$start+bStart,e=cint$start+bEnd)
      spb<-firingRateMap2d(sp,stb,pt)
      df$r[which(df$condition==cond&df$block==block)]<-firingRateMapCorrelation(spl1,spb)
    }
  }
  
  ## rename the conditions, had problem with split unsplit later on, maybe because some sessions return NA as list
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
  df<-rename.condition.data.frame(df)
  
  return(list(blockMapCor=df))
}