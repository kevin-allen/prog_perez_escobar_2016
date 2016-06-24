spatialPropertiesTrials<-function(rs){
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
    sp<-getMapStats(sp,st,ptt,border="circular") ## get info score, sparsity, border, grid score
    maps<-mapsAsDataFrame(sp) ## get the maps out before shuffling
    maps$condition=cond
    maps$session=rs@session
    sp<-getMapStatsShuffle(sp,st,ptt,border="circular") ## get the shuffling values
    
    ## need the head direction selectivity for open field, histo, vector length and shuffled ##
    hd<-headDirectionStats(hd,st,ptt)
    hd<-headDirectionStatsShuffle(hd,st,ptt)
    
    ## get speed scores
    st<-ifr(st)
    sp<-speedScore(sp,st,ptt,minSpeed=3,maxSpeed=100,runLm=F)
    sp<-speedScoreShuffle(sp,st,ptt,minSpeed=3,maxSpeed=100)
    
    ## mean rate
    st<-meanFiringRate(st)
    ### save results in data.frames
    if(index==1){
      tstats<-data.frame(session=rs@session,
                         condition=cond,
                         clu.id=cg@id,
                         mean.rate=st@meanFiringRate,
                         info.score=sp@infoScore, 
                         border.score=sp@borderScore, cm=sp@borderCM, dm=sp@borderDM,polarity=sp@mapPolarity,
                         grid.score=sp@gridScore,
                         grid.spacing=sp@gridSpacing,
                         speed.score=sp@speedScore,
                         hd.vl=hd@vectorLength,hd.peak.rate=hd@peakRates)
      tstats.shuf<-data.frame(session=rs@session,
                              condition=cond,
                              clu.id=cg@id,
                              info.score=sp@infoScoreShuffle,
                              border.score=sp@borderScoreShuffle,cm=sp@borderCMShuffle, dm=sp@borderDMShuffle, polarity=sp@mapPolarityShuffle,
                              grid.score=sp@gridScoreShuffle, 
                              speed.score=sp@speedScoreShuffle,
                              hd.vl=hd@vectorLengthShuffle)
      tmaps<-maps
    } else{
      tstats<-rbind(tstats,data.frame(session=rs@session,
                                      condition=cond,
                                     clu.id=cg@id,
                                     mean.rate=st@meanFiringRate,
                                     info.score=sp@infoScore, 
                                     border.score=sp@borderScore, cm=sp@borderCM, dm=sp@borderDM,polarity=sp@mapPolarity,
                                     grid.score=sp@gridScore,
                                     grid.spacing=sp@gridSpacing,
                                     speed.score=sp@speedScore,
                                     hd.vl=hd@vectorLength,hd.peak.rate=hd@peakRates))
      tstats.shuf<-rbind(tstats.shuf,data.frame(session=rs@session,
                              condition=cond,
                              clu.id=cg@id,
                              info.score=sp@infoScoreShuffle,
                              border.score=sp@borderScoreShuffle,cm=sp@borderCMShuffle, dm=sp@borderDMShuffle, polarity=sp@mapPolarityShuffle,
                              grid.score=sp@gridScoreShuffle, 
                              speed.score=sp@speedScoreShuffle,
                              hd.vl=hd@vectorLengthShuffle))
      tmaps<-rbind(tmaps,maps)
    }
    index=index+1
  }
  return(list(tmaps=tmaps,tstats=tstats,tstats.shuf=tstats.shuf))
}