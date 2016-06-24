spatialPropertiesBaseline<-function(rs){
  print(rs@session)
  
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  cg<-myList$cg
  sp<-myList$sp
  hd<-myList$hd
  
  ## get the baseline intervals
  bfile<-paste(paste(rs@path,rs@session,sep="/"),".light_baselines_intervals",sep="")
  if(!file.exists(bfile))
    stop(paste("file missing:",bfile))
  int<-read.table(file=bfile,header=F)
  if(length(int$V1)!=2)
    stop("length of int$V1 is not 2")
  
  ## filter for speed of the animal
  pt<-speedFilter(pt,minSpeed=3,maxSpeed = 100)
 
  for(i in 1:2){
  
    ## get the light condition
    cond<-int$V2[i]
    
    ## limit analysis to one baseline
    st<-setIntervals(st=st,s=int$V3[i],e=int$V4[i])
    ptb<-setInvalidOutsideInterval(pt,s=int$V3,e=int$V4)
    
    
    ## need the head direction selectivity for open field, histo, vector length and shuffled ##
    hd<-headDirectionStats(hd,st,ptb)
    hdHisto<-hdHistoAsDataFrame(hd)
    hdHisto$baseline=i
    hdHisto$session=st@session
    hd<-headDirectionStatsShuffle(hd,st,ptb)
    
    ## get the 2d map properties
    sp<-getMapStats(sp,st,ptb,border="circular") ## get info score, sparsity, border, grid score
    
    #firingRateMapsPlot(sp@maps,sp@cellList)
    maps<-mapsAsDataFrame(sp) ## get the maps out before shuffling
    maps$baseline=i
    maps$session=st@session
    sp<-getMapStatsShuffle(sp,st,ptb,border="circular") ## get the shuffling values
  
    ## get speed scores
    st<-ifr(st)
    sp<-speedScore(sp,st,ptb,minSpeed=3,maxSpeed=100,runLm=F)
    sp<-speedScoreShuffle(sp,st,ptb,minSpeed=3,maxSpeed=100)
    
    ## mean rate
    st<-meanFiringRate(st)
    ### save results in data.frames
    if(i==1){
      bstats<-data.frame(session=rs@session,
                         baseline=i,
                         condition=cond,
                         clu.id=cg@id,
                         mean.rate=st@meanFiringRate,
                         info.score=sp@infoScore, 
                         border.score=sp@borderScore, cm=sp@borderCM, dm=sp@borderDM,polarity=sp@mapPolarity,
                         grid.score=sp@gridScore,grid.spacing=sp@gridSpacing,
                         speed.score=sp@speedScore,
                         hd.vl=hd@vectorLength,hd.peak.rate=hd@peakRates)
      bstats.shuf<-data.frame(session=rs@session,
                              baseline=i,
                              condition=cond,
                              clu.id=cg@id,
                       info.score=sp@infoScoreShuffle,
                       border.score=sp@borderScoreShuffle,cm=sp@borderCMShuffle, dm=sp@borderDMShuffle, polarity=sp@mapPolarityShuffle,
                       grid.score=sp@gridScoreShuffle, 
                       speed.score=sp@speedScoreShuffle,
                       hd.vl=hd@vectorLengthShuffle)
      bmaps<-maps
      bHdHisto<-hdHisto
      
    } else{
      bstats<-rbind(bstats,data.frame(session=rs@session,
                                      baseline=i,
                                      condition=cond,
                                     clu.id=cg@id,
                                     mean.rate=st@meanFiringRate,
                                     info.score=sp@infoScore, 
                                     border.score=sp@borderScore, cm=sp@borderCM, dm=sp@borderDM,polarity=sp@mapPolarity,
                                     grid.score=sp@gridScore, grid.spacing=sp@gridSpacing,
                                     speed.score=sp@speedScore,
                                     hd.vl=hd@vectorLength,hd.peak.rate=hd@peakRates))
      bstats.shuf<-rbind(bstats.shuf,data.frame(session=rs@session,
                                                baseline=i,
                                                condition=cond,
                                                clu.id=cg@id,
                              info.score=sp@infoScoreShuffle,
                              border.score=sp@borderScoreShuffle,cm=sp@borderCMShuffle, dm=sp@borderDMShuffle,polarity=sp@mapPolarityShuffle,
                              grid.score=sp@gridScoreShuffle, 
                              speed.score=sp@speedScoreShuffle,
                              hd.vl=hd@vectorLengthShuffle))
      bmaps<-rbind(bmaps,maps)
      bHdHisto<-rbind(bHdHisto,hdHisto)
    }
  }
  bHdHisto
  
  return(list(bmaps=bmaps,bhdhisto=bHdHisto,bstats=bstats, bstats.shuf=bstats.shuf))
}