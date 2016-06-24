### function to get the 2d and 1d maps on the linear track
### done separately for directions and lighting conditions
linear.track.maps.stats<-function(rs){
  print(paste(rs@session,rs@path))

  ## load the data in R
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  sp<-myList$sp
  sp1<-myList$sp1
  
  ## get the trial intervals for the light
  ti.file<-paste(rs@fileBase,"light_intervals",sep=".")
  if(!file.exists(ti.file))
    stop(paste("needs",ti.file,"in linear.track.analysis"))
  ti<-read.table(ti.file,header=F)
  colnames(ti)<-c("c","s","e") ## condition start end
  
  ## keep only the data from the linear track and linearize
  ptlt<-setInvalidOutsideInterval(pt,s=getIntervalsEnvironment(rs,env="lt"))
  ptlt<-linearzeLinearTrack(ptlt)
  
  ## data frames to store the data of all conditions
  maps2d<-data.frame()
  stats2d<-data.frame()  
  histo1d<-data.frame()
  stats1d<-data.frame()
  
  ## get the 2d and 1d stats and maps for the 6 possible conditions
  for (condition in unique(ti$c)){
    for(dir in 0:1){
  
     #### get intervals for direction
     dirInt<-getIntervalsAtDirection(ptlt,direction=dir)
     ### join direction and condition intervals
     jInt<-joinIntervalsAND(s1=ti$s[which(ti$c==condition)],
                      e1=ti$e[which(ti$c==condition)],
                      s2=dirInt[,1],
                      e2=dirInt[,2])
     ## set intervals in spike train object
     st<-setIntervals(st,s=jInt[,1],e=jInt[,2])
     ## 2d maps
     sp<-firingRateMap2d(sp,st,ptlt)
     df<-mapsAsDataFrame(sp)
     maps2d<-rbind(maps2d,data.frame(clu.id=df$clu.id,
                                     condition=condition,
                                     direction=dir,
                                     x=df$x,
                                     y=df$y,
                                     rate=df$rate))
      ## 2d stats
      sp<-getMapStats(sp,st,ptlt) 
      df<-statsAsDataFrame(sp,shuffle=FALSE)
      stats2d<-rbind(stats2d,data.frame(clu.id=df$clu.id,
                               condition=condition,
                               direction=dir,
                               peakRate=df$peakRate,
                               infoScore=df$infoScore,
                               sparsity=df$sparsity))
     
     ## 1d maps
     sp1<-firingRateHisto(sp1,st,ptlt)
     df<-rateHistoAsDataFrame(sp1)
     histo1d<-rbind(histo1d,data.frame(clu.id=df$clu.id,
                                     condition=condition,
                                     direction=dir,
                                     x=df$x,
                                     rate=df$rate))
     ## get stats 1d
     sp1<-getHistoStats(sp1,st,ptlt)
     df<-histoStatsAsDataFrame(sp1,shuffle=FALSE)
     stats1d<-rbind(stats1d,data.frame(clu.id=df$clu.id,
                              condition=condition,
                              direction=dir,
                              peakRate=df$peakRate,
                              infoScore=df$infoScore,
                              sparsity=df$sparsity))
    }
  }
  
  
  ## get the difference between rate at each combination of cell, position and direction.
  obs.diff<-function(x,level1,level2){
    if(any(x$rate==-1.0))
      return(NA)
    return(abs(x$rate[which(x$condition==level1)]-x$rate[which(x$condition==level2)]))
  }
  
  b<-by(histo1d,list(histo1d$direction, histo1d$x,histo1d$clu.id), obs.diff,"l1","l2")
  c<-by(histo1d,list(histo1d$direction, histo1d$x,histo1d$clu.id), obs.diff,"l1","d")
  d<-by(histo1d,list(histo1d$direction, histo1d$x,histo1d$clu.id), obs.diff,"l2","d")
  di<-data.frame(cell.id=rep(as.character(dimnames(b)[[3]]),each=length(dimnames(b)[[1]])*length(dimnames(b)[[2]])),
                 direction=rep(as.numeric(dimnames(b)[[1]]),length(dimnames(b)[[2]])*length(dimnames(b)[[3]])),
                 x=rep(rep(as.numeric(dimnames(b)[[2]]),each=length(dimnames(b)[[1]])),length(dimnames(b)[[3]])),
                 l1.l2.obs=as.numeric(b),
                 l1.d.obs=as.numeric(c),
                 l2.d.obs=as.numeric(d))
  ## get the x values in cm
  histo1d$x<-histo1d$x*sp1@cmPerBin
  
  
  ## return a list of 4 data frames
  return(list(lt.maps.2d=maps2d,
              lt.maps.1d=histo1d, 
              lt.stats.2d=stats2d,
              lt.stats.1d=stats1d,
              lt.obs.diff=di))
}


