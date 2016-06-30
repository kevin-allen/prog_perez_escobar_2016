### function doing all the work on individual sessions
sqr70.cell.properties<-function(rs){
  print(paste(rs@session,rs@path))
 
  ## load the data in R
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  cg<-myList$cg
  sp<-myList$sp
  hd<-myList$hd
  
  eFile<-paste(paste(rs@path,rs@session,sep="/"),"electrode_location",sep=".")
  if(!file.exists(eFile))
    stop(paste("spatialPropertiesBaseline:",eFile,"is missing"))
  eLoc<-read.table(eFile)  
  colnames(eLoc)<-c("hemisphere","region")
  if(length(eLoc$hemisphere)!=rs@nElectrodes)
    stop("spatialPropertiesBaseline: eLoc$hemisphere length is not equal to rs@nElectrodes")
  
  ## get the position data for sqr70 
  ptsqr70<-setInvalidOutsideInterval(pt,s=getIntervalsEnvironment(rs,env="sqr70"))
  ptsqr70<-speedFilter(ptsqr70,minSpeed=3,maxSpeed = 100)
  
  ## get the spatial properties during the sqr70 in order to identify the neurons
  sp<-getMapStats(sp,st,ptsqr70) ## get info score, sparsity, border, grid score
  sqr70.maps<-mapsAsDataFrame(sp) ## get the maps out before shuffling
  sp<-getMapStatsShuffle(sp,st,ptsqr70) ## get the shuffling values
  
  
  ## need the head direction selectivity for open field, histo, vector length and shuffled ##
  hd<-headDirectionStats(hd,st,ptsqr70)
  hd<-headDirectionStatsShuffle(hd,st,ptsqr70)
  
  ## get speed scores
  st<-ifr(st)
  sp<-speedScore(sp,st,ptsqr70,minSpeed=3,maxSpeed=100,runLm=F)
  sp<-speedScoreShuffle(sp,st,ptsqr70,minSpeed=3,maxSpeed=100)
  
  ## get mean firing rate of the neurons
  st<-setIntervals(st,s=getIntervalsEnvironment(rs,environment = "sqr70"))
  st<-meanFiringRate(st)

  ## create a data frame containing the data for each cell ##
  cells<-data.frame(mouse=rs@animalName,session=rs@session,cell.id=cg@id,tetrode.id=cg@tetrodeId,
                    hemisphere=eLoc$hemisphere[cg@tetrode], region=eLoc$region[cg@tetrode],
             clu.to.tet=cg@cluToTetrode,
             mean.rate=st@meanFiringRate, 
             info.score=sp@infoScore, border.score=sp@borderScore, grid.score=sp@gridScore,speed.score=sp@speedScore,
             hd.vl=hd@vectorLength,hd.peak.rate=hd@peakRates)
  ## create a data frame with the shuffled data
  shuf<-data.frame(mouse=rs@animalName,session=rs@session,cell.id=cg@id,
               info.score=sp@infoScoreShuffle,border.score=sp@borderScoreShuffle,grid.score=sp@gridScoreShuffle, speed.score=sp@speedScoreShuffle,
               hd.vl=hd@vectorLengthShuffle)

  return(list(sqr70.maps=sqr70.maps,cells=cells,sqr70.shuf=shuf))
}
