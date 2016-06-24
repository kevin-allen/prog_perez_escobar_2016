cellTable<-function(rs){
  print(paste(rs@session,rs@path))
  cg<-new("CellGroup",session=rs@session,path=rs@path,nTetrodes=rs@nElectrodes)
  cg<-loadCellGroup(cg)
  st<-new("SpikeTrain",session=rs@session,path=rs@path)
  st<-loadSpikeTrain(st)
  st<-meanFiringRate(st)
  eFile<-paste(paste(rs@path,rs@session,sep="/"),"electrode_location",sep=".")
  if(!file.exists(eFile))
    stop(paste("spatialPropertiesBaseline:",eFile,"is missing"))
  eLoc<-read.table(eFile)  
  colnames(eLoc)<-c("hemisphere","region")
  if(length(eLoc$hemisphere)!=rs@nElectrodes)
    stop("spatialPropertiesBaseline: eLoc$hemisphere length is not equal to rs@nElectrodes")
  ## create the cells dataframe
  cells<-data.frame(mouse=rs@animalName,session=rs@session,cell.id=cg@id,tetrode.id=cg@tetrodeId,
                    clu.to.tet=cg@cluToTetrode,
                    mean.rate=st@meanFiringRate,
                    hemisphere=eLoc$hemisphere[cg@tetrode],
                    region=eLoc$region[cg@tetrode])
  return(list(cells=cells))
}