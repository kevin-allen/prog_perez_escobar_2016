####################################################
## check the isolation quality of spike clusters ###
####################################################
runOnSessionList(ep,sessionList=rss,clusterIsolationCheck,save=T,overwrite = T)
load(paste(ep@resultsDirectory,"cluster.check",sep="/"))
if(any(cluster.check$refractoryRatio > 0.125 | cluster.check$isolationDistance < 5,na.rm=T))
{
  print("There are still poorly isolated neurons in the database")
} else{
  print("Database has only well-isolated neurons")
}
rm(cluster.check)



##########################################################
## check if the recording sessions contain 2 baselines  ##
## and at least 60 trials                               ##
## a maximum of 2 lights                                ##
##########################################################
source("~/repo/pintegration/analysis/R_scripts/light_interval_check.R")
runOnSessionList(ep,sessionList=rss,lightIntervalCheck,save=T,overwrite = T)
load(paste(ep@resultsDirectory,"session.intervals",sep="/"))
if(any(session.intervals$tn!=60)){
  print("Recording sessions with a number of trials different than 60")
  print(as.character(session.intervals$session[which(session.intervals$tn!=60)]))
} else{
  print("All recording sessions have 60 trials")
}
if(any(session.intervals$bn!=2)){
  print("Recording sessions with a number of baseline different than 2")
} else {
  print("All recording sessions have 2 baseline periods")
}
print(paste("Number of sessions with 4 conditions:",length(which(session.intervals$cn==4))))
print(paste("Number of sessions with 2 conditions:",length(which(session.intervals$cn==2))))
rm(lightIntervalCheck,session.intervals)



###################################
## some spikes in both baselines ##
###################################
numSpikesBaselines<-function(rs){
  if(class(rs)!="RecSession")
    stop("clusterIsolationCheck: rs is not a RecSession")
  st<-new("SpikeTrain",session=rs@session,path=rs@path)
  st<-loadSpikeTrain(st)
  cg<-new("CellGroup",session=rs@session,path=rs@path,nTetrodes=rs@nElectrodes)
  cg<-loadCellGroup(cg)
  ## get the baseline intervals
  bfile<-paste(paste(rs@path,rs@session,sep="/"),".light_baselines_intervals",sep="")
  if(!file.exists(bfile))
    stop(paste("file missing:",bfile))
  int<-read.table(file=bfile,header=F)
  colnames(int)<-c("no","condition","s","e")
  if(length(int$no)!=2)
    stop("length of int$V1 is not 2")
  
  st1<-setIntervals(st=st,s=int$s[1],e=int$e[1])
  st1<-meanFiringRate(st1)
  st2<-setIntervals(st=st,s=int$s[2],e=int$e[2])
  st2<-meanFiringRate(st2)
  df<-data.frame(clu.id=cg@id,
                 tetrode=cg@tetrode,
                 cluToTetrode=cg@cluToTetrode,
                 rate1=st1@meanFiringRate,
                 rate2=st2@meanFiringRate)
  return(list(nsb=df))
}
runOnSessionList(ep,sessionList=rss,numSpikesBaselines,save=T)
rm(numSpikesBaselines)
load(paste(ep@resultsDirectory,"nsb",sep="/"))
if(length(nsb[which(nsb$rate1==0|nsb$rate2==0),1])>0){
  print("These clusters have no spikes in one of the 2 baselines")
  nsb[which(nsb$rate1==0|nsb$rate2==0),]
} else {
  print("All clusters have spikes in the two baselines")
}
  