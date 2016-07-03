crosscorrelation<-function(rs){
  #devtools::load_all("~/repo/relectro/")
  print(paste(rs@session,rs@path))
  st<-new("SpikeTrain",session=rs@session,path=rs@path)
  st<-loadSpikeTrain(st)
  
  # autocorrelation
  st<-spikeTimeAutocorrelation(st,binSizeMs=0.5,windowSizeMs=50,probability = T)
  dfa<-spikeTimeAutocorrelationAsDataFrame(st)
  dfa$clu.id<-paste(rs@session,dfa$clu,sep="_")
  
  
  # get all possible cell pairs (a-b and b-a)
  st@cellPairList<-makePairs(cl1=st@cellList,cl2=st@cellList)
  makePairs(cl1=st@cellList,cl2=st@cellList)
  
  # crosscorrelation count
  st<-spikeTimeCrosscorrelation(st,binSizeMs=0.5,windowSizeMs=50)
  df<-spikeTimeCrosscorrelationAsDataFrame(st)
  df$clu.id1<-paste(rs@session,df$clu1,sep="_")
  df$clu.id2<-paste(rs@session,df$clu2,sep="_")
  df$pair.id<-paste(df$clu.id1,df$clu.id2,sep=".")
  
  # crosscorrelation probability
  st<-spikeTimeCrosscorrelation(st,binSizeMs=0.5,windowSizeMs=50,probability=T)
  dfp<-spikeTimeCrosscorrelationAsDataFrame(st)
  dfp$clu.id1<-paste(rs@session,dfp$clu1,sep="_")
  dfp$clu.id2<-paste(rs@session,dfp$clu2,sep="_")
  dfp$pair.id<-paste(dfp$clu.id1,dfp$clu.id2,sep=".")
  
  return(list(stcc=df,stac=dfa,stccp=dfp))
}