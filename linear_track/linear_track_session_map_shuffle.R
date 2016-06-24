### makes maps after shuffling trial identity, n = 500
### find the 99 percentile of difference at each location and direction
linear.track.maps.condition.shuffle<-function(rs){
  print(paste(rs@session,rs@path))
  
  ## load the data in R
  myList<-getRecSessionObjects(rs)
  st<-myList$st
  pt<-myList$pt
  cg<-myList$cg
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
  
  ## get the size of data.frame for one iteration
  sp1<-firingRateHisto(sp1,st,ptlt)
  df<-rateHistoAsDataFrame(sp1)
  ldf<-length(df$clu.id)
  ## prepare the data.frame holding the maps of shuffling
  nshuf=500 # n of shufflings 
  all.cond<-sort(unique(as.character(ti$c)))
  ncond=length(all.cond)
  ndir=length(c(0,1))
  histo1d<-data.frame(clu.id=rep(df$clu.id,nshuf*ncond*ndir),
                      condition=rep(all.cond,each=ndir*ldf),
                      direction=rep(c(0,1),each=ldf),
                      shuf=rep(1:nshuf,each=ncond*ndir*ldf),
                      x=rep(df$x,nshuf*ncond*ndir),
                      rate=NA)
 
  for (shuf in 1:nshuf){
    ti$c<-sample(ti$c) ## shuffle the trial id
    for (cond in 1:ncond){
      for(dir in 0:1){
       # print(paste(shuf,condition,dir,ti$c[condition]))
        #### get intervals for direction
        dirInt<-getIntervalsAtDirection(ptlt,direction=dir)
        ### join direction and condition intervals
        jInt<-joinIntervalsAND(s1=ti$s[which(ti$c==all.cond[cond])],
                               e1=ti$e[which(ti$c==all.cond[cond])],
                               s2=dirInt[,1],
                               e2=dirInt[,2])
        ## set intervals in spike train object
        st<-setIntervals(st,s=jInt[,1],e=jInt[,2])
        ####
        
        ## 1d maps
        sp1<-firingRateHisto(sp1,st,ptlt)
        df<-rateHistoAsDataFrame(sp1)
        
        start.index<-((shuf-1)*ncond*ndir*ldf)+((cond-1)*ndir*ldf)+((dir)*ldf)+1
        end.index<-start.index+ldf-1
        
        histo1d$rate[start.index:end.index]<-df$rate
      }
    }
  }

  ## get the quantile of shuffle distribution for each combination of cell, position and direction.
  prob.diff<-function(x,level1,level2,prob=0.99){
    ## return the quantile of rate difference between 2 conditions
    if(length(x$rate[which(x$condition==level1)])!=length(x$rate[which(x$condition==level2)])){
      stop(paste("problem with length of vectors",
                 length(x$rate[which(x$condition==level1)]),
                 length(x$rate[which(x$condition==level2)]),
                 "for",x$clu.id[1],x$x[1], x$direction[1]))
    }
    return(quantile(abs(x$rate[which(x$condition==level1)]-x$rate[which(x$condition==level2)]),prob))
  }
  min.prob=0.99 ## cutoff for significance
  b<-by(histo1d,list(histo1d$direction, histo1d$x,histo1d$clu.id), prob.diff,"l1","l2",min.prob)
  c<-by(histo1d,list(histo1d$direction, histo1d$x,histo1d$clu.id), prob.diff,"l1","d",min.prob)
  d<-by(histo1d,list(histo1d$direction, histo1d$x,histo1d$clu.id), prob.diff,"l2","d",min.prob)
  di<-data.frame(cell.id=rep(as.character(dimnames(b)[[3]]),each=length(dimnames(b)[[1]])*length(dimnames(b)[[2]])),
                 direction=rep(as.numeric(dimnames(b)[[1]]),length(dimnames(b)[[2]])*length(dimnames(b)[[3]])),
                 x=rep(rep(as.numeric(dimnames(b)[[2]]),each=length(dimnames(b)[[1]])),length(dimnames(b)[[3]])),
                 l1.l2.shuf=as.numeric(b),
                 l1.d.shuf=as.numeric(c),
                 l2.d.shuf=as.numeric(d))
  return(list(lt.diff.sign=di))
}