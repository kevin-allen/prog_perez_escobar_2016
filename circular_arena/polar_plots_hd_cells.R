plot.page.full.same.graph<-function(fn="page.full.plot"){
  num.cols<-6
  num.rows<-8
  plot.per.page=num.cols*num.rows
  mm<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
              rep(seq(1/num.cols,1,1/num.cols),num.rows),
              rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
              rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  pdf(file=fn,onefile=TRUE,paper="a4",width=8,height=10)
  index=1
  for (cellid in cell.list) ## any cell list
  {
    print(cellid)
    if(index==1)
    {
      split.screen(mm)  
    }

    screen(index)
    ## insert your plot function here
    m<-bmaps[which(bmaps$clu.id==cellid&bmaps$baseline==1),]
    n<-bstats[which(bstats$clu.id==cellid&bstats$baseline==1),]
    firingRateMapPlot(m=m,name=cellid,
                      main.title=paste("gs:",round(n$grid.score,2),
                                        "cm:",round(n$cm,2),
                                        "dm:",round(n$dm,2),
                                        "is:",round(n$info.score,2)))
    screen(index+1)
    ## insert your plot function here
    m<-tmaps[which(tmaps$clu.id==cellid&tmaps$condition=="l1"),]
    n<-tstats[which(tstats$clu.id==cellid&tstats$condition=="l1"),]
    firingRateMapPlot(m=m,name="",
                      main.title=paste("gs:",round(n$grid.score,2),
                                       "cm:",round(n$cm,2),
                                       "dm:",round(n$dm,2),
                                       "is:",round(n$info.score,2)))
    screen(index+2)
    ## insert your plot function here
    m<-tmaps[which(tmaps$clu.id==cellid&tmaps$condition=="d1"),]
    n<-tstats[which(tstats$clu.id==cellid&tstats$condition=="d1"),]
    firingRateMapPlot(m=m,name="",
                      main.title=paste("gs:",round(n$grid.score,2),
                                       "cm:",round(n$cm,2),
                                       "dm:",round(n$dm,2),
                                       "is:",round(n$info.score,2)))
    
    ## for sessions with several conditions ##
    if(sessions$num.lights[which(sessions$session==unique(bstats$session[which(bstats$clu.id==cellid)]))]==2){
      screen(index+3)
      ## insert your plot function here
      m<-tmaps[which(tmaps$clu.id==cellid&tmaps$condition=="l2"),]
      n<-tstats[which(tstats$clu.id==cellid&tstats$condition=="l2"),]
      firingRateMapPlot(m=m,name="",
                        main.title=paste("gs:",round(n$grid.score,2),
                                         "cm:",round(n$cm,2),
                                         "dm:",round(n$dm,2),
                                         "is:",round(n$info.score,2)))
      screen(index+4)
      ## insert your plot function here
      m<-tmaps[which(tmaps$clu.id==cellid&tmaps$condition=="d2"),]
      n<-tstats[which(tstats$clu.id==cellid&tstats$condition=="d2"),]
      firingRateMapPlot(m=m,name="",
                        main.title=paste("gs:",round(n$grid.score,2),
                                         "cm:",round(n$cm,2),
                                         "dm:",round(n$dm,2),
                                         "is:",round(n$info.score,2)))
    }
    
    screen(index+5)
    ## insert your plot function here
    m<-bmaps[which(bmaps$clu.id==cellid&bmaps$baseline==2),]
    n<-bstats[which(bstats$clu.id==cellid&bstats$baseline==2),]
    firingRateMapPlot(m=m,name="",
                      main.title=paste("gs:",round(n$grid.score,2),
                                       "cm:",round(n$cm,2),
                                       "dm:",round(n$dm,2),
                                       "is:",round(n$info.score,2)))
    
    
    screen(index+6)
    h<-bhdhisto[which(bhdhisto$clu.id==cellid&bhdhisto$baseline==1),]
    headDirectionPolarPlot(h)
    
    screen(index+7)
    h<-thdhisto[which(thdhisto$clu.id==cellid&thdhisto$condition=="l1"),]
    headDirectionPolarPlot(h)
    
    screen(index+8)
    h<-thdhisto[which(thdhisto$clu.id==cellid&thdhisto$condition=="d1"),]
    headDirectionPolarPlot(h)
    
    ## for sessions with several conditions ##
    if(sessions$num.lights[which(sessions$session==unique(bstats$session[which(bstats$clu.id==cellid)]))]==2){
      screen(index+9)
      h<-thdhisto[which(thdhisto$clu.id==cellid&thdhisto$condition=="l2"),]
      headDirectionPolarPlot(h)
      
      screen(index+10)
      h<-thdhisto[which(thdhisto$clu.id==cellid&thdhisto$condition=="d2"),]
      headDirectionPolarPlot(h)
      
    }
    
    screen(index+11)
    h<-bhdhisto[which(bhdhisto$clu.id==cellid&bhdhisto$baseline==2),]
    headDirectionPolarPlot(h)
    
    
    
    index=index+11
    if(index==plot.per.page)
    {
      close.screen( all = TRUE )
      index=0
    }
    index=index+1
  }
  close.screen(all = TRUE)
  dev.off()
}


load(paste(ep@directory,"results","bstats",sep="/"))
load(paste(ep@directory,"results","tstats",sep="/"))
load(paste(ep@directory,"results","bmaps",sep="/"))
load(paste(ep@directory,"results","tmaps",sep="/"))
load(paste(ep@directory,"results","thdhisto",sep="/"))
load(paste(ep@directory,"results","bhdhisto",sep="/"))
load(paste(ep@directory,"results","sessions",sep="/"))

## rename conditions of trials
tstats<-unsplit(lapply(split(tstats,tstats$session),rename.condition.data.frame), tstats$session)
tmaps<-unsplit(lapply(split(tmaps,tmaps$session),rename.condition.data.frame), tmaps$session)
thdhisto<-unsplit(lapply(split(thdhisto,thdhisto$session),rename.condition.data.frame), thdhisto$session)



## plot hd cells
cell.list<-cells$cell.id[which(cells$hd==TRUE)]
fi<-paste(ep@directory,"figures_relectro","hd_polar_hd_cells.pdf",sep="/")
print(paste("writting",fi))
plot.page.full.same.graph(fn=fi)


rm(bstats,tstats,bmaps,tmaps,sessions)
rm(bhdhisto,thdhisto)
rm(plot.page.full.same.graph)