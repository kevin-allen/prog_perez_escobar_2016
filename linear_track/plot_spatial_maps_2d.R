#############################################################
### plot the firing rate maps of place cells in a pdf file ###
#############################################################
firing.rate.map.plot <- function(df,outma=c(2.0,.0,2.0,.0),margin=c(0,.0,1.0,.0),axis.x.mgp=c(0,0,0),axis.y.mgp=c(0,0,0),
                                 cex.x.axis=0.6,cex.y.axis=0.6,cex.lab=0.6,xlab="",ylab="",show.xlab=TRUE,main.title="",peak.rate.prefix="")
{
  # x is a row in R matrix  
  jet.colors = colorRampPalette(c("#00007F", "blue","#007FFF",  "cyan", "#7FFF7F", "yellow", "#FF7F00","red"))
  xlen <- length(unique(df$x))
  ylen <- length(unique(df$y))
  #print(paste(unique(df$clu.id),xlen,ylen))
  zzz <- matrix(df$rate,nrow=xlen,ncol=ylen)
  par(oma=outma,mar=margin)
  image(unique(df$x),unique(df$y),zzz,zlim=c(0,max(df$rate,na.rm=T)), col=jet.colors(200),xlab='',ylab='',axes=FALSE)
  mtext(paste(peak.rate.prefix,round(max(df$rate,na.rm=T),digits=2),"Hz"),side=3,at=median(unique(df$x)),line=-0.1,cex=0.5)
  if(main.title!="")
  {
    mtext(main.title,side=3,at=median(unique(df$x)),line=0.3,cex=0.4)
  }
}


plot.page.full.maps<-function(fn="place.cell.plot.pdf",cell.list=cell.list,maps=s){
  plot.per.page=6*8
  num.cols<-6
  num.rows<-8
  m<-matrix(c(rep(seq(0,1-(1/num.cols),1/num.cols),num.rows),
              rep(seq(1/num.cols,1,1/num.cols),num.rows),
              rep(seq(1-(1/num.rows),0,0-1/num.rows),each=num.cols),
              rep(seq(1,1/num.rows,0-1/num.rows),each=num.cols)),ncol=4)
  
  pdf(file=fn,onefile=TRUE,paper="a4",height=8, width = 6)
  
  index=1
  for (cellid in cell.list) ## any cell list
  {
   # print(paste(cellid,index))
    if(index==1)
    {
      split.screen(m)  
    }
    x<-maps[which(maps$clu.id==cellid),]
    screen(index)
    firing.rate.map.plot(df=x,main.title=cellid)
    
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

## load all the firing rate maps
load(paste(ep@directory,"results","sqr70.maps",sep="/"))

## save in a figures directory
dir<-paste(ep@directory,"figures",sep="/")
if(!dir.exists(dir)){
  dir.create(dir)
}


## plot grid cells
print(paste("save figures in",dir))
print("plot grid cells map in grid_cells_map2d.pdf")
plot.page.full.maps(fn=paste(dir,"grid_cells_map2d.pdf",sep="/"),cell.list=cells$cell.id[which(cells$grid==T)], maps=sqr70.maps)
## plot border cells
print("plot border cells map in border_cells_map2d.pdf")
plot.page.full.maps(fn=paste(dir,"border_cells_map2d.pdf",sep="/"),cell.list=cells$cell.id[which(cells$border==T)], maps=sqr70.maps)
## plot place cells
print("plot place cells map in place_cells_map2d.pdf")
plot.page.full.maps(fn=paste(dir,"place_cells_map2d.pdf",sep="/"),cell.list=cells$cell.id[which(cells$place==T)], maps=sqr70.maps)
## plot speed cells
print("plot speed cells map in speed_cells_map2d.pdf")
plot.page.full.maps(fn=paste(dir,"speed_cells_map2d.pdf",sep="/"),cell.list=cells$cell.id[which(cells$speed==T)], maps=sqr70.maps)
## clean the environment
rm(plot.page.full.maps,firing.rate.map.plot,sqr70.maps,dir)

