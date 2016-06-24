load(paste(ep@directory,"results","sqr70.shuf",sep="/"))
## significance levels
prob=0.99

t.grid.score<-quantile(sqr70.shuf$grid.score,prob)
t.info.score<-quantile(sqr70.shuf$info.score,prob)
t.hd.vl<-quantile(sqr70.shuf$hd.vl,prob,na.rm=T)
t.border.score<-quantile(sqr70.shuf$border.score,prob,na.rm=T)
t.speed.score<-quantile(sqr70.shuf$speed.score,prob)

print(paste("threshold from shuffling with a probability of",prob))
print(paste("grid threshold:",round(t.grid.score,3)))
print(paste("info threshold:",round(t.info.score,3)))
print(paste("hd threshold:",round(t.hd.vl,3)))
print(paste("border threshold:",round(t.border.score,3)))
print(paste("speed threshold:",round(t.speed.score,3)))

### grid cells
print(paste("grid cells above",round(t.grid.score,4)))
cells$grid<-ifelse(cells$grid.score>t.grid.score,TRUE,FALSE)
print(paste("Number of grid cells:",sum(cells$grid)))

### hd cells
print(paste("head direction cells, hd.vl > ",round(t.hd.vl,4)," and hd.peak.rate >",5))
cells$hd<-ifelse(cells$hd.vl>t.hd.vl &cells$hd.peak.rate>5,TRUE,FALSE)
print(paste("Number of hd cells",sum(cells$hd)))

### border cells
print(paste("border cells, border.score >",round(t.border.score,4)))
cells$border<-ifelse(cells$border>t.border.score,TRUE,FALSE)
cells$border[is.na(cells$border)]<-FALSE
cells$border[cells$grid==T]<-FALSE
print(paste("Number of border cells",sum(cells$border)))

### place cells
print(paste("place cells, info.score >", round(t.info.score,4)))
cells$place<-ifelse(cells$info.score>t.info.score,TRUE,FALSE)
print(paste("Number of place cells",sum(cells$place)))
cells$place[cells$grid==T|cells$border==T]<-FALSE
print(paste("Number of place cells after removing grid and border cells",sum(cells$place)))

### speed cells
print(paste("speed cells, speed.score >",round(t.speed.score,3)))
cells$speed<-ifelse(cells$speed.score>t.speed.score,TRUE,FALSE)
print(paste("Number of speed cells",sum(cells$speed)))

### high.rate cells
print(paste("high-rate cells, mean.rate >",10))
cells$high.rate<-ifelse(cells$mean.rate>10,TRUE,FALSE)
cells$high.rate[which(is.na(cells$high.rate))]<-FALSE ## if a cell has no spikes, it does not have a high rate
print(paste("Number of high-rate cells",sum(cells$high.rate,na.rm=T)))

rm(sqr70.shuf,t.grid.score,t.hd.vl,t.info.score,t.border.score,t.speed.score,prob)
