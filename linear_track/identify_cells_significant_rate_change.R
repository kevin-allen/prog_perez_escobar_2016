## load all the firing rate maps
load(paste(ep@directory,"results","lt.diff.sign",sep="/"))
load(paste(ep@directory,"results","lt.obs.diff",sep="/"))

names(lt.diff.sign)
names(lt.obs.diff)

if(length(lt.obs.diff$cell.id)!=length(lt.diff.sign$cell.id))
  stop(paste("Problem with length of data files:"))

di<-merge(lt.diff.sign,lt.obs.diff,by=c("cell.id","direction","x"))
di<-di[order(di$cell.id,di$x),]
di$x<-di$x*2 # to cm



###################################################
### Find neurons with significant difference    ###
###################################################
### at least 5/78 bins >  than 0.01 probability ###
###################################################

cells<-cells[,seq(1,19)]

print(paste("detect cells with more than 5 bins above chance level (0.01)"))
b<-by(di,di$cell.id,function(x){5<sum(x$l1.l2.obs>x$l1.l2.shuf,na.rm=T)})
c<-by(di,di$cell.id,function(x){5<sum(x$l1.d.obs>x$l1.d.shuf,na.rm=T)})
d<-by(di,di$cell.id,function(x){5<sum(x$l2.d.obs>x$l2.d.shuf,na.rm=T)})
diff<-data.frame(cell.id=names(b),
                 l1.l2=as.logical(b),
                 l1.d=as.logical(c),
                 l2.d=as.logical(d))


cells<-merge(cells,diff,by="cell.id")
rm(di,b,c,d,diff)


print(paste("Cells changing firing rate l1vsl2 or l1vsd conditions"))

print(paste("Overall sign: ",sum(cells$l1.l2), "all:",length(cells$l1.l2)))
print(paste("Overall probability of neurons being significant: ",round(sum(cells$l1.l2)/length(cells$l1.l2),3)))


l1.l2.m<-matrix(ncol=6,nrow=2)
rownames(l1.l2.m)<-c("ja","nein")
colnames(l1.l2.m)<-c("grid","place","border","hd","speed","uid")
l1.l2.m["ja","grid"] <-sum(cells$l1.l2[which(cells$grid==T)])
l1.l2.m["nein","grid"] <-length(cells$l1.l2[which(cells$grid==T)])-sum(cells$l1.l2[which(cells$place==T)])
l1.l2.m["ja","place"] <-sum(cells$l1.l2[which(cells$place==T)])
l1.l2.m["nein","place"] <-length(cells$l1.l2[which(cells$place==T)])-sum(cells$l1.l2[which(cells$place==T)])
l1.l2.m["ja","border"] <-sum(cells$l1.l2[which(cells$border==T)])
l1.l2.m["nein","border"] <-length(cells$l1.l2[which(cells$border==T)])-sum(cells$l1.l2[which(cells$border==T)])
l1.l2.m["ja","hd"] <-sum(cells$l1.l2[which(cells$hd==T)])
l1.l2.m["nein","hd"] <-length(cells$l1.l2[which(cells$hd==T)])-sum(cells$l1.l2[which(cells$hd==T)])
l1.l2.m["ja","speed"] <-sum(cells$l1.l2[which(cells$speed==T)])
l1.l2.m["nein","speed"] <-length(cells$l1.l2[which(cells$speed==T)])-sum(cells$l1.l2[which(cells$speed==T)])
l1.l2.m["ja","uid"] <-sum(cells$l1.l2[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
l1.l2.m["nein","uid"] <-length(cells$l1.l2[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])-
  sum(cells$l1.l2[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
print("change for l1 l2")
print(l1.l2.m)
print(chisq.test(l1.l2.m))
prop.change<-apply(l1.l2.m,2,function(x){x[1]/sum(x)})
print(prop.change)

print("Changes between l1 and d")
print(paste("Overall sign: ",sum(cells$l1.d), "all:",length(cells$l1.d)))
print(paste("Overall probability of neurons being significant: ",round(sum(cells$l1.d)/length(cells$l1.d),3)))

l1.d.m<-matrix(ncol=6,nrow=2)
rownames(l1.d.m)<-c("ja","nein")
colnames(l1.d.m)<-c("grid","place","border","hd","speed","uid")
l1.d.m["ja","grid"] <-sum(cells$l1.d[which(cells$grid==T)])
l1.d.m["nein","grid"] <-length(cells$l1.d[which(cells$grid==T)])-sum(cells$l1.d[which(cells$place==T)])
l1.d.m["ja","place"] <-sum(cells$l1.d[which(cells$place==T)])
l1.d.m["nein","place"] <-length(cells$l1.d[which(cells$place==T)])-sum(cells$l1.d[which(cells$place==T)])
l1.d.m["ja","border"] <-sum(cells$l1.d[which(cells$border==T)])
l1.d.m["nein","border"] <-length(cells$l1.d[which(cells$border==T)])-sum(cells$l1.d[which(cells$border==T)])
l1.d.m["ja","hd"] <-sum(cells$l1.d[which(cells$hd==T)])
l1.d.m["nein","hd"] <-length(cells$l1.d[which(cells$hd==T)])-sum(cells$l1.d[which(cells$hd==T)])
l1.d.m["ja","speed"] <-sum(cells$l1.d[which(cells$speed==T)])
l1.d.m["nein","speed"] <-length(cells$l1.d[which(cells$speed==T)])-sum(cells$l1.d[which(cells$speed==T)])
l1.d.m["ja","uid"] <-sum(cells$l1.d[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
l1.d.m["nein","uid"] <-length(cells$l1.d[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])-
  sum(cells$l1.d[which(cells$speed==F&cells$hd==F&cells$grid==F&cells$border==F&cells$place==F)])
print("change for l1 l2")
print(l1.d.m)
print(chisq.test(l1.d.m))
prop.change<-apply(l1.d.m,2,function(x){x[1]/sum(x)})
print(prop.change)
rm(l1.d.m,l1.l2.m,lt.diff.sign,lt.obs.diff,prop.change)
