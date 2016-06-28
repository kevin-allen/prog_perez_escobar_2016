library("relectro") ## see https://github.com/kevin-allen/relectro
library("snow")

#################################
## get the recording sessions ###
#################################
# you need to modify the path to the location of the data on your system.
ep<-new("ElectroProject",directory="~/data/perez_escobar_2016/data_perez_escobar_2016/linear_track")
ep<-setSessionList(ep)
save(ep,file=paste(ep@directory,"ep",sep="/"))
load("~/data/perez_escobar_2016/data_perez_escobar_2016/linear_track/ep")

## list of session that we will analyze
rss<-getSessionList(ep,clustered=T,region="mec",env="lt")
rss<-sortRecSessionListChronologically(rss)
print(paste("Number of recording sessions",length(rss)))
rs<-rss[[11]] ## example of single session to play with

print(ep@directory)
print(paste("Number of mec recording sessions:",length(rss)))
print(paste("Number of mec mice:",length(unique(sapply(rss,function(x){x@animalName})))))

########################
## check clustering ####
#############################
# cluster isolation quality #
# need .fet files not available online
# checked 24.06.2016, ok
#runOnSessionList(ep,sessionList=rss,clusterIsolationCheck,save=T,overwrite = T)
#load(paste(ep@resultsDirectory,"cluster.check",sep="/"))
#length(cluster.check$cluId)
#summary(cluster.check$isolationDistance)
#cluster.check[which(cluster.check$refractoryRatio<.15&cluster.check$refractoryRatio>0.125),]
#cluster.check[which(cluster.check$isolationDistance<5),]



######################################
## cluster for parallel processing  ##
######################################
workers<-c(rep("localhost",6))
cl<-makeCluster(workers, type = "SOCK",outfile="")
print(paste("Using",length(workers), "threads"))
## load the relectro package on each thread
clusterEvalQ(cl,devtools::load_all("~/repo/relectro/"))

################################################
## only keep sessions with at least 20 blocks ##
## remove from rss                            ##
################################################
min.blocks<-20
source("~/repo/prog_perez_escobar_2016/linear_track/number_light_blocks_per_session.R")
list.res<-runOnSessionList(ep,sessionList=rss,fnct=number.light.blocks.per.session,save=F)
num.light.blocks<-as.numeric(do.call("rbind", sapply(list.res,function(x){x["num.light.blocks"]})))
rss<-rss[ifelse(num.light.blocks>min.blocks,TRUE,FALSE)]
print(paste("Number of recording sessions with at least",min.blocks,"light blocks:",length(rss)))
rm(min.blocks,list.res,num.light.blocks,number.light.blocks.per.session)

## make the cells data.frame
source("~/repo/prog_perez_escobar_2016/linear_track/make_sqr70_cell_table.R") 
runOnSessionList(ep,sessionList=rss,fnct=sqr70.cell.properties,save=T,overwrite=T,parallel=T,cluster=cl)
# makes sqr70.maps, cells, sqr70.shuf data.frames
rm(sqr70.cell.properties)

## get firing rate differences on linear track
source("~/repo/prog_perez_escobar_2016/linear_track//linear_track_session_map_analysis.R")
runOnSessionList(ep,sessionList=rss,fnct=linear.track.maps.stats,save=T,overwrite=T,parallel=T,cluster=cl)
# makes lt.maps.2d, histo1d, stats2d, stats1d, lt.obs.diff
rm(linear.track.maps.stats)

## get the firing rate differences observed during shuffling
source("~/repo/prog_perez_escobar_2016/linear_track/linear_track_session_map_shuffle.R")
runOnSessionList(ep,sessionList=rss,fnct=linear.track.maps.condition.shuffle,save=T,overwrite=T,parallel=T,cluster=cl)
# makes lt.diff.sign
rm(linear.track.maps.condition.shuffle)

stopCluster(cl)
rm(cl,workers)

#######################################################
### analysis on merged data from recording sessions ###
#######################################################
load(paste(ep@directory,"results","cells",sep="/"))
print(paste("Number of neurons:",length(cells$cell.id)))
print(paste("Number of mice:",length(unique(cells$mouse))))

## identify the different cell types
source("~/repo/prog_perez_escobar_2016/linear_track/identify_cell_types.R")
## plot 2d maps to check cell selection
source("~/repo/prog_perez_escobar_2016/linear_track/plot_spatial_maps_2d.R")
## plot 2d and 1d maps according to light condition
source("~/repo/prog_perez_escobar_2016/linear_track/plot_maps_light.R") # takes a while (15 min)
## plot 1d rate difference and significance levels
source("~/repo/prog_perez_escobar_2016/linear_track/plot_light_difference_stats.R")
## get the cells with significant changes ##
source("~/repo/prog_perez_escobar_2016/linear_track/identify_cells_significant_rate_change.R")
## make the graphs for the first linear track figure
source("~/repo/prog_perez_escobar_2016/linear_track/figure_linear_track_rate_change.R")
