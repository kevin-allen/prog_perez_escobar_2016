###################################################################
# Analysis of manuscript submitted at elife                      ##
# June 2016                                                      ##
# Title: Visual landmarks sharpen grid cell metric and confer    ##
# context specificity to neurons of the medial entorhinal cortex ##
###################################################################
library(relectro) ## see https://github.com/kevin-allen/relectro
library(snow)
library(lme4)
library(car)
library(plotrix)
library(devtools)

##########################################################################
## create an ElectroProject object that contains all RecSession objects ##
##########################################################################
# you need to modify the path to the location of the data on your system.
ep<-new("ElectroProject",directory="~/data/perez_escobar_2016/data_perez_escobar_2016/circular_arena/")
ep<-setSessionList(ep)

rss<-getSessionList(ep,clustered=T,region="mec",env="circ")
rss<-sortRecSessionListChronologically(rss)
rs<-getRecSession(ep,name="jp19841-04072015-0108")

print(ep@directory)
print(paste("Number of mec recording sessions:",length(rss)))
print(paste("Number of mec mice:",length(unique(sapply(rss,function(x){x@animalName})))))

#############################################
## check inclusion criteria for neurons  ####
#############################################
# requires .fet files not available online
# source("~/repo/prog_perez_escobar_2016/circular_arena/clean_database.R")
# checked on 08.06.2016 -> all ok

##############################################################
## Cluster of computer threads to run the analysis          ##
## in parallel                                              ##
##############################################################
workers<-c(rep("localhost",6)) # number is the number of threads
print(paste("Using",length(workers), "threads"))
cl<-makeCluster(workers, type = "SOCK",outfile="")
## load the relectro package on each thread
clusterEvalQ(cl,devtools::load_all("~/repo/relectro/"))

##############################################
##############################################
## GETTING DATA FROM EACH RECORDING SESSION ##
##############################################
##############################################
#
# Now run analysis on individual recording sessions
# Apply to all cells in the database

#####################
## make cell table ##
#####################
source("~/repo/prog_perez_escobar_2016/circular_arena/cell_table.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=cellTable,save=T,parallel=F,cluster=cl))
 # create cells
rm(cellTable)

#########################
## session description ##
#########################
source("~/repo/prog_perez_escobar_2016/circular_arena/session_description.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=sessionDescription,save=T,parallel=F,cluster=cl))
# create sessions
rm(sessionDescription)

#############################################################################
## calculate grid, info, DM, CM, border, polarity and hd, speed scores     ##
## during the first baseline, with their shuffling distribution            ##
## this will be used to identify the different cell types                  ##
#############################################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/spatial_properties_baselines.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=spatialPropertiesBaseline,save=T,parallel=T,cluster=cl))
# to create bmaps, bhdhisto, bstats, bstats.shuf
rm(spatialPropertiesBaseline)

#########################################################################
## calculate grid, info, DM, CM, border, polarity and hd, speed scores ##
## during each trial condition                                         ##
#########################################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/spatial_properties_trials.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=spatialPropertiesTrials,save=T,parallel=T,cluster=cl))
# to create tmaps, tstats
rm(spatialPropertiesTrials)

###########################################
## get the hd histo for different trials ##
###########################################
source("~/repo/prog_perez_escobar_2016/circular_arena/hd_properties_trials.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=hdPropertiesTrials,save=T,parallel=F,cluster=cl))
# to create thdhisto, 
rm(hdPropertiesTrials)

######################################################################
## get correlations between l1 and l2 maps after rotation of l2 map ##
######################################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/trial_map_rotations.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=trialMapRotation,save=T,parallel=F))
# create mapRotation
rm(trialMapRotation)

###################################################################
## get correlation between 10-sec block maps to complete l1 maps ##
###################################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/trial_block_map_correlation.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=trialBlockMapCorrelation,save=T,parallel=T,cluster=cl))
# create blockMapCor
rm(trialBlockMapCorrelation)

##############################
## get spike-trigger maps  ###
##############################
source("~/repo/prog_perez_escobar_2016/circular_arena/trial_spike_triggered_maps.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=trialSpikeTriggeredMaps,save=T,parallel=T,cluster=cl))
# create trigMaps trigMapsShuf
rm(trialSpikeTriggeredMaps)

##########################################################
## speed score, intercept, slope, speed tuning curves  ###
##########################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/speed_rate_trials.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=speedRateTrials,save=T,parallel=T,cluster=cl))
# create speedRateT srTuningCurve
rm(speedRateTrials)
 
#############################################
## mean running speed in l1 and d1 trials ###
#############################################
source("~/repo/prog_perez_escobar_2016/circular_arena/running_speed_trials.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=runningSpeedTrials,save=T,parallel=F))
# create rSpeed
rm(runningSpeedTrials)

######################################################
## putative excitatory connections between neurons ###
######################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/crosscorrelation.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=crosscorrelation,save=T,parallel=F,cluster=cl))
# create stcc
rm(crosscorrelation)

#######################################
## rate change compare to shuffling ###
#######################################
source("~/repo/prog_perez_escobar_2016/circular_arena/mean_rate_change_trials.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=meanRateChangeTrials,save=T,parallel=T,cluster=cl))
# create rateChange and rateChangeShuf
rm(meanRateChangeTrials)

#####################################################
## ifr associations in different trial conditions ###
#####################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/ifr_associations_trials.R")
system.time(runOnSessionList(ep,sessionList=rss,fnct=ifrAssociationsTrials,save=T,parallel=T,cluster=cl))
# create ifrAss
rm(ifrAssociationsTrials)

## Delete cluster of threads
stopCluster(cl)
rm(cl,workers)

#########################
#########################
## IDENTIFY CELL TYPES ##
#########################
#########################

## uses sink to send the output to a file instead of terminal, remove this to see the output
## It will create a file with all the statistical analysis.
print(paste("saving statistical analysis to",paste(ep@resultsDirectory,"output",sep="/")))
sink(paste(ep@resultsDirectory,"output",sep="/"),append=FALSE,split=FALSE)

print(ep@directory)
print(paste("Number of mec recording sessions:",length(rss)))
print(paste("Number of mec mice:",length(unique(sapply(rss,function(x){x@animalName})))))

load(paste(ep@resultsDirectory,"cells",sep="/"))
print(paste("Number of cells:",length(cells$mouse)))
print(paste("Cells from mec tetrodes (only mec site in the respective hemisphere):",length(cells$mouse[which(cells$region=="mec")])))
print(paste("Other neurons (Mix of mec with adjacent areas):",length(cells$mouse[which(cells$region=="na")])))
print(paste("Mean number of cells per session:", round(length(cells$mouse)/length(unique(cells$session)),3)))
## you need to modify the path to your own software directory.
source("~/repo/prog_perez_escobar_2016/circular_arena/identify_all_cell_types.R") ## change the cells data.frame
#source("~/repo/prog_perez_escobar_2016/circular_arena/plot_rate_maps_per_cell_types.R") ## show all maps
#source("~/repo/prog_perez_escobar_2016/circular_arena/polar_plot_hd_cells.R") ## show polar plots
sink()



##########################################################
##########################################################
### spike distance metric from Hardcastle et al. 2015  ###
### only on grid cells to limit the size of dataset    ###
### we use the cells data.frame to identify grid cells ###
##########################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/spike_distance_metric.R") # define function
grid.session.names<-as.character(unique(cells$session[which(cells$grid==T)]))
runOnSessionList(ep=ep,sessionList=getSessionListFromSessionNames(ep,grid.session.names),
                 fnct=spikeErrorDistanceMetric,save=T,parallel=F,cells=cells) ## pass the cells data.frame as argument to fnct
rm(spikeErrorDistanceMetric,grid.session.names)


#################################
#################################
### Do stats and Figures ########
#################################
#################################
sink(paste(ep@resultsDirectory,"output",sep="/"),append=TRUE,split=FALSE)
####################################
## figure 1 Example and rotation ###
####################################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_1.R")

###########################
## figure 2 Grid cells ####
###########################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_2.R")

################################################
## figure 2 supplement, Spike distance metric ##
################################################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_2_supplement.R")

########################
## figure 3 Distance  ##
########################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_3.R")

##########################
## figure 4 Speed cells ##
##########################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_4_1.R")
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_4_2.R")

###########################
## figure 5 Border cells ##
###########################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_5.R")

#######################
## figure 6 HD cells ##
#######################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_6.R")

###########################
## figure 7 Rate changes ##
###########################
source("~/repo/prog_perez_escobar_2016/circular_arena/figure_7.R")

sink()
