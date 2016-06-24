### function doing all the work on individual sessions
number.light.blocks.per.session<-function(rs){
  ## get the trial intervals for the light
  ti.file<-paste(rs@fileBase,"light_intervals",sep=".")
  if(!file.exists(ti.file))
    stop(paste("needs",ti.file,"in number.light.blocks.per.session",rs@session))
  ti<-read.table(ti.file,header=F)
  return(list(num.light.blocks=length(ti$V1)))
}
