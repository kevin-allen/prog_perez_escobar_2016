rename.condition.data.frame <- function(x){
  # return a data frame in which condition is changed to l1 l2 d1 and d2
  c<-sort(as.character(unique(x$condition))) # d1 d2 l1 l2 ## alphabetically
  x$condition<-as.character(x$condition)
  if(length(c)==4){
    x$condition[which(x$condition==c[1])]<-"d1"
    x$condition[which(x$condition==c[2])]<-"d2"
    x$condition[which(x$condition==c[3])]<-"l1"
    x$condition[which(x$condition==c[4])]<-"l2"
  }
  else{
    x$condition[which(x$condition==c[1])]<-"d1"
    x$condition[which(x$condition==c[2])]<-"l1"
  }
  x$condition<-as.factor(x$condition)
  return(x)
}

