#' @title varsig

#' @param x number of standard deviation over mean to defined theshold
#' @usage variable<-varsig(final,x=1)
#' @examples data(exp)
#' @examples final<-filtermatrix(exp)
#' @examples variable<-varsig(final,x=1)
#' @examples head(variable)


varsig <- function(data,x=1){


  variable <- apply(data,1, var)
  var<-variable[!is.na(variable)]
  tb<-mean(var)+(x*sd(var))
  baseline<-mean(var)
  vartable<-data.frame(var)

  combined<-merge(vartable,data,by="row.names")
  row.names(combined)<-combined$Row.names
  combord<-combined[with(combined,order(-var)),]
  colord<-ncol(combord)
  combord<-combord[,2:colord]

  m <- apply(combord,1, mean)



  sub<-subset(combord, combord$var > tb)
  head(sub,n=30)[,1:3]
  nrow(sub)
  sub$var<-NULL
  x=nrow(sub)
  plot(var,m,main=paste(x," Unsupervized most variable genes"),xlab="Variance of expression",
       ylab="Mean of expression")
  abline(v=tb,col="blue",lty=3)
  abline(v=baseline,col="red",lty=1)

  return(sub)

}



