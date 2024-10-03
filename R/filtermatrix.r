#' @title filtermatrix

#' @usage final<-filtermatrix(exp)
#' @examples data(exp)
#' @examples final<-filtermatrix(exp)
#' @examples head(final)

filtermatrix <- function(data)
{
  #install require R packages if necessary and load them
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }

  nbc=ncol(data)
  data2<-data[,2:nbc]
  m2 <- apply(data2,1, mean)
  combined2<-data.frame(m2,data$gene,data)
  ord2<-combined2[with(combined2,order(-m2)),]
  ok<-ord2 %>% distinct(gene, .keep_all = T)
  nbcok=ncol(ok)
  row.names(ok)<-ok$gene
  final<-ok[,4:nbcok]
  return(final)
}
