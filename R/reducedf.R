#' @title reducedf

#' @param res results from limma
#' @param data original dataframe output from filtermatrix
#' @param n number of genes
#' @usage data(exp)
#' @usage data(pheno)
#' @usage final<-filtermatrix(exp)
#' @usage res<-deg(final,pheno$group,control="NT")
#' @usage process<-reducedf(res,final,n=35)
#' @examples  data(exp)
#' @examples data(pheno)
#' @examples final<-filtermatrix(exp)
#' @examples res<-deg(final,pheno$group,control="NT")
#' @examples sig<-filtresig(res)
#' @examples process<-reducedf(sig,final,n=35)
#' @examples head(process)





reducedf<-function(res,data,n=30){

  #install require R packages if necessary and load them
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }

res%>%arrange(-desc(P.Value))%>%head(n=n)->small

process<-merge(small,data,by="row.names")

row.names(process)<-process$Row.names


process<-process[,8:ncol(process)]

process
}
