#' @title filtresig

#' @param res results from limma deg analysis
#' @usage data(exp)
#' @usage data(pheno)
#' @usage final<-filtermatrix(exp)
#' @usage res<-deg(final,pheno$group,control="NT")
#' @usage sig<-filtresig(res)
#' @examples  data(exp)
#' @examples data(pheno)
#' @examples final<-filtermatrix(exp)
#' @examples res<-deg(final,pheno$group,control="NT")
#' @examples sig<-filtresig(res)
#' @examples dim(sig)
#' @examples head(sig)





filtresig<-function(res){
  # require libraries
  if(!require(dplyr)){install.packages("dplyr")}
  library(dplyr)

  # filtre signficant genes
  res%>%filter (abs(logFC) >= 1 & adj.P.Val <= 0.05) -> filtre
  return(filtre)

}



