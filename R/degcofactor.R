#' @title degcofactor

#' @param group group column from phenotype file
#' @param control string defining control in pheno$group data
#' @param cofactor string defining cofactor variable in design matrix
#' @usage data(exp)
#' @usage data(pheno)
#' @usage final<-filtermatrix(exp)
#' @usage res<-degcofactor(final,pheno$group,control="NT",cofactor=pheno$batch)
#' @examples  data(exp)
#' @examples data(pheno)
#' @examples final<-filtermatrix(exp)
#' @examples res<-degcofactor(final,pheno$group,control="NT",cofactor=pheno$batch)
#' @examples head(res)





degcofactor<-function(data,group,control="HD",cofactor)
{
  if(!require(limma)){

    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    BiocManager::install("limma")}

  library(limma)

  # prepare design matrix
  levels <- relevel(as.factor(group),ref=control)
  design <- model.matrix(~levels+cofactor)
  rownames(design) = colnames(data)

  # perform limma analysis
  tmp <- lmFit(data,design=design)
  fit <- eBayes(tmp)
  res = topTable(fit,number = nrow(data),coef=2)
  res
}




