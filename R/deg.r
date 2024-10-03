#' @title deg

#' @param group group column from phenotype file
#' @param control string defining control in pheno$group data
#' @usage data(exp)
#' @usage data(pheno)
#' @usage final<-filtermatrix(exp)
#' @usage res<-deg(final,pheno$group,control="NT")
#' @examples  data(exp)
#' @examples data(pheno)
#' @examples final<-filtermatrix(exp)
#' @examples res<-deg(final,pheno$group,control="NT")
#' @examples head(res)



deg<-function(data,group,control="HD")
{
  if(!require(limma)){

    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    BiocManager::install("limma")}

  library(limma)

  # prepare design matrix
  levels <- relevel(as.factor(group),ref=control)
  design <- model.matrix(~levels)
  rownames(design) = colnames(data)

  # perform limma analysis
  tmp <- lmFit(data,design=design)
  fit <- eBayes(tmp)
  res = topTable(fit,number = nrow(data),coef=2)
  res
}




