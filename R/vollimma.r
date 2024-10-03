#' @title vollimma

#' @param res results from limma deg analysis
#' @param nb number of gene names to write
#' @param fc fold change threshold
#' @param p p-value threshold
#' @param size size of the gene labels
#' @param alpha transparent points

#' @usage data(exp)
#' @usage data(pheno)
#' @usage final<-filtermatrix(exp)
#' @usage res<-deg(final,pheno$group,control="NT")
#' @usage vollimma(res,nb=500,fc=0.5,p=0.05,size=4,alpha=1)
#' @examples data(exp)
#' @examples data(pheno)
#' @examples final<-filtermatrix(exp)
#' @examples res<-deg(final,pheno$group,control="NT")
#' @examples vollimma(res,nb=500,fc=0.5,p=0.05,size=4,alpha=1)




vollimma<-function(res,nb=30,fc=0.5,p=0.05,size=3,alpha=1){
  # require libraries
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(ggplot2)
  if(!require(ggrepel)){install.packages("ggrepel")}
  library(ggrepel)

  # add a column of NO UP DOWN
  res$diffexpressed <- "NO"
  res$diffexpressed[res$logFC> fc & res$P.Value < p] <- "UP"
  res$diffexpressed[res$logFC < -fc & res$P.Value < p] <- "DOWN"

  # grep labels on row.names
  res$delabel<-row.names(res)

  # perform the graph
  ggplot(data=res, aes(x=logFC, y=-log10(P.Value), col=diffexpressed, label=delabel)) +
    geom_point(alpha=alpha) +
    theme_minimal()+
    geom_text_repel(data=head(res, nb), aes(label=delabel),size=size) +
    scale_color_manual(values=c("#4697e1", "darkgray", "#FF00FF")) +
    geom_vline(xintercept=c(-fc, fc), col="orange",linetype="dashed") +
    geom_hline(yintercept=-log10(p), col="orange",linetype="dashed") +
    theme(legend.position = "none")

}


