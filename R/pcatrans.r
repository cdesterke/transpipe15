#' @title pcatrans

#' @param group group column from phenotype file
#' @param alpha tranparency of the points
#' @param names see or not names of the samples
#' @param x first axis
#' @param y second axis
#' @usage data(exp)
#' @usage data(pheno)
#' @usage final<-filtermatrix(exp)
#' @usage res<-deg(final,pheno$group,control="NT")
#' @usage sig<-filtresig(res)
#' @usage process<-reducedf(sig,final,n=20)
#' @usage pcatrans(process,pheno,group="group",pal="Set1",alpha=0.7,names=TRUE)## palette examples : Set1, Set2, Set3, Pastel1, Pastel2, Dark1, Dark2, Accent
#' @examples data(exp)
#' @examples data(pheno)
#' @examples final<-filtermatrix(exp)
#' @examples variable<-varsig(final,x=1)
#' @examples res<-deg(variable,pheno$group,control="NT")
#' @examples sig<-filtresig(res)
#' @examples process<-reducedf(sig,final,n=20)
#' @examples pcatrans(process,pheno,group="group",pal="Set1",alpha=0.7,names=TRUE)
#' @examples pcatrans(variable,pheno,group="group",pal="Set1",alpha=0.7,names=TRUE)





pcatrans<-function(data,pheno,group="group",pal="Dark2",alpha=0.5,names = FALSE,x=1,y=2){

  #install require R packages if necessary and load them
  if(!require(ggfortify)){
    install.packages("ggfortify")
    library(ggfortify)}
  if(!require(ggrepel)){
    install.packages("ggrepel")
    library(ggrepel)}

# transpose data
trans<-t(data)

# perform PCA on transposed data
pca<-prcomp(trans,scale=TRUE)

# graphical output if names FALSE
if(names==FALSE){
autoplot(pca,data=pheno,colour=group,label=F, label.size=0,size=4,alpha=alpha,x = x, y = y)+
  theme_classic(base_size = 16)+ scale_color_brewer(palette=pal)}

# graphical output if names TRUE
else{
  autoplot(pca,data=pheno,colour=group,label=F, label.size=4,size=4,alpha=0.5,x = x, y = y)+
    theme_classic(base_size = 16)+ scale_color_brewer(palette=pal)+
    geom_text_repel(label = rownames(pheno))}

}

