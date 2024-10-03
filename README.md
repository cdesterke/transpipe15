# transpipe15
pipeline transcriptome updated


R package to pipeline limma analysis for transcriptome between two samples groups


### package installation
```r
library(devtools)
install_github("cdesterke/transpipe15")
```


## classical script
```r
library(transpipe)

## load data
data(exp)
data(pheno)

##prepare data
final<-filtermatrix(exp)

## select variable genes unsupervized
variable<-varsig(final,x=1)

## perform DEG analysis
res<-deg(variable,pheno$group,control="NT")

## DEG analysis with one cofactor
res<-degcofactor(final,pheno$group,control="NT",cofactor=pheno$batch)

## filter significant DEGs
sig<-filtresig(res)

## draw volcanoplot
vollimma(res,nb=500,fc=0.5,p=0.05,size=4,alpha=1)

## subset matrix to significant genes
process<-reducedf(sig,final,n=20)

## perform pca with ellipses
pcatransellipses(process,pheno,group="group",pal="Set1",alpha=0.5,names=F,x=1,y=2,level=0.75)
#### palette examples : Set1, Set2, Set3, Pastel1, Pastel2, Dark1, Dark2, Accent
pcatransellipses(variable,pheno,group="group",pal="Set1",alpha=0.5,names=F,x=1,y=2,level=0.75)

## perform PCA without ellipses
pcatrans(variable,pheno,group="group",pal="Set1",alpha=0.7,names=TRUE)

## draw heatmap
bestheat(process,pheno,scale="none",font=10,rownames=T)


```
