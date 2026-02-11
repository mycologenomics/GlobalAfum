library(ggtree)
library(ggplot2)
library(ggnewscale)
library(treeio)
library(tidytree)
library(dplyr)
library(ggstar)
library(reshape2)
library(ggtreeExtra)

tree=read.tree("raxml-ng_global.raxml.bestTree")
trroot=root(treefile,outgroup="C250")

globalMetadata=read.csv("globalMetadata.csv",header=T)

# add DAPC clusters to tip points
DAPC_colours=viridis(5)
p=ggtree(trroot,layout="circular",size=0.15)+geom_treescale()
p=p %<+% globalMetadata
p1=p+geom_tippoint(mapping=aes(colour=DAPC))+scale_colour_manual(name="DAPC",values=DAPC_colours)

# add antifungal susceptibliity/mutation as heatmap
globalMetadata=read.csv("globalMetadata.csv",header=T)
heatmap_dat=globalMetadata %>% select(c("ID","ITR.MIC","TEB"))
heatmap_dat=melt(heatmap_dat,id="ID",variable.name="Antifungal",value.name="type")
heatmap_dat$type=paste(heatmap_dat$Antifungal,heatmap_dat$type)
heatmap_dat$type[grepl("Not_resistant",heatmap_dat$type)]="Susceptible"
heatmap_dat$Antifungal=factor(heatmap_dat$Antifungal,levels=c("ITR.MIC","TEB"))
heatmap_dat$type=factor(heatmap_dat$type,levels=c("Not_tested","Susceptible","ITR.MIC Resistant","TEB Resistant"))

p2=p1+geom_fruit(data=heatmap_dat,geom=geom_tile,mapping=aes(x=Antifungal,y=ID,fill=type),width=0.075,color="white",pwidth=0.075,offset=0.075)+
scale_fill_manual(name="Antifungal susceptibility",values=c("#7FBF7B","#AF8DC3","#B6026A"),na.translate=FALSE,guide=guide_legend(keywidth=0.5,keyheight=0.5,order=3))+
new_scale_fill()
