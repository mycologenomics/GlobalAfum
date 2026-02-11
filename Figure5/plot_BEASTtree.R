library(ggtree)
library(ggnewscale)

beast_tree=read.beast("tree.MCC.tree")
p=ggtree(beast_tree,mrsd="2021-01-01")
temporalMetadata=read.csv("temporal.csv",header=T)
p=p %<+% temporalMetadata
p1=p+geom_tippoint(mapping=aes(colour=Cluster),size=3,alpha=.75)+
scale_colour_gradientn(colours=DAPC_colours)+
theme_tree2()

#Colour branches by cluster:

tree=read.beast("tree.MCC.tree")
#look at the clade node number:
ggtree(tree,mrsd="2021-01-01")+theme_tree2()+geom_text(aes(label=node))
clade=c(Cluster1=702,Cluster2=746,Cluster3=516,Cluster4=796,Cluster5=961)
groupedTree=groupClade(tree,clade)
cols=c(Cluster1="#440154",Cluster2="#3b528b",Cluster3="#21918c",Cluster4="#5ec962",Cluster5="#fde725")

#basic tree with only branches coloured:
basic=ggtree(groupedTree,aes(color=group),ladderize=FALSE,mrsd="2021-01-01")+theme_tree2()
colouredTree=basic+scale_colour_manual(values=c(cols,"black"),na.value="black",breaks=c("Cluster1","Cluster2","Cluster3","Cluster4","Cluster5"))+new_scale_colour()

#Add info as tippoint (e.g. benA info):
cols_benA=c(Wildtype="#6d6d64",F219Y="#e656dd")
p1=colouredTree %<+% temporalMetadata+
geom_tippoint(aes(colour=benA),alpha=0.75,size=2,stroke=0)+
scale_colour_manual(values=cols_benA,guide=guide_legend(keywidth=0.5,keyheight=0.5,order=2,override.aes=list(size=2,alpha=1)))
