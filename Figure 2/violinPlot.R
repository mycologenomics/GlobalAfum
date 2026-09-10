variants=read.csv("Variants.csv",header=T)
v=ggplot(variants,aes(x=DAPC,y=SNPs,color=DAPC))+
geom_violin()+
scale_color_manual(values=DAPC_colours)+
geom_jitter(shape=16,position=position_jitter(0.2))+
theme_classic()