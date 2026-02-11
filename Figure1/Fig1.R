library(mapmixture)
library(ggplot2)
library(viridisLite)

admixture1=read.csv("pruned.5.Q.mapmixture.forPlotting.csv",header=T)
coordinates=read.csv("coords.csv",header=T)

map4 <- mapmixture(
  admixture_df = admixture1,
  coords_df = coordinates,
  cluster_cols = viridis(5),
  cluster_names = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4", "Cluster 5"),
  pie_size = 2.5,
)+
  # Adjust theme options
  theme(
    legend.position = "top",
    plot.margin = margin(l = 10, r = 10),
  )+
  # Adjust the size of the legend keys
  guides(fill = guide_legend(override.aes = list(size = 5, alpha = 1)))

siteOrder=unique(coordinates$sites)

#Traditional structure barplot
structure_barplot <- structure_plot(
  admixture_df = admixture1,
  type = "structure",
  cluster_cols = viridis(5),
  site_dividers = TRUE,
  divider_width = 0.4,
  site_order = siteOrder,
  labels = "site",
  flip_axis = FALSE,
  site_ticks_size = -0.05,
  site_labels_y = -0.35,
  site_labels_size = 2.2
)+
  # Adjust theme options
  theme(
    axis.title.y = element_text(size = 8, hjust = 1),
    axis.text.y = element_text(size = 5),
  )

#Arrange plots:
grid.arrange(map4,structure_barplot,nrow=2,beights=c(4,1))