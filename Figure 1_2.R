# ANTIDEPRESSANT USE AND SPATIAL SOCIAL CAPITAL

# This script generates the co-efficient plot, 
# the scatterplot of predicted antidepressant use and spatial diversity, 
# and the interplot visualizations.



# Figure 1B

# Fig 1B.3 Thematic map
library(haven)

town_a=read_dta("usage_stats2012_all_settlements.dta")
head(town_a)

# test the town-size antidep correlation - THERE IS NO CORRELATION

population=read.table("population.csv", sep=",", header = T)
head(population)
size_anti = town_a %>% left_join(population, by=c("kshkod"="KSHK"))

png("size_anti.png", width=600, height=600)
plot(log10(size_anti$pop), size_anti$antidep_bin2012)
dev.off()

a=lm(antidep_bin2012 ~ log10(pop), data=size_anti)
summary(a)

size_anti$logpop=log10(size_anti$pop)

cor(size_anti$antidep_bin2012, size_anti$logpop, use = "complete.obs", method="pearson")


# draw map
setwd("iwiw shape")

#install.packages("rgeos")
library(rgeos)
#install.packages("rgdal")
library(rgdal)
library(dplyr)
library(ggplot2)
#install.packages("raster")
require(raster)
require(viridis)
require(gtable)
require(grid)
require(tidyr)
#install.packages("cowplot")
require(cowplot)
#install.packages("maptools")
library(maptools)

# load the shapefile and convert it into a dataframe
towns <- readOGR("iwiw shape/OSN_region.shp", layer="OSN_region")  
towns_fortified <- fortify(towns, region = "KSHKOD") %>% mutate(id = as.numeric(id))

summary(towns_fortified)

# MAP OUTLINE
theme_map <- function(...) {  
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# join data with geometric data

map_data = towns_fortified %>% left_join(town_a, by=c("id" = "kshkod"))

summary(town_a$antidep_bin2012)

# plot the map

pretty_breaks <- c(0.01,0.02,0.03,0.04,0.05,0.1)

# find the extremes
minVal <- min(map_data$antidep_bin2012, na.rm = T)  
maxVal <- max(map_data$antidep_bin2012, na.rm = T)  
# compute labels
labels <- c()  
brks <- c(minVal, pretty_breaks, maxVal)  
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){  
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]

# define a new variable on the data with the breaks
map_data$brks <- cut(map_data$antidep_bin2012,  
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(map_data$brks)  
labels_scale <- rev(brks_scale)
#labels_scale=c("0.05","0.04","0.03","0.02","0.01")


cairo_pdf(filename="Fig_antidep_map.pdf", width=12, height=7)
ggplot() +  
  geom_polygon(data = map_data, aes(fill = brks, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_data, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  theme(
    legend.position = c(0.77, 0.07),
    legend.text.align = 0,
    legend.background = element_rect(fill = alpha('white', 0.0)),
    legend.text = element_text(size = 18, hjust = 0, color = "#4e4d47"),
    legend.title = element_text(size = 25),
    plot.title = element_text(size = 28, hjust = 0.8, color = "#4e4d47"),
    plot.subtitle = element_text(size = 40, hjust = 0.8, face = "italic", color = "#4e4d47"),
    plot.caption = element_text(size = 30, hjust = 0.95, color = "#4e4d47"),
    plot.margin = unit(c(0.5,0.5,0.2,0.5), "cm"),
    panel.border = element_blank()
  ) +
  labs(x = NULL, 
       y = NULL) + 
  scale_fill_manual(
    values = rev(magma(8, alpha = 0.8)[2:8]),
    breaks = rev(brks_scale),
    name = "Antidepressant Probability",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(150/length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"
    )
  )
dev.off()


# Figure 1B.2 and 1B.3 Analyzed Towns and Network map

library(igraph)

nodes=read.table("iWiW_town_nodes.csv", sep="\t", header = T)
edges= read.table("iWiW_town_edges.csv", sep="\t", header = T)

head(nodes)


# 1B.2 Analyzed towns 

d=read_table("id_master_v3.csv")

c=unique(d[,c("cityid1","kshkod")])
c=merge(c,nodes, by.x="kshkod", by.y = "code", all=F)


cairo_pdf(filename="towns_dark.pdf", width=12, height=7)
plot(c$longitude, c$latitude, 
     col="darkgray",
     axes=F, frame=F,
     xlab="", ylab="",
     pch=19, cex=1.5)
mtext("Analyzed towns",cex=3, side=1, adj=0)
dev.off()



# 1B.1 Network of analyzed towns

# delete edges where analyzed towns are not present
e=unique(rbind(edges,edges[,c(2,1,3)]))
nrow(edges)
nrow(e)

head(e)
head(c)

e=merge(e,c, by.x="id1", by.y="id", all.x=F)

g_a=graph.data.frame(e,vertices=nodes,directed=FALSE)

head(nodes)

lat<-as.numeric(nodes[,5])
long<-as.numeric(as.character(nodes[,6]))
kord<-matrix(c(long,lat),nrow=2558,ncol=2)
lat2<-(lat-47)/2
long2<-(long-19.5)/2
kord2<-matrix(c(long2,lat2),nrow=2558,ncol=2)



g1=subgraph.edges(g_a, eids = E(g_a)[which(E(g_a)$connect>=3000 & E(g_a)$connect<5000)], delete.vertices=F)
g2=subgraph.edges(g_a, eids = E(g_a)[which(E(g_a)$connect>=5000 & E(g_a)$connect<15000)], delete.vertices=F)
g3=subgraph.edges(g_a, eids = E(g_a)[which(E(g_a)$connect>=15000)], delete.vertices=F)
#topo.colors(4, alpha=1)[4]

g1=simplify(g1)
g2=simplify(g2)
g3=simplify(g3)

library(viridis)
magma(3, alpha = 0.8)[3]

cairo_pdf(filename="network.pdf", width=12, height=7)
plot.igraph(x=g1,
            layout=kord2, 
            vertex.label=NA, vertex.size=0, vertex.color=NA, 
            vertex.frame.color=NA,
            edge.color=viridis(3, alpha = 0.8)[3], edge.curved=F,
            edge.width=2,
            axes=F, frame=F, rescale=F,
            vertex.label=NA
)
plot.igraph(x=g2,
            add=T,
            layout=kord2, 
            vertex.label=NA, vertex.size=0, vertex.color=NA, 
            vertex.frame.color=NA,
            edge.color=viridis(3, alpha = 0.8)[2], edge.curved=F,
            edge.width=2,
            axes=F, frame=F, rescale=F,
            vertex.label=NA
)
plot.igraph(x=g3,
            add=T,
            layout=kord2, 
            vertex.label=NA, vertex.size=0, vertex.color=NA, 
            vertex.frame.color=NA,
            edge.color=viridis(3, alpha = 0.8)[1], edge.curved=F,
            edge.width=2,
            axes=F, frame=F, rescale=F,
            vertex.label=NA
)
legend(-1.64, 0.9, 
       legend=c("9000","6000","3000"),
       text.col="black",
       col=c(viridis(3, alpha = 0.8)[1], viridis(3, alpha = 0.8)[2],viridis(3, alpha = 0.8)[3]),
       cex=1.3, bty="n",
       fill="white",
       border='white',
       title= "Social Connections of Analyzed Towns",
       c(lty=1,lty=1,lty=1,lty=1),
       pt.bg='white',
       lwd=3,
       horiz=T
)
dev.off()


# Figure 2 - Probability barplots
library(confintr)
library(ggplot2)
#install.packages("patchwork")
library(patchwork)

d=read.table("id_master_v3.csv")

d$b1=as.numeric(d$b1)

d$SD_group="High SD"
d$SD_group[d$H_norm<=median(d$H_norm, na.rm=T)]="Low SD"

d$LC_group="High LC"
d$LC_group[d$cc_in_ERnorm<=median(d$cc_in_ERnorm, na.rm=T)]="Low LC"


d$SD_LC_group="High SD High LC"
d$SD_LC_group[d$cc_in_ERnorm<=median(d$cc_in_ERnorm, na.rm=T) & 
                d$H_norm<=median(d$H_norm, na.rm=T)]="Low SD Low LC"
d$SD_LC_group[d$cc_in_ERnorm<=median(d$cc_in_ERnorm, na.rm=T) & 
                d$H_norm>median(d$H_norm, na.rm=T)]="High SD Low LC"
d$SD_LC_group[d$cc_in_ERnorm>median(d$cc_in_ERnorm, na.rm=T) & 
                d$H_norm<=median(d$H_norm, na.rm=T)]="Low SD High LC"

p1<-ggplot(aes(x=SD_group, y=b11, fill=SD_group), data = d)+
  stat_summary(fun=mean, geom="bar", width=.5)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=.25)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position = "none")

p2<-ggplot(aes(x=LC_group, y=b11, fill=LC_group), data = d)+
  stat_summary(fun=mean, geom="bar", width=.5)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=.25)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position = "none")

p3<-ggplot(aes(x=SD_LC_group, y=b11, fill=SD_LC_group), 
           data = d[d$SD_LC_group=="High SD High LC" | d$SD_LC_group=="Low SD Low LC",])+
  stat_summary(fun=mean, geom="bar", width=.5)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=.25)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+
  theme(legend.position = "none")



p1+p2+p3

cairo_pdf(filename="Fig 2 barplots_b11.pdf", width=18, height=6)
p1+p2+p3
dev.off()

# Figure 2 - Days of Treatment Dynamics

# Data
d=read.table("id_master_v3.csv")

# Correlation plot
c_d=d[d$b1==1,c("antidep_dot2011", 
         "antidep_dot2012", "antidep_dot2013", 
         "antidep_dot2014", "antidep_dot2015")]

names(c_d)=c("2011", 
             "2012", "2013", 
             "2014", "2015")
cormat=cor(c_d, method="pearson", use = "complete.obs")

get_upper_tri <- function(cormat){
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}


upper_tri <- get_upper_tri(cormat)
upper_tri


#install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

library(ggplot2)
cairo_pdf("Fig2C_antidep_dynamic_corr.pdf", width=6, height=6)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white")+
  theme_minimal() + 
  scale_fill_gradient2(space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()

# Average dose per year
means=c(mean(c_d$`2011`, na.rm=T),mean(c_d$`2012`, na.rm=T),
        mean(c_d$`2013`, na.rm=T),mean(c_d$`2014`, na.rm=T), 
        mean(c_d$`2015`, na.rm=T))
years=c(2011,2012,2013,2014,2015)

cairo_pdf("Fig2D_antidep_dose_dynamics.pdf", width=6, height=3)
plot(years, means, type = "b", frame = FALSE, pch = 19, 
     col = "red", ylab = "Average days of treatment in a year")
dev.off()
