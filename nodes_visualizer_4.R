library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(data.table)
library(igraph)
library(visNetwork)
library(htmlwidgets)

#setwd("C:/Users/Raza/OneDrive - University of Toronto/UK Plasma Systematic Review/Nodes Visualization/NodeVisualizer")

#https://kateto.net/networks-r-igraph



######### Network generation ##############

linksx <- read.csv("new_links5.csv", header=T, as.is=T) #copying code from above

nodesx <- read.csv("result6_mod.csv", header=T, as.is=T) #copying code from above

nodesx$weight<-linksx$fromCount[match(unlist(nodesx$media),linksx$from)]
nodesx <- replace(nodesx,is.na(nodesx),1) #transferrign weights from links df to nodes df
nodesx$label_names <- gsub("\\[.*\\]", "", nodesx$media) #adding label names

net3 <- graph_from_data_frame(d=linksx, vertices=nodesx, directed=F) #creating graph 




V(net3)$category<-nodesx$category #adding color to graph

V(net3)$color<-nodesx$color #adding color to graph
#V(net3)$weight<-nodesx$weight #adding weights to graph
V(net3)$weight<-nodesx$nodeweight #adding weights to graph

V(net3)$font<-nodesx$font #adding font style (bold/plain) to graph
V(net3)$fontsize<-nodesx$fontsize #adding font size to graph
V(net3)$fontcolor<-nodesx$fontcolor #adding font size to graph
V(net3)$edgewidth<-nodesx$edgewidth #adding font size to graph
V(net3)$edgecolor<-nodesx$edgecolor #adding font size to graph

#V(net3)$edgewidth<-linksx$edgewidth #adding color to graph

#V(net3)$name <- ifelse ((V(net3)$weight > 10),paste0(V(net3)$name, "=", V(net3)$weight),(V(net3)$name)) #add sample weights to the name


edge.start <- ends(net3, es=E(net3), names=F)[,1] #what's happenign is that you are using 'ends' to get the edges of this network,which gives you a 2 column list of pairs of numbers for start and end of the edges. you then pick [,2] which represents the end (you could've picked the start which is [,1]), which you can then use to feed into edge.start for later use in the next
edge.col <- V(net3)$color[edge.start] #set link color to source (edge.start) or target (edge.target)

edge.end <- ends(net3, es=E(net3), names=F)[,2] #what's happenign is that you are using 'ends' to get the edges of this network,which gives you a 2 column list of pairs of numbers for start and end of the edges. you then pick [,2] which represents the end (you could've picked the start), which you can then use to feed into edge.start for later use in the next

edge.width <- V(net3)$weight[edge.end]/5 #set link color to source (edge.start) or target (edge.target)


#tail_of(fg, edge.of.interest)$value

#edge.start <- ends(net3, es=E(net3), names=F)[,1]
edge.col <- V(net3)$color[edge.start] #set link color to source (edge.start) or target (edge.target)

normalize_01 <- function(x) (x - min(x)) / (max(x) - min(x)) + 0.25


#######remove level 4#####

# Create a logical vector where each element is TRUE if the node name contains "4"
nodes_to_remove <- grepl("[4]", V(net3)$name)

# Remove nodes and their associated edges
net3 <- delete.vertices(net3, nodes_to_remove)


##remove certain entire clusters to generate individual graphs

clusterRemove <- c("Resource", "Clinical", "Life")

# for (cluster in clusterRemove) {
#   
#   # Create a logical vector where each element is TRUE if the node name contains "4"
#   nodes_to_remove <- grepl(cluster, V(net3)$cluster)
#   
#   # Remove nodes and their associated edges
#   net3 <- delete.vertices(net3, nodes_to_remove)
# }



############### Plot Creation #########

pdf("test_file_UKPSR_test57.pdf",30,25)



##################
fr <- layout_with_fr(net3, start.temp=10, niter = 1000 )

#partial graph
#plot(net3, edge.arrow.size=.2,edge.curved=.1,edge.color=edge.col,vertex.label = V(net3)$label_names , vertex.size=V(net3)$weight/3,vertex.label.cex	=(V(net3)$fontsize/10)+0.4, vertex.label.dist=V(net3)$distance, vertex.label.degree = pi/2, vertex.label.adjust=c(0.5,0.5), vertex.color=V(net3)$color, vertex.frame.color =V(net3)$color,  asp=0.5, layout=fr, vertex.label.font=V(net3)$font, vertex.label.family="sans",  vertex.label.color=c(V(net3)$fontcolor), vertex.shape="circle", edge.width=edge.width/5, edge.curved=1) #displaying graph weighed for several things, and aspect ratio of 0.35


#full graph
plot(net3, edge.arrow.size=.2,edge.curved=.1,edge.color=edge.col,vertex.label = V(net3)$label_names , vertex.size=V(net3)$weight/3,vertex.label.cex	=(V(net3)$fontsize/10)+0.4, vertex.label.dist=V(net3)$distance, vertex.label.degree = pi/2, vertex.label.adjust=c(0.5,0.5), vertex.color=V(net3)$color, vertex.frame.color =V(net3)$color,  asp=0.7, layout=fr, vertex.label.font=V(net3)$font, vertex.label.family="sans",  vertex.label.color=c(V(net3)$fontcolor), vertex.shape="circle", edge.width=edge.width/5, edge.curved=1) #displaying graph weighed for several things, and aspect ratio of 0.35

dev.off()



##################################


#####Interactive Plot ########

net3plot <- toVisNetworkData(net3) # convert the graph (or use visIgraph)

net3vis <- visNetwork(nodes = net3plot$nodes,
                      color = "orange", 
                      background = net3plot$nodes$color,
                      edges = net3plot$edges,
                      main = "Plasma Systematic Review",
                      submain = "Interactive Figure",
                      footer = "", 
                      font.size = net3plot$nodes$weight,  # Set the font size to nodeweight
                      label = net3plot$nodes$id
) 

net3vis <- net3vis %>%
  visIgraphLayout(layout = "layout_with_kk",
                  smooth = FALSE,
                  physics = TRUE
  ) 

net3vis


