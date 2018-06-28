#New friendship matrix visualized with different colours for gender
sociomatrix<-matrix(c(0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,0,0,0,1,0),5,5,T)
rownames(sociomatrix)<-colnames(sociomatrix)<-c("Ahmed","Sofia","Berthold","Carlo","Niamh")
net3<-network(sociomatrix,matrix.type="adjacency")
plot(net3,vertex.col=c(4,2,4,4,2),vertex.cex=3,label=network.vertex.names(net3),arrowhead.cex=3,edge.lwd=2)

plot(net3,vertex.col=c(4,2,4,4,2),vertex.cex=3,label=network.vertex.names(net3),arrowhead.cex=3,edge.lwd=2,mode="circle")

#Similar to before but using gplot command instead
gplot(net3,vertex.col=c(4,2,4,4,2),vertex.cex=3,label=network.vertex.names(net3),arrowhead.cex=1.5,edge.lwd=2, mode="spring")

#Plotting with different shapes instead of colours
plot(net3,vertex.sides=c(3,50,3,3,50),vertex.cex=3,label=network.vertex.names(net3),arrowhead.cex=3,edge.lwd=2)

#Change to igraph object to use tkplot command
library(intergraph)
inet3<-asIgraph(net3)
library(igraph)
coord<-tkplot(inet3, vertex.size=3, vertex.label=c(4,2,4,4,2))
MCoords <- tkplot.getcoords(coord)
plot(inet3, layout=MCoords, vertex.size=5, vertex.label=NA, vertex.color="lightblue")

#Graph summaries
#Density
network.density(net3)

#Reciprocity
grecip(net3, measure = "dyadic.nonnull")
grecip(net3, measure = "edgewise")

#Number of connected components
components(net3,connected="strong")
components(net3,connected="weak")

#Diameter
#For the directed graph
max(geodist(net3)$gdist)
#For a symmetrized version of the directed graph, i.e. undirected
max(geodist(symmetrize(net3))$gdist)

#Transitivity
gtrans(net3, mode="weak")
gtrans(net3, mode="strong")

#Cliques
#To find the largest size of clique
clique.number(asIgraph(net3))
#To identify cliques of size between 2 and 3
cliques(asIgraph(net3),min=2,max=3)

#k-Cores
graph.coreness(asIgraph(net3))

#Modularity
net3%v%"vertex.names"
gender<-c("M","F","M","M","F")
inet3<-asIgraph(net3)
modularity(inet3,as.factor(gender))

#Community detection 
#setwd() Set the working directory to the folder where the ckm_network.dat.txt file is stored
data<-read.table("ckm_network.dat.txt")
anti.net<-network(data, directed = F)
anti.inet<-asIgraph(anti.net)
#anti.inet<-graph_from_adjacency_matrix(as.matrix(data))

cw<-cluster_walktrap(anti.inet) 
head(membership(cw))
table(membership(cw))
modularity(cw)
plot(cw, anti.inet)

cb<-cluster_edge_betweenness(anti.inet)
table(membership(cb))
modularity(cb)

chisq.test(table(membership(cw),membership(cb)))
compare(cw,cb,method="adjusted.rand")

#Degree calculation
#Reminding ourselves of the node names in the net3 network
net3 %v% "vertex.names"
#Indegree
degree(net3,cmode="indegree")
#Outdegree
degree(net3,cmode="outdegree")
#Total degree
degree(net3,cmode="freeman")

#Closeness centrality
closeness(net3,gmode="graph")

#Betweenness centrality
betweenness(net3, cmode="directed", rescale=1)

#Eigenvector centrality
round(evcent(net3),2)

#Plotting centrality
deg<-degree(net3)
gplot(net3, vertex.cex=deg, vertex.col=c(4,2,4,4,2),label = network.vertex.names(net3))
gplot(net3, vertex.cex=sqrt(deg), vertex.col=c(4,2,4,4,2),label = network.vertex.names(net3))
gplot.target(net3, betweenness(net3), vertex.col=c(4,2,4,4,2), circ.rad=c(1:4)/4,label = network.vertex.names(net3))

#For example the following gives us a decreasing list of outdegrees of 
#nodes in the first vector and an index list in the second vectors
sort(degree(net3,cmode="outdegree"),decreasing = T, index.return=T)
(net3%v%"vertex.names")[sort(degree(net3,cmode="outdegree"),decreasing = T, index.return=T)$ix]

#Cutpoints
cutpoints(net3,connected="weak")
cutpoints(net3,connected="strong")

#Bridge identification function
bridges <- function(dat,mode="graph", connected=c("strong", "weak")) 
  {
  e_cnt <- network.edgecount(dat) 
  if(mode == "graph") {
    cmp_cnt <- components(dat) 
    b_vec <- rep(FALSE,e_cnt) 
    for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2) != cmp_cnt) }
  }
  else {
    cmp_cnt <- components(dat,connected=connected) 
    b_vec <- rep(FALSE,e_cnt)
    for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2,connected=connected)
                   != cmp_cnt) }
  }
  return(b_vec) }

id.brid<-bridges(net3)
#Use green to identify bridge edges and red otherwise
gplot(net3,edge.col=id.brid+2, displaylabels = T)

#Assortativity
#For categorical variable, gender
net3%v%"vertex.names"
gender<-c("M","F","M","M","F")
assortativity_nominal(asIgraph(net3),as.factor(gender))
#For continuous variable height
height<-c(1.5,1.4,1.7,1.4,1.3)
assortativity(asIgraph(net3),height)
