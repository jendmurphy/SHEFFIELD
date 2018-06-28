#  Packages needed for social network analysis
install.packages(c("statnet","igraph","intergraph","blockmodels"))
require(statnet)
require(igraph)
require(intergraph)
require(blockmodels)

# Set working directory
setwd("//stfdata07/home/MI/Mip18jdm/ManW10/Desktop/SHEFFIELD/SHEFFIELD")

#Section 3
#Reading in sociomatrices
sociomatrix<-matrix(c(0,1,1,0), # list of values
                    2,2,# 2 by 2 matrix
                    byrow=T) # means to fill in the matrix row by row using the list


rownames(sociomatrix)<-colnames(sociomatrix)<-c("Ahmed","Sofia")  #  gives the columns names

#  Now make a bigger one
X<-matrix(c(0,1,0,0,1,0,0,0,1,1,0,1,0,1,1,0),4,4,T)  #  a 4 by 4 matrix
rownames(X)<-colnames(X)<-c("Ahmed","Sofia","Berthold","Carlo")

################################################
#Task: Summarise the above (second) sociomatrix)
################################################

#Size
size.dir.X <- sum(X)

#Density
dens.X <- sum(X)/(nrow(X)*(nrow(X)-1))

# Reciprocity
X.Xt <- X*t(X)  #  multiply matrix by its transpose to give matrix of reciprocated ties
X.recip <- sum(X.Xt)/2 # this is effectively an undirected network as all ties are reciprocated
X.notrecip <- sum(X-X.Xt) #  taking the difference
X.reciprocity <- X.notrecip/(X.notrecip + X.recip) # working out the ratio

# In degree and out degree
X.in <-colSums(X) # sum of the columns.  View this, gives the in degree for each actor
X.out <-rowSums(X) # sum of the rows.  View this, gives the out degree for each actor

# View in and out
X.in
X.out

#################################################
#END OF TASK
#################################################


# Each edge is a pair of nodes
# Reading in lists of edges
# this is computationally efficient for large networks as it doesnt store any zeros.
friend.edges<- rbind(c("Ahmed","Sofia"),  #  rbind takes the list of lists and puts them into rows
                     c("Sofia","Ahmed"),
                     c("Berthold","Ahmed"),
                     c("Berthold","Sofia"),
                     c("Berthold","Carlo"),
                     c("Carlo","Sofia"),
                     c("Carlo","Berthold"))
friend.edges<-data.frame(friend.edges)

#Creating a network object from a sociomatrix
library(statnet)
# Turn the matrix X into a network object for statnet
net1<-network(X,matrix.type="adjacency")

#Check summary of object
#assumes it is a directed matrix
summary(net1)

#Creating a network object from an edge list
net1.1<-network(friend.edges, matrix.type="edgelist")
#  This gives the same information as reading in X, just done from an edge list rather than an adjacency matrix.
#  Reading in an edge list might be easier, can still be converted into a network quite simply
summary(net1.1)

#Adding/changing node names
network.vertex.names(net1)<-c("Ahmed","Sofia","Berthold","Carlo")


#Getting an edge list from a network object
as.sociomatrix(net1)
as.matrix(net1, matrix.type="edgelist")

#Adding vertex information
set.vertex.attribute(net1, "gender",c("M","F","M","M")) # may need to specify that this command comes from network package because R is a bit confused
list.vertex.attributes(net1)  # may need to specify that this command comes from network package because R is a bit confused
summary(net1) # Now this lists out the 

require(network)
#Getting vertex information
get.vertex.attribute(net1,"gender")
get.vertex.attribute(net1,"vertex.names")
net1 %v% "gender"

#  Adding edge covariate
#  This is weighting the ties in the matrix
#  The weight is a separate matrix
w.mat<-matrix(c(0,3,0,0,2,0,0,0,1,1,0,3,0,2,4,0),4,4,T)
set.edge.value(net1,"weight",w.mat)

#Getting edge information
get.edge.attribute(net1,"weight")
get.edge.value(net1,"weight")
as.sociomatrix(net1,"weight")

summary(net1)

############################################################################################
#NOW LOOKING AT IGRAPH - NEED TO UNLIBRARY THE STATNET PACKAGES SO THAT IGRAPH CAN BE USED #
############################################################################################

#Detach statnet packages and load igraph
detach("package:statnet",unload=TRUE)
detach("package:sna", unload=TRUE)
detach("package:ergm.count", unload=TRUE)
detach("package:tergm", unload=TRUE)
detach("package:ergm", unload=TRUE)
detach("package:networkDynamic", unload=TRUE)
detach("package:network", unload=T)

library(igraph)
# Create an igraph object from the sociomatrix
# Putting i in front of the objects to distinguish between statnet and igraph objects
inet1<-graph_from_adjacency_matrix(sociomatrix)
summary(inet1) # gives some information but in a condensed format.


#Create an igraph object from the edges list
inet1.1<-graph_from_edgelist(as.matrix(friend.edges),directed = T)

#Add vertex covariate
V(inet1)$gender <- c("M","F","M","M")
summary(V(inet1)$gender)
summary(inet1)

#Adding a weight for strength of friendship
E(inet1)$strength <- c(3,3,1,2,2,1,4) # add weights rowise.  igraph knows to only add weights to existing ties.
#Looking at the strength covariate
summary(E(inet1)$strength)
summary(inet1)

#Converting between statnet and igraph objects using intergraph library
library(intergraph)
class(net1)
class(inet1)
inet1.2<-asIgraph(net1)
net1.2<-asNetwork(inet1)
class(inet1.2)
class(net1.2)
summary(inet1.2)
summary(net1.2)

#  Getting a subgraph using node characteristics
#  Can take as many subgraphs as you want to
library(statnet)
netM<-get.inducedSubgraph(net1,which(net1 %v% "gender" == "M")) # This is picking out the network just of men
summary(netM)


#Getting a subgraph using edge characteristics
#Summarising number of edges having each covariate value
table(net1 %e% "weight")
#If we were only interested in edges with weight greater than 2, say
#First we extract a matrix version of the edge covariate
w.val<-as.sociomatrix(net1,"weight")
#Replace the elements below the threshold with 0
w.val[w.val<2]<-0
#Create a new sociomatrix with ties corresponding to the new zeros filtered out
net.filt<-as.network(w.val,directed=TRUE,matrix.type="a",ignore.eval=FALSE,names.eval="weight")
summary(net.filt)

#Create a new network with an isolated node 
new.soc<-rbind(cbind(sociomatrix,rep(0,4)),rep(0,5))
rownames(new.soc)<-colnames(new.soc)<-c("Ahmed","Sofia","Berthold","Carlo","Sean")
net.new<-network(new.soc)
summary(net.new)

#Create a copy of the network and remove the isolate(s) in the copy
net.copy <- net.new
delete.vertices(net.copy,isolates(net.copy))
summary(net.copy)

#Change directed matrix to undirected matrix - this is making the network symmetric. 
net.symm.weak<-symmetrize(net1,rule="weak") # we can make a 10 -> 11  so even a weak tie becomes a full tie
net.symm.strong<-symmetrize(net1,rule="strong") # only includes reciprocated ties, all (1,0) ties become (0,0)

summary(net.symm.strong)
summary(net.symm.weak)

#Read in antibiotic data
#setwd("~/Desktop/NetworkCMU") set this to the directory the data file is in

##############################
#ANTIBIOTICS NETWORK TASK    #
##############################
data<-read.table("ckm_network.dat.txt")

# default is undirected so make sure you definitely specify false within the converison to a network object
datanet <- network(data,matrix.type="adjacency", directed = FALSE)
summary(datanet, print.adj=F) #print.adj = F stops it printing out the adjacency matrix as part of the summary

isolates(datanet)

degree(datanet)
?degree
 
# Plot a histogram of degree
hist(degree(datanet))
# Plot the network
plot(datanet)

#Number of ties
sum(data)/2

# Store degree data as an object
# Allows summary statistics
degree<- degree(datanet)
summary(degree)

################################
# Onto plotting network data   #
################################

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