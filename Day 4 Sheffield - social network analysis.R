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
E(inet1)$strength <- c(3,3,1,2,2,1,4)
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

#Getting a subgraph using node characteristics
library(statnet)
netM<-get.inducedSubgraph(net1,which(net1 %v% "gender" == "M"))
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

#Change directed matrix to undirected matrix
net.symm.weak<-symmetrize(net1,rule="weak")
net.symm.strong<-symmetrize(net1,rule="strong")

#Read in antibiotic data
#setwd("~/Desktop/NetworkCMU") set this to the directory the data file is in
data<-read.table("ckm_network.dat.txt")
