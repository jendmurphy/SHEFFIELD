#Section 3
#Reading in sociomatrices
sociomatrix<-matrix(c(0,1,1,0),2,2,byrow=T)
rownames(sociomatrix)<-colnames(sociomatrix)<-c("Ahmed","Sofia")

sociomatrix<-matrix(c(0,1,0,0,1,0,0,0,1,1,0,1,0,1,1,0),4,4,T)
rownames(sociomatrix)<-colnames(sociomatrix)<-c("Ahmed","Sofia","Berthold","Carlo")

################################################
#Task: Summarise the above (second) sociomatrix)
################################################

#Reading in lists of edges
friend.edges<- rbind(c("Ahmed","Sofia"),
                     c("Sofia","Ahmed"),
                     c("Berthold","Ahmed"),
                     c("Berthold","Sofia"),
                     c("Berthold","Carlo"),
                     c("Carlo","Sofia"),
                     c("Carlo","Berthold"))
friend.edges<-data.frame(friend.edges)

#Creating a network object from a sociomatrix
library(statnet)
sociomatrix<-matrix(c(0,1,0,0,1,0,0,0,1,1,0,1,0,1,1,0),4,4,T)
rownames(sociomatrix)<-colnames(sociomatrix)<-c("Ahmed","Sofia","Berthold","Carlo")
net1<-network(sociomatrix,matrix.type="adjacency")

#Check summary of object
summary(net1)

#Creating a network object from an edge list
net1.1<-network(friend.edges, matrix.type="edgelist")
summary(net1.1)

#Adding/changing node names
#network.vertex.names(net1)<-c("Ahmed","Sofia","Berthold","Carlo")

#Getting an edge list from a network object
as.sociomatrix(net1)
as.matrix(net1, matrix.type="edgelist")

#Adding vertex information
set.vertex.attribute(net1, "gender",c("M","F","M","M"))
list.vertex.attributes(net1)
summary(net1)

#Getting vertex information
get.vertex.attribute(net1,"gender")
get.vertex.attribute(net1,"vertex.names")
net1 %v% "gender"

#Adding edge covariate
w.mat<-matrix(c(0,3,0,0,2,0,0,0,1,1,0,3,0,2,4,0),4,4,T)
set.edge.value(net1,"weight",w.mat)

#Getting edge information
get.edge.attribute(net1,"weight")
get.edge.value(net1,"weight")
as.sociomatrix(net1,"weight")

summary(net1)

#Detach statnet packages and load igraph
detach("package:statnet",unload=TRUE)
detach("package:sna", unload=TRUE)
detach("package:ergm.count", unload=TRUE)
detach("package:tergm", unload=TRUE)
detach("package:ergm", unload=TRUE)
detach("package:networkDynamic", unload=TRUE)
detach("package:network", unload=T)

library(igraph)
#Create an igraph object from the sociomatrix
inet1<-graph_from_adjacency_matrix(sociomatrix)
summary(inet1)

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
