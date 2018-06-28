#Comparing doctor network to simulated networks
library(igraph)
#setwd("~/Desktop/NetworkCMU") Set this to the directory where the data file is
data<-read.table("ckm_network.dat.txt")
anti.net<-network(data, directed = F)
anti.inet<-asIgraph(anti.net)
anti.inet<-graph_from_adjacency_matrix(as.matrix(data),mode="undirected")
summary(anti.inet)
gr.density<-edge_density(anti.inet)
deg.dist<-igraph::degree(anti.inet)
ave.deg.anti<-mean(deg.dist)

anti.erdos<-erdos.renyi.game(246,gr.density,type="gnp")
anti.small<-watts.strogatz.game(dim=1, size=246, nei=2, p=0.25 )
anti.scale.free<-barabasi.game(246, out.dist=c(0.15,0.6,0.25),directed=F,zero.appeal = 2)


#Read in the Florentine data
data(florentine)
#Look at the summary
summary(flomarriage)
gplot(flomarriage,displaylabels = T, gmode = "graph")

#Fit the null model
library(ergm)
data(florentine)
iflomarriage<-asIgraph(flomarriage)

flo.mod.0<-ergm(flomarriage ~ edges, control=control.ergm(seed=101))
class(flo.mod.0)
summary(flo.mod.0)
plogis(flo.mod.0$coef)

#Examine wealth versus degree 
deg<-sna::degree(flomarriage,gmode="graph")
wealth<-get.vertex.attribute(flomarriage, "wealth")
cor(deg,wealth)
scatter.smooth(wealth,deg,xlab="Family Wealth", ylab="Degree")

#Model including node covariate wealth
flo.mod.1<-ergm(flomarriage~edges + nodecov('wealth'), control=control.ergm(seed=101))
summary(flo.mod.1)

#Predict tie probability for two families with wealth 10 and 100
edge.coef<-coef(flo.mod.1)[1]
wealth.coef<-coef(flo.mod.1)[2]
plogis(edge.coef + 10*wealth.coef + 100*wealth.coef)

#Look at friendship network and gender
sociomatrix<-matrix(c(0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,0,0,0,1,0),5,5,T)
rownames(sociomatrix)<-colnames(sociomatrix)<-c("Ahmed","Sofia","Berthold","Carlo","Niamh")
net3<-network(sociomatrix,matrix.type="adjacency")
inter<-asIgraph(net3)
net3%v%"vertex.names"
gender<-c("M","F","M","M","F")
deg<-sna::degree(net3)
mean(deg[gender=="M"])
mean(deg[gender=="F"])

#Friendship ergms
network::set.vertex.attribute(net3, "gender",gender)
net3.0<-ergm(net3~edges,control=control.ergm(seed=404))
net3.1<-ergm(net3~edges+nodefactor('gender'),control=control.ergm(seed=404))
summary(net3.0)
summary(net3.1)

mixingmatrix(net3,'gender')

#Fitting gender as a dyadic covariate
net3.2<-ergm(net3~edges+nodematch("gender"),control=control.ergm(seed=404))
summary(net3.2)

net3.3<-ergm(net3~edges+nodematch("gender",diff=TRUE),control=control.ergm(seed=404))
#summary(net3.3)

net3.4<-ergm(net3~edges+nodemix("gender",base=1),control=control.ergm(seed=404))
summary(net3.4)        

#Including edge covariates in the friendship example
w.mat<-matrix(c(0,3,0,0,0,2,0,0,0,0,1,1,0,3,0,0,2,4,0,0,0,0,0,2,0),5,5,T)
set.edge.value(net3,"weight",w.mat)
net3.5<-ergm(net3~edges+edgecov(net3,"weight"))
summary(net3.5)

#Returning to florentine marriage network and using dyadic continous covariates
flo.mod.4<-ergm(flomarriage~edges+nodecov("priorates")+absdiff("wealth"),control=control.ergm(seed=101))
summary(flo.mod.4)
flo.mod.5<-ergm(flomarriage~edges+absdiff("wealth"),control=control.ergm(seed=101))
summary(flo.mod.5)

#Including structural covariates
#Triangles
flo.mod.6<-ergm(flomarriage~edges+triangles,control=control.ergm(seed=404))
summary(flo.mod.6)

coef1 = flo.mod.6$coef[1]
coef2 = flo.mod.6$coef[2]
logodds = coef1 + c(0,1,2) * coef2
plogis(logodds)

#Mutual
net3.6<-ergm(net3~edges+mutual,control=control.ergm(seed=101))
summary(net3.6)
coef(net3.6)
sum(coef(net3.6))
plogis(coef(net3.6))
plogis(sum(coef(net3.6)))

#gwesp
flo.mod.7<-ergm(flomarriage~edges+gwesp(0.7, fixed=T)+absdiff("wealth"),control=control.ergm(seed=101))
summary(flo.mod.7)

#Goodness of fit
gof.flo.mod.5<-gof(flo.mod.5)
gof.flo.mod.5
par(mfrow=c(3,1),cex=0.75, mar=c(4,4,1,1), mgp=c(2,0.5,0), bty="L")
plot(gof.flo.mod.5)

mcmc.diagnostics(flo.mod.7)

#Stochastic block models
library(mixer)
setSeed(202)
flo.sbm<-mixer(as.matrix(as.sociomatrix(flomarriage)),qmin=2,qmax=4)
flo.sbm
flo.sbm.output<-getModel(flo.sbm)
#Number of classes, q
flo.sbm.output$q
#Proportion/probability of nodes belonging to each class
flo.sbm.output$alphas
#Assign each node to their class of maximum probability
apply(flo.sbm.output$Taus,2,which.max)
#Show which family belonging to which class
rbind(flomarriage%v%"vertex.names",apply(flo.sbm.output$Taus,2,which.max))
#Connectivity matrix of classes
round(flo.sbm.output$Pis,2)
plot(flo.sbm)

library(blockmodels)
library(statnet)
data(florentine)
flo_matrix<-as.sociomatrix(flobusiness)
block_flo<-BM_bernoulli("SBM",flo_matrix)
block_flo$estimate()
which.max(block_flo$ICL)

#Plotting the observed and predicted matrices
block_flo
block_flo$plot_obs_pred(2)

#Estimated model parameters for 2 blocks
block_flo$model_parameters[[2]]

#Check the membership probabilities (attach the node names)
memb.probs<-block_flo$memberships[[2]]$Z
rownames(memb.probs)<-get.vertex.attribute(flobusiness,"vertex.names")
memb.probs
round(memb.probs)[,1]

block_flo$memberships[[2]]$plot()

#Plot the observed and predicted matrices under the 2 block estimated model
block_flo$plot_obs_pred(2)

#Plot the model parameters
block_flo$plot_parameters(2)
