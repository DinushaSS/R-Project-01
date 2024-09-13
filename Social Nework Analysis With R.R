## Social Network Analysis

install.packages("igraph")

library(igraph)

g <- graph(c(1,2,2,3,3,4,4,1),directed=F,n=7)
g

plot(g,vertex.color="green",vertex.size=20,edge.color="red")
g[]

#ex:person 1 send to email person 2

g1 <- graph(c("Amy","Ram","Ram","Li","Li","Amy","Amy","Li","Kate","Li"),
            directed=T)
g1

plot(g1,vertex.color="green",vertex.size=20,edge.color="red")

#Network Measures

degree(g1,mode="all")
degree(g1,mode="in")
degree(g1,mode="out")

diameter(g1,directed="F",weights=NA)

edge_density(g1,loops=F)

ecount(g1)/(vcount(g1)*(vcount(g1)-1))

reciprocity(g1)	#Only for directed graph

closeness(g1,mode="all",weights=NA)

betweenness(g1,directed=T,weights=NA)

edge_betweenness(g1,directed=T,weights=NA)

#Read Data File

#data <- read.table("F:/R/R Project/networkdata.csv",sep=",",header=T)
#data

data <- read.csv(file.choose(),header=T)
data

#ex:
#Twitter
#1st column - ID of a twitter acccount
#2nd column - ID of a person who followed by a first column
#Facebook
#AA is a friend of DD
#AB is a friend of DD
#E-Mail
#AA send email to DD
#AB send email to DD

#attach(data)
#y <- data.frame(first,second)
#y

y <- data.frame(data$first,data$second)
y

#Create Network

net <- graph.data.frame(y,directed=T)
V(net)
E(net)

V(net)$label <- V(net)$name
V(net)$label
V(net)$degree <- degree(net)
V(net)$degree

#Histogram of Node diagram

hist(V(net)$degree,main="Histogram of Node Degree",col="green",
     ylab="Frequency",xlab="Degree of vertices") 

#Network diagram

set.seed(222)
plot(net,
     vertex.color="green",vertext.size=2,edge.arrow.size=0.3,
     vertex.label.cex=0.8)

#Highlighting degrees and layout

plot(net,
     vertex.color=rainbow(52),vertex.size=V(net)$degree*0.4,
     edge.arrow.size=0.1,layout=layout.fruchterman.reingold)

plot(net,
     vertex.color=rainbow(52),vertex.size=V(net)$degree*0.4,
     edge.arrow.size=0.1,layout=layout.graphopt)

plot(net,
     vertex.color=rainbow(52),vertex.size=V(net)$degree*0.4,
     edge.arrow.size=0.1,layout=layout.kamada.kawai)

#Hub & authorities

#Hub - Outgoing leads
hs <- hub_score(net)$vector
hs

#Authorities - Incoming leads
as <- authority.score(net)$vector
as

par(mfrow=c(1,2))
plot(net,vertex.size=hs*30,main="Hubs",vertex.color=rainbow(52),
     edge.arrow.size=0.1,layout=layout.kamada.kawai)
plot(net,vertex.size=as*30,main="Authorities",vertex.color=rainbow(52),
     edge.arrow.size=0.1,layout=layout.kamada.kawai)

#Comunity Detection

net <- graph.data.frame(y,directed=F)
net
cnet <- cluster_edge_betweenness(net)
cnet

plot(cnet,net,vertex.size=10,vertex.label.cex=0.8)

modularity(cnet)
