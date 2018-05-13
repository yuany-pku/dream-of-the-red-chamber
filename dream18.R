library(Libra)
library(igraph)

load("dream.RData")
# Choose the first 80 chapters authored by Cao, Xueqin
data<-dream[dream[,1]>0,]
dim(data)
s0<-colSums(data)
# restrict to the most important characters 
data1<-data[,s0>=30]
#Eng_names <- c('Jia, Zheng','Jia, Zhen','Jia, Lian','Jia, Baoyu','Jia, Tanchun','Jia, Rong','Lady Dowager','Shi, Xiangyun','Lady Wang','Wang, Xifeng','Aunt Xue','Xue, Baochai','Lin, Daiyu','Lady Xing','Madam You','Li, Wan','Xiren','Ping\'er')
p = dim(data1)[2];
X<-as.matrix(2*as.matrix(data1[,2:p])-1);
obj = ising(X,10,0.1,nt=1000,trate=100)
sparsity=NULL
for (i in 1:1000) {sparsity[i]<-(sum(abs(obj$path[,,i])>1e-10))/(p^2-p) }

# Choose sparsity=20% at point 373
g<-graph.adjacency(obj$path[,,373],mode="undirected",weighted=TRUE)	
E(g)[E(g)$weight<0]$color<-"red"
E(g)[E(g)$weight>0]$color<-"green"
V(g)$name<-attributes(data1)$names[2:p]
plot(g,vertex.shape="rectangle",vertex.size=25,vertex.label=V(g)$name,edge.width=2*abs(E(g)$weight),vertex.label.family='STKaiti',main="Ising Model (LB): sparsity=20%")

# Choose the later 40 chapters authored by Gao, E
data<-dream[dream[,1]<1,]
data2<-data[,s0>=30]
X<-as.matrix(2*as.matrix(data2[,2:p])-1);
obj = ising(X,10,0.1,nt=1000,trate=100)
sparsity=NULL
for (i in 1:1000) {sparsity[i]<-(sum(abs(obj$path[,,i])>1e-10))/(p^2-p) }

# Choose sparsity=20% at point 344.
g<-graph.adjacency(obj$path[,,344],mode="undirected",weighted=TRUE)	
E(g)[E(g)$weight<0]$color<-"red"
E(g)[E(g)$weight>0]$color<-"green"
V(g)$name<-attributes(data2)$names[2:p]
plot(g,vertex.shape="rectangle",vertex.size=25,vertex.label=V(g)$name,edge.width=2*abs(E(g)$weight),vertex.label.family='STKaiti',main="Ising Model (LB): sparsity=20%")