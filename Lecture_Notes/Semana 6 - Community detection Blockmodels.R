library(sna)
library(network)
library(foreign)
library(igraph)
library(intergraph)
install.packages("NetCluster")
library(NetCluster)
install.packages("dendextend")
library(dendextend)

#Cargamos data y atributos
FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep = ";")
FriendMat<-as.matrix(FriendMat)
suppressPackageStartupMessages(library(igraph))
FriendGraph<-graph_from_adjacency_matrix(FriendMat, weighted=TRUE)
HR_att<-read.csv("HR_Imputed.csv", header=TRUE)
roles<-HR_att$Position
names<-HR_att$Name
roles_vector<-vector()

#Eliminamos nodos aislados (no tienen nada de interesante a pesar de ser estructuralmente equivalentes)
FriendGraph <- delete.vertices(FriendGraph, which(degree(FriendGraph)==0))
gorder(FriendGraph) #Vemos acá los nodos que quedan

#Asignamos atributos a los nodos
for(i in 1:114){ # this is our set of all network nodes
  for(j in 1:122){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(FriendGraph)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      roles_vector[i]<-roles[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{roles_vector[i]<-99
    }
  }
}
detach(package:igraph)
length(roles_vector)
table(roles_vector)
suppressPackageStartupMessages(library(knitr))
kable(table(roles_vector), col.names=c("Roles","Frequencies"),
      caption="Frequency distribution of actors' roles")

#Convertimos el grafo a red
suppressPackageStartupMessages(library(intergraph))
FriendNet<-asNetwork(FriendGraph)
suppressPackageStartupMessages(library(sna))
network.size(FriendNet)

#Simetrizamos la red para que no sea dirigida (es importante para encontrar patrones)
 # Create the rules
orRule <- symmetrize(FriendNet, rule='weak') # "or" rule
class(orRule) # symmetrize transformed the network into a matrix
orRule <- network(symmetrize(FriendNet, rule='weak'),
                  directed = FALSE) # 'or' rule
class(orRule) # network
andRule <- network(symmetrize(FriendNet, rule='strong'),
                   directed = FALSE) # 'and' rule
 #Para elegir cuál regla usar, vemos los resultados en la red
n<-network.size(FriendNet)
v1<-sample((0:(n-1))/n) #create a vector of random numbers
v2<-sample(v1)
x <- n/(2 * pi) * sin(2 * pi * v1)
y <- n/(2 * pi) * cos(2 * pi * v2)
mycoord <- cbind(x,y)
par(mar=c(1,1,2,1))
par(mfrow=c(2,2))
plot(FriendNet, main = 'Original Friendship Network', coord=mycoord, vertex.cex =3,
     edge.col='azure4', vertex.col="#E41A1C", vertex.border='azure4',
     label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')
plot(orRule, main = 'FriendNet Symmetrized with OR Rule', coord=mycoord, vertex.cex =3,
     edge.col='azure4', vertex.col="#377EB8", vertex.border='azure4',
     label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')
plot(andRule, main = 'FriendNet Symmetrized with AND Rule', coord=mycoord, vertex.cex =3,
     edge.col='azure4', vertex.col="#4DAF4A", vertex.border='azure4',
     label=seq(1:20),label.pos=5,label.cex=.5,label.col='gray15')
Nos quedaremos con el orRule, ya que con el AND se pierden muchos lazos

 #Antes de construir el modelo, coloreemos la red según los roles
library(RColorBrewer)
par(mar=c(1,1,1,1),mfrow=c(2,3))
col11 <- brewer.pal(11, 'Set3')
cols<-vector()
for(i in 1:114) {
  for(j in 1:9) {
    if (roles[i]==j) {
      cols[i]<-col11[j]
      break;}
    else{cols[i]<-"white"}
  }
}

##A priori blockmodel (blockmodel basado en roles)
roles<-roles_vector #pass the roles
Friend_orRule<-orRule

# Build the "a priori" model
suppressPackageStartupMessages(library(network))
Friend_apriori<-blockmodel(Friend_orRule, roles,
                           block.content="density", mode="graph",
                           diag=FALSE)
par(mar=c(0,0,0,0),mfrow=c(1,1))
plot(Friend_orRule,
     vertex.cex =3, edge.col='azure4', vertex.col=cols,
     vertex.border='azure4', label=seq(1:20), label.pos=5,
     label.cex=.5, label.col='gray15')
#Construimos el heatmap
heatmap(Friend_apriori[[4]])
Friend_apriori$block.membership
Friend_apriori[[1]]
Friend_apriori[[2]]
Friend_apriori[[3]]
Friend_apriori[[4]]

##Exploratory community detection
#Creamos ahora un blockmodel basado en clusterización jerárquica
Friend_dist <- dist(Friend_orRule, method="euclidian", diag=FALSE)
thick <- as.vector(Friend_dist)
#Plot:
par(mar=c(0,0,0,0))
plot(Friend_orRule,
     vertex.cex =3, edge.col='azure4', vertex.col=col11[5],
     vertex.border='azure4', label=seq(1:20),label.pos=5,
     label.cex=.5,label.col='gray15', edge.lwd = thick^2)
Gráfico nos muestra que las distancias en el cluster más pequeño son menores pues tienen lazos más delgados

Friend_clust <- hclust(Friend_dist, method = "complete")
Friend_explore <- blockmodel(Friend_orRule, Friend_clust, k=6,
                             block.content = "density", mode = "graph", diag = FALSE)
 
 #Comparamos nuestro blockmodel exploratorio con el apriori
par(mar=c(0,0,2,0))
plot.blockmodel(Friend_apriori)
plot.blockmodel(Friend_explore)
heatmap(Friend_apriori[[4]], main = "Apriori blockmodel")
heatmap(Friend_explore[[4]], main = "Exploratory blockmodel")
Ver comparación de heatmaps en pdf

##Otras formas de construir blockmodels
#Comparamos lazos de amistad y profesionales
 #Cargamos la data y dicotomizamos
FriendMatFull<-read.csv("FriendFull.csv",header=TRUE, row.names=1)
FriendMat<-as.matrix(FriendMatFull)
FriendMat_binary <- ifelse(FriendMatFull > 0, 1, 0)
ProfMatfull<-read.csv("ProfFull.csv",header=TRUE, row.names=1)
ProfMat<-as.matrix(ProfMatfull)
ProfMat_binary <- ifelse(ProfMatfull > 0, 1, 0)

Friend_full<-rbind(FriendMatFull,t(FriendMatFull))
Prof_full<-rbind(ProfMatfull,t(ProfMatfull))
Friend_Prof_Matrix<-rbind(Friend_full,Prof_full) #Hemos juntado las matrices, puesto una encima de otra
dim(Friend_Prof_Matrix)

suppressPackageStartupMessages(library(NetCluster))
suppressPackageStartupMessages(library(dendextend))

 #Hacemos un cluster jerárquico de esta red enorme y graficamos dendogramas
Matrix_dist<-dist(t(Friend_Prof_Matrix))
Full_Cluster <- hclust(Matrix_dist)
dend <- as.dendrogram(Full_Cluster)
par(mar=c(0,0,0,0))
dend %>%
  # Custom branches
  set("branches_col", "blue") %>% set("branches_lwd", 2) %>%
  # Custom labels
  set("labels_col", "black") %>% set("labels_cex", 0.3) %>%
  plot()
par(mar=c(0,0,0,0)) #Volteamos gráfico y añadimos color
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey", "red","green"), k=5) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey", "red","green"), k = 5) %>%
  set("labels_col", "blue") %>% set("labels_cex", 0.5) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)
Este es un cluster en el que hemos analizado todas las relaciones: los lazos de amistad y profesionales
formados por ese nodo y que van hacia ese nodo
Vemos que la red se puede partir en 5 clusters distintos
cutree(Full_Cluster,k=5) #Con esta función podemos ver qué nodo pertenece a cada cluster
num_clusters = 5
clusters <- cutree(Full_Cluster, k=num_clusters)
clusters

#Blockmodels on valued data
 # Blockmodel on valued Friend data
Friend_valued_blockmodel <- blockmodel(FriendMat, clusters)
 # Blockmodel on binary Friend data
Friend_binary_blockmodel <- blockmodel(FriendMat_binary, clusters)
 # Blockmodel on valued Prof data
Prof_valued_blockmodel <- blockmodel(ProfMat, clusters)
 # Blockmodel on binary Prof data
Prof_binary_blockmodel <- blockmodel(ProfMat_binary, clusters)
 #Comparamos una valued con una binaria
par(mar=c(0,0,2,0))
heatmap(Friend_valued_blockmodel[[4]], main = "Valued")
heatmap(Friend_binary_blockmodel[[4]], main = "Binary")
