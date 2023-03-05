library(sna)
library(network)
library(foreign)
library(igraph)
library(RColorBrewer)
install.packages("CINNA")
library(CINNA)
install.packages("corrplot")
library(corrplot)

###Cargamos las bases de HR Turnover, creamos matrices, grafos y redes
FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep = ";")
ProfMat<-read.csv("Profnet.csv",header=TRUE, sep = ";")
BossMat<-read.csv("BossNet.csv",header=TRUE, sep = ";")
SupportMat<-read.csv("SupportNet.csv",header=TRUE, sep = ";")

FriendMat<-as.matrix(FriendMat)
ProfMat<-as.matrix(ProfMat)
BossMat<-as.matrix(BossMat)
SupportMat<-as.matrix(SupportMat)

##Dicotomizamos fuerza de los lazos
Friend.any <- ifelse(FriendMat > 0, 1, 0)
Boss.any <- ifelse(BossMat > 0, 1, 0)
Prof.any <- ifelse(ProfMat > 0, 1, 0)
Support.any <- ifelse(SupportMat > 0, 1, 0)

suppressPackageStartupMessages(library(igraph))

##Creamos grafos
FriendGraph<-graph_from_adjacency_matrix(FriendMat, weighted=TRUE)
ProfGraph<-graph_from_adjacency_matrix(ProfMat, weighted=TRUE)
BossGraph<-graph_from_adjacency_matrix(BossMat, weighted=TRUE)
SupportGraph<-graph_from_adjacency_matrix(SupportMat, weighted=TRUE)

#Direccionamos los grafos
FriendGraph.any <-graph.adjacency(Friend.any,
                                  mode=c("directed"),
                                  weighted=NULL,
                                  diag=FALSE)
BossGraph.any <-graph.adjacency(Boss.any,
                                mode=c("directed"),
                                weighted=NULL,
                                diag=FALSE)
ProfGraph.any <-graph.adjacency(Prof.any,
                                mode=c("directed"),
                                weighted=NULL,
                                diag=FALSE)
SupportGraph.any <-graph.adjacency(Support.any,
                                   mode=c("directed"),
                                   weighted=NULL,
                                   diag=FALSE)

##Creamos redes
detach(package:igraph)
suppressPackageStartupMessages(library(igraph))
FriendNet.any<-as.network(Friend.any)
BossNet.any<-as.network(Boss.any)
ProfNet.any<-as.network(Prof.any)
SupportNet.any<-as.network(Support.any)

##Miremos nuestra red primero con sna
detach(package: igraph)
library(sna)
library(network)

#Empezamos con el número de nodos y el número de lazos
network.size(FriendNet.any)
network.edgecount(FriendNet.any)
network.edgecount(BossNet.any)
network.edgecount(ProfNet.any)
network.edgecount(SupportNet.any)

#Seguimos con densidad
network.density(FriendNet.any)
network.density(BossNet.any)
network.density(ProfNet.any)
network.density(SupportNet.any)

#Podemos sacar las correlaciones entre las redes (creamos matriz de correlaciones)
 #Calculamos las correlaciones individuales
FP<-gcor(FriendNet.any, ProfNet.any)
FB<-gcor(FriendNet.any, BossNet.any)
FS<-gcor(FriendNet.any, SupportNet.any)
PB<-gcor(ProfNet.any, BossNet.any)
PS<-gcor(ProfNet.any, SupportNet.any)
BS<-gcor(BossNet.any, SupportNet.any)

Matrix_Names <- c("FriendNet", "ProfNet", "BossNet", "SupportNet") #Geneamos un vector con los nombres
Corr_Vector <- c(1, FP, FB, FS, 1, PB, PS, 1, BS, 1) #Los 1s son porque la correlación de una red con sí misma es 1

 #Volvemos matriz nuestro vector
n <- length(Matrix_Names) ##How many cells will the matrix have?
Corr_Matrix <- diag(n) ##Create a diagonal matrix
dimnames(Corr_Matrix) <- list(Matrix_Names, Matrix_Names) #Nombres de filas y columnas

 #Create a lower diagnonal matrix from the vector of correlations
Corr_Matrix[lower.tri(Corr_Matrix, diag = TRUE)] <- Corr_Vector
 # Let's look at it:
Corr_Matrix
  
 #Podemos hacer la tabla más bonita
dim(Corr_Matrix)
Corr_Matrix<-as.matrix(Corr_Matrix)
row.names(Corr_Matrix)<-c("Friendship network",
                          "Professional network", "Boss-subordinate network", "Support network")
suppressPackageStartupMessages(library(knitr))
kable(Corr_Matrix, col.names=c("Friendship network",
                               "Professional network", "Boss-subordinate network", "Support network"),
      row.names=TRUE, caption="Network correlations of HR Turnover data")

##Dyads 
#Contamos dyads
network.dyadcount(FriendNet.any)
#Calculamos dyad census
dyad.census(FriendNet.any)
dyad.census(BossNet.any)
dyad.census(SupportNet.any)
dyad.census(ProfNet.any)
#Mutuality (solo lazos mutuos)
mutuality(FriendNet.any)
mutuality(ProfNet.any)
mutuality(BossNet.any)
mutuality(SupportNet.any)
#Reciprocity (proporción de dyads simétricos: mutual y nulls)
grecip(FriendNet.any)
grecip(ProfNet.any)
grecip(BossNet.any)
grecip(SupportNet.any)

##Triads
triad.census(FriendNet.any)
triad.census(ProfNet.any)
triad.census(BossNet.any)
triad.census(SupportNet.any)

##Medidas de conectividad
#Componentes
components(Friend.any, connected = "weak")
components(Friend.any, connected = "strong")
#Conectedness
connectedness(FriendNet.any)
#Cutpoints
cutpoints(FriendNet.any, mode = "digraph", connected = c("strong"),
          return.indicator = FALSE)
#Distancias geodésicas
geo.dist.Friend<-geodist(FriendNet.any)
class(geo.dist.Friend) # check the structure
#Summary de distancias geodésicas para el nodo 3
summary(geo.dist.Friend$counts[,3]) #Distancias geodésicas que van AL nodo 3 (columnas)
summary(geo.dist.Friend$counts[3,]) #Distancias geodésicas que van DESDE el nodo 3 (filas)

#Cliques más grandes (con paquete igraph). Clique: estructura donde todos están conectados entre todos
detach(package:sna)
suppressPackageStartupMessages(library(igraph))
largest_cliques(FriendGraph.any)
average

###Medidas de centralidad (análisis local)
## Indegree centrality
indegree_Friend <- degree(FriendGraph.any, mode='in')
head(indegree_Friend) # first six values
## Outdegree centrality measures outgoing ties
outdegree_Friend <- degree(FriendGraph.any, mode='out')
head(outdegree_Friend) # first six values
## Total degree centrality (in+out)
total_degree_Friend <- degree(FriendGraph.any, mode='total')
head(total_degree_Friend) # first six values
##Closeness centrality
No sirve para red con componentes desconectados, debemos calcularla entonces para cada componente
##Betweeness
betweenness_Friend <- betweenness(FriendGraph.any)
head(betweenness_Friend)
##Eigenvector
evcent_Friend <- evcent(FriendGraph.any)
eigen_Friend <- evcent_Friend$vector
head(eigen_Friend)
##Podemos dibujar la degree centrality
barplot(indegree_Friend, names.arg = V(FriendGraph.any)$name)
##Podemos hacer tabla con todas las medidas
central_Friend <- data.frame(indegree_Friend, outdegree_Friend, betweenness_Friend, eigen_Friend)
central_Friend<-as.matrix(central_Friend)
head(central_Friend)
library(knitr)
kable(head(central_Friend), col.names=c("Indegree Centrality",
                                        "Outdegree Centrality", "Betweenness Centrality", "Eigenvector Centrality"),
      caption="Friendship Network Centralities")
##Podemos sacar las correlaciones de las medidas (hacemos matriz de correlaciones)
cent_cor <- cor(central_Friend)
row.names(cent_cor)<-c("Indegree Centrality",
                       "Outdegree Centrality", "Betweenness Centrality", "Eigenvector Centrality")
kable(cent_cor, col.names=c("Indegree Centrality",
                            "Outdegree Centrality", "Betweenness Centrality", "Eigenvector Centrality"),row.names=TRUE,
      caption="Friendship network centrality correlations")
##Calculamos con CINNA (puede calcular cientos de distintas centralidades)
#Primero separamos la red en componentes
comps<-graph_extract_components(FriendGraph.any)
#How many components do we have?
length(comps)
#Veamos los componentes
comps
#Analizamos la centralidad del primer componente (el más grande)
Comp1 <- comps[[1]]
plot(Comp1)
pr_cent<-proper_centralities(Comp1) #Nos permite ver todas las centralidades que pueden ser calculadas en este componente
 #Vemos que son varias medidas, calculemos algunas (rank, eccentricity)
NewCent_Friend<-calculate_centralities(Comp1, include = c(pr_cent[3], pr_cent[14],pr_cent[36]))
NewCent_Friend<-data.frame(NewCent_Friend, stringsAsFactors = FALSE)
kable(head(NewCent_Friend), col.names=c("Page Rank", "Eccentricity", "Leverage"),
      caption="Additional Centralities of the Friendship Network")

