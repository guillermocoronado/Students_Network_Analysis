library(sna)
library(network)
library(foreign)
library(igraph)
library(RColorBrewer)
install.packages("graph")
library(graph)

#Generamos una matriz aleatoria
num_nodes <- 15
my_matrix<-matrix(round(runif(num_nodes*num_nodes)), #Vemos que es matriz de adyacencia (cuadrada)
                               nrow = num_nodes,     #el runif es para generar números aleatorios
                               ncol = num_nodes)
my_matrix

#Veamos que no hayan self-refering loops (que el nodo esté concectado a sí mismo)
diag(my_matrix) <- 0

# We can check dimensions of the new object:
dim(my_matrix)
class(my_matrix)

# Let's check whether any data is missing:
sum(is.na(my_matrix))

#Construimos la red
my_network<-as.network(x = my_matrix, # the network object
                       directed = TRUE, # specify whether the network is directed
                       loops = FALSE, # do we allow self ties (should not allow them)
                       matrix.type = "adjacency")# the type of input
plot(my_network)

#Vamos a hacer que no se vea tan pequeña
par(mar=c(0,0,1,0))
plot(my_network)
summary(my_network)

###Analicemos una red que venga de otro archivo
drugpaj <- read.paj('drugnet2.paj')
names(drugpaj) # objects in drugpaj
names(drugpaj$networks)
names(drugpaj$partitions)
drug <- drugpaj$networks[[1]] # extract network
class(drug)
plot(drug)

#Extraemos los atributos
names(drugpaj$partitions) # attributes included
gender <- drugpaj$partitions[[1]] # extract gender
gender #check the values assigned to each gender
table(gender)
suppressPackageStartupMessages(library(knitr))
kable(table(gender), col.names=c("Gender","Frequency")) # how many women/men

#It is actually better to recode gender into 1 and 0 as opposed to 2 and 1; setting the 0/1 variable as an attribute is cleaner that way.
female <- ifelse(gender == 2, 1, 
                 ifelse(gender == 1, 0, NA))
## set attributes
drug <-set.vertex.attribute(drug, 'female', value=c(female))
ethnicity <- drugpaj$partitions[[2]] # extract ethnicity
kable(table(ethnicity), col.names=c("Ethnicity","Frequency"))

###Cargamos data nativa de R (en formato R)
load('flo.Rdata')
flo.marriage <- as.network(as.matrix(flo.marriage), directed = FALSE)
flo.biz <- as.network(as.matrix(flo.biz), directed= FALSE)
plot(flo.marriage)
plot(flo.biz)
#Añadimos atributos
set.vertex.attribute(flo.marriage, 'wealth', flo.att[,2])
set.vertex.attribute(flo.biz, 'wealth', flo.biz [,2]) #Necesitamos todas las filas pero solo la segunda columna (wealth)
#Simple plots
par(mar=c(1,1,1,1), mfrow=c(1,2))
plot(flo.marriage, vertex.cex=(get.vertex.attribute(flomarriage, 'wealth')/25+.4), displaylabels =TRUE)
plot(flo.biz, vertex.cex=(get.vertex.attribute(flomarriage, 'wealth')/25+.4), displaylabels =TRUE)

###Cargamos data de excel (HR Turnover)
FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep = ";")
head(FriendMat)
class(FriendMat)
#Convertimos a matriz
FriendMat<-as.matrix(FriendMat)
#Convertimos a red
FriendNet <- as.network(FriendMat, directed = TRUE, matrix.type = "adjacency")
par(mfrow=c(1,1))
plot(FriendNet, main="Friendship Network of HR Turnover Data")
##Cargamos data de atributos
HR_att<-read.csv("HR_att.csv",header=TRUE, sep = ";")
head(HR_att)
sex<-HR_att$Sex
length(sex)
dim(FriendMat)
#Acá hay un problema, tenemos 68 atributos pero 122 nodos, debemos match los nodes a sus atributos, hacemos lo siguiente:
names<-HR_att$Name # pull the names out of attributes dataset
gender_vector<-vector() #create a vector for gender
detach(package:sna)
detach(package:network)
suppressPackageStartupMessages(library(igraph))
FriendGraph<-graph_from_adjacency_matrix(FriendMat, weighted=TRUE) #create a graph

#Corremos un doble-loop y asignamos el género al nodo correspondiente
for(i in 1:122){ # this is our set of all network nodes
  for(j in 1:68){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(FriendGraph)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      gender_vector[i]<-sex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{gender_vector[i]<-NA}
  }
}
#Veamos el resultado
head(gender_vector)
length(gender_vector)

#Pongámosle color a los nodos según género y grafiquemos
colors<-ifelse(gender_vector==1,"palevioletred",
               ifelse(gender_vector==0,"royalblue2","gray"))
FriendGraph<-set_vertex_attr(FriendGraph, 'gender', value=c(gender_vector))
par(mar=c(0,0,1,0), mfrow=c(1,1))
plot(FriendGraph, vertex.size=6.5, vertex.color=colors, edge.arrow.size=.2, edge.color="black",
     vertex.label=NA, main= "Friendship Network of HR Turnover")

##Hacemos lo mismo con las otras redes
# Professional ties:
ProfMat<-read.csv("Profnet.csv",header=TRUE, sep = ";")
ProfMat<-as.matrix(ProfMat)
ProfGraph<-graph_from_adjacency_matrix(ProfMat, weighted=TRUE)
# Boss-subordinate ties:
BossMat<-read.csv("BossNet.csv",header=TRUE, sep = ";")
BossMat<-as.matrix(BossMat)
BossGraph<-graph_from_adjacency_matrix(BossMat, weighted=TRUE)
# Support ties:
SupportMat<-read.csv("SupportNet.csv",header=TRUE, sep = ";")
SupportMat<-as.matrix(SupportMat)
SupportGraph<-graph_from_adjacency_matrix(SupportMat, weighted=TRUE)

# Loading node attributes:
ProfGraph<-graph_from_adjacency_matrix(ProfMat, weighted=TRUE)
for(i in 1:122){ # this is our set of all network nodes
  for(j in 1:68){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(ProfGraph)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      gender_vector[i]<-sex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{gender_vector[i]<-NA}
  }
}
BossGraph<-graph_from_adjacency_matrix(BossMat, weighted=TRUE)
for(i in 1:122){ # this is our set of all network nodes
  for(j in 1:68){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(BossGraph)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      gender_vector[i]<-sex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{gender_vector[i]<-NA}
  }
}
SupportGraph<-graph_from_adjacency_matrix(SupportMat, weighted=TRUE)
for(i in 1:122){ # this is our set of all network nodes
  for(j in 1:68){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(SupportGraph)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      gender_vector[i]<-sex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{gender_vector[i]<-NA}
  }
}

###Distintas formas de dibujar redes
#Hacemos gráficos simples de cada una
par(mfrow=c(2,2))
plot(FriendGraph, vertex.size=6.5, vertex.color=colors, edge.arrow.size=.2, edge.color="black",
     vertex.label=NA, main= "Friendship Network", layout=coords)
plot(ProfGraph, vertex.size=6.5, vertex.color=colors, edge.arrow.size=.2, edge.color="black",
     vertex.label=NA, main= "Professional Network", layout=coords)
plot(BossGraph, vertex.size=6.5, vertex.color=colors, edge.arrow.size=.2, edge.color="black",
     vertex.label=NA, main= "Boss-Subordinate Network", layout=coords)
plot(SupportGraph, vertex.size=6.5, vertex.color=colors, edge.arrow.size=.2, edge.color="black",
     vertex.label=NA, main= "Support Network", layout=coords)
#Experimentamos con layouts
coords = layout.fruchterman.reingold(FriendGraph)
coords <- layout.circle(SupportGraph)
coords <- layout.kamada.kawai(BossGraph)
coords <- layout.auto(BossGraph)
coords <- layout.davidson.harel(FriendGraph)
coords <- layout.gem(FriendGraph)
coords <- layout.grid(FriendGraph)

#Experimentamos con colores
library(RColorBrewer)
colBright <- brewer.pal(5, 'Set1') #pick this set for bright colors
colAccent<-brewer.pal(5, 'Accent') #pick this set for pastel colors
display.brewer.pal(12, 'Paired')
par(mar=c(1,1,1,1),mfrow=c(2,3))
display.brewer.pal(5, 'Accent')
display.brewer.pal(5, 'Dark2')
display.brewer.pal(5, 'Pastel1')
display.brewer.pal(5, 'Set1')
display.brewer.pal(5, 'Set2')
display.brewer.pal(5, 'Set3')
colBright <- brewer.pal(5, 'Set1') #pick this set for bright colors
colAccent<-brewer.pal(5, 'Accent') #pick this set for pastel colors
#Lo asignamos a nuestro vector de género
colors <- ifelse(gender_vector==1, colAccent[5], colBright[1])
#Graficamos otra vez para ver cómo queda
plot(FriendNet, vertex.col=colors, main = "Frienship Network" )
par(mar=c(1,1,1,1),mfrow=c(1,1))

##Dicotomizando data con valor
#0 es 0 y todo el resto es 1 (cualquier lazo presente)
Friend.any <- ifelse(FriendMat > 0, 1, 0)
#Cualquier valor mayor que 1
Friend.2 <- ifelse(FriendMat > 1, 1, 0)
#Máximo valor. Ej: si valor más fuerte es 7 (solo se muestran lazos fuertes, de 7)
Friend.max <- ifelse(FriendMat == 7, 1, 0)

##Usamos esto para dibujar con igraph
#Volvemos los grafos en adyacencia
Friendgraph.any <-graph.adjacency(Friend.any,
                                  mode=c("directed"),
                                  weighted=NULL,
                                  diag=FALSE)
Friendgraph.2 <-graph.adjacency(Friend.2,
                                mode=c("directed"),
                                weighted=NULL,
                                diag=FALSE)
Friendgraph.7 <-graph.adjacency(Friend.max,
                                mode=c("directed"),
                                weighted=NULL,
                                diag=FALSE)
"Dibujamos"
par(mar=c(0,0,0,0), mfrow=c(1,3))
plot(Friendgraph.any, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(Friendgraph.2, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(Friendgraph.7, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")

###Ojo: Debemos tener buen fundamento teórico para dicotomizar