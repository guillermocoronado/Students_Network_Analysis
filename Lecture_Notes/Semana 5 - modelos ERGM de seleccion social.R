library(sna)
library(network)
library(foreign)
library(igraph)
library(intergraph)
install.packages("ergm")
library(ergm)
install.packages("coda")
library(coda)

#Cargamos la data
FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep = ";")
ProfMat<-read.csv("Profnet.csv",header=TRUE, sep = ";")
BossMat<-read.csv("BossNet.csv",header=TRUE, sep = ";")
SupportMat<-read.csv("SupportNet.csv",header=TRUE, sep = ";")

#Convertimos a matriz
FriendMat<-as.matrix(FriendMat)
ProfMat<-as.matrix(ProfMat)
BossMat<-as.matrix(BossMat)
SupportMat<-as.matrix(SupportMat)

#Dicotomizamos
Friend.any <- ifelse(FriendMat > 0, 1, 0) #Es importante dicotomizar para construir modelo
Boss.any <- ifelse(BossMat > 0, 1, 0)
Prof.any <- ifelse(ProfMat > 0, 1, 0)
Support.any <- ifelse(SupportMat > 0, 1, 0)

#Creamos grafos
suppressPackageStartupMessages(library(igraph))

FriendGraph<-graph_from_adjacency_matrix(FriendMat, weighted=TRUE)
ProfGraph<-graph_from_adjacency_matrix(ProfMat, weighted=TRUE)
BossGraph<-graph_from_adjacency_matrix(BossMat, weighted=TRUE)
SupportGraph<-graph_from_adjacency_matrix(SupportMat, weighted=TRUE)

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

#Creamos red
detach(package:igraph)
suppressPackageStartupMessages(library(sna))
FriendNet.any<-as.network(Friend.any)
BossNet.any<-as.network(Boss.any)
ProfNet.any<-as.network(Prof.any)
SupportNet.any<-as.network(Support.any)

#Dibujamos las redes
detach(package: sna)
detach(package: network)
library(igraph)
coords <- layout.circle(FriendGraph.any)
coords1 <- layout.circle(ProfGraph.any)
coords2 <- layout.circle(BossGraph.any)
coords3 <- layout.circle(SupportGraph.any)
par(mar=c(0,0,0,0), mfrow=c(2,2))
plot(FriendGraph.any, vertex.size=2, edge.arrow.size= .2, vertex.label.cex= .2,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(ProfGraph.any, vertex.size=2, edge.arrow.size= .2, vertex.label.cex= .2,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(BossGraph.any, vertex.size=2, edge.arrow.size= .2, vertex.label.cex= .2,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(SupportGraph.any, vertex.size=2, edge.arrow.size= .2, vertex.label.cex= .2,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")

###Construcción del modelo

#Primer modelo: incondicionado (la única condición son los edges)
suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(coda))
set.seed(0)
mFriend01<-ergm(FriendNet.any~edges, verbose=FALSE)
summary(mFriend01)
Vemos que sale p-value menor a 0.05, por lo cual nuestra red es distinta a la red random
Además, podemos ver que nuestra red tiene menos lazos que los esperados por la red random
Ahora bien, recordemos que los estimados son log odds, por lo cual tenemos que sacar la función logit inversa
invlogit <- function(x) {1/(1+exp(-x))}
x <- coef(mFriend01)
x <- coef(mFriend03)
invlogit(x)
El resultado es la probabilidad de formación de lazos con nuestro modelo, vemos que es exacto a la densidad de la Red
network.density(FriendNet.any)
Si tomamos el modelo, podemos ver varias cosas
mFriend01$coefficients
mFriend01$iterations

#Modelo ERGM con variables estructurales
set.seed(0)
mFriend02 <- ergm(FriendNet.any ~ edges + triangle)
Acá pregunto cómo añadir un triángulo cambia la probabilidad de formación de un lazo?
O en todo caso, la estructura del triángulo afecta la formación de nuevos lazos?
El modelo falla, ver por qué en pdf de semana 5
mFriend02 <- ergm(FriendNet.any ~ edges + triangle, verbose=FALSE,
                  control=control.ergm(seed=0,MCMLE.maxit=1, MCMLE.density.guard.min = 1206639))
Modelo sigue fallando, así que usamos diagnóstico MCMC
mcmc.diagnostics(mFriend02, verbose=FALSE)
El modelo no corre, pero en resumen el análisis mcmc muestra que el número de edges se salió de control
Vemos como el 0 está muy arriba y no se parece en nada a una distribución normal
Una solución es utilizar el GWESP (ver pdf)
#mFriend03<-ergm(FriendNet.any~edges+gwesp(0.10,fixed=T),control = control.ergm(seed=1))
Tambiém trae problemas, así que un camino más fácil es utilizar el mutual como parámetro
set.seed(0)
mFriend03<-ergm(FriendNet.any~edges+mutual, verbose=FALSE)
mcmc.diagnostics(mFriend03)
El diagnóstico nos muestra que el 0 si oscila al medio y que distribución es normmal, por ende es buen modelo
summary(mFriend03)
Ambas variables son significativas, en este caso mutuality es positiva
Esto quiere decir que la mutualidad es mayor que la esperada por red random: formado un lazo recíproco, hay mayor probabilidad de formación de lazo
invlogit(mFriend03$coef[1]+mFriend03$coef[2])
La función logit inversa nos muestra que con estos dos parámetros, la probabilidad de formar un lazo es 33.8%

#Modelo ERGM con covariates (atributos del nodo)
HR_att<-read.csv("HR_att1.csv",header=TRUE)
names<-HR_att$Name
age<-HR_att$Age
sex<-HR_att$Sex

 #Tenemos que ajustar atributos a nodos (modelo no corre con missing values)
gender_vector<-vector()
age_vector<-vector()
for(i in 1:122){ # this is our set of all network nodes
        for(j in 1:122){ # this is our set of attribute-containing nodes
                # for each node in i, we run through all node in j
                # and compare names
                if(V(FriendGraph.any)$name[i]==names[j]){
                        #if we match, we add the attribute to a vector
                        gender_vector[i]<-sex[j]
                        age_vector[i]<-age[j]
                        # and exit the inner loop for the next i
                        break;}
                # if not, we are assigning a missing value
                # and keep going until we find a matching node
                else{gender_vector[i]<-2
                age_vector[i]<-30
                }
        }
}

## Set vertex attributes:
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'gender', value=c(gender_vector))
FriendGraph.any<-set_vertex_attr(FriendGraph.any, 'age', value=c(age_vector))

library(intergraph)
FriendNet.any<-asNetwork(FriendGraph.any)
detach(package:igraph)

##Ahora sí construimos modelo ERGM con atributos del nodo
mFriend04<-ergm(FriendNet.any ~ edges+mutual+absdiff('age')+
                        nodematch('gender'),
                control = control.ergm(seed=1), verbose=FALSE)
mcmc.diagnostics(mFriend04)
Vemos que modelo es bueno según diagnóstico
summary(mFriend04)
Las únicas variables no significativas son el género como tal (aunque sí lo es que coincidan el género)
Por ende, al predecir, no tomamos en cuenta coeficientes 2 y 3 (incluso deberíamos sacarlos del modelo)

##Predecimos
#Probabilidad total
invlogit(mFriend04$coef[1]+mFriend04$coef[2]+mFriend04$coef[3]+mFriend04$coef[4])
Probabilidad de que lazo se forme es de 39.59%
# Case 1: A woman forming a tie with another woman, not reciprocating a tie and three years of age difference
invlogit(mFriend04$coef[1]+3*mFriend04$coef[3]+ mFriend04$coef[2]) #Multiplico el coef 4 por 3 (años de diferencia)
Probabilidad de 30.7%
# Case 2: A man forming a tie with a man of the same age, not reciprocating a tie;
invlogit(mFriend04$coef[1]+mFriend04$coef[4])
Probabilidad de 2.1% #Vemos que probabilidad baja
# Case 3: A woman forming a tie with a man of the same age, reciprocating a tie.
invlogit(mFriend04$coef[1]+mFriend04$coef[2])
Probabildad de 32.84%

##Chequeamos la bondad de ajuste del modelo
mFriend04.gof <- gof(mFriend04 ~ idegree)
mFriend04.gof$pval.ideg
par(mfrow=c(1,1))
plot(mFriend04.gof)

No es un buen modelo, hay que seguir ajustándolo
