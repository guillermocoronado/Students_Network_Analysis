library(sna)
library(network)
library(foreign)
library(igraph)
library(RColorBrewer)
install.packages("kableExtra")

###Cargamos la data, creamos matrices, las dicotomizamos, creamos grafos y redes
FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep = ";")
ProfMat<-read.csv("Profnet.csv",header=TRUE, sep = ";")
BossMat<-read.csv("BossNet.csv",header=TRUE, sep = ";")
SupportMat<-read.csv("SupportNet.csv",header=TRUE, sep = ";")

FriendMat<-as.matrix(FriendMat)
ProfMat<-as.matrix(ProfMat)
BossMat<-as.matrix(BossMat)
SupportMat<-as.matrix(SupportMat)

Friend.any <- ifelse(FriendMat > 0, 1, 0) #Es importante dicotomizar para construir modelo
Boss.any <- ifelse(BossMat > 0, 1, 0)
Prof.any <- ifelse(ProfMat > 0, 1, 0)
Support.any <- ifelse(SupportMat > 0, 1, 0)

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

detach(package:igraph)
suppressPackageStartupMessages(library(sna))
FriendNet.any<-as.network(Friend.any)
BossNet.any<-as.network(Boss.any)
ProfNet.any<-as.network(Prof.any)
SupportNet.any<-as.network(Support.any)

###Dibujamos las redes
detach(package: sna)
detach(package: network)
library(igraph)
coords <- layout.circle(FriendGraph.any)
coords1 <- layout.circle(ProfGraph.any)
coords2 <- layout.circle(BossGraph.any)
coords3 <- layout.circle(SupportGraph.any)
par(mar=c(0,0,0,0), mfrow=c(2,2))
plot(FriendGraph.any, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(ProfGraph.any, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(BossGraph.any, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")
plot(SupportGraph.any, vertex.size=8, edge.arrow.size= .2, vertex.label.cex= .5,
     vertex.color = "aquamarine4", edge.color= "red", vertex.shape= "square",
     vertex.label.dist = .5, vertex.label.color = "black")

###Construcción del modelo
HR_att<-read.csv("HR_att1.csv",header=TRUE)
#Let's get the sex of our respondents into its own vector:
sex<-HR_att$Sex
age<-HR_att$Age

#Dependent variables
Emotional_part<-HR_att$Emot_Part
Intent_to_leave<-HR_att$Intent_toLeave
Personal_conflicts<-HR_att$Personal_conflicts

#Predictors
#Challenge stressors (en el corto plazo aumentan la productividad, pero en el largo plazo son nocivos):
Work_Quant<-HR_att$Work_quant #Work quantity
Work_Resp<-HR_att$Work_Resp #Work responsibility
Work_Diff<-HR_att$Work_Diff #work difficulty
Work_Speed<-HR_att$Work_Speed # Work speed
#Hindrance stressors (interfieren en el trabajo normal):
Admin_problems<-HR_att$Admin_problems
Personal_conflicts<-HR_att$Personal_conflicts

##Regresión lineal múltiple
Model1<- lm(Emotional_part ~ Work_Resp + Work_Diff)
summary(Model1)
Model1a<-lm(Emotional_part~Work_Resp+Work_Diff+sex+age)
summary(Model1a) #Controles no son significativos, por lo cual se remueven del modelo
Model2 <- lm(Intent_to_leave ~ Personal_conflicts + Work_Quant)
summary(Model2)

##Utilizamos network measures como predictores (en este caso, centralidad)
Debe haber fundamento teórico, en este caso usamos degree, betwenness y eigenvector
Grado: Contrarrestaría deseo de irse
Betwennes y Eigenvector: Estar rodeado de gente poderosa (jefes) aumenta estrés

#Sacamos las medidas de centralidad
library(igraph)
indegree_Friend <- degree(FriendGraph.any, mode='in')
outdegree_Friend <- degree(FriendGraph.any, mode='out')
total_degree_Friend <- degree(FriendGraph.any, mode='total')
indegree_Support <- degree(SupportGraph.any, mode='in')
outdegree_Support <- degree(SupportGraph.any, mode='out')
total_degree_Support <- degree(SupportGraph.any, mode='total')
indegree_Prof <- degree(ProfGraph.any, mode='in')
outdegree_Prof <- degree(ProfGraph.any, mode='out')
total_degree_Prof <- degree(ProfGraph.any, mode='total')
#Betweenness and eigenvector: professional network?
betweenness_Prof <- betweenness(ProfGraph.any)
Prof_Evcent <- evcent(ProfGraph.any)
Prof_eigen <- Prof_Evcent$vector

#Ahora tenemos que pasar estas medidas a nuestras filas de atributos
 #Primero creamos vectores que sostengan nuestras medidas
Friend_indegree<-vector()
Friend_outdegree<-vector()
Friend_totaldegree<-vector()
Support_indegree<-vector()
Support_outdegree<-vector()
Support_totaldegree<-vector()
Prof_between<-vector()
Prof_eigenv<-vector()
Prof_indegree <- vector()
Prof_outdegree <- vector()
Prof_totaldegree <- vector()
 
 #we run the double loop to pass on centralities to these vectors based on the node names in the attributes file.
names<-HR_att$Name # pull the names out of attributes dataset
for(i in 1:122){
  for(j in 1:68){
    if(V(FriendGraph.any)$name[i]==names[j]){
      Friend_indegree[j]<-indegree_Friend[i]
      Friend_outdegree[j]<-outdegree_Friend[i]
      Friend_totaldegree[j]<-total_degree_Friend[i]
      Support_indegree[j]<-indegree_Support[i]
      Support_outdegree[j]<-outdegree_Support[i]
      Support_totaldegree[j]<-total_degree_Support[i]
      Prof_between[j]<-betweenness_Prof[i]
      Prof_eigenv[j]<-Prof_eigen[i]
      Prof_indegree[j] <- indegree_Prof[i]
      Prof_outdegree[j] <- outdegree_Prof[i]
      Prof_totaldegree[j] <- total_degree_Prof[i]
      break;}
    else{}
  }
}

#Construimos el modelo
 #Creamos una matriz de correlaciones de nuestros vectores
var_mat<-cbind(
  Personal_conflicts,
  Intent_to_leave,
  Work_Resp,
  Work_Speed,
  Friend_indegree,
  Friend_outdegree,
  Friend_totaldegree,
  Support_indegree,
  Support_outdegree,
  Support_totaldegree,
  Prof_between,
  Prof_eigenv,
  Prof_indegree,
  Prof_outdegree,
  Prof_totaldegree)

cor_mat <- cor(var_mat)

row.names(cor_mat)<-c("1.Personal conflicts", "2.Intent to leave", "3.Work responsibilities",
                      "4.Work speed", "5.Friend_network indegree","6.Friend_network outdegree",
                      "7.Friend network total degree","8.Support network indegree",
                      "9.Support network outdegree","10.Support network total degree",
                      "11.Prof network betweenness","12.Prof network eigenvalue",
                      "13.Prof network indegree","14.Prof network outdegree",
                      "15.Prof network total degree")

suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(kableExtra))

kable(cor_mat, digits=2, col.names=c("1","2","3","4","5","6","7","8","9","10","11","12",
                                     "13","14","15"),
      row.names=TRUE, caption="Correlations of HR Influence Model Variables") %>%
  kable_styling(latex_options=c("scale_down", "hold_position"))

 #Armamos el modelo de influencia social (regresión lineal múltiple)
Model2<-lm(Emotional_part ~ Work_Resp+Work_Diff+Support_totaldegree+Prof_eigenv)
summary(Model2)
Model3 <- lm(Intent_to_leave~Work_Resp+Work_Diff+Support_totaldegree+Prof_eigenv)
summary(Model3)

Variables de redes son significativas
Modelo explica 41.59% de la variabilidad de variable dependiente: variables de redes agregaron un 10% de explicación

