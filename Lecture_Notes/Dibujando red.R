###Paquetes de redes
install.packages("sna")
install.packages("network")
install.packages("foreign")
install.packages("igraph")
install.packages("ndtv")
install.packages("RColorBrewer")
install.packages("rgl")
install.packages("ergm")
suppressPackageStartupMessages(library(sna))

###Red de consumidores de droga en Hartford 
drugpaj <- read.paj('drugnet2.paj') #read the data
drug <- drugpaj$networks[[1]] # extract network
plot(drug) #Graficamos la red
gender<-drugpaj$partitions[[1]] #extract the attributes (gender)
suppressPackageStartupMessages(library(knitr)) #allows for better-looking tables
kable(table(gender), col.names=c("Gender","Frequency")) #Tabla de atributos (gender)
ethnicity <- drugpaj$partitions[[2]] #extraemos atributos (ethnicity)
kable(table(ethnicity), col.names=c("Ethnicity","Frequency")) #Tabla de atributos (ethnicity)

#Las tablas no nos dicen nada m�s que la cantidad de personas negras/blancas o hombres/mujeres en la red
#Por ende, tenemos que poner vectores basados en los atributos: ponemos forma a la etnicidad y color al g�nero

sides <- ifelse(ethnicity==1, 12, ifelse(ethnicity==2, 3, ifelse(ethnicity==3, 4, 6)))
colors <- ifelse(gender==2, "palevioletred", ifelse(gender==1, "royalblue2", "gray8"))
par(mar=c(0, 0, 0, 0))

#Creamos el gr�fico con los atributos
plot(drug, vertex.col=colors, vertex.sides=sides, vertex.cex=1.5)

##Gr�fico no es muy bueno para diferencias etnicidad, hacemos otro (le ponemos color a la etnicidad y forma al g�nero)
colors2 <- ifelse(ethnicity== 1, "red", ifelse(ethnicity== 2, "purple", ifelse(ethnicity== 3, "brown", "yellow")))
sides.2 <- ifelse(gender==2, 12, ifelse(gender==1, 3, 4))
plot(drug, vertex.col= colors2, vertex.sides= sides.2, vertex.cex=1)

Con esto vemos que los consumidores de droga est�n conectados por raza (marrones[latinos] y azules[negros] estan juntos)
El an�lisis de redes nos muestra m�s que las tablas de atributos, es inferencial en s� mismo

###Red de familias florencianas
load('flo.Rdata')
suppressPackageStartupMessages(library(RColorBrewer))
flomarriage <- as.network(as.matrix(flo.marriage), directed=FALSE)
# Add attributes
set.vertex.attribute(flomarriage, 'wealth', flo.att[,2])
FloColors <- c(brewer.pal(11,'RdYlBu'),brewer.pal(9,'RdYlBu')) #set a vector of colors
##Simple plot (with wealth)
plot(flomarriage, vertex.col=FloColors, 
     vertex.cex=(get.vertex.attribute(flomarriage, 'wealth')/25 +.4),
     displaylabels=TRUE)
##A simple 3D plot:
gplot3d(flomarriage, vertex.col=FloColors, vertex.radius=1.5, edge.lwd=.2)
#Add names
gplot3d(flomarriage, vertex.col=FloColors, vertex.radius=1.5, edge.lwd=.2, displaylabels=TRUE)
#A�adimos atributo:wealth
gplot3d(flomarriage, vertex.col=FloColors, 
        vertex.radius=(get.vertex.attribute(flomarriage, 'wealth')/30 +.5),
        edge.lwd=.2, displaylabels=TRUE)

###Red de pel�cula: Dragon
suppressPackageStartupMessages(library(ndtv))
suppressPackageStartupMessages(library(foreign))
col20 <- c(brewer.pal(11, "RdYlBu"), brewer.pal(9, "RdYlBu"))
#Create a list object to contain all individual network time-slices
drakon=list()
for (i in 1:65) {
  drakon[[i]]<-read.paj(paste("T",i,".paj",sep=""))
}
drakNet<-networkDynamic(network.list=drakon, start=1)

activity<-read.csv("Activity.csv")
activity<-activity[,4:68]
for (i in 1:65) {activate.vertex.attribute(drakNet,'activity',activity[[i]],onset=i,terminus=i)}
render.d3movie(drakNet, plot.par=list(displaylabels=T,
vertex.col=col20, vertex.cex = function(slice){slice%v%'activity'/5+0.5}))

