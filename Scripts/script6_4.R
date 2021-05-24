datos<-read.csv("datos6_4.csv", header = TRUE, sep = ";")
head(datos)

attach(datos)

# Prueba de Friedman
library(agricolae)
out1<-friedman(Cliente,Marca,Calificación,group=FALSE)
out1


# Dos formas de aplicar la prueba de Friedman con o sin grupos

# Prueba de Friedman
library(agricolae)
out1<-friedman(Cliente,Marca,Calificación,group=FALSE)
out1

out2<-friedman(Cliente,Marca,Calificación,group=TRUE)
out2

