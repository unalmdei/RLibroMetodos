datos<-read.csv("datos6_2.csv", header = TRUE, sep = ";")
head(datos)
attach(datos)

# Dos formas de aplicar la prueba de Kruskal-Wallis con o sin grupos

# Prueba de Kruskal-Wallis
library(agricolae)
out1<-kruskal(Diseños,Metodos, group=FALSE)
out1

out2<-kruskal(Diseños,Metodos, group=TRUE)
out2


