datos<-read.csv("datos6_1.csv", header = TRUE, sep = ";")
head(datos)
attach(datos)
# Prueba de normalidad
library(nortest)
ad.test(Tiempo)

# Prueba de homogeneidad de varianzas
bartlett.test(Tiempo~Drogas)

# Prueba de Kruskal-Wallis

library(agricolae)
out1<-kruskal(Tiempo,Drogas, group=FALSE)
out1

out2<-kruskal(Tiempo,Drogas, group=TRUE)
out2
