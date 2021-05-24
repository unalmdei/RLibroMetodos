datos<-read.csv(file.choose(), header = TRUE, sep = ";")
#datos<-read.csv("datos10_p3.csv", header = TRUE, sep = ";")
X1<-datos[,1]
X2<-datos[,2]
X3<-datos[,3]
Y<-datos[,4]

model3<-lm(Y~X1+X2+X3)
summary(model3)

Regresoras<-as.matrix(datos[,-4])
anva<-aov(lm(Y~Regresoras))
summary(anva)
