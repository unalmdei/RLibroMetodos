datos<-read.csv("datos10_p2.csv", header = TRUE, sep = ";")
sabor<-datos[,3]
sal<-datos[,1]
cuajo<-datos[,2]

model2<-lm(sabor~sal+cuajo)
summary(model2)

predict(model2,data.frame(sal=6,cuajo=0.31))

Regresoras<-as.matrix(datos[,-3])
anva<-aov(lm(sabor~Regresoras))
summary(anva)




