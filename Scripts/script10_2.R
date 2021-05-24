datos<-read.csv("datos10_2.csv", header = TRUE, sep = ";")
Y<-datos[,1]
X1<-datos[,2]
X2<-datos[,3]
X3<-datos[,4]
X4<-datos[,5]

model2<-lm(Y~X1+X2+X3+X4)
summary(model2)

m3=step(model2, direction = "both")

r<-rstandard(m3)
shapiro.test(r)

library(lmtest)
bptest(m3)

durbinWatsonTest(m3)
vif(m3)
summary(m3)

Regresoras<-as.matrix(datos[,c(-1,-4)])

anva<-aov(lm(Y~Regresoras))
summary(anva)

summary(m3)
predict(m3,data.frame(X1=33,X2=45.5,X4=5.3),interval="confidence")
