datos<-read.csv("datos10_p1.csv", header = TRUE, sep = ";")
Y<-datos[,1]
X1<-datos[,2]
X2<-datos[,3]
X3<-datos[,4]

model1<-lm(Y~X1+X2+X3)
summary(model1)

r<-rstandard(m1)
shapiro.test(r)

library(lmtest)
bptest(m1)

library(car)

durbinWatsonTest(m1)

vif(m1)

summary(m1)

Regresoras<-as.matrix(datos[,c(-1,-4)])

anva<-aov(lm(Y~Regresoras))
summary(anva)

predict(m1,data.frame(X1=11,X2=2.4),interval="confidence",level=0.95)
