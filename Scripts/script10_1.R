datos<-read.csv("datos10_1.csv", header = TRUE, sep = ";")
Y<-datos[,1]
X1<-datos[,2]
X2<-datos[,3]
model1<-lm(Y~X1+X2)
vif(model1)
r<-rstandard(model1)
shapiro.test(r)
library(lmtest)
bptest(model1)
library(car)
ncvTest(model1)
durbinWatsonTest(model1)

rept<-rep(1,15)
X<-cbind(rept,datos[,-1])
X

X1<-as.matrix(X)

t(X1)%*%X1

t(X1)%*%Y

solve(t(X1)%*%X1)

betaest=solve(t(X1)%*%X1)%*%(t(X1)%*%Y)
betaest

summary(model1)

Regresoras<-as.matrix(datos[,-1])
anva<-aov(lm(Y~Regresoras))
summary(anva)

summary(model1)
predict(model1,data.frame(X1=1350,X2=3),interval="prediction")