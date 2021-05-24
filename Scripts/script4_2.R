# La version del R, con la cual se escribio el siguiente codigo es: R-4.0.1
# Se recomienda usar codificacion utf-8 por contener el archivo letras especiales como la ñ

## ejercicio 1
datos <- read.csv("datos4_2.csv",T, sep=";")
datos$Suelo <- factor(datos$Suelo)
datos$Formula <- factor(datos$Formula)

a<-tapply(datos$Rendimiento,datos$Formula, mean)
b<-tapply(datos$Rendimiento,datos$Formula, sd)
c<-tapply(datos$Rendimiento,datos$Formula, median)

cbind(Media = a,sd=b,Mediana=c, n=3)

# b
modelo<-lm(formula = Rendimiento~Formula+Suelo,data= datos)
anva<-aov(modelo)
summary(anva)
# c
# Ho: mu4 = mu2
# Ha: mu4 > mu2
library(multcomp)
contraste=c(0,-1,0,1)
prueba1<-glht(anva,linfct=mcp(Formula=contraste))
summary(prueba1)
# d
#lsd
library(agricolae)
pruebat <- LSD.test(anva, "Formula" ,p.adj = "none", console = TRUE)
pruebat

# e
# dunnet
library(multcomp) 
dunnett<-glht(anva,linfct=mcp(Formula="Dunnett"))
summary(dunnett)

# f
# tukey
library(multcomp) 
tukey<-glht(anva,linfct=mcp(Formula="Tukey"))
summary(tukey)

# g
# 
contraste=c(2,0,-1,-1)
prueba2<-glht(anva,linfct=mcp(Formula=contraste))
summary(prueba2)


