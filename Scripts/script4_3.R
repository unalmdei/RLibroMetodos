# La version del R, con la cual se escribio el siguiente codigo es: R-4.0.1
# Se recomienda usar codificacion utf-8 por contener el archivo letras especiales como la ñ


## ejercicio 2 
datos2 <- read.csv("datos4_3.csv",T, sep=";")
datos2$Tratamiento <- factor(datos2$Tratamiento)
datos2$Tamaño <- factor(datos2$Tamaño)

a<-tapply(datos2$Ganancia,datos2$Tratamiento, mean)
b<-tapply(datos2$Ganancia,datos2$Tratamiento, sd)
c<-tapply(datos2$Ganancia,datos2$Tratamiento, median)

cbind(Media = a,sd=b,Mediana=c, n=5)

# prueba de supuestos
modelo<-lm(formula = Ganancia~Tratamiento+Tamaño,data= datos2)
residuos=rstandard(modelo)

# Prueba de normalidad
shapiro.test(residuos)
library(nortest)
ad.test(residuos)
# Prueba de homogeneidad de varianzas
attach(datos2)
bartlett.test(Ganancia~Tratamiento)
library(car)
leveneTest(Ganancia~Tratamiento,center="median", data = datos2)


# analisis de varianza
modelo<-lm(formula = Ganancia~Tratamiento+Tamaño,data= datos2)
anva<-aov(modelo)
summary(anva)

#d
library(multcomp)
contraste3=c(0,1,0,-1)
prueba3<-glht(anva,linfct=mcp(Tratamiento=contraste3),alternative = "greater")
summary(prueba3)

# dunnet
library(multcomp) 
dunnett<-glht(anva,linfct=mcp(Tratamiento="Dunnett"))
summary(dunnett)

# f
# tukey
library(multcomp) 
tukey<-glht(anva,linfct=mcp(Tratamiento="Tukey"))
summary(tukey)

