# La version del R, con la cual se escribio el siguiente codigo es: R-4.0.1
# Se recomienda usar codificacion utf-8 por contener el archivo letras especiales como la ñ

datos<-read.csv("datos4_1.csv", header = TRUE, sep = ";")
head(datos)
datos$Tratamiento = factor(datos$Tratamiento, labels = c("T-1","T-2","T-3","T-4","T-5"))

# descriptivos para el tratamiento
attach(datos)
MediaTrat=tapply(datos$Rendimiento,datos$Tratamiento, mean)
SdTrat= tapply(datos$Rendimiento,datos$Tratamiento, sd)
df1 = data.frame(MediaTrat,SdTrat)
library(knitr)
kable(df1,caption = "Descriptivos Tratamientos",
      digits = 2, format.args = list( decimal.mark = "."))

# descriptivos para el bloque
MediaBloq= tapply(datos$Rendimiento,datos$Llanura, mean)
SdBloq= tapply(datos$Rendimiento,datos$Llanura, sd)
df2 = data.frame(MediaBloq,SdBloq)
kable(df2,caption = "Descriptivos Bloques",
      digits = 2, format.args = list( decimal.mark = "."))

# Graficos descriptivos 
library(ggplot2)
ggplot(data=datos, aes(x=Tratamiento,y=Rendimiento, color=Tratamiento))+
        geom_boxplot(show.legend = FALSE) +
        xlab("Tratamiento") +  
        ylab("Rendimiento (kg.ha-1)")+
        ggtitle("Grafico de cajas del Rendimiento según Tratamiento")

ggplot(data=datos, aes(x=Llanura,y=Rendimiento, color=Llanura))+
        geom_boxplot(show.legend = FALSE) +
        xlab("Grupos de Llanura") +  
        ylab("Rendimiento (kg.ha-1)")+
        ggtitle("Grafico de cajas del Rendimiento según Llanura")


# Pruebas de supuestos
modelo<-lm(formula = Rendimiento~Tratamiento+Llanura,data= datos)
ri<-rstandard(modelo)

# plot(modelo,2)
# plot(modelo,3)

# Graficos para los supuestos

# Grafico de homogeneidad
library(broom)
modelo2 <- augment(modelo)
g1<-ggplot(modelo2, aes(.fitted, sqrt(abs(.std.resid))))+geom_point(na.rm=TRUE)
g1<-g1+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
g1<-g1+ylab(expression(sqrt("|Standardized residuals|")))
g1<-g1+ggtitle("Scale-Location")+theme_bw()
g1

# grafico de normalidad
g2<-ggplot(modelo2, aes(qqnorm(.std.resid)[[1]], .std.resid))+geom_point(na.rm = TRUE)
g2<-g2+geom_smooth(method = "lm", se = TRUE)+
        xlab("Theoretical Quantiles")+
        ylab("Standardized Residuals")
g2<-g2+ggtitle("Normal Q-Q")+theme_bw()
g2

# pruebas de hipotesis sobre supuestos
# prueba de normalidad
library(nortest)
ad.test(ri)
shapiro.test(ri)
# prueba de homogeneidad de varianza
# Prueba de bartlet
bartlett.test(ri~Tratamiento)
library(car)
# Prueba de Levene
leveneTest(ri~Tratamiento, center = "median")
# Prueba de Breusch-Pagan test
library(lmtest)
bptest(modelo)

# Desarrollo del analisis de varianza
anva<-aov(modelo)
summary(anva)

# ejercicio d
contraste1 = c(0,-1,0,1,0)
library(multcomp)
compara <-glht(anva,linfct=mcp(Tratamiento=contraste1))
summary(compara)

# ejercicio e
# Pruebas de comparaciones multiples t o LSD
pairwise.t.test(x=datos$Rendimiento, g= datos$Tratamiento, p.adjust.method="none")
# con otro paquete
library(agricolae)
LSD.test(anva, "Tratamiento" ,p.adj = "none", console = TRUE)

# Tukey 
library(multcomp) 
compTratamientos<-glht(anva,linfct=mcp(Tratamiento="Tukey"))
confint(compTratamientos)
summary(compTratamientos)
par(mfrow=c(1,1))
plot(compTratamientos)

# Con otro paquete
library(agricolae)
HSD.test(anva, "Tratamiento" , console = TRUE, group=FALSE)
HSD.test(anva, "Tratamiento" , console = TRUE, group=TRUE)

# dunnet
# deseo que E sea el testigo recodifico
library(multcomp) 
compTratDunnett <-glht(anva,linfct=mcp(Tratamiento="Dunnett"))
summary(compTratDunnett)


# contrastes ortogonales
library(emmeans)
resultado.emm = emmeans(anva, specs =~Tratamiento, data=datos)
Contraste = list(Tra4y2_vs_Tra1y3y5  = c(-2,3,-2,3,-2))
contrast(resultado.emm, Contraste)



