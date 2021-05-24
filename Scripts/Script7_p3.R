
################### Pregunta 3 ####################

# Caso DCA factorial: Proteina vs Tratamiento y Periodo

datos = read.delim("clipboard",T) # lectura de datos
datos

head(datos) # presentacion de datos
str(datos)

# Algunas funciones necesitan que los factores se trabajen como factores
# y no como caracteres

is.factor(datos$Tratamiento)
is.factor(datos$Periodo)
datos$Tratamiento=as.factor(datos$Tratamiento)
datos$Periodo=as.factor(datos$Periodo)
str(datos)

attach(datos) # Asignacion de variables

interaction.plot(Tratamiento, Periodo, Proteina, lwd = 4, col = 18,
                 ylab = "Proteina")
interaction.plot(Periodo, Tratamiento, Proteina, lwd = 2, col = 22,
                 ylab = "Proteina")
Tratamiento
Periodo
Proteina

# Construccion del modelo
modelo = lm(Proteina ~ Tratamiento*Periodo,data=datos) 
modelo = lm(Proteina ~ Tratamiento+Periodo+Tratamiento*Periodo,data=datos) 
model.tables(aov(modelo), type = "means", se = T )
model.tables(aov(modelo), type = "effects", se = T )
summary(aov(modelo))
anova(modelo)

# SUPUESTOS
datos
# Prueba de normalidad de errores
plot(modelo,which=2)
shapiro.test(residuals(modelo)) 

library(nortest)
ad.test(residuals(modelo)) # Prueba de normalidad de errores de Anderson Darling

# Prueba de homocedasticidad de Breush Pagan
# (homogeneidad de varianzas)
plot(modelo, which = 3) # La variabilidad es distinta entre los factores?
plot(modelo, which = 1) # La variabilidad se incrementa con la media de Y?
library(car)
ncvTest(modelo)

# Prueba de independencia de errores
library(lmtest)
dwtest(modelo,alternative = c("two.sided")) 

# promedio y desviaciÃ³n estÃ¡ndar de Proteina por niveles de los
# factores, y por combinaciones de los niveles de ambos factores
aggregate(Proteina ~ Tratamiento, FUN = mean)
aggregate(Proteina ~ Tratamiento, FUN = sd)
aggregate(Proteina ~ Periodo, FUN = mean)
aggregate(Proteina ~ Periodo, FUN = sd)
aggregate(Proteina ~ Tratamiento*Periodo, FUN = mean)
aggregate(Proteina ~ Tratamiento*Periodo, FUN = sd)
aggregate(Proteina ~ Periodo*Tratamiento, FUN = mean)
aggregate(Proteina ~ Periodo*Tratamiento, FUN = sd)

# MÃ¡s Estad??sticas descriptivas:
library(psych)
describeBy(Proteina,list(Tratamiento),digits=2)
describeBy(Proteina,list(Periodo),digits=2)
describeBy(Proteina,list(Tratamiento,Periodo),digits=2)
describeBy(Proteina,list(Periodo,Tratamiento),digits=2)

# Medias estad??sticas de las interacciones bajo el modelo:
library(phia)
library(car)
library(carData)

(means.proteina = interactionMeans(modelo))
plot(means.proteina, abbrev.levels=TRUE)

interactionMeans(modelo)

library(phia)

# Efectos simples de PT1, PT2 y PT3
testInteractions(modelo, fixed="Tratamiento", across="Periodo",adjustment="none")

# Efectos simples de TD0, TD24, TD48, TD72 y TD96
testInteractions(modelo, fixed="Periodo", across="Tratamiento")

# PRUEBAS DE EFECTOS SIMPLES

# ComparaciÃ³n de medias m??nimo cuadrÃ¡ticas
library(lsmeans)

#Efectos simples de TD0, TD24, TD48, TD72 y TD96
lsmeans(modelo, pairwise ~ Tratamiento|Periodo)

#Efectos simples de PT1, PT2 y PT3
lsmeans(modelo, pairwise ~ Periodo|Tratamiento)

# Prueba de Tukey
comparaciones<-HSD.test(modelo,c("Periodo","Tratamiento"),group=FALSE)
comparaciones

comparaciones1<-HSD.test(modelo,c("Tratamiento","Periodo"),group=FALSE)
comparaciones1

# Prueba de LSD (o DLS que es equivalente a la prueba t)

comparaciones<-LSD.test(modelo,c("Periodo","Tratamiento"),p.adj="none",group=FALSE)
comparaciones

comparaciones1<-LSD.test(modelo,c("Tratamiento","Periodo"),p.adj="none",group=FALSE)
comparaciones1