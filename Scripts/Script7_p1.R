
############# PREGUNTA 1 ###########
datos = read.delim("clipboard",T) # lectura de datos
datos

head(datos) # presentacion de datos
str(datos)
attach(datos) # Asignacion de variables

interaction.plot(pH, Madurez, Pectina, lwd = 2, col = 18, ylab = "Pectina media")
interaction.plot(Madurez, pH, Pectina, lwd = 2, col = 22, ylab = "Pectina media")

mod.ef1 = lm(Pectina ~ pH*Madurez+Lote) # Construcci?n del modelo
mod.ef1 = lm(Pectina ~ pH+Madurez+pH*Madurez+Lote) # Construcci?n del modelo

model.tables(aov(mod.ef1), type = "means", se = T )

summary(aov(mod.ef1)) # An?lisis de varianza
summary(mod.ef1)

library(agricolae)
cv.model(mod.ef1)

plot(mod.ef1,which=2)
shapiro.test(residuals(mod.ef1)) # Prueba de normalidad de errores

library(nortest)
ad.test(residuals(mod.ef1)) # Prueba de normalidad de errores de Anderson Darling

plot(mod.ef1, which = 5) # ?La variabilidad es distinta entre los factores?
plot(mod.ef1, which = 1) # ?la variabilidad se incrementa con la media de Y?

library(car)
ncvTest(mod.ef1) # Prueba de homocedasticidad de Breush Pag?n (homogeneidad de varianzas)

plot(residuals(mod.ef1), pch = 18,
     type = "b", ylab = "residuales",
     xlab = "?ndice", main="Residuales")
abline(h=0,col="pink")

library(lmtest)
dwtest(mod.ef1,alternative = c("two.sided")) # Prueba de independencia de errores

# Prueba de Efectos Simples

# install.packages(phia)
library(phia)
testInteractions(mod.ef1, fixed="pH", across="Madurez")
aggregate(Pectina ~ pH*Madurez, FUN = mean) 

testInteractions(mod.ef1, fixed="Madurez", across="pH")

aggregate(Pectina ~ pH*Madurez, FUN = mean) 

# install.packages("lsmeans")
library(lsmeans)
lsmeans(mod.ef1, pairwise ~ pH|Madurez)
lsmeans(mod.ef1, pairwise ~ Madurez|pH)
