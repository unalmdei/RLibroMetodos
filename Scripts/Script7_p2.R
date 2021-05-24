
############## Pregunta II ################

datos = read.delim("clipboard",T) # lectura de datos
datos

head(datos) # presentaci?n de datos
str(datos)
attach(datos) # Asignaci?n de variables

interaction.plot(Pulpa, Temperatura, Antocianina, lwd = 4, col = 18, ylab = "Antocianina")
interaction.plot(Temperatura, Pulpa, Antocianina, lwd = 2, col = 22, ylab = "Antocianina")

Pulpa
Temperatura
Antocianina


mod.ef1 = lm(Antocianina ~ Pulpa*Temperatura) # Construcci?n del modelo

model.tables(aov(mod.ef1), type = "means", se = T )

summary(aov(mod.ef1)) # An?lisis de varianza

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
abline(h=0,col=11)

library(lmtest)
dwtest(mod.ef1,alternative = c("two.sided")) # Prueba de independencia de errores

# Prueba de Efectos Simples

# install.packages(phia)

library(phia)
testInteractions(mod.ef1, fixed="Pulpa", across="Temperatura")
aggregate(Antocianina ~ Pulpa*Temperatura, FUN = mean) 

testInteractions(mod.ef1, fixed="Temperatura", across="Pulpa")

aggregate(Antocianina ~ Temperatura*Pulpa, FUN = mean) 

# install.packages("lsmeans")
library(lsmeans)
lsmeans(mod.ef1, pairwise ~ Pulpa|Temperatura)
lsmeans(mod.ef1, pairwise ~ Temperatura|Pulpa)
