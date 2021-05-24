## Pregunta 4

# Caso DBCA - factorial: pH vs Sacarosa, Carragenina y Tiempo

datos = read.delim("clipboard",T) # lectura de datos
datos

head(datos) # presentacion de datos

str(datos)

attach(datos) # Asignacion de variables

interaction.plot(Sacarosa, Carragenina, pH, lwd = 2, 
                 col = 18, ylab = "pH")

interaction.plot(Carragenina, Sacarosa, pH, lwd = 2, 
                 col = 22, ylab = "pH")


# Construccion del modelo
mod.ef1 = lm(pH ~ Sacarosa*Carragenina+Tiempo) 
mod.ef1 = lm(pH ~ Sacarosa+Carragenina+Sacarosa*Carragenina+Tiempo)

model.tables(aov(mod.ef1), type = "means", se = T )


summary(aov(mod.ef1)) # Análisis de varianza

summary(mod.ef1)

library(agricolae)

cv.model(mod.ef1)

plot(mod.ef1,which=4)

model.tables(aov(mod.ef1), type = "means", se = F )

# SUPUESTOS

# Prueba de normalidad de errores
plot(mod.ef1,which=2)

shapiro.test(residuals(mod.ef1)) # Normalidad de errores

library(nortest)
ad.test(residuals(mod.ef1)) # Prueba de normalidad de errores de Anderson Darling

# Prueba de homocedasticidad de Breush Pagan
# (homogeneidad de varianzas)
plot(mod.ef1, which = 3) # La variabilidad es distinta entre los factores?

plot(mod.ef1, which = 1) # La variabilidad se incrementa con la media de Y?

library(car)

ncvTest(mod.ef1) 

# Prueba de independencia de errores

plot(residuals(mod.ef1), pch = 18,
     type = "b", ylab = "residuales",
     xlab = "Indice", main="Residuales")
abline(h=0,col="pink")

library(lmtest)

dwtest(mod.ef1,alternative = c("two.sided")) 

#EFECTOS SIMPLES

library(phia)
testInteractions(mod.ef1, fixed="Sacarosa", across="Carragenina")

aggregate(pH ~ Sacarosa*Carragenina, FUN = mean)
aggregate(pH ~ Sacarosa*Carragenina, FUN = sd)
aggregate(pH ~ Carragenina*Sacarosa, FUN = mean)
aggregate(pH ~ Carragenina*Sacarosa, FUN = sd)
testInteractions(mod.ef1, fixed="Carragenina", across="Sacarosa")

library(lsmeans)
lsmeans(mod.ef1, pairwise ~ Sacarosa|Carragenina)
lsmeans(mod.ef1, pairwise ~ Carragenina|Sacarosa)

# Prueba de LSD (o DLS que es equivalente a la prueba t)

library(agricolae)

comparaciones<-LSD.test(mod.ef1,c("Sacarosa","Carragenina"),p.adj="none",group=FALSE)
comparaciones

comparaciones1<-LSD.test(mod.ef1,c("Carragenina","Sacarosa"),p.adj="none",group=FALSE)
comparaciones1






