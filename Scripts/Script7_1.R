
#################################################################################
############## Caso 1 - DCA:Sobrevivencia VS Temperatura y Verdura  #############
#################################################################################


# Sobrevivencia VS Temperatura y Verdura

D = expand.grid( Temperatura = c("T1","T2","T3"), 
                 Verdura = c("B","L","P","Z") )
D
D = rbind(D,D,D,D,D,D)
D
set.seed(153)
D = D[order(sample(1:24)), ]
D

datos = read.delim("clipboard",T) # lectura de datos
head(datos) # presentacion de datos
str(datos)

# Algunas funciones necesitan que los factores se trabajen como factores
# y no como caracteres
is.factor(datos$temperatura)
is.factor(datos$verdura)
datos$temperatura=as.factor(datos$temperatura)
datos$verdura=as.factor(datos$verdura)
str(datos)

attach(datos) # Asignacion de variables

interaction.plot(temperatura, verdura, sobrevivencia, lwd = 4, col = "black",
                 ylab = "Sobrevivencia")
interaction.plot(verdura, temperatura, sobrevivencia, lwd = 2, col = "black",
                 ylab = "Sobrevivencia")

#De Ana Beatriz Villaseñor Altamirano para todos:  01:26 PM
#Ese código te genera la gráfica de líneas

library("ggpubr")
ggline(datos, x = "temperatura", y = "sobrevivencia",
       color = "verdura",title = "Gráfico de interacción de Temperatura vs Verdura",
       palette = "Set2"
)

ggline(datos, x = "verdura", y = "sobrevivencia",
       color = "temperatura", title = "Gráfico de interacción de Verdura vs Temperatura",
       palette = "Set2"
)

# Construccion del modelo

modelo = lm(sobrevivencia ~ temperatura*verdura,data=datos) 
modelo = lm(sobrevivencia ~ temperatura+verdura+temperatura*verdura,data=datos)
modelo

aggregate(sobrevivencia ~ temperatura, FUN = sum)
aggregate(sobrevivencia ~ verdura, FUN = sum)
aggregate(sobrevivencia ~ temperatura*verdura, FUN = sum)
aggregate(sobrevivencia ~ temperatura, FUN = mean)
aggregate(sobrevivencia ~ verdura, FUN = mean)
aggregate(sobrevivencia ~ temperatura*verdura, FUN = mean)
1412/72
aggregate(sobrevivencia ~ verdura*temperatura, FUN = sum)
aggregate(sobrevivencia ~ verdura*temperatura, FUN = mean)


modelo=lm(sobrevivencia ~ temperatura*verdura,data=datos)
summary(aov(modelo))
anova(modelo)
qf(0.95,6,60,lower.tail = F)
model.tables(aov(modelo), type = "means", se = F )
model.tables(aov(modelo), type = "effects", se = T )
?model.tables

library(agricolae)
cv.model(modelo)
6.22^.5/19.61111 

### SUPUESTOS ####

# Prueba de normalidad de errores
plot(modelo,which=2)
shapiro.test(residuals(modelo)) 

head(datos)

datos %>%
  group_by(temperatura, verdura) %>%
  shapiro_test(sobrevivencia) ### Shapiro para verificar la normalidad (paquete rstatix)


library(nortest)
ad.test(residuals(modelo)) # Prueba de normalidad de errores de Anderson Darling

# Prueba de homocedasticidad de Breush Pagan y Levene
# (homogeneidad de varianzas)
plot(modelo, which = 3) # La variabilidad es distinta entre los factores?
plot(modelo, which = 1) # La variabilidad se incrementa con la media de Y?
library(car)
ncvTest(modelo)

pchisq(5.365112,1,lower.tail=FALSE)#pvalor

qchisq(0.05,1,lower.tail=FALSE)
library(fastGraph)
shadeDist( 3.841459, "dchisq", 1, 
           lower.tail=FALSE,col = c("greenyellow","mediumorchid2") )

plot(residuals(modelo), pch = 18,
     type = "b", ylab = "residuales",
     xlab = "Indice", main="Residuales")
abline(h=0,col=11)

library("rstatix")
head(datos)
levene_test(modelo, center=mean)### Test de Levene
levene_test(datos, sobrevivencia~as.factor(temperatura)*as.factor(verdura), center=mean)

# Prueba de independencia de errores
library(lmtest)
dwtest(modelo,alternative = c("two.sided")) 

# Más Estadísticas descriptivas:
library(psych)
describeBy(sobrevivencia,list(temperatura),digits=2)
describeBy(sobrevivencia,list(verdura),digits=2)
describeBy(sobrevivencia,list(temperatura,verdura),digits=2)
describeBy(sobrevivencia,list(verdura,temperatura),digits=2)

# Medias estadísticas de las interacciones bajo el modelo:
library(phia)
library(car)
library(carData)

(means.sobrevivencia = interactionMeans(modelo))
plot(means.sobrevivencia, abbrev.levels=TRUE)
library(phia)
interactionMeans(modelo)

library(phia)
#Efectos simples de Ab1, Ab2 y Ab3
testInteractions(modelo, fixed="temperatura", across="verdura",adjustment="none")

#Efectos simples de Ba1 y Ba2
testInteractions(modelo, fixed="verdura", across="temperatura")

# PRUEBAS DE EFECTOS SIMPLES

# Comparación de medias mínimo cuadráticas
library(lsmeans)

#Efectos simples de Ab1, Ab2 y Ab3
lsmeans(modelo, pairwise ~ temperatura|verdura)

#Efectos simples de Ba1 y Ba2
lsmeans(modelo, pairwise ~ verdura|temperatura)

# Prueba de Tukey

###### Test a posteriori###### 

head(datos)

phtukey <- datos %>% 
  group_by(temperatura) %>%
  tukey_hsd(sobrevivencia ~ verdura) #### Test a posteriori de Tukey

phtukey
?tukey_hsd

#### Grafico de los resultados 

phtukey <- phtukey %>% add_xy_position(x = "temperatura")

res.aov4a <- datos %>% anova_test(sobrevivencia~ temperatura * verdura)
res.aov4a
plot (res.aov4a)

Fig_6a <- ggbarplot(
  datos, x = "temperatura", y = "sobrevivencia", 
  fill="verdura",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Sobrevivencia"), xlab=c("Temperatura")
)
Fig_6a + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )


Fig_6a + stat_pvalue_manual(phtukey) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )
Fig_6a + stat_pvalue_manual(phtukey, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

#####

phtukey <- datos %>% 
  group_by(verdura) %>%
  tukey_hsd(sobrevivencia ~ temperatura) #### Test a posteriori de Tukey

phtukey

phtukey <- phtukey %>% add_xy_position(x = "verdura")


Fig_6b <- ggbarplot(
  datos, x = "verdura", y = "sobrevivencia", 
  fill="temperatura",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Sobrevivencia"), xlab=c("Verduras")
)
Fig_6b + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

Fig_6b + stat_pvalue_manual(phtukey, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

?stat_pvalue_manual

library(agricolae)
comparaciones<-HSD.test(modelo,c("verdura","temperatura"),group=FALSE)
comparaciones

comparaciones1<-HSD.test(modelo,c("temperatura","verdura"),group=FALSE)
comparaciones1

# Prueba de LSD (o DLS que es equivalente a la prueba t)

comparaciones<-LSD.test(modelo,c("verdura","temperatura"),p.adj="none",group=FALSE)
comparaciones

comparaciones1<-LSD.test(modelo,c("temperatura","verdura"),p.adj="none",group=FALSE)
comparaciones1


# Potencia de la prueba (Este tema no se evaluará)

library(daewr)
rmin = 2 
rmax = 10
sigma = sigma(mod.ef1)
alpha = 0.10
Delta = 3
nlev  = c(2,3)
nreps = c(rmin:rmax)
power = Fpower2(alpha, nlev, nreps, Delta, sigma)
power
