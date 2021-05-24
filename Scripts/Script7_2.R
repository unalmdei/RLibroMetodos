
#################################################################################
############## Caso 2 - DBCA: Fenoles VS Parte, Solvente y Variedad #############
#################################################################################

D = expand.grid( Parte = c("P1","P2","P3"), 
                 Solvente = c("S1","S2","S3","S4"), Variedad=c("V1","V2") )
D

set.seed(153)

D = D[order(sample(1:32)), ]
D

datos = read.delim("clipboard",T) # lectura de datos
datos
head(datos) # presentacion de datos
str(datos)
datos$Parte=as.factor(datos$Parte)
datos$Solvente=as.factor(datos$Solvente)
datos$Variedad=as.factor(datos$Variedad)

attach(datos)  # Asignacion de variables

aggregate(Fenoles ~ Parte, FUN = sum)
aggregate(Fenoles ~ Solvente, FUN = sum)
aggregate(Fenoles ~ Variedad, FUN = sum)
aggregate(Fenoles ~ Parte*Solvente, FUN = sum)
aggregate(Fenoles ~ Parte, FUN = mean)
aggregate(Fenoles ~ Solvente, FUN = mean)
aggregate(Fenoles ~ Parte*Solvente, FUN = mean)
aggregate(Fenoles ~ Variedad, FUN = mean)

#### Grafico de Interaccion 

interaction.plot(Parte, Solvente, Fenoles, lwd = 2, 
                 col = 18, ylab = "Fenoles")
interaction.plot(Solvente, Parte, Fenoles, lwd = 2, 
                 col = 22, ylab = "Fenoles")

#De Ana Beatriz Villaseñor Altamirano para todos:  01:26 PM
#Ese código te genera la gráfica de líneas

library("ggpubr")
ggline(datos, x = "Parte", y = "Fenoles",
       color = "Solvente",title = "Gráfico de interacción de Parte vs Solvente",
       palette = "Set2"
)

ggline(datos, x = "Solvente", y = "Fenoles",
       color = "Parte", title = "Gráfico de interacción de solvente vs Parte",
       palette = "Set2"
)

# Construccion del modelo
mod.ef1 = lm(Fenoles ~ Parte*Solvente+Variedad,data=datos)
modelo = lm(Fenoles ~ Parte*Solvente,data=datos)

model.tables(aov(mod.ef1), type = "means", se = T )

summary(aov(mod.ef1)) # Analisis de varianza
summary(mod.ef1)

library(agricolae)
cv.model(mod.ef1)

# La distancia de Cook detecta valores influyentes. Un valor es influyente
# si su distancia de Cook es mayor que 1.
plot(mod.ef1,which=4)

# SUPUESTOS

# Prueba de normalidad de errores
plot(mod.ef1,which=2)

shapiro.test(residuals(mod.ef1)) # Normalidad de errores

library(nortest)
ad.test(residuals(mod.ef1)) # Prueba de normalidad de errores de Anderson Darling

# Prueba de homocedasticidad de Breush Pagan
# (homogeneidad de varianzas)

library("rstatix")
head(datos)
levene_test(modelo, center=mean)### Test de Levene
levene_test(datos, Fenoles~as.factor(Parte)*as.factor(Solvente), center=mean)

plot(mod.ef1, which = 3) # La variabilidad es distinta entre los factores?
plot(mod.ef1, which = 1) # La variabilidad se incrementa con la media de Y?
library(car)
ncvTest(mod.ef1) 
pchisq(0.0102957,1,lower.tail=FALSE)#pvalor

qchisq(0.05,1,lower.tail=FALSE)
library(fastGraph)
shadeDist( 3.841459, "dchisq", 1, 
           lower.tail=FALSE,col = c("greenyellow","mediumorchid2") )

# Prueba de independencia de errores
plot(residuals(mod.ef1), pch = 18,
     type = "b", ylab = "residuales",
     xlab = "Indice", main="Residuales")
abline(h=0,col="pink")

library(lmtest)
dwtest(mod.ef1,alternative = c("two.sided")) 

library(phia)
interactionMeans(mod.ef1)

library(phia)
#Efectos simples de P(S1), P(S2), P(S3) y P(S4)
testInteractions(mod.ef1, fixed="Solvente", across="Parte",adjustment="none")

#Efectos simples de S(P1), S(P2) y S(P3)
testInteractions(mod.ef1, fixed="Parte", across="Solvente")

# PRUEBAS DE EFECTOS SIMPLES

# Comparación de medias mínimo cuadráticas
library(lsmeans)

#Efectos simples de P(S1), P(S2), P(S3) y P(S4)
lsmeans(modelo, pairwise ~ temperatura|verdura)

#Efectos simples de S(P1), S(P2) y S(P3)
lsmeans(modelo, pairwise ~ verdura|temperatura)

# Prueba de Tukey

###### Test a posteriori###### 

library("rstatix")

head(datos)

phtukey <- datos %>% 
  group_by(Parte) %>%
  tukey_hsd(Fenoles ~ Solvente) #### Test a posteriori de Tukey

phtukey

?tukey_hsd

#### Grafico de los resultados 

phtukey <- phtukey %>% add_xy_position(x = "Parte")

res.aov4a <- datos %>% anova_test(Fenoles~ Parte * Solvente+Variedad)
res.aov4a
plot (res.aov4a)

Fig_6a <- ggbarplot(
  datos, x = "Parte", y = "Fenoles", 
  fill="Solvente",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Fenoles"), xlab=c("Parte")
)
Fig_6a + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

Fig_6a + stat_pvalue_manual(phtukey) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )
Fig_6a + stat_pvalue_manual(phtukey, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

#####

head(datos)

phtukey1 <- datos %>% 
  group_by(Solvente) %>%
  tukey_hsd(Fenoles ~ Parte) #### Test a posteriori de Tukey

phtukey1
head(phtukey1)
View(phtukey1)


#### Grafico de los resultados 

phtukey1 <- phtukey1 %>% add_xy_position(x = "Solvente")

res.aov4a <- datos %>% anova_test(Fenoles~ Parte * Solvente+Variedad)
res.aov4a
plot (res.aov4a)

Fig_6b <- ggbarplot(
  datos, x = "Solvente", y = "Fenoles", 
  fill="Parte",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Fenoles"), xlab=c("Solvente")
)
Fig_6b + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

Fig_6b + stat_pvalue_manual(phtukey1) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )
Fig_6b + stat_pvalue_manual(phtukey1, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )


comparaciones<-HSD.test(mod.ef1,c("Solvente","Parte"),group=FALSE)
comparaciones

comparaciones1<-HSD.test(mod.ef1,c("Parte","Solvente"),group=FALSE)
comparaciones1

# Prueba de LSD (o DLS que es equivalente a la prueba t)

head(datos)

prueba_t=datos %>%
  group_by(Parte) %>%
  t_test(Fenoles~ Solvente) %>%
  add_significance("p.adj")

prueba_t
View(prueba_t)
filter(prueba_t[-c(5,6)])
?t_test
#### Grafico de los resultados 

prueba_t <- prueba_t %>% add_xy_position(x = "Parte")

res.aov4a <- datos %>% anova_test(Fenoles~ Parte * Solvente+Variedad)
res.aov4a
plot (res.aov4a)

Fig_6a <- ggbarplot(
  datos, x = "Parte", y = "Fenoles", 
  fill="Solvente",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Fenoles"), xlab=c("Parte")
)
Fig_6a + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

Fig_6a + stat_pvalue_manual(prueba_t) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )
Fig_6a + stat_pvalue_manual(prueba_t, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )



head(datos)

prueba_t1=datos %>%
  group_by(Solvente) %>%
  t_test(Fenoles ~ Parte) %>%
  add_significance("p.adj")

prueba_t1
filter(prueba_t1[-c(5,6,8)])
#### Grafico de los resultados 

prueba_t1 <- prueba_t1 %>% add_xy_position(x = "Solvente")

res.aov4a <- datos %>% anova_test(Fenoles~ Parte * Solvente+Variedad)
res.aov4a
plot (res.aov4a)

Fig_6b <- ggbarplot(
  datos, x = "Solvente", y = "Fenoles", 
  fill="Parte",  add = "mean_sd", palette="Spectral",
  position = position_dodge(0.8),
  ylab=c("Fenoles"), xlab=c("Solvente")
)
Fig_6b + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )

Fig_6b + stat_pvalue_manual(prueba_t1) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )
Fig_6b + stat_pvalue_manual(prueba_t1, hide.ns = TRUE) + labs( subtitle = get_test_label(res.aov4a, detailed = TRUE)  )


comparaciones<-LSD.test(mod.ef1,c("Solvente","Parte"),p.adj="none",group=FALSE)
comparaciones

comparaciones1<-LSD.test(mod.ef1,c("Parte","Solvente"),p.adj="none",group=FALSE)
comparaciones1

Pvalor = pt(1.389, 60, lower.tail=F)     
Pvalor
