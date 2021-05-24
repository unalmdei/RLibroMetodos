#Ejercicios Propuestos: Ejercicio 3
#########################################
#carga de librerias a usar
library(dplyr)
library(agricolae)
library(ggplot2)
library(gridExtra)
library(nortest)
library(lmtest)
library(multcomp)

#Lectura de datos
datos = read.csv("datos3_5.csv", sep = ";")
datos$Tratamientos<-as.factor(datos$Tratamientos)
Tratamientos=datos[,1]
Y=datos[,2]

#Pregunta b
modelo=lm(Y~Tratamientos)
anova(modelo , test="F")

#Pregunta c
 #Normalidad de los errores
resi3 = rstandard(modelo)
ad.test(resi3)

  #Grafico:
g1 = ggplot(modelo, aes(qqnorm(resi3)[[1]], .resid)) +
  geom_point(na.rm = TRUE) +
  geom_abline(intercept = 0, slope = summary(modelo)$sigma,
              color = "black", size = 1.5, alpha = 0.8) +
  xlab("Cuantiles teóricos") +
  ylab("Residuales estandarizados") +
  ggtitle("Normal Q-Q") +
  theme_bw()

g2 = resi3 %>% as.data.frame() %>%
  ggplot(aes(x=resi3)) +
  geom_histogram(bins = 6) +
  xlab("Residuales estandarizados") +
  ylab("Frecuencia") +
  theme_bw()

windows(width = 600, height = 300);grid.arrange(g1,g2,ncol=2)

 #Homogenidad de varianzas
bartlett.test(resi3,Tratamientos)

  #Gráfico: 
g3 = modelo %>%
  ggplot(aes(.fitted, .resid)) +
  geom_point(size = 3.5) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  xlab("Valores ajustados")+ylab("Residuales") +
  ggtitle("Residuales vs Valores ajustados") +
  theme_bw()

g4 = modelo %>%
  ggplot(aes(modelo$model$Tratamientos, .resid)) +
  geom_point(size = 3.5) +
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  xlab("Tratamiento")+ylab("Residuales") +
  ggtitle("Residuales vs Valores ajustados") +
  theme_bw()

windows(width = 700, height = 350);grid.arrange(g3,g4,ncol=2)

 #Independencia
dwtest(modelo,alternative =c("two.sided"))

  #Gráfico:
g5 = as.data.frame(resi3) %>%
  ggplot(aes(1:length(resi3), y=resi3)) +
  geom_point(size = 3.5) +
  geom_line() +
  geom_hline(yintercept = 0) +
  xlab("Observación")+ylab("Residuales") +
  ggtitle("Residuales ordenados según observaciones") +
  theme_bw()

windows(width = 300, height = 300);g5

#Pregunta d

  #-Prueba de Tukey
outHSD<-HSD.test(modelo, "Tratamiento",console=TRUE, group=FALSE)

#Pregunta e

#-Prueba de LSD (T2-T3)
m<-subset(datos,Tratamientos=="T2")
m2<-subset(datos,Tratamientos=="T3")
trata<-rbind(m,m2)
Tr=trata[,1]
Y3=trata[,2]
modelo2=lm(Y3~Tr)
outLSD <-LSD.test(modelo2, "Tr",console=TRUE, group = FALSE)

#Pregunta f
  #DUNET
library(multcomp)
g2=glht(modelo, linfct = mcp(Tratamientos = "Dunnett"))
summary(g2)

#Pregunta g
contraste <- rbind(c(0,2,-1,-1)) 
compara <-glht(modelo, linfct = mcp(Tratamientos = contraste)) 
compara %>% summary()
