#Ejercicios Propuestos: Ejercicio 1
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
datos = read.csv("datos3_3.csv", sep = ";")
datos$Tratamientos<-as.factor(datos$Tratamientos)
Tratamientos=datos[,1]
Y=datos[,2]

#Pregunta b
modelo=lm(Y~Tratamientos)
anova(modelo , test="F")

#Pregunta c
 #Normalidad de los errores
resi1 = rstandard(modelo)
ad.test(resi1)

   #Grafico:
g1 = ggplot(modelo, aes(qqnorm(resi1)[[1]], .resid)) +
  geom_point(na.rm = TRUE) +
  geom_abline(intercept = 0, slope = summary(modelo)$sigma,
              color = "black", size = 1.5, alpha = 0.8) +
  xlab("Cuantiles teóricos") +
  ylab("Residuales estandarizados") +
  ggtitle("Normal Q-Q") +
  theme_bw()

g2 = resi1 %>% as.data.frame() %>%
  ggplot(aes(x=resi1)) +
  geom_histogram(bins = 6) +
  xlab("Residuales estandarizados") +
  ylab("Frecuencia") +
  theme_bw()

windows(width = 600, height = 300);grid.arrange(g1,g2,ncol=2)

 #Homogenidad de varianzas
bartlett.test(resi1,Tratamientos)

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
g5 = as.data.frame(resi1) %>%
  ggplot(aes(1:length(resi1), y=resi1)) +
  geom_point(size = 3.5) +
  geom_line() +
  geom_hline(yintercept = 0) +
  xlab("Observación")+ylab("Residuales") +
  ggtitle("Residuales ordenados según observaciones") +
  theme_bw()

windows(width = 300, height = 300);g5

#Pregunta d
cv.model(modelo)

#Pregunta e

  #Prueba de Tukey
outHSD<-HSD.test(modelo, "Tratamiento",console=TRUE, group=FALSE)

#Pregunta f

(tc = ((75.25-66.25)-2)/sqrt(2*5.88/4))
pt(q = tc,df = 15,lower.tail = F)

#Pregunta g

contraste <-rbind(c(0,1,1,-1,-1))
compara <-glht(modelo, linfct = mcp(Tratamientos = contraste)) 
compara %>% summary()
