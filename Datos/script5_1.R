
# Esquema experimental
library(agricolae)
trt = c("A","B","C","D")
(esquema = design.lsd(trt)$book)

# Lectura de datos
library(readxl)
datos = read_excel("datos5_1.xlsx")
datos$Abono = as.factor(datos$Abono)
datos$Insecticida = as.factor(datos$Insecticida)
attach(datos)

# Estadística descriptiva
attach(datos)
MediaTrat=tapply(Rendimiento,Variedad, mean)
SdTrat= tapply(Rendimiento,Variedad, sd)
df1 = data.frame(MediaTrat,SdTrat)
library(knitr)
kable(df1,caption = "Descriptivos Tratamientos (Variedad)",
      digits = 2, format.args = list( decimal.mark = "."))

MediaBF=tapply(Rendimiento,Abono, mean)
SdBF= tapply(Rendimiento,Abono, sd)
df2 = data.frame(MediaBF,SdBF)
kable(df2,caption = "Descriptivos Bloque Fila (Abono)",
      digits = 2, format.args = list( decimal.mark = "."))

MediaBC=tapply(Rendimiento,Insecticida, mean)
SdBC= tapply(Rendimiento,Insecticida, sd)
df3 = data.frame(MediaBC,SdBC)
kable(df3,caption = "Descriptivos Bloque Fila (Insecticida)",
      digits = 2, format.args = list( decimal.mark = "."))

library(ggplot2)
ggplot(data=datos, aes(x=Variedad,y=Rendimiento, color=Variedad))+
  geom_boxplot(show.legend = FALSE) +
  xlab("Variedad") +  
  ylab("Rendimiento (kg/parcela)")+
  ggtitle("Gráfico de cajas del Rendimiento según Variedad")

# Análisis de varianza
modelo.dcl = lm(Rendimiento ~ Variedad+Abono+Insecticida)
summary(aov(modelo.dcl))

# Coeficiente de variabilidad
cv.model(modelo.dcl)

# Residuales
residuales = residuals(modelo.dcl)

# Normalidad de errores
shapiro.test(residuales)

# Homogeneidad de varianzas de errores
bartlett.test(residuales~Variedad)

# Independencia de errores
library(lmtest)
dwtest(modelo.dcl, alternative="two.sided")

# Comparación de medias
library(dplyr)
HSD.test(modelo.dcl,trt="Variedad",group=F) %>% print()

qtukey(p = 0.95, nmeans = 4, df =  6) 
1-ptukey(0.175,nmeans = 4,df = 6)

HSD.test(modelo.dcl,trt="Variedad",group=T) %>% plot()
