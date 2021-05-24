library(dplyr)
library(agricolae)
library(ggplot2)
library(gridExtra)
library(nortest)
library(ggfortify)
library(lmtest)
library(rstatix)
library(ggpubr)
library(multcomp)
library(nCDunnett)
library(DescTools)
library(fastGraph)

# Esquema experimental
trt = c("C1","C2","C3","C4")
r   = c(5,4,3,4)
(esquema = design.crd(trt,r)$book)

# Lectura de datos
datos = read.csv("datos3_2.csv",sep=";")
datos$CATA = as.factor(datos$CATA)
attach(datos)

# Estimación de efectos
(media.general <- mean(CONCENTRA))
(media.tratam   <- aggregate(CONCENTRA ~ CATA, FUN = mean)) %>% 
  rename(Media = CONCENTRA) %>% 
  mutate(Efecto = Media - media.general)

# Análisis de varianza
mod.dca2 = lm(CONCENTRA ~ CATA, data = datos)
mod.dca2 %>% aov() %>% summary()
pf(q = 9.916, df1 = 3, df2 = 12, lower.tail = FALSE)

# Coeficiente de variabilidad
mod.dca2 %>% cv.model()

# Residuales
resi2 = rstandard(mod.dca2)

# Normalidad
g1 = ggplot(mod.dca2, aes(qqnorm(.resid)[[1]], .resid)) + 
  geom_point(na.rm = TRUE) + 
  geom_abline(intercept = 0, slope = summary(mod.dca2)$sigma,
              color = "black", size = 1.5, alpha = 0.8) + 
  xlab("Cuantiles teóricos") + 
  ylab("Residuales") + 
  ggtitle("Normal Q-Q") + 
  theme_bw()

g2 = resi2 %>% as.data.frame() %>% 
  ggplot(aes(x=resi2)) + 
  geom_histogram(bins = 6) + 
  xlab("Residuales") + 
  ylab("Frecuencia") + 
  theme_bw()

ej2_normalidad = grid.arrange(g1,g2,ncol=2)

# En adelante, los gráficos generados se pueden almacenar localmente con la función ggsave:
ggsave("ej2_normalidad.png",ej2_normalidad,width=8,height=4)

shapiro.test(resi2) 
ad.test(resi2)

# Homogeneidad de varianzas
g3 = mod.dca2 %>% 
  ggplot(aes(.fitted, .resid)) + 
  geom_point(size = 3.5) + 
  #stat_smooth(method="loess") + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Valores ajustados")+ylab("Residuales") + 
  ggtitle("Residuales vs Valores ajustados") + 
  theme_bw()

g4 = mod.dca2 %>% 
  ggplot(aes(mod.dca2$model$CATA, .resid)) + 
  geom_point(size = 3.5) + 
  #stat_smooth(method="loess") + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Tratamiento")+ylab("Residuales") + 
  ggtitle("Residuales vs Valores ajustados") + 
  theme_bw()

ej2_homogeneidad <- grid.arrange(g3,g4,ncol=2)
ggsave("ej2_homogeneidad.png",ej2_homogeneidad,width=8,height=4)

bartlett.test(resi2,CATA)

# Independencia
g5 = mod.dca2 %>% 
  ggplot(aes(1:nrow(datos), .resid)) +
  geom_point(size = 3.5) +
  geom_line() +
  geom_hline(yintercept = 0) + 
  xlab("Observación") + 
  ylab("Residuales") + 
  ggtitle("Residuales ordenados según observaciones") + 
  theme_bw()

acf_ <- acf(resi2, plot = FALSE)
acf_ <- with(acf_, data.frame(lag, acf))

g6 = autoplot(acf(resi2, plot = FALSE))

ej2_independencia <- grid.arrange(g5,g6,ncol=2)
ggsave("ej2_independencia.png",ej2_independencia,width=8,height=4)

dwtest(mod.dca2, alternative =c("two.sided"))

# Prueba LSD
lsd_grupo   = LSD.test(mod.dca2, "CATA",group = TRUE, alpha = 0.05, p.adj = "bonferroni", console=TRUE)
lsd_nogrupo = LSD.test(mod.dca2, "CATA",group = FALSE, alpha = 0.05, p.adj = "bonferroni", console=TRUE)

png("ej2_lsd.png", width=500,height=500)
plot(lsd_grupo, variation = "range")
dev.off()

# Prueba HSD
hsd_grupo   = HSD.test(mod.dca2, "CATA",group = TRUE, alpha = 0.05,console=TRUE)
hsd_nogrupo = HSD.test(mod.dca2, "CATA",group = FALSE, alpha = 0.05, console=TRUE)

png("ej2_hsd.png", width=500,height=500)
plot(hsd_grupo, variation = "range")
dev.off()

pkw <- datos %>% 
  rstatix::tukey_hsd(CONCENTRA ~ CATA) %>% 
  add_xy_position(x = "type")

hsd_sign = ggbarplot(
  datos, x = "CATA", y = "CONCENTRA", 
  add = c("mean_sd"),color="royalblue1", 
  fill= "dodgerblue", ylab=c("% de germinación"), xlab=c("Tratamiento")) +
  stat_pvalue_manual(pkw, hide.ns = TRUE) +
  labs(title = "Comparación mediante el criterio HSD Tukey")

ggsave("ej2_hsd_sig.png",hsd_sign,scale=1.15)
dev.off()

# Prueba SNK
snk_grupo   = SNK.test(mod.dca2, "CATA",group = TRUE, alpha = 0.05, console=TRUE)
snk_nogrupo = SNK.test(mod.dca2, "CATA",group = FALSE, alpha = 0.05, console=TRUE)

png("ej2_snk.png", width=500,height=500)
plot(snk_grupo, variation = "range")
dev.off()

# Duncan
duncan_grupo   = duncan.test(mod.dca2, "CATA",group = TRUE,alpha = 0.05, console=TRUE)
duncan_nogrupo = duncan.test(mod.dca2, "CATA",group = FALSE,alpha = 0.05, console=TRUE)

png("ej2_duncan.png", width=500,height=500)
plot(duncan_grupo, variation = "range")
dev.off()


library(multcomp)
contraste <- rbind(c(35,0,-15,-20)) 
compara <-glht(mod.dca2, linfct = mcp(CATA = contraste),
               rhs =175, alternative = c("two.sided"))  
compara %>% summary()

graf_t = ggplot() +
  stat_function(fun = dt,
                args = list(df = 12),
                geom = "area",
                fill = "steelblue",
                alpha = .3) +
  stat_function(fun = dt,
                args = list(df = 12),
                geom = "area",
                fill = "steelblue",
                xlim = c(qt(.975,18), 3)) + 
  stat_function(fun = dt,
                args = list(df = 12),
                geom = "area",
                fill = "steelblue",
                xlim = c(-3,qt(.025,12)))+
  labs(y = "f(x)", x = "x") + 
  scale_x_continuous(limits = c(-3,3),
                     breaks=c(-4,qt(.025,12),0,qt(.975,12),4))+
  theme_classic() + 
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))

ggsave("ej2_contraste.png", plot = graf_t, width = 6, height = 4)


