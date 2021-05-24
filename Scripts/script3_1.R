
# Esquema experimental
trt = c("ACSULF","KNA","ASNA","REMOJO","ESCARLIJ","TESTIGO")
r   = rep(4,6)
library(agricolae)
(esquema = design.crd(trt,r)$book)

# Lectura de datos
datos = read.csv("datos3_1.csv", sep = ";")
datos$TRATA = as.factor(datos$TRATA)
attach(datos)

# Estimación de efectos
library(dplyr)
(media.general  <- mean(GERMINA))
(media.tratam   <- aggregate(GERMINA ~ TRATA, FUN = mean)) %>% 
  rename(Media = GERMINA) %>% 
  mutate(Efecto = Media - media.general)

# Análisis de varianza
mod.dca1 = lm(GERMINA ~ TRATA, data = datos)
mod.dca1 %>% aov() %>% summary()
pf(q = 31.8275, df1 = 5, df2 = 18, lower.tail = FALSE)

# Coeficiente de variabilidad
mod.dca1 %>% cv.model()

# Residuales
resi1 = rstandard(mod.dca1)

# Normalidad
library(ggplot2)
g1 = ggplot(mod.dca1, aes(qqnorm(.resid)[[1]], .resid)) + 
  geom_point(na.rm = TRUE, size = 3) + 
  geom_abline(intercept = 0, slope = summary(mod.dca1)$sigma,
              color = "black", size = 1.5, alpha = 0.8) + 
  xlab("Cuantiles teóricos") + 
  ylab("Residuales") + 
  ggtitle("Normal Q-Q") + 
  theme_bw()

g2 = resi1 %>% as.data.frame() %>% 
  ggplot(aes(x=resi1)) + 
  geom_histogram(bins = 6) + 
  xlab("Residuales") + 
  ylab("Frecuencia") +
  ggtitle("Distribución de los residuales")
theme_bw()

library(gridExtra)
ej1_normalidad = grid.arrange(g1,g2,ncol=2)
ggsave("ej1_normalidad.png",ej1_normalidad,width=8,height=4)

shapiro.test(resi1) 
library(nortest)
ad.test(resi1)

# Homogeneidad de varianzas
g3 = mod.dca1 %>% 
  ggplot(aes(.fitted, .resid)) + 
  geom_point(size = 3.5) + 
  #stat_smooth(method="loess") + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Valores ajustados")+ylab("Residuales") + 
  ggtitle("Residuales vs Valores ajustados") + 
  theme_bw()

g4 = mod.dca1 %>% 
  ggplot(aes(mod.dca1$model$TRATA, .resid)) + 
  geom_point(size = 3.5) + 
  #stat_smooth(method="loess") + 
  geom_hline(yintercept=0, col="red", linetype="dashed") + 
  xlab("Tratamiento")+ylab("Residuales") + 
  ggtitle("Residuales vs Valores ajustados") + 
  theme_bw()

ej1_homogeneidad <- grid.arrange(g3,g4,ncol=2)
ggsave("ej1_homogeneidad.png",ej1_homogeneidad,width=8,height=4)

bartlett.test(resi1,TRATA)

# Independencia
g5 = as.data.frame(resi1) %>% 
  ggplot(aes(x=1:length(resi1),y=resi1)) +
  geom_point(size = 3.5) +
  geom_line() +
  geom_hline(yintercept = 0) + 
  xlab("Observación") + 
  ylab("Residuales") + 
  ggtitle("Residuales ordenados según observación") + 
  theme_bw()

acf_ <- acf(resi1, plot = FALSE)
acf_ <- with(acf_, data.frame(lag, acf))

library(ggfortify)
g6 = autoplot(acf(resi1, plot = FALSE))

ej1_independencia <- grid.arrange(g5,g6,ncol=2)
ggsave("ej1_independencia.png",ej1_independencia,width=8,height=4)

library(lmtest)
dwtest(mod.dca1, alternative =c("two.sided"))

# Prueba t
(tc = (65.5-55)/sqrt(2*16.9583/4))
pt(q = tc,df = 18,lower.tail = F)

# Prueba LSD
lsd_grupo   = LSD.test(mod.dca1, "TRATA",group = TRUE, alpha = 0.05, console=TRUE)
lsd_nogrupo = LSD.test(mod.dca1, "TRATA",group = FALSE, alpha = 0.05, console=TRUE)

png("ej1_lsd.png", width=500,height=500)
plot(lsd_grupo, variation = "range")
dev.off()

# Prueba HSD
hsd_grupo   = HSD.test(mod.dca1, "TRATA",group = TRUE, alpha = 0.05,console=TRUE)
hsd_nogrupo = HSD.test(mod.dca1, "TRATA",group = FALSE, alpha = 0.05, console=TRUE)
qtukey(p = 0.95, nmeans = 6, df =  18) 

media1 = 65.5 
media2 = 55 
CME    = 16.9583
Vc     = abs((media1-media2)/sqrt(CME/4))
1-ptukey(q = Vc, nmeans = 6, df = 18)

png("ej1_hsd.png", width=500,height=500)
plot(hsd_grupo, variation = "range")
dev.off()

library(rstatix)
library(ggpubr)

pkw <- datos %>% 
  rstatix::tukey_hsd(GERMINA ~ TRATA) %>% 
  add_xy_position(x = "type")

hsd_sign = ggbarplot(
  datos, x = "TRATA", y = "GERMINA", 
  add = c("mean_sd"),color="royalblue1", 
  fill= "dodgerblue", ylab=c("% de germinación"), xlab=c("Tratamiento")) +
  stat_pvalue_manual(pkw, hide.ns = TRUE) +
  labs(title = "Comparación mediante el criterio HSD Tukey")

ggsave("ej1_hsd_sig.png",hsd_sign,scale=1.15)
dev.off()

# Prueba SNK
snk_grupo   = SNK.test(mod.dca1, "TRATA",group = TRUE, alpha = 0.05, console=TRUE)
snk_nogrupo = SNK.test(mod.dca1, "TRATA",group = FALSE, alpha = 0.05, console=TRUE)
qtukey(0.90, 2, df =  8) 
qtukey(0.90, 3, df =  8) 
qtukey(0.90, 4, df =  8) 

png("ej1_snk.png", width=500,height=500)
plot(snk_grupo, variation = "range")
dev.off()

# Duncan
duncan_grupo   = duncan.test(mod.dca1, "TRATA",group = TRUE,alpha = 0.05, console=TRUE)
duncan_nogrupo = duncan.test(mod.dca1, "TRATA",group = FALSE,alpha = 0.05, console=TRUE)

png("ej1_duncan.png", width=500,height=500)
plot(duncan_grupo, variation = "range")
dev.off()

# Prueba Dunnett
datos$TRATA_ = factor(datos$TRATA, 
                      levels=c("TESTIGO","ACSULF","ASNA","ESCARLIJ","KNA","REMOJO"))
mod.dca1_ = lm(GERMINA ~ TRATA_, data = datos)

library(multcomp)
dunnett = glht(mod.dca1_, linfct = mcp(TRATA_ = "Dunnett"))
summary(dunnett)
plot(dunnett,cex.axis=0.9,las=3)

png("ej1_dunnett.png", width=800,height=800)
plot(dunnett,cex.axis=0.9,las=3)
dev.off()

library(nCDunnett)
qNCDun(0.95, 18, rep.int(0.5,5), rep.int(0,5), 64, two.sided=T)

Vc = abs(22.75)/sqrt(2*16.9583/4)
1-pNCDun(Vc, 18, rep.int(0.5,5), rep.int(0,5), 64, two.sided=T)

Vc = abs(12.25)/sqrt(2*16.9583/4)
1-pNCDun(Vc, 18, rep.int(0.5,5), rep.int(0,5), 64, two.sided=T)

Vc = abs(11.50)/sqrt(2*16.9583/4)
1-pNCDun(Vc, 18, rep.int(0.5,5), rep.int(0,5), 64, two.sided=T)

Vc = abs(-7.75)/sqrt(2*16.9583/4)
1-pNCDun(Vc, 18, rep.int(0.5,5), rep.int(0,5), 64, two.sided=T)

Vc = abs(-4)/sqrt(2*16.9583/4)
1-pNCDun(Vc, 18, rep.int(0.5,5), rep.int(0,5), 64, two.sided=T)

library(DescTools)
DunnettTest(x=GERMINA, g=as.factor(TRATA),  data = datos, control="TESTIGO", conf.level=0.95)

# Contrastes

graf_t = ggplot() +
  stat_function(fun = dt,
                args = list(df = 18),
                geom = "area",
                fill = "steelblue",
                alpha = .3) +
  stat_function(fun = dt,
                args = list(df = 18),
                geom = "area",
                fill = "steelblue",
                xlim = c(qt(.975,18), 3)) + 
  stat_function(fun = dt,
                args = list(df = 18),
                geom = "area",
                fill = "steelblue",
                xlim = c(-3,qt(.025,18)))+
  labs(y = "f(x)", x = "x") + 
  scale_x_continuous(limits = c(-3,3),
                     breaks=c(-4,qt(.025,18),0,qt(.975,18),4))+
  theme_classic() + 
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))

ggsave("ej1_contraste.png", plot = graf_t, width = 6, height = 4)

contraste <- rbind(c(0.5,-1,0,0.5,0,0)) 
compara <-glht(mod.dca1, linfct = mcp(TRATA = contraste)) 
compara %>% summary()

graf_t2 = ggplot() +
  stat_function(fun = dt,
                args = list(df = 18),
                geom = "area",
                fill = "steelblue",
                alpha = .3) +
  stat_function(fun = dt,
                args = list(df = 18),
                geom = "area",
                fill = "steelblue",
                xlim = c(qt(.95,18), 3)) + 
  labs(y = "f(x)", x = "x") + 
  scale_x_continuous(limits = c(-3,3),
                     breaks=c(-4,0,qt(.95,18),4))+
  theme_classic() + 
  theme(axis.text=element_text(size=12,face="bold"),
        axis.title=element_text(size=12,face="bold"))

ggsave("ej1_contraste2.png", plot = graf_t2, width = 6, height = 4)

contraste <- rbind(c(1/3,1/3,0,1/3,-1,0)) 
compara <-glht(mod.dca1, linfct = mcp(TRATA = contraste), 
               rhs = 7.5, alternative = c("greater")) 
compara %>% summary()
