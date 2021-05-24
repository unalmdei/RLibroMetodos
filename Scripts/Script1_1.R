
### Ejercicios de aplicaci贸n ####
# 1.2. Ejemplo aplicaci贸n 1
observados <- c(59,15,2,4)
probabilidad_esperada <-  c(9/16,3/16,3/16,1/16)
chisq.test(observados, p=probabilidad_esperada)


#1.2  Ejemplo de aplicaci贸n 2
#Parte 1
o<-c(36,48,38,23,10,3,1,1,0,0)
x<-c(0,1,2,3,4,5,6,7,8,9)
Ex<-sum(o*x)/sum(o)
Ex

r<-9
p<-Ex/r
p

pe<-round(dbinom(x,r,p),7)
pe
n<-sum(o)
n
e<-n*pe
e
table<-data.frame(x,o,pe,e,chi=(o-e)^2/e)
table

#Parte 2
x<-0:4
o<-c(36,48,38,23,15)
pe<-dbinom(0:3,9,0.18125)-(1-pbinom(4,9,0.18125))
pe

pe<-round(c(dbinom(0:3,r,p),(1-pbinom(3,r,p))),4)
pe

n<-sum(o)
n

e<-n*pe
e
table<-data.frame(x,o,pe,e,chi=(o-e)^2/e)
table
chisq.test(o,p=pe)
pruebachi<-chisq.test(o,p=pe)
chi<-pruebachi$statistic
gl<-pruebachi$parameter-1
pvalor<-1-pchisq(chi,gl)
pvalor


#1.2 Ejemplo de aplicaci贸n 3-a)
#Parte 1
x<-0:7
o<-c(15,30,25,20,5,4,1,0)
lambda<-sum(x*o)/sum(o)
prob<-round(c(dpois(0:6,lambda),1-ppois(6,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table

#Parte 2
x<-0:4
o<-c(15,30,25,20,10)
prob<-round(c(dpois(0:3,lambda),1-ppois(3,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table

chisq.test(o,p=prob)

pruebachi<-chisq.test(o,p=prob)
pvalor<-1-pchisq(pruebachi$statistic,pruebachi$parameter-1)
pvalor



#1.2 Ejemplo de aplicaci贸n 3-b)
#Parte 1
x<-0:7
o<-c(15,30,25,20,5,4,1,0)
lambda<-2
prob<-round(c(dpois(0:6,lambda),1-ppois(6,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table

#Parte 2
x<-0:5
o<-c(15,30,25,20,5,5)
prob<-round(c(dpois(0:4,lambda),1-ppois(4,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table

chisq.test(o,p=prob)

pruebachi<-chisq.test(o,p=prob)
pvalor<-1-pchisq(pruebachi$statistic,pruebachi$parameter-1)
pvalor


### Ejercicios propuestos ####
# Ejercicio propuesto 1
observados <-  c(60,120,94,23)
probabilidad_esperada <-  c(3/10,3/10,3/10,1/10)
chisq.test(observados, p=probabilidad_esperada)

# Ejercicio propuesto 2
x<-0:9
o<-c(6,24,42,59,62,44,41,14,6,2)
lambda<-sum(x*o)/sum(o)
prob<-round(c(dpois(0:8,lambda),1-ppois(8,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table
chisq.test(o,p=prob)
pruebachi<-chisq.test(o,p=prob)
pvalor<-1-pchisq(pruebachi$statistic,pruebachi$parameter-1)
pvalor

# Ejercicio propuesto 3
#Parte 1
x<-0:6
o<-c(448,133,43,22,4,3,2)
lambda<-sum(x*o)/sum(o)
prob<-round(c(dpois(0:5,lambda),1-ppois(5,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table

#Parte 2
x<-0:3
o<-c(448,133,43,31)
prob<-round(c(dpois(0:2,lambda),1-ppois(2,lambda)),7)
e=sum(o)*prob
table<-data.frame(x,o,prob,e,chi=(o-e)^2/e)
table
chisq.test(o,p=prob)
pruebachi<-chisq.test(o,p=prob)
pvalor<-1-pchisq(pruebachi$statistic,pruebachi$parameter-1)
pvalor

# Ejercicio propuesto 4
observados = c(14,27,32,15,29,15)
probabilidad_esperada = c(1/6,1/6,1/6,1/6,1/6,1/6)
chisq.test(observados, p=probabilidad_esperada)

# Ejercicio propuesto 5
#Parte 1
o<-c(9,14,39,53,29,11)
x<-c(0,1,2,3,4,5)
Ex<-sum(o*x)/sum(o)
Ex
r<-5
p<-Ex/r
p
pe<-round(dbinom(x,r,p),7)
pe
n<-sum(o)
n
e<-n*pe
e
table<-data.frame(x,o,pe,e,chi=(o-e)^2/e)
table
chisq.test(o,p=pe)
pruebachi<-chisq.test(o,p=pe)
chi<-pruebachi$statistic
gl<-pruebachi$parameter-1
pvalor<-1-pchisq(chi,gl)
pvalor

## 1.3 Ejemplos de aplicacion

# Ejemplo 1:

tabla<-matrix(c(23,28,9,60,79,49,29,60,63), ncol = 3) 
rownames(tabla)<-c("Alta", "Media", "Baja")
colnames(tabla)<-c("Lisa","Intermedia","Rugosa")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)
library(vcd)
assocstats(tabla)$contingency

# Ejemplo 2:

tabla<-matrix(c(32,14,6,12,22,9), ncol = 2) 
rownames(tabla)<-c("Baja", "Media", "Alta")
colnames(tabla)<-c("Si","No")
tabla
chisq.test(tabla)$expected
chisq.test(tabla)
library(vcd)
assocstats(tabla)$contingency


# Ejemplo 3:

tabla<-matrix(c(51,33,16,58,29,13,48,42,30,26,38,16), ncol = 4) 
rownames(tabla)<-c("Poca", "Media", "Alta")
colnames(tabla)<-c("Tipo I","Tipo II","Tipo III","Tipo IV")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)

# Ejemplo 4:

tabla<-matrix(c(32,12,22,14,22,19,6,9,8), ncol = 3) 
rownames(tabla)<-c("1", "2", "3")
colnames(tabla)<-c("Especie 1","Especie 2","Especie 3")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)

## 1.3.Codigos ejercicios propuestos

# Ejercicio propuesto 1

tabla<-matrix(c(7,8,11,11,5,7,40,75,20,9,13,95,230,101,42,
                6,23,113,82,17,5,7,18,16,39), ncol = 5) 
rownames(tabla)<-c("Negro", "Cafe", "Bayo","Castano","Gris")
colnames(tabla)<-c("Negro", "Cafe", "Bayo","Castano","Gris")
chisq.test(tabla,correct=TRUE)$observed

# Ejercicio propuesto 2
tabla<-matrix(c(11,32,7,13,28,9,9,27,14), ncol = 3) 
rownames(tabla)<-c("Bajo", "Medio", "Alto")
colnames(tabla)<-c("A", "B", "C")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)

# Ejercicio propuesto 3

tabla<-matrix(c(32,6,12,9), ncol = 2) 
rownames(tabla)<-c("Fumadora", "No fumadora")
colnames(tabla)<-c("Si", "No")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)

tabla<-matrix(c(127,71,53,47,50,43,57,42), ncol = 4) 
rownames(tabla)<-c("De 2 a 5 kg.", "Mas de 5 kg.")
colnames(tabla)<-c("A", "B", "C", "D")
chisq.test(tabla)$observed
chisq.test(tabla)$expected

# Ejercicio propuesto 5

tabla<-matrix(c(32,12,22,14,22,19,6,9,8), ncol = 3) 
rownames(tabla)<-c("1", "2" , "3")
colnames(tabla)<-c("Especie 1", "Especie 2", "Especie 3")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)

# Ejercicio propuesto 6

tabla<-matrix(c(17006,14464,788,126,37,48,38,5,1,1), ncol = 2) 
rownames(tabla)<-c("0", ",1" , "1-2", "3-5", ">=6")
colnames(tabla)<-c("Ausentismo", "Presencia")
chisq.test(tabla)$observed
chisq.test(tabla)$expected
chisq.test(tabla)

# Ejemplo 1:
Dat<-read.csv(file.choose(),sep=";")
library(reshape)
Dat=melt(Dat)
names(Dat)<-c("Tratamientos","Altura")
bartlett.test(Altura ~ Tratamientos, data = Dat)

# Ejemplo 2:
Dat<-read.csv(file.choose(),sep=";")
head(Dat,8)
bartlett.test(Concentraci髇 ~ Laboratorio, data = Dat)

# Ejercicio propuesto 1
ni<-c(78,133,18)
Sdi<-c(0.0870377,0.0926798,0.0836254)
Sp2<-sum((ni-1)*Sdi^2)/sum(ni-1)
Q1<-(sum(ni-1)*log(Sp2)-sum((ni-1)*log(Sdi^2)))
Q2<-1+(1/(3*(length(ni-1))))*(sum(1/(ni-1))-1/sum(ni-1))
Q<-Q1/Q2
Q
Pvalue<-dchisq(Q,3)
Pvalue

# Ejercicio propuesto 2
A<-c(1.195,1.144,1.167,1.249,1.177,1.217,1.187,NA)
B<-c(1.155,1.173,1.171,1.175,1.153,1.139,1.185,1.144)
C<-c(1.021,1.037,1.022,1.064,1.094,0.992,1.072,1.136)
D<-c(1.163,1.171,1.182,1.184,1.175,1.134,1.169,NA)
Dat<-data.frame(A,B,C,D)
library(reshape)
Dat=melt(Dat)
names(Dat)<-c("Laboratorios","Concentracion")
bartlett.test(Concentracion ~ Laboratorios, data = Dat)

# Ejercicio propuesto 3
A<-c(29,32,36,34,35)
B<-c(40,39,41,44,46)
C<-c(50,45,46,52,49)
D<-c(41,38,39,44,46)
Dat<-data.frame(A,B,C,D)
Dat=melt(Dat)
names(Dat)<-c("Dosis","Rendimiento")
bartlett.test(Rendimiento ~ Dosis, data = Dat)

# Ejercicio propuesto 4
Arequipa<-c(610,560,490,550,NA,NA,NA)
Iquitos<-c(710,730,660,610,460,NA,NA)
Piura<-c(560,610,470,510,580,620,650)
Trujillo<-c(500,400,500,500,500,400,NA)
Dat<-data.frame(A,B,C,D)
Dat=melt(Dat)
names(Dat)<-c("Ciudades","Ventas")
bartlett.test(Ventas ~ Ciudades, data = Dat)

