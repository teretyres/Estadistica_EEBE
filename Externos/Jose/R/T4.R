#------------------------------------------------------------------------
#  T4 - Variables aleatorias discretas y distribuciones de probabilidad
#------------------------------------------------------------------------

#--- Función de densidad (o probabilidad) ---
#Tirar dado normal y trucado
x1 = 1:6
f1 = rep(1/6,6)
plot(x1,f1,type='h',col='red',lwd=3,main='f1(x) ejemplo 1',
     xlab='X1',ylab='f1(x)',xlim=c(0.5,6.5),ylim=c(0,0.2))
points(x1,f1,col='red',lwd=10); gra.fx.ej1 = recordPlot()

x2 = 1:6
f2 = x2/21
plot(x2,f2,type='h',col='red',lwd=3,main='f1(x) ejemplo 2',
     xlab='X2',ylab='f2(x)',xlim=c(0.5,6.5),ylim=c(0,0.35))
points(x2,f2,col='red',lwd=10); gra.fx.ej2 = recordPlot()

lines(4,f2[4],type='h',col='blue',lwd=3)
points(4,f2[4],col='blue',lwd=10)

#--- Función de distribución (o probabilidad) ---
#Dado ok
F1 = cumsum(f1)
plot(c(0,x1,7),c(0,F1,1),type='s',col='red',lwd=3)
points(x1,F1,col='red',lwd=8)

#Dado trucado
F2 = cumsum(f2)
plot(c(0,x2,7),c(0,F2,1),type='s',col='red',lwd=3)
points(x2,F2,col='red',lwd=8)
lines(c(4,5), rep(F2[4],2), type="s", col="blue", lwd=3)
points(4, F2[4], col="blue", lwd=8)

#--- Valor esperado y varianza ---

E=sum(x1*f1) #μ=1*(1/6)+2*(1/6)+3*(1/6)+4*(1/6)+5*(1/6)+6*(1/6)=3.5
V=sum((x1-E)^2*f1) #σ^2=(1-3.5)^2*1/6+...=2.916...

#--- Generación de simulaciones aleatorias ---
sample(x1)
sample(x1,size=1,prob=)
sampleo1 = sample(x1,100,replace=T,prob=f1);
sampleo2 = sample(x2,100,replace=T,prob=f2)
h1 = hist(sampleo1,breaks=seq(0.5,6.5,1),freq=FALSE)
h2 = hist(sampleo2,breaks=seq(0.5,6.5,1),freq=FALSE)

#--- Comparación de experimentos y funciones teóricas ---

gra.fx.ej1
hist(sampleo1,breaks=seq(0.5,6.5,by=1), freq=FALSE, add=T)
lines(x1,f1,type="h", col='red',lwd=3)
points(x1,f1,col='red',lwd=10)

gra.fx.ej2
hist(sampleo2,breaks=seq(0.5,6.5,by=1), freq=FALSE, add=T)

#----------------------------
# Distribuciones más comunes
#----------------------------

#Probabilidades elementales
dbinom(x,size=n,prob=p) #P(X=x)=f(x)
dbinom(4,size=10,prob=1/3)

#Probabilidades acumuladas

pbinom(c(4),size=10,prob=1/3,lower.tail=TRUE) #lower.tail=TRUE para P(X<=x), si lower.tail=FALSE P(X>x)

#Gráfica de una distribución

    #Función de probabilidad
x = 0:8 #Posibles resultados
fd = dbinom(x,size=10,prob=1/3)
plot(x,fd,type="h", col='red', lwd=3, main="B(10,0.33)", xlab="X", 
     ylab="f(x)", xlim=c(-0.8,8.2), ylim=c(0,0.30))
points(x, fd, col='red', lwd=10)

x4 = 4; f_4 = dbinom(4, size=10, prob=1/3) #Cálculo de P(X=4)=f(4)
lines(x4, f_4, type="h", col="blue", lwd=3) #Agrega la línea en X=4
points(x4, f_4, col="blue", lwd=10) #Agrega el punto en (4,f(4))
text(x4, f_4,expression(P(X==4)), pos=3, col="blue")

    #Función de distribución
x = 0:8 #Posibles resultados
fp = pbinom(x,size=10,prob=1/3)
plot(x,fp, type="s", col='red', lwd=3, main="B(10,0.33)", xlab="X", 
     ylab="f(x)", xlim=c(-0.8,8.2), ylim=c(0,1))
points(x, fp, col='red', lwd=10)

x = 4; f_4 = pbinom(4, size=10, prob=1/3)
abline(v=x, col="blue"); abline(h=f_4, col="blue")
text(x, f_4,expression(P(X<=4)== P(X<5)), pos=2, col="blue")

#Cuantil p-ésimo 

#P(X<=x)>=p con lower.tail=TRUE
#P(X>x)>=P con lower.tail=FALSE

qbinom(c(0.15), size=10, prob=1/3, lower.tail=a)

#Muestreo

rbinom(4,size=10,prob=1/3)

#------- Cheat Sheet -------
curve(dnorm()) para graficar distribución
d #Función de distribución
p #Función de probabilidad
q #Quantil
r #Muestra aleatoria

binom #Distribución binomial
geom #Distribución geométrica
nbinom #Distribución binomial negativa
hyper #Distribución hipergeométrica
pois #Distribución de Poisson
unif #Distribución Uniforme
