#------------------------------------------------------------------------
#  T5 - Variables aleatorias contínuas y distribuciones de probabilidad
#------------------------------------------------------------------------

#----
#---- Función de densidad ---
#----

x = seq(0,2,0.0001) #Posibles resultados
f = 1/8 + 3*x/8
plot(x,f, type="l", col='red', lwd=3)

# Ahora con algunos valores de X en que la probabilidad es 0

LowLim = 0 #Mínimo valor que toma X
UppLim = 2 #Máximo valor que toma X
delta = 0.001 #Resolución, diferencia entre valores de X
#Valores parciales de X 
x.less0 = seq(LowLim-1, LowLim, delta) #-1 <X <0
x.less2 = seq(LowLim, UppLim, delta) #0 <X <2
x.greater2 = seq(UppLim, UppLim+1, delta) #2 <X <3
#Valores parciales de la función de densidad
fx.less0 = rep(0, length(x.less0)) #f(x) para -1 <X <0
fx.less2 = 1/8+3/8*x.less2 #f(x) para 0 <X <2
fx.greater2 = rep(0, length(x.greater2)) #f(x) para 2 <X <3
#Vectores finales
x = c(x.less0, x.less2, x.greater2) #-1 <X <3
f = c(fx.less0, fx.less2, fx.greater2) #f(x) para -1 <X <3
#Representación gráfica
plot(x, f, type="l", col='red', lwd=3)
gra.fx = recordPlot()

# Para resaltar una probabilidad específica

#P(1<X<1.5)=(P(1<=X<=1.5))
x.i = 1 #Límite inferior
x.f = 1.5 #Límite superior
x.x = seq(x.i, x.f, delta) #Valores de X dentro de los límites
coord.x = c(x.i, x.x, x.f) #Vector de vértices en la coordenada x
coord.y = c(0, 1/8+3/8*x.x, 0) #Vector de vértices en la coordenada y
polygon(coord.x, coord.y, col="skyblue") #Área bajo la curva
text(x.i, 0.7, "P(1 <X <1.5)", pos=3, col="blue")

#----
#---- Función de distribución ---
#----

LowLim = 0 #Mínimo valor que toma X
UppLim = 2 #Máximo valor que toma X
delta = 0.001 #Resolución, diferencia entre valores de X
#Valores parciales de X
x.less0 = seq(LowLim-1, LowLim, delta) #-1 <X <0
x.less2 = seq(LowLim, UppLim, delta) #0 <X <2
x.greater2 = seq(UppLim, UppLim+1, delta) #2 <X <3
#Valores parciales de la función de distribución
Fx.less0 = rep(0, length(x.less0)) #F(x) para -1 <X <0
Fx.less2 = 1/8*x.less2+3/16*x.less2^2 #F(x) para 0 <X <2
Fx.greater2 = rep(1, length(x.greater2)) #F(x) para 2 <X <3
#Vectores finales
x = c(x.less0, x.less2, x.greater2) #-1 <X <3
F = c(Fx.less0, Fx.less2, Fx.greater2) #F(x) para -1 <X <3
#Representación gráfica
plot(x, F, type="l", col='red', lwd=3)

# Para resaltar una probabilidad específica

#P(1<X<1.5)=(P(1<=X<=1.5))
a = 1; b = 1.5; Fa = 1/8*a+3/16*a^2; Fb = 1/8*a +3/16*b^2
abline(v=b,col="blue")
abline(h=Fb,col="blue")
text(0,Fb, expression(P(X <= 1.5) == F(1.5)), pos=3, col="blue")
abline(v=a,col="blue"); abline(h=Fa,col="blue")
text(0,Fa, expression(P(X <= 1) == F(1)), pos=1, col="blue")

#----
#---- Medidas características de las VAC ---
#----

# Valor esperado

#Calcula f(0) y f(2)
f1 <- function(x) 1/8 + 3/8*(x)
y0 = f1(0); y0
yf = f1(2); yf
A = y0*2+(yf-y0)*2/2; A

# Generar simus
x = seq(0,2,0.0001); f = 1/8+3/8*x; sample(x, size=1, prob=f)
sample(x,size=100,replace=T,prob=f)
simdata = sample(x, size=10000, replace=T, prob=f)
hist(simdata,freq=FALSE)
gra.fx; hist(simdata,freq=FALSE, add=T)

mean(simdata) #Media
var(simdata) #Varianza corregida
sd(simdata) #Desviación estándar corregida
N = length(simdata)
((N-1)/N)*var(simdata) #Varianza NO corregida
sqrt((N-1)/N)*sd(simdata) #Desviación estándar NO corregida

#----
#---- Distribuciones de probabilidad continuas más comunes ---
#----

# Probabilidades
Ej
mu = 1.25
sigma = 0.46
P(X<=1.75)
x -> N(1.25,0.46)
#lower.tail=TRUE, probabilidad parte izquierda. lower.tail=FALSE, probabilidad parte derecha.
#pnorm(x5,mean,sd)
pnorm(1.75,mean=1.25,sd=0.46)

# Gráfica de una distribución
    # Función de densidad
x = seq(mu-4*sigma, mu+4*sigma, length=1000) #Posibles resultados
f = dnorm(x,mean=mu, sd=sigma) #Función densidad
plot(x, f, type="l", col='red', lwd=3, main="N(1.25,0.46^2)",xlab="X", ylab="f(x)")
curve(dnorm(x,mean=mu,sd=sigma),xlim=c(mu-4*sigma,mu+4*sigma),col='red', 
      lwd=3, main="N(1.25,0.46^2)", xlab="X", ylab="f(x)")

#Resaltar partes de la gráfica
cord.x=c(-0.264, seq(-0.264,1.75,0.01),1.75)
cord.y=c(0, dnorm(seq(-0.264,1.75,0.01),1.25,0.46),0)
polygon(cord.x, cord.y, col='skyblue')

    # Función de distribución
curve(pnorm(x,mean=mu,sd=sigma),xlim = c(mu-4*sigma, mu+4*sigma), col='red', lwd=3,
main="N(1.25,0.46^2)", xlab="X", ylab="F(x)")

x = 1.75 
F_x = pnorm(c(1.75), mean=1.25, sd=0.46, lower.tail=TRUE)
abline(v=x, col="blue");
abline(h=F_x, col="blue")
text(x, F_x,expression(P(X<=1.75)), pos=2, col="blue")

    # Cuantiles

#P(X<=x)>=p con lower.tail=TRUE
#P(X>x)>=P con lower.tail=FALSE
#El 0.1 y 0.5 es el % del cuantil
qnorm(c(0.1,0.5), mean=1.25, sd=0.46, lower.tail=TRUE)

  # Muestreo

NormalSamples <- as.data.frame(matrix(rnorm(100*1, mean=1.25, sd=0.46), ncol=1))

#------- Cheat Sheet -------

d #Función de distribución
p #Función de probabilidad
q #Quantil
r #Muestra aleatoria

norm #Distribución normal
exp #Distribución exponencial
t #Distribución t-Student
unif #Distribución Uniforme

expr = pnorm...pexp...
curve(expr,from=a,to=b)

