
#################### PREGUNTA 1 ####################

# El contenido de una probeta del laboratorio de química se distribuye normalmente con media 4. cl y desviación estándar de 0.97 cl:
mu = 3.3; sd = 0.535
# 1.¿Cuál es la gráfica correcta de la función de densidad delcontenido de una probeta?
curve(dnorm(x,mu,sd),xlim=c(mu-4*sd,mu+4*sd))

# 2. ¿Cuál es la probabilidad de que una probeta determinada tenga más de 4.1 cl?
x1 = 2.2
1-pnorm(x1,mu,sd) # 1-prob = "tenga más de x1"

# 3. ¿Cuál es la probabilidad de que una probeta determinada tenga entre 3.1 cl y 4. cl?
x2 = 2.2 ; x3 = 4.3 # x3 >= X >= x2
pnorm(x3,mu,sd)-pnorm(x2,mu,sd)

# 4. Calcule el percentil 0.86 de la función de densidad:
percentil = 0.34
qnorm(percentil,mu,sd)

# 5. En un conjunto de 7 probetas, ¿Cuál es la probabilidad de que el contenido líquido total sea inferior a 22.9 cl?
n = 9; x4 = 26.5
pnorm(x4,n*mu,sqrt(n)*sd)

# Matlab: daxContModels.p; 3; mu=mu*n, sd=sd*sqrt(n); 1; 26.5(mostra)

###################################################




#################### PREGUNTA 2 ###################

# Al probar un cierto tipo de neumático de camión en un terreno accidentado, se encuentra que el 52% de los camiones no puede completar la
# prueba sin una explosión en los neumáticos. Se quiere saber:

#1.¿Cuál es la probabilidad de que se prueben 94 camiones hasta que 50 sufran una explosión en los neumáticos?
X = 94 # num. total camions
r1 = 50 # num. encerts (94 camiones hasta que 50 sufran)
p = 0.52 # probabilidad 48% de los camiones no puede completar
dnbinom(X-r1,r1,p)

#2.¿Cuál es la probabilidad de que se pruebe entre 48 y 51 (ambos incluidos) camiones hasta encontrar 25 que sufran explosión en losneumáticos?
x1 = 51; x2 = 48; r2 = 25
pnbinom(x1-r2,r2,p)-pnbinom((x2-1)-r2,r2,p)

#3. Si Z es la variable aleatoria que cuenta el número de camiones a probar hasta que se obtengan 33 que sufran explosión en losneumáticos, calcula:
# E(Z) = r/p
r3 = 33

# El valor esperado de Z:
r3/p

# La varianza de Z:    V(Z) = n*(1-p)/p^2
n = 33
n*(1-p)/p^2

####################################################




#################### PRGUNTA 3 ####################

# El número de llamadas telefónicas que llegan a una central telefónica a menudo se modela como una variable aleatoria de Poisson.
# Supongamos que en promedio hay 22 llamadas por hora.
lambda = 22

# 1. ¿Cuál es la probabilidad de que pase un tiempo de 0.03 horas entre dos llamadas?

# SOLUCIÓN: La probabilidad de que una variable continua tome un valor exacto es siempre 0!

# 2. ¿Cuál es la probabilidad de que pase un tiempo MÁXIMO de 0.054 horas entre dos llamadas?
t = 0.054
pexp(t,lambda) # també pexp(t,mu,lower.tail=F)

# 3. Un experimento se define como contabilizar el tiempo entre dos llamadas. Si se simulan 200000 experimentos, la media del tiempo
# entre dos llamadas es un valor cercano a: (Seleccione una de las siguientes opciones)
1/lambda
set.seed(23); mean(rexp(200000,22))

###################################################




#################### PRGUNTA 4 ####################

#Un estudio examinó las actitudes nacionales sobre los antidepresivos. El estudio reveló que el 47% de la gente cree que "los antidepresivos
#realmente no curan nada, solo encubren el problema real". De acuerdo con dicho estudio:
p= 0.47
#1. Calcula la probabilidad de que exactamente 14 de las siguientes 26 personas seleccionadas al azar sean de esta opinión?
X1= 26
x1 = 14

dbinom(x1,X1,p)

# 2. ¿Cuál es la probabilidad de que como máximo 19 de las siguientes 30 personas seleccionadas al azar sean de esta opinión?
X2= 30
x2 = 19
pbinom(x2,X2,p)

# 3. Si X es la variable aleatoria que cuenta el número de personas (de 34 seleccionadas al azar) que no están de acuerdo con dicha opinión, calcula:
n = 34

# El valor esperado de X:
n*(1-p)

# La varianza de X:     V(X)=n*p*(1-p) = 34*0.53*(1-0.53)
n*(1-p)*(1-(1-p))

# El primer cuartil de :
q1 = 0.25
qbinom(q1,n,(1-p))


#4. Un experimento se define como la selección aleatoria de 28 personas y determinar el número de personas con esta opinión. Si se hace
#la simulación de 500000 experimentos, el promedio del número de personas con esta opinión es un valor cercano a (seleccione una de
#las siguientes opciones):

n2 = 28
n2*p

exp = 500000
mean(rbinom(exp,n2,p))

###################################################




#################### PRGUNTA 5 ####################

# El número de llamadas telefónicas que llegan a una central telefónica a menudo se modela como una variable aleatoria de Poisson.
# Supongamos que en promedio hay 22 llamadas por hora.
lambda = 22

# 1. ¿Cuál es la probabilidad de que haya más de 15 llamadas en una hora?
n3= 15
1-ppois(n3,lambda)

# 2. ¿Cuál es la probabilidad de que haya exactamente 64 llamadas en 4 horas?
L=64
t = 4
dpois(L,t*lambda)

# 3. Un experimento se define como el número de llamadas en una hora determinada. Si simulamos 500000 experimentos, la varianza de
# las llamadas en un hora es un valor cercano a: (Seleccione una de las siguientes opciones)

set.seed(23); var(rpois(500000,lambda))

# 4. ¿Cuál es el gráfico correcto para la función de probabilidad de la variable que representa el número de llamadas en una hora?
# (Seleccione una de las siguientes opciones)

plot(7:51, dpois(7:51,lambda),type="h"); points(7:51,dpois(7:51,lambda))

###################################################





#################### PRGUNTA BUENARDO 1.4 ####################

# Una investigación previa ha demostrado que el número de imperfecciones en un alambre fino de cobre tiene una media de 10 imperfecciones por centímetro de longitud.
mu = 10

# 1. ¿Cuál es la probabilidad de que se encuentren más de 11 imperfecciones en un 1 centímetro del alambre?
x5 = 11
1-ppois(x5,mu)

# 2. ¿Cuál es la probabilidad de que se encuentren 41 imperfecciones en 4 centímetros del alambre?
x6 = 41
l1 = 4
dpois(x6,l1*mu)

# 3. Un experimento se define como la inspección de 1 centímetro de alambre. Si se simulan 400000 experimentos, la varianza del númerode imperfecciones encontradas en 1 centímetro de alambre es un valor cercano a: (Seleccione una de las siguientes opciones)
s = 400000

set.seed(23); var(rpois(s,mu))

# 4.¿Cuál es la gráfica correcta de la función de probabilidad de la variable aleatoria que representa el número de imperfeccionesencontradas en 1 centímetro de alambre? (Seleccione una de las siguientes opciones)

plot(0:27,dpois(0:27,lambda=10),type="h"); points(0:27, dpois(0:27,lambda=10))


###################################################




#################### PRGUNTA BUENARDO 1.5 ####################

# Al probar un cierto tipo de neumático de camión en un terreno accidentado, se encuentra que el 42% de los camiones no puede completar laprueba sin una explosión. De los siguientes 33 camions probados, se quiere saber:
p=0.42
x7=33
# 1. ¿Cuál es la probabilidad de que 11 camiones tengan una explosión?
cam=11
dbinom(cam,x7,p)

# 2. ¿Cuál es la probabilidad de que más de 19 camiones tengan una explosión?
x8=19
1-pbinom(x8,x7,p)

# 3. Un experimento se define como la selección aleatoria de 33 camiones y determinar el número de camiones con explosión. Si sesimulan 100000 experimentos, el promedio del número de camiones con explosión es un valor cercano a (Escoja una de las siguientesopciones):
s = 100000
mean(rbinom(s,x7,p))

# 4. ¿Cuál es la gráfica correcta de la función de distribución para el número de camiones con explosión en sus neumáticos (Escoja una delas siguientes opciones):
plot(0:33,dbinom(0:33,size=33,prob=p),type="h"); points(0:33, dbinom(0:33,size=33,prob=p))

###################################################
