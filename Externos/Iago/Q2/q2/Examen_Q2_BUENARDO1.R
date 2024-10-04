
#################### PREGUNTA 1 ###################

#1.¿Cuál es la probabilidad de que se prueben 95 camiones hasta que 45 sufran una explosión en los neumáticos?
X = 95 # num. total camions
r1 = 45 # num. encerts (95 camiones hasta que 45 sufran)
p = 0.48 # probabilidad 48% de los camiones no puede completar
dnbinom(X-r1,r1,p)

#2.¿Cuál es la probabilidad de que se pruebe entre 84 y 87 (ambos incluidos) camiones hasta encontrar 45 que sufran explosión en losneumáticos?
x1 = 87; x2 = 84; r2=45 # Entre 84 y 87
# CUIDAAAAAO, a la sol posa 83 enlloc de 84!!!
pnbinom(x1-r2,r2,p)-pnbinom((x2-1)-r2,r2,p)

#3. Si Z es la variable aleatoria que cuenta el número de camiones a probar hasta que se obtengan 49 que sufran explosión en losneumáticos, calcula:
# E(Z) = r/p
r = 49

# El valor esperado de Z:
r/p

# La varianza de Z:    V(Z) = n*(1-p)/p^2
n = 49
n*(1-p)/p^2

####################################################




#################### PREGUNTA 2 ####################

# El contenido de una probeta del laboratorio de química se distribuye normalmente con media 4. cl y desviación estándar de 0.97 cl:
mu = 4; sd = 0.97
# 1.¿Cuál es la gráfica correcta de la función de densidad delcontenido de una probeta?
curve(dnorm(x,mu,sd),xlim=c(mu-4*sd,mu+4*sd))

# 2. ¿Cuál es la probabilidad de que una probeta determinada tenga más de 4.1 cl?
x1 = 4.1
1-pnorm(x1,mu,sd) # 1-prob = "tenga más de x1"

# 3. ¿Cuál es la probabilidad de que una probeta determinada tenga entre 3.1 cl y 4. cl?
x2 = 3.1 ; x3 = 4 # x3 >= X >= x2
pnorm(x3,mu,sd)-pnorm(x2,mu,sd)

# 4. Calcule el percentil 0.86 de la función de densidad:
percentil = 0.86
qnorm(percentil,mu,sd)

# 5. En un conjunto de 7 probetas, ¿Cuál es la probabilidad de que el contenido líquido total sea inferior a 22.9 cl?
n = 7; x4 = 22.9
pnorm(x4,n*mu,sqrt(n)*sd)

###################################################




#################### PRGUNTA 3 ####################

# El número de llamadas telefónicas que llegan a una central telefónica a menudo se modela como una variable aleatoria de Poisson.
# Supongamos que en promedio hay 8 llamadas por hora.
lambda = 8

# 1. ¿Cuál es la probabilidad de que pase un tiempo de 0.101 horas entre dos llamadas?

# SOLUCIÓN: La probabilidad de que una variable continua tome un valor exacto es siempre 0!

# 2. ¿Cuál es la probabilidad de que pase un tiempo de MÁS de 0.167 horas entre dos llamadas?
t = 0.167
1-pexp(t,lambda) # també pexp(t,mu,lower.tail=F)

# 3. Un experimento se define como contabilizar el tiempo entre dos llamadas. Si se simulan 200000 experimentos, la media del tiempo
# entre dos llamadas es un valor cercano a: (Seleccione una de las siguientes opciones)
1/lambda
set.seed(23); mean(rexp(200000,8))

###################################################




#################### PRGUNTA 4 ####################

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





#################### PRGUNTA 5 ####################

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