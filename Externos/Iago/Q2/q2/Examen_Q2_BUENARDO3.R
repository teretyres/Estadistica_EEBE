#################### PRGUNTA 1 ####################

#Se supone que los resultados de un examen siguen una distribución normal con media 80 y varianza 396.4:

##### ninguna solución está verificada  ##########

mu = 80
sd = 396.4

# 1. ¿Cuál es la gráfica correcta de la función de densidad de los resultados del examen? (Seleccione una de las siguientes opciones)

curve(dnorm(x,mu,sd),xlim=c(mu-4*sd,mu+4*sd))

# 2. ¿Cuál es la probabilidad de que una persona que se presenta al examen obtenga una calificación superior a 90.2?

x = 90.2

dnorm(x, mu, sd)
 
# 3. ¿Cuál es la probabilidad de que una una persona que se presenta al examen obtenga una calificación entre 56.2 y 102.?

x1 = 102
x2 = 56.2
pnorm(x1,mu,sd)-pnorm(x2,mu,sd)

# 4. Se declaran como NO-Aptos al 22% de los estudiantes con las notas más bajas. Calcule el valor de frontera entre Aptos y NOAptos:

#### SIN SOLUCION ###


# 5. Calcular la proporción de estudiantes que tienen puntuaciones que exceden en 2 puntos de la puntuación que marca la frontera
# entre el Apto y el No-Apto

#### SIN SOLUCION ###


###################################################





#################### PRGUNTA 2 ####################

# Una investigación previa ha demostrado que el número de imperfecciones en un alambre fino de cobre tiene una media de
# 7 imperfecciones por centímetro de longitud.

lambda = 7

# 1. ¿Cuál es la probabilidad de que haya una distancia de 0.089 centímetros entre dos imperfecciones del alambre?

   #SOLUCION: La probabilidad de que una variable continua tome un valor exacto es siempre 0

# ¿Cuál es la probabilidad de que haya una distancia de más de 0.2 centímetros entre dos imperfecciones del alambre?

d = 0.2

1-pexp(d,lambda)     # TAMBIÉN pexp(d,lambda,lower.tail=F)

# 3. Un experimento se define como tomar medida de la distancia entre dos imperfecciones del alambre. Si se simulan 400000
# experimentos, la media de la distancia entre dos imperfecciones es un valor cercano a: (Seleccione una de las siguientes opciones)

N = 400000

set.seed(23); mean(rexp(N,lambda))

# TAMBÉ E(X) = 1/lambda


###################################################






#################### PRGUNTA 3 ####################

# Al probar un cierto tipo de neumático de camión en un terreno accidentado, se encuentra que el 60% de los camiones no puede
# completar la prueba sin una explosión en los neumáticos. Se quiere saber:


# 1. ¿Cuál es la probabilidad de que se prueben 53 camiones hasta que 30 sufran una explosión en los neumáticos?

# X es la variable aleatoria que cuenta el número de camiones a probar hasta que 30 sufran pinchazo. Entonces, sigue una
# distribución BINOMIAL NEGATIVA con parámetros r=30 y p =0.6. Por lo tanto, la probabilidad pedida es:
                                                                                                               
r1=30; p =0.6

dnbinom(53-30,r,p)


# 2. ¿Cuál es la probabilidad de que se pruebe entre 40 y 43 (ambos incluidos) camiones hasta encontrar 25 que sufran explosión en los neumáticos?

pnbinom(43-25,r,p)-pnbinom(39-25,r,p)

# 3. Si Z es la variable aleatoria que cuenta el número de camiones a probar hasta que se obtengan 32 que sufran explosión en los
# neumáticos, calcula:

# El valor esperado de Z:
r2= 32

r2/p

# La varianza de Z => V(Z)=n*(1-p)/p^2

r2*(1-p)/p^2


###################################################





#################### PRGUNTA 4 ####################

# De acuerdo a la revista de Procesos de Ingeniería Química (Nov. 1990), aproximadamente el 48% de todos los fallos de las tuberías en
# las plantas químicas son causados por el error del operador. De los próximos 32 fallos de tuberías, queremos saber:

p = 0.48
r = 32

# 1. ¿Cuál es la probabilidad de que 16 fallos sean causados por el operador?
x= 16
dbinom(x,r,p)

# 2. ¿Cuál es la probabilidad de que al menos 21 fallos sean causados por el operador?

n = 21

1-pbinom(n-1,r,p)

# 3. Si un experimento consiste en contar el número de fallos causados por el operador en los siguientes 32 fallos, al simular
# 300000 experimentos, el promedio del número de fallos causados por el operador es un valor cercano a: (seleccione una de las siguientes opciones)

ex = 300000
mean(rbinom(ex,r,p))

# ¿Cuál es la gráfica correcta de la función de probabilidad del número de fallos causados por el operador? (seleccione una de las
# siguientes opciones)



plot(0:32,dbinom(0:32,r,p),type="h"); points(0:32, dbinom(0:32,r,p))

###################################################





#################### PRGUNTA 5 ####################

# El número de llamadas telefónicas que llegan a una central telefónica a menudo se modela como una variable aleatoria de Poisson.
# Supongamos que en promedio hay 16 llamadas por hora.
lambda = 16

# 1. ¿Cuál es la probabilidad de que haya más de 14 llamadas en una hora?
n1 = 14

1-ppois(n1,lambda)  #también: ppois(n1,lambda,lower.tail=F)

# 2. ¿Cuál es la probabilidad de que haya exactamente 65 llamadas en 3 horas?
n2 = 65
t2 = 3

dpois(n2,t2*lambda)

# 3. Un experimento se define como contabilizar el tiempo entre dos llamadas. Si se simulan 100000 experimentos, la media del tiempo
# entre dos llamadas es un valor cercano a: (Seleccione una de las siguientes opciones)
1/lambda
set.seed(23); var(rpois(100000,lambda))

# 4. ¿Cuál es el gráfico correcto para la función de probabilidad de la variable que representa el número de llamadas en una hora?
# (Seleccione una de las siguientes opciones)

plot(3:39, dpois(3:39,lambda),type="h");
points(3:39, dpois(3:39,lambda))


###################################################