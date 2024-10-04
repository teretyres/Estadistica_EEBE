#-----------------------------------------------------------
#EXERCICI 1 FIL COURE/HILO ALAMBRE
#-----------------------------------------------------------

#Una investigació prévia ha demostrat que el nombre d'imperfexions en un fil fi de coure té una mitjana de 11 imperfexions per centímetre de longitud.

#1.Quina és la probabilitat que hi hagi una distáncia de 0.065 centímetres entre dues imperfexions del fil?
  #La V.A.D qque representa el número d`imperfeccions en un centímetre segueix una distribució de Poisson.
  #Però la V.A.C que representa la distància entre dues imperfeccions segeuix una distribució exponencial amb
  #lamda = 11.
  #La probabilitat que una variable aleatòria contínua prengui un valor exacte és sempre 0.
# 0

#2.Quina és la probabilitat que hi hagí una distáncia de más de 0.108 centímetres entre dues imperfexions del fil?
lamda<-11
pexp(q = 0.108, lamda, lower.tail = FALSE)
# 0.3048303

#3.Un experiment es defineix com a prendre mesura de la distáncia entre dues imperfexions del fil. Si es simulen 100000 experiments, la mitjana de la distáncia entre dues imperfexions és un valor proper a: 
experiment_fil<-rexp(n=100000, lamda)
mean(experiment_fil)


#-----------------------------------------------------------
#EXERCICI 2 TELEVISORS
#-----------------------------------------------------------

#Un estudi ha mostrat que al barri de Sant Pere, el 48% dels llars tenen com a mínim dos televisors. Es vol saber:

#1.Si s'escull a l'atzar una mostra de 47 llars en el citat barri, quina és la probabilitat que 23 llars d'aquesta mostra tinguin com a mínim dos televisors?
dbinom(x = 18,size = 32,prob = 0.58)
# 0.1148408

#2.Si s'escull a l'atzar una mostra de 35 llars en el citat barri, quina és la probabilitat que más de 16 llars d'aquesta mostra tinguin com a mínim dos televisors?
pbinom(q=11, size = 24, prob = 0.58, lower.tail= FALSE)
# 0.5394266

#3.Si s'escull a l'atzar una mostra de 40 llars, X és la variable aleatória que representa el nombre de llars que tenen com a máxim un televisor, calcula:
#El valor esperat de X:
n<-20
prob_màx_1_televisor<-1-0.58
mean(n*prob_màx_1_televisor)
# 20.8

#La variáncia de X:n*p*(1-p)
var<-n*prob_màx_1_televisor*(1-prob_màx_1_televisor);
var
# 9.984

#El quantil quart de X:
#El quantil quart de X és 1/4, per tant, 0.25
qbinom(0.25, size = 20, prob = (1-0.58))
# 19

#4.Si un experiment consisteix en seleccionar aleatóriament 30 llars en el citat barri i comptar el nombre de llars que tenen com a mínim dos televisors, en simular 300000 experiments, la mitjana del nombre de llars que tenen com a mínim dos televisors és un valor proper a:
#(Seleccioneu una de les següents opcions):
experiment<- rbinom(n=100000, size=26, prob=0.58)
mean(experiment)
#14.40507


#-----------------------------------------------------------
#EXERCICI 3 CERT TIPUS CABLE
#-----------------------------------------------------------

#Un cert tipus de cable es fabrica amb una resisténcia mitjana a la tracció de 90.6 kg i una desviació típica de 7.8 kg. Respongueu les segúents preguntes:

#1.(a) Si es selecciona de manera aleatória una mostra de 25 cables \((X_1,X_2,...,X_{25})\), quin és el valor esperat de la suma de les resisténcies de la mostra?:
# Pregunta 1(a)
n_1 <- 25
mu <- 87.2
E<- n1 * mu1;
E

#2.(b) Si es selecciona de manera aleatória una mostra de 81 cables \((X_1,X_2,...,X_{81})\), quina és la varianza de la media de les resisténcies de la mostra?:
# Pregunta 2(b)
n2 <- 16
sigma <- 5.7
Var <- sigma^2 / n2;
sqrt(Var)

#3.(c) Si es selecciona de manera aleatória un cable, quina és la probabilitat que la resisténcia d'aquest cable no sea mayor sigui de 98 kg?:
# Pregunta 3(c)
resistència<- 95
pnorm(resistència, mean = mu, sd = sigma, lower.tail=FALSE)

x3<- 95

# Utilitzem una funció de densitat acumulativa (pnorm) per a calcular la probabilitat
prob_less_than_98 <- pnorm(x3, mean = mu, sd = sigma)
print(prob_less_than_98)

#4.(d) Si es selecciona de manera aleatória una mostra de 16 cables \((X_1,X_2,...,X_{16})\) quina és la probabiltat que la media de les resisténcies de la mostra sea mayor sigui de 92 kg?
n_4 <- 9
resistències <- 88
m<- 1 - pnorm(resistències, mean = 87.2, sd = 5.7, lower.tail = FALSE)
mean(m)

n4 <- 9
x4 <- 88

# Utilitzem una funció de densitat acumulativa (pnorm) per a calcular la probabilitat
prob_mean_greater_than_92 <- 1 - pnorm(x4, mean = 87.2, sd = 5.7/ sqrt(n4), lower.tail = FALSE)
print(prob_mean_greater_than_92)

#5.(e) Si es selecciona de manera aleatória una mostra de 100 cables \((X_1,X_2,...,X_{100})\),quina és la probabilitat que la media de les resisténcies de la mostra no sea menor sigui de 89 kg?:
# Pregunta 5(e)
n_5 <- 9
res <- 88
g<- 1 - pnorm(n_5, mean = mu, sd = sigma, lower.tail=FALSE)
mean(g)

n5 <- 9
x5 <- 88

# Utilitzem una funció de densitat acumulativa (pnorm) per a calcular la probabilitat
prob_mean_not_less_than_89 <- 1 - pnorm(x5, mean = mu, sd = sigma / sqrt(n5))
print(prob_mean_not_less_than_89)

#NOTA: Si creieu que alguna pregunta no es pot contestar, poseu NaN a la casella corresponent.


#-----------------------------------------------------------
#EXERCICI 4 PROBETA 
#-----------------------------------------------------------
#VAR(X) = sd^2 ---> sd = SQRT(VAR(X))

# El contenido de una probeta del laboratorio de química se distribuye normalmente con media 10. cl y desviación estándar de 2.44 cl:
# ¿Cuál es la gráfica correcta de la función de densidad del contenido de una probeta
datos_normales = rnorm(100000, mean = 4.8, sd = 1.09)
m1=4.8
s1=1.09
# Creo un histograma
hist(datos_normales, main = "Histograma de la Distribución Normal",
     xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black", freq=FALSE)
#3a grafica, fixa't q la freq. max és 0.16!

plot(0:22, dnorm(0:22,mean=m1, sd= s1))
lines(0:22, dnorm(0:22,mean=m1, sd= s1),type="l",col= "red")

# ¿Cuál es la probabilidad de que una probeta determinada tenga más de 14.1 cl?

# x->normal(x,media,sd)

1 - pnorm(6.6,m1,s1) # 0.04644677


# ¿Cuál es la probabilidad de que una probeta determinada tenga entre 8.1 cl y 11. cl? 

pnorm(5.8,m1,s1) - pnorm(3.6,m1,s1) # 0.4409553


# Calcule el percentil 0.65 de la función de densidad: Resposta
qnorm(0.27, m1, s1)  #10.94018

# En un conjunto de 5 probetas, ¿Cuál es la probabilidad de que el contenido líquido total sea inferior a 42.1 cl? 

sd = 1.09
media = 4.8
n = 13
media_suma = n * media
sd_suma <- sqrt(n) * sd

pnorm(57.5, mean = media_suma, sd = sd_suma) #0.07381612


#-----------------------------------------------------------
#EXERCICI 5 DEPART. MATES 
#-----------------------------------------------------------
# El Departamento de Matemática propone un examen de Estadística que consistente en 23 preguntas. 
# En cada pregunta hay 2 opciones, siendo correcta sólo una de ellas. Si un estudiante no conoce ninguna respuesta y decide probar suerte, queremos saber:
# Sigue una Binomial B(x,n,p)
n =23
p =0.5

# ¿Cuál es la probabilidad de que ese estudiante responda correctamente 12 preguntas?
dbinom(12,n,p) #0.1611803

# Si el examen se aprueba cuando se responden correctamente 14 preguntas o más, = MÁS DE 13, P(x>13) = 1 - P(x<=13) 
# ¿cuál es la probabilidad de que ese estudiante apruebe el examen?
# Binomial con n=23, p=0.5

1 - pbinom(13,n,p) #0.2024364

# Si el profesor quiere que la probabilidad de que ese estudiante apruebe sea menos de 0.1, 
# ¿en cuanto debería fijar el número mínimo de preguntas correctas para que se obtenga un aprobado en el examen?
qbinom(0.9,n,p) #15

# Si un experimento consiste en contestar de forma aleatoria el examen y contar el número de respuestas correctas, al simular 100000 experimentos, 
# el promedio del número de respuestas correctas es un valor cercano a:

mean(rbinom(100000,n,p)) #11.5    #Mirant l'enunciat sabria que E=n*p = 11.5


#-----------------------------------------------------------
#EXERCICI 6 CERT TIPUS CABLE 2.0 
#-----------------------------------------------------------
# Un cierto tipo de cable se fabrica con una resistencia media a la tracción de 89.4 kg 
# y una desviación típica de 7.8 kg. Teniendo en cuenta que dicha resistencia se distribuye de forma normal, conteste las siguientes preguntas: 
media= 89.4
sd = 7.8

#1. Si se selecciona de forma aleatoria una muestra de 4 cables (X1,X2,...,X4) , ¿cuál el valor esperado de la media de las resistencias de la muestra?:

#E(Xn) = E(X) = 89.4 


#2. Si se selecciona de forma aleatoria una muestra de 9 cables (X1,X2,...,X9) , ¿cuál es la desviación estándar de la suma de las resistencias de la muestra?:
#sd_Media = sd/sqrt(n) 
sd / sqrt(9)  #2.6

# Parámetros
tamano_muestra <- 9

# Desviación estándar de la suma de la muestra
desviacion_estandar_suma_muestra <- desviacion_tipica * sqrt(tamano_muestra)
desviacion_estandar_suma_muestra


#3. Si se selecciona de forma aleatoria un cable, ¿cuál es la probabilidad de que la resistencia de dicho cable sea mayor que 81 kg?:
1 - pnorm(81, media , sd) #0.8592427


#4. Si se selecciona de forma aleatoria una muestra de 4 cables (X1,X2,...,X4) , ¿cuál es la probabilidad de que la media de las resistencias de la muestra sea menor que 85 kg?:
varianza = 7.8^2
n = 4
pnorm(85, media, sqrt(varianza/n)) #0.1296166

# Parámetros
valor_corte_media_muestra <- 85

# Probabilidad
probabilidad_media_muestra_menor_que_85 <- pnorm(valor_corte_media_muestra, mean = media_poblacional, sd = desviacion_tipica/sqrt(tamano_muestra))
probabilidad_media_muestra_menor_que_85


#5. Si se selecciona de forma aleatoria una muestra de 64 cables (X1,X2,...,X64), ¿cuál es la probabilidad de que la suma de las resistencias de la muestra sea menor que 5747 kg?:
n = 64
varianza = 7.8^2
pnorm(5747, mean = media*n, sd=sqrt(n*varianza)) #0.6580148

# Parámetros
tamano_muestra <- 64
valor_corte_suma_muestra <- 5747

# Probabilidad
probabilidad_suma_muestra_menor_que_5747 <- pnorm(valor_corte_suma_muestra, mean = tamano_muestra * media_poblacional, sd = sqrt(tamano_muestra) * desviacion_tipica)
probabilidad_suma_muestra_menor_que_5747


#-----------------------------------------------------------
#EXERCICI 7 NEUMÁTICO
#-----------------------------------------------------------
#Al probar un cierto tipo de neumático de camión en un terreno accidentado, se encuentra que el 60% de los camiones no puede completar la prueba sin una explosión en los neumáticos. Se quiere saber:

#1. Cuál es la probabilidad de que se prueben 53 camiones hasta que 30 sufran una explosión en los neumáticos?

#Es binomial negativa
r<-30
p<-0.6
dnbinom(53-30, size = 30, prob = 0.6)

#2. Cuál es la probabilidad que se pruebe entre 40 y 43 (ambos incluidos) camiones hasta encontrar 25 que sufran explosión en los neumáticos?

#Es binomial negativa
r<-25
p<-0.6
pnbinom(43-25, size=25, prob=0.6)-pnbinom(39-25,size=25, prob=0.6)
pnbinom(43.5-25, size=25, prob=0.6)-pnbinom(39.5-25,size=25, prob=0.6)
pnbinom(43-25, size=25, prob=0.6)-pnbinom(39.5-25,size=25, prob=0.6)
pnbinom(43.5-25, size=25, prob=0.6)-pnbinom(39-25,size=25, prob=0.6)

#3. Si Z es la variable aleatoria que cuenta el nº de camiones a probar hasta que se obtengan 32 que sufran explosión en los neumáticos, calcula:

#Es binomial negativa
r<-32
p<-0.6
#Valor esperado de Z: ESPERANZA
#E(Z) = r/p
32/0.6

#Varianza de Z:
#V(Z)=n*(1-p)/p^2
(32*(1-0.6))/(0.6^2)


#-----------------------------------------------------------
#EXERCICI 8 PROCESOS ING: QUÍMICA (1990)
#-----------------------------------------------------------

#De acuerdo a la revista de Procesos de Ingeniería Química (Nov. 1990), aproximadamente el 48% de todos los fallos de las tuberías en
#las plantas químicas son causados por el error del operador. De los próximos 32 fallos de tuberías, queremos saber:

#1.¿Cuál es la probabilidad de que 16 fallos sean causados por el operador? 
dbinom(22,43,0.51)

#2.¿Cuál es la probabilidad de que al menos 21 fallos sean causados por el operador?
pbinom(16,43,0.51)
pbinom(20,32,0.48, lower.tail = FALSE)
1 - pbinom(21,32,0.48)
#3.Si un experimento consiste en contar el número de fallos causados por el operador en los siguientes 32 fallos, al simular
#300000 experimentos, el promedio del número de fallos causados por el operador es un valor cercano a: 
mean(rbinom(200000,43,0.51))

#4. ¿Cuál es la gráfica correcta de la función de probabilidad del número de fallos causados por el operador? (seleccione una de las siguientes opciones):
plot(0:43,dbinom(0:43,size=43,prob=0.51),type="h")                                                                                                            
points(0:43,dbinom(0:43,size=43,prob=0.51))

#-----------------------------------------------------------
#EXERCICI 9 TELEFÓNICAS
#-----------------------------------------------------------

#El número de llamadas telefónicas que llegan a una central telefónica a menudo se modela como una variable aleatoria de Poisson.
#Supongamos que en promedio hay 16 llamadas por hora.

#1. ¿Cuál es la probabilidad de que haya más de 14 llamadas en una hora?
1-ppois(14,16)
ppois(14,16,lower.tail = FALSE)

#2. ¿Cuál es la probabilidad de que haya exactamente 65 llamadas en 3 horas?
dpois(65,3*16)

#3. Un experimento se define como el número de llamadas en una hora determinada. Si simulamos 100000 experimentos, la varianza
#de las llamadas en un hora es un valor cercano a: (Seleccione una de las siguientes opciones)
var(rpois(100000,16))

#4. ¿Cuál es el gráfico correcto para la función de probabilidad de la variable que representa el número de llamadas en una hora?
#(Seleccione una de las siguientes opciones)
plot(3:39,dpois(3:39,lambda=16),type="h")
points(3:39,dpois(3:39,lambda=16))

#-----------------------------------------------------------
#EXERCICI 10 HILO ALAMBRE 2.0
#-----------------------------------------------------------

#Una investigación previa ha demostrado que el número de imperfecciones en un alambre fino de cobre tiene una media de 13 imperfecciones por centímetro de longitud.

#1. Cuál es la probabilidad de que se encuentren menos de 13 imperfecciones en 1 centímetro de alambre?
# Punto 1
# Probabilidad de encontrar menos de 13 imperfecciones en 1 centímetro de alambre
prob_menos_13 <- ppois(12, lambda = 13)
cat("1. Probabilidad de encontrar menos de 13 imperfecciones:", prob_menos_13, "\n")

#2.¿Cuál es la probabilidad de que se encuentren 61 imperfecciones en 4 centímetros del alambre?
# Punto 2
# Probabilidad de encontrar 61 imperfecciones en 4 centímetros de alambre
prob_61_en_4cm <- dpois(61, lambda = 4 * 13)
cat("2. Probabilidad de encontrar 61 imperfecciones en 4 centímetros:", prob_61_en_4cm, "\n")

#3. Un experimento se define como la inspección de 1 centímetro de alambre. Si se simulan 500000 experimentos, la varianza del número de imperfecciones encontradas en 1 centímetro de
#alambre es un valor cercano a:
# Punto 3
# Varianza del número de imperfecciones en 1 centímetro de alambre para 500000 experimentos
simulaciones <- rpois(500000, lambda = 13)
varianza_experimentos <- var(simulaciones)
cat("3. Varianza del número de imperfecciones en 1 centímetro de alambre:", varianza_experimentos, "\n")

#4.¿Cuál esla gráfica correcta de la función de probabilidad de la variable aleatoria que representa el número de imperfecciones encontradas en 1 centímetro de alambre?
# Punto 4
# Gráfica de la función de probabilidad de la variable aleatoria
library(ggplot2)

valores <- 0:30
probabilidades <- dpois(valores, lambda = 13)

data <- data.frame(valores, probabilidades)

ggplot(data, aes(x = valores, y = probabilidades)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Función de probabilidad de imperfecciones en 1 centímetro de alambre",
       x = "Número de imperfecciones",
       y = "Probabilidad") +
  theme_minimal()

#-----------------------------------------------------------
#EXERCICI 11 RESULTADOS EXAMEN
#-----------------------------------------------------------

#Se supone que los resultados de un examen siguen una distribución normal con media 70 y varianza 304.2:

#1. Cuál es la gráfica correcta de la función de densidad de los resultados del examen?
# Cargar la biblioteca necesaria para las funciones de distribución normal
library(ggplot2)

# Parámetros de la distribución normal
media <- 70
varianza <- 304.2
desviacion_estandar <- sqrt(varianza)

# Punto 1
# Gráfica de la función de densidad de los resultados del examen
valores <- seq(30, 110, by = 0.1)
densidad <- dnorm(valores, mean = media, sd = desviacion_estandar)

data <- data.frame(valores, densidad)

ggplot(data, aes(x = valores, y = densidad)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Función de densidad de los resultados del examen",
       x = "Puntuación",
       y = "Densidad") +
  theme_minimal()

#2. 2. ¿Cuál es la probabilidad de que una persona que se presenta al examen obtenga una calificación superior a 100.1? 
# Punto 2
# Probabilidad de obtener una calificación superior a 100.1
prob_superior_100.1 <- 1 - pnorm(100.1, mean = media, sd = desviacion_estandar)
cat("2. Probabilidad de obtener una calificación superior a 100.1:", prob_superior_100.1, "\n")

#3. ¿Cuál es la probabilidad de que una una persona que se presenta al examen obtenga una calificación entre 41.1 y 89?
# Punto 3
# Probabilidad de obtener una calificación entre 41.1 y 89
prob_entre_41.1_y_89 <- pnorm(89, mean = media, sd = desviacion_estandar) - pnorm(41.1, mean = media, sd = desviacion_estandar)
cat("3. Probabilidad de obtener una calificación entre 41.1 y 89:", prob_entre_41.1_y_89, "\n")

#4. Se declaran como NO-Aptos al 25% de los estudiantes con las notas más bajas. Calcule el valor de frontera entre Aptos y NO-Aptos:
# Punto 4
# Valor de frontera entre Aptos y NO-Aptos (percentil 25)
frontera_no_aptos <- qnorm(0.25, mean = media, sd = desviacion_estandar)
cat("4. Valor de frontera entre Aptos y NO-Aptos:", frontera_no_aptos, "\n")

#5. Calcular la proporción de estudiantes que tienen puntuaciones que exceden en 2 puntos de la puntuación que marca la frontera entre el Apto y el No-Apto 
# Punto 5
# Proporción de estudiantes con puntuaciones que exceden en 2 puntos la frontera
prop_exceden_2_puntos <- 1 - pnorm(frontera_no_aptos + 2, mean = media, sd = desviacion_estandar)
cat("5. Proporción de estudiantes con puntuaciones que exceden en 2 puntos la frontera:", prop_exceden_2_puntos, "\n")


#-----------------------------------------------------------
#EXERCICI 12 TIEMPO DURACIÓN COMPONENTE
#-----------------------------------------------------------

#El tiempo de duración de una componente sigue una distribución exponencial con media 10000 horas.
# Cargar la biblioteca necesaria para las funciones de distribución exponencial
library(ggplot2)

# Fijar la semilla para reproducibilidad
set.seed(12)

# Parámetro de la distribución exponencial
media_exponencial <- 10000

#1. ¿Cuál es la probabilidad de que una componente dure por lo menos 11000 horas? 
# Punto 1
# Probabilidad de que una componente dure por lo menos 11000 horas
prob_duracion_al_menos_11000 <- 1 - pexp(11000, rate = 1/media_exponencial)
cat("1. Probabilidad de que una componente dure por lo menos 11000 horas:", prob_duracion_al_menos_11000, "\n")

#2. ¿Cuál es la duración que superan las componentes con una probabilidad de 0.4? 
# Punto 2
# Duración que superan las componentes con una probabilidad de 0.4
duracion_prob_0.4 <- qexp(0.4, rate = 1/media_exponencial)
cat("2. Duración que superan las componentes con una probabilidad de 0.4:", duracion_prob_0.4, "\n")

#Realiza 100000 simulaciones de la variable X. (Utiliza como semilla el número 12).
# Punto 3
# Realizar 100000 simulaciones de la variable X
simulaciones <- rexp(100000, rate = 1/media_exponencial)

#1. ¿Cuál es el valor de la media de los resultados de las simulaciones?
# Punto 4
# Valor de la media de los resultados de las simulaciones
media_simulaciones <- mean(simulaciones)
cat("4. Valor de la media de los resultados de las simulaciones:", media_simulaciones, "\n")

#2. ¿Cuál es la mediana de los resultados de las simulaciones? 
# Punto 5
# Mediana de los resultados de las simulaciones
mediana_simulaciones <- median(simulaciones)
cat("5. Mediana de los resultados de las simulaciones:", mediana_simulaciones, "\n")

#3. ¿Cuál es la varianza de los resultados de las simulaciones? 
# Punto 6
# Varianza de los resultados de las simulaciones
varianza_simulaciones <- var(simulaciones)
cat("6. Varianza de los resultados de las simulaciones:", varianza_simulaciones, "\n")

# Histograma de las simulaciones
ggplot(data.frame(x = simulaciones), aes(x = x)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de las simulaciones",
       x = "Duración de la componente",
       y = "Frecuencia") +
  theme_minimal()





