############# Ejercicio 2 ############

#apartado 1
mu <- 498
sd <- abs((489 - 498)) / qnorm(0.99865)  #si se utilizan los datos como la fórmula dice, el resultado es NaN porque el numerador sale negativo y una desviación nunca puede ser negativa, la formula que se ha usado es la siguiente (489-498)/z = sd. Para poner un valor se ha añadido un abs() al numerado 
aceptable<- pnorm2_Zk(492,501,mu,sd,"< <")
1 - aceptable

#apartado 2

dnorm(502,498,abs((489 - 498)) / qnorm(0.99865))

#apartado 3

mu_nueva <- 499.5
sigma_nuevo <- (495 - mu_nueva) / qnorm(0.025)
sigma_nuevo

#apartado 4

set.seed(321) 
mu_nueva2 <- 502
sigma_nuevo2 <- 2
n<- 10000
paquetes <- rnorm(n, mu_nueva2,sigma_nuevo2)
mean(paquetes >= 497 & paquetes <= 507)

#apartado 5


n5<- 500
k5<- 10
p5<- 0.02
dbinom(k5,n5,p5)










######### you chiat

##### apartado 1

# Datos
mu_1 <- 498
p_489 <- 0.99865
z_489 <- qnorm(p_489) # Valor z correspondiente a P(X > 489)
sigma_1 <- (489 - mu_1) / z_489 # Calculamos sigma

# Probabilidad de que un paquete sea aceptable
p_aceptable <- pnorm(501, mean = mu_1, sd = sigma_1) - pnorm(492, mean = mu_1, sd = sigma_1)
p_no_aceptable <- 1 - p_aceptable
cat("Answer 1: Probabilidad de que el paquete NO sea aceptable:", p_no_aceptable, "\n")

##### apartado 2

# Probabilidad de que el paquete tenga exactamente 502 hojas
prob_502 <- dnorm(502, mean = mu_1, sd = sigma_1)
cat("Answer 2: Probabilidad de exactamente 502 hojas:", prob_502, "\n")

#apartado 3
# Datos
mu_3 <- 499.5
p_495 <- 0.025
z_495 <- qnorm(p_495)

# Desviación estándar
sigma_3 <- (495 - mu_3) / z_495
cat("Answer 3: Desviación estándar del proceso:", sigma_3, "\n")

#apartado 4

# Datos
set.seed(321) # Semilla para reproducibilidad
mu_4 <- 502
sigma_4 <- 2
n_sim <- 10000

# Simulación
paquetes <- rnorm(n_sim, mean = mu_4, sd = sigma_4)
proporcion <- mean(paquetes >= 497 & paquetes <= 507)
cat("Answer 4: Proporción de paquetes entre 497 y 507 hojas:", proporcion, "\n")




#apartado 5

# Datos
n_5 <- 500
k_5 <- 10
p_5 <- 0.02

# Probabilidad
prob_10 <- dbinom(k_5, size = n_5, prob = p_5)
cat("Answer 5: Probabilidad de exactamente 10 hojas añadidas o retiradas:", prob_10, "\n")



