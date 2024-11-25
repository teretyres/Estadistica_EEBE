############# Ejercicio 3 ############
#apartado 1

sd <- 6.6
sd^2


#apartado 2
n<-4
n_b <- 4
n_b * sd^2

#apartado 3
mu <- 73.2
sd<-6.6
p<- 79
pnorm(p,mu,sd)

#apartado 4
mu <- 73.2
sd<-6.6
muestra <- 9
n <- 75
sigma_nueva <- sd / sqrt(muestra)
1 - pnorm(n,mu,sigma_nueva)

#apartado 5
sd<-6.6
muestra<-100

pchisq((muestra - 1) * 47 / sd^2,muestra - 1)

n_1 <- 100
chi2(n_e - 1) * 47 / sd^2
pchisq(chi2,n_1 - 1)



######### you chiat


# Datos generales
mu <- 73.2          # Media de la resistencia (kg)
sigma <- 6.6        # Desviación estándar de la resistencia (kg)

# (a) Valor esperado de la varianza de las resistencias de la muestra
expected_variance <- sigma^2
cat("(a) Valor esperado de la varianza: ", expected_variance, "\n")

# (b) Varianza de la suma de las resistencias de una muestra de 4 cables
n_b <- 4
variance_sum <- n_b * sigma^2
cat("(b) Varianza de la suma de las resistencias: ", variance_sum, "\n")

# (c) Probabilidad de que la resistencia de un cable no sea mayor que 79 kg
x_c <- 79
p_c <- pnorm(x_c, mean = mu, sd = sigma)
cat("(c) Probabilidad de que la resistencia no sea mayor que 79 kg: ", p_c, "\n")

# (d) Probabilidad de que la media de las resistencias de una muestra de 9 cables sea mayor que 75 kg
n_d <- 9
mu_sample <- 75
sigma_sample <- sigma / sqrt(n_d)
p_d <- 1 - pnorm(mu_sample, mean = mu, sd = sigma_sample)
cat("(d) Probabilidad de que la media sea mayor que 75 kg: ", p_d, "\n")

# (e) Probabilidad de que la varianza de las resistencias de una muestra de 100 cables sea menor que 47 kg
n_e <- 100
chi2_threshold <- (n_e - 1) * 47 / sigma^2
p_e <- pchisq(chi2_threshold, df = n_e - 1)
cat("(e) Probabilidad de que la varianza sea menor que 47 kg: ", p_e, "\n")





