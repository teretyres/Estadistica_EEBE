# Parámetros de la distribución normal
media <- 807  # media de sueldos
n <- 64  # tamaño de la muestra
media_muestra <- 830  # media de la muestra

# Encontrar el valor z correspondiente a la probabilidad 0.99
z1 <- qnorm(0.99)

# Calcular la desviación estándar de la población
desviacion_estandar <- (media_muestra - media) / (z / sqrt(n))

# Calcular el valor esperado de la suma de la muestra
valor_esperado_suma <- n * media

# Imprimir el resultado
print(valor_esperado_suma)

# Calcular la desviación estándar de la suma de la muestra
desviacion_estandar_suma <- sqrt(n) * desviacion_estandar

# Imprimir el resultado
print(desviacion_estandar_suma)


# Calcular la probabilidad de que la suma de los sueldos no sea menor a 51942 euros
prob_no_menor <- 1 - pnorm(51942, mean = valor_esperado_suma, sd = desviacion_estandar_suma)

# Imprimir el resultado
print(prob_no_menor)

# Calcular la probabilidad de que la suma de los sueldos sea mayor a 51942 euros
prob_mayor <- 1 - pnorm(51942, mean = valor_esperado_suma, sd = desviacion_estandar_suma)

# Imprimir el resultado
print(prob_mayor)


# Parámetros de la distribución normal
media <- 807  # media de sueldos
media_muestra <- 789  # media de la muestra

# Encontrar el valor z correspondiente a la probabilidad 0.02
z <- qnorm(0.02)

# Calcular la desviación estándar de la población
desviacion_estandar <- (media - media_muestra) / z1

# Calcular el tamaño mínimo de la muestra
n_minimo <- (z1 * desviacion_estandar / (media - media_muestra))^2

# Redondear al número entero más grande porque el tamaño de la muestra debe ser un número entero
n_minimo 


# Encontrar el valor z correspondiente a la probabilidad 0.55 en una distribución normal estándar
z <- qnorm(0.55)

# Calcular el aumento necesario en los sueldos
aumento_sueldos <- z * desviacion_estandar / sqrt(n_minimo)

# Imprimir el resultado
print(aumento_sueldos)

