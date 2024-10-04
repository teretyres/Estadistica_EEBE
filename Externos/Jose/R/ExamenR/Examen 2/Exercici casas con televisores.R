# Parámetros de la distribución binomial
n <- 32  # tamaño de la muestra
p <- 0.58  # probabilidad de éxito (casa con al menos dos televisores)

# Cálculo de la probabilidad
prob <- dbinom(18, size = n, prob = p)

# Imprimir el resultado
print(prob)

# Parámetros de la distribución binomial
n <- 24  # tamaño de la muestra
p <- 0.58  # probabilidad de éxito (casa con al menos dos televisores)

# Cálculo de la probabilidad
prob <- 1 - pbinom(11, size = n, prob = p)

# Imprimir el resultado
print(prob)



# Parámetros de la distribución binomial
n <- 20  # tamaño de la muestra
p <- 0.42  # probabilidad de éxito (casa con máximo un televisor)

# Cálculo del valor esperado
valor_esperado <- n * p

# Cálculo de la varianza
varianza <- n * p * (1 - p)

# Cálculo del cuarto cuantil
cuarto_cuantil <- qbinom(0.75, size = n, prob = p)

# Imprimir los resultados
print(paste("Valor esperado: ", valor_esperado))
print(paste("Varianza: ", varianza))
print(paste("Cuarto cuantil: ", cuarto_cuantil))


# Parámetros de la distribución binomial
n <- 26  # tamaño de la muestra
p <- 0.58  # probabilidad de éxito (casa con al menos dos televisores)

# Número de simulaciones
num_simulaciones <- 100000

# Simulación de los experimentos
resultados <- rbinom(num_simulaciones, size = n, prob = p)

# Cálculo de la media
media <- mean(resultados)

# Imprimir el resultado
print(media)
