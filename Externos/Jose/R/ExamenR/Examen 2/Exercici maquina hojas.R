# Parámetros de la distribución normal
media <- 498  # media de hojas por paquete

# Usamos la función qnorm para encontrar la desviación estándar de la distribución
# sabiendo que la probabilidad de empaquetar más de 489 hojas es de 0.9986501
desviacion_estandar <- (media - 489) / qnorm(0.9986501)

# Cálculo de la probabilidad de que un paquete sea inaceptable
prob_inaceptable <- pnorm(492, mean = media, sd = desviacion_estandar) +
  (1 - pnorm(501, mean = media, sd = desviacion_estandar))

# Imprimir el resultado
print(prob_inaceptable)


# Parámetros de la distribución normal
media <- 498  # media de hojas por paquete

# Usamos la función qnorm para encontrar la desviación estándar de la distribución
# sabiendo que la probabilidad de empaquetar más de 489 hojas es de 0.9986501
desviacion_estandar <- (media - 489) / qnorm(0.9986501)

# Cálculo de la probabilidad de que un paquete contenga un máximo de 501 hojas
prob_max_501 <- pnorm(501, mean = media, sd = desviacion_estandar)

# Imprimir el resultado
print(prob_max_501)


# Parámetros de la distribución normal
media <- 498  # media de hojas por paquete

# Usamos la función qnorm para encontrar la desviación estándar de la distribución
# sabiendo que la probabilidad de empaquetar más de 489 hojas es de 0.9986501
desviacion_estandar <- (media - 489) / qnorm(0.9986501)
desviacion_estandar
# Crear una secuencia de números para el eje x
x <- seq(media - 4*desviacion_estandar, media + 4*desviacion_estandar, length = 100)

# Crear el gráfico de la función de densidad
curve(dnorm(x, mean = media, sd = desviacion_estandar), 
      from = media - 4*desviacion_estandar, 
      to = media + 4*desviacion_estandar, 
      xlab = "Número de hojas por paquete", 
      ylab = "Densidad",
      main = "Función de densidad del número de hojas por paquete")
