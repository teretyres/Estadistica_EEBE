media = 87.2
desviacion = 5.7
#Valor esperado de la suma de una muestra
muestra = 4
valor.esperado = media*muestra
valor.esperado

#Varianza de la suma con una muestra distinta
muestra.2 = 16
varianza = desviacion^2*muestra.2
varianza

#Se selecciona un cable al azar, probabilidad que su resistencia sea mayor que 61
resistencia.minima = 95
probabilidad.resistencia = pnorm(c(resistencia.minima), mean=media, sd=desviacion, lower.tail=FALSE)
probabilidad.resistencia

#Probabilidad que la suma sea mayor a 966
muestra.3 = 16
valor.minimo = 966
media.suma = media*muestra.3
desviacion.suma = sqrt(muestra.3)*desviacion
probabilidad.suma = 1 - pnorm(valor.minimo, mean = media.suma, sd = desviacion.suma)
probabilidad.suma

#Probabilidad que la varianza sea menor que 33
muestra.4 = 64
valor.minimo = 32
grados.libertad = muestra.4-1

valor = grados.libertad*valor.minimo/(desviacion)^2
probabilidad.menor = pchisq(valor, df = grados.libertad)
probabilidad.menor
