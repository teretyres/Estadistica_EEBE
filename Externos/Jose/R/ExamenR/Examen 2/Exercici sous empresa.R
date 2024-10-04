media = 807
media.estimada = 830
muestra = 64
probabilidad = 0.99 #Probabilidad de que la media del sueldo sea menor a media.estimada
varianza = sqrt(muestra)*(media.estimada-media)/(qnorm(probabilidad))
esperanza.varianza = varianza^2 #Calcular el valor esperado
esperanza.varianza

desviacion.estandar = varianza #Calcular la desviacion estandar = varianza
desviacion.estandar

suma.mayor = 51942
probabilidad.suma = pnorm(suma.mayor,mean=muestra*media,sd=sqrt(muestra)*desviacion.estandar)
probabilidad.suma

probabilidad.menos = 2
media.maxima = 789
tamaño.minimo = (qnorm(probabilidad.menos/100)*desviacion.estandar/(media-media.maxima))^2
tamaño.minimo

muestreo = 9
promedio = 807
probabilidad.menor = 5
cantidad.a.aumentar = (promedio-qnorm(probabilidad.menor/100)*desviacion.estandar/sqrt(muestreo))-media
cantidad.a.aumentar