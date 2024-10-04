media = 11
media.exponencial = 1/media
#Probabilidad que haya una distancia X entre imperfecciones
distancia = 0.097
probabilidad = dexp(distancia, rate = 1/media.exponencial)
probabilidad

#Probabilidad que haya una distancia maxima entre imperfeciones
distancia.2 = 0.134
probabilidad = pexp(distancia.2, rate = 1/media.exponencial)
probabilidad

#Media entre las distancia simulando 30000 experimentos
numero.experimentos = 300000
distancias = rexp(numero.experimentos, rate = 1/media.exponencial)
media.distancias = mean(distancias)
media.distancias

#Probabilidad que haya menos de X imperfecciones en 1 centimetro
numero.imperfecciones = media - 1
probabilidad.menos.imperfecciones = ppois(numero.imperfecciones, lambda = media)
probabilidad.menos.imperfecciones

#Probabilidad de que se encuentren X imperfecciones en 2 centimetros
centimetros = 2
imperfecciones = 26
probabilidad.imperfeciones = dpois(imperfecciones, lambda = media*centimetros)
probabilidad.imperfeciones

#Varianza del numero de imperfecciones haciendo experimentos
numero.experimentos = 500000
imperfecciones = rpois(numero.experimentos, lambda = media)
varianza.imperfecciones = var(imperfecciones)
varianza.imperfecciones