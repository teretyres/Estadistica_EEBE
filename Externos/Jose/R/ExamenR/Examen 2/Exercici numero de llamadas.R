promedio = 9
#Probabilidad que haya menos de 12 llamadas
numero.maximo = 12
probabilidad.menor = ppois(numero.maximo-1, promedio)
probabilidad.menor

#Probablidad que haya exactamente 18 llamadas en 2 horas
numero.llamadas = 18
numero.horas = 2
probabilidad.exacta = dpois(numero.llamadas, promedio*numero.horas)
probabilidad.exacta

#Varianza a partir de iteraciones
numero.experimentos = 100000
sumlacion = rpois(numero.experimentos, promedio)
varianza = var(sumlacion)
varianza