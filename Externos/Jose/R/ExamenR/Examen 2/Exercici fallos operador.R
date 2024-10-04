probabilidad.operador = 0.58
proximos.fallos = 24
fallos.operador = 19

#Probabilidad que los siguientes proximos.fallos, haya fallos.operador causados por un operador
probabilidad = dbinom(fallos.operador,proximos.fallos,probabilidad.operador) #dbinom(lo que espero que pase, cantidad de la muestra, probabilidad que pase cada vez)
probabilidad

#Probabilidad que menos de fallos operador sean causados por un operador
fallos.operador = 11
probabilidad = pbinom(fallos.operador,proximos.fallos,probabilidad.operador) #pbinom(lo que espero que pase, cantidad de la muestra, probabilidad que pase cada vez)
probabilidad

#Contar el numero de fallos realizado por un operario durante 37 fallos, realizar numero.sampleo experimentos, hacer la media
numero.sampleo = 200000
suma.fallos.operador = rbinom(numero.sampleo,proximos.fallos, probabilidad.operador) #rbinom hace un sampleo de numero.sampleo fallos, el maximo de fallos, probabilidad que sean del operador
fallos.operador = mean(suma.fallos.operador)
fallos.operador

#Grafico de la funcion de probabilidad del numero de fallos
local({
  .x <- 11:30
  plotDistr(.x, dbinom(.x, size=proximos.fallos, prob=probabilidad.operador), xlab="Number of Successes", 
            ylab="Probability Mass", 
            main="Binomial Distribution:  Binomial trials=37, Probability of success=0.56",
            discrete=TRUE)
})