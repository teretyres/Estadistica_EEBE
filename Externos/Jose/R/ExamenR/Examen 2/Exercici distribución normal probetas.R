media = 5.1
desviacion.estandar = 1.055
#Grafico
local({
  .x <- seq(2.489, 8.511, length.out=1000)  
  plotDistr(.x, dnorm(.x, mean=media, sd=desviacion.estandar), cdf=FALSE, xlab="x", 
            ylab="Density", 
            main=paste("Normal Distribution:  Mean=5.5, Standard deviation=0.915"))
})
#Probabilidad que tenga más de x.cantidad
x.cantidad = 4
pnorm(c(x.cantidad), mean=media, sd=desviacion.estandar, lower.tail=FALSE)

#Probabilidad que este entre inferior y superior
inferior = 3
superior = 5.1
pnorm(c(inferior), mean=media, sd=desviacion.estandar, lower.tail=FALSE)-pnorm(c(superior), mean=media, sd=desviacion.estandar, lower.tail=FALSE)

#Percentil de la funció densitat
percentil = 0.74
qnorm(c(percentil), mean=media, sd=desviacion.estandar, lower.tail=TRUE)

#Contenido total menor a contenido.maximo
muestra = 14
contenido.maximo = 74.5
# Calcular la probabilidad usando la función de distribución acumulada
media.total = muestra * media
desviacion.total = sqrt(muestra)*desviacion.estandar
probabilidad <- pnorm(contenido.maximo, mean = media.total, sd = desviacion.total)
probabilidad



