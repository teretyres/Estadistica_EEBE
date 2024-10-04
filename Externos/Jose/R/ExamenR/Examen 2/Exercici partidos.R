probabilidad.ganar = 0.37
partidos = 26
partidos.ganados = 11
probabilidad = dbinom(partidos.ganados,partidos,probabilidad.ganar)
probabilidad

#probabilidad de que gane más de partidos.ganados
probabilidad.ganar = 0.37
partidos = 26
partidos.ganados = 7
probabilidad = 1- pbinom(partidos.ganados,partidos,probabilidad.ganar)
probabilidad

#Numero de partidos a ganar
