media=10000
desviacion=2000
muestra=22
#Valor esperado = Esperanza, E = desviación tipica^2
esperanza = desviacion^2
#lim n-> infinito V(S^2)=0
#Probabilidad que la viaranza esté entre inferior y superior
inferior = 2400000
superior = 4800000
#P(inferior < S^2 z superior)
P = pchisq(superior*(muestra-1)/desviacion^2, df=muestra-1)-pchisq(inferior*(muestra-1)/desviacion^2, df=muestra-1)
P