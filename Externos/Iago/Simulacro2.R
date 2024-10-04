#Ejercicio 1
#a
#probabilidad averia o no = 0.5
#lambda1=-log(0.5)
l1=-log(0.5); l1
#3 meses; l3=3*l1 
l3 = 3*l1; l3
1-ppois(1,l3)
#b
#probabilidad trimestre sin averias: P(Y=0) = exp(-l3)
noav = exp(-l3); noav
#probabilidad trimestre con averias: P(Y=0) = 1- exp(-l3)
siav = 1- exp(-l3); siav
#Probabilidad de que tengan que pasar dos trimestres con averias antes de observar el primer trimestre ”plus”.
dnbinom(2,1,noav) #binomial negativa (cuenta 0 y para en 1)

#Ejercicio 2
#a 
p = 0.3
n = 20
1 - pbinom(2,20,0.3) #binomial positiva (cuenta 1 y para en 0)
#b
#K = 20*0.25 = 5 (enfermos de alergia)
#N = 20 (nº total)
#n = 7 (los seleccionados de los 20)
1 - phyper(3,5,15,7)
#phyper(x,K,N-K,h)
#el modelo hipergeomerico se usa cuando coges una muestra del experimento total 