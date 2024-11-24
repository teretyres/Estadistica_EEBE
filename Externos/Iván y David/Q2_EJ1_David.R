############# Ejercicio 1 ############
#apartado 1
p <- 60 / 150
p
dbinom(10, size = 25,p)

#apartado 2

1 - pbinom(14, size = 40,p)

#apartado 3

N <- 200
K <- 80
n <- 30

p <- K / N

n * (p) * (1 - p) * ((N - n) / (N - 1))


qhyper(0.25, m = K, n = N - K, k = n)

#apartado 4

set.seed(789)
N <- 120
K <- 48
n <- 35

mean(rhyper(10000,K,N - K,n))

#apartado 5

p <- 0.4

dnbinom(12 - 5,5,p)




