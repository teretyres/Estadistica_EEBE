##EJERCICIO 1
mu<- 0.4412712
sigma<-sqrt(0.07851927)

f <- function(x) {
  4 / (pi * (1 + x^2))
}

x_vals<- seq(0,1, length.out=500)
probf<- f(x_vals)
set.seed(321)
sim<- sample(x_vals,4,replace=T, prob= probf)

obs<- as.data.frame(matrix(sim, ncol=4))
sum_obs<- apply(obs,1,sum)
var(sum_obs)

var_obs<- apply(obs,1,var)
var_obs
sqrt(var_obs)
4*mu

#EJERCICIO 2

