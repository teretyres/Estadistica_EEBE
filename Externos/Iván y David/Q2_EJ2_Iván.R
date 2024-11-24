mu <- 498 
sigma <- 4
limite_infe<- 492  
limite_supe<- 501  


pnorm(limite_infe, mu, sigma) + (1 - pnorm(limite_supe, mu, sigma))

##########################

mu <- 498 
sigma <- 4

pnorm(502.5, mu, sigma) - pnorm(501.5, mu, sigma)

pnorm_Zk(502, mu, sigma, "=")


###################################33



mu <- 499.5
x <- 495
p <- 0.025

z <- qnorm(p)
sigma <- (x - mu) / z

sigma

##########################3


set.seed(321)

media <- 502
desviacion_estandar <- 2  
npaquetes <- 10000 


simulaion <- rnorm(npaquetes, media, desviacion_estandar)

sum(simulacion >= 497 & simulacion<= 507) / npaquetes


##################################################33

n <- 500  
x <- 10   
p <- 0.02  

dbinom(x, n,p)
pbinom_Zk(n,p,x,"=")








