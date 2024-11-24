n <- 25
p <- 60 / 150  
x <- 10 


dbinom(x, n, p)


pbinom_Zk(n, p, x, "=")

####################333

n <- 40  
p <- 60 / 150  
x <- 15 

1 - pbinom(14, n, p)

pbinom_Zk(n,p,x,">=")


#####################3



N <- 200 
x <- 80   
n <- 30 


varianza <- (n * x * (N - x) * (N - n)) / (N^2 * (N - 1))
varianza

primer_cuartil <- qhyper(0.25, x, N - x, n)
primer_cuartil
###############################################33




set.seed(789)


N <- 120  
x <- 48 
n <- 35   
simulaciones <- 10000  


resul <- rhyper(simulaciones, x, N - x, n)


promedio <- mean(resul)
promedio

#############################################33

r <- 5  
p <- 0.40  
x <- 12

dnbinom(x - r, r, p)
pnbinom_Zk(r,p,x,"=")
