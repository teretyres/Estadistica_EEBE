
## NORMAL
#mu= esperanza/media
#sigma= desviación típica
dnorm(x, mu, sigma)
f_n<- 1/(sigma*sqrt(2*pi))
Var<- sigma^2
Z<- (x-mu)/sigma
#graficar
curve(dnorm(x,mu, sd=sigma), xlim=c(x0, xf), col='red') #NO es necesario definir x (x0 y xf marcan inicio y final de la gráfica)


## UNIFORME CONTÍNUA: probabilidad uniforme (densidad plana)
dunif(x,a,b)#ay b son parámetros
f_u<- 1/(b-a)
F_u<- (x-a)/(b-a)
E_u<- (a+b)/2
V_u<- (b-a)^2/12


## EXPONENCIAL: tiempo q transcurre antes de un evento
dexp(x, lambda)
f_e<- lambda*exp(-lambda*x)
F_e<- 1-exp(-lambda*x)
E_e<- 1/lambda
V_e<- 1/lambda^2


