## RESUMEN
#d-> f(x)
#p-> F(x)
#r-> simular
#q-> quantile

#f(x)
X<- 0:40
graph<- dpois(X,lambda)
plot(X, graph, type='h', col='red')
#F(x)
plot(X, ppois(X,lambda), type='s', col='red')
#simular
set.seed(123)#misma simu para la misma semilla
sim<- rpois(n,lambda) #importante que la simu este anclada a una variable
mean(sim) #media de simu
median(sim)#mediana de simu
var(sim)#variación de simu


## BINOMIAL
#pruebas repetidas con éxito/fracaso
dbinom(x,n,p)
E_b<-n*p
Var_b<- n*p(1-p)


## HIPERGEOMÉTRICA
#Población finita de éxitos/fracasos, cuanso una unidad es seleccionada, esta disminuye en proporción

#x= nº éxitos buscados
#N= nº población total
#k= nº éxitos totales
#n= nº de repeticiones (sin reemplazo)
dhyper(x,k,N-k,n)
E_h<- n*k/N
Var_h<- n*k/N*(1-k/N)*((N-n)/(N-1))


## BINOMIAL NEGATIVA
#nº de pruebas hasta un nº fijo de éxitos

#x= nº de pruebas
#r= nº éxitos
dnbinom(x,r,p)
E_nb<- r/p
Var_nb<- r*(1-p)/p^2


## POISSON
#nº de resultados en un intervalo de tiempo
dpois(x, lambda)
f_p<- (exp(-lambda)*lambda^x)/factorial(x)
E_p<- lambda
Var_p<- lambda
