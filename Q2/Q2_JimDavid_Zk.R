source("Q2/Probabilidad_Distribucion_Soko.R")
constantes <- ls()[sapply(ls(), function(x) is.atomic(get(x)))]
rm(list=constantes)

## Ejercicio 1 ----
poblacio=150
satisfets=60

X=10 #nº éxitos buscados
N=150 #nº población total
k=60 #nº éxitos totales
n=25 #nº de repeticiones (sin reemplazo)

phyper_Zk(n,k,N,X)
# 0.1764776

n=40
X=15

phyper_Zk(n,k,N,X,">=")
#0.7125035

n=30
N=200
k=80
Var_h<- n*k/N*(1-k/N)*((N-n)/(N-1));Var_h
#6.150754

#primer quartil
qhyper(1/4,k,N-k,n) #10
qhyper_Zk(n,k,N,1/4) #10

#simulacio
#(en R) 
# m=k  n=N-k  k=n
n=35
N=120
k=48
seed=789
set.seed(seed)
simu <- rhyper(10000,k,N-k,n)
mean(simu) #14.0131

#binomial negativa
pnbinom_Zk(5,40/100,12,"=") #0.09459597

## Ejercicio 2----
#dist normal
mu=498
p_489=0.99865

desv.tipica=tlc_desv.tipica(489,mu,p_489,">")

# Probabilidad aceptable
P_aceptable=pnorm2_Zk(492,501,mu,desv.tipica)
No_aceptable=1-P_aceptable;No_aceptable
#0.1814081

P_502=pnorm_Zk(502,mu,desv.tipica,"=");P_502
#0 pq la normal es continua y en todas las continuas P(X=x)=0

mu=499.5
desv.tipica=tlc_desv.tipica(495,mu,0.025,"<");desv.tipica
#2.295961

mu=502
desv.tipica=2
seed=321

set.seed(seed)
simu=rnorm(10000,mu,desv.tipica)
mu=mean(simu)
desv.tipica=sd(simu)

pnorm2_Zk(497,507,mu,desv.tipica)
# 0.9880147

pbinom_Zk(500,0.02,10,"=")
# 0.1263798


## Ejercicio 3----
mu=73.2
desv.tipica=6.6
n=121
#n>>> así q dist normal

Var<- desv.tipica^2;Var
#43.56

#n<<< asi q no sabem la distribucio
NaN

#n<<< asi q no sabem la distribucio
NaN

#n<<< asi q no sabem la distribucio
NaN

#n>>> así q dist normal
#P(S^2<47)

pchisq_Zk(47,desv.tipica,100,"<")
#0.7219768
