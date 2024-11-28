source("Q2/Probabilidad_Distribucion_Soko.R")
constantes <- ls()[sapply(ls(), function(x) is.atomic(get(x)))]
rm(list=constantes)

##----
#cada muestra de 1 cc
  # 15% de prob de tener 1 molecula rara

#1
p=15/100
n=3 #num de exitos
X=8
pnbinom_Zk(n,p,X,"=") #0.03144761

#2
n=5
X=10
pnbinom_Zk(n,p,10,">=")

#3
#cada muestra p=20% de tener molecula rara

n=4
p=20/100

E_nb<- n/p; E_nb #20
Var_nb<- n*(1-p)/p^2; Var_nb #80

#4
p=12/100
n=6
set.seed(789)
simu<- rnbinom(10000,6,p)
mean(simu) #44.244

#5
n=15
p=10/100
X=4
pbinom_Zk(n,p,X,"=") #0.04283515

##----
#dist. normal
mu=330 #por lata
# P(X>320)=0.995
# Aceptable sii 325<X<335

desv.tipica=tlc_desv.tipica(320,mu,0.995,">")

#1
1-pnorm2_Zk(325,335,mu,desv.tipica) #0.1977757

#2
x=336
pnorm_Zk(336,mu,desv.tipica,"=") # Es continua así que P(X=x)=0

#3
mu=331
p=0.05
X=327
desv.tipica=tlc_desv.tipica(X,mu,p,"<") #  2.431827

#4
mu=329
desv.tipica=2
n=10000
set.seed(456)
sample <- rnorm(10000,mu,desv.tipica)


sum(sample>=324 & sample<=334)/n


#5
p=3/100 # de añadir/quitar a saber cuantos mL
# cada lata contiene 330ml

NaN #no se sabe cuanto liquido se extrae o añade cuando "se quita un poco de liquido de cualquier lata"
# tanto pueden sacar 1ml como 20ml o 175ml

##----
mu=913
desv.tipica=80
#dist normal

n=100
mu_s=desv.tipica^2;mu_s

n=81
NaN # No se sabe (=0 si n=infinity)

n=1
pnorm_Zk(1001,mu,desv.tipica,">")

n=16
mu_mean=mu
desv_mean=desv.tipica/sqrt(n)
pnorm_Zk(891,mu_mean,desv_mean,">")

n=121
pchisq_Zk(891,desv.tipica,n,">")
