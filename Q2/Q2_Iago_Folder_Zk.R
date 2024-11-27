source("Q2/Probabilidad_Distribucion_Soko.R")
constantes <- ls()[sapply(ls(), function(x) is.atomic(get(x)))]
rm(list=constantes)

## Ejercicio 1 ----
#dist normal
mu=838
n=49

desv.tipica=(tlc_desv.tipica(864,mu,0.975,"<"))*sqrt(n);desv.tipica
# 92.85885 esta es sigma_x

mu_t=mu*n
desv.tipica_t=desv.tipica*sqrt(n)

E_t=mu_t;E_t
# 41062

Var_t=desv.tipica_t^2;Var_t
# 422515.5

#P de que la suma NO sea menor a 41317
1-pnorm_Zk(41317,mu_t,desv.tipica_t,"<")
# 0.3474181

desv.tipica_media=tlc_desv.tipica(863,mu,5/100,"<")
n=(desv.tipica/desv.tipica_media)^2;n
# 37.32683

n=16 #el enunciado general dice que esta dist. normalmente
tlc_mu(941,desv.tipica/sqrt(n),10/100,">")-mu
# 85.99951

## Ejercicio 2 ----
#dist normal
mu=5100
desv.tipica=1020
n=22

Var=desv.tipica^2;Var
# 1040400

NaN

pchisq2_Zk(520200,1352520,desv.tipica,n)
# 0.8105192

## Ejercicio 3 ----
mu=70.5
desv.tipica=8.3

# n<<< no sabemos la distribucion
NaN

# n<<< no sabemos la distribucion
NaN

# n<<< no sabemos la distribucion
NaN

# n<<< no sabemos la distribucion
NaN

n=121
mu_t=mu*n
desv.tipica_t=desv.tipica*sqrt(n)

pnorm_Zk(8640,mu_t,desv.tipica_t,"<")
#0.8848027

## Ejercicio 4----
x=c(0,1,2,3,4,5,6)
f=c(0.19,0.18,0.17,0.16,0.13,0.12,0.05)
n=12
N=1
size=n*N
set.seed(591)
sample=sample(x,size,replace=T,prob=f)
obs=as.data.frame(matrix(sample,ncol=n))

sum(sample)  # 37     
mean(sample) # 3.083333
var(sample) # 2.44697

n=12
N=800
size=n*N
set.seed(591)
sample=sample(x,size,replace=T,prob=f)
obs=as.data.frame(matrix(sample,ncol=n))

sum_x=apply(obs,1,sum)
mean(sum_x) # 29.14375
sd(sum_x) # 6.373489

mean_x=apply(obs,1,mean)
mean(mean_x) # 2.428646
sd(mean_x) # 0.5311241
var(mean_x) # 0.2820928



n=12*5
N=800
size=n*N
set.seed(591)
sample=sample(x,size,replace=T,prob=f)
obs=as.data.frame(matrix(sample,ncol=n))

sum_x=apply(obs,1,sum)
mean(sum_x) # 145.4375
sd(sum_x) # 14.86943

mean_x=apply(obs,1,mean)
mean(mean_x) # 2.423958
sd(mean_x) # 0.2478238
var(mean_x) # 0.06141666
