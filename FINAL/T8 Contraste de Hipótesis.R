## CRITERIOS IC
#1. IC
# mu0 dentro del IC-> rechaza H1
# mu0 fuera del IC-> rechaza H0

#2. VALOR CRÍTICO
#Sigma conocida
Zc<- c(qnorm(alfa/2),qnorm(1-alfa/2))# 2 colas  Zobs<=-Zc o Zobs>= Zc -> Rechazo H0
Zc<- qnorm(1-alfa)# cola sup.  Zobs>= Zc-> Rechazo H0
Zc<- qnorm(alfa)# cola inf.  Zobs<= Zc-> Rechazo H0
Zobs<- (xbar-mu0)/(sigmax/sqrt(n))

#Sigma desconocida
tc<- c(qt(alfa/2,df=n-1),qt(1-alfa/2,df=n-1))# 2 colas
tc<- qt(1-alfa, df=n-1)# cola sup.
tc<- qt(alfa, df=n-1)# cola inf.
tobs<- (xbar-mu0)/(s/sqrt(n))

#Sigma desconocida, pero n>>>
Zc<- c(qnorm(alfa/2),qnorm(1-alfa/2))# 2 colas
Zc<- qnorm(1-alfa)# cola sup.
Zc<- qnorm(alfa)# cola inf.
Zobs<- (xbar-mu0)/(s/sqrt(n))

#3. P-VALOR
#p-valor< alfa -> Rechazamos H0
pvalor<- 2*pnorm(-Zobs)# 2 colas
pvalor<- pnorm(Zobs)# cola inf.
pvalor<- 1-pnorm(Zobs)# cola sup.
#-------------------------------------------------------------------------------

## ERRORES
#Error I: P(Rechazar H0| H0 cierta)= alfa
#Error: P(NO Rechazar H0| H0 falsa)= beta
beta<- 2*pnorm(xbar, mu0, sqrt(sigma/n))# 2 colas
beta<- 1-pnorm(xbar, mu0, sqrt(sigma/n))# cola sup.
beta<- pnorm(xbar, mu0, sqrt(sigma/n))# cola inf. 

## ACIERTOS
#No error: P(No rechazar H0| H0 cierta)= 1-alfa
#Potencia: P(Rehazar H0| H0 falsa)= 1-beta