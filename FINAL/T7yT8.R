library(BSDA)
################################################################################
## T7 ESTIMACIÓN PUNTUAL
#propiedades ln: ln(a*b)= ln(a)+ln(b), ln(a/b)=ln(a)-ln(b), ln(a^b)= b*ln(a) 
################################################################################
## T7 ESTIMACIÓN INTERVALAR
# 1. Est. Media

#Z<- (xbar-mux)/(sigmax/sqrt(n)) #varianza conocida y distr. normal
#Z<- qnorm(1-alfa/2)
#EE<- sigmax/sqrt(n); EE #Error estándar
#ME<- Z*EE #Margen de error
#IC<- c(xbar-ME, xbar+ME)
#z.test(x, sigma.x=sigmax, conf.level= 1-alfa)#alternative='greater' (cola sup.)/ 'less' (cola inf.)
#-------------------------------------------------------------------------------
#TS<- (xbar-mux)/(s/sqrt(n))#varianza desconocida y distr. normal
#TS<- qt(1-alfa/2, df=n-1)
#EE<- sd(X)/sqrt(n)
#ME<- TS*EE
#IC<- c(xbar-ME, xbar+ME)
#t.test(x, conf.level= 1-alfa)
#-------------------------------------------------------------------------------
#IC<- c(xbar-Z*sigmax/sqrt(n), xbar+Z*sigmax/sqrt(n)) #n>> y sigmax conocida
#IC<- c(xbar-Z*s/sqrt(n), xbar+Z*s/sqrt(n)) #n>> y sigmax desconocida
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2. Est. Varianza
#chi1<- qchisq(alfa/2, df=n-1)#doble cola
#chi2<- qchisq(1-alfa/2, df=n-1)#doble cola
#IC<- c((n-1)*s^2/chi2, (n-1)*s^2/chi1) #para desviación hacer sqrt(IC)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 3. Est. Proporción
#Distr. binom. si:n*p>5 y n*(1-p)>5 o n*pbar>5 t n*pbar(1-pbar)>5
#pbar<- X/n
#Z<- (pbar-p)/sqrt(p*(1-p)/n)
#Z<- qnorm(1-alfa/2)
#IC<- c(pbar- Z*sqrt(p*(1-p)/n), pbar+ Z*sqrt(p*(1-p)/n)) # si n>>> en vez de p podemos usar pbar
#p=0.5 para maximizar (preguntan n)
################################################################################
#SIMULACIÓN LIMITES
#L_inf<- function(i){
#  variable1<- rnorm(n,mux,sigmax)
#  mean(variable1)-qnorm(1-alfa/2)*sigmax/sqrt(n)
#}
#set.seed()
#simL_inf<- sapply(1:N, L_inf)
#L_sup<- function(i){
#  variable2<- rnorm(6,mux,sigmax)
#  mean(variable2)+qnorm(1-alfa/2)*sigmax/sqrt(n)
#}
#set.seed()
#simL_sup<- sapply(1:N, L_sup)

################################################################################
## T8 CONTRASTE HIPÓTESIS
# 1. Valor Crítico

#Sigma conocida
#Zc<- c(qnorm(alfa/2),qnorm(1-alfa/2))# 2 colas  Zobs<=-Zc o Zobs>= Zc -> Rechazo H0
#Zc<- qnorm(1-alfa)# cola sup.  Zobs>= Zc-> Rechazo H0
#Zc<- qnorm(alfa)# cola inf.  Zobs<= Zc-> Rechazo H0
#Zobs<- (xbar-mu0)/(sigmax/sqrt(n))
#-------------------------------------------------------------------------------
#Sigma desconocida
#tc<- c(qt(alfa/2,df=n-1),qt(1-alfa/2,df=n-1))# 2 colas
#tc<- qt(1-alfa, df=n-1)# cola sup.
#tc<- qt(alfa, df=n-1)# cola inf.
#tobs<- (xbar-mu0)/(s/sqrt(n))
#-------------------------------------------------------------------------------
#Sigma desconocida, pero n>>>
#Zc<- c(qnorm(alfa/2),qnorm(1-alfa/2))# 2 colas
#Zc<- qnorm(1-alfa)# cola sup.
#Zc<- qnorm(alfa)# cola inf.
#Zobs<- (xbar-mu0)/(s/sqrt(n))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 3. p-Valor
#p-valor< alfa -> Rechazamos H0
#pvalor<- 2*pnorm(-Zobs)# 2 colas
#pvalor<- pnorm(Zobs)# cola inf.
#pvalor<- 1-pnorm(Zobs)# cola sup.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Errores
#Error I: P(Rechazar H0| H0 cierta)= alfa
#Error II: P(NO Rechazar H0| H0 falsa)= beta
#beta<- 2*pnorm(xbar, mu0, sqrt(sigma/n))# 2 colas
#beta<- 1-pnorm(xbar, mu0, sqrt(sigma/n))# cola sup.
#beta<- pnorm(xbar, mu0, sqrt(sigma/n))# cola inf. 

# Aciertos
#No error: P(No rechazar H0| H0 cierta)= 1-alfa
#Potencia: P(Rehazar H0| H0 falsa)= 1-beta

