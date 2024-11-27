###DISCRETAS###
#Binomial Negativa----
# El mio calcula el numero de PRUEBAS hasta conseguir el valor deseado 
# El de R calcula el numero de FRACASOS hasta conseguir el valor deseado (X=X-n)
# No somos lo mismo
pnbinom_Zk <- function(n,p,X,signo="=")
{
  #' Esta funcion hace la probabilidad de Binomiales Negativas
  #' @param n número de "éxitos"
  #' @param p probabilidad de "éxito"
  #' @param X número de intentos hasta llegar a r "éxitos"
  #' @param signo OPCIONES: "=",">",">=","<","<=". DEFAULT: "="
  Probabilidad = 0
  r=n
  if (X<r) {return(Probabilidad)}
  if (signo == "="){
    Probabilidad = choose(X-1,r-1)*p^r*(1-p)^(X-r)
    return(Probabilidad)
  }
  if (signo == ">"){
    pruebas = r:X
    for (prueba in pruebas){
      Probabilidad = Probabilidad + choose(prueba-1,r-1)*p^r*(1-p)^(prueba-r)
    }
    Probabilidad = 1-Probabilidad
    return(Probabilidad)
  }
  if (signo == ">="){
    pruebas = r:(X-1)
    for (prueba in pruebas){
      Probabilidad = Probabilidad + choose(prueba-1,r-1)*p^r*(1-p)^(prueba-r)
    }
    Probabilidad = 1-Probabilidad
    return(Probabilidad)
  }
  if (signo == "<"){
    pruebas = r:(X-1)
    for (prueba in pruebas){
      Probabilidad = Probabilidad + choose(prueba-1,r-1)*p^r*(1-p)^(prueba-r)
    }
    return(Probabilidad)
  }
  if (signo == "<="){
    pruebas = r:X
    for (prueba in pruebas){
      Probabilidad = Probabilidad + choose(prueba-1,r-1)*p^r*(1-p)^(prueba-r)
    }
    return(Probabilidad)
  }
  return("ERROR")
}
pnbinom2_zk <- function(n,p,X1,X2,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=pnbinom_Zk(n,p,X1,"<=")
    Prob2=pnbinom_Zk(n,p,X2,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=pnbinom_Zk(n,p,X1,"<=")
    Prob2=pnbinom_Zk(n,p,X2,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=pnbinom_Zk(n,p,X1,"<")
    Prob2=pnbinom_Zk(n,p,X2,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=pnbinom_Zk(n,p,X1,"<")
    Prob2=pnbinom_Zk(n,p,X2,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=pnbinom_Zk(n,p,X2,"<=")
    Prob1=pnbinom_Zk(n,p,X1,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=pnbinom_Zk(n,p,X2,"<")
    Prob1=pnbinom_Zk(n,p,X1,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
  Prob2=pnbinom_Zk(n,p,X2,"<=")
  Prob1=pnbinom_Zk(n,p,X1,"<=")
  Probabilidad = Prob1-Prob2
  return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=pnbinom_Zk(n,p,X2,"<")
    Prob1=pnbinom_Zk(n,p,X1,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}

#Hipergeometrica----
phyper_Zk <- function(n,k,N,X,signo="=")
{
  #' Esta funcion hace la probabilidad de HiperGeometricas
  #' @param n numero de repeticiones sin reemplazo
  #' @param k numero de posibilidades de exito (elementos del grupo q queremos)
  #' @param N numero total de elementos
  #' @param X Numero de exitos
  #' @param signo OPCIONES: "=",">",">=","<","<=". DEFAULT: "="
  Probabilidad = 0
  if (X>n) {return("Caso de X>n!!, P=0")}
  if (signo == "="){
    Probabilidad = dhyper(X,k,N-k,n)
    return(Probabilidad)
  }
  if (signo == ">"){
    pruebas = (X+1):n
    Probabilidad = Probabilidad + sum(dhyper(pruebas,k,N-k,n))
    return(Probabilidad)
  }
  if (signo == ">="){
    pruebas = X:n
    Probabilidad = Probabilidad + sum(dhyper(pruebas,k,N-k,n))
    return(Probabilidad)
  }
  if (signo == "<"){
    pruebas = X:n
    Probabilidad = Probabilidad + sum(dhyper(pruebas,k,N-k,n))
    Probalilidad = 1 - Probabilidad
    return(Probabilidad)
  }
  if (signo == "<="){
    pruebas = (X+1):n
    Probabilidad = Probabilidad + sum(dhyper(pruebas,k,N-k,n))
    Probalilidad = 1 - Probabilidad
    return(Probabilidad)
  }
  return("ERROR")
}
phyper2_Zk <- function(n,k,N,X1,X2,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=phyper_Zk(n,k,N,X1,"<=")
    Prob2=phyper_Zk(n,k,N,X2,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=phyper_Zk(n,k,N,X1,"<=")
    Prob2=phyper_Zk(n,k,N,X2,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=phyper_Zk(n,k,N,X1,"<")
    Prob2=phyper_Zk(n,k,N,X2,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=phyper_Zk(n,k,N,X1,"<")
    Prob2=phyper_Zk(n,k,N,X2,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=phyper_Zk(n,k,N,X2,"<=")
    Prob1=phyper_Zk(n,k,N,X1,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=phyper_Zk(n,k,N,X2,"<")
    Prob1=phyper_Zk(n,k,N,X1,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=phyper_Zk(n,k,N,X2,"<=")
    Prob1=phyper_Zk(n,k,N,X1,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=phyper_Zk(n,k,N,X2,"<")
    Prob1=phyper_Zk(n,k,N,X1,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}
qhyper_Zk <- function(n,k,N,X,lower.tail=T)
{return(qhyper(X,k,N-k,n,lower.tail = lower.tail))}
#Binomial----
pbinom_Zk <- function(n,p,X,signo="=")
{
  Probabilidad = 0
  if (X>n) {return("Caso X<n, P=0")}
  if (signo == "="){
    Probabilidad = dbinom(X,n,p)
    return(Probabilidad)
  }
  if (signo == "<"){
    exitos = 0:(X-1)
    for (exito in exitos){
      Probabilidad = Probabilidad + dbinom(exito,n,p)
    }
    return(Probabilidad)
  }
  if (signo == "<="){
    exitos = 0:X
  for (exito in exitos){
    Probabilidad = Probabilidad + dbinom(exito,n,p)
  }
    return(Probabilidad)
  }
  if (signo == ">"){
    exitos = 0:X
    for (exito in exitos){
      Probabilidad = Probabilidad + dbinom(exito,n,p)
    }
    return(1-Probabilidad)
  }
  if (signo == ">="){
    exitos = 0:(X-1)
    for (exito in exitos){
      Probabilidad = Probabilidad + dbinom(exito,n,p)
    }
    return(1-Probabilidad)
  }
  return("ERROR")
}
pbinom2_Zk <- function(n,p,X1,X2,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=pbinom_Zk(n,p,X1,"<=")
    Prob2=pbinom_Zk(n,p,X2,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=pbinom_Zk(n,p,X1,"<=")
    Prob2=pbinom_Zk(n,p,X2,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=pbinom_Zk(n,p,X1,"<")
    Prob2=pbinom_Zk(n,p,X2,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=pbinom_Zk(n,p,X1,"<")
    Prob2=pbinom_Zk(n,p,X2,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=pbinom_Zk(n,p,X2,"<=")
    Prob1=pbinom_Zk(n,p,X1,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=pbinom_Zk(n,p,X2,"<")
    Prob1=pbinom_Zk(n,p,X1,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=pbinom_Zk(n,p,X2,"<=")
    Prob1=pbinom_Zk(n,p,X1,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=pbinom_Zk(n,p,X2,"<")
    Prob1=pbinom_Zk(n,p,X1,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}

#Geometrica----
pgeom_Zk <- function(X,p,signo="=")
{
  Probailidad = 0
  if (signo == "=")
  {
    Probabilidad = dgeom(X,p)
    return(Probabilidad)
  }
  if (signo == ">")
  {
    intentos = 0:X
    for (intento in intentos){
      Probabilidad = Probabilidad + dgeom(intento,p)
    }
    return(1-Probabilidad)
  }
  if (signo == ">=")
  {
    intentos = 1:(X-1)
    for (intento in intentos){
      Probabilidad = Probabilidad + dgeom(intento,p)
    }
    return(1-Probabilidad)
  }
  if (signo == "<")
  {
    intentos = 1:(X-1)
    for (intento in intentos){
      Probabilidad = Probabilidad + dgeom(intento,p)
    }
    return(Probabilidad)
  }
  if (signo == "<=")
  {
    intentos = 1:X
    for (intento in intentos){
      Probabilidad = Probabilidad + dgeom(intento,p)
    }
    return(Probabilidad)
  }
  return("ERROR")
}
pgeom2_Zk <- function(X1,X2,p,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=pgeom_Zk(X1,p,"<=")
    Prob2=pgeom_Zk(X2,p,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=pgeom_Zk(X1,p,"<=")
    Prob2=pgeom_Zk(X2,p,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=pgeom_Zk(X1,p,"<")
    Prob2=pgeom_Zk(X2,p,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=pgeom_Zk(X1,p,"<")
    Prob2=pgeom_Zk(X2,p,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=pgeom_Zk(X2,p,"<=")
    Prob1=pgeom_Zk(X1,p,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=pgeom_Zk(X2,p,"<")
    Prob1=pgeom_Zk(X1,p,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=pgeom_Zk(X2,p,"<=")
    Prob1=pgeom_Zk(X1,p,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=pgeom_Zk(X2,p,"<")
    Prob1=pgeom_Zk(X1,p,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}

#Poisson----
ppois_Zk <- function(X,lambda,signo="=")
{
  Probabilidad = 0
  if (signo == "=")
  {
    Probabilidad = dpois(X,lambda)
    return(Probabilidad)
  }
  if (signo == ">")
  {
    intentos = 0:X
    for (intento in intentos){
      Probabilidad = Probabilidad + dpois(X,lambda)
    }
    return(1-Probabilidad)
  }
  if (signo == ">=")
  {
    intentos = 0:(X-1)
    for (intento in intentos){
      Probabilidad = Probabilidad + dpois(intento,lambda)
    }
    return(1-Probabilidad)
  }
  if (signo == "<")
  {
    intentos = 0:(X-1)
    for (intento in intentos){
      Probabilidad = Probabilidad + dpois(intento,lambda)
    }
    return(Probabilidad)
  }
  if (signo == "<=")
  {
    intentos = 0:X
    for (intento in intentos){
      Probabilidad = Probabilidad + dpois(intento,lambda)
    }
    return(Probabilidad)
  }
  return("ERROR")
}
ppois2_Zk <- function(X1,X2,lambda,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=ppois_Zk(X1,lambda,"<=")
    Prob2=ppois_Zk(X2,lambda,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=ppois_Zk(X1,lambda,"<=")
    Prob2=ppois_Zk(X2,lambda,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=ppois_Zk(X1,lambda,"<")
    Prob2=ppois_Zk(X2,lambda,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=ppois_Zk(X1,lambda,"<")
    Prob2=ppois_Zk(X2,lambda,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=ppois_Zk(X2,lambda,"<=")
    Prob1=ppois_Zk(X1,lambda,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=ppois_Zk(X2,lambda,"<")
    Prob1=ppois_Zk(X1,lambda,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=ppois_Zk(X2,lambda,"<=")
    Prob1=ppois_Zk(X1,lambda,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=ppois_Zk(X2,lambda,"<")
    Prob1=ppois_Zk(X1,lambda,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}


###CONTINUAS###
#Normal----
pnorm_Zk <-function(X,media,desv.tipica,signo="=")
{
  Probabilidad = 0
  if (signo == "=")
  {
    Probabilidad = 0
    return(Probabilidad)
  }
  if (signo == ">"|signo == ">=")
  {
    Probabilidad = pnorm(X,media,desv.tipica)
  
    return(1-Probabilidad)
  }
  
  if (signo == "<"|signo == "<=")
  {
    Probabilidad = pnorm(X,media,desv.tipica)
    return(Probabilidad)
  }
  return("ERROR")
}
pnorm2_Zk <- function(X1,X2,media,desv.tipica,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<=")
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<=")
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<")
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<")
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<=")
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<")
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<=")
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=pnorm_Zk(X2,media,desv.tipica,"<")
    Prob1=pnorm_Zk(X1,media,desv.tipica,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}
tlc_desv.tipica <- function(X,mu,p,signo ="<")
{
  if (signo==">"|signo==">="){
    p=1-p
  }
  answer=(X-mu)/qnorm(p)
  return(answer)
}
tlc_mu <- function(X,desv.tipica,p,signo ="<")
{
  if (signo==">"|signo==">="){
    p=1-p
  }
  answer=(X-qnorm(p)*desv.tipica)
  return(answer)
}
tlc_X <- function(mu,desv.tipica,p,signo="<")
{
  if (signo==">"|signo==">="){
    p=1-p
  }
  answer=qnorm(p)*desv.tipica+mu
  return(answer)
}
#Exponencial----
pexp_Zk <- function(X,lambda,signo="=")
{
  Probabilidad = 0
  if (signo == "=")
  {
    Probabilidad = 0
    return(Probabilidad)
  }
  if (signo == ">"|signo == ">=")
  {
    Probabilidad = pexp(X,lambda)
    
    return(1-Probabilidad)
  }
  
  if (signo == "<"|signo == "<=")
  {
    Probabilidad = pexp(X,lambda)
    return(Probabilidad)
  }
  return("ERROR")  
}
pexp2_Zk <- function(X1,X2,lambda,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=pexp_Zk(X1,lambda,"<=")
    Prob2=pexp_Zk(X2,lambda,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=pexp_Zk(X1,lambda,"<=")
    Prob2=pexp_Zk(X2,lambda,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=pexp_Zk(X1,lambda,"<")
    Prob2=pexp_Zk(X2,lambda,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=pexp_Zk(X1,lambda,"<")
    Prob2=pexp_Zk(X2,lambda,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=pexp_Zk(X2,lambda,"<=")
    Prob1=pexp_Zk(X1,lambda,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=pexp_Zk(X2,lambda,"<")
    Prob1=pexp_Zk(X1,lambda,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=pexp_Zk(X2,lambda,"<=")
    Prob1=pexp_Zk(X1,lambda,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=pexp_Zk(X2,lambda,"<")
    Prob1=pexp_Zk(X1,lambda,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
}

#Uniforme----
punif_Zk <- function(X,min,max,signo="=")
{
  Probabilidad = 0
  if (signo == "=")
  {
    Probabilidad = 0
    return(Probabilidad)
  }
  if (signo == ">"|signo == ">=")
  {
    Probabilidad = punif(X,min,max)
    
    return(1-Probabilidad)
  }
  
  if (signo == "<"|signo == "<=")
  {
    Probabilidad = punif(X,min,max)
    return(Probabilidad)
  }
  return("ERROR")
}
punif2_Zk <- function(X1,X2,min,max,signos="< <")
{
    Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=punif_Zk(X1,min,max,"<=")
    Prob2=punif_Zk(X2,min,max,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=punif_Zk(X1,min,max,"<=")
    Prob2=punif_Zk(X2,min,max,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=punif_Zk(X1,min,max,"<")
    Prob2=punif_Zk(X2,min,max,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=punif_Zk(X1,min,max,"<")
    Prob2=punif_Zk(X2,min,max,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=punif_Zk(X2,min,max,"<=")
    Prob1=punif_Zk(X1,min,max,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=punif_Zk(X2,min,max,"<")
    Prob1=punif_Zk(X1,min,max,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=punif_Zk(X2,min,max,"<=")
    Prob1=punif_Zk(X1,min,max,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=punif_Zk(X2,min,max,"<")
    Prob1=punif_Zk(X1,min,max,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")

}

#X^2----

pchisq_Zk <- function(S_2,desv.tipica,n,signo="=")
{
  Probabilidad = 0
  if (signo == "=")
  {
    Probabilidad = 0
    return(Probabilidad)
  }
  if (signo == ">"|signo == ">=")
  {
    Probabilidad = pchisq((n-1)*S_2/desv.tipica^2, df=n-1)
    
    return(1-Probabilidad)
  }
  
  if (signo == "<"|signo == "<=")
  {
    Probabilidad = pchisq((n-1)*S_2/desv.tipica^2, df=n-1)
    return(Probabilidad)
  }
  return("ERROR")
}
pchisq2_Zk <- function(S1_2,S2_2,desv.tipica,n,signos="< <")
{
  Probabilidad = 0;Prob1 = 0;Prob2 = 0;
  if (signos == "< <"){
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<=")
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "< <="){
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<=")
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <"){
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<")
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  if (signos == "<= <="){
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<")
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<=")
    Probabilidad = Prob2-Prob1
    return(Probabilidad)
  }
  
  if (signos == "> >"){
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<=")
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == "> >="){
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<")
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  if (signos == ">= >"){
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<=")
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)}
  
  if (signos == ">= >="){
    Prob2=pchisq_Zk(S2_2,desv.tipica,n,"<")
    Prob1=pchisq_Zk(S1_2,desv.tipica,n,"<=")
    Probabilidad = Prob1-Prob2
    return(Probabilidad)
  }
  return("ERROR")
  
}
