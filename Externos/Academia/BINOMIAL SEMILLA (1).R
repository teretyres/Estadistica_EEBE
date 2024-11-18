#BINOMIAL CON SEMILLA

    n <- ;
    p <- ;
    numeroSimulaciones <- ;
    semilla <- ;



set.seed(semilla); 
mediaSimulaciones<-mean(rbinom(numeroSimulaciones,n,p));
set.seed(semilla);
medianaSimulaciones<-median(rbinom(numeroSimulaciones,n,p));
set.seed(semilla);
varianzaSimulaciones<-var(rbinom(numeroSimulaciones,n,p));



mediaSimulaciones
medianaSimulaciones
varianzaSimulaciones