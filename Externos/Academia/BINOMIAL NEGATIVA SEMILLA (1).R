#BINOMIAL NEGATIVA CON SEMILLA

    r <- ;
    p <- ;
    numeroSimulaciones <- ;
    semilla <- ;



set.seed(semilla); 
mediaSimulaciones<-mean(rnbinom(numeroSimulaciones,r,p));
set.seed(semilla);
medianaSimulaciones<-median(rnbinom(numeroSimulaciones,r,p));
set.seed(semilla);
varianzaSimulaciones<-var(rnbinom(numeroSimulaciones,r,p));



mediaSimulaciones
medianaSimulaciones
varianzaSimulaciones