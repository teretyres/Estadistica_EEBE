#POISSON CON SEMILLA

    landa <- ;
    numeroSimulaciones <- ;
    semilla <- ;



set.seed(semilla); 
mediaSimulaciones<-mean(rpois(numeroSimulaciones,landa));
set.seed(semilla);
medianaSimulaciones<-median(rpois(numeroSimulaciones,landa));
set.seed(semilla);
varianzaSimulaciones<-var(rpois(numeroSimulaciones,landa));



mediaSimulaciones
medianaSimulaciones
varianzaSimulaciones