#NORMAL CON SEMILLA

    media <- ;
    desviacion <- ;
    numeroSimulaciones <- ;
    semilla <- ;



set.seed(semilla); 
mediaSimulaciones<-mean(rnorm(numeroSimulaciones,media,desviacion));
set.seed(semilla);
medianaSimulaciones<-median(rnorm(numeroSimulaciones,media,desviacion));
set.seed(semilla);
varianzaSimulaciones<-var(rnorm(numeroSimulaciones,media,desviacion));



mediaSimulaciones
medianaSimulaciones
varianzaSimulaciones