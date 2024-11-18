#EXPONENCIAL CON SEMILLA

    media <- ;
    almenos <- ;#APARTADO 1
    superan <- ;#APARTADO 2
    numeroSimulaciones <- ;
    semilla <- ;


apartado1<-pexp(almenos,1/media,lower.tail=FALSE);
apartado2<-qexp(superan,1/media,lower.tail=FALSE);
set.seed(semilla); 
mediaSimulaciones<-mean(rexp(numeroSimulaciones,1/media));
set.seed(semilla);
medianaSimulaciones<-median(rexp(numeroSimulaciones,1/media));
set.seed(semilla);
varianzaSimulaciones<-var(rexp(numeroSimulaciones,1/media));


apartado1
apartado2
mediaSimulaciones
medianaSimulaciones
varianzaSimulaciones