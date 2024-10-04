#VARIABLE ALEATORIA CONTINUA

      a <-  ;
  
      b <-  ;
  
      h<-0.001; x=seq(a,b,by=h); fx = ;
  
  
  n <- 1000000;
  trials=sample(x,n,replace=T,prob=fx)
  media<-mean(trials);
  varianza<-var(trials);
  
  plot(x,fx,type="l",col="red",lwd=3)
  
  media
  varianza
  

  
#PREGUNTA TEORICA:

#    trials=sample(x,4000,replace=T,prob=fx)

  