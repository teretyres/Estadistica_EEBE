#POISSON


      landa <-  ;
  
      valor1 <-  ;
  
      valor2 <- ;
      k2  <- ;
  
  a_exactamente <- dpois(c(valor1),lambda=landa); 
  a_menoroigual <- ppois(c(valor1),lambda=landa,lower.tail=TRUE); 
  b_mayoroigual <- ppois(c(valor2)-1,lambda=k2*landa,lower.tail=FALSE); 
  b_mayor <- ppois(c(valor2),lambda=k2*landa,lower.tail=FALSE);  
  b_exactamente <- dpois(c(valor2),lambda=k2*landa); 
  n<-1000000;
  trials <- rpois(n, lambda=landa) 
  c <- var(trials)
  ini<-0;
  fin<-landa+3*sqrt(landa);
  plot(dpois(ini:fin,landa))
  
  #apartado 1#
  a_exactamente
  a_menoroigual
 
  #apartado 2
  b_mayoroigual
  b_mayor
  b_exactamente
  
  c