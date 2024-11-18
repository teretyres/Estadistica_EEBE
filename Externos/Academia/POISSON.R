#POISSON


      landa <-  ;
      valor1 <-  ;
      valor2 <- ;
      k2  <- ;
  
apartado1_exactamente <- dpois(c(valor1),lambda=landa); 
apartado1_menoroigual <- ppois(c(valor1),lambda=landa,lower.tail=TRUE); 
apartado2_mayoroigual <- ppois(c(valor2)-1,lambda=k2*landa,lower.tail=FALSE); 
apartado2_mayor <- ppois(c(valor2),lambda=k2*landa,lower.tail=FALSE);  
apartado2_exactamente <- dpois(c(valor2),lambda=k2*landa); 
n<-1000000;
trials <- rpois(n, lambda=landa) 
apartado3 <- landa
ini<-0;
fin<-landa+3*sqrt(landa);
plot(dpois(ini:fin,landa))
  
  
apartado1_exactamente
apartado1_menoroigual
apartado2_mayoroigual
apartado2_mayor
apartado2_exactamente
apartado3
 