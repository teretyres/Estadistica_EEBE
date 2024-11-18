#NORMAL: problema de probetas


      media <- ;
      desviacion <- ;
      valor2 <- ;#mayor
      valor3inf <- ;#entre
      valor3sup <- ;#estos dos
      percentil <- ;
      n5 <- ;#SUMA
      valor5 <- ;#inferior
      


apartado2 <- pnorm(c(valor2),mean=media,sd=desviacion,lower.tail=FALSE); 
c1 <- pnorm(c(valor3inf),mean=media,sd=desviacion,lower.tail=TRUE);
c2 <- pnorm(c(valor3sup),mean=media,sd=desviacion,lower.tail=TRUE); 
apartado4 <- qnorm(c(percentil),mean=media,sd=desviacion,lower.tail=TRUE); 
apartado5 <- pnorm(c(valor5),mean=n5*media,sd=sqrt(n5)*desviacion,lower.tail=TRUE); 
apartado3=c2-c1;
ini <- media-4*desviacion;
fin <- media+4*desviacion;
curve(dnorm(x,media,desviacion),xlim=c(ini,fin))		

apartado2
apartado3
apartado4
apartado5