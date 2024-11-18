#NORMAL: problema de aptos/no aptos


      media <- ;
      varianza <- ;
      valor2 <- ;#mayor                               
      valor3inf <- ;#entre 
      valor3sup <- ;#estos dos
      porcentaje4 <- ;#menor
      puntosMas5 <- ;#mayor
      

desviacion <- sqrt(varianza);
b <- pnorm(c(valor2),mean=media,sd=desviacion,lower.tail=FALSE); 
c1 <- pnorm(c(valor3inf),mean=media,sd=desviacion,lower.tail=TRUE);
c2 <- pnorm(c(valor3sup),mean=media,sd=desviacion,lower.tail=TRUE);
d <- qnorm(c(porcentaje4),mean=media,sd=desviacion,lower.tail=TRUE); 
e <- pnorm(d+puntosMas5,mean=media,sd=desviacion,lower.tail=FALSE);
apartado2=b;
apartado3=c2-c1;
apartado4=d;
apartado5=e;
ini <- media-4*desviacion;
fin <- media+4*desviacion;
curve(dnorm(x,media,desviacion),xlim=c(ini,fin))		


apartado2
apartado3
apartado4
apartado5