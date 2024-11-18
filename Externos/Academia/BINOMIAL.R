#BINOMIAL

    n <- ;
    probabilidad <- ;
    n1 <- n;
    valor1 <- ;#exactamente
    n2 <- n;
    valor2 <- ;#menor o igual
    n3 <- n;    
    prob3 <- probabilidad;
    n_media <- n ;
    

apartado1_mayoroigual <- pbinom(c(valor1-1),size=n1,prob=probabilidad,lower.tail=FALSE); 
apartado1_exactamente <- dbinom(c(valor1),size=n1,prob=probabilidad); 

apartado2<- pbinom(c(valor2+1),size=n2,prob=probabilidad); 


n4 <- 1000000
trials <- rbinom(n4, size=n_media,prob=probabilidad) 
promedio <- c(n*probabilidad);

ini <-  n1*probabilidad-4*sqrt(n1*probabilidad*(1-probabilidad));
fin <-  n1*probabilidad+4*sqrt(n1*probabilidad*(1-probabilidad));
x=seq(floor(ini),floor(fin),by=1);
fx=dbinom(x, size=n1, prob=probabilidad);
plot(x, fx, type="h", col="red", lwd=3, xlab="X", ylab="f(x)")
     
          

apartado1_mayoroigual
apartado1_exactamente
apartado2
promedio
