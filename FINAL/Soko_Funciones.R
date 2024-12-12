IC_mu_Zk <- function(x,n,desv.tipica=NaN,confianza=0.95,S_n1=NaN,S=NaN)
{
  x_bar=mean(x)
  beta=confianza+(1-confianza)/2
  if (is.nan(S_n1) & length(x)>1)
  {
    S_n1=sqrt((sum(x-x_bar)^2)/n-1)
  }
  if (!is.nan(S)) 
  {
    S_n1=S*(sqrt(n/(n-1)))
  }
  if (is.nan(desv.tipica) & length(x)>1)
  {
    desv.tipica=sd(x)
  }
  if (!is.nan(desv.tipica))
  {
    z=qnorm(beta)
    other=desv.tipica/sqrt(n)
  }
  else if (!is.nan(S_n1))
  {
    z=qt(beta,df=n-1)
    other=S_n1/sqrt(n)
  }
  else return("ERROR")
  max=x_bar+z*other
  min=x_bar-z*other
  print("Margen de error:")
  print(z*other)
  print("")
  print("IC:")
  return(c(min,max))
}
IC_desv.tipica_Zk <- function(x,n,S_n1=NaN,S=NaN,confianza=0.95)
{
  x_bar=mean(x)
  beta=confianza+(1-confianza)/2
  if (is.nan(S_n1) & length(x)>1)
  {
    S_n1=sqrt((sum(x-x_bar)^2)/n-1)
  }
  if (!is.nan(S)) 
  {
    S_n1=S*(sqrt(n/(n-1)))
  }
  if (!is.nan(S_n1))
  {
    x_2_pos=qchisq(confianza+(1-confianza)/2,df=n-1)
    x_2_neg=qchisq(1-(confianza+(1-confianza)/2),df=n-1)
    other=(n-1)*S_n1^2
  }
  else return("ERROR")
  max= sqrt(other/x_2_neg)
  min= sqrt(other/x_2_pos)
  return(c(min,max))
}
IC_p_ZK <- function(n,p,confianza=0.95)
{
  p_bar=p
  beta=confianza+(1-confianza)/2
  if (n*p<5 | n*(1-p)<5) return("NONE POSIBLE")
  if(!is.nan(p))
  {
    z=qnorm(beta)
    other=sqrt((p_bar*(1-p_bar))/n)
  }
  else return("ERROR")
  max=p_bar+z*other
  min=p_bar-z*other
  print("Margen de error:")
  print(z*other)
  print("")
  print("IC:")
  return(c(min,max))
}

IC_mu_Zk(x=16,n=64,desv.tipica = 3)
IC_mu_Zk(x=3314.1,S_n1=sqrt(11457626.9/(11-1)),confianza=0.95,n=11)

IC_mu_Zk(x=324.5,n=10,S_n1=14.83,confianza=0.99)

IC_p_ZK(200,136/200)
IC_desv.tipica_Zk(x=324.5,n=10,S_n1=14.663)
