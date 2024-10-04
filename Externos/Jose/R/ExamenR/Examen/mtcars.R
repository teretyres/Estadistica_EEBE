rm(list=ls())
dat = mtcars;
plot(dat$wt, dat$qsec)

modelo=lm(dat$qsec~dat$wt, data=dat)
summary(modelo)

coef(modelo)[2]

coef(modelo)[1] 

sqrt(0.2319)

228.98*0.10531+5.69371

barplot(table(dat$wt))
dat$wt