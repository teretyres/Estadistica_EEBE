attach(ester)
ester
plot(t,log(conc))
recta = lm(log(conc)~t)
abline(recta)
summary(recta)
m = recta$coef[1]
m
b = recta$coef[2]
b
y = b*70+m
y
10^(y)