x1 = 1:6
f1 = rep(1/6,6)
F1 = cumsum(f1)
plot(c(0,x1,7), c(0,F1,1),xlab="X1", ylab="F1(x)", type="s" , col="red", lwd=3)