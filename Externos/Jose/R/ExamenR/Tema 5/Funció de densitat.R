x = seq(0,2,0.0001)
f = 1/8+3/8*x
plot(x,f,type="l", col="red", lwd=3)
delta = 0.0001
Limit.inferior = 1
Limit.superior = 1.5
x.x = seq(Limit.inferior, Limit.superior, delta)
coord.x = c(Limit.inferior, x.x, Limit.superior)
coord.y = c(0, 1/8+3/8*x.x, 0)
polygon(coord.x, coord.y, col = "blue")
text(Limit.inferior, 0.7, "P(1 < X < 1,5", pos = 3, col = "blue")