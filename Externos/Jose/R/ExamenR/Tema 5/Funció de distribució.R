LowLim = 0
UppLim = 2
delta = 0.0001

x.less0 = seq(LowLim-1, LowLim, delta)
x.less2 = seq(LowLim, UppLim, delta)
x.greater2 = seq(UppLim, UppLim+1, delta)

Fx.less0 = rep(0, length(x.less0))
Fx.less2 = 1/8*x.less2+3/16*x.less2^2
Fx.greater2 = rep(1, length(x.greater2))

x = c(x.less0, x.less2, x.greater2)
f = c(Fx.less0, Fx.less2, Fx.greater2)

plot(x, f, type="l", col="red", lwd=3)

comment("Si es vol trobar la probabilitat de trobar un valor entre dos punts s ha de restar")

a = 1
b = 1.5

