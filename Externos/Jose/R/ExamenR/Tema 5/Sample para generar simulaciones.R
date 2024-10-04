x = seq(0,2,0.01) 
f = 1/8+3/8*x
sample(x,size=1,prob=f)
comment("Per simular més observacions augmentem el size")
sample(x,size=100,replace = T, prob=f)

comment("Histgrama amb 10000 resultats, augmentem el nombre del vectors reduint el delta")
x = seq(0,2,0.0001) 
f = 1/8+3/8*x
set.seed((10))
Sim.data = sample(x,size=10000,replace = T,prob=f)
plot(x,f,type="l", col="red", lwd=3)
hist(Sim.data, freq=FALSE, add=T)
gra.his = recordPlot()

comment("Calculem la mitjana i la variança")
mean(Sim.data)
var(Sim.data)