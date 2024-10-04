x = 1:6
dau = sample(x,size=100,replace=T, prob=)
histograma_dau = hist(dau, breaks = seq(0.5,6.5,by=1), freq = FALSE, main="Histograma Dau")
data.frame(Resultat=histograma_dau$mids, Frequancia = histograma_dau$density)
mean(dau)
var(dau)