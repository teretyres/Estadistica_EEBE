datos_2=c(105,221,183,186,121,181,180,143,167,141,97,154,153,174,120,
          168,176,110,158,133,245,228,174,199,181,158,156,123,229,146,
          163,131,154,115,160,208,158,169,148,158,207,180,190,193,194,
          133,150,135,118,149,134,178,76,167,184,135,218,157,101,171,
          165,172,199,151,142,163,145,171,160,175,149,87,160,237,196,
          201,200,176,150,170)
breaks = seq(70,250,by=20);
breaks
datos_2 = cut(datos_2, breaks, right = FALSE)
datos_2
ni = table(datos_2)
fi = table(datos_2)/length(datos_2)
Ni = cumsum(ni)
Fi = cumsum(fi)
Tabla_Frecuencia = cbind(ni,fi,Ni,Fi)
Tabla_Frecuencia
summary(datos_2)