Datos = read.table(DatosCoches.txt)
Data = read.table("DatosCoches.txt",header=TRUE, sep="\t", dect=".")
Error in read.table("DatosCoches.txt", header = TRUE, sep = "\t", dect = ".") : 
  unused argument (dect = ".")
> Data = read.table("DatosCoches.txt",header=TRUE, sep="\t", dec=".")
> head(Datos)
Error in head(Datos) : objeto 'Datos' no encontrado
> head(Data)
Model Origin Acceleration Cylinders Displacement Horsepower MPG
1 chevrolet    USA         12.0         8          307        130  18
2     buick    USA         11.5         8          350        165  15
3  plymouth    USA         11.0         8          318        150  18
4       amc    USA         12.0         8          304        150  16
5      ford    USA         10.5         8          302        140  17
6      ford    USA         10.0         8          429        198  15
Mfg Weigth
1  70   3504
2  70   3693
3  70   3436
4  70   3433
5  70   3449
6  70   4341
> head(Data)
Model Origin Acceleration Cylinders Displacement Horsepower MPG
1 chevrolet    USA         12.0         8          307        130  18
2     buick    USA         11.5         8          350        165  15
3  plymouth    USA         11.0         8          318        150  18
4       amc    USA         12.0         8          304        150  16
5      ford    USA         10.5         8          302        140  17
6      ford    USA         10.0         8          429        198  15
Mfg Weigth
1  70   3504
2  70   3693
3  70   3436
4  70   3433
5  70   3449
6  70   4341
> dim(Data)
[1] 100   9
> ni <- table(Data$Origin)
> fi <- table(Data$Origin)
> Tablef = cbind(ni,fi)
> Tablef
ni fi
France   4  4
Germany  9  9
Italy    1  1
Japan   15 15
Sweden   2  2
USA     69 69
> fi <- table(Data$Origin)/length(Data$Origin)
> Tablef = cbind(ni,fi)
> Tablef
ni   fi
France   4 0.04
Germany  9 0.09
Italy    1 0.01
Japan   15 0.15
Sweden   2 0.02
USA     69 0.69
> Data$Acceleration
[1] 12.0 11.5 11.0 12.0 10.5 10.0  9.0  8.5 10.0  8.5 17.5 11.5 11.0
[14] 10.5 11.0 10.0  8.0  8.0  9.5 10.0 15.0 15.5 15.5 16.0 14.5 20.5
[27] 17.5 14.5 17.5 12.5 15.0 14.0 15.0 13.5 18.5 15.5 16.9 14.9 17.7
[40] 15.3 13.0 13.0 13.9 12.8 15.4 14.5 17.6 17.6 22.2 22.1 14.2 17.4
[53] 17.7 21.0 16.2 17.8 12.2 17.0 16.4 13.6 15.7 13.2 21.9 15.5 16.7
[66] 12.1 12.0 15.0 14.0 19.6 18.6 18.0 16.2 16.0 18.0 16.4 20.5 15.3
[79] 18.2 17.6 14.7 17.3 14.5 14.5 16.9 15.0 15.7 16.2 16.4 17.0 14.5
[92] 14.7 13.9 13.0 17.3 15.6 24.6 11.6 18.6 19.4
> min(Data$Acceleration)
[1] 8
> max(Data$Acceleration)
[1] 24.6
> breaks <- seq(8,27,by=3); breaks
[1]  8 11 14 17 20 23 26
> Acel.grp <- cut(Data$Acceleration, breaks, right=FALSE)
> ni <- table(Acel.grp)
> fi <- table(Acel.grp)/length(Acel.grp)
> Ni <- cumsum(ni)
> Fi <- cumsum(fi)
> Tablef3 = cbind(ni,fi,Ni,Fi)
> Tablef3
ni   fi  Ni   Fi
[8,11)  12 0.12  12 0.12
[11,14) 21 0.21  33 0.33
[14,17) 38 0.38  71 0.71
[17,20) 22 0.22  93 0.93
[20,23)  6 0.06  99 0.99
[23,26)  1 0.01 100 1.00
> Data$MPG
[1] 18.0 15.0 18.0 16.0 17.0 15.0 14.0 14.0 14.0 15.0  NaN  NaN  NaN
[14]  NaN  NaN 15.0 14.0  NaN 15.0 14.0 24.0 22.0 18.0 21.0 27.0 26.0
[27] 25.0 24.0 25.0 26.0 21.0 10.0 10.0 11.0  9.0 28.0 25.0 25.0 26.0
[40] 27.0 17.5 16.0 15.5 14.5 22.0 22.0 24.0 22.5 29.0 24.5 29.0 33.0
[53] 20.0 18.0 18.5 17.5 29.5 32.0 28.0 26.5 20.0 13.0 19.0 19.0 16.5
[66] 16.5 13.0 13.0 13.0 28.0 27.0 34.0 31.0 29.0 27.0 24.0 23.0 36.0
[79] 37.0 31.0 38.0 36.0 36.0 36.0 34.0 38.0 32.0 38.0 25.0 38.0 26.0
[92] 22.0 32.0 36.0 27.0 27.0 44.0 32.0 28.0 31.0
> min(Data$MPG)
[1] NaN
> max(Data$MPG)
[1] NaN
> min(Data$MPG, NaN = False)
Error: inesperado '=' in "min(Data$MPG, NaN ="
> min(Data$MPG, NaN = 0)
Error: inesperado '=' in "min(Data$MPG, NaN ="
> min(Data$MPG, NaN == 0)
[1] NA
> max(Data$MPG)
[1] NaN
> mpg <- na.omit(Datos$MPG)
Error in na.omit(Datos$MPG) : objeto 'Datos' no encontrado
> mpg <- na.omit(DatosMPG)
Error in na.omit(DatosMPG) : objeto 'DatosMPG' no encontrado
> mpg <- na.omit(Data$MPG)
> mpg
[1] 18.0 15.0 18.0 16.0 17.0 15.0 14.0 14.0 14.0 15.0 15.0 14.0 15.0
[14] 14.0 24.0 22.0 18.0 21.0 27.0 26.0 25.0 24.0 25.0 26.0 21.0 10.0
[27] 10.0 11.0  9.0 28.0 25.0 25.0 26.0 27.0 17.5 16.0 15.5 14.5 22.0
[40] 22.0 24.0 22.5 29.0 24.5 29.0 33.0 20.0 18.0 18.5 17.5 29.5 32.0
[53] 28.0 26.5 20.0 13.0 19.0 19.0 16.5 16.5 13.0 13.0 13.0 28.0 27.0
[66] 34.0 31.0 29.0 27.0 24.0 23.0 36.0 37.0 31.0 38.0 36.0 36.0 36.0
[79] 34.0 38.0 32.0 38.0 25.0 38.0 26.0 22.0 32.0 36.0 27.0 27.0 44.0
[92] 32.0 28.0 31.0
attr(,"na.action")
[1] 11 12 13 14 15 18
attr(,"class")
[1] "omit"
> max(mpg)
[1] 44
> breaks <- seq(5,45, by=10);breaks
[1]  5 15 25 35 45
> mpg.grup <- cut(mpg, breaks, right=TRUE)
> ni <- table(mpg.grup)
> fi <- table(mpg.grup)/length(mpg.grup)
> Ni <- cumsum(ni)
> Fi <- cumsum(fi)
> Tabla_f10 = cbind(ni,fi,Ni,Fi); Tabla_f10
ni        fi Ni        Fi
(5,15]  19 0.2021277 19 0.2021277
(15,25] 35 0.3723404 54 0.5744681
(25,35] 29 0.3085106 83 0.8829787
(35,45] 11 0.1170213 94 1.0000000
> max(Data$Displacement)
[1] 455
> new_breaks <- seq(50,500, by=50)
> Data$Displacement
[1] 307 350 318 304 302 429 454 440 455 390 133 350 351 383 360 383 340
[18] 302 400 455 113 198 199 200  97  97 110 107 104 121 199 360 307 318
[35] 304 107 116 140  98 101 305 318 304 351 225 250 200 232  85  98  90
[52]  91 225 250 250 258  97  85  97 140 130 318 120 156 168 350 350 302
[69] 318 112 112 112 112 135 151 140 151 105  91  91 105  98 120 107 108
[86]  91  91  91 181 262 156 232 144 135 151 140  97 135 120 119
> h2.wind <- hist(Data$Displacement, breaks==new_breaks)
Error in hist.default(Data$Displacement, breaks == new_breaks) : 
  some 'x' not counted; maybe 'breaks' do not span range of 'x'
> h2.wind <- hist(Data$Displacement, breaks=new_breaks)
> h2.wind.counts[4]
Error: objeto 'h2.wind.counts' no encontrado
> h2.wind$counts[4]
[1] 7
> bp = boxplot(Data[-c(1,2)])
