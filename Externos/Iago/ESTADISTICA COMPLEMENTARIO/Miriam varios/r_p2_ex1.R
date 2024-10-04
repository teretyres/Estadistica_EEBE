data("stackloss")
> head(stacklos)
Error in head(stacklos) : objeto 'stacklos' no encontrado
> head(stackloss)
Air.Flow Water.Temp Acid.Conc. stack.loss
1       80         27         89         42
2       80         27         88         37
3       75         25         90         37
4       62         24         87         28
5       62         22         87         18
6       62         23         87         18
>     #frecuencias
  > stackloss$Air.Flow
[1] 80 80 75 62 62 62 62 62 58 58 58 58 58 58 50 50 50 50 50 56 70
> stackloss$Air.Flow[5]
[1] 62
> dim(stackloss)
[1] 21  4
> dim(stackloss)[1]
[1] 21
> dim(stackloss)[2]
[1] 4
> help("stackloss")
> #sin ""
  > datos_1 = c(stackloss$Air.Flow)
> ni = table(datos_1)
> fi = table(datos_1)
> ni <- table(stackloss$Air.Flow)
> fi <- table(stackloss$Air.Flow)/length(stackloss$Air.Flow)
> Ni <- consum(ni)
Error in consum(ni) : no se pudo encontrar la función "consum"
> Ni <- cumsum(ni)
> Fi <- cumsum(fi)
> Tabla_frec= cbind(ni,fi,Ni,Fi)
> Tabla_frec
ni         fi Ni        Fi
50  5 0.23809524  5 0.2380952
56  1 0.04761905  6 0.2857143
58  6 0.28571429 12 0.5714286
62  5 0.23809524 17 0.8095238
70  1 0.04761905 18 0.8571429
75  1 0.04761905 19 0.9047619
80  2 0.09523810 21 1.0000000
> stem(stackloss$Air.Flow)

The decimal point is 1 digit(s) to the right of the |
  
  5 | 000006888888
6 | 22222
7 | 05
8 | 00

> Temp <- stackloss$Water.Temp[stackloss$Air.Flow==58]
> Temp
[1] 23 18 18 17 18 19
> dotchart(Temo)
Error in dotchart(Temo) : objeto 'Temo' no encontrado
> dotchart(Temp)
> barplot(table(stackloss$Acid.Conc.))
> pie(table(stackloss$Air.Flow))
> stackloss$Air.Flow
[1] 80 80 75 62 62 62 62 62 58 58 58 58 58 58 50 50 50 50 50 56 70
> bp = boxplot(stackloss)
> mean(stackloss$Air.Flow)
[1] 60.42857
> median(stackloss$Water.Temp)
[1] 20
> quantile(stackloss$Acid.Conc.0,25)
Error in quantile.default(stackloss$Acid.Conc.0, 25) : 
  'probs' outside [0,1]
> quantile(stackloss$Acid.Conc.0.25)
0%  25%  50%  75% 100% 
NA   NA   NA   NA   NA 
> quantile(stackloss$Acid.Conc.,0.25)
25% 
82 
> quantile(stackloss$stack.loss,0.18)
18% 
8.6 
> IQR(stackloss$Water.Temp)
[1] 6
> sd(stackloss$Acid.Conc.)
[1] 5.358571
> var(stackloss$Air.Flow)
[1] 84.05714