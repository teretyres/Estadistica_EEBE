lluvia = c(97,27,93,175,38,192,28,182,61,77)
incendios = c(521,863,712,163,138,811,534,442,963,313)
tabla = data.frame(lluvia,incendios)
tabla
plot(incendios,lluvia)