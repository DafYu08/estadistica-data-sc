"
Tp Inicial
Integrantes: Emi, Agus, Dafne
Fecha de entrega: 02/04
"

library("ggplot2")
library('aplpack')

datosBebes <- read.table("ENNyS_menorA2.txt", header = TRUE)
names(datosBebes)
attach(datosBebes)

#Ejercicio 1


# ej 6 

bagplot(Perim_cef, datosBebes$Talla,
        ylim= c(30,140),xlab = "Perímetro Cefálico", ylab = "Talla", main = "Bagplot Perimetro cefálico vs Talla")


# Estos puntos que están fuera del polígono grande se consideran atípicos y pueden representar valores inusuales o extremos en la relación entre las dos variables.
# se interpreta que es muy poco comun tener un perimetro cefalico muy pequeño y a la vez una talla "normal" , lo que tambien nos dice que es inusual tener un perimetro cefalico muy grande y al mismo tiempo una talla muy grande

#¿Se registraron bebes de Talla alta y Perímetro Cefálico chico? no
# ¿Y de Talla baja y Perimetro Cefalico grande? si 

mean(datosBebes$Talla)


# ej 7 


#Ejercicio 6

bagplot(Perim_encef, Talla, approx.limit = 100, 
        xlab = "Perímetro Encefálico", ylab = "Talla", xlim = c(0, 60))
#Los datos atípicos corresponden a aquellos bebés con perímetro encefálico menor a 30 cm y de Talla menor a 40 o mayor a 100
#Se puede observar que el perímetro encefálico suele ser proporcional a la Talla
#Si consideramos talla alta a partir de 80cm, podemos observar dos datos con perímetro 10 (deben estar mal cargados)
##Si consideramos talla baja hasta 60cm, hay más outliers que corresponden a pe grande.

#Ejercicio 7
bagplot(Perim_encef, Talla, approx.limit = 100, 
        xlab = "Perímetro Encefálico", ylab = "Talla", xlim = c(0, 60),
        show.outlier = FALSE)


bagplot(Perim_encef ~ Talla | Sexo, approx.limit = 100, 
        xlab = "Perímetro Encefálico", ylab = "Talla", xlim = c(0, 60),
        show.outlier = FALSE)
