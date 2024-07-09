library(tidyverse)
library(dplyr)
view(Titanic)
?Titanic
?mean
glimpse(Titanic) #Podemos conocer cuáles son las categoías con las que cuenta

?filter
mpg_efficient <- filter(mpg, cty >= 20)
View(mpg_efficient)

mpg_ford <- filter(mpg, manufacturer == "ford")
View(mpg_ford)

mpg_metric <- mpg %>% 
  mutate(cty_metric = 0.425144 * cty)
  group_by(class) %>%
  summarise(mean(cty), median(cty))  

mpg %>% 
  group_by(class) %>%
  summarise(mean(cty), median(cty))  

glimpse(mpg_metric)

#Data visualization with ggplot2

ggplot(mpg, aes(x = cty)) + 
  geom_histogram() + 
  labs(x = "City mileage")

ggplot(mpg, aes(x = cty)) + 
  geom_freqpoly() + 
  labs(x = "City mileage")

ggplot(mpg, aes(x = cty)) + 
  geom_histogram() + 
  geom_freqpoly() + 
  labs(x = "City mileage")

ggplot(mpg, aes(x = cty,
                y = hwy,
                color = class)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2")


#Clase 1 Jemina
x <- 1
y <- 2

x+y
x-y
x/y

c <- "caracteres"
#R distingue mayusculas de minusculas
#Separador decimal es el punto

#Funciones en R ya instaladas
#nombre(arg1, arg2, ...)

v <- c(7, 8, 0, -1) #vectores (todos los elementos deben 
                    #ser del mismo tipo)
v[1]
v[c(2,3)]

#Numeros enteros consecutivos
w <- 1:10

#Secuencias de numeros
z <- seq(from=1, to=4, by=0.04)
help(seq)

x<-rep(x=1, 5)
y<-rep(c("a", "b"), 3)

v_mas_5 <- v+5

x+y #Suma elementos a elemento
x*y #Multiplica elemento a elemento
x^y #eleva elemento a elemento

producto_escalar <- x%*%y

lenght(x)
sum(x)
max(y)
sort(c(5,7,1,5))

#Vectores logicos

x <- c(5,7,1)
y <- c(3,8,1)
z <- x<y #vector logico
sum(z)
x==y
x!=y

#Matrices
A <- matrix(c(1,5,2,-1,4,0,2,7,8), nrow = 3, byrow = TRUE)
help(matrix) #Estan reservadas para tablas de numeros

#Otra forma
f1 <- c(1,5,2)
f2 <- c(-1,4,0)
f3 <- c(2,7,8)

A <- rbind(f1,f2,f3)

c1 <- c(1,-1,2)
c2 <- c(5,4,7)
C3 <- C(2,0,8)

A <- cbind(c1,c2,c3)

#Si quiero el elemento de la posicion [2,3]

A[2,3]
A[C(1,2),3]
A[1, ] #Primera fila todas las columnas
A[ ,3] #Tercera columna todas las filas

#Inversa
solve(A)

#Misma funcion para sistemas de ecuaciones

b <- c(3,5,1)
solve(A,b)

#Aplica funciones a las filas o columnas de la matriz
apply(A, 1, sum) #1 aplica a filas y 2 a columnas

apply(A, 2, mean)


#Experimentos con urnas y bolitas
set.seed(27)

urna <- c("bolita1", "bolita2", "bolita3", "bolita4", "bolita5")
sample(urna, 3, replace=TRUE)

urna <- c("bolita1", "bolita2", "bolita3", "bolita4", "bolita5")
sample(urna, 3, replace=FALSE)

urna <-  c(rep("R", 5), rep("V",3)) #5 rojas y 3 verdes

muestra <- sample(urna, 3, replace = TRUE)

#Ahora genero un vector logico
muestra =="R"
sum(muestra =="R")


#Ahora quiero jugar a las cartas
Mazo <- 1:40

sample(Mazo, length(Mazo), replace = FALSE) #mezclo


#Funciones y ciclos

mezclar <- function(v)
{
  v2 <- sample(v, length(v), replace = FALSE)
  return(v2) #le aviso a R que quiero que me devuelva la funcion
}

cartas <- 1:40
mezclar(cartas)

suma <- function(n)
{
  acum <- 0
  for (i in 1:n) {
    acum <- acum+1
  }
  return(acum)
}


#GRAFICOS SIIIIII

#Funcion Plot

x <- c(1,2,3)
y <-c(4,5,6)
plot(x,y)

x <- seq(-10,10,by=1) #tablita de valores
y=x^2
plot(x,y)

#Cambio que los puntos sean gordos
plot(x,y,pch=20)

#Pero quiero una linea
plot(x,y,type="l", col="chocolate", lwd=2,
     xlab = "Hola x", ylab = "Hola y", main = "Este es mi grafico")


#Superponemos graficos

abline(v=-5, col="deepskyblue")
abline(h=60, col="darkolivegreen", lty=2)
points(x,x/2, col="mediumpurple3", pch=18)
lines(x,x^3)
help(lines)


#IMPORTAR DATASETS

#importamos archivos txt
autos <- read.table("autos.txt", header = TRUE)
#porque tienen nombre las columnas

datos <- read.table("abalone.txt", sep = ',')

alturas <- read.csv("alturas_n_200.csv")

#Contar la cantidad de mujeres con mamas altas
alturas$contextura_madre #puedo guardarla en otro objeto y es un vector
sum(alturas$contextura_madre=="alta")

#Ahora contamos la cantidad de hijas con mamas altas
alturas$genero[alturas$contextura_madre=="altas"]=="F"
sum(alturas$genero[alturas$contextura_madre=="altas"]=="F")

#Tambien lo podemos hacer asi
nuevas <- alturas[alturas$contextura_madre=="altas" & alturas$genero=="F"]
nrow(nuevas)













