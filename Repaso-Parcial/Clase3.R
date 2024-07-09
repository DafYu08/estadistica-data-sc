library("MASS")
library("aplpack")

datos <- read.table("buffalo.txt", sep = " ")
datos <- as.numeric(datos)

hist(datos) #default
ej1 <-  seq(20, 130, length.out = 20)
hist(datos, breaks = ej1)

ejc <- function(puntos, h, datos){
  probabilidades <- c()
  for (x in puntos) {
    suma_x <- 0
    for (d in datos) {
      if (x-h <= d && d <= x+h){
        suma_x<- suma_x + 1
      }
    }
    probabilidades <- c(probabilidades, suma_x/length(datos))
  }
  return(probabilidades)
}

for (h in c(10,20, 30)) {
  print(ejc(datos, h, datos))
  print("------------------------")
}

densidad_est_parzen <- function(datos, h, x) {
  n <- length(datos)
  f_h <- (1/2) * sum( abs((x - datos)/h) <= 1) / (h * n)
  return(f_h)
}
